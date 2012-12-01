-module(tcp_proxy_handler).

-behavior(e2_task).

-export([start_link/2]).

-export([handle_task/1, handle_msg/3]).

-record(state, {lsock, csock, usock, upstream}).

start_link(LSock, Upstream) ->
    e2_task:start_link(?MODULE, init_state(LSock, Upstream), []).

init_state(LSock, Upstream) ->
    #state{lsock=LSock, upstream=Upstream}.

handle_task(State) ->
    ClientConnectedState = wait_for_client(State),
    start_next_handler(State),
    ReadyState = connect_upstream(ClientConnectedState),
    set_active_once(ReadyState),
    {wait_for_msg, ReadyState}.

wait_for_client(#state{lsock=LSock}=State) ->
    handle_client_accept(gen_tcp:accept(LSock), State).

handle_client_accept({ok, Sock}, State) ->
    State#state{csock=Sock};
handle_client_accept({error, Err}, _State) ->
    error({accept_error, Err}).

connect_upstream(#state{upstream={Host, Port}}=State) ->
    ConnectOptions = connect_upstream_options(),
    handle_upstream_connect(
      gen_tcp:connect(Host, Port, ConnectOptions), State).

connect_upstream_options() ->
    [{active, once}].

handle_upstream_connect({ok, Sock}, State) ->
    State#state{usock=Sock};
handle_upstream_connect({error, Err}, State) ->
    close_client(State),
    error({upstream_connect_error, Err}).

close_client(#state{csock=Sock}) ->
    gen_tcp:close(Sock).

start_next_handler(#state{lsock=LSock, upstream=Upstream}) ->
    tcp_proxy_handler_sup:start_handler(LSock, Upstream).

set_active_once(#state{csock=CSock, usock=USock}) ->
    ok = inet:setopts(CSock, [{active, once}]),
    ok = inet:setopts(USock, [{active, once}]);
set_active_once(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]).

handle_msg({tcp, CSock, Packet}, _From, #state{csock=CSock}=State) ->
    handle_client_packet(Packet, State);
handle_msg({tcp, USock, Packet}, _From, #state{usock=USock}=State) ->
    handle_upstream_packet(Packet, State);
handle_msg({tcp_closed, CSock}, _From, #state{csock=CSock}=State) ->
    handle_client_closed(State);
handle_msg({tcp_closed, USock}, _From, #state{usock=USock}=State) ->
    handle_upstream_closed(State).

handle_client_packet(Packet0, #state{csock=CSock, usock=USock}=State) ->
    set_active_once(CSock),
    Packet = tcp_proxy_middleware:handle_client(Packet0),
    send_packet(Packet, USock),
    {noreply, State}.

send_packet(Packet, Sock) ->
    handle_tcp_send(gen_tcp:send(Sock, Packet)).

handle_tcp_send(ok) -> ok;
handle_tcp_send({error, Err}) ->
    error({send_error, Err}).

handle_upstream_packet(Packet0, #state{csock=CSock, usock=USock}=State) ->
    set_active_once(USock),
    Packet = tcp_proxy_middleware:handle_upstream(Packet0),
    send_packet(Packet, CSock),
    {noreply, State}.

handle_client_closed(#state{usock=USock}=State) ->
    close_socket(USock),
    {stop, normal, State}.

close_socket(Sock) ->
    gen_tcp:close(Sock).

handle_upstream_closed(#state{upstream=Upstream}=State) ->
    {stop, {upstream_closed, Upstream}, State}.
