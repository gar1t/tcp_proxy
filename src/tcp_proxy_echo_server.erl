-module(tcp_proxy_echo_server).

-behavior(e2_task).

-export([start_link/1]).

-export([handle_task/1]).

start_link(Port) ->
    e2_task:start_link(?MODULE, Port, []).

handle_task(Port) ->
    LSock = listen(Port),
    next_connection(LSock).

listen(Port) ->
    ListenOptions = listen_options(),
    handle_tcp_listen(gen_tcp:listen(Port, ListenOptions), Port).

listen_options() ->
    [{active, false},
     {reuseaddr, true}].

handle_tcp_listen({ok, LSock}, Port) ->
    e2_log:info("Echo server listening on port ~p~n", [Port]),
    LSock;
handle_tcp_listen({error, Err}, _Port) ->
    error({listen_error, Err}).

next_connection(LSock) ->
    handle_connection(accept_connection(LSock), LSock).

accept_connection(LSock) ->
    handle_tcp_accept(gen_tcp:accept(LSock)).

handle_tcp_accept({ok, Sock}) -> Sock;
handle_tcp_accept({error, Err}) -> error({accept_error, Err}).

handle_connection(Sock, LSock) ->
    start_handler(Sock),
    next_connection(LSock).

start_handler(Sock) ->
    proc_lib:spawn_link(tcp_echo_handler, start_link, [Sock]).
