-module(tcp_proxy).

-behavior(e2_task).

-export([start/0, stop/0]).

-export([start_link/0]).

-export([init/1, handle_task/1]).

-record(state, {upstream, local_port, lsock}).

start() ->
    e2_application:start_with_dependencies(tcp_proxy).

stop() ->
    application:stop(tcp_proxy).

start_link() ->
    e2_task:start_link(?MODULE, [], [registered]).

init([]) ->
    LocalPort = validate_port(env(local_port)),
    Upstream = validate_upstream(env(upstream)),
    ListenOptions = listen_options(),
    LSock = listen(LocalPort, ListenOptions),
    {ok, #state{upstream=Upstream, lsock=LSock}}.

env(Name) ->
    handle_required_env(application:get_env(Name), Name).

handle_required_env(undefined, Name) ->
    error({required_env, Name});
handle_required_env({ok, Value}, _Name) ->
    Value.

validate_port(Port) when is_integer(Port) -> Port;
validate_port(Other) -> error({invalid_port, Other}).

validate_upstream({Host, Port}=Up) when is_list(Host), is_integer(Port) ->
    Up;
validate_upstream(Other) ->
    error({invalid_upstream, Other}).

listen_options() ->
    [{active, false},
     {reuseaddr, true}].

listen(Port, Options) ->
    handle_tcp_listen(gen_tcp:listen(Port, Options), Port).

handle_tcp_listen({ok, LSock}, Port) ->
    e2_log:info("Proxy listening to port ~p~n", [Port]),
    LSock;
handle_tcp_listen({error, Err}, _Port) ->
    error({listen_error, Err}).

handle_task(State) ->
    start_first_handler(State),
    {hibernate, State}.

start_first_handler(#state{lsock=LSock, upstream=Upstream}) ->
    tcp_proxy_handler_sup:start_handler(LSock, Upstream).
