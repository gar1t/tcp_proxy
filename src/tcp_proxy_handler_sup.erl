-module(tcp_proxy_handler_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_handler/2]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, tcp_proxy_handler, [registered]).

start_handler(LSock, Upstream) ->
    handle_start_task(
      e2_task_supervisor:start_task(?MODULE, [LSock, Upstream])).

handle_start_task({ok, _Pid}) -> ok;
handle_start_task({error, Err}) ->
    error({handler_start_err, Err}).
