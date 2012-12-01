-module(tcp_proxy_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, [{tcp_proxy_handler_sup, [supervisor]}, tcp_proxy]}.
