-module(tcp_proxy_middleware).

-export([handle_client/1, handle_upstream/1]).

handle_client(Data) ->
    io:format(">>> ~s", [Data]),
    Data.

handle_upstream(Data) ->
    io:format("<<< ~s", [Data]),
    Data.
