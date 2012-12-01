-module(tcp_echo_handler).

-behavior(e2_task).

-export([start_link/1]).

-export([handle_task/1]).

start_link(Sock) ->
    e2_task:start_link(?MODULE, Sock, []).

handle_task(Sock) ->
    next_echo(Sock).

next_echo(Sock) ->
    send(recv(Sock), Sock).

recv(Sock) -> gen_tcp:recv(Sock, 0).

send({ok, Packet}, Sock) ->
    handle_tcp_send(gen_tcp:send(Sock, Packet), Sock);
send({error, closed}, _Sock) ->
    {stop, normal};
send({error, Err}, _Sock) ->
    {stop, Err}.

handle_tcp_send(ok, Sock) ->
    next_echo(Sock);
handle_tcp_send({error, Err}, _Sock) ->
    {stop, Err}.
