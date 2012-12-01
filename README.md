# tcp_proxy

This small OTP app can be used to proxy TCP connections.

My original intent is to use this for debugging an annoying MySQL problem, but
it should be usable for lots of other things.

Smarter people would use Wireshark or another TCP sniffer. For the life of me I
can't get that thing to work right :(

## Super Simple Usage

Copy `priv/dev.config.in` to `priv/dev.config`. Take a look at the file you
created -- the info there is reflected below.

In one OS terminal/shell, run the tcp_proxy app and start an echo server:

```
$ make shell
...
1> tcp_proxy_echo_server:start_link(8888).

=INFO REPORT==== 30-Nov-2012::20:57:19 ===
Echo server listening on port 8888
{ok,<0.52.0>}
2>
```

In another OS terminal, use telnet to connect to the proxy:

```
$ telnet 127.0.0.1 9999
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
```

Start typing stuff and press ENTER. You'll see your typing echoed!

Almost as fun as World of Warcraft, on LSD!

## To Do

- Pluggable middleware layer
- Some useful middleware like writing to a file
