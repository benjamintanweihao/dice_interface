# DiCE Interface

This is a TCP server that communicates between a client application over JSON and DiCE, the Distributed Cache in Elixir.

The TCP server is inspired by Chapter 11 (Implementing a TCP server) of Erlang/OTP in Action.

## Example Run

```
> iex -S mix
```

In this example, we use a simple Ruby program to send a message over a TCP socket.

```ruby
require 'socket'

sock = TCPSocket.new('localhost', 1155)
sock.write "Elixir Sockets FTW"

while line = sock.gets # Read lines from socket
  puts line            # and print them
end
sock.close
```

Observe in the `iex` console:

```elixir
iex> "Elixir Sockets FTW"
```
