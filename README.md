# DiCE Interface

This is a TCP server that communicates between a client application over JSON and DiCE, the Distributed Cache in Elixir.

The TCP server is inspired by Chapter 11 (Implementing a TCP server) of Erlang/OTP in Action.

## Example Run

```
> iex -S mix
```

Here is a simple Ruby Client to talk to DiCE:

```ruby
#!/usr/bin/env ruby
 
require 'json'
require 'socket'
 
class DiceClient
  def initialize(host, port)
    @host = host
    @port = port
    puts "Connecting to server..."
  end
 
  def put(key, value)
    op({"op" => "put", "key" => key, "value" => value})
  end
 
  def get(key)
    op({"op" => "get", "key" => key})
  end
 
  def remove(key)
    op({"op" => "remove", "key" => key})
  end
 
  def op(data)
    @socket = TCPSocket.new(@host, @port)
    @socket.write(JSON.generate(data))
    @socket.read.to_json
  end
 
end
 
client = DiceClient.new('localhost', 1155)
 
puts "=== PUT ==="
puts client.put("elixir", "awesome sauce")
puts client.put("ruby", "is pretty great too")
puts "=== GET ==="
puts client.get("elixir")
puts client.get("ruby")
puts "=== REMOVE ==="
puts client.remove("elixir")
puts "=== PUT ==="
puts client.put("elixir", "really awesome sauce")
puts "=== GET ==="
puts client.get("elixir")
puts client.get("ruby")
```
