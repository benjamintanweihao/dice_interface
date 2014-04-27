defmodule DiceInterface do
  use Application.Behaviour

  @default_port 1155

  def start(_type, _args) do
    port = case :application.get_env(:dice_interface, :port) do
             {:ok, port} -> port
             _ -> @default_port
           end
    tcp_options  = [{:active, true}]
    {:ok, lsock} = :gen_tcp.listen(port, tcp_options)

    case DiceInterface.Supervisor.start_link(lsock) do
      {:ok, pid} -> 
        # Start a listening socket.
        DiceInterface.Supervisor.start_child
        {:ok, pid} 
      other ->
        {:error, other}
    end
  end

  def stop(_state) do
    :ok
  end

end
