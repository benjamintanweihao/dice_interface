defmodule DiceInterface.Server do
  use GenServer.Behaviour

  defrecord State, lsock: nil

  #######
  # API #
  #######

  def start_link(lsock) do
    :gen_server.start_link(__MODULE__, [lsock], [])
  end

  def stop do
    :gen_server.cast(:stop)
  end

  #############
  # Callbacks #
  #############

  def init([lsock]) do
    # NOTE: The last argument here is 0, which means that
    # the _caller_ to init/1 immediately returns out.
    {:ok, State.new(lsock: lsock), 0}
  end

  def handle_call(msg, _from, state) do
    {:reply, {:ok, msg}, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info({:tcp, socket, raw_data}, state) do
    new_state = handle_data(socket, raw_data, state)
    {:noreply, new_state}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    {:stop, :normal, state}
  end

  def handle_info(:timeout, State[lsock: lsock] = state) do
    # NOTE: All the interesting stuff happens here:
    {:ok, _sock} = :gen_tcp.accept(lsock)
    # Once a connection had been made ... 
    DiceInterface.Supervisor.start_child
    {:noreply, state}
  end

  ######################
  # Internal Functions #
  ######################

  defp handle_data(socket, raw_data, state) do  
    json = case "#{raw_data}" |> JSON.decode do
             {:ok, hash} ->

               result = case hash["op"] do
                          "put" ->
                            :gen_server.call({:global, Dice.Server}, {:put, hash["key"], hash["value"]})
                          "get" ->
                            :gen_server.call({:global, Dice.Server}, {:get, hash["key"]})
                          "remove" ->
                            :gen_server.call({:global, Dice.Server}, {:remove, hash["key"]})
                          _ ->
                            :error
                        end

               [result: result] |> JSON.encode!
             _ ->
               [result: []] |> JSON.encode!
          end

    :gen_tcp.send(socket, json)
    :gen_tcp.close(socket)
    state
  end
  
end
