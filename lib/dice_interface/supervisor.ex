defmodule DiceInterface.Supervisor do
  use Supervisor.Behaviour

  def start_link(lsock) do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [lsock])
  end

  def start_child do
    :supervisor.start_child(__MODULE__, [])
  end

  def init([lsock]) do
    worker_opts = [restart: :temporary, shutdown: :brutal_kill]
    children    = [
      worker(DiceInterface.Server, [lsock], worker_opts)
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end
