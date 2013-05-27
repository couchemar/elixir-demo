defmodule KVS.Supervisor do
  use Supervisor.Behaviour

  def start_link() do
    :supervisor.start_link({:local, :kvs_sup}, __MODULE__, [])
  end

  def init([]) do
    children = [ supervisor(KVS.Store.Supervisor, []),
                 worker(KVS.Lua_server, []) ]
    supervise children, strategy: :one_for_one
  end

end