defmodule Kvs do
  use Application.Behaviour

  def start(_type, []) do
    {:ok, listen_port} = :application.get_env(:listen_port)
    {:ok, nodes} = :application.get_env(:nodes)
    Enum.map nodes, Node.connect &1
    {:ok, _} = :ranch.start_listener(KVS.Protocol, 100,
                                     :ranch_tcp, [port: listen_port],
                                     KVS.Protocol, [])
    KVS.Supervisor.start_link()
  end

  def push(key, value) do
    KVS.Store.Supervisor.push(key, value)
  end

  def pop(key) do
    KVS.Store.Supervisor.pop(key)
  end

  def sum(key) do
    KVS.Store.Supervisor.sum(key)
  end
end
