defmodule Kvs do
  use Application.Behaviour

  def start(_type, []) do
    KVS.Store.Supervisor.start_link()
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
