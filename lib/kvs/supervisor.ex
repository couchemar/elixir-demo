defmodule KVS.Store.Supervisor do
  use Supervisor.Behaviour

  def start_link() do
    :supervisor.start_link({:local, :store_sup}, __MODULE__, [])
  end

  def init([]) do
    :kvs_reg = :ets.new(:kvs_reg, [:set, :named_table, :public,
                                   {:read_concurrency, true},
                                   {:write_concurrency, true}])

    children = [ worker(KVS.Store, []) ]
    supervise children, strategy: :simple_one_for_one
  end

  def push(key, value) do
    case :ets.lookup(:kvs_reg, key) do
      [] ->
        :supervisor.start_child(:store_sup, [key, value])
      [{^key, pid}|_] ->
        :gen_server.call(pid, {:push, value})
    end
    :ok
  end

  def pop(key) do
    case :ets.lookup(:kvs_reg, key) do
      [] ->
        :nothing
      [{^key, pid}|_] ->
        :gen_server.call(pid, :pop)
    end
  end

  def sum(key) do
    case :ets.lookup(:kvs_reg, key) do
      [] ->
        :nothing
      [{^key, pid}|_] ->
        :gen_server.call(pid, :sum)
    end
  end

  def amax(key) do
    case :ets.lookup(:kvs_reg, key) do
      [] ->
        :nothing
      [{^key, pid}|_] ->
        :gen_server.call(pid, :amax)
    end
  end

  def add_hook(key) do
    case :ets.lookup(:kvs_reg, key) do
      [] ->
        :no_process
      [{^key, pid}|_] ->
        :gen_server.call(pid, :add_hook)
    end
  end

end