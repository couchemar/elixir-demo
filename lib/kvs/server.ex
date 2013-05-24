defmodule KVS.Store do
  use GenServer.Behaviour

  def start_link(key, value) do
    :gen_server.start_link(__MODULE__, [key, value], [])
  end

  def init([key, value]) do
    :ets.insert_new(:kvs_reg, {key, self})
    {:ok, [value]}
  end

  def handle_call({:push, value}, _from, stack) do
    {:reply, :ok, [value|stack]}
  end

  def handle_call(:pop, _from, stack) when length(stack) == 0, do: {:reply, :nothing, stack}

  def handle_call(:pop, _from, [value|stack]) do
    {:reply, value, stack}
  end

  def handle_call(:sum, _from, stack) when length(stack) < 2, do: {:reply, :not_enough, stack}

  def handle_call(:sum, _from, stack) do
    [a|stack] = stack
    [b|stack] = stack
    s = a + b
    {:reply, s, [s|stack]}
  end
end