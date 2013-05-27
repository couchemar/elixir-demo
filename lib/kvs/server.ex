defmodule KVS.Store do
  use GenServer.Behaviour

  defrecord State, stack: nil,
                   hooked: false

  def start_link(key, value) do
    :gen_server.start_link(__MODULE__, [key, value], [])
  end

  def init([key, value]) do
    :ets.insert_new(:kvs_reg, {key, self})
    {:ok, State.new(stack: [value])}
  end

  def handle_call({:push, value}, _from, State[stack: stack,
                                               hooked: hooked] = state) do
    if hooked do
      value = :gen_server.call(:lua_server, {:hook, value})
    end
    {:reply, :ok, state.stack([value|stack])}
  end

  def handle_call(:pop, _from, State[stack: stack] = state) when length(stack) == 0, do: {:reply, :nothing, state}

  def handle_call(:pop, _from, State[stack: [value|stack]] = state) do
    {:reply, value, state.stack(stack)}
  end

  def handle_call(:sum, _from, State[stack: stack] = state) when length(stack) < 2, do: {:reply, :not_enough, state}

  def handle_call(:sum, _from, State[stack: stack] = state) do
    [a|stack] = stack
    [b|stack] = stack
    s = a + b
    {:reply, s, state.stack([s|stack])}
  end

  def handle_call(:amax, {return_pid, _}, State[stack: stack] = state) do
    rb = :crypto.rand_bytes(8)
    token = :base64.encode(rb)
    self_pid = self
    Process.spawn(fn() ->
                      :timer.sleep(5000)
                      max = Enum.max(stack)
                      :gen_server.cast(return_pid, {:async_result, token, max})
                      :gen_server.cast(self_pid, {:delete, max})
                  end)
    {:reply, {:token, token}, state}
  end

  def handle_call(:add_hook, _from, state) do
    {:reply, :ok, state.hooked(true)}
  end

  def handle_call(:remove_hook, _from, state) do
    {:reply, :ok, state.hooked(false)}
  end

  def handle_cast({:delete, element}, State[stack: stack] = state) do
    stack = List.delete(stack, element)
    {:noreply, state.stack(stack)}
  end

end