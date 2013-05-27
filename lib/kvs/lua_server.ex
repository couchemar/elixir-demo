defmodule KVS.Lua_server do
  use GenServer.Behaviour

  def start_link() do
    :gen_server.start_link({:local, :lua_server}, __MODULE__, [], [])
  end

  def init([]) do
    lua = :luerl.init
    {:ok, lua}
  end

  def handle_call({:load, hook}, _from, lua) do
    {result, lua} = try do
                      {_, lua} = :luerl.do(hook, lua)
                      {:ok, lua}
                    rescue
                      _error ->
                        {:error, lua}
                    end
    {:reply, result, lua}
  end

  def handle_call({:hook, value}, _from, lua) do
    {[res], lua} = :luerl.call_function([:hook], [value], lua)
    {:reply, trunc(res), lua}
  end

  def load_hook(hook) do
    :gen_server.call(:lua_server, {:load, hook})
  end

end