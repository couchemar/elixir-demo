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
    {_, lua} = :luerl.do(hook, lua)
    {:reply, :ok, lua}
  end

  def handle_call({:hook, value}, _from, lua) do
    {[res], lua} = :luerl.do("return hook(#{value})", lua)
    {:reply, trunc(res), lua}
  end

  def load_hook(hook) do
    :gen_server.call(:lua_server, {:load, hook})
  end

end