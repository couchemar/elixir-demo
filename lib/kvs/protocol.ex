defmodule KVS.Protocol do
  use GenServer.Behaviour

  defrecord State, socket: nil,
                   transport: nil

  def start_link(ref, socket, transport, opts) do
    :proc_lib.start_link(__MODULE__, :init, [ref, socket, transport, opts])
  end

  def init(ref, socket, transport, _opts) do
    :erlang.process_flag(:trap_exit, true)
    :ok = :proc_lib.init_ack({:ok, self})
    :ok = :ranch.accept_ack(ref)
    :ok = transport.setopts(socket, active: :once)
    :gen_server.enter_loop(__MODULE__, [], State.new(socket: socket,
                                                     transport: transport))
  end

  def handle_cast({:async_result, token, result},
                  State[socket: socket,
                        transport: transport] = state) do
    result = integer_to_binary(result)
    transport.send(socket, "+#{token} #{result}\n\r")
    {:noreply, state}
  end

  def handle_info({:tcp, socket, data}, State[socket: socket,
                                              transport: transport] = state) do
    :ok = transport.setopts(socket, active: :once)
    result = case process_data(String.strip(data)) do
               data when is_atom(data) ->
                 atom_to_binary(data)
               {:token, token} ->
                 "token " <> token
               data ->
                 integer_to_binary(data)
             end
    transport.send(socket, result <> "\n\r")
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, State[socket: socket] = state) do
    {:stop, :normal, state}
  end

  defp process_data(<<"pop ", key :: bitstring>>) do
    KVS.Store.Supervisor.pop(key)
  end

  defp process_data(<<"sum ", key :: bitstring>>) do
    KVS.Store.Supervisor.sum(key)
  end

  defp process_data(<<"amax ", key :: bitstring>>) do
    KVS.Store.Supervisor.amax(key)
  end

  defp process_data(data) do
    process_command(String.split(data))
  end

  defp process_command(["push", key, value]) do
    case String.to_integer(value) do
      {value, _} ->
        KVS.Store.Supervisor.push(key, value)
      :error ->
        :wrong_value
    end
  end

  defp process_command(_command) do
    :unknown_command
  end

end