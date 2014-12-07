defmodule JSGenServer do
  use GenServer

  def start_link(js_script , opts \\ []) do
    GenServer.start_link(__MODULE__, {js_script}, opts)
  end

  @base_cmd 'node #{:code.priv_dir(:gen_server_js)}/genserver.js '

  def init({js_script}) do
    port = :erlang.open_port({:spawn, @base_cmd ++ to_char_list(js_script)}, packet: 4)
    {:ok, %{port: port,
            script: js_script,
            waiting: %{},
            counter: 1}}
  end

  def handle_call({:call, f, args}, from, state) do
    :erlang.port_command(state.port,
                         Poison.Encoder.encode(%{method: to_string(f),
                                                 counter: state.counter,
                                                 args: args}, []))

    {:noreply, %{state | waiting: Map.put(state.waiting, state.counter, from),
                         counter: state.counter + 1}}
  end

  def handle_info({_pid, {:data, msg}}, state) do
    parsed = Poison.decode!(msg)
    GenServer.reply Map.get(state.waiting, parsed["counter"]), parsed["response"]

    {:noreply, %{state | waiting: Map.delete(state.waiting, parsed["counter"])}}
  end

  def call(pid, arg) do
    [f|args] = Tuple.to_list(arg)

    GenServer.call(pid, {:call, f, args})
  end
end
