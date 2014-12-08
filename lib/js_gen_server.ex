defmodule JSGenServer do
  use GenServer

  def start_link(js_script, state, opts \\ []) do
    GenServer.start_link(__MODULE__, {js_script, state}, opts)
  end

  @base_cmd 'node #{:code.priv_dir(:gen_server_js)}/genserver.js '

  def init({js_script, state}) do
    port = :erlang.open_port({:spawn, @base_cmd ++ to_char_list(js_script)}, packet: 4)

    :erlang.port_command(port,
                         Poison.Encoder.encode(%{type: "init",
                                                 state: state}, []))

    {:ok, %{port: port,
            script: js_script,
            waiting: %{},
            counter: 1}}
  end

  def handle_call(arg, from, state) do
    :erlang.port_command(state.port,
                         Poison.Encoder.encode(%{type: "handle_call",
                                                 arg: arg,
                                                 counter: state.counter}, []))

    {:noreply, %{state | waiting: Map.put(state.waiting, state.counter, from),
                         counter: state.counter + 1}}
  end

  def handle_info({_pid, {:data, msg}}, state) do
    parsed = Poison.decode!(msg)
    GenServer.reply Map.get(state.waiting, parsed["counter"]), parsed["response"]

    {:noreply, %{state | waiting: Map.delete(state.waiting, parsed["counter"])}}
  end

  def call(pid, arg) do
    GenServer.call(pid, Tuple.to_list(arg))
  end
end
