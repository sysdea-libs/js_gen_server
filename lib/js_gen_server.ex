defmodule JSGenServer do
  use GenServer
  require Logger

  defmacro __using__(opts) do
    quote do
      def start_link(state, opts \\ []) do
        GenServer.start_link(__MODULE__,
          {Path.join([__DIR__, unquote(opts[:path])]), state}, opts)
      end

      def init(arg), do: JSGenServer.init(arg)
      def handle_call(arg, from, state), do: JSGenServer.handle_call(arg, from, state)
      def handle_cast(arg, state), do: JSGenServer.handle_call(arg, state)
      def handle_info({port, {:data, msg}}=arg, %{port: port}=state) do
        JSGenServer.handle_info(arg, state)
      end
    end
  end

  def start_link(js_script, state, opts \\ []) do
    GenServer.start_link(__MODULE__, {js_script, state}, opts)
  end

  @base_cmd 'node #{:code.priv_dir(:js_gen_server)}/genserver.js '

  def init({js_script, state}) do
    port = :erlang.open_port({:spawn, @base_cmd ++ to_char_list(js_script)},
                             [{:packet, 4}, :nouse_stdio])

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
                         Poison.Encoder.encode(%{type: "call",
                                                 arg: Tuple.to_list(arg),
                                                 counter: state.counter}, []))

    {:noreply, %{state | waiting: Map.put(state.waiting, state.counter, from),
                         counter: state.counter + 1}}
  end

  def handle_cast(arg, state) do
    :erlang.port_command(state.port,
                         Poison.Encoder.encode(%{type: "cast",
                                                 arg: Tuple.to_list(arg)}, []))

    {:noreply, state}
  end

  def handle_info({port, {:data, msg}}, %{port: port}=state) do
    case Poison.decode!(msg) do
      %{"type" => "log", "level" => level, "message" => message} ->
        Logger.log(String.to_atom(level), message)
        {:noreply, state}
      %{"type" => "response", "counter" => counter, "response" => response } ->
        GenServer.reply Map.get(state.waiting, counter), response
        {:noreply, %{state | waiting: Map.delete(state.waiting, counter)}}
    end
  end
end
