defmodule JSGenServer do
  use GenServer
  require Logger

  defmacro __using__(opts) do
    quote do
      def start_link(state, opts \\ []) do
        GenServer.start_link(__MODULE__, state, opts)
      end

      def init(state) do
        module_path = Path.join([__DIR__, unquote(opts[:path])])
        cmd = 'node #{:code.priv_dir(:js_gen_server)}/genserver.js #{module_path}'

        port = :erlang.open_port({:spawn, cmd}, [{:packet, 4}, :nouse_stdio])

        :erlang.port_command(port,
                             Poison.Encoder.encode(%{type: "init",
                                                     state: state}, []))

        {:ok, %{port: port,
                waiting: %{},
                counter: 1,
                tasks: %{}}}
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
          %{"type" => "call", "name" => name, "args" => args, "counter" => counter} ->
            %Task{pid: pid, ref: ref} = Task.async(__MODULE__, String.to_atom(name), args)

            {:noreply, %{state | tasks: Map.put(state.tasks, ref, counter)}}
        end
      end

      def handle_info({ref, response}, %{port: port}=state) do
        :erlang.port_command(port,
                             Poison.Encoder.encode(%{type: "response",
                                                     counter: state.tasks[ref],
                                                     response: response}, []))

        {:noreply, %{state | tasks: Map.delete(state.tasks, ref)}}
      end

      def handle_info({:DOWN, ref, :process, pid, :normal}, state) do
        {:noreply, state}
      end
    end
  end
end
