defmodule JSGenServer do
  defmacro __using__(opts) do
    quote location: :keep do
      use GenServer
      require Logger

      def start_link(state, opts \\ []) do
        GenServer.start_link(__MODULE__, state, opts)
      end

      defp send_command(port, cmd) do
        :erlang.port_command(port, Poison.encode!(cmd))
      end

      def init(state) do
        module_path = Path.join([__DIR__, unquote(opts[:path])])
        cmd = 'node #{:code.priv_dir(:js_gen_server)}/genserver.js #{module_path}'
        port = :erlang.open_port({:spawn, cmd}, [{:packet, 4}, :nouse_stdio])

        send_command(port, %{type: "init", state: state})

        {:ok, %{port: port,
                waiting: %{},
                counter: 1,
                tasks: %{}}}
      end

      def handle_call(arg, from, state) do
        send_command(state.port, %{type: "call",
                                   arg: Tuple.to_list(arg),
                                   counter: state.counter})
        {:noreply, %{state | waiting: Map.put(state.waiting, state.counter, from),
                             counter: state.counter + 1}}
      end

      def handle_cast(arg, state) do
        send_command(state.port, %{type: "cast", arg: Tuple.to_list(arg)})
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
            %Task{ref: ref} = Task.async(__MODULE__, String.to_atom(name), args)
            {:noreply, %{state | tasks: Map.put(state.tasks, ref, counter)}}
        end
      end
      def handle_info({ref, response}, state) when is_reference(ref) do
        case state.tasks[ref] do
          nil ->
            {:noreply, state}
          counter ->
            send_command(state.port, %{type: "response",
                                       counter: counter,
                                       response: response})
            {:noreply, %{state | tasks: Map.delete(state.tasks, ref)}}
        end
      end
      def handle_info(_msg, state) do
        {:noreply, state}
      end
    end
  end
end
