defmodule JSGenServer do
  defmacro __using__(opts) do
    quote location: :keep do
      @before_compile JSGenServer
      @js_path unquote(opts[:path])

      def start_link(state, opts \\ []) do
        GenServer.start_link(__MODULE__, state, opts)
      end

      defp send_command(port, cmd) do
        :erlang.port_command(port, Poison.encode!(cmd))
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

      # Handle data message
      # response -> return back out to waiting callers
      # call -> initiate Elixir Task
      def handle_info({port, {:data, msg}}, %{port: port}=state) do
        case Poison.decode!(msg) do
          %{"type" => "response", "counter" => counter, "response" => response } ->
            GenServer.reply Map.get(state.waiting, counter), response
            {:noreply, %{state | waiting: Map.delete(state.waiting, counter)}}
          %{"type" => "call", "name" => name, "args" => args, "counter" => counter} ->
            %Task{ref: ref} = Task.async(__MODULE__, String.to_atom(name), args)
            {:noreply, %{state | tasks: Map.put(state.tasks, ref, counter)}}
        end
      end
      def handle_info({port, {:exit_status, _status}}, %{port: port}=state) do
        exit(:child_died)
      end
      # Handle returning Task initiated by JS
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
      # Other
      def handle_info(msg, state) do
        {:noreply, state}
      end

      # Clean up port on terminate
      def terminate(:child_died, _state) do end
      def terminate(_reason, %{port: port}=state) do
        :erlang.port_close(port)
      end
    end
  end

  @injected_functions %{terminate: true,
                        start_link: true,
                        handle_info: true,
                        send_command: true,
                        handle_cast: true,
                        start_link: true,
                        init: true,
                        code_change: true,
                        handle_call: true}

  defmacro __before_compile__(env) do
    fns = Enum.reduce(Module.definitions_in(env.module), HashSet.new, fn ({k, _}, set) ->
      if @injected_functions[k] do
        set
      else
        Set.put(set, k)
      end
    end)
    |> Enum.to_list
    |> Enum.map(&to_string(&1))
    |> Macro.escape

    quote location: :keep do
      def init(state) do
        module_path = Path.join([__DIR__, @js_path])
        cmd = 'node #{:code.priv_dir(:js_gen_server)}/genserver.js #{module_path}'
        port = :erlang.open_port({:spawn, cmd}, [{:packet, 4}, :nouse_stdio, :exit_status])

        send_command(port, %{type: "init", state: state, fns: unquote(fns)})

        {:ok, %{port: port,
                waiting: %{},
                counter: 1,
                tasks: %{}}}
      end
    end
  end
end
