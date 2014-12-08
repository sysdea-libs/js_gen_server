defmodule JSGenServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = JSGenServer.start_link(
      Path.join([__DIR__, "/fixtures/adder.js"]), %{val: 10}, name: Adder)
    {:ok, [pid: pid]}
  end

  test "basic functionality", %{pid: pid} do
    assert JSGenServer.call(pid, {:add, Enum.to_list(1..4)}) == 10
    assert JSGenServer.call(pid, {:twoarg, 6, 7}) == 42
    assert JSGenServer.call(pid, {:async, 5}) == 15
  end

  test "named servers" do
    assert JSGenServer.call(Adder, {:add, Enum.to_list(1..4)}) == 10
  end

  test "casts", %{pid: pid} do
    assert JSGenServer.call(pid, {:async, 5}) == 15
    assert JSGenServer.cast(pid, {:add_state, 5})
    assert JSGenServer.call(pid, {:async, 5}) == 20
  end

  test "timeouts", %{pid: pid} do
    assert {:timeout, {GenServer, :call, [pid, [:timeout, 500], 100]}}
           = catch_exit(JSGenServer.call(pid, {:timeout, 500}, 100))
  end
end
