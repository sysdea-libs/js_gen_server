defmodule JSGenServerTest.TestServer do
  use JSGenServer, path: "/fixtures/adder.js"

  def multiply(a, b) do
    a * b
  end
end

defmodule JSGenServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = JSGenServerTest.TestServer.start_link(%{val: 10}, name: Adder)
    {:ok, [pid: pid]}
  end

  test "basic functionality", %{pid: pid} do
    assert GenServer.call(pid, {:add, Enum.to_list(1..4)}) == 10
    assert GenServer.call(pid, {:twoarg, 6, 7}) == 42
    assert GenServer.call(pid, {:async, 5}) == 15
  end

  test "named servers" do
    assert GenServer.call(Adder, {:add, Enum.to_list(1..4)}) == 10
  end

  test "external calls" do
    assert GenServer.call(Adder, {:call, 6, 9}) == 54
    assert GenServer.call(Adder, {:call, 7, 9}) == 63
    assert GenServer.call(Adder, {:call, 8, 9}) == 72
  end

  test "casts", %{pid: pid} do
    assert GenServer.call(pid, {:async, 5}) == 15
    assert GenServer.cast(pid, {:add_state, 5})
    assert GenServer.call(pid, {:async, 5}) == 20
  end

  test "timeouts", %{pid: pid} do
    assert {:timeout, {GenServer, :call, [^pid, {:timeout, 500}, 100]}}
           = catch_exit(GenServer.call(pid, {:timeout, 500}, 100))
  end
end
