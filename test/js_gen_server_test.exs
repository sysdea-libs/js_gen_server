defmodule JSGenServerTest do
  use ExUnit.Case

  setup do
    {:ok, pid} = JSGenServer.start_link(Path.join([__DIR__, "/fixtures/adder.js"]))
    {:ok, [pid: pid]}
  end

  test "basic functionality", %{pid: pid} do
    assert JSGenServer.call(pid, {:add, Enum.to_list(1..4)}) == 10
    assert JSGenServer.call(pid, {:twoarg, 6, 7}) == 42
  end
end
