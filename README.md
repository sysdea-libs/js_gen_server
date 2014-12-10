# JSGenServer

Simple Port wrapper for exposing JavaScript functions to Elixir. Arguments passed are simply encoded and decoded in JSON using [Poison](https://github.com/devinus/poison). (Mostly a placeholder as there are much more efficient transfer encodings that could be used.)

## Status

Early prototype/proof of concept.

## Example

```javascript
// adder.js

function Server(state, module) {
  this.state = state;
  this.module = module;
};
Server.prototype.add = function (args) {
  return args.reduce(function (a, b) { return a + b });
};
Server.prototype.twoarg = function (a, b) {
  return a * b;
};
Server.prototype.async = function (a, cb) {
  setTimeout(function () {
    cb(a + this.state.val);
  }.bind(this), 100);
};

module.exports = Server;
```

```elixir
defmodule AdderServer do
  use JSGenServer, path: "/adder.js"
end

{:ok, pid} = AdderServer.start_link(%{val: 10}, name: Adder)

GenServer.call(pid, {:add, Enum.to_list(1..4)})
# => 10
GenServer.call(pid, {:twoarg, 6, 7})
# => 42
GenServer.call(Adder, {:async, 5})
# => 15
```

# TODO

- [ ] Failure handling
- [ ] More efficient transfer encoding
- [ ] Automatically pipe stdout/stderr from child into log?
