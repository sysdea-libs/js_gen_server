# JSGenServer

Simple Port wrapper for exposing JavaScript functions to Elixir. Arguments passed are simply encoded and decoded in JSON using [Poison](https://github.com/devinus/poison). (Mostly a placeholder as there are much more efficient transfer encodings that could be used.)

## Status

Early prototype/proof of concept.

## Example

```javascript
// adder.js

var functions = {
  add: function (args) {
    return args.reduce(function (a, b) { return a + b });
  },
  twoarg: function (a, b) {
    return a * b;
  },
  async: function (a, cb) {
    var state = this;
    setTimeout(function () {
      cb(a + state.val);
    }, 100);
  }
};

function Server(state) {
  this.state = state;
};
// Convert ["fn", 4, 5] to functions.fn.apply(state, [4, 5, from])
Server.prototype.handle_call = function (args, from) {
  return functions[args[0]].apply(this.state, args.slice(1).concat([from]));
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
