# JSGenServer

Simple Port wrapper for exposing JavaScript functions to Elixir. Arguments passed are simply encoded and decoded in JSON using [Poison](https://github.com/devinus/poison). (Mostly a placeholder as there are much more efficient transfer encodings that could be used.)

## Status

Early prototype/proof of concept.

## Example

```javascript
// adder.js

module.exports = {
  add: function (args, cb) {
    cb(args.reduce(function (a, b) { return a + b }));
  },
  twoarg: function (a, b, cb) {
    cb(a * b);
  }
};
```

```elixir
{:ok, pid} = JSGenServer.start_link(Path.join([__DIR__, "/adder.js"]))
JSGenServer.call(pid, {:add, Enum.to_list(1..4)})
# => 10
JSGenServer.call(pid, {:twoarg, 6, 7})
# => 42
```

# TODO

- [ ] Failure handling
- [ ] More efficient transfer encoding
