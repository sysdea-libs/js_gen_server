# React Rendering

```javascript
// react_renderer.js

var MyComponent = React.createClass({
  render: function () {
    return React.createElement('div', null, "Hello ", this.props.user.forename, "!");
  }
});

var templates = {
  component: MyComponent
};

function Server(state) {
  this.state = state;
};
// Convert ["component", props] to React.createElement(MyComponent, props), then render
Server.prototype.handle_call = function (args, cb) {
  var component = React.createElement(components[args[0]], args[1]);
  return React.renderToString(component);
};
```

```elixir
{:ok, _} = JSGenServer.start_link([Path.join([__DIR__, "../priv/react_renderer.js"]), %{}],
                                  name: ReactRenderer)

GenServer.call(ReactRenderer, {:component, %{user: %{forename: "Chris"}}})
```
