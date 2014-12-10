# React Rendering

```javascript
// react_renderer.js
var React = require('react');

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
  this.log.debug("Rendering React component " + args[0]);
  var component = React.createElement(components[args[0]], args[1]);
  return React.renderToString(component);
};
```

```elixir
defmodule ReactRenderer do
  use JSGenServer, path: "../priv/react_renderer.js"
end

{:ok, _} = ReactRenderer.start_link(%{}, name: ReactRenderServer)

GenServer.call(ReactRenderServer, {"component", %{user: %{forename: "Chris"}}})
```
