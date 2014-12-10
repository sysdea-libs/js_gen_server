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

function Server(state, module) {
  this.state = state;
  this.module = module;
};
Server.prototype.component = function (component_name, state) {
  this.module.debug("Rendering React component " + args[0]);
  var component = React.createElement(components[component_name], state);
  return React.renderToString(component);
}
```

```elixir
defmodule ReactRenderer do
  use JSGenServer, path: "../priv/react_renderer.js"
  require Logger

  def debug(message) do
    Logger.debug(message)
  end
end

{:ok, _} = ReactRenderer.start_link(%{}, name: ReactRenderServer)

GenServer.call(ReactRenderServer, {"component", %{user: %{forename: "Chris"}}})
```
