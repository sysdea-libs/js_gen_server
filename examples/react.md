# React Rendering

```javascript
// react_renderer.js
var React = require('react');

var MainComponent = React.createClass({
  render: function () {
    return React.createElement('div', null, "Hello ", this.props.user.forename, "!");
  }
});

var components = {
  main: MainComponent
};

function Server(state, module) {
  this.state = state;
  this.module = module;
};
Server.prototype.render = function (component_name, props) {
  this.module.debug("Rendering React component " + component_name);
  var component = React.createElement(components[component_name], props);
  return React.renderToString(component);
};
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

GenServer.call(ReactRenderServer, {:render, "main", %{user: %{forename: "Chris"}}})
```
