# Webpack Watcher

This simple server allows to easily add webpack compilers into the supervision tree, with some basic logging on compile to :info.

```javascript
// webpack_watcher.js
var webpack = require('webpack');
var path = require('path');

function Server(state, module) {
  process.chdir(path.dirname(state.config_path));

  var config = require(state.config_path);

  var compiler = webpack(this.state.config);

  compiler.watch(200, function (err, stats) {
    module.info(stats.toString({
      hash: false,
      version: false,
      assets: true,
      chunks: false
    }));
  });
};

module.exports = Server;
```

```elixir
defmodule WebpackWatcher do
  use JSGenServer, path: "../priv/webpack_watcher.js"
  require Logger

  def info(message) do
    Logger.info(message)
  end
end

WebpackWatcher.start_link(%{config_path: Path.join([__DIR__, "../priv/webpack.config.js"])})
```
