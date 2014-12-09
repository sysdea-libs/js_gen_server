# Webpack Watcher

This simple server allows to easily add webpack compilers into the supervision tree, with some basic logging on compile to :info.

```javascript
// webpack_watcher.js
var webpack = require('webpack');
var path = require('path');

function Server(state) {
  this.state = state;

  process.chdir(path.dirname(state.config_path));

  this.state.config = require(state.config_path);

  this.compiler = webpack(this.state.config);

  this.compiler.watch(200, function (err, stats) {
    this.log.info(stats.toString({
      hash: false,
      version: false,
      assets: true,
      chunks: false
    }));
  }.bind(this));
};

module.exports = Server;
```

```elixir
JSGenServer.start_link([Path.join([__DIR__, "../priv/webpack_watcher.js"]),
                       %{config_path: Path.join([__DIR__, "../priv/webpack.config.js"])}],
                       [id: :webpack_compiler])
```
