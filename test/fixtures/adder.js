var functions = {
  add: function (args) {
    return args.reduce(function (a, b) { return a + b });
  },
  twoarg: function (a, b) {
    return a * b;
  },
  async: function (a, cb) {
    var state = this.state;
    setTimeout(function () {
      cb(a + state.val);
    }, 100);
  },
  add_state: function (x) {
    this.state.val += x;
  },
  timeout: function (x) {
    setTimeout(function () {
      cb(x);
    }, x);
  },
  call: function (a, b, cb) {
    this.call('multiply', [a, b], function (c) {
      cb(c);
    });
  }
};

function Server(state) {
  this.state = state;
};
// Convert ["fn", 4, 5] to functions.fn.apply(state, [4, 5, cb])
Server.prototype.handle_call = function (args, cb) {
  return functions[args[0]].apply(this, args.slice(1).concat([cb]));
};
Server.prototype.handle_cast = function (args) {
  var cb = function () {};
  return functions[args[0]].apply(this, args.slice(1).concat([cb]));
};

module.exports = Server;
