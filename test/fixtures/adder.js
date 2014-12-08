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
