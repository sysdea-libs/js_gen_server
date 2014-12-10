function Server(state, module) {
  this.state = state;
  this.module = module;
};
Server.prototype.add = function (args) {
  return args.reduce(function (a, b) { return a + b });
};
Server.prototype.twoarg = function (a, b) {
  return a * b;
};
Server.prototype.async = function (a, cb) {
  setTimeout(function () {
    cb(a + this.state.val);
  }.bind(this), 100);
};
Server.prototype.add_state = function (x) {
  this.state.val += x;
};
Server.prototype.timeout = function (x) {
  setTimeout(function () {
    cb(x);
  }, x);
};
Server.prototype.call = function (a, b, cb) {
  this.module.call('multiply', [a, b], function (c) {
    cb(c);
  });
};

module.exports = Server;
