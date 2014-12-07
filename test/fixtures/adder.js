module.exports = {
  add: function (args, cb) {
    cb(args.reduce(function (a, b) { return a + b }));
  },
  twoarg: function (a, b, cb) {
    cb(a * b);
  }
};
