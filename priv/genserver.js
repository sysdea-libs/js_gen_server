var fs = require('fs');

var Promise = function () {
  this.callbacks = [];
  this.resolved = false;
  this.value = null;
}
Promise.prototype.then = function (cb) {
  if (this.resolved) {
    cb(this.value);
    return this;
  } else {
    this.callbacks.push(cb);
    return this;
  }
};
Promise.prototype.resolve = function (v) {
  if (this.resolved) {
    throw "Already Resolved";
  }
  this.resolved = true;
  this.callbacks.forEach(function (cb) {
    cb(v);
  });
  return this;
};

var Module = function (server, fns) {
  fns.forEach(function (fn) {
    this[fn] = function () {
      var args = Array.prototype.slice.call(arguments);
      return server.call(fn, args);
    };
  }.bind(this));
};
Module.prototype.error = function (message) {
  this.server.sendMessage({type: "log", level: "error", message: message});
};
Module.prototype.warn = function (message) {
  this.server.sendMessage({type: "log", level: "warn", message: message});
};
Module.prototype.debug = function (message) {
  this.server.sendMessage({type: "log", level: "debug", message: message});
};
Module.prototype.info = function (message) {
  this.server.sendMessage({type: "log", level: "info", message: message});
};

function JSGenServer(cls) {
  var me = this;

  me.istream = fs.createReadStream(null, {fd: 3});
  me.ostream = fs.createWriteStream(null, {fd: 4});

  me.buffer = new Buffer([]);
  me.cls = cls;
  me.waiting = {};
  me.counter = 1;

  me.istream.on('readable', function () {
    var chunk = me.istream.read();
    if (chunk) {
      me.buffer = Buffer.concat([me.buffer, chunk]);
      me.try_read();
    }
  });

  me.istream.on('end', function () {
    process.exit();
  });
};
JSGenServer.prototype.try_read = function () {
  if (this.buffer.length < 4) return;
  var len = this.buffer.readInt32BE(0);
  if (this.buffer.length >= 4 + len) {
    this.handleMessage(this.buffer.slice(4, 4 + len));
    this.buffer = this.buffer.slice(4+len);
    this.try_read();
  }
};
JSGenServer.prototype.sendMessage = function (json) {
  var response = JSON.stringify(json);

  var utfresp = new Buffer(response, 'utf-8');

  var len = new Buffer(4);
  len.writeInt32BE(utfresp.length, 0);

  this.ostream.write(Buffer.concat([len, utfresp]));
};
JSGenServer.prototype.call = function (name, args, cb) {
  var counter = ++this.counter;
  var promise = new Promise();
  this.waiting[counter] = promise;

  this.sendMessage({ type: "call",
                     name: name,
                     args: args,
                     counter: counter });

  return promise;
};

var empty_fn = function () {};
empty_fn.NOOP = true;
JSGenServer.prototype.handleMessage = function (buf) {
  var message = JSON.parse(buf.toString('utf-8'));

  switch (message.type) {
    case "init":
      this.handler = new this.cls(message.state, new Module(this, message.fns));
      break;
    case "call":
      var cb = function (result) {
        this.sendMessage({ type: "response",
                           counter: message.counter,
                           response: result });
      }.bind(this);

      if (this.handler.handle_call) {
        var resp = this.handler.handle_call(message.arg, cb);
      } else {
        var args = message.arg;
        var fn = this.handler[args[0]];
        if (!fn) return; // todo: error propagation
        var resp = fn.apply(this.handler, args.slice(1).concat([cb]));
      }

      if (resp != undefined) cb(resp);

      break;
    case "cast":
      if (this.handler.handle_cast) {
        this.handler.handle_cast(message.arg);
      } else {
        var args = message.arg;
        var fn = this.handler[args[0]];
        if (!fn) return; // todo: error propagation
        fn.apply(this.handler, args.slice(1).concat([empty_fn]));
      }
      break;
    case "response":
      if (!this.waiting[message.counter]) return;
      this.waiting[message.counter].resolve(message.response);
      delete this.waiting[message.counter];
      break;
  }
};

new JSGenServer(require(process.argv[2]));
