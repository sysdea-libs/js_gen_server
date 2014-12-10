var fs = require('fs');

function JSGenServer(cls) {
  var me = this;

  me.istream = fs.createReadStream(null, {fd: 3});
  me.ostream = fs.createWriteStream(null, {fd: 4});

  me.buffer = new Buffer([]);
  me.cls = cls;
  me.waiting = {};
  me.counter = 1;

  cls.prototype.log = {
    error: function (message) {
      me.sendMessage({type: "log", level: "error", message: message});
    },
    warn: function (message) {
      me.sendMessage({type: "log", level: "warn", message: message});
    },
    debug: function (message) {
      me.sendMessage({type: "log", level: "debug", message: message});
    },
    info: function (message) {
      me.sendMessage({type: "log", level: "info", message: message});
    }
  };

  cls.prototype.call = this.call.bind(this);

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
  this.waiting[counter] = cb;

  this.sendMessage({ type: "call",
                     name: name,
                     args: args,
                     counter: counter });
};
JSGenServer.prototype.handleMessage = function (buf) {
  var message = JSON.parse(buf.toString('utf-8'));

  switch (message.type) {
    case "init":
      this.handler = new this.cls(message.state, this.logger);
      break;
    case "call":
      if (!this.handler.handle_call) return;
      var cb = function (result) {
        this.sendMessage({ type: "response",
                           counter: message.counter,
                           response: result });
      }.bind(this);

      var resp = this.handler.handle_call(message.arg, cb);
      if (resp != undefined) {
        cb(resp);
      }

      break;
    case "cast":
      if (!this.handler.handle_cast) return;
      this.handler.handle_cast(message.arg);
      break;
    case "response":
      if (!this.waiting[message.counter]) return;
      this.waiting[message.counter].call(null, message.response);
      delete this.waiting[message.counter];
      break;
  }
};

new JSGenServer(require(process.argv[2]));
