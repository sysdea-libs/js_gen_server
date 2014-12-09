function JSGenServer(cls) {
  var me = this;

  this.buffer = new Buffer([]);
  this.cls = cls;

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

  process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk) {
      me.buffer = Buffer.concat([me.buffer, chunk]);
      me.try_read();
    }
  });

  process.stdin.on('end', function () {
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

  process.stdout.write(Buffer.concat([len, utfresp]));
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
  }
};

new JSGenServer(require(process.argv[2]));
