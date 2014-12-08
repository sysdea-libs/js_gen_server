function JSGenServer(cls) {
  this.buffer = new Buffer([]);
  this.cls = cls;

  process.stdin.on('readable', function () {
    var chunk = process.stdin.read();
    if (chunk) {
      this.buffer = Buffer.concat([this.buffer, chunk]);
      this.try_read();
    }
  }.bind(this));

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
JSGenServer.prototype.handleMessage = function (buf) {
  var message = JSON.parse(buf.toString('utf-8'));

  var cb = function (result) {
    var response = JSON.stringify({ counter: message.counter,
                                    response: result });

    var utfresp = new Buffer(response, 'utf-8');

    var len = new Buffer(4);
    len.writeInt32BE(utfresp.length, 0);

    process.stdout.write(Buffer.concat([len, utfresp]));
  };

  if (message.type == "init") {
    this.handler = new this.cls(message.state);
  } else if (message.type == "handle_call") {
    var resp = this.handler.handle_call(message.arg, cb);
    if (resp != undefined) {
      cb(resp);
    }
  }
};

new JSGenServer(require(process.argv[2]));
