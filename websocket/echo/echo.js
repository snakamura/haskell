var requester = {
    socket_: null,
    pendingText_: null,

    send: function(text) {
        var s = this.socket();
        if (s.readyState == WebSocket.OPEN)
            s.send(text);
        else
            this.pendingText_ = text;
    },

    socket: function() {
        if (!this.socket_) {
            var self = this;
            var s = new WebSocket('ws://localhost:8080/');
            s.onmessage = function(event) {
                $('#response').text(event.data);
            };
            s.onopen = $.proxy(function(event) {
                if (this.pendingText_) {
                    this.socket_.send(this.pendingText_);
                    this.pendingText_ = null;
                }
            }, this);
            s.onerror = $.proxy(function(event) {
                this.socket_ = null;
            }, this);
            s.onclose = $.proxy(function(event) {
                this.socket_ = null;
            }, this);
            this.socket_ = s;
        }
        return this.socket_;
    }
};

$('#echo').submit(function(event) {
    try {
        requester.send(event.target.message.value);
    }
    finally {
        return false;
    }
});
