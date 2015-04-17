
var ddd = {};
	
var ws_initialize = function(WebSocketURL) {

    ddd = {
	pengine: undefined,

	currentTool: "oval",

	connection: undefined,

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},
	
	openWebSocket: function() {
	    connection = new WebSocket("ws://"+
				window.location.host+WebSocketURL,
			     ['echo']);

	    connection.onerror = function (error) {
                  console.log('WebSocket Error ' + error);
              };

			  /* you probably want to change this */
        connection.onmessage = function (e) {
			console.log(e.data);
			var data = eval(e.data);
	      };
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };

    ddd.openWebSocket();
};

