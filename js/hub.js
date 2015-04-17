
		  
var ddd = {};
	
var ws_initialize = function(WebSocketURL, MessageHandler) {

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
        connection.onmessage = MessageHandler;
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };

    ddd.openWebSocket();
};



