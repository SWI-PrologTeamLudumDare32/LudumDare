
var ddd = {};

function stopEvent(event){
	if(event.preventDefault != undefined)
		event.preventDefault();
	if(event.stopPropagation != undefined)
		event.stopPropagation();
}
	
var ws_initialize = function(WebSocketURL) {

    ddd = {
	pengine: undefined,

	currentTool: "oval",

	connection: undefined,

	writeln: function(string) {
		$('#output').append(string + "<br />")
	},

	unchoose_tools: function() {
		$("#diagrammer .componentbar IMG").removeClass("selected");
	},
	
	mouseDownAtX: 0,
	mouseDownAtY: 0,
	
	newElement: function(e) {
			$("#msg").text("down " + e.offsetX + " " + e.offsetY);
		//	ddd.sendChat("down(" + e.offsetX + "," + e.offsetY + ")");
			ddd.mouseDownAtX = e.offsetX;
			ddd.mouseDownAtY = e.offsetY;
			stopEvent(e);
			return false;
	},

	newElementMoveOrDrag: function(e) {
		if (mouseDownCount === 0)
			return;

		var x = e.offsetX;
		var y = e.offsetY;

		$("#msg").text("drag " + x + " " + y + " " + e.button);
	},

	newElementCommit: function(e) {
		$("#msg").text("commit " + e.offsetX + " " + e.offsetY);
		ddd.sendChat("commit(" + ddd.currentTool + ", " + 
					ddd.mouseDownAtX + ", " + ddd.mouseDownAtY + ", " +
					e.offsetX + ", " + e.offsetY + ", " + e.button + ")");
	},
	
	ctx: function() {
		return $("#diagrammer .drawarea").get(0).getContext('2d');
	},
	
	strokeOnly: function() {
        var context = ddd.ctx();
		context.lineWidth = 3;
		context.strokeStyle = 'black';
		context.stroke();	
	},
	
	strokeFill: function() {
        var context = ddd.ctx();
		context.fillStyle = 'yellow';
		context.fill();
		context.lineWidth = 3;
		context.strokeStyle = 'black';
		context.stroke();	
	},

	addRect: function(x, y) {
		console.log('rect' + x + ', ' + y);
        var context = ddd.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.rect(x-50, y-37.5, 100, 75);
		ddd.strokeFill();
	},
	
	addOval: function(x, y) {
		console.log('rect' + x + ', ' + y);
        var context = ddd.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.arc(x, y, 37.5, 0, Math.PI * 2);
		ddd.strokeFill();
	},
	
	addDiamond: function(x, y) {
		console.log('rect' + x + ', ' + y);
		var context = ddd.ctx();
        context.setTransform(1, 0, 0, 1, 0, 0);
        context.beginPath();
		context.moveTo(x-50, y);
		context.lineTo(x, y-37.5);
		context.lineTo(x+50,y);
		context.lineTo(x, y+37.5);
		context.closePath();
		ddd.strokeFill();
	},
	
	clear: function() {
		var canvas = $("#diagrammer .drawarea").get(0);
		ddd.ctx().clearRect(0, 0, canvas.width, canvas.height);
	},
	
	openWebSocket: function() {
	    connection = new WebSocket("ws://"+
				window.location.host+WebSocketURL,
			     ['echo']);

	    connection.onerror = function (error) {
                  console.log('WebSocket Error ' + error);
              };

        connection.onmessage = function (e) {
			console.log(e.data);
			var data = eval(e.data);
	      };
	},

        sendChat: function(msg) {
              connection.send(msg);
        }
    };
	
	var tools = ["rect", "oval", "diamond", "text"];
	
	function make_mouseup(name) {
		return function() {
			   var tool_name = name;
			   ddd.unchoose_tools();
			   ddd.currentTool = tool_name;
			   $("#" + tool_name + "_tool").addClass("selected");
		}
	}
	
	for(var i = 0 ; i < tools.length; i++) {
		var mouseup_fn = make_mouseup(tools[i]);
		
		$("#" + tools[i] + "_tool").on("mouseup", mouseup_fn);
	};

    $("#diagrammer .drawarea").on(
	      {	"mousedown": ddd.newElement,
		"mousemove": ddd.newElementMoveOrDrag,
		"mouseup": ddd.newElementCommit,
		"contextmenu": function(){ return false; }});

    ddd.openWebSocket();
};

var mouseDown = [0, 0, 0, 0, 0, 0, 0, 0, 0],
    mouseDownCount = 0;
	
$(document).ready( function() {
	document.body.onmousedown = function(evt) {
	  ++mouseDown[evt.button];
	  ++mouseDownCount;
	};
	document.body.onmouseup = function(evt) {
	  --mouseDown[evt.button];
	  --mouseDownCount;
	};
});
