var playerid = "new";
var botName = "None";
var locationName = "Void";

$(document).ready(function() {
  $("#say").keypress(function(e){
    if(e.keyCode == 13) {
      var said = $("#say").val();
      said = said.replace(/"/g,"'");
      $("#say").val("");
      sendChat(botName, said);
    }
  });

  $("#npcAvatar").click(function(e) { $("#say").focus(); });
  $("#npcBackground").click(function(e) { $("#say").focus(); });

  setTimeout("runTutorial()", 100);
  //setTimeout("endGame('You was captured by Nazi.')", 100);
});

// Inits a game
function init() {
  makeQuery("get_state(" + playerid + ", X)");
}

// Sends chat line to server
function sendChat(botName, chatLine) {
  makeQuery("tell_bot(" + playerid + ",\"" + botName+ "\",\"" + chatLine + "\", X)");
}

// Makes a Pengine call to server
function makeQuery(goal) {
  return new Pengine({
    ask: goal,
    template: "X",
    onsuccess: function() {
      var js = this.data[0].functor;
      eval(js);
    },
    onerror: function() {
      alert(this.data);
    },
    ondestroy: function() { }
  })
}

// Adds a chat line to NPC message box
function say(who, what, callbackFunc) {
  callbackFunc = typeof callbackFunc !== 'undefined' ? callbackFunc : function() { };

  var chatLine = $("<p class=\"chat\"></p>")
    .append($("<span class=\"who\"></span>").text(who + ":"))
    .append($("<pre class=\"message\"/>").text("-").typed({
        strings: [what],
        callback: callbackFunc,
        typeSpeed: 0
      }));
  $("#npcMessages").append(chatLine);
  $('#npcMessages').scrollTop($('#npcMessages').prop("scrollHeight"));  
}

function wait(time, callbackFunc) {
  if (typeof callbackFunc != 'undefined') {
    setTimeout(callbackFunc, time);
  }
}

// Processes commands. Command is eg.
//   {func:"say", args:["Bot", "Hello"]}
function processCommands(commands, i) {
  i = typeof i !== 'undefined' ? i : 0;
  if (i < commands.length) {
    var args = commands[i].args.slice(0);
    var funcName = commands[i].func;
    args.push(function() { processCommands(commands, i + 1); });
    window[funcName].apply(undefined, args);
  }
}

function notify(eventText, callbackFunc) {
  var eventLine = $("<p class=\"event\"></p>").text(eventText);
  $("#npcMessages").append(eventLine);
  $('#npcMessages').scrollTop($('#npcMessages').prop("scrollHeight"));  

  if (typeof callbackFunc != 'undefined') {
    callbackFunc();
  }
}

// Callback func is not used since it shold be last command in sequence.
function mudAction(name, callbackFunc) {
  makeQuery("tell_mud(" + playerid + ",\"" + name + "\", X)");
}

function addAction(name, text) {
  var actionBtn = $("<p class=\"action\" />").text(text);
  actionBtn.click(function(e) { mudAction(name); });
  $("#actions").append(actionBtn);
}

function setLocation(locName) {
  locationName = locName;
  $("#npcBackground").css("background-image", "url(/img/loc/" + locName + ".png)");
}

function setBot(btName) {
  botName = btName;
  if (btName != "None") {
    $("#npcAvatar").css("background-image", "url(/img/bot/" + btName + ".png)");
    $("#npcAvatar").show();
  } else {
    $("#npcAvatar").hide();
  }
}

function endGame(text) {
  $("#overlay").hide();
  $("#overlay").css("width", $("#gameSceen").width());
  $("#overlay").css("height", $("#gameSceen").height());
  $("#overlay").css({
    top: $('#gameSceen').position().top,
    left: $('#gameSceen').position().left,
    position:'absolute'});
  $("#overlay").fadeIn("slow", function() {
    $("#endGameText").typed({
      strings: [text + "<br><br><br><br><br><br><br><br><br><br> <span class='appendix'>Made for Ludum Dare 32 by SWI-Prolog community.<br>Thanks for playing.</span>."],
      typeSpeed: 0
    });
  });
}

function addInventory(obj) {
  $("#invEmpty").hide();
  $("#invList").append($("<li />").text(obj));
}

function clearAll() {
  $("#npcMessages").empty();
  $("#actions > p").remove();
  $("#invList").empty();
}

