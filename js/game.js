var playerId = -1;
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

  // sample setup
  say("You", "So, will you join us?");
  say("M.Martin", "Yes, i will fight and die for France!");
  eventResult("(Martin takes a gun)");

  addAction("give_gun", "Give gun to Martin");
  addAction("goto_8th", "Go to 8th Arondissment");

  addInventory("Notebook");

  if (Math.random() > 0.5) {
    setLocation("test_paris1");
  } else {
    setLocation("test_paris2");
  }

  if (Math.random() > 0.5) {
    setBot("test_bot1");
  } else {
    setBot("test_bot2");
  }
});

function sendChat(botName, chatLine) {
  pengine.ask(
    "tell_bot(" + playerId + ",\"" + botName+ "\",\"" + said + "\", X)",
    { template:'X' });
}

var pengine = new Pengine({
  oncreate: function() {
    pengine.ask("get_state(" + playerId + ", X)", { template:'X' })
  },
  onsuccess: function() {
    alert(this.data);
    eval(this.data);
  },
  onerror: function() {
    alert(this.data);
  } 
});

function say(who, what) {
  var chatLine = $("<p class=\"chat\"></p>")
    .append($("<span class=\"who\"></span>").text(who + ":"))
    .append($("<pre class=\"message\"/>").text(what));
  $("#npcMessages").append(chatLine);
}

function eventResult(eventText) {
  var eventLine = $("<p class=\"event\"></p>").text(eventText);
  $("#npcMessages").append(eventLine);
}

function addAction(name, text) {
  var actionBtn = $("<p class=\"action\" />").text(text);
  actionBtn.click(function(e) {
    // TODO: tell_mud
    alert("Action: " + name);
  });
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

function addInventory(obj) {
  $("#invEmpty").hide();
  $("#invList").append($("<li />").text(obj));
}

