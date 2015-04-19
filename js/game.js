var playerid = -1;
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
});

function init() {
  makeQuery("get_state(" + playerid + ", X)");

  // sample setup
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

  say("You", "So, will you join us?");
  say("M.Martin", "...");
  say("You", "So, will you join us?");
  say("M.Martin", "...");
  say("You", "So, will you join us?");
  say("M.Martin", "...");
  say("You", "So, will you join us?");
  say("M.Martin", "Yes, i will fight and die for France!");
  eventResult("(Martin takes a gun)");
}

function sendChat(botName, chatLine) {
  makeQuery("tell_bot(" + playerid + ",\"" + botName+ "\",\"" + chatLine + "\", X)");
}

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

function say(who, what) {
  var chatLine = $("<p class=\"chat\"></p>")
    .append($("<span class=\"who\"></span>").text(who + ":"))
    .append($("<pre class=\"message\"/>").text("-").typed({
        strings: [what],
        typeSpeed: 0
      }));
  $("#npcMessages").append(chatLine);
  $('#npcMessages').scrollTop($('#npcMessages').prop("scrollHeight"));  
}

function eventResult(eventText) {
  var eventLine = $("<p class=\"event\"></p>").text(eventText);
  $("#npcMessages").append(eventLine);
  $('#npcMessages').scrollTop($('#npcMessages').prop("scrollHeight"));  
}

function mudAction(name) {
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

function addInventory(obj) {
  $("#invEmpty").hide();
  $("#invList").append($("<li />").text(obj));
}

function clearAll() {
  $("#npcMessages").empty();
  $("#actions > p").remove();
  $("#invList").empty();
}

