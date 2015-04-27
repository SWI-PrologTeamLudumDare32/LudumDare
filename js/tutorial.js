var tutorName = "Tutor";

var oldSendChat;
var oldMudAction;

var timeouts = [];

function runTutorial() {
  overrideControls()
  clearAll();
  addAction("skip", "Skip Tutorial");
  addInventory("Codebook");
  setBot("test_bot1");
  setLocation("test_paris1");
  timeouts.push(setTimeout('say(tutorName, "Hello!")', 100));
  timeouts.push(setTimeout('say(tutorName, "We need your help to grow resistance strong.")', 1000));
  timeouts.push(setTimeout('say(tutorName, "You will have to talk to many people and convince them to join us.")', 4000));
  timeouts.push(setTimeout('say(tutorName, "Do you understand?")', 8000));
  timeouts.push(setTimeout('confirmSay()', 10000));
}

function tutorialJustSay (b, e) { 
  say("You", e);
}

function overrideControls() {
  oldSendChat = sendChat;
  oldMudAction = mudAction;
  sendChat = tutorialJustSay;
  mudAction = function(e) {
    if (e == "skip") {
      curtainsDown();
    }
  }
}

function confirmSay() {
  $("#say").showBalloon({ 
    position: "right",
    contents: '<p>Type \"Yes\" here</p>'
  });
  sendChat = function (b, e) {
    say("You", e);
    if (e.toLowerCase() == "yes") {
      sendChat = tutorialJustSay;
      $("#say").hideBalloon();
      timeouts.push(setTimeout('say(tutorName, "Good.")', 1000));
      timeouts.push(setTimeout('say(tutorName, "Sometimes you will need to perform some actions.")', 2500));
      timeouts.push(setTimeout('say(tutorName, "Please give me a secret codebook.")', 5500));
      timeouts.push(setTimeout('confirmGive()', 8000));
    }
  }
}

function confirmGive() {
  addAction("give_codebook", "Give Codebook");
  $("#actions").showBalloon({ 
    position: "right",
    contents: '<p>Select right action</p>'
  });
  mudAction = function(e) {
    if (e == "give_codebook") {
      $("#actions").hideBalloon();
      notify(tutorName + " takes codebook.");
      timeouts.push(setTimeout('say(tutorName, "Very well.")', 1000));
      timeouts.push(setTimeout('say(tutorName, "During your quest you will have to travel to different places.")', 3000));
      timeouts.push(setTimeout('say(tutorName, "Go to city center to begin your mission.")', 7000));
      timeouts.push(setTimeout('confirmGo()', 8000));
    }
  }
}

function confirmGo() {
  addAction("goto_citycenter", "Go to city center");
  $("#actions").showBalloon({ 
    position: "right",
    contents: '<p>Select right action</p>'
  });
  mudAction = function(e) {
    if (e == "goto_citycenter") {
      $("#actions").hideBalloon();
      notify("You are leaving the tutorial.");
      timeouts.push(setTimeout('say(tutorName, "Good luck!")', 100));
      timeouts.push(setTimeout('curtainsDown()', 2000));
    }
  }
}

function curtainsDown() {
  sendChat = oldSendChat;
  mudAction = oldMudAction;
  var arrayLength = timeouts.length;
  for (var i = 0; i < arrayLength; i++) {
    clearTimeout(timeouts[i]);
  }
  $("#say").hideBalloon();
  $("#actions").hideBalloon();

  $("#overlay").hide();
  $("#overlay").css("width", $("#gameSceen").width());
  $("#overlay").css("height", $("#gameSceen").height());
  $("#overlay").css({
    top: $('#gameSceen').position().top,
    left: $('#gameSceen').position().left,
    position:'absolute'});
  $("#overlay").fadeIn("slow", function() {
    curtainsUp();
  });
}

function curtainsUp() {
  clearAll();
  init();
  $("#overlay").fadeOut("slow", function() {
  });
}


