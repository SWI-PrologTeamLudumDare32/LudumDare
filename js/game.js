$(document).ready(function() {
  $("#say").keypress(function(e){
    if(e.keyCode == 13) {
      var said = $("#say").val();
      $("#say").val("");
      say("You", said);
    }
  });
});

var pengine = new Pengine({
  oncreate: handleCreate,
  onsuccess: handleSuccess,
  onerror: handleSuccess 
});

function handleCreate () {
    pengine.ask("q(X)", {
    template:'X'
  });
}
            
function handleSuccess() {
  alert(this.data);
}

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
