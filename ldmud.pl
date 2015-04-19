:- module(ldmud, [get_state/2,
		  tell_mud/3]).
/** <module> Ludum Dare MUD
 *
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(chatscript).

%%	get_state(+ID:atom, -Javascript:string) is det
%
%	Retrieve javascript for player ID which,
%	eval'ed on the web page, sets the page for the
%	current state of the game.
%	If ID is "new" it sets the page for a new game,
%	and the Javascript will set a javascript var playerid
%	for a new player
get_state(new, Javascript) :-
	gensym(player, Player),
	debug(chatscript(conversation), 'started conversation between ~w and ~w',
	      [Player, baron]),
	start_conversation(Player, baron, Said),
	debug(chatscript(talk),
	      '~w started by saying ~w to ~w', [baron, Said, Player]),
	format(string(Javascript),
	       'playerid = \'~w\'; setLocation("Mansion"), setBot("Baron"); say("Baron", "~w"); addAction("go_park", "Go to park")', [Player, Said]),
	debug(chatscript(javascript), '~w', [Javascript]).

get_state(ID, Javascript) :-
	ID \= new,
	gensym(player, Player),  % punt and give them a new player
	debug(chatscript(conversation), 'no page reload yet, making new ~',
	      [Player]),
	format(string(Javascript),
	       'playerid = \'~w\'; alert(\'we dont support page reload yet\');',
	       [Player]),
	debug(chatscript(javascript), '~w', [Javascript]).

%%	tell_mud(+ID:atom, +Bot:atom, +Message:string, -Reply:string) is
%	det
%
%	Initiate a volley with the MUD
%
%	@param ID  the player ID
%	@param Message   what the user typed in
%	@param Reply    The mud's reply, javascript to execute
tell_mud(ID, Message, Reply) :-
<<<<<<< HEAD
	format(string(Reply), 'notify(\'mud replied to ~w, who said ~w\');',
	       [ID, Message]).
=======
	format(string(Reply), 'eventResult(\'mud replied to ~w, who said ~w\');',
	       [ID, Message]),
	debug(ldmud(javascript), '~w', [Reply]).
>>>>>>> afb7dc02c91b3c2ceb8cbd85c990500c635b1fba

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(ldmud:get_state(_, _)).
sandbox:safe_primitive(ldmud:tell_mud(_, _, _)).


