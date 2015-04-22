:- module(ldmud, [get_state/2,
		  tell_mud/3]).
/** <module> Ludum Dare MUD
 *
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(chatscript).
:- use_module(nanisearch).

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
	      [Player, antonette]),
	start_conversation(Player, antonette, Said),
	with_output_to(string(MudInit), nani_server(Player)),
	uri_encoded(query_value, MudInit, EncMudInit),
	debug(chatscript(talk),
	      '~w started by saying ~w to ~w', [antonette, Said, Player]),
	format(string(Javascript),
	       'playerid = \'~w\'; setLocation("Flat"), setBot("Antonette"); say("Action:", "~w"); say("Antonette", "~w"); addAction("goto(office)", "Go to office")',
	       [Player, EncMudInit, Said]),
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
	read_term_from_atom(Message, Command, []),
	with_output_to(string(Out), nani_volley(ID, Command)),
	uri_encoded(query_value, Out, Encoded),
	format(string(Reply), 'notify(\'~w\');',
	       [Encoded]),
	debug(ldmud(javascript), '~w', [Reply]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(ldmud:get_state(_, _)).
sandbox:safe_primitive(ldmud:tell_mud(_, _, _)).


