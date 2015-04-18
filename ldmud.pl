:- module(ldmud, [get_state/2,
		  tell_mud/3]).
/** <module> Ludum Dare MUD
 *
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

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
	format(string(Javascript),
	       'playerid = ~w; alert(\'you called get_state\');', [Player]).
get_state(ID, Javascript) :-
	ID \= new,
	gensym(player, Player),  % punt and give them a new player
	format(string(Javascript),
	       'playerid = \'~w\'; alert(\'we dont support page reload yet\');',
	       [Player]).

%%	tell_mud(+ID:atom, +Bot:atom, +Message:string, -Reply:string) is
%	det
%
%	Initiate a volley with the MUD
%
%	@param ID  the player ID
%	@param Message   what the user typed in
%	@param Reply    The mud's reply, javascript to execute
tell_mud(ID, Message, Reply) :-
	format(string(Reply), 'alert(\'mud replied to ~w, who said ~w\');',
	       [ID, Message]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(ldmud:get_state(_, _)).
sandbox:safe_primitive(ldmud:tell_mud(_, _, _)).


