:- module(ldchat, [tell_bot/4]).
/** <module> Ludum Dare MUD
 *
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

%%	tell_bot(+ID:atom, +Bot:atom, +Message:string, -Reply:string) is
%	det
%
%	Initiate a volley with a bot
%
%	@param ID  the player ID
%	@param Bot the name of the bot
%	@Message   what the user typed in
%	@Reply    The bot's reply, javascript to execute
tell_bot(ID, Bot, Message, Reply) :-
	format(string(Reply), 'alert(\'bot ~w replied to ~w, who said ~w\');',
	       [Bot, ID, Message]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(ldchat:tell_bot(_, _, _, _)).



