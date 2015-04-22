:- module(ldchat, [tell_bot/4]).
/** <module> Ludum Dare MUD
 *
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).
:- use_module(chatscript).
:- use_module(ui).
:- use_module(library(settings)).

:- initialization setting(ludumdare:chatscript_location, Loc),
	set_chatscript_address(Loc).

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
	talk(ID, Bot, Message, BotSaid),
	debug(chatscript(talk),
	      '~w said ~w to ~w', [ID, Message, Bot]),
	debug(chatscript(talk),
	      '~w said ~w to ~w', [Bot, BotSaid, ID]),
        make_ui_call([say("You", Message), say(Bot, BotSaid)], Reply),
	debug(chatscript(javascript),
	      '~w', [Reply]).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(ldchat:tell_bot(_, _, _, _)).



