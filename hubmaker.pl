:- module(hubmaker, [make_hub/1,
		     broadcast/2,
		     current_visitor/2,
		     hub_script//0]).
/** <module> API for making hubs easily
 * To use:
 *
 * 1. Make a hub with make_hub/1, typically once
 * per hub at program start
 *
 * 2. the websocket is now at /hub/Name
 *
 * 3. Define a message handler by making a clause of
 *    hubmaker:message_handler/3 with the room name
 *    as first arg.
 *
 * 4. Optionally define a visitor_handler/5 to intercept leaving and
 * arriving visitors
 *
 * 5.
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/websocket)).
:- use_module(library(http/html_write)).
:- use_module(library(debug)).
:- use_module(library(http/json)).
:- use_module(library(http/hub)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/js_write)).
:- use_module(library(dcg/basics)).


:- use_module(diagrammer).

:- debug(websocket).

:-
   current_prolog_flag(version, X),
   (   X >= 70135
   ->  true
   ;   writeln('You must use SWI-Prolog version >= 7.1.35')
   ).

http:location(hub, root(hub), []).

%%	make_hub(+Name:atom) is det
%
%        starts up a hub
%
% The server must be running

make_hub(Name) :-
	hub_create(Name, Room, _{}),
	thread_create(hub_loop(Room), _, [alias(Name)]),
	atom_concat(Name, '_websocket', WebsocketName),
	http_handler(hub(Name),
		http_upgrade_to_websocket(
		    accept_chat(Name),
		    [ guarded(false),   % see below
		      subprotocols([echo])
		    ]),
		[ id(WebsocketName)
		]).

%	Normally,  the  goal  called    by   http_upgrade_to_websocket/3
%	processes all communication with the   websocket in a read/write
%	loop. In this case however,  we tell http_upgrade_to_websocket/3
%	that we will take responsibility for the
%	websocket (guarded(false) )and we hand it to the chat room.

%%	accept_chat(+Hub:atom, +WebSocket) is det.
%
%	@param Hub is the name of the chatroom
%	@param WebSocket is the websocket
%
%       Add a user to the hub

accept_chat(Hub, WebSocket) :-
	hub_add(Hub, WebSocket, _Id).

%%	hub_loop(+Room)
%
%	Realise the hubs main loop: listen  for an event, update the
%	state and possibly broadcast status updates.

hub_loop(Room) :-
	thread_get_message(Room.queues.event, Message),
	handle_message(Message, Room),
	hub_loop(Room).


		 /*******************************
		 *         Handle messages      *

		 *******************************/

:- multifile message_handler/3.
%%	message_handler(+Name:atom, +Room:dict, +Message:dict)
%
%   define a clause for this that matches hub name to handle messages
%   typically one does something like
%   websocket{opcode:text, data:String} :< Message,
%   read_term_from_atom(String, Term, [])
%
%   to get a prolog term, then
%	broadcast(Room.name, Message.put(data, SomeAtom))
%
%   to broadcast a change, or sometimes hub_send

:- dynamic
	visitor/2.			% joined visitors

:- multifile visitor_handler/4.

%%	visitor_handler(+Name:atom, +Status:atom, +ID:text, Message:dict) is semidet
%
%	Name is the hub name
%	Status is joined or left
%	ID is the ID of the user
%	Message is used to broadcast
%
%	multifile predicate, define a clause to handle
%	visitors arriving and leaving
%
%   often it's appropriate to send new joiners a message with the state
%	hub_send(Id, text(Update)).


%%	handle_message(+Message, +Room) is det
%
%	Handed a dict with a hub message, and a
%	dict that defines the Room, handles the message.
%
handle_message(Message, Room) :-
	websocket{opcode:text, data:_} :< Message,
	message_handler(Room.name, Room, Message).

% someone joined
% we are sending all in one hub_send here, but it's not clear that
% we have to. I did this while looking for a thread starve issue, and
% it's better architecture, so I'm leaving it.
handle_message(Message, Room) :-
	hub{joined:Id} :< Message, !,
	assertz(visitor(Room.name, Id)),
	ignore(visitor_handler(Room.name, joined, Id, Message)).
handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !,
	ignore(visitor_handler(Room.name, joined, Id, Message)),
	retractall(visitor(Room.name, Id)).
handle_message(Message, _Room) :-
	debug(chat, 'Ignoring message ~p', [Message]).


%%	broadcast(+Name:atom, +Message:dict) is det
%
% Send a message to everyone in room
%
% typically this would happen in message_handler, and be somthing like
% broadcast(Room.name, Message.put(data, SomeAtom))
broadcast(Name, Message) :-
	hub_broadcast(Name, Message).

%%	current_visitor(+Room:atom, -ID:atom) is nondet
%
%	binds to all current id's in the room
%
current_visitor(Room, ID) :-
	visitor(Room, ID).

		 /*******************************
		 *   Utilities to make making pages
		 *   that connect to hubs easier
		 *******************************/

:- html_resource(js('hub.js'),[requires(js('jquery-2.0.3.min.js'))]).

%%	hub_script(+Room:atom)//
%
%	Generate the JavaScript  that  establishes   the  websocket  and
%	handles events on the websocket for this room

hub_script(Room) -->
	html_requires(js('hub.js')),
	{ atom_concat(Room, '_websocket', WebsocketName),
	  http_link_to_id(WebsocketName, [], WebSocketURL)
	},
	js_script({|javascript(WebSocketURL)||
$(document).ready(function() {
    ws_initialize(WebSocketURL);
});
		  |}).

