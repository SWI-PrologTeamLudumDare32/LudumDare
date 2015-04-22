% Module for communicating to UI.

:- module(ui, [make_ui_call/2]).

%% make_ui_call(+Cmds:list, -Js:string)
%
% Create a Javascript to triggering events in UI. Available commands:
% - say(Who, What)
% - wait(Time)
% - notify(Event)
% - mudAction(ActionName)
%
% @param Cmds a list of commands
% @param Js Javascript to be sent to frontend
%
make_ui_call(Cmds, Js) :-
  make_message(Cmds, JsArgs),
  format(string(Js), "processCommands(~w)", JsArgs).

% Create Javascript for a list of commands
%
make_message([H], Js) :-
  !, make_message(H, Js).

make_message([H | T], Js) :-
  make_message(H, Js1),
  make_message(T, Js2),
  format(string(Js), "~w, ~w", [Js1, Js2]).

% Create Javascript for chat event.
%
make_message(say(Who, What), Js) :-
  format(string(Js), '{ func:"say", args:["~w", "~w"] }', [Who, What]).

% Create Javascript for wait event.
%
make_message(wait(T), Js) :-
  format(string(Js), '{ func:"wait", args:["~w"] }', [T]).

% Create Javascript for generic notification event.
%
make_message(notify(Txt), Js) :-
  format(string(Js), '{ func:"notify", args:["~w"] }', [Txt]).

% Create Javascript for event triggering MUD action.
%
make_message(mud(Act), Js) :-
  format(string(Js), '{ func:"mudAction", args:["~w"] }', [Act]).
