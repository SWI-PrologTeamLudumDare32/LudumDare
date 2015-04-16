:- module(ludumdare, [ludum_server/0]).
/** <module> Main predicates for the demo server for ludumdare 32

This module contains the main predicates for the server.
It loads but does not start the server.

Start the server by querying ludumdare_server/0 - although
the usual way to start the server is by consulting debug.pl (dev
version) or load.pl (production).

@author Anne Ogborn
@license lgpl
@version 0.9.0

*/

% load the multi-threaded http server
:- use_module(library(http/thread_httpd)).
% and the standard handler dispatcher
:- use_module(library(http/http_dispatch)).
:- use_module(library(settings)).

:- load_settings('settings.db').

:- setting(production_port, integer, 7777, 'Port to run server on').

%%	ludumdare_server is det
%
%	Start the ludumdare server on 7777
%
ludum_server :-
	setting(production_port, Port),
	ludumdare_server(Port).

%%	ludumdare_server(+Port:int) is det
%
%	Start the ludumdare server on the specified port
%
%	@arg Port the port number to start on
%
ludumdare_server(Port) :-
	format('Starting ludumdare server on ~w', [Port]),
         http_server(http_dispatch, [port(Port)]).
% The magic happens on the line above. It starts the server
% and passes it the callback that dispatches requests to handlers





