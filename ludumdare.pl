:- module(strangeloop, [strangeloop_server/0]).
/** <module> Main predicates for the demo server for strangeloop 2013

This module contains the main predicates for the sample server.
It loads but does not start the server.

Start the server by querying strangeloop_server/0 - although
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

%%	strangeloop_server is det
%
%	Start the strangeloop server on 7777
%
strangeloop_server :- strangeloop_server(7777).

%%	strangeloop_server(+Port:int) is det
%
%	Start the strangeloop server on the specified port
%
%	@arg Port the port number to start on
%
strangeloop_server(Port) :-
	format('Starting strangeloop server on ~w', [Port]),
         http_server(http_dispatch, [port(Port)]).
% The magic happens on the line above. It starts the server
% and passes it the callback that dispatches requests to handlers





