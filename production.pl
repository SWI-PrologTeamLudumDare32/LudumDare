:- module(production, []).
/** <module> production mode starter file

    Consult this file to start the system in production
    don't load both it and debug

*/

% Make it easier to read 'codes' style strings
:- portray_text(true).


% pldoc is SWI-Prolog's equivilent of doxygen
% pldoc is served at / by default. We need to
% move pldoc's URI path so it doesn't interfere with the main
% application. Each SWI-Prolog process has a single global
% URI space for dispatch.

% abstract URI paths allow us to disconnect the served URI path
% from the name we refer to it by. By default, pldoc('foo/bar') maps
% to /foo/bar. We'll move pldoc to /help/source, so pldoc('foo/bar')
% will serve from /help/source/foo/bar

% First we import the abstract path stuff
:- use_module(library(http/http_path)).

% load our application server
:- ensure_loaded(load).
:- use_module(strangeloop).

%
% And start that puppy
:- strangeloop_server.


