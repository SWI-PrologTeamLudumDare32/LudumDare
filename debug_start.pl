:- module(debug_start, []).
/** <module> Dev mode starter file

    Consult this file to start the system in dev mode

*/

:- use_module(library(settings)).

:- load_settings('settings.db').

:- setting(debug_port, between(80, 49000), 7777, 'Port to run server on').


% Make sure we're on a reasonable version
%

check_version :- current_prolog_flag(version, X), X > 70129,!.
check_version :-
      current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
      format('OOOPS - you need swipl version 7.1.29 or better, you are on ~w.~w.~w~n',
	[Major, Minor, Patch]).

:- check_version.

%%	force_right_directory
%
%	Change the working directory to the directory this file
%	loaded from so we don't have weird relative path issues
%
force_right_directory :-
	source_file(check_version, File),
	file_directory_name(File, Dir),
	working_directory(_, Dir).

:- force_right_directory.

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

% Now we define a new clause for the *multifile* predicate
% http:location/3. Default priority is 0, we use a higher
% priority to override pldoc's default location
% We define pldoc in terms of [system defined] root.
%
http:location(pldoc, root('help/source'), [priority(10)]).

% load our application server
% We start it first
% so it doesn't collide with pldoc
:-ensure_loaded(load).

% we don't actually start the server ourselves, in dev mode
% we piggyback on the pldoc server
%
% :- strangeloop_server.

% Now we can start pldoc. This starts our application server
% as well, a workaround for one server per process
:- setting(debug_port, Port), doc_server(Port).


% Nice thing about SWI-Prolog, the interface to most
% development environment is fairly simple, so it's practical
% to set your environment for your own convenience. Here, we
% launch our first handler
:-  setting(debug_port, Port),
	format(atom(X), 'http://localhost:~w', [Port]),
	www_open_url(X).

% and bring up a module in the editor
:- edit('debug_start.pl').

% open the navigator
% put it in module user so programmer doesn't have to
% use a module name
user:open_tools :- prolog_ide(open_navigator).
