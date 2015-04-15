:- module(static_handlers, []).
/** <module> handle static files

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

% we'll need to fiddle with abstract locations
:- use_module(library(http/http_path)).

% and we need file support
:- use_module(library(http/http_files)).
:- use_module(library(http/http_server_files)).

%
% In a real application, we'd do this in strangeloop.pl
%
% Add an abstract URI path root to serve files from
%
% this is NOT a file location, it's an URI path
http:location(files, '/f', []).

%
% Now we want an abstract *file path* - not a URI, but
% an abstract name for a location, as when we say
% use_module(library(blah)).
%
user:file_search_path(static_files, assets).

% and now we're ready to serve files in a directory
:- http_handler(files(.),    % abstract name of URI path
		serve_files_in_directory(static_files),
		[prefix]).   % prefix option on handler so we handle every
			     % URI which starts /f/
