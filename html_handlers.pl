:- module(html_handlers, []).
/** <module> Handlers for Ludum Dare web pages

*/

% Needed for handler definitions
:- use_module(library(http/http_dispatch)).

% Needed to generate html
:- use_module(library(http/html_write)).

%
% this handler uses an abstract path. The absolute
% paths we've been using are not good practice
:- http_handler(/, intro_page , []).

%%	intro_page(+Request:request) is det
%
%	A simple page using the 'termerized html' form
%
intro_page(_Request) :-
	reply_html_page(
	    ucsd,
	    title('Team SWI-Prolog Ludum Dare!'),
	    div([
		 p(['Dare to Play!'])
		])).

