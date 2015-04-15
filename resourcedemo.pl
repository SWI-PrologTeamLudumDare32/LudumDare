:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).

% html generation and mailman
:- use_module(library(http/html_write)).
% html_resource
:- use_module(library(http/html_head)).

:- http_handler(root(resourcedemo), a_handler, []).

%%	a_handler(+Request:request) is det
%
%  when a section of html code needs a css or js resource
%  directly including it has many problems
%
%       * the resource can be included when it's not needed
%
%       * the resource can be included multiple times
%
%       * the resource can overlap another resource
%
%       The solution is a separate system for including resiources.
%       html_requires//1 knows how to generate the meta tags to include
%	css and js, and does some reasoning to include an aggregate when
%	rather than several small files when most of the aggregate is
%	needed
%
a_handler(_Request) :-
	reply_html_page(
	    [title('a page with style')],
	    [h1('A Page That Glitters'),
	     \html_requires(files('specialstyle.css')),
	     p('This para is green')]).

%
% define that specialstyle.css needs gradstyle.css
%
:- html_resource(files('specialstyle.css'), [requires(files('gradstyle.css'))]).
