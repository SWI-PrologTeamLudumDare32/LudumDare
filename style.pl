:- module(style, []).
/** <module> Styling for UCSD
 *
 */
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).

:- multifile
        user:body//2.

% Body will be included
user:body(ludum, Body) -->
        html(body([ div(id(top), h1('UCSD Course Planner')),
                    div(id(content), Body),
		    \html_requires(files('style.css'))
                  ])).
