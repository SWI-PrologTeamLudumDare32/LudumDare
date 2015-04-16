:- module(hellopengine, [factorial/2, my_unsafe/1]).
/** <module> Simple hello world module demonstrating pengines
 *
 * this lets a remote program or web page call factorial/2
 *
 *  See bottom of load.pl as well, thats where this module is imported
 *  into the sandbox.
 */

:- use_module(library(pengines)).
:- use_module(library(sandbox)).

:- use_module(library(clpfd)).

factorial(X, _) :- X < 1, !, fail.
factorial(1, 1).
factorial(X, FactX) :-
	FactX #= X * NFactX,
	NX is X - 1,
	factorial(NX, NFactX).

my_unsafe(X) :-
	atom_length(X, Len),
	Len < 25,
	open('foo.txt', write, Stream),
	format(Stream, 'Hello Out There ~w~n', [X]),
	close(Stream).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(hellopengine:my_unsafe(_)).
