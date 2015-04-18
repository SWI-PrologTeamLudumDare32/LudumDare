% NANI SEARCH - A sample adventure game

% Nani Search is designed to illustrate Prolog programming.  It
% is an implementation of the principle example used in
% this tutorial.

main:- nani_search.       % main entry point

nani_search:-
  init_dynamic_facts,     % predicates which are not compiled

  write('NANI SEARCH - A Sample Adventure Game'),nl,
  write('Copyright (C) Amzi! inc. 1990-2010'),nl,
  write('No rights reserved, use it as you wish'),nl,
  nl,
  write('Nani Search is designed to illustrate Prolog programming.'),nl,
  write('As such, it might be the simplest adventure game.  The game'),nl,
  write('is the primary example used in this tutorial.'),nl,
  write('Full source is included as well.'),nl,
  nl,
  write('Your persona as the adventurer is that of a three year'),nl,
  write('old.  The Nani is your security blanket.  It is getting'),nl,
  write('late and you''re tired, but you can''t go to sleep'),nl,
  write('without your Nani.  Your mission is to find the Nani.'),nl,
  nl,
  write('You control the game by using simple English commands'),nl,
  write('expressing the action you wish to take.  You can go to'),nl,
  write('other rooms, look at your surroundings, look in things'),nl,
  write('take things, drop things, eat things, inventory the'),nl,
  write('things you have, and turn things on and off.'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,
  write('Enjoy the hunt.'),nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(quit):-quit,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound:-
  have(nani),
  write('Congratulations, you saved the Nani.'),nl,
  write('Now you can rest secure.'),nl,nl.

quit:-
  write('Giving up?  It''s going to be a scary night'),nl,
  write('and when you get the Nani it''s not going'),nl,
  write('to smell right.'),nl,nl.

% The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   eat something         (ex. eat the apple)'),nl,
  write('   turn something on     (ex. turn on the light)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

hint:-
  write('You need to get to the cellar, and you can''t unless'),nl,
  write('you get some light.  You can''t turn on the cellar'),nl,
  write('light, but there is a flash light in the desk in the'),nl,
  write('office you might use.'),nl,nl,
  look.

% Initial facts describing the world.  Rooms and doors do not change,
% so they are compiled.

room(office).
room(kitchen).
room('dining room').
room(hall).
room(cellar).

door(office,hall).
door(hall,'dining room').
door('dining room',kitchen).
door(kitchen,cellar).
door(kitchen,office).

connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).

% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the listener at
% run time.  This predicate is called when "nanisrch" starts up.

init_dynamic_facts:-
  assertz(location(desk,office)),
  assertz(location(apple,kitchen)),
  assertz(location(flashlight,desk)),
  assertz(location('washing machine',cellar)),
  assertz(location(nani,'washing machine')),
  assertz(location(table,kitchen)),
  assertz(location(crackers,desk)),
  assertz(location(broccoli,kitchen)),
  assertz(here(kitchen)),
  assertz(turned_off(flashlight)).

furniture(desk).
furniture('washing machine').
furniture(table).

edible(apple).
edible(crackers).

tastes_yuchy(broccoli).

%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Room):-
  can_go(Room),                 % check for legal move
  puzzle(goto(Room)),           % check for special conditions
  moveto(Room),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Room):-                  % if there is a connection it
  here(Here),                   % is a legal move.
  connect(Here,Room),!.
can_go(Room):-
  respond(['You can''t get to ',Room,' from here']),fail.

moveto(Room):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Room)).

% look lists the things in a room, and the connections

look:-
  here(Here),
  respond(['You are in the ',Here]),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following rooms:'),nl,
  list_connections(Here).

list_things(Place):-
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_things(_).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

% look_in allows the player to look inside a thing which might
% contain other things

look_in(Thing):-
  location(_,Thing),               % make sure there's at least one
  write('The '),write(Thing),write(' contains:'),nl,
  list_things(Thing).
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).

% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]).

is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  fail.

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  furniture(Thing),
  respond(['You can''t pick up a ',Thing]),
  !,fail.
is_takable(_).                     % not furniture, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old place
  asserta(have(Thing)).            % and add to your possessions

% drop - allows the player to transfer a possession to a room

drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)).
drop(Thing):-
  respond(['You don''t have the ',Thing]).


% eat, because every adventure game lets you eat stuff.

eat(Thing):-
  have(Thing),
  eat2(Thing).
eat(Thing):-
  respond(['You don''t have the ',Thing]).

eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was good']).
eat2(Thing):-
  tastes_yuchy(Thing),
  respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).

% inventory list your possesions

inventory:-
  have(X),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

% turn_on recognizes two cases.  If the player tries to simply turn
% on the light, it is assumed this is the room light, and the
% appropriate error message is issued.  Otherwise turn_on has to
% refer to an object which is turned_off.

turn_on(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_on(Thing):-
  have(Thing),
  turn_on2(Thing).
turn_on(Thing):-
  respond(['You don''t have the ',Thing]).

turn_on2(Thing):-
  turned_on(Thing),
  respond([Thing,' is already on']).
turn_on2(Thing):-
  turned_off(Thing),
  retract(turned_off(Thing)),
  asserta(turned_on(Thing)),
  respond([Thing,' turned on']).
turn_on2(Thing):-
  respond(['You can''t turn a ',Thing,' on']).

% turn_off - I didn't feel like implementing turn_off

turn_off(Thing):-
  respond(['I lied about being able to turn things off']).

% The only special puzzle in Nani Search has to do with going to the
% cellar.  Puzzle is only called from goto for this reason.  Other
% puzzles pertaining to other commands could easily be added.

puzzle(goto(cellar)):-
  have(flashlight),
  turned_on(flashlight),!.
puzzle(goto(cellar)):-
  write('You can''t go to the cellar because it''s dark in the'),nl,
  write('cellar, and you''re afraid of the dark.'),nl,
  !,fail.
puzzle(_).

% respond simplifies writing a mixture of literals and variables

respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_on) --> [switch,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining,room].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,flashlight) --> [flash,light].
noun(thing,'washing machine') --> [washing,machine].
noun(thing,'dirty clothes') --> [dirty,clothes].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing,light) --> [X,light], {room(X)}.
noun(thing,flashlight) --> [light], {have(flashlight)}.
noun(thing,light) --> [light].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !,
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until
  restword(C1,Cs,C2),       % we have all the word
  name(W, [NewC|Cs]).       % then make it an atom
readword(C,W,C2) :-         % otherwise
  get0(C1),
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :-
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).
Family

% GENE.PRO - genealogical relationships
%
% A Prolog database of relations derived from basic information about
% individuals.  The relations ships can all be read as 'relationship
% of', so for example, parent(P,C) means P is parent of C.
%
% When there is a performance trade-of in the implementation of a rule,
% it is assumed that in general the second argument of a relation will
% most likely be bound.  See for example full_sibling/2, which will
% have a smaller search for full_sibling(X,joe), than full_sibling(joe,X).
%
% This code is used as an example of an embedded Prolog application.
% One is a C++ application and the other Visual Basic.
%
% To use this code from Prolog, consult it in the listener and use the
% following predicates:
%
% open(F) - opens a file of family relationships, ex. open('england.fam').
%    open/1 just does a consult, so you can use consult instead.
% close - retracts all the persons currently defined
% save(F) - saves the persons in the named file
% add_person(Name, Mother, Father, Gender, Spouse) - adds a person
%     fact with the specified attributes, checking semantics as it does
% Relationship(P1, P2) - any relationship query, such as child(X,Y).
% relation(R, P1, P2) - can be used to find the relationship between
%     individuals as well as pose relationship queries.

parent(P,C) :-
 (mother(P,C) ; father(P,C)).

child(C,P) :- parent(P,C).

son(C,P) :- parent(P,C), male(C).

daughter(C,P) :- parent(P,C), female(C).

wife(W,P) :-
  spouse(W,P),
  female(W).

husband(H,P) :-
  spouse(H,P),
  male(H).

ancestor(A,P) :-
  parent(A,P).
ancestor(A,P) :-
  parent(X,P),
  ancestor(A,X).

descendent(D,P) :-
  parent(P,D).
descendent(D,P) :-
  parent(P,X),
  descendent(D,X).

full_sibling(S1, S2) :-
  mother(M,S2),
  mother(M,S1),
  S1 \= S2,
  father(F,S1),
  father(F,S2).

half_sibling(S1, S2) :-
  mother(M,S2),
  mother(M,S1),
  S1 \= S2,
  father(F1,S1),
  father(F2,S2),
  F1 \= F2.
half_sibling(S1, S2) :-
  father(F,S2),
  father(F,S1),
  S1 \= S2,
  mother(M1,S1),
  mother(M2,S2),
  M1 \= M2.

sibling(S1, S2) :-
  full_sibling(S1,S2).
sibling(S1, S2) :-
  half_sibling(S1,S2).

sister(S,P) :-
  sibling(S,P),
  female(S).

brother(B,P) :-
  sibling(B,P),
  male(B).

step_sibling(S1, S2) :-
  parent(P2, S2),
  spouse(M2, P2),
  parent(M2, S1),
  not(parent(M2,S2)),
  not(half_sibling(S1,S2)).

uncle(U,X) :-
  parent(P,X),
  brother(U,P).

aunt(A,X) :-
  parent(P,X),
  sister(A,P).

step_parent(P2,C) :-
  parent(P,C),
  spouse(P2,P),
  not(parent(P2,C)).

step_mother(M,C) :- step_parent(M,C), female(M).

step_father(F,C) :- step_parent(F,C), male(F).

step_child(C2,P) :- step_parent(P,C2).

step_daughter(D,P) :- step_child(D,P), female(D).

step_son(S,P) :- step_child(S,P), male(S).

nephew(N,X) :-
  sibling(S,X),
  parent(S,N),
  male(N).

niece(N,X) :-
  sibling(S,X),
  parent(S,N),
  female(N).

cousin(X,Y) :-
  parent(P,Y),
  sibling(S,P),
  parent(S,X).

grandmother(GM,X) :-
  parent(P,X),
  mother(GM,P).

grandfather(GF,X) :-
  parent(P,X),
  father(GF,P).

grandparent(GP,X) :-
  parent(P,X),  parent(GP,P).

grandson(GS,X) :-
  grandchild(GS,X),
  male(GS).

granddaughter(GD,X) :-
  grandchild(GD,X),
  female(GD).

grandchild(GC,X) :-
  parent(X,C),
  parent(C,GC).

%----------------------------------------------------------------------
% relation/3 - used to find relationships between individuals
%

relations([parent, wife, husband, ancestor, descendent, full_sibling,
    half_sibling, sibling, sister, brother, step_sibling, uncle,
    aunt, mother, father, child, son, daughter, step_parent,
    step_child, step_mother, step_father, step_son, step_daughter,
    nephew, niece, cousin, grandmother, grandfather, grandparent,
    grandson, granddaughter, grandchild]).

relation(R, X, Y) :-
  relations(Rs),
  member(R,Rs),
  Q =.. [R,X,Y],
  call(Q).


%----------------------------------------------------------------------
% person object
%
% These predicates define the interface to a person.  All of the
% genealogical rules are based on these predicates, which are
% based on the basic representation of a person.  These are the
% only rules which need to be changed if the representation of
% a person is changed.
%
% The current representation is flat database relations of the form:
%   person(Name, Gender, Mother, Father, Spouse).
%

add(Name,Gender,Mother,Father,Spouse) :-
  assert(person(Name,Gender,Mother,Father,Spouse)).
add(Name,_,_,_,_) :-
  delete(Name),
  fail.

open(FileName) :-
  consult(FileName).

close :-
  retractall(person(_,_,_,_,_)).

save(FileName) :-
  tell(FileName),
  listing(person),
  told.

delete(X) :-
  retract(person(X,_,_,_,_)).

person(X) :-
  person(X,_,_,_,_).

male(X) :-
  person(X,male,_,_,_).

female(Y) :-
  person(Y,female,_,_,_).

mother(M,C) :-
  person(C,_,M,_,_).

father(F,C) :-
  person(C,_,_,F,_).

spouse(S,P) :-
  person(P,_,_,_,S),
  S \= single.

%----------------------------------------------------------------------
% Semantic Integrity Checks on Update
%

add_person(Name,Gender,Mother,Father,Spouse) :-
  retractall(message(_)),
  dup_check(Name),
  add(Name,Gender,Mother,Father,Spouse),
  ancestor_check(Name),
  mother_check(Name, Gender, Mother),
  father_check(Name, Gender, Father),
  spouse_check(Name, Spouse).

dup_check(Name) :-
  person(Name),
  assert(message($Person is already in database$)),
  !, fail.
dup_check(_).

ancestor_check(Name) :-
  ancestor(Name,Name),
  assert(message($Person is their own ancestor/descendent$)),
  !, fail.
ancestor_check(_).

mother_check(_, _, Mother) :- not(person(Mother)), !.
mother_check(_, _, Mother) :-
  male(Mother),
  assert(message($Person's mother is a man$)),
  !, fail.
mother_check(Name, male, _) :-
  mother(Name, X),
  assert(message($Person, a male, is someone's mother$)),
  !, fail.
mother_check(_,_,_).

father_check(_, _, Father) :- not(person(Father)), !.
father_check(_, _, Father) :-
  female(Father),
  assert(message($Person's father is a man$)),
  !, fail.
father_check(Name, female, _) :-
  father(Name, X),
  assert(message($Person, a female, is someone's father$)),
  !, fail.
father_check(_,_,_).

spouse_check(Name, Spouse) :-
  spouse(Name, X),
  X \= Spouse,
  assert(message($Person is already someone else's spouse$)),
  !, fail.
spouse_check(Name, Spouse) :-
  blood_relative(Name, Spouse),
  assert(message($Person is a blood relative of spouse$)),
  !, fail.
spouse_check(_,_).

blood_relative(X,Y) :- (ancestor(X,Y); ancestor(Y,X)).
blood_relative(X,Y) :- sibling(X,Y).
blood_relative(X,Y) :- cousin(X,Y).
blood_relative(X,Y) :- (uncle(X,Y); uncle(Y,X)).
blood_relative(X,Y) :- (aunt(X,Y); aunt(Y,X)).
Custord

% CUSTORD

% This is a sample Prolog program which implements a portion
% of a customer order inventory application.  It is not intended to
% be complete, and only illustrates the concept of writing a database
% application in Prolog.

% This example extends the concept of an intelligent database to include
% a full database application.  It is really a rule based approach to
% transaction processing.  In fact a large percentage of the procedural
% code normally written in database applications has to do with
% enforcing semantic integrity rules involving multiple records.

% The distinction between data and process is thoroughly blurred.  Both
% reside together in the same logicbase.

% There is pure data as it might be defined in a relational database
% (customer, item, inventory, order); there are rules which really
% represent data views (item_quant); there are rules which add
% intelligence to the logicbase (good_customer, valid_order); and there
% are rules which are processes (order, report_inventory).

main :- order.

% customer(Name, Town, Credit-rating).

customer(dennis, winchester, xxx).
customer(dave, lexington, aaa).
customer(ron, lexington, bbb).
customer(julie, winchester, aaa).
customer(jawaid, cambridge, aaa).
customer(tom, newton, ccc).

% item(Number, Name, Reorder-quantity).

item(p1,thing,10).
item(p2,stuff,10).
item(p3,article,10).
item(p4,object,10).
item(p5,substance,10).
item(p6,piece,10).
item(p7,matter,10).

% inventory(Number, Quantity).

inventory(p1,10).
inventory(p2,10).
inventory(p3,10).
inventory(p4,78).
inventory(p5,23).
inventory(p6,14).
inventory(p7,8).

% item-inv view or join

item_quant(Item, Quantity):-
  item(Partno, Item, _),
  inventory(Partno, Quantity).

% reorder if inventory below reorder point

reorder(Item):-
  item(Partno, Item, Reorder_point),
  inventory(Partno, Quantity),
  Quantity < Reorder_point,
  write('Time to reorder '),
  write(Item), nl.
reorder(Item):-
  write('Inventory level ok for '),
  write(Item), nl.

% a good customer has a credit rating of aaa
% or lives in winchester
% or has ordered something

good_customer(Cust):-
  customer(Cust, _, aaa).
good_customer(Cust):-
  customer(Cust, winchester, _).
good_customer(Cust):-
  order(Cust, _, _).

% process order

order:-
  write('Customer: '),
  read(Customer),
  write('Item: '),
  read(Item),
  write('Quantity: '),
  read(Quantity),
  valid_order(Customer,Item,Quantity),
  asserta(order(Customer,Item,Quantity)),
  update_inventory(Item,Quantity),
  reorder(Item).

% an order is valid if
% it doesn't go below zero inventory and
% the customer is a good customer

valid_order(C, I, Q):-
  item(Partno, I, _),
  inventory(Partno, Onhand),
  Q =< Onhand,
  good_customer(C).
valid_order(C, I, Q):-
  write('Bad order'),
  nl,
  fail.

% update the inventory

update_inventory(I,Q):-
  item(Pn, I, _),
  inventory(Pn, Amount),
  NewQ is Amount - Q,
  retract(inventory(Pn, Amount)),
  asserta(inventory(Pn, NewQ)).

% inventory report

report_inventory:-
  item_quant(I, Q),
  write(I), tab(1),
  write(Q), nl,
  fail.
report_inventory:-true.
Birds

% BIRDS

% This is a sample of a classification expert system for identification
% of certain kinds of birds. The rules are rough excerpts from "Birds of
% North America" by Robbins, Bruum, Zim, and Singer.

% This type of expert system can easily use Prolog's built in inferencing
% system. While trying to satisfy the goal "bird" it tries to satisfy
% various subgoals, some of which will ask for information from the
% user.

% The information is all stored as attribute-value pairs. The attribute
% is represented as a predicate, and the value as the argument to the
% predicate. For example, the attribute-value pair "color-brown" is
% stored "color(brown)".

% "identify" is the high level goal that starts the program. The
% predicate "known/3" is used to remember answers to questions, so it
% is cleared at the beginning of the run.

% The rules of identification are the bulk of the code. They break up
% the problem into identifying orders and families before identifying
% the actual birds.

% The end of the code lists those attribute-value pairs which need
% to be asked for, and defines the predicate "ask" and "menuask"
% which are used to get information from the user, and remember it.

main :- identify.

identify:-
  retractall(known(_,_,_)),         % clear stored information
  bird(X),
  write('The bird is a '),write(X),nl.
identify:-
  write('I can''t identify that bird'),nl.

order(tubenose):-
  nostrils(external_tubular),
  live(at_sea),
  bill(hooked).
order(waterfowl):-
  feet(webbed),
  bill(flat).
order(falconiforms):-
  eats(meat),
  feet(curved_talons),
  bill(sharp_hooked).
order(passerformes):-
  feet(one_long_backward_toe).

family(albatross):-
  order(tubenose),
  size(large),
  wings(long_narrow).
family(swan):-
  order(waterfowl),
  neck(long),
  color(white),
  flight(ponderous).
family(goose):-
  order(waterfowl),
  size(plump),
  flight(powerful).
family(duck):-
  order(waterfowl),
  feed(on_water_surface),
  flight(agile).
family(vulture):-
  order(falconiforms),
  feed(scavange),
  wings(broad).
family(falcon):-
  order(falconiforms),
  wings(long_pointed),
  head(large),
  tail(narrow_at_tip).
family(flycatcher):-
  order(passerformes),
  bill(flat),
  eats(flying_insects).
family(swallow):-
  order(passerformes),
  wings(long_pointed),
  tail(forked),
  bill(short).

bird(laysan_albatross):-
  family(albatross),
  color(white).
bird(black_footed_albatross):-
  family(albatross),
  color(dark).
bird(fulmar):-
  order(tubenose),
  size(medium),
  flight(flap_glide).
bird(whistling_swan):-
  family(swan),
  voice(muffled_musical_whistle).
bird(trumpeter_swan):-
  family(swan),
  voice(loud_trumpeting).
bird(canada_goose):-
  family(goose),
  season(winter),                % rules can be further broken down
  country(united_states),        % to include regions and migration
  head(black),                   % patterns
  cheek(white).
bird(canada_goose):-
  family(goose),
  season(summer),
  country(canada),
  head(black),
  cheek(white).
bird(snow_goose):-
  family(goose),
  color(white).
bird(mallard):-
  family(duck),                  % different rules for male
  voice(quack),
  head(green).
bird(mallard):-
  family(duck),                  % and female
  voice(quack),
  color(mottled_brown).
bird(pintail):-
  family(duck),
  voice(short_whistle).
bird(turkey_vulture):-
  family(vulture),
  flight_profile(v_shaped).
bird(california_condor):-
  family(vulture),
  flight_profile(flat).
bird(sparrow_hawk):-
  family(falcon),
  eats(insects).
bird(peregrine_falcon):-
  family(falcon),
  eats(birds).
bird(great_crested_flycatcher):-
  family(flycatcher),
  tail(long_rusty).
bird(ash_throated_flycatcher):-
  family(flycatcher),
  throat(white).
bird(barn_swallow):-
  family(swallow),
  tail(forked).
bird(cliff_swallow):-
  family(swallow),
  tail(square).
bird(purple_martin):-
  family(swallow),
  color(dark).

country(united_states):- region(new_england).
country(united_states):- region(south_east).
country(united_states):- region(mid_west).
country(united_states):- region(south_west).
country(united_states):- region(north_west).
country(united_states):- region(mid_atlantic).

country(canada):- province(ontario).
country(canada):- province(quebec).
country(canada):- province(etc).

region(new_england):-
  state(X),
  member(X, [massachusetts, vermont, etc]).
region(south_east):-
  state(X),
  member(X, [florida, mississippi, etc]).

region(canada):-
  province(X),
  member(X, [ontario,quebec,etc]).

nostrils(X):- ask(nostrils,X).
live(X):- ask(live,X).
bill(X):- ask(bill,X).
size(X):- menuask(size,X,[large,plump,medium,small]).
eats(X):- ask(eats,X).
feet(X):- ask(feet,X).
wings(X):- ask(wings,X).
neck(X):- ask(neck,X).
color(X):- ask(color,X).
flight(X):- menuask(flight,X,[ponderous,powerful,agile,flap_glide,other]).
feed(X):- ask(feed,X).
head(X):- ask(head,X).
tail(X):- menuask(tail,X,[narrow_at_tip,forked,long_rusty,square,other]).
voice(X):- ask(voice,X).
season(X):- menuask(season,X,[winter,summer]).
cheek(X):- ask(cheek,X).
flight_profile(X):- menuask(flight_profile,X,[flat,v_shaped,other]).
throat(X):- ask(throat,X).
state(X):- menuask(state,X,[massachusetts,vermont,florida,mississippi,etc]).
province(X):- menuask(province,X,[ontario,quebec,etc]).

% "ask" is responsible for getting information from the user, and remembering
% the users response. If it doesn't already know the answer to a question
% it will ask the user. It then asserts the answer. It recognizes two
% cases of knowledge: 1) the attribute-value is known to be true,
% 2) the attribute-value is known to be false.

% This means an attribute might have multiple values. A third test to
% see if the attribute has another value could be used to enforce
% single valued attributes. (This test is commented out below)

% For this system the menuask is used for attributes which are single
% valued

% "ask" only deals with simple yes or no answers. a "yes" is the only
% yes value. any other response is considered a "no".

ask(Attribute,Value):-
  known(yes,Attribute,Value),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),         % fail if we know its false
  !, fail.

ask(Attribute,_):-
  known(yes,Attribute,_),           % fail if we know its some other value.
  !, fail.                          % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                       % if we get here, we need to ask.
  write('? (yes or no): '),
  read(Y),                          % get the answer
  asserta(known(Y,A,V)),            % remember it so we dont ask again.
  Y = yes.                          % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer. In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(yes,Attribute,Value),       % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(yes,Attribute,_),           % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('What is the value for '),write(Attribute),write('?'),nl,
  display_menu(Menu),
  write('Enter the number of choice> '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-        % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                       % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.        % start at one
  pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                    % what they entered as the value

pic_menu(_,_,none_of_the_above,[]). % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).      % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,               % try the next one
  pic_menu(NextCtr, N, Val, Rest).
