:-module(chatscript, [talk/4,
		      start_conversation/3,
		      set_chatscript_address/1]).
/** <module> Connection to chatscript server
 *
 *   http://chatscript.sourceforge.net/
 *
 *   @author Anne Ogborn
 *   @license mit
 *   @version 1.0.0
 */
:- multifile license:license/3.

license:license(mit, lgpl,
                [ comment('MIT License'),
                  url('http://opensource.org/licenses/MIT')
                ]).
:- license(mit).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_header)).

:- dynamic server_address/1.

%%	set_chatscript_address(+Address:term) is det
%
%	@param Address is a term of form domain:port
%
%       Set the address of the chatscript server
%
set_chatscript_address(Address) :-
	retractall(server_address(_)),
	asserta(server_address(Address)).

%%	talk(+User:atom, +Bot:atom, +Message:atom, -Reply:string) is
%	semidet
%
%	Send a volley to the server
%
%	@User User name
%	@Bot  name of the bot. The default bot is ''
%	@Message user's input to the bot. No nl. Must not be the null
%	string
%       @Reply bots response
%
%     You must call start_conversation before calling this
%     for each user
%
talk(User, Bot, Message, Reply) :-
	Message \= '',
	talk_(User, Bot, Message, Reply).

talk_(User, Bot, Message, Reply) :-
	format(string(S), '~w\x00~w\x00\~w\x00', [User, Bot, Message]),
	server_address(Address),
	tcp_connect(Address, StreamPair, []),
	stream_pair(StreamPair, Read, Write),
	write(Write, S),
	flush_output(Write),
	read_stream_to_codes(Read, Codes1),
	delete(Codes1, 0, Codes2),  % they insert nuls
	string_codes(Reply, Codes2),
	close(StreamPair).

%%	start_conversation(+User:atom, +Bot:atom, -Reply:string) is det
%
%	call this for each user to get initial response prior to calling
%	talk/4
%
start_conversation(User, Bot, Reply) :-
	talk_(User, Bot, '', Reply).
