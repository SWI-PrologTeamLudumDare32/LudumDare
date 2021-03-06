:- module(hubmaker_demo, []).
/** <module>  Demo of hubmaker system
 *
 */

:-use_module(hubmaker).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/hub)).

:- (\+ current_hub(demo, _), make_hub(demo) ; true).

:- http_handler(root(hubdemo), hub_demo, []).

hub_demo(_Request) :-
	reply_html_page(
	    title('Demo of hubmaker'),
	    \hub_demo_page).

hub_demo_page -->
	{
            gensym(id, ID),
            format(atom(Send), 'ddd.sendChat(this.value)', [ID])
        },
	html([
	    h1('Hub Demo Page'),
	    p(id(messages), []),
	    \js_script({|javascript(ID)||var my_handler = function (e) {
			console.log(e.data);
			var data = eval(e.data);
	      };|}),
	  input([type(text), onchange(Send), name(foo), value('type here')], []),
	  \hub_script(demo, my_handler)]).

:- multifile hubmaker:message_handler/4.

hubmaker:message_handler(demo, _Room, _ID, Message) :-
	websocket{opcode:text, data:String} :< Message,
	format(atom(A), '$("#messages").append("~w");', [String]),
	NewMessage = Message.put(data, A),
	broadcast(demo, NewMessage),
        debug(chat, 'Broadcast: ~p', [NewMessage]).

