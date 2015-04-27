# LudumDare

Our Ludum Dare entry

## Installing Francs Tireurs

You'll need to install chatscript from chatscript.sourceforge.net

It comes with binaries for linux 32 and 64 and windows.exe
./LinuxChatScript on a mac

copy the files in LudumDare/chatscript into the root of the chatscript install

Copy the directories under RAWDATA into the chatscript RAWDATA.

start chatscript and type

:build 0
:build antonette
:build baron

to run chatscript as a server, add port=1024

if you need to run on diff port, change the port in chatscript, and in the prolog query 

set_setting(ludumdare:chatscript_location, localhost:4050).
save_settings.

This makes a settings.db file in your LD install root
Please don't check this file in.

Start prolog and browse 

http://localhost:7777/f/francs_tireurs.html






## Starting the server

To start in debug mode just consult debug_start.pl

This starts the doc server and the main server on 7777, opens the editor to the
debug_start page, opens the navigator, and opens the root web page.

to start in production mode consult production.pl
as of this writing thats not working, owign to the settings bizarrity

to change the port to start on generate a settings.db file. Please *do not* check this file in.
To generate file:

~~~~~
?- set_setting(debug_start:debug_port, 8080).  % or whatever port you want to debug on


?- set_setting(ludumdare:production_port, 80). % or whatever port you run in production
~~~~~

then

~~~~~
?- save_settings.
~~~~~

## Pengines demo

Browse  http://localhost:7777/f/hellopengine.html

the first pengine is setting the second line dynamically. Click next to see subsequent solutions

The second pengine is setting the fifth line dynamically. It calls the server to compute the factorial,
which is something that was imported into the sandbox in  hellopengine.pl

Also called by the second pengine is my_unsafe/1, which appends its argument to Hello Out There and writes the string
to a file named foo.txt.   Obviously writing files is usually unsafe, but we control the file name, and we limit the length
to 25 chars, so it's safe to do.

## Whiteboard demo

Browse  http://localhost:7777/hub/

Draw some nodes and arcs. Left click to make a new node. Left drag starting within one to move it.
Right drag to connect nodes with arcs.

then open a second browser page on the same http://localhost:7777/hub/

Whee - collaboration.

## Javascript demo game

Grzegorz has provided a starter js game in createjs.com 's system

you can see it at

http://localhost:7777/f/game.html

use arrow keys to move

## generic factory for hubs 

in hubmaker.pl

Demo in http://localhost:7777/hubdemo   

Open this page in two or more browser windows and type something in the boxe in one of them
It'll appear in the others

## Ludum Dare

We'll submit our game in the hour after it ends on monday

Judging
All participants that submit a game are allowed to judge. Games are given 1-5 star ratings in each category, or N/A where 
not applicable. The categories include:

 * Innovation – The unexpected. Things in a unique combination, or something so different it’s notable.
 * Fun – How much you enjoyed playing a game. Did you look up at the clock, and found it was 5 hours later?
 * Theme – How well an entry suits the theme. Do they perhaps do something creative or unexpected with the theme?
 * Graphics – How good the game looks, or how effective the visual style is. Nice artwork, excellent generated or geometric graphics, charming programmer art, etc.
 * Audio – How good the game sounds, or how effective the sound design is. A catchy soundtrack, suitable sound effects given the look, voice overs, etc.
 * Humor – How amusing a game is. Humorous dialog, funny sounds, or is it so bad it’s good?
 * Mood – Storytelling, emotion, and the vibe you get while playing.
 * Overall – Your overall opinion of the game, in every aspect important to you.

In addition, there is a special category Coolness. The more games you play and rate, the higher your score. 
ALSO: we prioritize people with a higher Coolness. More players will find (and likely) rate your game if you have 
a high Coolness.






