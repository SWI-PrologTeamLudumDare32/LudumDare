# LudumDare

Our Ludum Dare entry

## Starting the server

To start in debug mode just consult debug_start.pl

This starts the doc server and the main server on 7777, opens the editor to the
debug_start page, opens the navigator, and opens the root web page.

to start in production mode consult production.pl
as of this writing thats not working, owign to the settings bizarrity

## Pengines demo

Browse  http://localhost:7777/f/hellopengine.html

the first pengine is setting the second line dynamically. Click next to see subsequent solutions

The second pengine is setting the fifth line dynamically. It calls the server to compute the factorial,
which is something that was imported into the sandbox in  hellopengine.pl

Also called by the second pengine is my_unsafe/1, which appends its argument to Hello Out There and writes the string
to a file named foo.txt.   Obviously writing files is usually unsafe, but we control the file name, and we limit the length
to 25 chars, so it's safe to do.




