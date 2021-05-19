"Music Player Daemon (MPD) is a flexible, powerful, server-side
application for playing music."  <https://www.musicpd.org/>.
mpdmacs is a lightweight MPD client for Emacs.

See also mpdel <https://gitea.petton.fr/mpdel/mpdel >, as well as
the mpc package which ships with Emacs beginning with version 23.2.

mpdmacs requires elmpd <https://github.com/sp1ff/elmpd>, a
lightweight asynchronous library for building MPD clients.  My goal
is to have Emacs never freeze or pause while running this package.

Communications Model:

mpd encourages "throwaway" connections; i.e. a pattern in which a
connection to the server is made, one or a few commands are issued,
and the connection then closed down (the default server-side
timeout is 60 seconds).

The one exception is the "idle" command, which clients can use
to receive notifications of server-side changes.  When a client
issues the "idle" command, server-side timeouts are disabled.

mpdmacs uses a single `elmpd-connection' (which itself uses a
socket over which it will talk to MPD along with a callback for
servicing server-side changes).  The connection will immediately be
put into "idle" mode.  Commands issued through this package will
cause a "noidle" command to be issued on that connection before the
commands are issued; after the commands complete, the connection
will again be placed into "idle" mode.
