'reco' establishes a TCP connection with the server and sends an
HTTP request string.  The server (hopefully) responds with some
header information describing the streaming audio channel, some
audio data and then the name of the song being played (usually in
that order).

Some stations like WCPE [http://wcpe.org], while giving excellent
broadcasts do not support title streaming over MP3 or Ogg.  Using
this software on such stations will only result in general station
information and not the artist name or title of the track being
played.

Functionality:

Currently supports Icecast and Shoutcast servers with Ogg and MP3
streams.

Important Notes:

1) This software does not parse, cache or save audio data at
   all. This software downloads between of 2k-65K Bytes of data
   from a given streaming audio channel per call. This software is
   optimized to download as little as possible from a given
   streaming audio channel and then to immediately disconnect.

2) This software disregards and then discards all audio data
   automatically after each call.

3) This software connects for a maximum of 10 seconds and then
   immediately disconnects. Usually the software will disconnect
   long before the 10 second limit is reached.

4) It is the responsibility of the user to read the Terms of
   Service of the streaming audio channel before running this
   software on that channel's service. Some streaming audio
   channels explicitly request 3rd party applications not to
   connect to their service. This is their prerogative. Respect it.
