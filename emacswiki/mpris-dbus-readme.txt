A simple library for simplify MPRIS(http://wiki.xmms2.xmms.se/wiki/MPRIS)
dbus call.

Example:
  (mpris-call "/TrackList" "GetMetadata" :int32 0)
  (mpris-track-list-call "GetLength")
  (mpris-player-call "GetCaps")
  (mpris-player-call "Next")
  (mpris-play)
  (mpris-toggle-play)
