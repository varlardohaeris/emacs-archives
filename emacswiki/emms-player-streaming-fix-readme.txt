This fixes subtitle check in streaming. Subtitle checking resorts to
`file-exists-p', leveraging on tramp. Unfortunately, this does not handle
URLs, so an error is triggered at each `emms-player-mplayer-start'
invocation.
