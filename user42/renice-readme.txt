;;; Commentary:
;;
;; This is an M-x renice command to "nice" down a sub-process, by running
;; the renice(1) program.  It can be used in `compilation-mode' and similar
;; buffers with a sub-process, and also from M-x list-processes.
;; (M-x proced and M-x top have their own renices too.)
;;
;; The main use is to turn down priority of a slow program so it can be left
;; to run without degrading other jobs.  A program you know will be slow can
;; be started with "nice -10 ...", or can itself lower priority with the
;; right system calls, but sometimes you don't know in advance and when you
;; see how slow it's good to turn it down without restarting etc.

