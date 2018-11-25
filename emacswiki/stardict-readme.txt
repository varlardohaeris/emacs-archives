Example:

(require 'stardict)
(setq dict
      (stardict-open "~/.stardict/dic/stardict-lazyworm-ec-2.4.2"
                     "lazyworm-ec"))
(stardict-word-exist-p dict "apple")
(stardict-lookup dict "apple")

(stardict-open-dict-file dict)
(mapcar (lambda (x) (stardict-lookup dict x)) (make-list 1000 "apple"))
