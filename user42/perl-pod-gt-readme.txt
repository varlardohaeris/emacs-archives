;;; Commentary:
;;
;; This spot of code helps when writing Perl POD markup forms like C<...>,
;; B<...>, etc.  `perl-pod-gt-enable' sets up
;;
;;    * ">" key for smart E<gt> insertion
;;    * suppress line breaks when filling
;;    * some warning face overlays
;;
;; Commands M-x perl-pod-gt-single and M-x perl-pod-gt-double convert
;; C<<..>> to C<...> or vice versa, when you want to upgrade or downgrade.

