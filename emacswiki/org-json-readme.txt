ChangeLog:
0.3.1: specify `json-object-type to 'alist.

0.3: fix 'true', 'false', '[]' and '{}'.

0.2: fix org-json-encode for mistake convert single element alist
to vector.

0.1: initial version.

Usage:

1. call (org-json-encode) to convert org(in current buffer) to json,
   return convert json.
2. call (org-json-decode json) to convert json to org,
   return converted text.

Example:

* i1 [2,3,4]
* i2 3.4
* i3 "fdsafs"
=> (("i1" . [2,3,4]) ("i2" . 3.4) ("i3" . "fdsafs"))


* i1
[2,3,4]
* i2
3.4
=> (("i1"  . [2,3,4]) ("i2" 3.4)).

* lv1_1
** lv2a 1
** lv2b 2
* lv1_2 "tt"
=> (("lv1_1" . (("lv2a" . 1) ("lv2b" . 2))) ("lv1_2" . "tt"))

* vect
** 0 "idx0"
** 1 1.23
** 2
[5.2, "2.3t", 1]
=> (("vect" . ["idx0" 1.23 [5.2 "2.3t" 1]]))

* long_src
+begin_src
line1
line2
+end_src
=> (("long_src" . "line1\nline2\n"))
