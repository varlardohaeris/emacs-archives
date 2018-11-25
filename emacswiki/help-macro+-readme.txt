   Extensions to `help-macro.el'.


 ***** NOTE: The following macro defined in `help-macro.el' has
             been REDEFINED HERE:

 `make-help-screen'


-> ***********************  Example of use *********************************
->
->(make-help-screen help-for-empire-redistribute-map
->              "c:civ m:mil p:population f:food ?"
->              "You have discovered the GEET redistribution commands
->   From here, you can use the following options:
->
->c   Redistribute civs from overfull sectors into connected underfull ones
->      The functions typically named by empire-ideal-civ-fcn control
->          based in part on empire-sector-civ-threshold
->m   Redistribute military using levels given by empire-ideal-mil-fcn
->p   Redistribute excess population to highways for max pop growth
->      Excess is any sector so full babies will not be born.
->f   Even out food on highways to highway min and leave levels
->      This is good to pump max food to all warehouses/dist pts
->
->
->Use \\[help-for-empire-redistribute-map] for help on redistribution.
->Use \\[help-for-empire-extract-map] for help on data extraction.
->Please use \\[describe-key] to find out more about any of the other keys."
->              empire-shell-redistribute-map)
->
->  (define-key c-mp "\C-h" 'help-for-empire-redistribute-map)
->  (define-key c-mp help-character 'help-for-empire-redistribute-map)
