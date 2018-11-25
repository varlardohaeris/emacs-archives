Definition of a new Emacs type: record.
A record is a set of named fields with their associated type.

The type definition is made through a predicate, the internal functions
will do type checking to make sure the type is correct.

In order to define a new record the syntax is:
  (defrecord type-name "Comment string"
     :field-name1 'predicate-1
     :field-name2 'predicate-2
     ....)

Once the type created, a generic set of functions are available in
order to work with it:
. recordp                 returns t if type-name is a record type
. get-record-comment      returns the comment associated with the
			     type
. make-new-record         creates and returns a new instance
			     (optional: initial values)
. instancep               returns t if the argument is an instance
			     of a record type
. get-record-type         returns the record-type associated with
			     an instance
. has-record-field        checks if a specific field name exists in
			     the record-type definition
. set-record-field-value  set a value to a specific field of an
			     instance
. set-record-field-values set values to different fields of an
			     instance
. get-record-field-value  returns the value of a specific field of
			     an instance

The record type definition will auto-generate the definition of the
following functions:
  a constructor function:
     make-new-<record-name>
. a predicate function:
     <record-name>-p
. a set function and a get function for each field name:
     set-<record-name>-<field-name>
     get-<record-name>-<field-name>

*Note*: the set functions will change the value of the instance
 without having to do a "setq".  If you just want to a field value
 without changing the variable, use: set-record-field-value.
