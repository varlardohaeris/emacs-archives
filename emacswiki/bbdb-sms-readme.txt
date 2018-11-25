This library contains 3 useful functions.

bbdb-sms-send-text-message :     Send text message to bbdb record at point (called from within *BBDB* buffer).
                                 An *SMS* buffer will be opened for editing the text message, and the
                                 "To:" header will be filled with the valid phone number(s) for the
                                 bbdb record at point.
                                 A phone number is valid if it's type is a member of the
                                 bbdb-sms-phone-number-types list.
                                 If bbdb-sms-all-phone-numbers is non-nil then all valid phone numbers
                                 for the current record will be used, otherwise just the first one will be used.
bbdb-sms-send-text-message-all : Send text message to all displayed records in current *BBDB* buffer.
                                 The same rules as for bbdb-sms-send-text-message apply, but now phone
                                 numbers for all currently displayed records are added.
bbdb-sms-complete-name :         Complete name from bbdb records, and insert valid phone number(s) for texting.
                                 If bbdb-sms-complete-name-allow-cycling is non-nil, and bbdb-sms-all-phone-numbers
                                 is nil, then subsequent calls to this function will cycle through all valid
                                 phone numbers.

The following variables are used:

bbdb-sms-phone-number-types : List of valid phone number types for sending sms text messages to.
                              Default value is '("Mobile")

bbdb-sms-all-phone-numbers : Whether or not to send text messages to all valid phone numbers of a record,
                             or just the first valid phone number. Default value is nil

bbdb-sms-complete-name-allow-cycling : Whether to allow cycling through valid phone numbers on subsequent
                                       calls to bbdb-sms-complete-name function. This only takes effect if
                                       bbdb-sms-all-phone-numbers is nil. Default value is t

INSTALLATION:

Make sure you have sms.el and bbdb 2.36 installed

Put bbdb-sms.el in your load-path.
The load-path is usually ~/.emacs.d/

And the following to your ~/.emacs startup file.

(require 'bbdb-sms)
