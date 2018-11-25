This library contains a major mode for composing and sending text messages.
It contains the function sms-send-buffer-as-text-message which is bound to "C-c C-c"
by default. This function will in turn call the sms-send-text-message function to send
the contents of the buffer to the phone numbers listed at the top of the buffer after "To:".
Currently sms-send-text-message is bound to sms-send-text-message-android which can be used
on Emacs running in Debian on Android phones with Android Scripting Environment running, as detailed
here: http://www.emacswiki.org/emacs/EmacsOnAndroid
You can create your own function for sending text messages on your device and bind sms-send-text-message to it.

If you have bbdb-sms installed then you can use tab completion of names in the "To:" header of the *SMS* buffer,
it will complete the name and phone number according to entries in your bbdb.

The following keybindings are defined by default:

      "C-c C-c" ; send the buffer as a text message to the phone numbers after "To:" at the top
      "C-c C-k" ; kill the buffer
      "<tab>"   ; if bbdb-sms.el is loaded, complete the name at point and insert the associated phone number(s)

INSTALLATION:

Put sms.el in your load-path.
The load-path is usually ~/.emacs.d/

And the following to your ~/.emacs startup file.

(require 'sms)
