This file should be useful only to authors of packages that
interface Emacs to Google services; it implements functions to
request an authorization token using the Google ClientLogin web
service (see
http://code.google.com/apis/accounts/AuthForInstalledApps.html )
and handle the error cases.

Sample usage::

  (require 'googleaccount)
  ...
  (setq auth-header
     (googleaccount-login service email passwd))
  ...
  (if auth-header
    (let ((url-request-extra-headers (list auth-headers)))
       (url-retrieve ...some Google service URL...)

Function `googleaccount-login' tries to do the right thing:
- returns the authorization header to be added to
  `url-request-extra-headers' when authorization is successful;
- returns nil when Google requires a CAPTCHA challenge to unlock
  the account, so that the calling program may retry login at a
  later time;
- signals an error on any other case.

If you want full control, use `googleaccount-login-response', that
simply returns an alist with all data sent and received, and never
interacts with the user.

Only tried in Emacs 22.1 and 23.0.91.1; all the code relating to
CAPTCHA and errors other than "BadAuthentication" is *totally*
untested.
