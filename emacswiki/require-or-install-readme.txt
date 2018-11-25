Use (require-or-install 'my-package) to automatically download and install
my-package.el if require fails.

require-or-install takes two others optionals arguments :
   - PAGENAME (the EmacsWiki PAGENAME where to download my-package)
   - URL (the download URL)

To install, to add to .emacs:
   (require 'require-or-install)
