(require 'package)
(add-to-list 'package-archives
             '("GELPA" . "http://internal-elpa.appspot.com/packages/"))
(package-refresh-contents)

(dolist (p package-selected-packages)
  (package-install p))
