(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)

(package-install 'autopair)
(package-install 'zenburn-theme)
(package-install 'markdown-mode)
(package-install 'lua-mode)
(package-install 'rainbow-delimiters)
(package-install 'yasnippet)
(package-install 'expand-region)
(package-install 'multiple-cursors)
(package-install 'smex)
(package-install 'ido-ubiquitous)
(package-install 'register-channel)
(package-install 'ido-vertical-mode)
(package-install 'magit)
(package-install 'goto-chg)

(add-to-list 'package-archives
             '("GELPA" . "http://internal-elpa.appspot.com/packages/"))
(package-refresh-contents)
(package-install 'squery)
