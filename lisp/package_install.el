(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-refresh-contents)

(package-install 'ace-jump-mode)
(package-install 'ace-jump-zap)
(package-install 'ace-window)
(package-install 'autopair)
(package-install 'expand-region)
(package-install 'goto-chg)
(package-install 'ido-ubiquitous)
(package-install 'ido-vertical-mode)
(package-install 'julia-mode)
(package-install 'lua-mode)
(package-install 'magit)
(package-install 'markdown-mode)
(package-install 'multiple-cursors)
(package-install 'rainbow-delimiters)
(package-install 'register-channel)
(package-install 'smex)
(package-install 'yasnippet)
(package-install 'zenburn-theme)

(add-to-list 'package-archives
             '("GELPA" . "http://internal-elpa.appspot.com/packages/"))
(package-refresh-contents)
(package-install 'squery)
