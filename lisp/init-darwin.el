(set-frame-font "Menlo-17" nil 't)
(setq mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-right-option-modifier 'super
      mac-right-command-modifier 'control)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/git/current/bin")
(add-to-list 'exec-path "/usr/local/brew/sbin")
(add-to-list 'exec-path "/usr/local/brew/bin")
(add-to-list 'exec-path "~/bin")
(setenv "PATH" (concat "~/bin:/usr/local/brew/bin:/usr/local/brew/sbin:/usr/local/git/current/bin:/usr/local/bin:"
                       (getenv "PATH")))
(setenv "LANG" "en_US.UTF-8")

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (setq ns-use-proxy-icon nil)

(set-fontset-font t 'unicode
                  (font-spec :name "Apple Color Emoji" :size 11) nil 'prepend)

(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (set-frame-size (selected-frame) 80 46))))
