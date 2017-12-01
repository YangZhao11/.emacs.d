(set-frame-font "Menlo-14" nil 't)
(setq mac-option-modifier 'none
      mac-command-modifier 'meta
      mac-right-option-modifier 'meta
      mac-right-command-modifier 'super)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/git/current/bin")
(setenv "PATH" (concat "/usr/local/git/current/bin:/usr/local/bin:"
                       (getenv "PATH")))
(setenv "LANG" "en_US.UTF-8")
;; (set-fontset-font t 'unicode
;;                   (font-spec :name "Apple Color Emoji" :size 11) nil 'prepend)
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (set-frame-size (selected-frame) 80 46))))
