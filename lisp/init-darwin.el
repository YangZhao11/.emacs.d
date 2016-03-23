(set-frame-font "Menlo-15" nil 't)
(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      mac-right-option-modifier 'meta
      mac-right-command-modifier 'super)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/git/current/bin")
(setenv "PATH" (concat "/usr/local/git/current/bin:/usr/local/bin:"
                       (getenv "PATH")))
(setenv "LANG" "en_US.UTF-8")
(set-fontset-font t 'unicode
                  (font-spec :name "Apple Color Emoji" :size 11) nil 'prepend)
