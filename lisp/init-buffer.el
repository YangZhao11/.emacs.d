; -*- coding: utf-8 -*-
;; ==================================================
;; ido
(use-package ido
  :bind ("M-o" . ido-switch-buffer)
  :config
  (ido-mode t)
  (setq ido-enable-flex-matching 't
        ido-auto-merge-delay-time 99999
        ido-create-new-buffer 'always
        ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
          "\\[r\\]\\(<[0-9]+>\\)?$" "\\[fundamental\\]\\(<[0-9]+>\\)?$"
          "_region_" " output\\*$" "^TAGS$" "^\*Ido")
        ido-ignore-directories
        '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
        ido-ignore-files
        '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#"
          "\\`\\.\\./" "\\`\\./"))

  (define-key ido-file-dir-completion-map
    [remap set-mark-command]  'ido-restrict-to-matches))

(use-package ido-ubiquitous :ensure
  :init (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode :ensure
  :init (ido-vertical-mode 1))
(setq ido-decorations
      '("\n► " "" "\n  " "\n  ..." "[" "]"
        " [No match]" " [Matched]" " [Not readable]"
        " [Too big]" " [Confirm]" "\n► " ""))

;; ==================================================
;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :functions ibuffer-switch-to-saved-filter-groups
  :config
  (defun g3-clients ()
    "Returns all g3 clients by iterating through all open files."
    (let* ((files (delq nil (mapcar 'buffer-file-name (buffer-list))))
           (f (cl-remove-if-not (lambda (x) (s-contains-p "/google3/" x)) files))
           (dirs (mapcar (lambda (x)
                           (replace-regexp-in-string
                            ".*/\\([^/]+\\)/google3/.*" "\\1" x)) f)))
      (cl-delete-duplicates dirs
                            :test (lambda (x y) (equal x y)))))

  (defvar z-default-g3-group
    '(("G3" (filename . "/googl3/")))
    "Default g3 group when g3 clients are not available,
e.g. no prodaccess.")

  (defun z-g3-group (d)
    "Function to map g3 client name to ibuffer filter group"
    (list (format "G3: %s" d)
          (cons 'filename
                (format "/%s/google3/" d))))

  (defun z-ibuffer-groups (name)
    "Generate ibuffer filter group definition. Each G3 client has its own group."
    (let ((g3 (g3-clients))
          (grps z-default-g3-group))
      (if g3
          (setq grps (mapcar 'z-g3-group g3)))
      (append (list name)
              grps
              `(("G3"
                 (filename . "/google3/"))
                (,(format "%s Home" (getenv "USER"))
                 ,(cons 'filename
                        (format "^%s/" (getenv "HOME"))))
                ("Misc" (or (mode . Custom-mode)
                            (mode . help-mode)
                            (mode . ess-help-mode)
                            (mode . apropos-mode)
                            (mode . completion-list-mode)
                            (name . "\\[r\\]$")
                            (name . "\\[fundamental\\]$")
                            (name . "^\\*Messages\\*$")
                            (name . "^\\*Backtrace\\*$")
                            (name . "^\\*.*Help.*\\*$")
                            (name . "^\\*[gP]4[- ]")
                            (name . "^\\*TeX ")
                            (name . "^\\*ESS\\*$")
                            (name . "^\\*ESS-")))))))

  (setq ibuffer-show-empty-filter-groups nil)
  (defadvice ibuffer-make-column-filename (after strip-google3-filename activate)
    "Strip experimental to ~user, .../a/google3 to //a/"
    (setq ad-return-value
          (replace-regexp-in-string "/google3/experimental/users/" "/google3/~"
                                    ad-return-value))
    (setq ad-return-value
          (replace-regexp-in-string "/google/src/cloud/.*/google3/" "//"
                                    ad-return-value))
    (setq ad-return-value
          (replace-regexp-in-string ".*/\\([^/]*\\)/google3/" "//\\1/"
                                    ad-return-value)))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark modified read-only " "
                (name 45 45 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide))))
  (defun z-ibuffer-mode-hook ()
    (ibuffer-switch-to-saved-filter-groups "Default"))
  (add-hook 'ibuffer-mode-hook 'z-ibuffer-mode-hook)
  (defun z-ibuffer-hook ()
    (setq ibuffer-saved-filter-groups (list (z-ibuffer-groups "Default"))))
  (z-ibuffer-hook)             ; ensure groups are set on the first run.
  (add-hook 'ibuffer-hook 'z-ibuffer-hook))

;; how to solve collision of buffer names: filename:pathpart
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))

;; midnight mode cleans up unused buffer
(use-package midnight
  :init
  (midnight-delay-set 'midnight-delay "4:30am")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*help\\[R\\]")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*[Pg]4[- ]"))

;; always need a scratch buffer.
(defun z-idle-timer () (get-buffer-create "*scratch*"))
(run-with-idle-timer 1 t 'z-idle-timer)
