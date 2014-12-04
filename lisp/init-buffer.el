; -*- coding: utf-8 -*-
;; ==================================================
;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-auto-merge-delay-time 99999
      ido-create-new-buffer 'always
      ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window
      ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
              "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
              "_region_" " output\\*$" "^TAGS$" "^\*Ido")
      ido-ignore-directories
      '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#"
        "\\`\\.\\./" "\\`\\./"))
(define-key ido-file-dir-completion-map
  [remap set-mark-command]  'ido-restrict-to-matches)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; ==================================================
;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defvar citc-root (format "/google/src/cloud/%s" (getenv "USER"))
  "The root location of all citc clients")

(defun citc-clients ()
  "Returns all citc clients by listing citc-root."
  (let ((citc (ignore-errors (directory-files citc-root))))
    (setq citc (delete "." citc))
    (delete ".." citc)))

(defvar z-default-citc-group
  `(("CitC"
     ,(cons 'filename (format "^%s/" citc-root))))
  "Default citc group when citc clients are not available,
e.g. no prodaccess.")

(defun z-citc-group (d)
  "Function to map citc client name to ibuffer filter group"
  (list (format "CitC %s" d)
        (cons 'filename
              (format "^%s/%s/" citc-root d))))

(defun z-ibuffer-groups (name)
  "Generate ibuffer filter group definition. Each CitC client has its own group."
  (let ((citc (citc-clients))
        (grps z-default-citc-group))
    (if citc
        (setq grps (mapcar 'z-citc-group citc)))
    (append (list name)
           grps
           `(("Google3"
              (filename . "/google3/"))
             (,(format "%s Home" (getenv "USER"))
              ,(cons 'filename
                     (format "^%s/" (getenv "HOME"))))
             ("Misc" (or (mode . Custom-mode)
                         (mode . help-mode)
                         (mode . ess-help-mode)
                         (mode . apropos-mode)
                         (mode . completion-list-mode)
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
(add-hook 'ibuffer-hook 'z-ibuffer-hook)

;; how to solve collision of buffer names: filename:pathpart
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; midnight mode cleans up unused buffer
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")
(add-to-list 'clean-buffer-list-kill-regexps "\\*help\\[R\\]")
(add-to-list 'clean-buffer-list-kill-regexps "\\*[Pg]4[- ]")

;; always need a scratch buffer.
(defun z-idle-timer () (get-buffer-create "*scratch*"))
(run-with-idle-timer 1 t 'z-idle-timer)
