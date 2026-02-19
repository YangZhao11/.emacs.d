; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
(push "~/.emacs.d/god-mode" load-path)

(defvar-local z-god-saved-input-method nil
  "Saved input method before god-mode")
(defvar-local z-god-saved-view-mode nil
  "Saved view-mode before god-mode")

(defun set-cursor-type (spec &optional frame)
  "Set cursor type for FRAME.

SPEC could be `box', `bar', or `hbar'. FRAME defaults to current frame.
On a text frame, we use xterm compatible escape code."
  (cond
   ((display-graphic-p frame)
    (modify-frame-parameters frame `((cursor-type . ,spec))))
   ((frame-terminal frame)
    ;; https://unix.stackexchange.com/questions/49485/escape-code-to-change-cursor-shape
    (let* ((shape (or (car-safe spec) spec))
           (param (cond ((eq shape 'bar) "5")
                        ((eq shape 'hbar) "3")
                        (:else "1"))))
      (send-string-to-terminal
       (concat "\e[" param " q") frame)))))

(defun set-cursor-type-all-frames (spec)
  "Set cursor to SPEC for all frames."
  (dolist (f (frame-list))
    (set-cursor-type spec f)))

;; maybe record kmacro during mortal mode, so that the whole action
;; can be repeated as a whole. However, emacs macro does not work
;; recursively.
(defun mortal-mode-exit ()
  "Exit mortal-mode and resume god mode. Bind to RET.

If the local binding of RET has semantic `comint-send-input', then call it."
  (interactive)
  (god-local-mode-resume)
  (mortal-mode 0)
  (let* ((binding (local-key-binding (kbd "RET")))
         (semantic (get binding 'command-semantic)))
    (when (eq  semantic 'comint-send-input)
      (call-interactively binding))))

(defvar-keymap mortal-mode-map
  :doc "Keymap for `mortal-mode'."
  "RET" #'mortal-mode-exit)

(define-minor-mode mortal-mode
  "Allow temporary departures from god-mode."
  :keymap mortal-mode-map
  (when mortal-mode
    (condition-case nil
        (progn (barf-if-buffer-read-only)
               (god-local-mode-pause))
      (buffer-read-only
       (mortal-mode 0)
       (user-error "Buffer is read-only.")))))

;; TODO: `substitute-command-keys' uses `where-is-internal' to decide
;; the key for a command. Advice it to work with god-mode.

(require 'god-mode)

(keymap-global-set "<home>" #'god-mode-all)
(diminish 'god-local-mode)

(require 'god-mode-isearch)
(keymap-set isearch-mode-map
            "<home>" #'god-mode-isearch-activate)
(keymap-set god-mode-isearch-map
            "<home>" #'god-mode-isearch-disable)
(dolist (i '(?s ?r ?w ?v ?% ?< ?>))
  (keymap-set god-mode-isearch-map
    (char-to-string i) 'god-mode-self-insert))

(setq god-mod-alist-default '((nil . "C-") (?m . "M-") (?g . "C-M-")))
(setq god-mod-alist-meta '((nil . "M-") (?g . "C-M-")))
(setq god-mod-alist-cm '((nil . "C-M-") (?m . "M-")))
(setq god-mod-alist god-mod-alist-default)
(defun god-mode-toggle-sticky-meta (arg)
  (interactive (list (string= "M-" (cdr (assoc nil god-mod-alist)))))
  (if arg
      (progn (setq god-mod-alist god-mod-alist-default)
             (keymap-set god-local-mode-map "m" #'god-mode-self-insert))
    (setq god-mod-alist god-mod-alist-meta)
    (keymap-set god-local-mode-map "m" #'god-mode-toggle-sticky-meta))
  (force-mode-line-update))

(defun god-mode-toggle-sticky-cm (arg)
  (interactive (list (string= "C-M-" (cdr (assoc nil god-mod-alist)))))
  (if arg
      (progn (setq god-mod-alist god-mod-alist-default)
             (keymap-set god-local-mode-map "g" #'god-mode-self-insert))
    (setq god-mod-alist god-mod-alist-cm)
    (keymap-set god-local-mode-map "g" #'god-mode-toggle-sticky-cm))
    (force-mode-line-update))

(bind-keys :map god-local-mode-map
           ("i" . mortal-mode)
           ("M" . god-mode-toggle-sticky-meta)
           ("G" . god-mode-toggle-sticky-cm))

(setq god-exempt-major-modes nil
      god-exempt-predicates nil
      god-mode-can-omit-literal-key 't)

(bind-keys ("C-h k" . god-mode-describe-key) ; this works in special mode too
)

(defun god-mode-low-priority-command-semantic (command)
  (eq 'self-insert-command
      (or (get command 'command-semantic) command)))
(setq god-mode-low-priority-command-predicate
      'god-mode-low-priority-command-semantic)

(setq god-mode-low-priority-keys
      '(?z ?# ?q ?\[ ?\] ?U ?`
        ?~ ?! ?@ ?$ ?% ?^ ?& ?* ?{ ?}
        ?< ?> ?: ?| ?\\ ?+ ?= ?? ?/))

(setq god-mode-translate-alist
      '(;; C-[ is a prefix key (ESC), remap here.
        ("C-x C-[" "C-x [") ;;("C-x C-]" "C-x ]")
        ;; use bracket for navigation
        ("C-[" "C-M-a") ("C-]" "C-M-e")
        ;; one-key command that makes most sense
        ("C-`" "C-x `") ("C-#" "C-x #") ("C-z" "C-x z")
        ;; C-i is interpreted as TAB, remap here.
        ("M-g C-i" "M-g i")
        ;; one-key command that maps to M-?
        ("C-~" "M-~") ("C-!" "M-!") ("C-@" "M-@") ("C-$" "M-$") ("C-%" "M-%")
        ("C-^" "M-^") ("C-&" "M-&") ("C-*" "M-*") ("C-(" "M-(") ("C-)" "M-)")
        ("C-_" "M-_") ("C-+" "M-+") ("C-=" "M-=") ("C-{" "M-{") ("C-}" "M-}")
        ("C-:" "M-:") ("C-;" "M-;") ; M-' is for abbrev, not useful in god-mode
        ("C-|" "M-|") ("C-\\" "M-\\")
        ("C-<" "M-<") ("C->" "M->") ("C-," "M-,") ("C-." "M-.")))

(defun z-god-mode-enabled-hook ()
  (mortal-mode 0)
  ;; only change cursor once for selected window. eldoc will trigger
  ;; major mode change on its buffer thus triggering this hook. If the
  ;; focusted buffer is in mortal mode, we'll mess up the cursor (for
  ;; the whole frame).
  (if (eq (get-buffer-window) (selected-window))
      (set-cursor-type-all-frames 'box))
  (setq-local z-god-saved-input-method current-input-method)
  (if current-input-method
      (deactivate-input-method))
  (setq-local z-god-saved-view-mode view-mode)
  (if view-mode
      (view-mode -1)))
(add-hook 'god-mode-enabled-hook 'z-god-mode-enabled-hook)

(defun z-god-mode-disabled-hook ()
  (if (eq (get-buffer-window) (selected-window))
      (set-cursor-type-all-frames 'bar))
  (if z-god-saved-input-method
      (set-input-method z-god-saved-input-method))
  (if z-god-saved-view-mode
      (view-mode 1)))
(add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook)

;; Enter god-mode after all the other stuff has been loaded.
(add-hook 'after-init-hook #'god-mode-all)

(provide 'init-god)
