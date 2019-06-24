; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
(defvar z-god-mode-lighter "")
(defvar-local z-god-saved-input-method nil
  "Saved input method before god-mode")
(defvar-local z-god-saved-view-mode nil
  "Saved view-mode before god-mode")

(defun set-cursor-type (spec)
  "Set cursor type for current frame. This also works for
terminals with support for setting cursor type.

SPEC could be `box', 'bar', or `hbar'."
  (cond
   ((display-graphic-p)
    (modify-frame-parameters nil `((cursor-type . ,spec))))
   ((frame-terminal)
    (let* ((shape (or (car-safe spec) spec))
           (param (cond ((eq shape 'bar) "6")
                        ((eq shape 'hbar) "4")
                        (t "2"))))
      (send-string-to-terminal
       (concat "\e[" param " q"))))))

(defun set-cursor-type-all-frames (spec)
  "Set cursor to SPEC for all frames."
  (dolist (f (frame-list))
    (with-selected-frame f (set-cursor-type spec))))

(defvar mortal-mode-map (make-sparse-keymap)
  "Keymap for `mortal-mode'.")
(defun mortal-mode-exit ()
  "Exit mortal-mode and resume god mode." (interactive)
                          (god-local-mode-resume)
                          (mortal-mode 0))
(define-key mortal-mode-map (kbd "RET") 'mortal-mode-exit)

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

;; use-package god-mode :ensure
(require 'god-mode)

(bind-keys ("<home>" . god-mode-all))
(diminish 'god-local-mode)

(require 'god-mode-isearch)
(bind-keys :map isearch-mode-map
           ("<home>" . god-mode-isearch-activate))
(bind-keys :map god-mode-isearch-map
           ("<home>" . god-mode-isearch-disable))

(setq god-mod-alist '((nil . "C-") ("g" . "M-") ("h" . "C-M-")))
(setq god-exempt-major-modes nil
      god-exempt-predicates nil)

;; Avoid remapped self-insert-command
(defalias 'true-self-insert-command 'self-insert-command)

(defvar god-mode-low-priority-exempt
  '(c-electric-lt-gt c-electric-brace)
  "Commands that do not take precedence of `god-mode-low-priority-map'.")

(defvar god-mode-low-priority-map (make-sparse-keymap)
  "A low priority map that takes precedence after local maps.")

(defun god-mode-low-priority ()
  "Honor local binding first, then use `god-mode-low-priority-map'."
  (interactive)
  (let* ((keys (this-command-keys))
         (local-binding (local-key-binding keys))
         (binding (if (and local-binding
                           (not (memq local-binding god-mode-low-priority-exempt)))
                      local-binding
                    (lookup-key god-mode-low-priority-map keys))))
    (unless binding (error "God: unknown binding for `%s'"  keys))
    (cond ((commandp binding t)
           (setq binding (or (command-remapping binding) binding))
           (setq this-original-command binding)
           (setq this-command binding)
           ;; `real-this-command' is used by emacs to populate
           ;; `last-repeatable-command', which is used by `repeat'.
           (setq real-this-command binding)
           (call-interactively binding))
          ((keymapp binding)
           ;; help-form does not work, but actual key is.
           (setq help-form `(describe-vector ,(vector binding)))
           (set-transient-map binding nil (lambda () (setq help-form nil))))
          (t (execute-kbd-macro binding)))))

(bind-keys :map god-mode-low-priority-map
           ("q" . quoted-insert)
           ("[" . beginning-of-defun)
           ("]" . end-of-defun)
           ("(" . true-self-insert-command)
           (")" . true-self-insert-command)
           ("`" . next-error)
           ("#" . server-edit))

(bind-keys :map god-local-mode-map
           ("i" . mortal-mode)
           ("z" . repeat))

(defun god-mode-self-insert-on-meta ()
  "Copy of `god-mode-self-insert', except binding is M-key."
  (interactive)
  (let* ((initial-key (aref (this-command-keys-vector)
                            (- (length (this-command-keys-vector)) 1)))
         (key-string (concat "M-" (char-to-string initial-key)))
         (binding (key-binding (kbd key-string))))
    (unless binding (error "God: unknown key binding for `%s`" key-string))
    (setq this-original-command binding)
    (setq this-command binding)
    ;; `real-this-command' is used by emacs to populate
    ;; `last-repeatable-command', which is used by `repeat'.
    (setq real-this-command binding)
    (setq god-literal-sequence nil)
    (if (commandp binding t)
        (call-interactively binding)
      (execute-kbd-macro binding))))

;; bind symbols to M-? with low priority
(dolist (i '("~" "!" "@" "$" "%" "^" "&" "*" "{" "}"
             "<" ">" ":" "|" "\\" "+" "=" "?"))
  (define-key god-mode-low-priority-map (kbd i)
    'god-mode-self-insert-on-meta))

(dolist (b (cdr god-mode-low-priority-map))
  (define-key god-local-mode-map (char-to-string (car b))
    'god-mode-low-priority))

;; Bind some second level modifier keys with C- prefix for easier
;; god-mode access. Directly bind these to commands, instead of making
;; it a keyboard macro so that messages work in god-mode.
(dolist (bindings
         '(("C-x" "0" "1" "2" "3" "9" "[" "]" "$" "," "." "?") ; C-x C-[ does not work
           ("M-g" "1" "2" "3" "4" "5" "6" "7" "8" "c" "n" "p")))
  (let ((prefix (car bindings))
        (chars (cdr bindings)))
    (dolist (i chars)
      (define-key global-map (kbd (concat prefix " C-" i))
        (key-binding (kbd (concat prefix " " i)))))))

;; Make it so "x 4" in god mode translates to "C-x 4" prefix.
(defun god-mode-call-with-prefix (prefix)
  "Call god-mode key binding, as if prefix and literal key are in effect"
  (interactive)
  (let ((god-literal-sequence t))
    (call-interactively (god-mode-lookup-key-sequence nil prefix))))
(define-key global-map (kbd "C-x C-4")
  (lambda () (interactive) (god-mode-call-with-prefix "C-x 4")))
(define-key global-map (kbd "C-x C-5")
  (lambda () (interactive) (god-mode-call-with-prefix "C-x 5")))
(define-key global-map (kbd "C-x C-6")
  (lambda () (interactive) (god-mode-call-with-prefix "C-x 6")))
(define-key global-map (kbd "C-x C-8")
  (lambda () (interactive) (god-mode-call-with-prefix "C-x 8")))

(defun z-god-mode-enabled-hook ()
  ;; somehow this hook can be called multiple times on a buffer,
  ;; which messes up saving states here. Maybe consider using
  ;; post-command-hook to run this once.
  (mortal-mode 0)
  (set-cursor-type 'box)
  (setq-local z-god-saved-input-method current-input-method)
  (if current-input-method
      (deactivate-input-method))
  (setq-local z-god-saved-view-mode view-mode)
  (if view-mode
      (view-mode -1)))
(add-hook 'god-mode-enabled-hook 'z-god-mode-enabled-hook)

(defun z-god-mode-disabled-hook ()
  (set-cursor-type 'bar)
  (if z-god-saved-input-method
      (set-input-method z-god-saved-input-method))
  (if z-god-saved-view-mode
      (view-mode 1)))
(add-hook 'god-mode-disabled-hook 'z-god-mode-disabled-hook)

;; Enter god-mode after all the other stuff has been loaded.
(add-hook 'after-init-hook 'god-mode-all)
