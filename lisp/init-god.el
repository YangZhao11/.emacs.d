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

(bind-keys ("(" . true-self-insert-command)
           (")" . true-self-insert-command))

(bind-keys :map god-local-mode-map
           ("i" . mortal-mode)
           ("z" . god-mode-low-priority)
           ("q" . god-mode-low-priority)
           ("[" . god-mode-low-priority)
           ("]" . god-mode-low-priority)
           ("(") (")"))

;; Translate some second level modifier keys with C- prefix for easier
;; god-mode access. E.g. Translate "C-x C-1" to "C-x 1", so that in
;; god-mode I only need to press "x 1" for this key combination.
(setq god-mode-translate-alist
      '(("C-x C-1" "C-x 1") ("C-x C-2" "C-x 2") ("C-x C-3" "C-x 3") ("C-x C-4" "C-x 4" t)
        ("C-x C-5" "C-x 5" t) ("C-x C-6" "C-x 6" t) ("C-x C-7" "C-x 7") ("C-x C-8" "C-x 8" t)
        ("C-x C-9" "C-x 9") ("C-x C-0" "C-x 0") ("C-x C-[" "C-x [") ("C-x C-]" "C-x ]")
        ("C-x C-$" "C-x $") ("C-x C-," "C-x ,") ("C-x C-." "C-x .") ("C-x C-?" "C-x ?")
        ("M-g C-1" "M-g 1") ("M-g C-2" "M-g 2") ("M-g C-3" "M-g 3") ("M-g C-4" "M-g 4")
        ("M-g C-5" "M-g 5") ("M-g C-6" "M-g 6") ("M-g C-7" "M-g 7") ("M-g C-8" "M-g 8")
        ("M-g C-c" "M-g c") ("M-g C-n" "M-g n") ("M-g C-p" "M-g p")
        ("C-[" "C-M-a") ("C-]" "C-M-e") ("C-`" "C-x `") ("C-#" "C-x #") ("C-z" "C-x z")))

;; Translate C-? to M-?, bound it with low priority.
(dolist (i '("~" "!" "@" "$" "%" "^" "&" "*" "{" "}"
             "<" ">" ":" "|" "\\" "+" "=" "?"))
  (define-key god-local-mode-map (kbd i) 'god-mode-low-priority)
  (push (list (concat "C-" i) (concat "M-" i)) god-mode-translate-alist))

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
