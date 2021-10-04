; -*- coding: utf-8; lexical-binding: t -*-

;;; Code:
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
           (param (cond ((eq shape 'bar) "5")
                        ((eq shape 'hbar) "3")
                        (:else "1"))))
      (send-string-to-terminal
       (concat "\e[" param " q"))))))

(defun set-cursor-type-all-frames (spec)
  "Set cursor to SPEC for all frames."
  (dolist (f (frame-list))
    (with-selected-frame f (set-cursor-type spec))))

(defun mortal-mode-exit ()
  "Exit mortal-mode and resume god mode."
  (interactive)
  (god-local-mode-resume)
  (mortal-mode 0))
(defvar mortal-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") 'mortal-mode-exit)
    m)
  "Keymap for `mortal-mode'.")

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
(dolist (i '(?s ?r ?w ?v ?% ?< ?>))
  (define-key god-mode-isearch-map
    (char-to-string i) 'god-mode-self-insert))

(setq god-mod-alist-default '((nil . "C-") (?g . "M-") (?h . "C-M-")))
(setq god-mod-alist god-mod-alist-default)
(setq god-exempt-major-modes nil
      god-exempt-predicates nil
      god-mode-can-omit-literal-key 't)

;; Avoid remapped self-insert-command
(defalias 'true-self-insert-command 'self-insert-command)

(bind-keys ("(" . true-self-insert-command)
           (")" . true-self-insert-command))

(setq god-mode-low-priority-exempt
      '(self-insert-command
        c-electric-lt-gt c-electric-brace
        sgml-slash
        ess-smart-pipe ess-insert-assign ess-cycle-assign))

(setq god-mode-low-priority-keys
      '(?z ?# ?q ?\[ ?\] ?U ?`
        ?~ ?! ?@ ?$ ?% ?^ ?& ?* ?{ ?}
        ?< ?> ?: ?| ?\\ ?+ ?= ?? ?/))

(defun god-mode-toggle-sticky-meta (arg)
  (interactive (list (string= "M-" (cdr (assoc nil god-mod-alist)))))
  (if arg
      (progn (setq god-mod-alist god-mod-alist-default)
             (define-key god-local-mode-map "g" #'god-mode-self-insert))
    (setq god-mod-alist '((nil . "M-") (?h . "C-M-")))
    (define-key god-local-mode-map "g" #'god-mode-toggle-sticky-meta))
  (force-mode-line-update))

(defun god-mode-toggle-sticky-cm (arg)
  (interactive (list (string= "C-M-" (cdr (assoc nil god-mod-alist)))))
  (if arg
      (progn (setq god-mod-alist god-mod-alist-default)
             (define-key god-local-mode-map "h" #'god-mode-self-insert))
    (setq god-mod-alist '((nil . "C-M-") (?g . "M-")))
    (define-key god-local-mode-map "h" #'god-mode-toggle-sticky-cm))
    (force-mode-line-update))

(bind-keys :map god-local-mode-map
           ("i" . mortal-mode)
           ("G" . god-mode-toggle-sticky-meta)
           ("H" . god-mode-toggle-sticky-cm)
           ("(") (")"))

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
        ("C-^" "M-^") ("C-&" "M-&") ("C-*" "M-*") ("C-{" "M-{") ("C-}" "M-}")
        ("C-<" "M-<") ("C->" "M->") ("C-:" "M-:") ("C-|" "M-|") ("C-\\" "M-\\")
        ("C-+" "M-+") ("C-=" "M-=") ("C-," "M-,") ("C-." "M-.")))

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
(add-hook 'after-init-hook #'god-mode-all)

(provide 'init-god)
