;;; window-role -- Window softly dedicated to buffers -*- lexical-binding: t -*-
;;; Commentary:
;;    Give window a 'window-role parameter to make the window
;;    dedicated to a set of buffers that pass a predicate. Works along
;;    with display-buffer-alist.

(eval-when-compile
  (require 'cl-macs))

(defvar window-role-list '()
  "List of all window cagetory symbols.")

(cl-defmacro defvar-window-role
    (role &optional doc &key predicate args-func mode-line new-buffer-func)
  "Define variable ROLE.

PREDICATE is a form for `buffer-match-p', and decides which buffers are
eligible for window with ROLE. MODE-LINE is a mode-line construct that
can be added using (:eval mode-line-window-role). NEW-BUFFER-FUNC points
to a function that creates a new buffer for this role.

TODO: a ROLE can have arguments.
"
  (declare (indent defun) (doc-string 2))
  (let ((alist `((predicate . ,predicate)
                 (args-func . ,args-func)
                 (mode-line . ,mode-line)
                 (new-buffer-func . ,new-buffer-func))))
    `(progn (defvar ,role ',alist ,doc)
            (add-to-list 'window-role-list ',role))))

;; TODO: add arguments
(defun window-role-display (role)
  "Display a window in ROLE.

A buffer suitable for ROLE is searched, if not found the new-buffer-func
of this role is called. Then display this buffer, with role symbol set,
so that it can match `disaplay-buffer-alist' entries with condition
like (category . ROLE)."
  (interactive)
  (let* ((pred (alist-get 'predicate (symbol-value role)))
         (buf (cl-find-if
            (lambda (b)
              (buffer-match-p pred b))
            (buffer-list)))
         win)

    (unless buf
      (let* ((new-buffer-func
              (alist-get 'new-buffer-func (symbol-value role)))
             (display-buffer-overriding-action
                         `(nil . ((category . ,role)))))
        (setq buf (funcall new-buffer-func))))

    (setq win (or (get-buffer-window buf)
                  (display-buffer buf `(nil (category . ,role)))))
    (set-window-parameter win 'window-role role)
    (force-mode-line-update)
    win))

(defun window-role-toggle (role)
  "Toggle window of ROLE.

If current window is of ROLE, delete it. If there is an unselected
window of ROLE, select it. Otherwise create one using
new-buffer-func on ROLE."
    (cond
     ((eq (window-parameter (selected-window) 'window-role)
            role)
      (delete-window))
     ((if-let* ((win (window-with-parameter
                      'window-role role)))
          (select-window win)))
     (t
      (select-window (window-role-display role)))))

;;Customize `switch-to-prev-buffer-skip' to respect window-role parameter.
;;parameter on windows. This is used by `previous-buffer' and
;;`next-buffer'.
(defun window-role-should-skip (window buffer bury-or-kill)
  (let ((role-symbol (window-parameter window 'window-role)))
    (and role-symbol
         (not (buffer-match-p
               (alist-get 'predicate (symbol-value role-symbol))
               buffer)))))
(setq switch-to-prev-buffer-skip 'window-role-should-skip)
(add-to-list 'window-persistent-parameters '(window-role . writable))


;; A display-buffer-action. We decouple this and
;; `window-role-display', so that a buffer can be configured to always display in window of certain role
(defun display-buffer-in-window-role ()
  "")


(defun mode-line-window-role ()
  (let ((role (window-parameter (selected-window) 'window-role)))
    (if role
        (propertize (alist-get 'mode-line (symbol-value role))
                    'help-echo (documentation-property role 'variable-documentation)))))

(provide 'window-role)
