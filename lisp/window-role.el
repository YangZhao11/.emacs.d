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

(defun window-role-display (role &rest args)
  "Display a window in ROLE.

A buffer suitable for ROLE is searched, if not found the new-buffer-func
of this role is called. Then display this buffer, with role symbol set,
so that it can match `disaplay-buffer-alist' entries with condition
like (category . ROLE)."
  (interactive)
  (let* ((pred (alist-get 'predicate (symbol-value role)))
         (buf (cl-find-if
            (lambda (b)
              (apply #'buffer-match-p pred b args))
            (buffer-list)))
         win)

    (unless buf
      (let* ((new-buffer-func
              (alist-get 'new-buffer-func (symbol-value role)))
             (display-buffer-overriding-action
                         `(nil . ((category . ,role)))))
        (if new-buffer-func
            (setq buf (apply new-buffer-func args))
          (error "No buffer matching `%s'" role))))

    (unless buf
      (error "Failed to create buffer for `%s'" role))

    (setq win (or (get-buffer-window buf)
                  (display-buffer buf `(nil (category . ,role)))))
    (set-window-parameter win 'window-role (cons role args))
    (force-mode-line-update)
    win))

(defun window-role-toggle (role &rest args)
  "Toggle ROLE on current window."
  (let ((role-param (cons role args)))
    (set-window-parameter
     (selected-window) 'window-role
     (unless (equal (window-parameter (selected-window) 'window-role)
                    role-param)
       role-param))
    (force-mode-line-update)))

(defun window-role-toggle-window (role &rest args)
  "Show / hide window of ROLE.

If current window is of ROLE, delete it. If there is an unselected
window of ROLE, select it. Otherwise create one using
new-buffer-func on ROLE."
  (cond
   ((equal (window-parameter (selected-window) 'window-role)
           (cons role args))
    (condition-case nil
        (delete-window)
      (error
       ;; Can not delete window, disable window-role on it.
       (set-window-parameter (selected-window) 'window-role nil)
       (force-mode-line-update))))
   ((if-let* ((win (window-with-parameter
                    'window-role (cons role args))))
        (select-window win)))
   (t
    (select-window (apply #'window-role-display role args)))))

;;Customize `switch-to-prev-buffer-skip' to respect window-role parameter.
;;parameter on windows. This is used by `previous-buffer' and
;;`next-buffer'.
(defun window-role-should-skip (window buffer bury-or-kill)
  (let* ((role-param (window-parameter window 'window-role))
         (role (car role-param))
         (args (cdr role-param)))
    (and role
         (not (apply #'buffer-match-p
                     (alist-get 'predicate (symbol-value role))
                     buffer args)))))
(setq switch-to-prev-buffer-skip 'window-role-should-skip)
(add-to-list 'window-persistent-parameters '(window-role . writable))


(defun mode-line-window-role ()
  (if-let* ((role-param (window-parameter (selected-window) 'window-role))
            (role (car role-param)))
    (propertize (format-mode-line (alist-get 'mode-line (symbol-value role)))
                'help-echo (documentation-property role 'variable-documentation))))

(provide 'window-role)
