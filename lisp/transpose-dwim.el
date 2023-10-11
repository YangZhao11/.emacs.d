;;; transpose-dwim -- Helpful transpose function -*- lexical-binding: t -*-
;;; Commentary:
;;   Provide transpose-dwim. Also an easier version of transpose-regions

;;; Code:

;; markers to remember where we want to transpose
(setq transpose-region-marker1 (make-marker))
(setq transpose-region-marker2 (make-marker))

(defun transpose-dwim-delete-markers ()
  (set-marker transpose-region-marker1 nil)
  (set-marker transpose-region-marker2 nil))

;;; autoload
(defun transpose-dwim-regions (beg end)
  "Similar to anchored-transpose or transpose-regions.
   2 step process to transpose 2 things."
  (interactive "r")
  (unless (and beg end)
    (user-error "The mark is not set now"))
  (cond
   ;; first call
   ((not (marker-position transpose-region-marker1))
    (set-marker transpose-region-marker1 beg)
    (set-marker transpose-region-marker2 end)
    (setq deactivate-mark 't)
    (message "Will transpose text \"%s\""
             (query-replace-descr
              (buffer-substring-no-properties beg end))))
   ;; same buffer
   ((eq (current-buffer)
        (marker-buffer transpose-region-marker1))
    (let ((beg2 (marker-position transpose-region-marker1))
          (end2 (marker-position transpose-region-marker2)))
      ;; delete markers first, we always confirm the action
      (transpose-dwim-delete-markers)
      (eval `(transpose-regions . ,(sort (list beg end beg2 end2) '<)))))

   ;; different buffer
   ('t
    (let (s1 s2
          (beg2 (marker-position transpose-region-marker1))
          (end2 (marker-position transpose-region-marker2))
          (buf2 (marker-buffer transpose-region-marker1)))
      (transpose-dwim-delete-markers)
      (setq s1 (filter-buffer-substring beg end 'delete))
      (condition-case nil
          (let ((s2 (with-current-buffer buf2
                      (filter-buffer-substring beg2 end2 'delete))))
            (insert s2)
            (with-current-buffer buf2
              (save-excursion
                (goto-char beg2)
                (insert s1))))
        ((buffer-read-only test-read-only)
         (insert s1)
         (signal 'text-read-only (list buf2))))))))

(defvar transpose-args-exclude-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "Modes for which we do not attempt transpose-args")

;;; autoload
(defun transpose-dwim (arg)
  "Transpose args, sexps, or sentences. "
  (interactive "*p")
  (cond ((derived-mode-p 'text-mode)
         (call-interactively #'transpose-sentences))
        ((and ;; We wouldn't need this for smie enabled modes.
          (not (memq major-mode transpose-args-exclude-modes))
          (not (eq forward-sexp-function #'smie-forward-sexp-command))
          (bounds-of-thing-at-point 'arg))
         (call-interactively #'transpose-args))
        ('t
         (call-interactively #'transpose-sexps))))
