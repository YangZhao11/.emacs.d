;;; transpose-dwim -- Helpful transpose function -*- lexical-binding: t -*-
;;; Commentary:
;;   Provide `transpose-dwim'. Also `transpose-dwim-regions' is an easier
;;   version of `transpose-regions'.

;;; Code:

(defvar transpose-dwim--beg (make-marker)
  "Marker used by `transpose-dwim-regions', to remember the region
in the first step.")
(defvar transpose-dwim--end (make-marker)
    "Marker used by `transpose-dwim-regions', to remember the region
in the first step.")

(defun transpose-dwim--delete-markers ()
  (set-marker transpose-dwim--beg nil)
  (set-marker transpose-dwim--end nil))

;;;###autoload
(defun transpose-dwim-regions (beg end)
  "Transpose in 2 steps, similar to `anchored-transpose'.

To use, mark the region we want to transpose then call this
function, then do the same for the second part; on the second
call the two regions will be transposed."
  (interactive "r")
  (unless (and beg end)
    (user-error "The mark is not set now"))
  (cond
   ;; first call
   ((not (marker-position transpose-dwim--beg))
    (set-marker transpose-dwim--beg beg)
    (set-marker transpose-dwim--end end)
    (setq deactivate-mark 't)
    (message "Will transpose text \"%s\""
             (query-replace-descr
              (buffer-substring-no-properties beg end))))
   ;; second call, same buffer
   ((eq (current-buffer)
        (marker-buffer transpose-dwim--beg))
    (let ((beg2 (marker-position transpose-dwim--beg))
          (end2 (marker-position transpose-dwim--end)))
      ;; delete markers first, we always confirm the action
      (transpose-dwim--delete-markers)
      ;; transpose-regions may fail, e.g. in read-only buffers
      (apply #'transpose-regions (sort (list beg end beg2 end2) '<))))

   ;; second call, different buffer
   ('t
    (let (s1
          (beg2 (marker-position transpose-dwim--beg))
          (end2 (marker-position transpose-dwim--end))
          (buf2 (marker-buffer transpose-dwim--beg)))
      (transpose-dwim--delete-markers)
      ;; If deleting the first string fails, we are OK.
      (setq s1 (filter-buffer-substring beg end 'delete))
      (condition-case nil
          (let ((s2 (with-current-buffer buf2
                      (filter-buffer-substring beg2 end2 'delete))))
            (insert s2)
            (with-current-buffer buf2
              (save-excursion
                (goto-char beg2)
                (insert s1))))
        ((buffer-read-only text-read-only)
         ;; Deleting the second string failed. Recover the first
         ;; string then signal error.
         (insert s1)
         (signal 'text-read-only (list buf2))))))))

(defvar transpose-args-exclude-modes
  nil
  "Modes for which we do not attempt transpose-args")

;;;###autoload
(defun transpose-dwim (arg &optional interactive)
  "Transpose args, sexps, or sentences. "
  (interactive "*p\nd")
  (cond ((derived-mode-p 'text-mode)
         (transpose-sentences arg))
        ((and ;; We wouldn't need this for smie enabled modes.
          (not (memq major-mode transpose-args-exclude-modes))
          (not (eq forward-sexp-function #'smie-forward-sexp-command))
          (bounds-of-thing-at-point 'arg))
         (transpose-args arg))
        ('t
         (transpose-sexps arg interactive))))

(provide 'transpose-dwim)
;;; transpose-dwim.el ends here
