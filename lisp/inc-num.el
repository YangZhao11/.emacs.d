;;; inc-num -- Increment number at point -*- lexical-binding: t -*-
;;; Commentary:
;;    Provide command `inc-num-at-point', that increases number at point.

;;; Code:

(defvar inc-num--last-arg nil)

;;;###autoload
(defun inc-num-at-point (arg)
  "Increase number at point by ARG.

With repeated call, use the same ARG as previous invocation."
  (interactive
   (list (if (and (not current-prefix-arg)
                  (eq last-command 'inc-num-at-point))
             inc-num--last-arg
           (prefix-numeric-value current-prefix-arg))))
  (when-let* ((bounds (bounds-of-thing-at-point 'number))
              (num (string-to-number (buffer-substring (car bounds) (cdr bounds))))
              (target-str (number-to-string (+ num arg)))
              (new-len (length target-str))
              (offset (- (point) (car bounds))))
    (setq inc-num--last-arg arg)
    (undo-auto-amalgamate)
    (delete-region (car bounds) (cdr bounds))
    (insert target-str)
    (if (< offset new-len)
        (forward-char (- offset new-len)))))

;;;###autoload
(defun inc-num-in-region (arg &optional start end region-noncontiguous-p)
  "Increase number in region by ARG.

START, END and REGION-NONCONTIGUOUS-P are similar to `query-replace-regexp'."
  (declare (interactive-args
	    (start (use-region-beginning))
	    (end (use-region-end))
	    (region-noncontiguous-p (use-region-noncontiguous-p))))
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
	 ;; These are done separately here
	 ;; so that command-history will record these expressions
	 ;; rather than the values they had this time.
	 (use-region-beginning) (use-region-end)
	 (use-region-noncontiguous-p)))
  (let ((offset 0))
    (dolist (bounds (if region-noncontiguous-p
                        (funcall region-extract-function 'bounds)
                      (list (cons start end))))
      (goto-char (+ (car bounds) offset))
      (while (re-search-forward "[0-9]+" (+ (cdr bounds) offset) 't 1)
        (let* ((str (match-string 0))
               (num (string-to-number str))
               (target-str (number-to-string (+ num arg)))
               (str-diff (- (length target-str) (length str))))
          (replace-match target-str)
          (setq offset (+ offset str-diff)))))))

(provide 'inc-num)
;;; inc-num.el ends here
