;;; argatpt -- Edit pairs wrapping region -*- lexical-binding: t -*-
;;; Commentary:
;;   Support 'arg for thingatpt.el

;;; Code:

(defvar arg-separator-alist
  ;; todo: this does not work for looking-back to skip a separator.
  ;; '((emacs-lisp-mode . " +\\. +")
  ;;   (lisp-interaction-mode . " +\\. +"))
  nil
  "Mode-specific setting for arg separator.
See doc for `arg-separator-default' for details.")

(defvar arg-separator-default "[ \n\t]*,[ \n\t]*"
  "Separator between args. This should generaly not match an
  empty string, and should include any spaces allowed.")

(defun arg-separator (mode)
  (alist-get mode arg-separator-alist
             arg-separator-default))

;;;###autoload
(defun forward-arg (&optional n)
  "Forward N args. An arg is a comma separted item within a list."
  (interactive "^p")
  (or n (setq n 1))
  (if (< n 0)
      (backward-arg (- n))
    ;; normal n>0 case
    (when-let ((b (bounds-of-thing-at-point 'list))
               (sep (arg-separator major-mode)))
      (while (and (> n 0)
                  (< (1+ (point)) (cdr b)))
        (if (looking-at sep)
            (goto-char (match-end 0)))
        (forward-sexp 1)
        (if (or (looking-at-p sep)
                (and (looking-at "[ \n\t]*")
                     (= (1+ (match-end 0)) (cdr b))))
            (setq n (1- n)))))))

;;;###autoload
(defun backward-arg (&optional n)
  "Backward N args. An arg is a comma separted item within a list."
  (interactive "^p")
  (or n (setq n 1))
  (if (< n 0)
      (forward-arg (- n))
    ;; normal n > 0 case
    (when-let ((b (bounds-of-thing-at-point 'list))
               (sep (arg-separator major-mode)))
      (while (and (> n 0)
                  (>= (1- (point)) (car b)))
        (when (looking-back sep (car b) 'greedy)
            (message "skipping sep to %d" (match-beginning 0))
            (goto-char (match-beginning 0)))
        (backward-sexp 1)
        (if (or (looking-back sep (car b) 'greedy)
                (and (looking-back "[ \n\t]*" (car b))
                     (= (1- (match-beginning 0)) (car b))))
            (setq n (1- n)))))
    ))

;;;###autoload
(defun transpose-args (&optional n)
  (interactive "^p")
  (transpose-subr #'forward-arg n))

(defvar transpose-args-exclude-modes
  '(emacs-lisp-mode lisp-interaction-mode)
  "Modes for which we do not attempt transpose-args")


;;;###autoload
(defun transpose-dwim (arg)
  "Transpose args, sexps, or sentences. "
  (interactive "*p")
  (if (derived-mode-p 'prog-mode)
      (cond ((and ;; We wouldn't need this for smie enabled modes.
              (not (memq major-mode transpose-args-exclude-modes))
              (not (eq forward-sexp-function #'smie-forward-sexp-command))
              (bounds-of-thing-at-point 'arg))
             (call-interactively #'transpose-args))
            ('t (call-interactively #'transpose-sexps)))
    :else ; text mode
    (call-interactively #'transpose-sentences)))

(provide 'argatpt)
;;; argatpt.el ends here
