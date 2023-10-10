;;; argatpt -- Edit pairs wrapping region -*- lexical-binding: t -*-
;;; Commentary:
;;   Support 'arg for thingatpt.el

;;; Code:

(defvar arg-separator-alist
  ;; todo: we don't handle alist here.
   '((emacs-lisp-mode . " +")
     (lisp-interaction-mode . " +"))
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
          (goto-char (match-beginning 0)))
        (backward-sexp 1)
        (if (or (looking-back sep (car b) 'greedy)
                (and (looking-back "[ \n\t]*" (car b))
                     (= (1- (match-beginning 0)) (car b))))
            (setq n (1- n)))))))

;;;###autoload
(defun transpose-args (&optional n)
  (interactive "^p")
  (transpose-subr #'forward-arg n))

(provide 'argatpt)
;;; argatpt.el ends here
