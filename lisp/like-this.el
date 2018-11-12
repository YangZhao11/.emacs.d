;;; like-this -- Move to next thing like this -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(defvar like-this-try-things
  '(email symbol word list))

(setq like-this--thing nil)

(defun like-this-bounds ()
  "Return bound of thing at point, using priority in
`like-this-try-things'."
  (let ((to-try like-this-try-things)
        (bounds))
    (while (and (not bounds)
                to-try)
      (setq bounds (bounds-of-thing-at-point (car to-try)))
      (setq like-this--thing (car to-try))
      (setq to-try (cdr to-try)))
    bounds))

(defun like-this-in-bound (beg end arg)
  "Search for next ARG'th occurrence of thing between BEG and END."
  (interactive "r\np")
  (let* ((str (buffer-substring-no-properties beg end))
         (regexp (concat
                  (if (eq like-this--thing 'symbol) "\\_<" "\\b")
                  (regexp-quote str)
                  (if (eq like-this--thing 'symbol) "\\_>" "\\b")))
         (offset (if (and (<= beg (point))
                          (<= (point) end))
                     (if (> arg 0) (- (point) end)
                       (- (point) beg))
                   0)))
    (goto-char (if (> arg 0) end beg))
    (re-search-forward regexp nil nil arg)
    (forward-char offset)))

;;;###autoload
(defun like-this-next (arg)
  "Navigate to ARG'th thing like this at point."
  (interactive "p")
  (let ((bounds (like-this-bounds)))
    (unless bounds
      (error "Not sure what to look for."))
    (like-this-in-bound (car bounds) (cdr bounds) arg)))

;;;###autoload
(defun like-this-prev (arg)
  "Navigate to previous ARG'th thing like this at point."
  (interactive "p")
  (like-this-next (- arg)))

(provide 'like-this)
;;; like-this.el ends here
