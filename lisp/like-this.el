;;; like-this -- Move to next thing like this -*- lexical-binding: t -*-
;;; Commentary:
;;    Move to next / previous thing like the thing at point. Here
;;    "thing" can be decided using font-lock faces or thing-at-pt
;;    library. Configure `like-this-try-faces' and
;;    `like-this-try-things' for which faces and things are
;;    considered.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar like-this-try-faces
  '(hi-yellow hi-pink hi-green hi-blue info-menu-header button)
  "Special faces that are searched for")

(defun like-this--face-matches (face)
  "Returns true if face is active at point"
  (let ((f (get-text-property (point) 'face)))
    (cond ((not f) nil)
          ((symbolp f) (and (eq face f) face))
          ((listp f) (and (memq face f) face))
          (:else nil))))

(defun like-this--next-matching-face (face)
  "Move to next place where the face matches"
  (while (like-this--face-matches face)
    (goto-char (next-single-property-change (point) 'face)))
  (while (not (like-this--face-matches face))
    (goto-char (next-single-property-change (point) 'face))))

(defun like-this--previous-matching-face (face)
  "Move to previous place where the face matches"
  (while (like-this--face-matches face)
    (goto-char (previous-single-property-change (point) 'face)))
  (while (not (like-this--face-matches face))
    (goto-char (previous-single-property-change (point) 'face))))

(defun like-this--get-face ()
  "Return a face to search for, if any"
  (seq-some 'like-this--face-matches like-this-try-faces))

(defun like-this--next-face (face arg)
  (while (> arg 0)
    (like-this--next-matching-face face)
    (setq arg (1- arg)))
  (while (< arg 0)
    (like-this--previous-matching-face face)
    (setq arg (1+ arg))))

;; TODO: use `isearch-forward-thing-at-point' setting
(defvar like-this-try-things
  '(email symbol word)
  "Thing at point to try search for")

(defun like-this--get-thing-bounds ()
  "Return bound of thing at point, using priority in
`like-this-try-things'."
  (let ((to-try like-this-try-things)
        (bounds)
        (thing))
    (while (and (not bounds)
                to-try)
      (setq bounds (bounds-of-thing-at-point (car to-try)))
      (setq thing (car to-try))
      (setq to-try (cdr to-try)))
    (if bounds (cons thing bounds))))

(defun like-this--next-thing-bounds (bounds arg)
  "Search for next ARG'th occurrence of thing between BEG and END."
  (let* ((thing (car bounds))
         (beg (cadr bounds))
         (end (cddr bounds))
         (str (buffer-substring-no-properties beg end))
         (regexp (concat
                  (if (eq thing 'symbol) "\\_<" "\\b")
                  (regexp-quote str)
                  (if (eq thing 'symbol) "\\_>" "\\b")))
         (offset (if (and (<= beg (point))
                          (<= (point) end))
                     (if (> arg 0) (- (point) end)
                       (- (point) beg))
                   0)))
    (goto-char (if (> arg 0) end beg))
    (re-search-forward regexp nil nil arg)
    (forward-char offset)))

;;;###autoload
(defun like-this-next (&optional arg)
  "Navigate to ARG'th thing like this at point."
  (interactive "p")
  (if-let ((face (like-this--get-face)))
      (like-this--next-face face arg)
    (if-let ((bounds (like-this--get-thing-bounds)))
        (like-this--next-thing-bounds bounds arg)
      (user-error "Not sure what to look for."))))

;;;###autoload
(defun like-this-prev (&optional arg)
  "Navigate to previous ARG'th thing like this at point."
  (interactive "p")
  (like-this-next (- arg)))

(provide 'like-this)
;;; like-this.el ends here
