;;; like-this -- Move to next thing like this -*- lexical-binding: t -*-
;;; Commentary:
;;    Move to next / previous thing like the thing at point. Here
;;    "thing" can be decided using font-lock faces or thing-at-pt
;;    library. Configure `like-this-try-list' for which faces and
;;    things are considered.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defvar like-this-try-list
  '((face . hi-yellow)
    (face . hi-pink)
    (face . hi-green)
    (face . hi-blue)
    (thing . email)
    (thing . symbol)
    (thing . word)
    (face . info-menu-header)
    (face . button))
  "Things to try look for.")

;; Remember the last thing we got, so successive calls do not
;; accidentally change what we search for.
(defvar like-this--last-match nil)

;; Handle faces case
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


(defun like-this--next-face (face arg)
  (while (> arg 0)
    (like-this--next-matching-face face)
    (setq arg (1- arg)))
  (while (< arg 0)
    (like-this--previous-matching-face face)
    (setq arg (1+ arg))))

;; Handle thing-at-point cases
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

(defun like-this--find-match ()
  "Find a thing to search for. For face, return (face . face-to-search)
pair. For thing, return (thing bounds) list."
  (seq-some
   (lambda (try-item)
     (let ((type (car try-item))
           (s (cdr try-item)))
       (cond ((eq type 'face)
              (if (like-this--face-matches s)
                  try-item))
             ((eq type 'thing)
              (if-let* ((bounds (bounds-of-thing-at-point s)))
                  (cons s bounds))))))
   like-this-try-list))

;;;###autoload
(defun like-this-next (&optional arg)
  "Navigate to ARG'th thing like this at point."
  (interactive "p")
  (when-let* ((match (like-this--find-match)))
      (setq like-this--last-match match)
      (cond ((eq (car match) 'face)
             (like-this--next-face (cdr match) arg))
            (:else                      ; other cases are things
             (like-this--next-thing-bounds match arg)))
    :else
    (user-error "Not sure what to look for.")))

;;;###autoload
(defun like-this-prev (&optional arg)
  "Navigate to previous ARG'th thing like this at point."
  (interactive "p")
  (like-this-next (- arg)))

(provide 'like-this)
;;; like-this.el ends here
