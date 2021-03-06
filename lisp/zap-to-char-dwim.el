;;; zap-to-char-dwim -- zap to char with some default guesses -*- lexical-binding: t -*-
;;; Commentary:
;;   An enhanced version of zap-up-to-char that specially handles
;;   right parenthesis and space.

;;; Code:
(require 'misc)                         ; for zap-up-to-char

(defun backward-sexp-point (arg)
  "Returns point after calling `backward-sexp'."
  (save-excursion
    (backward-sexp arg)
    (point)))

(defun zap-to-char-matching-pairs (arg char)
  "Zap up to ARG'th CHAR, considering matching parens."
  (let ((direction (if (>= arg 0) 1 -1))
        (start (point)))
    (search-forward (char-to-string char) nil nil direction)
    (while (if (> direction 0)
               (>= (backward-sexp-point direction) start)
               (<= (backward-sexp-point direction) start))
      (search-forward (char-to-string char) nil nil direction))
    (backward-char direction)
    (kill-region start (point))))

(defun zap-to-space (arg)
  "Zap up to ARG'th consecutive space."
  (let ((direction (if (>= arg 0) 1 -1))
        (on-space (if (>= arg 0)
                      (looking-at "\\s ")
                    (looking-back "\\s " (1- (point)))))
        (has-space (if (>= arg 0)
                      (looking-back "\\s " (1- (point)))
                    (looking-at "\\s ")))
        (sep-regex (if (>= arg 0) "\\S \\s " "\\s \\S "))
        (start (point)))
    (when on-space
      (re-search-forward "\\S " nil nil direction)
      (backward-char direction))
    (re-search-forward sep-regex nil nil arg)
    (backward-char direction)
    (when has-space
      (re-search-forward "\\S " nil nil direction)
      (backward-char direction))
    (kill-region start (point))))

(defun zap-to-end-of-list (arg)
  "Zap to end of the list where the point is in."
  (let ((direction (if (>= arg 0) 1 -1))
        (start (point)))
    (up-list direction t t)
    (backward-char direction)
    (kill-region start (point))))

;;;###autoload
(defun zap-to-char-dwim (arg char)
  "Delete text up to ARG'th CHAR similar to `zap-up-to-char', except:
- Repeat last key (press \\[zap-to-char-dwim] a second time)
  to `zap-to-end-of-list'.
- zap to space handles consequtive spaces
- zap to right parenthses goes to the end of enclosing list

ARG is passed to respective functions mentioned here."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-key "Zap to: ")))
  (cond
   ((eq char last-command-event)
    (zap-to-end-of-list arg))
   ((eq (char-syntax char) (if (< arg 0) ?\( ?\)))
    (zap-to-char-matching-pairs arg char))
   ((eq char ?\ )
    (zap-to-space arg))
   (:else
    (zap-up-to-char arg char))))

;;;###autoload
(defun zap-back-to-char-dwim (arg char)
  "Call `zap-to-char-dwim' with negated ARG and CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-key "Zap back to: ")))
  (zap-to-char-dwim (- arg) char))

(provide 'zap-to-char-dwim)
;;; zap-to-char-dwim.el ends here
