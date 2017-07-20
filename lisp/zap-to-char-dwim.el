;;; zap-to-char-dwim -- zap to char with some default guesses -*- lexical-binding: t -*-
;;; Commentary:
;;

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

;;;###autoload
(defun zap-to-char-dwim (arg char)
  "Delete text up to ARG'th CHAR similar to `zap-up-to-char', except:
- Repeat last key (press \\[zap-to-char-dwim] a second time)
  again for `zap-to-char'
- zap to space handles consequtive spaces
- zap to right parenthses goes to the end of enclosing list

ARG is passed to respective functions mentioned here."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap to: " 't)))
  (cond
   ((eq char last-command-event)
    (setq this-original-command 'zap-to-char
          this-command 'zap-to-char
          real-this-command 'zap-to-char)
    (call-interactively #'zap-to-char))
   ((eq (char-syntax char) (if (< arg 0) ?\( ?\)))
    (zap-to-char-matching-pairs arg char))
   ((eq char ?\ )
    (zap-to-space arg))
   ('t
    (zap-up-to-char arg char))))

;;;###autoload
(defun zap-back-to-char-dwim (arg char)
  "Call `zap-to-char-dwim' with negated ARG and CHAR."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap back to: " 't)))
  (zap-to-char-dwim (- arg) char))

(provide 'zap-to-char-dwim)
;;; zap-to-char-dwim.el ends here
