;;; format-expand --- expand a template -*- lexical-binding: t -*-

;;; Commentary:

;;; Select a template string, and expand it like a loop, using loop
;;; variable x. For example, if the template is "%d-%(1+ x)d", and we
;;; loop through 1 to 10, we will get 1-2, 2-3, ... till 10-11. We do
;;; not automatically add a separator.

;; util functions for date handling
(require 'time-date)

(defconst parse-time-short-alist
  '((?y 1 :year 1)
    (?m 1 :month 2)
    ;; we do not allow mixing w and d for now, not doing math at the moment
    (?w 7 :day 3)
    (?d 1 :day 3)
    (?H 1 :hour 5)
    (?h 1 :hour 5)
    (?M 1 :minute 6)
    (?S 1 :second 7)
    (?s 1 :second 8))
  "Units for `parse-time-interval-short'. This is an alist with the unit
letter, construct used for `make-decoded-time', and an order, which goes
from large to smaller units."
  )

(defun parse-time-interval-short (string)
  "Parse time interval in short format.

String should be something like 1m14d, here meaning 1 month + 14 days.
See `parse-time-short-alist' for list of units supported. We expect the
units go from large to small, so 1M14d is an invalid input and will
trigger an error.
"
  (let ((start 0)
        (result nil))
    (while (string-match "\\(-?[0-9]\\)\\([a-zA-Z]\\)" string start)
      (let* ((order 0)
             (num (string-to-number (match-string 1 string)))
             (char (aref (match-string 2 string) 0))
             (entry (alist-get char parse-time-short-alist)))
        (unless entry
          (error "Time interval unit %c not recognized." char))
        (when (<= (nth 2 entry) order)
          (error "Time interval unit %c appeared out of order." char))
        (setq order (nth 2 entry))
        (push (list (nth 1 entry) (* (nth 0 entry) num)) result))
      (setq start (match-end 0)))
    (apply 'make-decoded-time (apply 'append (nreverse result)))))

(defun time-sequence (from &optional to inc)
  "Generate a sequence of time values. The input is decoded time but output
is encoded.

FROM, TO and INC are decoded time, coming from `parse-time-string' or
`make-decoded-time'."
  (when (stringp from)
    (setq from (decoded-time-set-defaults (parse-time-string from))))
  (unless to (setq to from))
  (when (stringp to)
      (setq to (decoded-time-set-defaults (parse-time-string to))))
  (unless inc (setq inc (make-decoded-time :day 1)))
  (when (stringp inc)
    (setq inc (parse-time-interval-short inc)))
  (let* ((current from)
         (target-time (encode-time to))
         (result nil))
    (while (not (time-less-p target-time (encode-time current)))
      (push (encode-time current) result)
      (setq current (decoded-time-add current inc)))
    (nreverse result)))

(defun timestampp (ts)
  "Returns non-nil if ts is a timestamp."
  (ignore-errors (time-equal-p ts ts)))

(defun valid-number-string-p (string)
  "Return t if STRING can be parsed as a number, nil otherwise."
  (ignore-errors
    (let ((obj (car (read-from-string string))))
      (numberp obj))))

(defun valid-time-string-p (string)
  "Return t if STRING can be parsed as a valid date or time, nil otherwise."
  (and (not (valid-number-string-p string))
       (when-let* ((parsed (ignore-errors (parse-time-string string))))
         (not (null (ignore-errors
                      (encode-time (decoded-time-set-defaults parsed))))))))

(defun format--read-sequence (prefix)
  "Read a sequence. With any prefix arg, prompt for lisp. Otherwise we
accept the following formats:
number to number [by number]
time to time [by time-interval]

If a sigle number or time is given, we follow up asking the `to' part,
which can include an optional `by' part.

The car of the result is a type; can be the symbol `number' or `time'.
"
  (if prefix
      (let* ((exp
              (read--expression
               "Seq lisp: "
               (format "(number-sequence 1 %d)"
                       (if (numberp prefix) prefix 4))))
             (result
              (eval (let ((lexical-binding t)) (macroexpand-all exp))
                    t)))
        (if (timestampp (car result))
            (cons 'time result)
          ;; 'number actually handles strings in the same formatting
          (cons 'number result)))
    ;; no prefix, read from-to-inc
    (let* ((from (read-from-minibuffer "Seq from: "))
           to inc)
      (when-let* ((parts (split-string from " to "))
                 ((cdr parts)))
          (setq from (car parts))
          (setq to (cadr parts)))
      (unless to
        (setq to (read-from-minibuffer "Seq to [by inc]: ")))
      (when-let* ((parts (split-string to " by "))
                 ((cdr parts)))
          (setq to (car parts))
          (setq inc (cadr parts)))
      (if (valid-time-string-p from)
          (cons 'time (time-sequence from to inc))
        (cons 'number (number-sequence
                       (string-to-number from)
                       (string-to-number to)
                       (if (null inc)
                           inc
                         (string-to-number inc))))))))

(defconst format--format-str
  (let ((flags "[+ #-0]\\{0,1\\}")
        (width "[0-9]*")
        (precision "\\(?:\\.[0-9]+\\)?")
        (character "[sdoxXefgcS]"))
    (format "%s%s%s%s"
            flags width precision character))
  "Matches format string (stuff after `%').")

(defvar format-loop-variable 'i
  "Symbol used as loop variable in `format-expand'.")

(defun format--parse-template (str)
  "Parse % forms in STR, return a list of (STR VARS FORMS).

Each element of FORMS corresponds to a `format'-style % form in STR."
  (let ((start 0)
        (default-var format-loop-variable)
        vars
        forms beg fexp)
    (condition-case nil
        (while (setq beg (string-match "%" str start))
          (setq start (1+ beg))

          (cond
           ;; skip %%
           ((= ?% (aref str start))
            (cl-incf start))

            ((= ?\( (aref str start))
             (cl-destructuring-bind (sexp . end)
                 (read-from-string str start)
               (cond ((eq (car sexp) 'setq)
                      (unless (eq (cadr sexp) format-loop-variable)
                        (push (cadr sexp) vars)))
                     ((eq (car sexp) 'setq*)
                      (unless (eq (cadr sexp) format-loop-variable)
                        (push (cadr sexp) vars))
                      (setcar sexp 'setq)
                      (setq default-var (cadr sexp)))
                     ((eq (car sexp) 'id)
                      (setq sexp (cadr sexp)))
                     ((eq (car sexp) 'id*)
                      (setq default-var (cadr sexp))
                      (setq sexp (cadr sexp))))
               (push sexp forms)
               (setq fexp (string-match format--format-str str end))
               (setq str (concat (substring str 0 start)
                                 (if (eq fexp end) "" "s")
                                 (substring str end)))))
            (t (push default-var forms))))
      (error (message "Malformed sexp: %s" (substring str start))))
    (list str vars (nreverse forms))))

(defun format-time-string-vars (string &rest objects)
  ;; TODO: actually implement this. Right now we just use the first value.
  (let ((val (car objects)))
    (mapc (lambda (v) (when (not (equal val v))
                        (warn "different time value not supported")))
          objects))
  (format-time-string string (car objects)))

;;;###autoload
(defun format-expand (beg end seq)
  "Expand and repeat region as if it is a format string, using items in SEQ
to fill out the % constructs.

The format string we support is exactly like what is described in
`format', except that the field description is not supported. Instead,
use parenthesis to indicate an expression to evaluate. For example,
%(identity emacs-version)s should give you the version string. The `s'
after a sexp can be omitted. In sexps, symbol `i' is available as loop
iterator (configurable using `format-loop-variable').

In %() forms, the following are specially handled:
- (setq var form) will assign var in a local environment, reset per
  iteration.
- (setq* var form) acts like setq, and set the default variable in
  following %-constructs.
- (id form) acts like identity.
- (id* form) acts like id, and set the default variable in following
  %-constructs.

BEG and END marks the format string, and defaults to active region or
the current line if region is not active.

SEQ is read through `format--read-sequence'. Accept from, to, inc, which
can be specified using A to B by C format. When prefix is specified,
accept a lisp expression.

Push mark if region is not active."

  (interactive
   (list (or (use-region-beginning) (line-beginning-position))
         (or (use-region-end)
             (let ((e (line-end-position)))
               (when (eq e (point-max))
                 (save-excursion
                   (goto-char e)
                   (insert "\n")))
               (1+ e)))
         (format--read-sequence current-prefix-arg)))
  (let* ((str (filter-buffer-substring beg end t))
         (parsed (format--parse-template str))
         (format-template (car parsed))
         (vars (cadr parsed))
         (form-vars-env (mapc (lambda (symbol) (cons symbol nil)) vars))
         (forms (caddr parsed))
         (type (car seq))
         (format-fun (if (eq type 'number) 'format 'format-time-string-vars))
         env)
    (or (use-region-p) (push-mark))
    (dolist (iter (cdr seq))
      (setq env (cons (cons format-loop-variable iter)
                      form-vars-env))
      (insert
       (apply format-fun format-template
              (mapcar (lambda (sexp) (eval sexp env))
                      forms))))))

(provide 'format-expand)
;;; format-expand.el ends here
