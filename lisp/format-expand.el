;;; format-expand -*- lexical-binding: t -*-

;; The format string we support is exactly like what is described in
;; `format', except that the field description is not supported.
;; Instead, use parenthesis to indicate an expression to evaluate. For
;; example, %(identity emacs-version)s should give you the version
;; string. The s after a sexp can be omitted.

;; In sexps, symbol `_' is available as loop iterator.

(defun format--parse-number-sequence (s)
  "Parse strings to number sequence.

   10 will be interpreted as 1 to 10.
   1 10 will be interpreted as 1 to 10.
   1 10 2 will be interpreted as 1 3 5 7 9.
   for more than 3 items interpret verbatim."
  (let ((n (read (format "(%s)" s))))
    (cond ((or (> (length n) 3)
               (not (seq-every-p #'numberp n)))
           n)
          ((eq (length n) 1)
           (number-sequence 1 (nth 0 n)))
          ((eq (length n) 2)
           (number-sequence (nth 0 n) (nth 1 n)))
          ((eq (length n) 3)
           (number-sequence (nth 0 n) (nth 1 n) (nth 2 n)))
          (t
           (user-error "Sequence length is 0")))))

(defun format--read-sequence (prefix)
  "Read a sequence. With any prefix arg, prompt for lisp. With universal
prefix, also prompt for a transformer."
  (if prefix
      (let* ((exp
              (read--expression
               "Seq: "
               (format "(number-sequence 1 %d)"
                       (if (numberp prefix) prefix 4))))
             (result
              (eval (let ((lexical-binding t)) (macroexpand-all exp))
                    t)))
        ;; maybe add a transform option?
        (if (consp prefix)
            (let* ((transform-exp
                   (read--expression
                    (format "%s ▷ " (query-replace-descr (format "%s" result)))
                    "(lambda (x) x)"))
                   (transform (eval (let ((lexical-binding t)) (macroexpand-all transform-exp))
                                    t)))
              (unless (functionp transform)
                (user-error "Transformer must be a function"))
              (mapcar transform result))
          result))
    (format--parse-number-sequence
     (read-from-minibuffer "Seq [from to inc]: "))))

(defconst format--format-str
  (let ((flags "[+ #-0]\\{0,1\\}")
        (width "[0-9]*")
        (precision "\\(?:\\.[0-9]+\\)?")
        (character "[sdoxXefgcS]"))
    (format "%s%s%s%s"
            flags width precision character))
  "Matches format string (stuff after `%').")

(defun format--parse-template (str)
  "Parse % forms in STR, return a list of (STR FORMS).

Each element of FORMS corresponds to a `format'-style % form in STR.
"
  (let ((start 0)
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
               (push sexp forms)
               (setq fexp (string-match format--format-str str end))
               (setq str (concat (substring str 0 start)
                                 (if (eq fexp end) "" "s")
                                 (substring str end)))))
            (t (push '_ forms))))
      (error (message "Malformed sexp: %s" (substring str start))))
    (cons str (nreverse forms))))

;;;###autoload
(defun format-expand (beg end seq)
  "Expand region as if it's a format string.

With prefix arg, SEQ is read as an expression that returns a list."
  (interactive
   (list (or (use-region-beginning) (line-beginning-position))
         (or (use-region-end) (1+ (line-end-position)))
         (format--read-sequence current-prefix-arg)))
  (let* ((str (filter-buffer-substring beg end t))
         (parsed (format--parse-template str)))
    (dolist (iter seq)
      (insert
       (apply 'format (car parsed)
              (mapcar (lambda (sexp)
                        (eval sexp `((_ . ,iter))))
                      (cdr parsed)))))))

(provide 'format-expand)
;;; format-expand.el ends here
