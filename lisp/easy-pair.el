;;; easy-pair -- Edit pairs wrapping region -*- lexical-binding: t -*-
;;; Commentary:
;;   Provides a few functions for easier manupulation of parenthesis,
;;   e.g. change parenthesis to brackets (`easy-pair-delete' then wrap
;;   new one using electric-pair-mode), or change content of list
;;   similar to change-inner (`easy-pair-kill-inside'). Also comes
;;   with a simple implementation of barf and slurp.

;;; Code:

(require 'elec-pair)

(defun easy-pair-match-p (s1 s2)
  "Return non-nil if S1 and S2 contain matching pairs.

For example, `[(' and `)]' matches while `[(' and `])' does not
match.  Rely on electric-pair logic here."
  (when (= (length s1) (length s2))
    (if (= 0 (length s1))
        t
      (and (eq (cadr (electric-pair-syntax-info (aref s1 0)))
              (aref (substring s2 -1) 0))
           (easy-pair-match-p
            (substring s1 1)
            (substring s2 0 -1))))))

;;;###autoload
(defun easy-pair-delete (arg beg end &optional killp)
  "Delete ARG number of pairs at BEG and END locations.

Keep region active if needed.  Optional KILLP kills instead of
deletes."
  (interactive "p\nr\nP")
  (let ((s1 (buffer-substring-no-properties beg (+ beg arg)))
        (s2 (buffer-substring-no-properties (- end arg) end))
        (active mark-active))
    (if (or (< (- end beg) (* arg 2))
            (not (easy-pair-match-p s1 s2)))
        (progn (goto-char beg)
               (delete-char (- end beg) killp))
      (save-excursion
        (goto-char (- end arg))
        (delete-char arg)
        (goto-char beg)
        (delete-char arg))
      (setq deactivate-mark (not active)))))

(defun easy-pair--kill-symbol-no-prefix (beg end)
  "Kill symbol but without the prefix"
  (let (p symbol-bounds length)
    (save-excursion
      (goto-char beg)
      (skip-syntax-forward "'" end)
      (setq p (point))
      (setq symbol-bounds (bounds-of-thing-at-point 'symbol))
      (when (and (< beg p)
                 symbol-bounds
                 (eq end (cdr symbol-bounds)))
        (setq length (- end p))))
    (when length
      (goto-char p)
      (delete-char length t)
      length)))

(defun easy-pair--kill-inside-pair (beg end)
  "Kill inside the pair between BEG and END.
Check if characters at the boundary are matching pairs, otherwise return nil."
  (let* ((s1 (buffer-substring-no-properties beg (1+ beg)))
         (s2 (buffer-substring-no-properties (1- end) end))
         (length (- (1- end) (1+ beg))))
    (when (and (>= length 0)
               (easy-pair-match-p s1 s2))
      (goto-char (1+ beg))
      (delete-char length t)
      length)))

(defun easy-pair--kill-inside-sexp-pair (beg end)
  "Kill inside the pair between BEG and END.
The boundary is assumed to be a sexp at each end."
  (let* ((sbeg (scan-sexps beg 1))
         (send (scan-sexps end -1))
         (length (- send sbeg)))
    (when (>= length 0)
      (goto-char sbeg)
      (delete-char length t)
      length)))

;;;###autoload
(defun easy-pair-kill-inside (beg end)
  "Kill contents inside the list between BEG and END.

The list boundary is kept."
  (interactive "r")
  (or (easy-pair--kill-symbol-no-prefix beg end)
      (easy-pair--kill-inside-pair beg end)
      (easy-pair--kill-inside-sexp-pair beg end)
      (kill-region beg end)))

;;;###autoload
(defun easy-pair-slurp (&optional arg)
  "Slurp ARG sexps into current list."
  (interactive "p")
  (if (< arg 0) (easy-pair-barf (- arg))
    (save-excursion
      (let (s beg)
        (up-list 1 't 't)
        (setq beg (point))
        (forward-sexp arg)
        (setq s (filter-buffer-substring beg (point) 'delete))
        (backward-char)
        (insert s)))))

;;;###autoload
(defun easy-pair-backward-slurp (&optional arg)
  "Slurp ARG sexps before current list into current list."
  (interactive "p")
  (if (< arg 0) (easy-pair-backward-barf (- arg))
    (save-excursion
      (let (end s)
        (backward-up-list 1 't 't)
        (setq end (point))
        (backward-sexp arg)
        (setq s (filter-buffer-substring (point) end 'delete))
        (forward-char)
        (insert s)))))

;;;###autoload
(defun easy-pair-barf (&optional arg)
  "Barf ARG sexps out of current list."
  (interactive "p")
  (if (< arg 0) (easy-pair-slurp (- arg))
  (save-excursion
    (let (s end)
      (up-list 1 't 't)
      (backward-char)
      (setq end (point))
      (backward-sexp arg)
      (while (looking-back "\\s-\\|\n" (1- (point))) (backward-char))
      (setq s (filter-buffer-substring (point) end 'delete))
      (forward-char)
      (insert s)))))

;;;###autoload
(defun easy-pair-backward-barf (&optional arg)
  "Barf ARG sexps out of current list."
  (interactive "p")
  (if (< arg 0) (easy-pair-backward-slurp (- arg))
  (save-excursion
    (let (beg s)
      (backward-up-list 1 't 't)
      (forward-char)
      (setq beg (point))
      (forward-sexp arg)
      (while (looking-at "\\s-\\|\n") (forward-char))
      (setq s (filter-buffer-substring beg (point) 'delete))
      (backward-char)
      (insert s)))))

(provide 'easy-pair)
;;; easy-pair.el ends here
