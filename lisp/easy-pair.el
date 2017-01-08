;;; easy-pair -- Edit pairs wrapping region -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'elec-pair)

(defun easy-pair-match-p (s1 s2)
  "Return true if S1 and S2 contain matching pairs.

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

(defun easy-pair--kill-inside-pair (beg end)
  (interactive "r")
  (let* ((s1 (buffer-substring-no-properties beg (1+ beg)))
         (s2 (buffer-substring-no-properties (1- end) end))
         (length (- (1- end) (1+ beg))))
    (if (and (>= length 0)
             (easy-pair-match-p s1 s2))
        (progn (goto-char (1+ beg))
               (delete-char length t)
               length))))

(defun easy-pair--kill-inside-sexp-pair (beg end)
  (interactive "r")
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
  (or (easy-pair--kill-inside-pair beg end)
      (easy-pair--kill-inside-sexp-pair beg end)))

(provide 'easy-pair)
;;; easy-pair.el ends here
