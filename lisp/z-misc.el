;;; z-misc -- Misc defuns -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(defvar z-align-alist
  '((?  "\\s-" 0)
    (?\" "\\s-\"" 0))
  "Specify regexp and spacing for z-align-char.")

(defun z-align-char (char beg end no-space)
  "Align CHAR in region specified by BEG and END.
With prefix arg (NO-SPACE), do not leave space before CHAR."
  (interactive "cAlign char: \nr\nP")
  (let* ((a (alist-get char z-align-alist))
         (regexp (concat  "\\(\\s-*\\)"
                          (if a (car a)
                            (regexp-quote (char-to-string char)))))
         (spacing (if a (cadr a)
                    (if no-space 0 1))))
    (align-regexp beg end regexp 1 spacing t)))

(defun z-toggle-selective-display (column)
  "Toggle selective display at COLUMN, defaulting to current column."
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun z-ediff-this-buffer ()
  "Call ediff on this buffer, with the version on disk or backup."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not associated with a file"))
  (if (buffer-modified-p)
      (ediff-current-file)
    (ediff-backup (buffer-file-name))))

(defun z-shrink-other-window-if-larger-than-buffer ()
    "Shrink other window if larger than buffer."
    (interactive)
    (shrink-window-if-larger-than-buffer
     (next-window (selected-window) nil nil)))

(defun z-prev-buffer-next-window ()
  "Switch other window to previous buffer."
  (interactive)
  (switch-to-prev-buffer (next-window)))
(defun z-next-buffer-next-window ()
  "Switch other window to next buffer."
  (interactive)
  (switch-to-next-buffer (next-window)))

(provide 'z-misc)
;;; z-misc.el ends here
