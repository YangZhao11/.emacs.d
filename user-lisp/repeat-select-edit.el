;;; repeat-select-edit.el --- Repeat the last select-edit combo -*- lexical-binding: t; -*-

;; Keywords: convenience, editing
;; Version: 1.2

;;; Commentary:
;; A Vim-dot-like repeat mechanism that repeats the last select-edit
;; combo or standalone edit group using raw keystrokes replayed
;; through `execute-kbd-macro'.
;;
;; This package uses a Cartesian-product state machine with two state
;; variables:
;; 1. `rse-state': The high-level macro phase ('idle, 'selecting,
;; 'editing, or 'editing-group).
;; 2. `rse-prefix-state': The prefix argument builder status ('none or
;; 'prefixing).
;;
;; This allows seamless capture of prefix arguments (e.g., C-u,
;; numeric arguments) which are entered before the actual select or
;; edit commands occur.

;;; Code:

(defgroup rse nil
  "Customization options for `repeat-select-edit'."
  :group 'editing)

;;; State Variables

(defvar rse-state 'idle
  "Current state of the macro phase: 'idle, 'selecting, 'editing, or 'editing-group.")

(defvar rse-prefix-state 'none
  "Current status of prefix argument entry: 'none or 'prefixing.")

(defvar rse-recording-keys nil
  "Vector of keys currently being recorded in the active chain.")

(defvar rse-saved-keys nil
  "The last successfully finalized select-edit combo ready for repetition.")

;;; Command & Buffer Tracking

(defvar rse--tick-before nil
  "Buffer modification tick before command execution.")

(defvar rse--mark-active-before nil
  "Whether mark was active before command execution.")

;;; Event Classification & Grouping Logic

(defun rse-classify-event (command)
  "Classify COMMAND into an event type for the state machine.
This function isolates the grouping and detection logic so it can be
iterated on and expanded easily."
  (cond
   ((eq command 'repeat-select-edit) 'repeat)
   ((memq command '(keyboard-quit minibuffer-keyboard-quit)) 'cancel)
   ((memq command '(universal-argument digit-argument negative-argument)) 'prefix)
   ((get command 'group-edit-start) 'group-start)
   ((get command 'group-edit-end) 'group-end)

   ;; Edit check: Did the buffer character content change?
   ((and rse--tick-before
         (/= rse--tick-before (buffer-chars-modified-tick)))
    'edit)

   ;; Activate mark: Mark transitioned from inactive to active
   ((and (not rse--mark-active-before) mark-active)
    'activate-mark)

   ;; Selection motion: Moving cursor while mark remains active
   (mark-active 'select)

   ;; Non-editing, non-selection commands
   (t 'other)))

(defun rse--append-keys (keys is-prefixing)
  "Append KEYS to `rse-recording-keys`.
If `rse-state' is 'idle and IS-PREFIXING is nil, this starts a new sequence."
  (if (and (eq rse-state 'idle) (not is-prefixing))
      (setq rse-recording-keys keys)
    (setq rse-recording-keys (vconcat rse-recording-keys keys))))

;;; State Transitions & Hooks

(defun rse-pre-command-hook ()
  "Record snapshot of buffer tick and mark state before command runs."
  (when (and rse-mode (not executing-kbd-macro) (not defining-kbd-macro))
    (setq rse--tick-before (buffer-chars-modified-tick)
          rse--mark-active-before mark-active)))

(defun rse-post-command-hook ()
  "Run Cartesian state machine transitions after command executes."
  (when (and rse-mode (not executing-kbd-macro) (not defining-kbd-macro))
    (let ((event (rse-classify-event this-command))
          (keys (this-single-command-keys)))

      (cond
       ;; 1. Handle Cancel (C-g)
       ((eq event 'cancel)
        (setq rse-recording-keys nil
              rse-state 'idle
              rse-prefix-state 'none))

       ;; 2. Handle Prefix Commands (C-u, digit arguments)
       ((eq event 'prefix)
        (if (and (eq rse-state 'idle) (eq rse-prefix-state 'none))
            (setq rse-recording-keys keys)
          (setq rse-recording-keys (vconcat rse-recording-keys keys)))
        (setq rse-prefix-state 'prefixing))

       ;; 3. Handle Action Commands (which consume the prefix)
       (t
        (let ((is-prefixing (eq rse-prefix-state 'prefixing)))
          (setq rse-prefix-state 'none)

          (cond
           ;; ==================== STATE: IDLE ====================
           ((eq rse-state 'idle)
            (cond
             ((eq event 'activate-mark)
              (rse--append-keys keys is-prefixing)
              (setq rse-state 'selecting))
             ((eq event 'edit)
              (rse--append-keys keys is-prefixing)
              (setq rse-state 'editing))
             ((eq event 'group-start)
              (rse--append-keys keys is-prefixing)
              (setq rse-state 'editing-group))
             (t
              (setq rse-recording-keys nil))))

           ;; ==================== STATE: SELECTING ====================
           ((eq rse-state 'selecting)
            (cond
             ((memq event '(select edit group-start))
              (rse--append-keys keys is-prefixing)
              (cond
               ((eq event 'edit) (setq rse-state 'editing))
               ((eq event 'group-start) (setq rse-state 'editing-group))))
             ((eq event 'activate-mark)
              (setq rse-recording-keys keys))
             ((memq event '(other group-end))
              (setq rse-recording-keys nil
                    rse-state 'idle))))

           ;; ==================== STATE: EDITING ====================
           ((eq rse-state 'editing)
            (cond
             ((eq event 'edit)
              (rse--append-keys keys is-prefixing))
             ((eq event 'group-start)
              (rse--append-keys keys is-prefixing)
              (setq rse-state 'editing-group))
             ((eq event 'activate-mark)
              (setq rse-saved-keys rse-recording-keys
                    rse-recording-keys keys
                    rse-state 'selecting))
             ((memq event '(select other group-end))
              (setq rse-saved-keys rse-recording-keys
                    rse-recording-keys nil
                    rse-state 'idle))))

           ;; ==================== STATE: EDITING-GROUP ====================
           ((eq rse-state 'editing-group)
            (rse--append-keys keys is-prefixing)
            (when (eq event 'group-end)
              (setq rse-saved-keys rse-recording-keys
                    rse-recording-keys nil
                    rse-state 'idle))))))))))

;;; Minor Mode

;;;###autoload
(define-minor-mode rse-mode
  "Global minor mode to track and repeat select-edit combos."
  :global t
  :group 'rse
  :lighter " RSE"
  (if rse-mode
      (progn
        (setq rse-state 'idle
              rse-prefix-state 'none
              rse-recording-keys nil)
        (add-hook 'pre-command-hook #'rse-pre-command-hook)
        (add-hook 'post-command-hook #'rse-post-command-hook))
    (remove-hook 'pre-command-hook #'rse-pre-command-hook)
    (remove-hook 'post-command-hook #'rse-post-command-hook)))

;;; Repetition Command

;;;###autoload
(defun repeat-select-edit (&optional arg)
  "Repeat the last recorded select-edit combination.
With prefix ARG, repeat it ARG times."
  (interactive "p")
  (unless (and rse-saved-keys (> (length rse-saved-keys) 0))
    (user-error "No select-edit combo recorded yet"))
  (let ((count (or arg 1)))
    (dotimes (_ count)
      (execute-kbd-macro rse-saved-keys))))

(provide 'repeat-select-edit)

;;; repeat-select-edit.el ends here
