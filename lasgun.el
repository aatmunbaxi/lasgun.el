;;; lasgun.el --- Avy-backed multi-marking -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Aatmun Baxi
;;
;; Author: Aatmun Baxi
;; Maintainer: Aatmun Baxi
;; Created: April 16, 2024
;; Modified: April 16, 2024
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/aatmunbaxi/lasgun.el
;; Package-Requires: ((emacs "24.3"))
;;
;;; Commentary:
;;;
;;; At its core, lasgun provides avy-backed marking of multiple positions
;;; in the current buffer. The hope is that users can plug this functionality
;;; into wherever they'd like, without shipping an opinonated final product
;;; that shoehorns a particular usage (teach a man to fish or whatever).
(require 'avy)

;;; Code:
(defgroup lasgun nil
  "Avy-backed marking of positions."
  :group 'convenience)

(defcustom lasgun-pop-before-make-multiple-cursors t
  "Pop `lasgun-mark-ring' and jump before making cursors.

When nil, makes cursor at point in addition to positions in
`lasgun-mark-ring'. When t, makes cursors only at positions
in `lasgun-mark-ring'. Requires `multiple-cursors'."
  :type 'boolean)

(defcustom lasgun-persist-negation-prefix-arg 0
  "Numerical prefix arg making `lasgun-action' negate behavior
of `lasgun-persist-lasgun-mark-ring'.

e.g. with value 0, preceding a lasgun action defined by `define-lasgun-action'
with numeric prefix arg 0  will negate persistence behavior."
  :type 'int)

(defcustom lasgun-persist-lasgun-mark-ring nil
  "Persist `lasgun-mark-ring' contents upon performing lasgun actions.
If t, the user is responsible for calling `lasgun-clear-lasgun-mark-ring' to
destroy overlays and clear the `lasgun-mark-ring'.

When nil, lasgun actions defined via `define-lasgun-action' will clear
the `lasgun-mark-ring' and overlays associated with the buffer positions, unless overriden
by universal prefix. See `lasgun-persist-negation-prefix-arg'."
  :type 'boolean)

(defcustom lasgun-mark-ring-max 16
  "Maximum capacity for `lasgun-mark-ring'."
  :type 'integer)

(defcustom lasgun-also-push-mark-ring nil
  "When t, push `mark-ring' as well as `lasgun-mark-ring'.

Note: when t, marks made with lasgun will persist in tsomehe `mark-ring' regardless
of the value of `lasgun-persist-lasgun-mark-ring.'"
  :type 'boolean)

(defcustom lasgun-use-lasgun-mark-overlay t
  "Use visual overlays for lasgun marks.

When t, use `lasgun-mark-face' as overlay at each position."
  :type 'boolean)

(defvar lasgun-mark-ring (make-ring lasgun-mark-ring-max)
  "Mark ring for lasgun marks. Capacity customizable with `lasgun-mark-ring-max'.")

(defvar lasgun--overlay-ring (make-ring lasgun-mark-ring-max)
  "List of overlays for lasgun marks.")

(defface lasgun-mark-face
  '((t (:inherit 'cursor)))
  "Face used for lasgun marks."
  :group 'lasgun)

(defun lasgun-mark-overlay-at-mark (pos)
  "Apply one-character-wide `lasgun-mark-face' overlay at POS, if desired.

See `lasgun-use-lasgun-mark-overlay'."
  (when lasgun-use-lasgun-mark-overlay
    (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
      (when (eq (ring-length lasgun--overlay-ring) lasgun-mark-ring-max)
        (ignore-errors
          (delete-overlay (ring-remove lasgun--overlay-ring))))
      (overlay-put overlay 'face 'lasgun-mark-face)
      (ring-insert lasgun--overlay-ring overlay))))

(defun lasgun-remove-overlays ()
  "Remove lasgun point overlays."
  (dolist (overlay (ring-elements lasgun--overlay-ring))
    (ignore-errors
      (delete-overlay overlay)))
  (setq lasgun--overlay-ring (make-ring lasgun-mark-ring-max)))

(defun lasgun--pop-overlay-ring ()
  "Pop `lasgun--overlay-ring', removing the overlay associated with it."
  (ignore-errors
    (delete-overlay (ring-remove lasgun--overlay-ring 0))))

(defun lasgun-pop-lasgun-mark ()
  "Pop most recently added position off `lasgun-mark-ring', returning it."
  (interactive)
  (lasgun--pop-overlay-ring)
  (ring-remove lasgun-mark-ring 0))

(defun lasgun-push-mark-no-activate (&optional pos)
  "Push POS (if present) to `lasgun-mark-ring' without activating mark.
Otherwise push result of `point' to `lasgun-mark-ring' without
activating mark.

Additionally push POS or value of `point' to `mark-ring', depending on
value of `lasgun-also-push-mark-ring'."

  (ring-insert lasgun-mark-ring (if pos pos (point)))
  (lasgun-mark-overlay-at-mark (car (ring-elements lasgun-mark-ring)))
  (when lasgun-also-push-mark-ring
    (push-mark (if pos pos (point)) t nil))
  (message "Pushed selection to lasgun mark ring"))

(defun lasgun-clear-lasgun-mark-ring ()
  "Clear `lasgun-mark-ring'."
  (interactive)
  (lasgun-remove-overlays)
  (setq-local lasgun-mark-ring (make-ring lasgun-mark-ring-max)))


(defun lasgun--safe-clear-lasgun-mark-ring (&optional persist)
  "Safely clears `lasgun-mark-ring'.
Respects wishes of `lasgun-persist-lasgun-mark-ring', unless
overridden with PERSIST."
  (cond ((or persist lasgun-persist-lasgun-mark-ring) nil)
        (t (lasgun-clear-lasgun-mark-ring))))

(defmacro define--lasgun-avy-mark (NAME CMD &rest args)
  "Define lasgun mark function NAME based on avy command CMD.
CMD is an avy command someaccepting ARG that negates
`avy-all-windows' behavior with interactive arguments. e.g. `avy-goto-char-2'.

ARGS passed to CMD in an interactive call to CMD."
  (let ((docstring (concat "Push `lasgun-mark-ring' via `" (symbol-name CMD) "' selection.")))
    `(defun ,NAME ()
       ,docstring
       (interactive)
       (save-excursion
         (let ((current-prefix-arg (if avy-all-windows '(4) nil)))
           (call-interactively ',CMD t (vector ,@args)))
         (lasgun-push-mark-no-activate)))))

(define--lasgun-avy-mark lasgun-mark-end-of-line avy-goto-end-of-line)
(define--lasgun-avy-mark lasgun-mark-word-0 avy-goto-word-0)
(define--lasgun-avy-mark lasgun-mark-char-timer avy-goto-char-timer)
(define--lasgun-avy-mark lasgun-mark-word-0-below avy-goto-word-0-below)
(define--lasgun-avy-mark lasgun-mark-word-0-above avy-goto-word-0-above)
(define--lasgun-avy-mark lasgun-mark-subword-0 avy-goto-subword-0)
(define--lasgun-avy-mark lasgun-mark-subword-1 avy-goto-subword-1)
(define--lasgun-avy-mark lasgun-mark-whitespace-end avy-goto-whitespace-end)
(define--lasgun-avy-mark lasgun-mark-whitespace-end-above avy-goto-whitespace-end-above)
(define--lasgun-avy-mark lasgun-mark-whitespace-end-below avy-goto-whitespace-end-below)
(define--lasgun-avy-mark lasgun-mark-char-2 avy-goto-char-2)
(define--lasgun-avy-mark lasgun-mark-char-2-above avy-goto-char-2-above)
(define--lasgun-avy-mark lasgun-mark-char-2-below avy-goto-char-2-below)
(define--lasgun-avy-mark lasgun-mark-symbol-1 avy-goto-symbol-1)
(define--lasgun-avy-mark lasgun-mark-symbol-1-above avy-goto-symbol-1-above)
(define--lasgun-avy-mark lasgun-mark-symbol-1-below avy-goto-symbol-1-below)

;; avy-goto-line has a different ARG convention
(defun lasgun-mark-line ()
  "Push selection to `lasgun-mark-ring' with `avy-line' selector."
  (interactive)
  ;; setq nonsense is a dirty hack
  ;; `avy-goto-line' only respect its ARG if `avy-all-windows-alt' is `nil'
  ;; Not sure if that is desired behavior, but we need to work around it.
  (let ((original-aawa (symbol-value avy-all-windows-alt)))
    (setq-local avy-all-windows-alt nil)
    (save-excursion
      (funcall 'avy-goto-line (if avy-all-windows 4 'nil))
      (lasgun-push-mark-no-activate))
    (setq avy-all-windows-alt (symbol-value original-aawa))))

(defun lasgun-make-multiple-cursors (ARG)
  "Enter `multiple-cursors-mode' at all positions in `lasgun-mark-ring'.

Invocation always clears `lasgun-mark-ring'.
When called with non-nil ARG, behavior of `lasgun-pop-before-make-multiple-cursors' is negated."
  (interactive "P")
  (if  (require 'multiple-cursors nil 'no-error)
      (let* ((lasgun-ring-copy (ring-copy lasgun-mark-ring))
             (pop-arg (xor ARG lasgun-pop-before-make-multiple-cursors))
             (start-pos (if pop-arg
                            (ring-remove lasgun-ring-copy 0)
                          (point))))
        (goto-char start-pos)
        (while (not (ring-empty-p lasgun-ring-copy))
          (save-excursion
            (goto-char (ring-remove lasgun-ring-copy 0))
            (mc/create-fake-cursor-at-point)))
        (mc/maybe-multiple-cursors-mode)
        (lasgun--safe-clear-lasgun-mark-ring))
    (user-error "Multiple-cursors not found. Install to use this function")))

(defun lasgun-embark-act-all ()
  "Call `emark-act-all' on candidates.

Invocation always clears `lasgun-mark-ring'."
  (interactive)
  (if (require 'embark nil 'no-error)
      (progn
        (save-excursion
          (while (not (ring-empty-p lasgun-mark-ring))
            (goto-char (lasgun-pop-lasgun-mark))
            (embark-select))
          (embark-act-all))
        (lasgun--safe-clear-lasgun-mark-ring)
        (setq-local embark--selection nil))
    (progn (user-error "Embark not found. Install to use this function")
           (lasgun-clear-lasgun-mark-ring))))


(defmacro define-lasgun-action (NAME PERSIST FUN  &rest FUN-ARGS)
  "Define lasgun action with name NAME from interactive function FUN.

Define lasgun action with name NAME, performing an interactive FUN at each
position in `lasgun-mark-ring'. FUN-ARGS are passed to FUN.

PERSIST specifies if `lasgun-mark-ring' persists after
FUN has been called at each position. If t, `lasgun-mark-ring' persists.
If nil, fall back to `lasgun-persist-lasgun-mark-ring'.
This behavior can be negated with prefix args, see
`lasgun-persist-negation-prefix-arg'

For example,

   (define-lasgun-action lasgun-action-upcase-word t upcase-word)

will run `upcase-word' on each lasgun mark, making lasgun marks
persist after operation."
  (let ((docstring
         (concat "Run `" (symbol-name FUN)
                 "\' on each position in \`lasgun-mark-ring' with"
                 (if PERSIST " \n" "out \n")
                 "persistence of `lasgun-mark-ring'.

When called with numeric universal arg
equal to `lasgun-persist-negation-prefix-arg', persistence is negated
and FUN is called with numerical prefix arg 1.")))
    `(defun  ,NAME (ARG)
       ,docstring
       (interactive "p")
       (cond ((ring-empty-p lasgun-mark-ring)
              (message "lasgun-mark-ring empty."))
             (t (let ((negator (xor (= ARG lasgun-persist-negation-prefix-arg)
                                    ,PERSIST)))
                  (unwind-protect
                      (save-excursion
                        (dolist (pos (ring-elements lasgun-mark-ring))
                          (goto-char pos)
                          (if (= ARG lasgun-persist-negation-prefix-arg)
                              (let ((current-prefix-arg nil))
                                (call-interactively ',FUN 't (vector ,@FUN-ARGS)))
                            (let ((current-prefix-arg ARG))
                              (call-interactively ',FUN 't (vector  ,@FUN-ARGS))))))
                    (user-error "%s" "Error running action")
                    (lasgun-clear-lasgun-mark-ring))
                  (lasgun--safe-clear-lasgun-mark-ring negator)))))))

(provide 'lasgun)
;;; lasgun.el ends here
