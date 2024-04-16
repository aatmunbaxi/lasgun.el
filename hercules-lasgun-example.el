;;; hercules-lasgun-example
;;;
;;; Commentary:
;;;
;;; Provides a demo setup for using lasgun.el with the hercules
;;; package. We define a lasgun-minor-mode that we can use to
;;; enter the hercules dispatch menu, where we can perform lasgun marks
;;; and actions. this is not meant to be "the" solution for getting
;;; a lasgun workflow. Users should investigate ways to incorporate
;;; lasgun naturally in their workflow. Possible solutions include
;;; hercules or hydras.
;;;
;;; This is an EXAMPLE configuration, and is not necessarily recommended
;;; for end-users.
;;;
;;; Requires general, hercules, and lasgun.
;;;
;;; hercules-lasgun-example.el -*- lexical-binding: t; -*-
(require 'hercules)
(require 'lasgun)
(require 'general)

;;; Code:

;; Defines some lasgun actions, `upcase-word', `downcase-word', and `kill-word'
(define-lasgun-action lasgun-action-upcase-word t upcase-word)
(define-lasgun-action lasgun-action-downcase-word t downcase-word)

;; Note it only makes sense to clear lasgun marks
;; upon killing, otherwise the buffer positions would get screwed up
(define-lasgun-action lasgun-action-kill-word nil kill-word)

;;; hercules-specific configuration with general
;;;
;;; Define keymap that hercules will display
(defvar-keymap lasgun-mode-map)
(general-define-key :keymaps
                    '(lasgun-mode-map)
                    ;; lasgun mark functions
                    "c"
                    (list :def #'lasgun-mark-char-timer :which-key "Char timer")
                    "w"
                    (list :def #'lasgun-mark-word-0 :which-key "Word")
                    "l"
                    (list :def #'lasgun-mark-line :which-key "Begin of line")
                    "s"
                    (list :def #'lasgun-mark-symbol-1 :which-key "Symbol")
                    "spc"
                    (list :def #'lasgun-mark-whitespace-end :which-key "Whitespace end")

                    ;; useful functions for interactivity
                    "x"
                    (list :def #'lasgun-clear-lasgun-mark-ring :which-key "Clear lasgun mark ring")
                    "u"
                    (list :def #'lasgun-pop-lasgun-mark :which-key "Undo lasgun mark")

                    ;; lasgun actions
                    "."
                    (list :def #'lasgun-embark-act-all :which-key "Embark act all")
                    "U"
                    (list :def #'lasgun-action-upcase-word :which-key "Upcase")
                    "l"
                    (list :def #'lasgun-action-downcase-word :which-key "Downcase")
                    "K"
                    (list :def #'lasgun-action-kill-word :which-key "Kill word")
                    "e"
                    (list :def #'lasgun-make-multiple-cursors :which-key "Make cursors")

                    ;; Quit
                    "C-g"
                    (list :def #'my/leave-lasgun-mode :which-key "Quit"))

;; Dummy minor mode for hercules to use
(define-minor-mode lasgun-minor-mode
  "Minor mode for lasgun.")

(add-hook 'lasgun-minor-mode-hook #'lasgun-clear-lasgun-mark-ring)

;; Functions to enter and exit lasgun-minor-mode
(defun my/enter-lasgun-mode ()
  "Enter lasgun minor mode."
  (interactive)
  (lasgun-minor-mode 1))

(defun my/leave-lasgun-mode ()
  (interactive)
  (lasgun--safe-clear-lasgun-mark-ring)
  (lasgun-minor-mode -1))

;; Define global keymap to enter 'lasgun-minor-mode
(general-define-key "C-c t g"
                    (list :def #'my/enter-lasgun-mode :which-key "Lasgun"))


;; Define our hercules dispatch
(hercules-def
 :show-funs '(my/enter-lasgun-mode)
 ;; creating cursors and embarking should kick us out of the hercules
 ;; dispatch
 :hide-funs '(lasgun-make-multiple-cursors ;; leave hercules so we can edit our cursors
              lasgun-embark-act-all
              my/leave-lasgun-mode)
 :keymap 'lasgun-mode-map)

(provide 'lasgun-hercules-example)
;; lasgun-hercules-example.el ends here
