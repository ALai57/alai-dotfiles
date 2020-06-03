;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove keymap so it can be rebound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      ":" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leader key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      (:when (featurep! :completion ivy)
       :desc "M-x" :nv "SPC" #'counsel-M-x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      :desc "magit status" :n "g s" #'magit
      :desc "magit todo list" :n "g T" #'magit-todos-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      :desc "Treemacs" :n "p t" #'treemacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key! +doom-dashboard-mode-map "C-n" nil)
(define-key! evil-normal-state-map "C-n" nil)
(map! :g "C-n" #'display-buffer-other-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      :desc "Open +bindings file" :g "h B"
      (lambda () (interactive) (find-file "~/.doom.d/+bindings.el"))

      :desc "Open .zshrc file" :g "h z"
      (lambda () (interactive) (find-file "~/.zshrc")))

(map! :leader
      :desc "Raise (promote) popup to buffer" :g "w r" #'+popup/raise
      :desc "Lower (demote) buffer to popup" :g "w R" #'+popup/buffer
      :desc "Toggle popup" :g "`" #'+popup/toggle)

(map!
 ;;:n "[S-return]" #'newline-and-indent
 :v "v" #'er/expand-region
 :v "u" #'er/contract-region
 :v "s" #'evil-surround-region)

(with-eval-after-load 'evil-surround
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CIDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cider-eval-around-point ()
  (interactive)
  ;; like save-excursion, but we need to set the marker type to 't
  (let ((m (point-marker)))
    (set-marker-insertion-type m 't)
    (lisp-state-prev-opening-paren)
    (cider-eval-sexp-at-point)
    (goto-char m)))

(map! :after clojure-mode
      :map clojure-mode-map
      :i "<backspace>" #'paredit-backward-delete
      :n ", d f" #'cider-debug-defun-at-point
      :n ", e a" #'cider-eval-around-point
      :n ", e b" #'cider-eval-buffer
      :n ", e c" #'cider-pprint-eval-last-sexp-to-comment
      :n ", e e" #'cider-eval-sexp-up-to-point
      :n ", e f" #'cider-eval-defun-at-point
      :n ", e n" #'cider-eval-sexp-at-point
      :n ", e l" #'cider-eval-last-sexp
      :n ", e p" #'cider-pprint-eval-last-sexp
      :n ", r t l" #' cljr-thread-last-all
      :n ", r t f" #' cljr-thread-first-all
      :n ", r e f" #' cljr-extract-function
      :n ", r e c" #' cljr-extract-constant
      :n ", r e d" #' cljr-extract-def
      :n ", r m l" #' cljr-move-to-let
      :n ", s s" #'cider-switch-to-repl-buffer
      :n ", s q" #'cider-quit
      :n ", t n" #'cider-test-run-ns-tests
      :n ", t p" #'cider-test-run-project-tests
      :n ", '" #'cider-jack-in-clj)

(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode    ;; Checks if you're entering the debugger
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state

(add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)
(set-popup-rule! "^\\*cider-repl.*?" :size 0.4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LISP state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :leader
      :desc "lisp-state" :n "k" #'lisp-state-toggle-lisp-state)

(define-key evil-lisp-state-map "c" 'sp-convolute-sexp)
(define-key evil-lisp-state-map "dx" 'sp-kill-sexp)
(define-key evil-lisp-state-map "D" 'evil-delete-line)
(define-key evil-lisp-state-map "e" 'sp-splice-sexp-killing-forward)
(define-key evil-lisp-state-map "E" 'sp-splice-sexp-killing-backward)
(define-key evil-lisp-state-map "r" 'sp-raise-sexp)
(define-key evil-lisp-state-map "s" 'sp-forward-slurp-sexp)
(define-key evil-lisp-state-map "S" 'sp-backward-slurp-sexp)
(define-key evil-lisp-state-map "t" 'sp-transpose-sexp)
(define-key evil-lisp-state-map "C-r" 'redo)
(define-key evil-lisp-state-map "u" 'undo)
(define-key evil-lisp-state-map "w" 'lisp-state-wrap)
(define-key evil-lisp-state-map "W" 'sp-unwrap-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
