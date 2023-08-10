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
      (:when (modulep! :completion ivy)
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
(map! :leader
      (:when (modulep! :completion ivy)
       :desc "Find file Other frame" :g "p o" #'projectile-find-file-other-frame))

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

;; Remove binding for +fold to use hs mode instead
(define-key! evil-normal-state-map "z r" #'hs-show-all)
(define-key! evil-normal-state-map "z m" #'hs-hide-level)

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

;; Unmap so it can be rebound
(map! :n "C-=" nil)
(map! :n "C--" nil)
(map! :n "C-=" #'increase-font-size)
(map! :n "C--" #'decrease-font-size)

;;(define-key evil-normal-state-map "C-=" 'increase-font-size t)
;;(define-key evil-normal-state-map "C--" 'decrease-font-size t)

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
      (progn
        (define-key cider--debug-mode-map "h" nil)
        (define-key cider--debug-mode-map "H" 'cider-debug-move-here)
        (define-key cider--debug-mode-map "i" nil)
        (define-key cider--debug-mode-map "I" 'evil-collection-cider-debug-in)
        (define-key cider--debug-mode-map "j" nil)
        (define-key cider--debug-mode-map "J" 'evil-collection-cider-debug-inject)
        (define-key cider--debug-mode-map "l" nil)
        (define-key cider--debug-mode-map "L" 'evil-collection-cider-debug-inspect)
        (evil-normal-state))  ;; Otherwise, turn on normal-state
    (evil-insert-state)    ;; If so, turn on evil-insert-state
    ))

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

(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-window" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-frame" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-frame (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(define-key evil-normal-state-map "gof" '+lookup/definition-other-frame)
(define-key evil-normal-state-map "gow" '+lookup/definition-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :after python
      :map python-mode-map
      :n ", e b" #'python-shell-send-buffer
      :n ", e f" #'python-shell-send-defun
      :n ", e l" #'python-shell-send-statement
      :n ", '" #'+python/open-repl
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :map emacs-lisp-mode-map
      :n ", e b" #'eval-buffer
      :n ", e f" #'eval-defun
      :n ", e l" #'eval-last-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(map! :after ruby-mode
      :map ruby-mode-map
      :n ", e b" #'ruby-send-buffer
      :n ", e f" #'ruby-send-block
      :n ", e l" #'ruby-send-last-sexp
      :n ", s s" #'inf-ruby-buffer
      :n ", t n" #'rspec-verify
      :n ", t p" #'rspec-verify-all
      :n "SPC m t t" #'rspec-verify-single
      :n ", '"   #'inf-ruby)

(map! :after lsp-mode
      :map lsp-mode-map

      ;; Go to Usages (g u)
      :n "g u" #'lsp-find-references)
