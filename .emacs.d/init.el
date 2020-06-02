(defun filter (pred lst)
  "Use PRED to filter a list LST of elements."
  (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) lst)))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("MELPA" . "http://melpa.org/packages/") t)

(package-initialize)

(defvar my-packages)
(setq my-packages
      '(aggressive-indent
	beacon
	cider
	clojure-mode
	clojure-snippets
        company
	expand-region
	clj-refactor
	git-timemachine
	helm
	helm-projectile
	hideshow
        htmlize
	lorem-ipsum
	magit
	markdown-mode
	multiple-cursors
	org
	paredit
	projectile
	rainbow-delimiters
	yasnippet
        hcl-mode
	json-mode
	which-key
	zenburn-theme))


;; Install missing packages:
(let ((uninstalled-packages (filter (lambda (x) (not (package-installed-p x)))
				    my-packages)))
  (when (and (not (equal uninstalled-packages '()))
             (y-or-n-p (format "Install packages %s?"  uninstalled-packages)))
    (package-refresh-contents)
    (mapc 'package-install uninstalled-packages)))


;; Startup.........................................................
;; Some configuration to make things quieter on start up:

(setq inhibit-splash-screen t
      initial-scratch-message nil)


;; Turn on recentf-mode for reopening recently used files:

(recentf-mode 1)

;; Stuff for running shells within Emacs...........................
;;
;; Path Magic
;; Smooth the waters for starting processes from the shell. “Set up
;; Emacs’ `exec-path’ and PATH environment variable to match the
;; user’s shell. This is particularly useful under Mac OSX, where GUI
;; apps are not started from a shell[fn:: See
;; http://stackoverflow.com/questions/8606954/\
;; path-and-exec-path-set-but-emacs-does-not-find-executable]”.
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))


;; Kill shell buffers quickly.....................................

;; “With this snippet, [a second] press of C-d will kill the
;; buffer. It’s pretty nice, since you then just tap C-d twice to get
;; rid of the shell and go on about your merry way[fn:: From
;; http://whattheemacsd.com.]”
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))


(global-unset-key "\C-o")


;; Moar Shells.........................................................
;; Create shell in new buffer when needed, rather than just loading up
;; the existing shell buffer.
(defun create-shell-in-new-buffer ()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

;; (global-unset-key "\C-o")

;; (global-set-key "\C-oS" 'create-shell-in-new-buffer)

;; Term stuff
;; (defun create-term-shell ()
;; (interactive)
;; (term "/bin/bash"))

;; Per http://stackoverflow.com/questions/18278310/emacs-ansi-term-not-tab-completing : fix autocomplete
;; (add-hook 'term-mode-hook (lambda()
;; 			    (setq yas-dont-activate t)))


;; (global-set-key "\C-oT" 'create-term-shell)
;; (global-set-key "\C-a" 'split-window-horizontally)

;; Highlighting of long lines.....................................
(defun highlight-long-lines ()
  "Turn on highlighting of long lines."
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-pink))


(defun unhighlight-long-lines ()
  "Turn off highlighting of long lines."
  (interactive)
  (unhighlight-regexp "^.*\\(?:.\\{81\\}\\).*$"))


;; Clojure mode hooks.............................................
(defun set-clojure-indents ()
  ;; Handling Clojure indentation for certain macros
  (put-clojure-indent 'DELETE* 2)
  (put-clojure-indent 'GET* 2)
  (put-clojure-indent 'POST* 2)
  (put-clojure-indent 'PUT* 2)
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'ANY 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'after 1)
  (put-clojure-indent 'after-all 1)
  (put-clojure-indent 'around 1)
  (put-clojure-indent 'without-logging 0)
  (put-clojure-indent 'before 0)
  (put-clojure-indent 'before-all 0)
  (put-clojure-indent 'context 2)
  (put-clojure-indent 'context* 2)
  (put-clojure-indent 'check-cw-metric 2)
  (put-clojure-indent 'describe 1)
  (put-clojure-indent 'describe-examples 2)
  (put-clojure-indent 'describe-with-dawg 1)
  (put-clojure-indent 'describe-with-db 1)
  (put-clojure-indent 'describe-with-es 1)
  (put-clojure-indent 'describe-with-mock-etl-state 1)
  (put-clojure-indent 'describe-with-server 1)
  (put-clojure-indent 'do-rate-limited 1)
  (put-clojure-indent 'do-until-input 1)
  (put-clojure-indent 'do-with-save-config 1)
  (put-clojure-indent 'html/at 1)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)
  (put-clojure-indent 'it 1)
  (put-clojure-indent 'is 1)
  (put-clojure-indent 'are 2)
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'section 1)
  (put-clojure-indent 'should 0)
  (put-clojure-indent 'test-location 1)
  (put-clojure-indent 'solves 0)
  (put-clojure-indent 'metrics/time 1)
  (put-clojure-indent 'problem 1)
  (put-clojure-indent 'mock-dl-good-and-fast 0)
  (put-clojure-indent 'mock-dl-bad 0)
  (put-clojure-indent 'patterns-match 0)
  (put-clojure-indent 'process-safely 2)
  (put-clojure-indent 'mock-dl-short 0)
  (put-clojure-indent 'middleware 1)
  (put-clojure-indent 'testing-salesforce 3)
  (put-clojure-indent 'try 0)
  (put-clojure-indent 'try+ 0)
  (put-clojure-indent 'watcher 1)
  (put-clojure-indent 'wcar 1)
  (put-clojure-indent 'wrap-response 3)
  (put-clojure-indent 'with 1)
  (put-clojure-indent 'subsection 1)
  (put-clojure-indent 'perf/p 1)
  (put-clojure-indent 'log-timing 1)
  (put-clojure-indent 'subsubsection 1))


(defun convert-selection-to-link ()
  "For unmark: Convert selected text to Hiccup link"
  (interactive)
  (let* ((url (read-from-minibuffer "Enter link URL: "))
	 (bounds (if (use-region-p)
		     (cons (region-beginning) (region-end))
		   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
	 (to-replace (insert (concat "\" [:a {:href \"" url "\"} \"" text "\"] \""))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert to-replace))))


(defun convert-selection-to-code ()
  "For unmark: Convert selected text to Hiccup link"
  (interactive)
  (let* ((bounds (if (use-region-p)
		     (cons (region-beginning) (region-end))
		   (bounds-of-thing-at-point 'symbol)))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
	 (to-replace (insert (concat "\" [:code \"" text "\"] \""))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert to-replace))))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (paredit-mode 1)
	     (aggressive-indent-mode 1)
             (highlight-long-lines)
	     (clj-refactor-mode 1)
	     (yas-minor-mode 1) ;; for adding require/use/import
	     (cljr-add-keybindings-with-prefix "C-c C-t")
             (define-key clojure-mode-map (kbd "C-o x")
	       'cider-eval-defun-at-point)
             (define-key clojure-mode-map (kbd "C-o j") 'cider-jack-in)
             (define-key clojure-mode-map (kbd "C-o J")
               (lambda () (interactive) (cider-quit) (cider-jack-in)))
	     ;;(define-key clojure-mode-map (kbd "C-o K") 'convert-selection-to-link)
	     (define-key clojure-mode-map (kbd "C-o C") 'convert-selection-to-code)
             (define-key clojure-mode-map (kbd "C-<up>") 'paredit-backward)
             (define-key clojure-mode-map (kbd "C-<down>") 'paredit-forward)
             (define-key clojure-mode-map (kbd "C-o SPC")
               (lambda ()
                 (interactive)
                 (cider-interactive-eval "(let [result (clojure.test/run-tests)]
      (if
          (->> result
               ((juxt :fail :error))
               (apply +)
               zero?)
        (clojure.java.shell/sh \"say\" \"ok\")
        (clojure.java.shell/sh \"say\" \"fail\"))
  result)")))
             (define-key clojure-mode-map (kbd "C-o y")
               (lambda ()
	         (interactive)
	         (insert "\n;;=>\n'")
	         (cider-eval-last-sexp 't)))
	     (define-key clojure-mode-map (kbd "C-o Y")
	       (lambda ()
	         (interactive)
	         (cider-pprint-eval-last-sexp)))
	     (define-key clojure-mode-map (kbd "s-i") 'cider-eval-last-sexp)
             (define-key clojure-mode-map (kbd "s-I")
	       '(lambda ()
		  (interactive)
		  (paredit-forward)
		  (cider-eval-last-sexp)))
             (define-key clojure-mode-map (kbd "C-o C-i")
               (lambda ()
                 (interactive)
                 (cider-auto-test-mode 1)))
	     (set-clojure-indents)))

(add-to-list 'auto-mode-alist '("\\.garden" . clojure-mode))


;; Find Leiningen.............................................
(add-to-list 'exec-path "/usr/bin")
;;(add-to-list 'exec-path "/Users/andrewlai/bin")

;; Cider setup................................................
;;
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-interactive-eval-result-prefix ";; => ")
(setq cider-repl-history-file (concat user-emacs-directory "../.cider-history"))
;; Fix https://github.com/clojure-emacs/cider/issues/1258:
(defvar cider-eval-progress-bar-show nil)

;; JSON
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Lots of keybindings
;;
;; Many of these are extremely old, having followed me from machine to
;; machine over the years. Some could probably be deleted.
;; (global-set-key [S-deletechar]  'kill-ring-save)
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
;; (global-set-key [delete] 'delete-char)
;; (global-set-key [kp-delete] 'delete-char)

;; (define-key function-key-map "\e[1~" [find])
;; (define-key function-key-map "\e[2~" [insertchar])
;; (define-key function-key-map "\e[3~" [deletechar])
;; (define-key function-key-map "\e[4~" [select])
;; (define-key function-key-map "\e[5~" [prior])
;; (define-key function-key-map "\e[6~" [next])
;; (define-key global-map [select] 'set-mark-command)
;; (define-key global-map [insertchar] 'yank)
;; (define-key global-map [deletechar] 'kill-region)

(global-set-key [?\C- ] 'other-window)
(global-set-key "\C-oW" (lambda ()
                          (interactive)
                          (org-babel-load-file (concat user-emacs-directory
						       "org/init.org"))))

;; set keys for Apple keyboard, for emacs in OS X
;;(setq mac-command-modifier 'super) ; make cmd key do Meta
;;(setq mac-option-modifier 'super) ; make opt key do Super
;;(setq mac-control-modifier 'control) ; make Control key do Control
;;(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper


(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\M-m" 'move-beginning-of-line)
(global-set-key "\C-q" 'set-mark-command)
(global-set-key "\C-oD" 'find-name-dired)
(global-set-key "\C-ok" 'comment-region)
(global-set-key "\C-oK" 'helm-show-kill-ring)
(global-set-key "\C-ou" 'uncomment-region)
(global-set-key "\C-on" 'er/expand-region)
(global-set-key "\C-oe" 'eval-current-buffer)
(global-set-key "\C-oH" 'highlight-long-lines)
(global-set-key "\C-oh" 'unhighlight-long-lines)
(global-set-key "\C-oq" 'query-replace-regexp)
(global-set-key "\C-or" 'rgrep)
(global-set-key "\C-L" 'delete-other-windows)
;; (global-set-key "\C-B" 'scroll-down)
;; (global-set-key "\C-F" 'scroll-up)
(global-set-key "\C-oG" 'helm-projectile-grep)
(global-set-key (kbd "s-0") 'org-todo-list)

;; Show trailing whitespace, `cause we hates it....
(setq-default show-trailing-whitespace t)

;; Stuff related to configuring Emacs-in-a-window
;;
;; When running GUI Emacs (i.e. on OS-X, which is the only way I run
;; Emacs these days anyways), set the theme to Zenburn, turn off
;; visual noise, fix up the PATH for shells, and allow resizing of
;; window.
(when window-system
  (load-theme 'zenburn t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-exec-path-from-shell-PATH)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease))

(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))
;; Don’t pop up newly-opened files in a new frame – use existing one:
(setq ns-pop-up-frames nil)

(defun jj-move-forward-and-eval ()
  (lambda ()
    (paredit-forward)
   (eval (preceding-sexp))))


;; General Lisp stuff
;; Rainbow delimiters for all programming major modes:
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;; Show paren balancing nicely:
(require 'paren)
(set-face-background 'show-paren-match "white")
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Stuff for Editing Emacs Lisp......................
;; I add a hook for evaluating the expression just before point; I’ve
;; played with auto-indent-mode and flycheck-mode but tired of them. I
;; do want paredit though.
(define-key emacs-lisp-mode-map (kbd "<s-return>") 'eval-last-sexp)

;;(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
;;(add-hook 'emacs-lisp-mode-hook 'auto-indent-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
	    (aggressive-indent-mode 1)))


(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))


;; Company Mode............
;; stolen from https://github.com/bbatsov/prelude/blob/\
;; fe7997bc6e05647a935e279094a9c571d175e2dc/modules/prelude-company.el
(require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)

;; Buffer movement
;;(require 'buffer-move)
;; define some keybindings. For example, i use :
(global-set-key (kbd "<s-up>")     'buf-move-up)
(global-set-key (kbd "<s-down>")   'buf-move-down)
(global-set-key (kbd "<s-left>")   'buf-move-left)
(global-set-key (kbd "<s-right>")  'buf-move-right)


;; Correcting single-whitespaced toplevel forms
(defun correct-single-whitespace ()
  "Correct single-spaced Lisp toplevel forms."
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp ")\n\n(" nil t)
    (replace-match ")\n\n\n(" t nil)))
(global-set-key "\C-oQ" 'correct-single-whitespace)

(require 'magit)

;; Helm.......................
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
;; https://github.com/bbatsov/helm-projectile/issues/116 :
;;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(helm-mode 1)

;; Projectile.................
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Hideshow Package...........
(load-library "hideshow")
(add-hook 'clojure-mode-hook 'hs-minor-mode)
(add-hook 'html-mode-hook 'hs-minor-mode)
(global-set-key [backtab] 'hs-toggle-hiding)

;; Org Mode...................
(require 'org)
;;(require 'org-install)
(require 'ob-tangle)
(org-babel-do-load-languages
 'org-babel-load-languages '((sh . t)
                             (clojure . t)
                             (plantuml . t)))

(setq org-plantuml-jar-path (concat (getenv "HOME")
                                    "/bin/plantuml.jar"))

(setq my/org-babel-evaluated-languages
      '(emacs-lisp plantuml sh))

(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "ditaa"))
       (not (string= lang "plantuml"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-babel-clojure-backend 'cider)
(require 'ob-clojure)
;;(require 'cider)

;; (org-babel-load-file (concat user-emacs-directory "org/init.org"))

;; (org-babel-load-file "tmp.org")

;; Export ” as “ and ”:
(setq org-export-with-smart-quotes t)
;; GTD-style TODO states:
(setq org-todo-keywords
      '((sequence "TODO(d!)" "STARTED(d!)" "DONE(d!)" "WAITING(d!)" "CANCELED(d!)")))
;; (setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("STARTED" . "yellow")
	("DONE" . "#5F7F5F")
	("ELSEWHERE" . "#5F7F5F")
	("CANCELED" . "#8CD0D3")))

(set-default-font "Bitstream Vera Sans Mono")
;; Magit / GitHub ...........
;; (require 'magit-gh-pulls)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "8d8423e863b3fbc6346758d726bae66b3d2bacac526067d7d8f2d710203066eb" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(magit-push-always-verify nil)
 '(markdown-command "/usr/local/bin/markdown")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (plantuml-mode terraform-mode solarized-theme kooten-theme adoc-mode aggressive-indent bea beacon cider clj-refactor clojure-mode clojure-snippets company expand-region git-timemachine hcl-mode helm helm-projectile htmlize json-mode lorem-ipsum magit magit-gh-pulls markdown-mode multiple-cursors olivetti paredit projectile rainbow-delimiters which-key yasnippet zenburn-theme
                   (quote
                    (recentf-max-menu-items 100)))))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36" t)
 '(term-default-fg-color "#839496" t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

;;; Fix junk characters in shell-mode
(add-hook 'shell-mode-hook
          'ansi-color-for-comint-mode-on)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))


;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)


(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-load-directory "~/.emacs.d/snippets")


;; Org / Yasnippet conflict (http://orgmode.org/manual/Conflicts.html):
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (yas-minor-mode 1)
            (require 'ob-plantuml)
            (define-key org-mode-map (kbd "C-a") 'split-window-horizontally)
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))


;; Moving sexps up and down.......................................
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp arg)
  (forward-sexp 1))


(global-set-key (kbd "S-s-<down>")
		(lambda ()
		  (interactive)
		  (transpose-sexps 1)))


(global-set-key (kbd "S-s-<up>")
		(lambda ()
		  (interactive)
		  (reverse-transpose-sexps 1)))


;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Backups...........................
;; Tell Emacs to write backup files to their own directory, and make backups even for files in revision control:

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

(setq vc-make-backup-files t)

;; Pesky dialog boxes :-(
;; http://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Omit #@!(*&^&! tabs!!!!
(setq-default indent-tabs-mode nil)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown" t)
(add-to-list 'auto-mode-alist '("\\.md.html\\'" . markdown-mode)
             (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; Beacon Mode
(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#888888")

;; which-key
(require 'which-key)
(which-key-mode)

;; Pop shells in current frame
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; suppress irritating terminal warnings:
(setenv "PAGER" "cat")

;; I'm blind. So sue me.
(set-face-attribute 'default nil :height 180)

;; Changing default font
(set-default-font "Bitstream Vera Sans Mono-10")

(provide 'init)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(setq term-default-bg-color nil) ;"#211E1E"
(setq term-default-fg-color nil) ;"#AAAAAA"


(add-hook 'shell-mode-hook (lambda ()
                             (setq show-trailing-whitespace nil)))

(defun my-hide-trailing-whitespace-maybe ()
  "Disable `show-trailing-whitespace' in selected modes."
  (when (derived-mode-p 'term-mode
                        'some-other-mode
                        '...) ;'shell-mode
    (setq show-trailing-whitespace nil)
    (set-background-color "black")))

(add-hook 'after-change-major-mode-hook
          'my-hide-trailing-whitespace-maybe)

(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode
                                        nil)))
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "#000000" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
(add-hook 'prog-mode-hook 'linum-mode)

;; Don't ask to follow symlinks to a vc file
(setq vc-follow-symlinks t)
