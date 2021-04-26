;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; TODO: Keybinding for newline and indent? S-RET?
;; TODO: Make sure insert always inserts balanced parens
;; TODO: Write fn that goes to beginning of line then does an
;;       evil-sp-delete-line

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andrew Lai"
      user-mail-address "andrew.s.lai5@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 16))
(setq org-directory "~/org/")       ;; MUST BE SET BEFORE ORG LOADS
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smartparens customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/hlissner/doom-emacs/issues/478
(after! smartparens
  (dolist (brace '("(" "{" "[" "\""))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p
                                 sp-point-before-same-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cider face customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-result-overlay-face ((t (:foreground "lightgreen" :slant italic)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'evil-smartparens-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs lisp state config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 'evil-smartparens-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil lisp state config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil-lisp-state
  :init (setq evil-lisp-state-global t))
(add-hook 'evil-lisp-state-entry-hook 'evil-smartparens-mode)
(add-hook 'evil-lisp-state-entry-hook 'aggressive-indent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :defer t)
(use-package! treemacs-persp
  ;;:defer t
  :when (featurep! :ui workspaces)
  :after (treemacs persp-mode)
  :config
  (treemacs-set-scope-type 'Perspectives))

(after! treemacs
  (defun +treemacs--init ()
    (require 'treemacs)
    (let ((origin-buffer (current-buffer)))
      (cl-letf (((symbol-function 'treemacs-workspace->is-empty?)
                 (symbol-function 'ignore)))
        (treemacs--init))
      (unless (bound-and-true-p persp-mode)
        (dolist (project (treemacs-workspace->projects (treemacs-current-workspace)))
          (treemacs-do-remove-project-from-workspace project)))
      (with-current-buffer origin-buffer
        (let ((project-root (or (doom-project-root) default-directory)))
          (treemacs-do-add-project-to-workspace
           (treemacs--canonical-path project-root)
           (doom-project-name project-root)))
        (setq treemacs--ready-to-follow t)
        (when (or treemacs-follow-after-init treemacs-follow-mode)
          (treemacs--follow))))))

(use-package treemacs-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
  :config
  (load-theme 'doom-one t)

  (doom-themes-visual-bell-config)              ;; Flashing mode-line on errors
  (doom-themes-treemacs-config)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-height 40)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline--flymake-icon nil)
  (setq doom-modeline--flycheck-text nil)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-percent-position nil)
  ;;(doom-themes-org-config)
  )

;; Custom doom-modeline
(defface doom-modeline-evil-lisp-state
  '((t (:inherit (font-lock-constant-face bold))))
  "Face for the lisp state tag in evil state indicator."
  :group 'doom-modeline-faces)

(after! doom-modeline
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode))
(setq evil-normal-state-tag " <NORMAL> ")
(setq evil-insert-state-tag " <INSERT> ")
(setq evil-visual-state-tag " <VISUAL> ")
(setq evil-motion-state-tag " <MOTION> ")
(setq evil-emacs-state-tag " <EMACS> ")
(setq evil-lisp-state-tag " <LISP> ")
(setq evil-multiedit-state-tag " <MULTI-EDIT> ")

(custom-set-faces!
  '(doom-modeline-evil-insert-state :inherit doom-modeline-urgent)
  '(doom-modeline-evil-visual-state :inherit doom-modeline-warning)
  '(doom-modeline-evil-normal-state :inherit doom-modeline-debug)
  ;;'(doom-modeline-evil-lisp-state :inherit font-lock-constant-face)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;(use-package org-bullets
  ;;:defer t
  ;;:config
  ;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;(use-package color :defer t)
;;(set-face-attribute 'org-block nil :background
                    ;;(color-darken-name
                     ;;(face-attribute 'default :background) 3))

(setq org-src-block-faces '(("emacs-lisp" (:background "#f9edff"))
                            ("clojure" (:background "#3b372b"))
                            ("bash" (:background "#171717"))
                            ("ruby" (:background "#1c0c0e"))
                            ("python" (:background "#102611"))))

(setq org-babel-clojure-backend 'cider)
;;(use-package cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indenting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package aggressive-indent
  :defer t
  :config
  (add-to-list 'aggressive-indent-protected-commands 'undo-fu-only-undo))

(after! clojure-mode
  (define-clojure-indent
    (PUT 2)
    (POST 2)
    (PATCH 2)
    (DELETE 2)
    (GET 2)
    (addtest 1)
    (are 1)
    (context 2)
    (defsystest 1)
    (lz-post-lead 2)
    (op/p 1)
    (ac/add-record 1)
    (prop/for-all 1)
    (middleware 1)
    (wrap-response 3)
    (pending 1)
    (reqfn 1)
    (route-middleware 1)
    (routes 0)))

(setq clojure-indent-style 'align-arguments)

(defun cider-repl-in-new-frame ()
  (let ((new-frame (make-frame '((name . "REPL")
                                 ;;(minibuffer . nil)
                                 )))
        (repl (car (seq-filter (lambda (buf) (string-prefix-p "*cider" (buffer-name buf)))
                      (buffer-list)))))
    (select-frame-set-input-focus new-frame)
    ;;(print (frame-first-window new-frame))
    (switch-to-buffer repl)))

(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-connected-hook '(cider-repl-in-new-frame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDiff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun update-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute 'diff-added nil
                      :foreground "white" :background "blue")
  (set-face-attribute 'diff-removed nil
                      :foreground "white" :background "red3")
  (set-face-attribute 'diff-changed nil
                      :foreground "white" :background "purple"))
(eval-after-load "diff-mode"
  '(update-diff-colors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java :init (add-hook 'java-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TLS fix to allow connection to 4clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gnutls-algorithm-priority
      "SECURE128:+SECURE192:-VERS-ALL:+VERS-TLS1.3:+VERS-TLS1.2:+SIGN-RSA-SHA1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization layers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq display-line-numbers-type nil)
;;(setq global-linum-mode t)

(load! "+bindings")
(load! "+treemacs")
(load! "local-packages/fourclojure.el/fourclojure")
