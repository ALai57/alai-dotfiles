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
(setq doom-font "Consolas 14")
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
  :when (modulep! :ui workspaces)
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

;;(let* ((variable-tuple
        ;;(cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ;;((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ;;((x-list-fonts "Verdana")         '(:font "Verdana"))
              ;;((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              ;;(nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       ;;(base-font-color     (face-foreground 'default nil 'default))
       ;;(headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
  ;;(custom-theme-set-faces
   ;;'user
   ;;`(org-level-8 ((t (,@headline ,@variable-tuple))))
   ;;`(org-level-7 ((t (,@headline ,@variable-tuple))))
   ;;`(org-level-6 ((t (,@headline ,@variable-tuple))))
   ;;`(org-level-5 ((t (,@headline ,@variable-tuple))))
   ;;`(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   ;;`(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   ;;`(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   ;;`(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   ;;`(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

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
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))

'(ediff-current-diff-A ((t (:background "#46382c"))))
'(ediff-current-diff-B ((t (:background "#343d32"))))
'(ediff-current-diff-C ((t (:background "#313939"))))
'(ediff-fine-diff-A ((t (:background "#66584c"))))
'(ediff-fine-diff-B ((t (:background "#545d52"))))
'(ediff-fine-diff-C ((t (:background "#515959"))))
'(ediff-odd-diff-A ((t (:background "#242424"))))
'(ediff-odd-diff-B ((t (:background "#242424"))))
'(ediff-odd-diff-C ((t (:background "#242424"))))
'(ediff-even-diff-A ((t (:background "#242424"))))
'(ediff-even-diff-B ((t (:background "#242424"))))
'(ediff-even-diff-C ((t (:background "#242424"))))
 )

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
  (setq clojure-indent-style 'always-indent)
  (define-clojure-indent
    (DELETE 2)
    (GET 2)
    (PATCH 2)
    (POST 2)
    (PUT 2)
    (are 1)
    (context 2)
    (for-all 1)
    (middleware 1)
    (pending 1)
    (quick-check 1)
    (route-middleware 1)
    (routes 0)
    (with-fake-routes 1)
    (wrap-response 3)
    ))


(defun always-indent! (params)
  (interactive "P")
  (setq clojure-indent-style 'always-indent))

(defun always-align! (params)
  (interactive "P")
  (setq clojure-indent-style 'always-align))


(set-formatter! 'cljstyle "cljstyle pipe" :modes '(clojure-mode))


;;(defun cider-repl-in-new-frame ()
  ;;(let ((new-frame (make-frame '((name . "REPL")
                                 ;;;;(minibuffer . nil)
                                 ;;)))
        ;;(repl (car (seq-filter (lambda (buf) (string-prefix-p "*cider" (buffer-name buf)))
                      ;;(buffer-list)))))
    ;;(select-frame-set-input-focus new-frame)
    ;;(print (frame-first-window new-frame))
    ;;(switch-to-buffer repl)))
;;
;;(setq cider-repl-pop-to-buffer-on-connect nil)
;;(setq cider-connected-hook '(cider-repl-in-new-frame))

;; Teach CIDER to remember the monorepo nREPL
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set me to your local `stonehenge' repo!
(setq STONEHENGE-PATH
      "~/spl/stonehenge/")

(setq MONOREPO-CONN
      '(:project "stonehenge" :host "localhost" :port "7888"))

(setq MONOREPO-CLJS-CONN
      '(:project "stonehenge-cljs" :host "localhost" :port "7889"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for parsing the connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ->known-endpoint (conn)
  (mapcar (lambda (x) (plist-get conn x))
          (list :project :host :port)))

(defun ->conn-string (conn)
  (string-join (->known-endpoint conn) ":"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for staring the monorepl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun init-monorepl (current-repl)
  ;; Need to set the current buffer so the REPL output gets piped to the right place
  (with-current-buffer current-repl
    (print "Starting MonoREPL....")
    (cider-nrepl-request:eval "(start)" (cider-repl-init-eval-handler nil))
    (cider-nrepl-request:eval "(println ::repl-ready)" (cider-repl-init-eval-handler nil))))

(defun monorepl? (b)
  (string-match (->conn-string MONOREPO-CONN) (buffer-name b)))

(defun cider-init-hook ()

  ;; override cider's nrepl session lookup dependency, to cover relative maven path to support Stonehenge
  ;; this is needed because orchard, the library cider-nrepl middleware depends on, will fetch symbol location from different path than classpath.
  ;; Because of that, once inside the dependency library jar, cider-find-var no longer works because it can't find the nrepl session since the filename does not fully match with classpath fetched from stonehenge nrepl session.
  ;; To fix it, the default cider function "sesman-friendly-session-p" is overridden to not check exact classpath to filepath match, but instead only the maven substring.
  ;; example:
  ;; file location found from repl:
  ;; "/private/var/tmp/_bazel_ywei/382bc82a66d87a56221cef203b9cc9cb/external/maven/v1/https/repo1.maven.org/maven2/com/cognitect/aws/api/0.8.498/api-0.8.498.jar:cognitect/aws/client/api.clj"
  ;; classpath returned from repl:
  ;; "/private/var/tmp/_bazel_ywei/382bc82a66d87a56221cef203b9cc9cb/execroot/stonehenge/bazel-out/darwin-fastbuild/bin/development/repl/repl.runfiles/maven/v1/https/repo1.maven.org/maven2/com/cognitect/aws/api/0.8.498/"
  ;;
  ;; default cider only checks prefix, where we will attempt to match "maven/v1/https/repo1.maven.org/maven2/com/cognitect/aws/api/0.8.498" part only.
  (defun try-trim-maven-path (path)
    (let ((maven-index (string-match "maven" path)))
      (if maven-index
          (substring path maven-index (length path))
        path)))

  (cl-defmethod sesman-friendly-session-p ((_system (eql CIDER)) session)
    "Check if SESSION is a friendly session."
    (setcdr session (seq-filter #'buffer-live-p (cdr session)))
    (when-let* ((repl (cadr session))
                (proc (get-buffer-process repl))
                (file (file-truename (or (buffer-file-name) default-directory))))
      ;; With avfs paths look like /path/to/.avfs/path/to/some.jar#uzip/path/to/file.clj
      (when (string-match-p "#uzip" file)
        (let ((avfs-path (directory-file-name (expand-file-name (or (getenv "AVFSBASE")  "~/.avfs/")))))
          (setq file (replace-regexp-in-string avfs-path "" file t t))))
      (when (process-live-p proc)
        (let* ((classpath (or (process-get proc :cached-classpath)
                              (let ((cp (with-current-buffer repl
                                          (cider-classpath-entries))))
                                (process-put proc :cached-classpath cp)
                                cp)))
               (classpath-roots (or (process-get proc :cached-classpath-roots)
                                    (let ((cp (thread-last classpath
                                                           (seq-filter (lambda (path) (not (string-match-p "\\.jar$" path))))
                                                           (mapcar #'file-name-directory)
                                                           (seq-remove  #'null))))
                                      (process-put proc :cached-classpath-roots cp)
                                      cp))))
          (or (seq-find (lambda (path) (string-match path file))
                        (mapcar 'try-trim-maven-path classpath))
              (seq-find (lambda (path) (string-match path file))
                        (mapcar 'try-trim-maven-path classpath-roots)))))))

  ;; initialize monorepl after starting cider
  (let ((current-repl (cider-current-repl nil 'ensure)))
    (when (monorepl? current-repl)
      (init-monorepl current-repl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AWS SSO Login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq AWS-PROFILES
      '(("prod"      . (("account" . 820416642700) ("role" . "tribe_sso_permission_set")))
        ("stage"     . (("account" . 820416642700) ("role" . "tribe_sso_permission_set")))
        ("dev"       . (("account" . 941333304678) ("role" . "tribe_sso_permission_set")))
        ("dev_admin" . (("account" . 941333304678) ("role" . "AWSPowerUser")))))

(setq SSO-TOKEN-NAME
      "~/.aws/sso/cache/7576c5160da8aa24c99cbadc0de700a7d4edd367.json")

;; Returns a hash map
(defun get-access-token (filename)
  (json-parse-string
   (shell-command-to-string (format "cat %s" filename))))

;; Returns a hash map
(defun get-creds (account role token)
  (json-parse-string
   (shell-command-to-string
    (format "aws --region us-east-1 sso get-role-credentials --account-id %s --role-name %s --access-token %s"
            account role token))))

;; Sets environment variables to be able to talk to AWS
(defun aws-login (profile-name)
  (shell-command-to-string (concat "aws sso login --profile " profile-name))
  (let* ((props (cdr (assoc profile-name AWS-PROFILES)))
         (acct  (cdr (assoc "account" props)))
         (role  (cdr (assoc "role" props)))
         (access-token (gethash "accessToken" (get-access-token SSO-TOKEN-NAME)))
         (creds        (gethash "roleCredentials" (get-creds acct role access-token))))
    (setenv "AWS_ACCESS_KEY_ID"     (gethash "accessKeyId" creds))
    (setenv "AWS_SECRET_ACCESS_KEY" (gethash "secretAccessKey" creds))
    (setenv "AWS_SESSION_TOKEN"     (gethash "sessionToken" creds))
    (setenv "AWS_REGION" "us-east-1")
    (print "Success setting AWS Environment variables: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN")
    creds))

;; (setq EXAMPLE (aws-login "prod"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuring CIDER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cider-known-endpoints
      (list (->known-endpoint MONOREPO-CONN)
            (->known-endpoint MONOREPO-CLJS-CONN)))

(setq cider-connected-hook '(cider-init-hook))


(use-package parseedn)

(defun create-env-vars (map)
  (maphash (lambda (k v)
             (setenv k v))
           map))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun apply-env-file (f)
  (let* ((edn-hashmap (parseedn-read-str (get-string-from-file f)))
         (okta-env    (gethash "OKTA_ENV" edn-hashmap)))
    (create-env-vars edn-hashmap)
    (when okta-env
      (print (format "Logging in to SSO in %s" okta-env))
      (aws-login okta-env))))

;;(apply-env-file "~/spl/stonehenge/splash/services/document_administration/.repl.document-administration.local.edn")

(defun select-env (params)
  (interactive "P")
  (let ((default-directory (projectile-project-root))
        (counsel-find-file-ignore-regexp nil))
    (ivy-read "Select environment: " #'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :predicate (lambda (x) (string-match-p "^\\.repl\\..*[^/]$" x))
              :initial-input nil
              :action (lambda (f)
                        (print (format "Applying %s environment configuration." f))
                        (apply-env-file f))
              :preselect (counsel--preselect-file)
              :require-match t
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'select-env)))

(defun cider-connect-stonehenge-cljs (params)
  (interactive "P")
  ;; Open a browser that connects a JS environment to the REPL
  (browse-url "http://localhost:9630")
  (browse-url "http://localhost:1337")
  (browse-url "http://localhost:9630/repl-js/browser-repl")
  (cider-connect-cljs (list :host           "localhost"
                            :port           7889
                            :cljs-repl-type 'shadow-select)))

;;(select-env nil)

(defun cider-jack-in-stonehenge (params)
  "Start an nREPL server for the stonehenge project and connect to it."
  (interactive "P")
  (select-env params)
  (let ((params (thread-first params
                              (cider--update-project-dir)
                              (cider--check-existing-session)
                              (cider--update-jack-in-cmd))))
    (nrepl-start-server-process
     STONEHENGE-PATH
     (concat STONEHENGE-PATH "repl")
     (lambda (server-buffer)
       (cider-connect-sibling-clj params server-buffer)))))

(defun get-nrepl-server-processes ()
  (seq-filter (lambda (proc)
                (string-match-p "nrepl-server" (process-name proc)))
              (process-list)))

(defun cider-jack-in-das (params)
  "Start an nREPL server for the stonehenge project and connect to it."
  (interactive "P")
  (let ((params (thread-first params
                              (cider--update-project-dir)
                              (cider--check-existing-session)
                              (cider--update-jack-in-cmd))))
    (nrepl-start-server-process
     STONEHENGE-PATH
     "bazel run //splash/ui/das:repl"
     (lambda (server-buffer)
       (setq cider-shadow-default-options ":browser-repl")
       (sit-for 20)
       (cider-connect-stonehenge-cljs params)
       ))))

(defun cider-jack-in-lpm (params)
  "Start an nREPL server for the stonehenge project and connect to it."
  (interactive "P")
  (let ((params (thread-first params
                              (cider--update-project-dir)
                              (cider--check-existing-session)
                              (cider--update-jack-in-cmd))))
    (nrepl-start-server-process
     STONEHENGE-PATH
     "bazel run //splash/ui/lpm:repl"
     (lambda (server-buffer)
       (setq cider-shadow-default-options ":browser-repl")
       (sit-for 20)
       (cider-connect-stonehenge-cljs params)
       ))))

(defun kill-all-REPL-servers (params)
  (interactive "P")
  (mapcar #'delete-process (get-nrepl-server-processes)))
;;
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
;; Portal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start-portal (params)
  (interactive "P")
  (cider-nrepl-sync-request:eval "(require '[portal.api :as p])")
  (cider-nrepl-sync-request:eval "(add-tap #'p/submit)")
  (cider-nrepl-sync-request:eval "(p/open {:window-title \"Portal taps\" :launcher :emacs})"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-java :init (add-hook 'java-mode-hook 'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package direnv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization layers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load! "+bindings")
(load! "+treemacs")
;;(load! "lisp/fourclojure")

(setq plantuml-jar-path "~/bin/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type "svg")
;; Open in same window
(add-to-list 'display-buffer-alist
             '("*PLANTUML Preview*" display-buffer-in-direction)
             )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html
(setq FONT-SIZE 14)

(defun set-font-size (size)
  "Set default font globally.
This command useful for making font large when you want to do video livestream.
URL `http://xahlee.info/emacs/emacs/emacs_set_default_font_size.html'
Version: 2021-07-26 2021-08-21 2022-08-05"
  (set-frame-font
   (cond
    ((string-equal system-type "darwin")
     (if (member "Consolas" (font-family-list)) (format "Consolas-%s" size) nil)
     ;;(if (member "Menlo" (font-family-list)) (format "Menlo-%s" $fSize) nil)
     )
    ((string-equal system-type "gnu/linux")
     (if (member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono" nil))
    (t nil))
   t t))

(defun increase-font-size ()
  (interactive)
  (setq FONT-SIZE (+ FONT-SIZE 2))
  (set-font-size FONT-SIZE))

(defun decrease-font-size ()
  (interactive)
  (if (< 4 (- FONT-SIZE 2))
      (setq FONT-SIZE (- FONT-SIZE 2)))
  (set-font-size FONT-SIZE))

;;(increase-font-size)
;;(decrease-font-size)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP mode helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-outgoing-called-children-hierarchy (args)
  (interactive "P")
  (lsp-treemacs-call-hierarchy t))

(defun lsp-incoming-callers-hierarchy (args)
  (interactive "P")
  (lsp-treemacs-call-hierarchy nil))
