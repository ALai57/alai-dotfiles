;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
(package! cider :pin "80408364f8e6870d8f5a33e0e096b86f1a7ef144")
;;(package! cider-nrepl :pin "8c57436fdd45b47d259cfe31714ff2b1b5908835")
(package! parseedn)
(package! treemacs
  :recipe (:type git :host github :repo "Alexander-Miller/treemacs"))
(package! treemacs-evil)
(package! all-the-icons)
(package! evil-smartparens)
(package! evil-lisp-state
  :recipe (:type git :host github :repo "syl20bnr/evil-lisp-state"))
(package! cider-eval-sexp-fu)
(package! aggressive-indent)
(package! treemacs-persp)

(package! transient :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440")
(package! with-editor :pin "391e76a256aeec6b9e4cbd733088f30c677d965b")
(package! compat :pin "6f73eac")
(package! go-dlv)
;;(package! org-bullets)
;;(package! color)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))
