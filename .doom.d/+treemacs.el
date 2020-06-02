;;; ~/.doom.d/+treemacs.el -*- lexical-binding: t; -*-


(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".clj" :v-adjust 0))
 :extensions ("clj"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".emacs" :v-adjust 0))
 :extensions ("el"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".json" :v-adjust 0))
 :extensions ("json"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".sql" :v-adjust 0))
 :extensions ("sql"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".git" :v-adjust 0))
 :extensions ("git" "gitignore"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file "dockerfile" :v-adjust 0))
 :extensions ("dockerfile" "dockerignore"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".py" :v-adjust 0))
 :extensions ("py" "pyc"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".md" :v-adjust 0))
 :extensions ("md"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".sh" :v-adjust 0))
 :extensions ("sh" "zsh"))
(treemacs-create-icon
 :icon (format "  %s\t" (all-the-icons-icon-for-file ".java" :v-adjust 0))
 :extensions (".java" ".class" ".jar"))
