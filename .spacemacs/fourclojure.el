;;; fourclojure.el --- 4Clojure problems at your fingertips!

;; Copyright (C) 2019 Andrew Lai

;; Author: Andrew Lai <andrew.s.lai5@gmail.com>
;; Version: 1.0
;; Package-Requires: ((dom "1.0"))
;; Keywords: clojure, 4clojure
;; URL:


;;; Commentary:

;; fourclojure.el is a way to easily import 4clojure problems directly
;; into Emacs.


;;; TODO:
;; Open two windows to start - all problems on the left
;;  fourclojure.clj on the right...
;; Jack into the project on loading
;; General cleaning up the code base

;;;

(require 'dom)
(require 'org)

(setq fourclojure-buffer "fourclojure.clj")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun third (x) (nth 2 x))

(defun curl (url)
  (let* ((b (url-retrieve-synchronously url t)))
    (with-current-buffer b
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      ;;(buffer-string)
      (libxml-parse-html-region (point-min) (point-max)))))

;;(curl "http://4clojure.com/problems")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving a single 4c problem and add to clj file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-4clojure-test-cases (full-dom)
  (mapcar (lambda (x) (nth 2 (nth 0 (dom-by-tag x 'pre))))
          (dom-by-tag
           (dom-by-class full-dom "testcases")
           'tr)))

(defun flatten (xs)
  (when xs
    (cond
     ;; remove (symbol . string)
     ((and (listp xs)
           (symbolp (car xs))
           (stringp (cdr xs))) "")
     ;; remove (table ...)
     ((and (listp xs)
           (symbolp (car xs))
           (eq 'table (car xs))) "")
     ((listp xs) (concat (flatten (car xs)) (flatten (cdr xs))))
     ((stringp xs) xs))))

(defun linewrap-and-comment (data width)
  (let ((acc ""))
    (while (< 0 (length data))
      (setq acc (concat acc ";; "(substring data 0 (min width (length data))) "\n"))
      (if (< width (length data))
          (setq data (substring data width nil))
        (setq data nil)))
    acc))

(defun print-4clojure-problem-to-clj-file (4c-problem-endpoint clj-buffer)

  ;; insert namespace declaration
  (if (get-buffer clj-buffer)
      (setq buff (get-buffer clj-buffer))
    (progn (setq buff (get-buffer-create clj-buffer))
           (set-buffer buff)
           (insert (format "(ns %s)\n\n" clj-buffer))))

  (let* ((url (concat "http://4clojure.com/" 4c-problem-endpoint))
         (parsed-body (curl url))
         (title (nth 2 (car (dom-by-id parsed-body "prob-title"))))
         (problem (dom-by-id parsed-body "prob-desc"))
         (problem-text (replace-regexp-in-string "\r\n" "" (flatten problem)))
         (test-cases (extract-4clojure-test-cases parsed-body))
         )
    (set-buffer buff)
    (clojure-mode)
    (insert (apply 'concat (make-list 80 ";")) "\n")
    (insert (concat ";; " title "\n"))
    (insert (concat ";; " url "\n;;\n"))
    (insert (linewrap-and-comment problem-text 77))
    (insert (apply 'concat (make-list 80 ";")) "\n")
    (insert "\n\n")
    (insert "(defn my-fn [& xs]\n\t)\n\n")
    (mapc (lambda (x)
            (when (stringp x)
              (insert (replace-regexp-in-string "__" "my-fn" (replace-regexp-in-string "\r" "" x))
                      "\n\n\n")))
          test-cases))
  (concat "Imported " 4c-problem-endpoint " to " clj-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieve 4c problem list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun follow-4clojure-link (event)
  (let ((problem (thing-at-point 'symbol 'no-properties)))
    (print-4clojure-problem-to-clj-file problem fourclojure-buffer)))

(define-button-type '4clojure-hyperlink
  'action 'follow-4clojure-link
  'follow-link t
  'help-echo "Go to "
  'help-args "test")

;; Extract into add-row function and format-4c function
(defun add-problem (problem)
  (cl-flet ((create-hyperlink ()
                              (progn (goto-char (point-max))
                                     (backward-sexp)
                                     (backward-sexp)
                                     (make-text-button (point) (point-max)
                                                       :type '4clojure-hyperlink)
                                     (goto-char (point-max))
                                     (insert "\n"))))
    (let ((name (nth 0 problem))
          (url (nth 1 problem))
          (difficulty (nth 2 problem)))
      (insert "|" name " | " difficulty " | " url " |")
      (create-hyperlink))))

(defun dom->4clojure-problems (full-dom)
  (cl-flet* ((get-link (row) (cdr (car (nth 1 (nth 2 (nth 2 row))))))
             (get-problem-name (row) (nth 2 (nth 2 (nth 2 row))))
             (get-difficulty (row) (nth 2 (nth 3 row)))
             (extract-problem (row) (let ((link (get-link row))
                                          (problem-name (get-problem-name row))
                                          (difficulty (get-difficulty row)))
                                      (list problem-name link difficulty))))
    (let ((all-rows (cdr (dom-by-tag full-dom 'tr))))
      (mapcar (lambda (x) (extract-problem x)) all-rows))))

(defun populate-buffer-with-4clojure-problems (b)
  (let* ((dom (curl "http://4clojure.com/problems"))
         (problems (dom->4clojure-problems dom))
         (problems-sorted-by-difficulty (cl-sort problems
                                                 'string-lessp
                                                 :key
                                                 'third)))
    ;; Add header
    (set-buffer b)
    (insert "| name | difficulty | url |\n")
    (insert "|---|---|---|\n")

    ;; Add all problems to buffer
    (mapc (lambda (x) (add-problem x)) problems-sorted-by-difficulty))

  ;; Format as org mode table
  (switch-to-buffer b)
  (markdown-mode)
  (org-table-align))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize 4clojure package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 4clojure ()
  (interactive)
  (populate-buffer-with-4clojure-problems (generate-new-buffer "*4clojure problems*"))
  (get-buffer-create fourclojure-buffer)
  (spacemacs/layout-double-columns)
  (evil-window-move-far-right)
  (switch-to-buffer fourclojure-buffer)
  (insert (format "(ns %s)\n\n" fourclojure-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun test-flatten ()
  ;; Basic flattening
  (assert (equal (flatten '("a" "b" ("c"))) "abc"))

  ;; Nested maps
  (assert (equal (flatten '(("a" "b" ("c")))) "abc"))

  ;; Ignore tables
  (assert (equal (flatten '((table "b" ("c")))) ""))

  ;; Ignore (symbol . string)
  (assert (equal (flatten '((div . "b"))) ""))

  ;; Don't ignore (symbol string)
  (assert (equal (flatten '((div "b"))) "b")))

(defun test-extract-test-cases ()
  (assert (equal
           '("(= 3 ((__ nth) 2 [1 2 3 4 5]))"
             "(= true ((__ >) 7 8))"
             "(= 4 ((__ quot) 2 8))"
             "(= [1 2 3] ((__ take) [1 2 3 4 5] 3))")
           (extract-4clojure-test-cases
            (with-temp-buffer
              (insert-file-contents "./test.html")
              (libxml-parse-html-region (point-min) (point-max)))))))

(defun test-extract-problem-info ()
  (let ((all-problems
         (dom->4clojure-problems (with-temp-buffer
                                   (insert-file-contents "./all_problems.html")
                                   (libxml-parse-html-region (point-min)
                                                             (point-max))))))
    (assert (eq 156 (length all-problems)))
    (assert (equal '("Nothing but the Truth" "\\\"/problem/1\\\"" "Elementary")
                   (car all-problems)))))

;;; Test runner

(defvar run-tests? t)

(when run-tests?
  (print "Running tests")
  (test-flatten)
  (test-extract-problem-info)
  (test-extract-test-cases)
  )
