;;; test-ox-context.el --- Tests for ox-context.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Jason Ross
;;
;; Author: Jason Ross <https://github.com/Jason-S-Ross>
;; Maintainer: Jason Ross <jasonross1024@gmail.com>
;; Created: June 07, 2021
;; Modified: June 07, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:



(unless (featurep 'ox-context)
  (signal 'missing-test-dependency "org-export-context"))
(unless (featurep 'org-inlinetask)
  (signal 'missing-test-dependency "org-inlinetask"))

(require 'cl)
(defun context-test-perms (l)
  "Return all permutations of l"
  (if (null l)
      (list '())
    (mapcan #'(lambda( a )
                (mapcan #'(lambda( p )
                            (list (cons a p)))
                        (context-test-perms (cl-remove a l :count 1))))
            l)))

(defun context-test-build-ConTeXt-argument-regex (arguments &optional nobrackets nopermute)
  "Construct a regexp to match combinations of ARGUMENTS.
ARGUMENTS is an alist of key, value pairs to pass to the
command."
  (concat
   (regexp-quote "[")
   "[[:space:]]*"
   (mapconcat
    (lambda (l)
     (concat
      "\\("
      (mapconcat
       (lambda (kv)
         (concat
          "[[:space:]]*"
          (car kv)
          "[[:space:]]*"
          (regexp-quote "=")
          "[[:space:]]*"
          (and (not nobrackets) (regexp-quote "{"))
          "[[:space:]]*"
          (cdr kv)
          "[[:space:]]*"
          (and (not nobrackets) (regexp-quote "}"))
          "[[:space:]]*"))
       l
       (regexp-quote ",")
       )
      ",?"
      "\\)"))
    (if nopermute (list arguments) (context-test-perms arguments))
    "\\|")
   "[[:space:]]*"
   (regexp-quote "]")
))

(defmacro context-test-with-temp-customization-value (varname value &rest body)
  "Execute BODY with VARNAME set to VALUE.
Sets VARNAME back to VALUE after execution is finished. Returns
result of exectuing BODY"
  `(let ((temp (copy-tree ,varname))
         (result
          (progn
            (setq ,varname ,value)
            (progn ,@body))))
     (setq ,varname temp)
     result))
(def-edebug-spec context-test-with-temp-customization-value (varname value body))
(defmacro context-test-with-temp-modification (varname &rest body)
  "Execute BODY, restorying VARNAME's value afterwards."
  `(let ((temp (copy-tree ,varname))
         (result (progn ,@body)))
     (setq ,varname temp)
     result))
(def-edebug-spec context-test-with-temp-modification (varname body))
(defmacro context-test-with-temp-text (text &rest body)
  "Run BODY in a tempory buffer with Org mode as the active mode
holding TEXT."
  `(with-temp-buffer
    (let ((inside-text (if (stringp ,text) ,text (eval ,text))))
      (org-mode)
      (insert inside-text)
      (goto-char (point-min))
      ,@body)))
(def-edebug-spec context-test-with-temp-text (form body))
;;; Protecting text
(ert-deftest test-org-context-clean-text ()
  "Test smart quotations"
  ;; Protected names
  (should
   (equal
    "testing names of \\TeX{}, \\LaTeX{}, and \\ConTeXt{}. tex, latex, and context shouldn't match."
    (context-test-with-temp-text "testing names of TeX, LaTeX, and ConTeXt. tex, latex, and context shouldn't match."
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Special characters in normal text
  (should
   (equal
    "`\\lettertilde !@\\#\\$\\%^&*()-_=+\\[\\{\\}\\]\\letterbackslash \\|;:'\",<.>/?"
    (context-test-with-temp-text "`~!@#$%^&*()-_=+[{}]\\|;:'\",<.>/?"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Smart Quotation
(ert-deftest test-org-context-smart-quote ()
  "Test smart quotations"
  ;; Basic
  (should
   (equal
    "Quote: \\quotation{Cogito ergo sum} - Descartes"
    (context-test-with-temp-text "Quote: \"Cogito ergo sum\" - Descartes"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Apostrophe
  (should
   (equal
    "Here's a quote: \\quotation{I think, therefore I am}"
    (context-test-with-temp-text "Here's a quote: \"I think, therefore I am\""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "Here's a nested quote: \\quotation{Descartes says \\quote{I think therefore I am}}"
    (context-test-with-temp-text "Here's a nested quote: \"Descartes says 'I think therefore I am'\""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Environments
(ert-deftest test-org-context-environments ()
  "Test org block environments."
  ;; Block quotes
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-blockquote-empty-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-blockquote-environment
             (cons name def)
             (context-test-with-temp-text
              "#+BEGIN_QUOTE
foo bar baz
#+END_QUOTE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("reference" . "org[0-9a-f]+")))
        "\\(.\\|\n\\)*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p def document)
      (string-match-p enumdef document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-blockquote-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-blockquote-environment
             (cons name def)
             (context-test-with-temp-text
              "#+CAPTION: foo
#+BEGIN_QUOTE
foo bar baz
#+END_QUOTE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (regexp-quote "[")
        "[^]]*"
        (regexp-quote "title={foo}")
        "[^]]*"
        (regexp-quote "reference={org")
        "[0-9a-f]+"
        (regexp-quote "}")
        "[^]]*"
        (regexp-quote "]")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname)
        )
       document)
      (string-match-p def document)
      (string-match-p enumdef document))))
  ;; Examples
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-example-empty-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-example-environment
             (cons name def)
             (context-test-with-temp-text
              "#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("reference" . "org[0-9a-f]+")))
        "\\(.\\|\n\\)*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-example-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-example-environment
             (cons name def)
             (context-test-with-temp-text
              "#+CAPTION: foo
#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("reference" . "org[0-9a-f]+")
           ("title" . "foo")))
        "\\(.\\|\n\\)*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  ;; Fixed
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-fixed-environment
            (cons name def)
            (context-test-with-temp-text
             ": foo bar baz"
             (org-trim
              (org-export-as 'context nil nil nil
                             '(:context-preset "empty")))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name))
       document)
      (string-match-p
       def document))))
  ;; Inline Source
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-syntax-engine
            'default
            (context-test-with-temp-customization-value
             org-context-inline-source-environment
             (cons name def)
             (context-test-with-temp-text
              "src_python[:exports code]{print(\"Hello, world!\")}"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "[option=python]")
        "\\(.\\|\n\\)*"
        (regexp-quote "{")
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote "}"))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-syntax-engine
            'vim
            (context-test-with-temp-customization-value
             org-context-inline-source-environment
             (cons name def)
             (context-test-with-temp-text
              "src_python[:exports code]{print(\"Hello, world!\")}"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote name)
        (regexp-quote "Python")
        "\\(.\\|\n\\)*"
        (regexp-quote "{")
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote "}"))
       document)
      (string-match-p
       def document))))
  ;; Property Drawers
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (name2 (format "%i" (random)))
          (def2 (format "%i" (random)))
          (key (format "%i" (random)))
          (val (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-export-with-properties
            t
            (context-test-with-temp-customization-value
             org-context-node-property-command
             (cons name2 def2)
             (context-test-with-temp-customization-value
              org-context-property-drawer-environment
              (cons name def)
              (context-test-with-temp-text
               (format "* Foo
:PROPERTIES:
:%s: %s
:END:" key val)
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))

             ))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\start%s" name))
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\%s" name2))
        "\\(.\\|\n\\)*"
        (context-test-build-ConTeXt-argument-regex
         (list (cons "key" key)
               (cons "value" val)))
        "test"
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\stop%s" name)))
       document)
      (string-match-p def document)
      (string-match-p def2 document))))
  ;; Description items
  (should
   (let* ((rand (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-description-command
            (cons "testitemdescription" rand)
            (context-test-with-temp-text
             "- foo :: bar"
             (org-trim
              (org-export-as 'context nil nil nil
                             '(:context-preset "empty")))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\starttestitemdescription{foo}")
        "\\(.\\|\n\\)*"
        (regexp-quote "bar")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stoptestitemdescription"))
       document)
      (string-match-p
       rand document))))
  ;; Block Source
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-listing-empty-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-syntax-engine
             'default
             (context-test-with-temp-customization-value
              org-context-block-source-environment
              (cons name def)
              (context-test-with-temp-text
               "#+BEGIN_SRC python
print(\"Hello, world!\")
#+END_SRC"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         '(("reference" . "org[0-9a-f]+")
           ("location" . "force,split")))
        "[[:space:]]*"
        (regexp-quote (format "\\start%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         '(("option" . "python")))
        "[[:space:]]*"
        (regexp-quote "print(\"Hello, world!\")")
        "[[:space:]]*"
        (regexp-quote (format "\\stop%s" name))
        "[[:space:]]*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-highlighted-langs-alist
            '(("foolang" . "barlang"))
            (context-test-with-temp-customization-value
            org-context-enumerate-listing-empty-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-syntax-engine
             'default
             (context-test-with-temp-customization-value
              org-context-block-source-environment
              (cons name def)
              (context-test-with-temp-text
               "#+BEGIN_SRC foolang
print(\"Hello, world!\")
#+END_SRC"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty"))))))))
           ))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("reference" . "org[0-9a-f]+")
           ("location" . "force,split")))
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\start%s" name))
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("option" . "barlang")))
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\stop%s" name))
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-listing-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-syntax-engine
             'default
             (context-test-with-temp-customization-value
              org-context-block-source-environment
              (cons name def)
              (context-test-with-temp-text
               "#+CAPTION: foo
#+BEGIN_SRC python
print(\"Hello, world!\")
#+END_SRC"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("title" . "foo")
           ("reference" . "org[0-9a-f]+")
           ("location" . "force,split")))
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\start%s" name))
        "[^[]*"
        (context-test-build-ConTeXt-argument-regex
         '(("option" . "python")))
        "ass"
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\stop%s" name))
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-syntax-engine
            'vim
            (context-test-with-temp-customization-value
             org-context-block-source-environment
             (cons name def)
             (context-test-with-temp-text
              "#+BEGIN_SRC python
print(\"Hello, world!\")
#+END_SRC"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\start%sPython" name))
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\stop%s" name)))
       document)
      (string-match-p
       (concat
        (regexp-quote "\\definevimtyping")
        "[^[]*"
        (regexp-quote (format "[%sPython]" name)))
       document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-vim-langs-alist
            '(("foolang" :vim-name "bazlang" :context-name "Barlang"))
            (context-test-with-temp-customization-value
             org-context-syntax-engine
             'vim
             (context-test-with-temp-customization-value
              org-context-block-source-environment
              (cons name def)
              (context-test-with-temp-text
               "#+BEGIN_SRC foolang
print(\"Hello, world!\")
#+END_SRC"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\start%sBarlang" name))
        "\\(.\\|\n\\)*"
        (regexp-quote "print(\"Hello, world!\")")
        "\\(.\\|\n\\)*"
        (regexp-quote (format "\\stop%s" name)))
       document)
      (string-match-p
       (concat
        (regexp-quote "\\definevimtyping")
        "[^[]*"
        (regexp-quote (format "[%sBarlang]" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         '(("syntax" . "bazlang")
           ("escape" . "command"))
         t))
       document))))
  ;; Verses
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-verse-empty-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-verse-environment
             (cons name def)
             (context-test-with-temp-text
              "#+BEGIN_VERSE
foo bar baz
#+END_VERSE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[^[]*"
        (regexp-quote "[")
        "[^]]*"
        (regexp-quote "reference={org")
        "[0-9a-f]+"
        (regexp-quote "}")
        "[^]]*"
        (regexp-quote "]")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "foo bar baz")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "\\(.\\|\n\\)*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (enumname (format "%i" (random)))
          (enumdef (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-enumerate-verse-environment
            (cons enumname enumdef)
            (context-test-with-temp-customization-value
             org-context-verse-environment
             (cons name def)
             (context-test-with-temp-text
              "#+CAPTION: foo
#+BEGIN_VERSE
foo bar baz
#+END_VERSE"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\start")
        (regexp-quote enumname)
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         '(("title" . "foo")
           ("reference" . "org[0-9a-f]+")))
        "[[:space:]]*"
        (regexp-quote "\\start")
        (regexp-quote name)
        "[[:space:]]*"
        (regexp-quote "foo bar baz")
        "[[:space:]]*"
        (regexp-quote "\\stop")
        (regexp-quote name)
        "[[:space:]]*"
        (regexp-quote "\\stop")
        (regexp-quote enumname))
       document)
      (string-match-p
       def document))))
  ;; Drawers
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-drawer-function
            (lambda (name contents info)
              (let ((formatter
                     (car (plist-get info :context-drawer-command))))
                (format "\\TEST%s{%s}{%s}" formatter name contents)))
            (context-test-with-temp-customization-value
             org-context-drawer-command
             (cons name def)
             (context-test-with-temp-text
              ":MyDrawer:
foo bar baz
:END:"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\TEST%s" name))
        "[^{]*"
        (regexp-quote "{")
        "[^}]*"
        (regexp-quote "MyDrawer")
        "[^}]*"
        (regexp-quote "}")
        "\\(.\\|\n\\)*"
        (regexp-quote "{")
        "[^}]*"
        (regexp-quote "foo bar baz")
        "[^}]*"
        (regexp-quote "}"))
       document)
      (string-match-p def document))))
  ;; Inline Tasks
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-inlinetask-function
            (lambda (todo todo-type priority title tags contents info)
              (let ((format-command
                     (org-string-nw-p (car (plist-get info :context-inlinetask-command)))))
                (if format-command
                    (format
                     "\\%s
  [%s]"
                     format-command
                     (org-context--format-arguments
                      (list
                       (cons "TestTodo" todo)
                       (cons "TestTodoType" todo-type)
                       (cons "TestPriority" priority)
                       (cons "TestTitle" title)
                       (cons "TestTags" (org-make-tag-string (mapcar #'org-context--protect-text tags)))
                       (cons "TestContents" contents))))
                  (concat title "\\hairline" contents "\\hairline")))
              )
            (context-test-with-temp-customization-value
             org-context-inlinetask-command
             (cons name def)
             (context-test-with-temp-text
              "*************** TODO [#A] my task :tag1:tag2:
foo bar baz
*************** END"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         '(("TestTodo" . "TODO")
           ("TestTodoType" . "todo")
           ("TestTitle" . "my task")
           ("TestTags" . ":tag1:tag2:")
           ("TestContents" . "foo bar baz"))
         nil t))
       document)
      (string-match-p def document))))
  ;; Planning
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (plan-string (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-timestamp-function
           (lambda (timestamp)
             (format-time-string
              "TEST TIMESTAMP %Y"
              (org-timestamp-to-time timestamp)))
            (context-test-with-temp-customization-value
             org-export-with-planning
             t
             (context-test-with-temp-customization-value
              org-context-planning-command
              (cons name def)
              (context-test-with-temp-text
               "*** TODO write article about the Earth for the Guide
DEADLINE: <2004-02-29 Sun>"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list (cons "DeadlineString" org-deadline-string)
               (cons "DeadlineTime" "TEST TIMESTAMP 2004"))))
       document)
      (string-match-p def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (plan-string (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-timestamp-function
           (lambda (timestamp)
             (format-time-string
              "TEST TIMESTAMP %Y"
              (org-timestamp-to-time timestamp)))
            (context-test-with-temp-customization-value
             org-export-with-planning
             t
             (context-test-with-temp-customization-value
              org-context-planning-command
              (cons name def)
              (context-test-with-temp-text
               "*** TODO write article about the Earth for the Guide
SCHEDULED: <2004-02-29 Sun>"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list (cons "ScheduledString" org-scheduled-string)
               (cons "ScheduledTime" "TEST TIMESTAMP 2004"))))
       document)
      (string-match-p def document))))
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (plan-string (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-timestamp-function
           (lambda (timestamp)
             (format-time-string
              "TEST TIMESTAMP %Y"
              (org-timestamp-to-time timestamp)))
            (context-test-with-temp-customization-value
             org-export-with-planning
             t
             (context-test-with-temp-customization-value
              org-context-planning-command
              (cons name def)
              (context-test-with-temp-text
               "*** TODO write article about the Earth for the Guide
CLOSED: <2004-02-29 Sun>"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list (cons "ClosedString" org-closed-string)
               (cons "ClosedTime" "TEST TIMESTAMP 2004"))))
       document)
      (string-match-p def document))))
  (should
   ;; NOTE This could fail if the function changes the order
   ;; the arguments are provided, but all arguments are present.
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (plan-string (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-format-timestamp-function
           (lambda (timestamp)
             (format-time-string
              "TEST TIMESTAMP %Y"
              (org-timestamp-to-time timestamp)))
            (context-test-with-temp-customization-value
             org-export-with-planning
             t
             (context-test-with-temp-customization-value
              org-context-planning-command
              (cons name def)
              (context-test-with-temp-text
               "*** TODO write article about the Earth for the Guide
SCHEDULED: <2002-02-29 Sun> CLOSED: <2003-02-29 Sun> DEADLINE: <2004-02-29 Sun>"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list (cons "ClosedString" org-closed-string)
               (cons "ClosedTime" "TEST TIMESTAMP 2003")
               (cons "DeadlineString" org-deadline-string)
               (cons "DeadlineTime" "TEST TIMESTAMP 2004")
               (cons "ScheduledString" org-scheduled-string)
               (cons "ScheduledTime" "TEST TIMESTAMP 2002")

               )
         nil t)
        )
       document)
      (string-match-p def document))))
  ;; Clock
  (should
   (let* ((name (format "%i" (random)))
          (def (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-export-with-clocks
            t
            (context-test-with-temp-customization-value
             org-context-format-clock-function
             (lambda (timestamp info)
               (let ((formatter
                      (car (plist-get info :context-clock-command)))
                     )
                 (format "\\%s[%s]"
                         formatter
                         (format-time-string
                          "%Y"
                          (org-timestamp-to-time timestamp)))))
             (context-test-with-temp-customization-value
              org-context-clock-command
              (cons name def)
              (context-test-with-temp-text
               "CLOCK: [2021-01-15 Fri 16:58   ]"
               (org-trim
                (org-export-as 'context nil nil nil
                               '(:context-preset "empty")))))))))
     (and
      (string-match-p
       (concat
        (regexp-quote (format "\\%s" name))
        "[^[]*"
        (regexp-quote "[")
        "[^]]*"
        (regexp-quote "2021")
        "[^]]*"
        (regexp-quote "]"))
       document)
      (string-match-p def document))))
  )


;;; Markup Functions
(ert-deftest test-org-context-markup ()
  "Test inline formatting"
  ;; Bold
  (should
   (equal
    "\\testbold{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'bold org-context-text-markup-alist) "\\testbold{%s}")
     (context-test-with-temp-text
      "*foo*"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Code
  (should
   (equal
    "\\testcode{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'code org-context-text-markup-alist) "\\testcode{%s}")
     (context-test-with-temp-text
      "~bar~"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Italic
  (should
   (equal
    "\\testitalic{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'italic org-context-text-markup-alist) "\\testitalic{%s}")
     (context-test-with-temp-text
      "/foo/"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Paragraph
  (should
   (equal
    "\\testpara{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'paragraph org-context-text-markup-alist) "\\testpara{%s}")
     (context-test-with-temp-text
      "foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Strikethrough
  (should
   (equal
    "\\teststrike{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'strike-through org-context-text-markup-alist) "\\teststrike{%s}")
     (context-test-with-temp-text
      "+bar+"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Subscript
  (should
   (equal
    "foo\\testsubscript{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'subscript org-context-text-markup-alist) "\\testsubscript{%s}")
     (context-test-with-temp-text
      "foo_bar"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Superscript
  (should
   (equal
    "foo\\testsuperscript{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'superscript org-context-text-markup-alist) "\\testsuperscript{%s}")
     (context-test-with-temp-text
      "foo^bar"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Underline
  (should
   (equal
    "\\testunderline{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'underline org-context-text-markup-alist) "\\testunderline{%s}")
     (context-test-with-temp-text
      "_foo_"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Verbatim
  (should
   (equal
    "\\testverbatim{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'verbatim org-context-text-markup-alist) "\\testverbatim{%s}")
     (context-test-with-temp-text
      "=bar="
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; TODO Verb

  )

;;; Items
(ert-deftest test-org-context-items ()
  "Test inline formatting"
  ;; Bullet off
  (should
   (let* ((rand (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-bullet-off-command
            (cons "TestItemOff" rand)
            (context-test-with-temp-text
             "- [ ] Item 1"
             (org-trim
              (org-export-as 'context nil nil nil
                             '(:context-preset "empty")))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\startitemize")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\sym")
        "[^{]*"
        (regexp-quote "{")
        "[^}]*"
        (regexp-quote "\\TestItemOff")
        "[^}]*"
        (regexp-quote "}"))
       document)
      (string-match-p
       rand document))))
  ;; Bullet on
  (should
   (let* ((rand (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-bullet-on-command
            (cons "TestItemOn" rand)
            (context-test-with-temp-text
             "- [X] Item 1"
             (org-trim
              (org-export-as 'context nil nil nil
                             '(:context-preset "empty")))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\startitemize")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\sym")
        "[^{]*"
        (regexp-quote "{")
        "[^}]*"
        (regexp-quote "\\TestItemOn")
        "[^}]*"
        (regexp-quote "}"))
       document)
      (string-match-p
       rand document))))
  ;; Bullet trans
  (should
   (let* ((rand (format "%i" (random)))
          (document
           (context-test-with-temp-customization-value
            org-context-bullet-trans-command
            (cons "TestItemTrans" rand)
            (context-test-with-temp-text
             "- [-] Item 1"
             (org-trim
              (org-export-as 'context nil nil nil
                             '(:context-preset "empty")))))))
     (and
      (string-match-p
       (concat
        (regexp-quote "\\startitemize")
        "\\(.\\|\n\\)*"
        (regexp-quote "\\sym")
        "[^{]*"
        (regexp-quote "{")
        "[^}]*"
        (regexp-quote "\\TestItemTrans")
        "[^}]*"
        (regexp-quote "}"))
       document)
      (string-match-p
       rand document))))
  )

;;; Empty environment
(ert-deftest test-org-context-export-empty ()
  "Test exporting nothing. No content with empty preset puts nothing in body"
  (should
   (equal
    ""
    (context-test-with-temp-text ""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Headlines
(ert-deftest test-org-context-export-headline ()
  "Test `org-context-headline'"
  (let ((inside-headline-regex
         "title={\\\\TestOrgHeadline[\s\n]*\\[Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}")
        (headline-command
         '("TestOrgHeadline" . "\\def\\TestOrgHeadline#1[#2]{%
  \\getparameters
    [TestOrgHeadline]
    [Todo=,
     TodoType=,
     Priority=,
     Text=,
     Tags=,
     #2]
  \\doifnot{\\TestOrgHeadlineTodo}{}{{\\sansbold{\\smallcaps{\\TestOrgHeadlineTodo}}\\space}}%
  \\doifnot{\\TestOrgHeadlinePriority}{}{{\\inframed{\\TestOrgHeadlinePriority}\\space}}%
  \\TestOrgHeadlineText%
  \\doifnot{\\TestOrgHeadlineTags}{}{{\\hfill\\tt\\TestOrgHeadlineTags}}%
}"))
        (headline-name "Headline 1"))

    ;; Trivial case
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format inside-headline-regex headline-name headline-name headline-name headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text
        "* Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Not-numbered
    (should
     (string-match-p
      (format "\\\\startsubject\\[%s\\][\s\n]*\\\\stopsubject"
              (format inside-headline-regex headline-name headline-name headline-name headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text
        "* Headline 1\n:PROPERTIES:\n:UNNUMBERED:\n:END:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Escape characters in headlines
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (let ((replacement
                     (regexp-quote
                      "Headline `\\lettertilde !@\\#\\$\\%^&*()_-=+\\[\\{\\}\\]\\|\\letterbackslash :;\"'<,>.?/")))
                (format
                 inside-headline-regex
                 replacement
                 replacement
                 replacement
                 replacement)))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* Headline `~!@#$%^&*()_-=+[{}]|\\:;\"'<,>.?/"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Nested headlines should wrap each other
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\startsubsection\\[%s\\][\s\n]*\\\\stopsubsection[\s\n]*\\\\stopsection"
              (format inside-headline-regex "Headline 1" "Headline 1" "Headline 1" "Headline 1")
              (format inside-headline-regex "Headline 2" "Headline 2" "Headline 2" "Headline 2"))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* Headline 1\n** Headline 2"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Alt text for title
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format inside-headline-regex headline-name "alt" "alt" "alt"))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* Headline 1\n:PROPERTIES:\n:ALT_TITLE: alt\n:END:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; todo keywords
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\TestOrgHeadline[\s\n]*\\[Todo={TODO},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* TODO Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Tags
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\TestOrgHeadline[\s\n]*\\[Text={%s},[\s\n]*Tags={tag1:tag2}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* Headline 1 :tag1:tag2:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
    ;; Priority
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\TestOrgHeadline[\s\n]*\\[Priority={A},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       headline-command
       (context-test-with-temp-text "* [#A] Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty" :with-priority t)))))))
    ))

;;; Keywords
(ert-deftest test-org-context-export-keywords ()
  "Test keywords."
  (should
   (equal
    "abc"
    (context-test-with-temp-text "#+CONTEXT: abc"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\index{foo}"
    (context-test-with-temp-text "#+INDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgConcept{foo}"
    (context-test-with-temp-text "#+CINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgFunction{foo}"
    (context-test-with-temp-text "#+FINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgKeystroke{foo}"
    (context-test-with-temp-text "#+KINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgProgram{foo}"
    (context-test-with-temp-text "#+PINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgDataType{foo}"
    (context-test-with-temp-text "#+TINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgVariable{foo}"
    (context-test-with-temp-text "#+VINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelistoftables[criterium=all]"
    (context-test-with-temp-text "#+TOC: tables"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelistoffigures[criterium=all]"
    (context-test-with-temp-text "#+TOC: figures"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[formula][criterium=all]"
    (context-test-with-temp-text "#+TOC: equations"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeindex"
    (context-test-with-temp-text "#+TOC: definitions"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgConcept]"
    (context-test-with-temp-text "#+TOC: cp"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgFunction]"
    (context-test-with-temp-text "#+TOC: fn"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgKeystroke]"
    (context-test-with-temp-text "#+TOC: ky"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgProgram]"
    (context-test-with-temp-text "#+TOC: pg"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgDataType]"
    (context-test-with-temp-text "#+TOC: tp"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgVariable]"
    (context-test-with-temp-text "#+TOC: vr"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[]"
    (context-test-with-temp-text "#+TOC: headlines"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[list={section,subject,subsection,subsubject}]"
    (context-test-with-temp-text "#+TOC: headlines 2"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[criterium=local,]"
    (context-test-with-temp-text "#+TOC: headlines local"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgListingEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: listings"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgVerseEnumerateEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: verses"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgBlockQuoteEnumEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: quotes"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgExampleEnumerationEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: examples"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; TODO test bibliography
  )

;;; Document Keywords

(ert-deftest test-org-context-export-doc-keywords ()
  "Test exporting document-level keywords."
  ;; ATTENTION
  (should
   (string-match-p
    (regexp-quote "letter:attention={bob}")
    (context-test-with-temp-text "#+ATTENTION: bob"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; CLOSING
  (should
   (string-match-p
    (regexp-quote "letter:closing={Sincerely, Bob}")
    (context-test-with-temp-text "#+CLOSING: Sincerely, Bob"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:closing={Sincerely, Bob}")
    (context-test-with-temp-customization-value
     org-context-closing
     "Sincerely, Bob"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; CONTEXT_HEADER
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text "#+CONTEXT_HEADER: foo bar baz"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; CONTEXT_HEADER_EXTRA
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text "#+CONTEXT_HEADER_EXTRA: foo bar baz"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; DATE
  (should
   (string-match-p
    (regexp-quote "metadata:date={foo}")
    (context-test-with-temp-text "#+DATE: foo"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; DESCRIPTION
  (should
   (string-match-p
    (regexp-quote "metadata:description={foo}")
    (context-test-with-temp-text "#+DESCRIPTION: foo"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; FROM_ADDRESS
  (should
   (string-match-p
    (regexp-quote "letter:fromaddress={foo}")
    (context-test-with-temp-text "#+FROM_ADDRESS: foo"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:fromaddress={foo}")
    (context-test-with-temp-customization-value
     org-context-from-address
     "foo"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; KEYWORDS
  (should
   (string-match-p
    (regexp-quote "metadata:keywords={foo bar baz}")
    (context-test-with-temp-text "#+KEYWORDS: foo bar baz"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; LANGUAGE
  (should
   (let ((content
          (context-test-with-temp-text "#+LANGUAGE: grobnatch"
            (org-export-as 'context nil nil nil '(:context-preset "empty")))))
     (and
      (string-match-p
       (regexp-quote "metadata:language={grobnatch}")
       content)
      (string-match-p
       (regexp-quote "\\language[grobnatch]")
       content))))
  ;; LOCATION
  (should
   (string-match-p
    (regexp-quote "letter:location={grokistan}")
    (context-test-with-temp-text "#+LOCATION: grokistan"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:location={grokistan}")
    (context-test-with-temp-customization-value
     org-context-location
     "grokistan"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; OPENING
  (should
   (string-match-p
    (regexp-quote "letter:opening={biz buz}")
    (context-test-with-temp-text "#+OPENING: biz buz"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:opening={biz buz}")
    (context-test-with-temp-customization-value
     org-context-opening
     "biz buz"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; PHONE_NUMBER
  (should
   (string-match-p
    (regexp-quote "metadata:phonenumber={314 159 2653}")
    (context-test-with-temp-text "#+PHONE_NUMBER: 314 159 2653"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "metadata:phonenumber={314 159 2653}")
    (context-test-with-temp-customization-value
     org-context-phone-number
     "314 159 2653"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; PLACE
  (should
   (string-match-p
    (regexp-quote "letter:place={Arlen Texas}")
    (context-test-with-temp-text "#+PLACE: Arlen Texas"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:place={Arlen Texas}")
    (context-test-with-temp-customization-value
     org-context-place
     "Arlen Texas"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; SIGNATURE
  (should
   (string-match-p
    (regexp-quote "letter:signature={buz buz}")
    (context-test-with-temp-text "#+SIGNATURE: buz buz"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "letter:signature={buz buz}")
    (context-test-with-temp-customization-value
     org-context-signature
     "buz buz"
     (context-test-with-temp-text ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  ;; SUBJECT
  (should
   (string-match-p
    (regexp-quote "metadata:subject={vorpal swords}")
    (context-test-with-temp-text "#+SUBJECT: vorpal swords"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; SUBTITLE
  (should
   (string-match-p
    (regexp-quote "metadata:subtitle={frumorious bandersnatches}")
    (context-test-with-temp-text "#+SUBTITLE: frumorious bandersnatches"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; TO_ADDRESS
  (should
   (string-match-p
    (regexp-quote "letter:toaddress={the palace}")
    (context-test-with-temp-text "#+TO_ADDRESS: the palace"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; TO_NAME
  (should
   (string-match-p
    (regexp-quote "letter:toname={bob}")
    (context-test-with-temp-text "#+TO_NAME: bob"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  ;; URL
  (should
   (string-match-p
    (regexp-quote "metadata:url={beware the jabberwock}")
    (context-test-with-temp-text "#+URL: beware the jabberwock"
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "metadata:url={beware the jabberwock}")
    (context-test-with-temp-customization-value
     org-context-url
     "beware the jabberwock"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
  )

;;; Images
(ert-deftest test-org-context-export-images ()
  "Test exporting links."
  ;; Simple case
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={TestWidth}]
\\stopplacefigure"
    (context-test-with-temp-customization-value
     org-context-image-default-height
     ""
     (context-test-with-temp-customization-value
      org-context-image-default-width
      "TestWidth"
      (context-test-with-temp-text
       "[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))
      ))))
  ;; With Caption
  (should
   (equal
    "\\startplacefigure[title={A cat},
   location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text "#+CAPTION: A cat\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; wrap
  (should
   (equal
    "\\startplacefigure[location={here,left}]
\\externalfigure[./images/cat.jpg][width={0.48\\hsize}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :float wrap\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; sideways
  (should
   (equal
    "\\startplacefigure[location={page,90}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :float sideways\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; multicolumn
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr\\makeupwidth - 1em\\relax}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :float multicolumn\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Placement
  (should
   (equal
    "\\startplacefigure[location={backspace}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :placement backspace\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; width
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={2in}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :width 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; height
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][height={2in}]
\\stopplacefigure"
    (context-test-with-temp-text "#+ATTR_CONTEXT: :height 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][height={TestHeight},
   width={TestWidth}]
\\stopplacefigure"
    (context-test-with-temp-customization-value
     org-context-image-default-width
     "TestWidth"
     (context-test-with-temp-customization-value
      org-context-image-default-height "TestHeight"
      (context-test-with-temp-text
       "[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][height={TestHeight}]
\\stopplacefigure"
    (context-test-with-temp-customization-value
     org-context-image-default-width
     ""
     (context-test-with-temp-customization-value
      org-context-image-default-height "TestHeight"
      (context-test-with-temp-text
       "[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
  )

;;; Links
(ert-deftest test-org-context-export-links ()
  "Test exporting links."
  ;; plain link
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (context-test-with-temp-text "[[http://orgmode.org]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; plain link
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (context-test-with-temp-text "http://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Link with description
  (should
   (equal
    "\\goto{Some Description}[url(http://orgmode.org)]"
    (context-test-with-temp-text "[[http://orgmode.org][Some Description]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; https
  (should
   (equal
    "\\goto{\\hyphenatedurl{https://orgmode.org}}[url(https://orgmode.org)]"
    (context-test-with-temp-text "https://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; doi
  (should
   (equal
    "\\goto{\\hyphenatedurl{doi:10.1000/182}}[url(doi:10.1000/182)]"
    (context-test-with-temp-text "doi:10.1000/182"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; ftp
  (should
   (equal
    "\\goto{\\hyphenatedurl{ftp:orgmode.org}}[url(ftp:orgmode.org)]"
    (context-test-with-temp-text "ftp:orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; other
  (should
   (equal
    "\\goto{\\hyphenatedurl{projects.org}}[url(projects.org)]"
    (context-test-with-temp-text "attachment:projects.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

;;; Labels
(ert-deftest test-org-context-export-labels ()
  "When things should have labels or not."
  ;; No label on center
  (should
   (string-match-p
    (regexp-quote "\\startalignment")
    (context-test-with-temp-text "#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Center with name
  (should
   (string-match-p
    (concat
     (regexp-quote "\\reference[org")
     "[0-9a-f]+"
     (regexp-quote "]{foo}")
     "\\(.\\|\n\\)*"
     (regexp-quote
     "\\startalignment"))
    (context-test-with-temp-text "#+NAME: foo\n#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

;;; Tables
(ert-deftest test-org-context-export-table ()
  "Table configuration options"
  ;; empty table structure
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]+"
     (regexp-quote "]")
     "[\s\n]*"
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]+"
     (regexp-quote "]")
     "[\s\n]*"
     "[\s\n]*"
     (regexp-quote "\\startxtablebody")
     "[\s\n]*"
     (regexp-quote "[OrgTableBody]")
     "[\s\n]*"
     (regexp-quote "\\startxrow")
     "[\s\n]*"
     (regexp-quote "[OrgTableTopRow]")
     "[\s\n]*"
     (regexp-quote "\\startxcell")
     "[\s\n]*"
     (regexp-quote "[OrgTableTopLeftCell]")
     ".+"
     "[\s\n]*"
     (regexp-quote "\\stopxcell")
     "[\s\n]*"
     (regexp-quote "\\stopxrow")
     "[\s\n]*"
     (regexp-quote "\\stopxtablebody")
     "[\s\n]*"
     (regexp-quote "\\stopxtable")
     "[\s\n]*"
     (regexp-quote "\\stopplacetable"))
    (context-test-with-temp-text "| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table location disabled in customization
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startplacetable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "location={"))
     (context-test-with-temp-customization-value
      org-context-table-split
      ""
      (context-test-with-temp-customization-value
       org-context-table-location
       ""
       (context-test-with-temp-text
        " | foo |"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
  ;; Table location enabled in customization
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={bar}"))
    (context-test-with-temp-customization-value
     org-context-table-split
     ""
     (context-test-with-temp-customization-value
      org-context-table-location
      "bar"
      (context-test-with-temp-text
       " | foo |"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
  ;; Table location in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={")
    "[^}]*"
    "there"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+TABLE_LOCATION: there\n
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table location in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={")
    "[^}]*"
    "there"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+ATTR_CONTEXT: :location there
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table location in both
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={")
    "[^}]*"
    "there"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+TABLE_LOCATION: here
#+ATTR_CONTEXT: :location there
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table header repeat with customization
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "repeat"
     "[^}]*"
     (regexp-quote "}")
     ",?"
     "[^]]*"
     (regexp-quote "]")
     "[\s\n]*"
     (regexp-quote "\\startxtablehead"))
    (context-test-with-temp-customization-value
     org-context-table-head
     "repeat"
     (context-test-with-temp-text
      "| foo |
|-----|
| bar |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; No header in customization
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startxtable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "header={"))
     (context-test-with-temp-customization-value
      org-context-table-head
      ""
      (context-test-with-temp-text
       "| foo |
|-----|
| bar |"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
  ;; Turn off header repeat in document keywords
  (should
   (not (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "repeat")
    (context-test-with-temp-text "#+TABLE_HEAD: nil
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Turn off header repeat in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "repeat")
    (context-test-with-temp-text "#+ATTR_CONTEXT: :header repeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Override header document setting in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "repeat")
    (context-test-with-temp-text "#+TABLE_HEAD: nil
#+ATTR_CONTEXT: :header repeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Override header document setting in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "norepeat")
    (context-test-with-temp-text
     "#+TABLE_HEAD: repeat
#+ATTR_CONTEXT: :header norepeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table footer customization enabled
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "footer={")
     "[^}]*"
     "repeat")
    (context-test-with-temp-customization-value
     org-context-table-foot
     "repeat"
     (context-test-with-temp-text
      "| foo |
|-----|
| bar |
|-----|
| baz |"
      (org-trim
       (org-export-as 'context nil nil t
                      '(:context-preset "empty")))))))
  ;; Table footer customization disabled
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startxtable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "footer={")
      "[^}]*"
      "repeat")
     (context-test-with-temp-customization-value
      org-context-table-foot
      ""
      (context-test-with-temp-text
       "| foo |
|-----|
| bar |
|-----|
| baz |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty"))))))))
  ;; Table footer not present by default
  (should
   (not
    (string-match-p
     (regexp-quote "\\startxtablefoot")
     (context-test-with-temp-text "| foo |
|-----|
| bar |
|-----|
| baz |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty")))))))
  ;; Turn on footer repeat in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "footer={")
     "[^}]*"
     "repeat"
     "\\(.\\|\n\\)*"
     (regexp-quote "\\startxtablefoot"))
    (context-test-with-temp-text "#+TABLE_FOOT: repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Turn on footer repeat in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "footer={")
     "[^}]*"
     "repeat"
     "\\(.\\|\n\\)*"
     (regexp-quote "\\startxtablefoot"))
    (context-test-with-temp-text "#+ATTR_CONTEXT: :footer repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Override footer document setting in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "footer={")
     "[^}]*"
     "repeat"
     "\\(.\\|\n\\)*"
     (regexp-quote "\\startxtablefoot"))
    (context-test-with-temp-text "#+TABLE_FOOT: nil
#+ATTR_CONTEXT: :footer repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Override document setting in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "footer={")
     "[^}]*"
     "norepeat"
     "\\(.\\|\n\\)*"
     (regexp-quote "\\startxtablefoot"))
    (context-test-with-temp-text "#+TABLE_FOOT: repeat
#+ATTR_CONTEXT: :footer norepeat
| foo |
|-----|
| bar |
|-----|
| baz |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table option in customization
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "option={")
    "[^}]*"
    "bar"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-customization-value
     org-context-table-option
     "bar"
     (context-test-with-temp-text
     "| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Table option not configured
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startxtable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "option={"))
     (context-test-with-temp-customization-value
      org-context-table-option
      ""
      (context-test-with-temp-text
       "| foo |"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
  ;; Table option in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "option={")
    "[^}]*"
    "bar"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+TABLE_OPTION: bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table option in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "option={")
    "[^}]*"
    "bar"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+ATTR_CONTEXT: :option bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table option in both
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "option={")
    "[^}]*"
    "baz"
    "[^}]*"
    (regexp-quote "}")
    ",?")
    (context-test-with-temp-text "#+TABLE_OPTION: bar
#+ATTR_CONTEXT: :option baz
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-text "#+TABLE_STYLE: bar

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-text "#+ATTR_CONTEXT: :table-style bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in both
  (should
   (string-match-p
   (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[baz]"))
    (context-test-with-temp-text "#+TABLE_STYLE: bar
#+ATTR_CONTEXT: :table-style baz
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in customization
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "bar"
     (context-test-with-temp-text "| foo |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty")))))))
  ;; Table float style in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-text "#+TABLE_FLOAT: bar

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table float style in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-text "#+ATTR_CONTEXT: :float-style bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table float style with customization
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-customization-value
     org-context-table-float-style
     "bar"
     (context-test-with-temp-text "| foo |"
      (org-trim
       (org-export-as 'context nil nil t
                      '(:context-preset "empty")))))))
  ;; Table float style in both
  (should
   (string-match-p
   (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[baz]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-text "#+TABLE_FLOAT: bar
#+ATTR_CONTEXT: :float-style baz
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table split when org-context-table-split
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={")
     "[^]]*"
     (regexp-quote "split")
     "[^]]*"
     (regexp-quote "}")
     "[^]]*"
     (regexp-quote "]")
     "[\s\n]*"
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "split={yes}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-customization-value
     org-context-table-split
     "yes"
     (context-test-with-temp-text "| foo |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty")))))))
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startplacetable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "location={")
      "[^]]*"
      (regexp-quote "split"))
     (context-test-with-temp-customization-value
      org-context-table-split
      "no"
      (context-test-with-temp-text "| foo |"
        (org-trim
         (org-export-as 'context nil nil t
                        '(:context-preset "empty"))))))))
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startxtable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "split={yes}"))
     (context-test-with-temp-customization-value
      org-context-table-split
      "no"
      (context-test-with-temp-text "| foo |"
        (org-trim
         (org-export-as 'context nil nil t
                        '(:context-preset "empty"))))))))
  ;; Disable Table split in document keywords
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startplacetable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "location={")
      "[^]]*"
      (regexp-quote "split"))
     (context-test-with-temp-text "#+TABLE_SPLIT: no

| foo |"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Custom split keywords are passed to startxtable
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "split={no}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text "#+TABLE_SPlIT: no

| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Disable table split in ATTR_CONTEXT (float)
  (should
   (not
    (string-match-p
     (concat
      (regexp-quote "\\startplacetable")
      "[\s\n]*"
      (regexp-quote "[")
      "[^]]*"
      (regexp-quote "location={")
      "[^]]*"
      (regexp-quote "split"))
     (context-test-with-temp-text "#+ATTR_CONTEXT: :split no
| foo |"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  ;; Disable table split in ATTR_CONTEXT (table)
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "split={no}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text "#+ATTR_CONTEXT: :split no
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table split in both
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={")
     "[^]]*"
     (regexp-quote "split")
     "[^]]*"
     (regexp-quote "}")
     "[^]]*"
     (regexp-quote "]")
     "[\s\n]*"
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "split={yes}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text "#+TABLE_SPLIT: no
#+ATTR_CONTEXT: :split yes
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

;;; Document Structure
(ert-deftest test-org-context-document-structure ()
  "Test that document structure matches what we expect generally."
  (should
   (let ((document
          (context-test-with-temp-customization-value
           org-context-texinfo-indices-alist
           '(("foo" . (:keyword "FOOINDEX" :command "TestFooIndex")))
           (context-test-with-temp-customization-value
            org-context-inner-templates-alist
            '(("TestFrame" . "
BACKMATTER
%b
INDEX
%i
CONTENT
%c
COPYING
%o
APPENDIX
%a
FRONTMATTER
%f
"))
            (context-test-with-temp-customization-value
             org-context-preset
             "TestPreset"
             (context-test-with-temp-customization-value
              org-context-presets-alist
              '(("TestPreset" .
                 (:literal "Test Literal"
                  :template "TestFrame"
                  :snippets ("TestSnippet"))))
              (context-test-with-temp-customization-value
               org-context-snippets-alist
               '(("TestSnippet" . "A Test Snippet"))
               (context-test-with-temp-text
                "
* Backmatter Headline
:PROPERTIES:
:BACKMATTER:
:END:
Some back matter

* Appendix Headline 1
:PROPERTIES:
:APPENDIX:
:END:
Appendix

* Normal Headline 1
Normal section

* Copying Headline 1
:PROPERTIES:
:COPYING:
:END:
Some copying section

* Index Headline 1
:PROPERTIES:
:INDEX: foo
:END:
Some Index

* Frontmatter Headline 1
:PROPERTIES:
:FRONTMATTER:
:END:
Some Frontmatter

* Copying Headline 2
:PROPERTIES:
:COPYING:
:END:
Some more copying section

* Appendix Headline 2
:PROPERTIES:
:APPENDIX:
:END:
More appendix

* Frontmatter Headline 2
:PROPERTIES:
:FRONTMATTER:
:END:
Some more frontmatter

* Index Headline 2
:PROPERTIES:
:INDEX: foo
:END:
Some More Index
"
                (org-trim
                 (org-export-as 'context nil nil nil))))))))))
     (and
      (string-match-p
       (concat
        "BACKMATTER"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Backmatter Headline")))))
                (cons "list" "Backmatter Headline")
                (cons "marking" "Backmatter Headline")
                (cons "bookmark" "Backmatter Headline")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Some back matter")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        "INDEX"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Index Headline 1")))))
                (cons "list" "Index Headline 1")
                (cons "marking" "Index Headline 1")
                (cons "bookmark" "Index Headline 1")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "\\placeregister[TestFooIndex]")
        "[[:space:]]*"
        (regexp-quote "Some Index")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Index Headline 2")))))
                (cons "list" "Index Headline 2")
                (cons "marking" "Index Headline 2")
                (cons "bookmark" "Index Headline 2")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "\\placeregister[TestFooIndex]")
        "[[:space:]]*"
        (regexp-quote "Some More Index")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        "CONTENT"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Normal Headline 1")))))
                (cons "list" "Normal Headline 1")
                (cons "marking" "Normal Headline 1")
                (cons "bookmark" "Normal Headline 1")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Normal section")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        "COPYING"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Copying Headline 1")))))
                (cons "list" "Copying Headline 1")
                (cons "marking" "Copying Headline 1")
                (cons "bookmark" "Copying Headline 1")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Some copying section")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Copying Headline 2")))))
                (cons "list" "Copying Headline 2")
                (cons "marking" "Copying Headline 2")
                (cons "bookmark" "Copying Headline 2")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Some more copying section")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        "APPENDIX"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Appendix Headline 1")))))
                (cons "list" "Appendix Headline 1")
                (cons "marking" "Appendix Headline 1")
                (cons "bookmark" "Appendix Headline 1")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Appendix")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Appendix Headline 2")))))
                (cons "list" "Appendix Headline 2")
                (cons "marking" "Appendix Headline 2")
                (cons "bookmark" "Appendix Headline 2")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "More appendix")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        "FRONTMATTER"
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Frontmatter Headline 1")))))
                (cons "list" "Frontmatter Headline 1")
                (cons "marking" "Frontmatter Headline 1")
                (cons "bookmark" "Frontmatter Headline 1")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Some Frontmatter")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*"
        (regexp-quote "\\startsection")
        "[[:space:]]*"
        (context-test-build-ConTeXt-argument-regex
         (list  (cons "title"
                      (concat
                       (regexp-quote "\\OrgHeadline")
                       "[[:space:]]*"
                       (context-test-build-ConTeXt-argument-regex
                        '(("Text" . "Frontmatter Headline 2")))))
                (cons "list" "Frontmatter Headline 2")
                (cons "marking" "Frontmatter Headline 2")
                (cons "bookmark" "Frontmatter Headline 2")
                (cons "reference" "sec:org[0-9a-f]+"))
         nil t)
        "[[:space:]]*"
        (regexp-quote "Some more Frontmatter")
        "[[:space:]]*"
        (regexp-quote "\\stopsection")
        "[[:space:]]*")
       document)
      (string-match-p "A Test Snippet" document)))))

(provide 'test-ox-context)
;;; test-ox-context ends here
