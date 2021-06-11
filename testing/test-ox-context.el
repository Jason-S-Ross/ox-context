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

;; TODO This shouldn't rely on the existing empty environment


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

(defun context-test-build-ConTeXt-argument-regex
    (arguments &optional nobrackets nopermute nonexhaustive)
  "Construct a regexp to match combinations of ARGUMENTS.
ARGUMENTS is an alist of key, value pairs to pass to the command.
NOBRACKETS disables surrounding arguments in {} brackets.
NONEXHAUSTIVE allows matching if not all arguments are present.
NOPERMUTE disables generating all possible permutations (necesary
if hitting the regexp max length limit)"
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
      (when nonexhaustive "[^]]*")
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
(ert-deftest test-org-context/protect-names ()
  "Protected names in text"
  (should
   (equal
    "testing names of \\TeX{}, \\LaTeX{}, and \\ConTeXt{}. tex, latex, and context shouldn't match."
    (context-test-with-temp-text
     "testing names of TeX, LaTeX, and ConTeXt. tex, latex, and context shouldn't match."
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/protect-special-para ()
  "Protect special characters in text"
  (should
   (equal
    "`\\lettertilde !@\\#\\$\\%^&*()-_=+\\[\\{\\}\\]\\letterbackslash \\|;:'\",<.>/?"
    (context-test-with-temp-text
     "`~!@#$%^&*()-_=+[{}]\\|;:'\",<.>/?"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Smart Quotation
(ert-deftest test-org-context/smart-quote-basic ()
  "Test simple smart quotes."
  (should
   (equal
    "Quote: \\quotation{Cogito ergo sum} - Descartes"
    (context-test-with-temp-customization-value
     org-context-export-quotes-alist
     '((primary-opening . "\\quotation{")
       (primary-closing . "}")
       (secondary-opening . "\\quote{")
       (secondary-closing . "}")
       (apostrophe . "'"))
     (context-test-with-temp-text
      "Quote: \"Cogito ergo sum\" - Descartes"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/smart-quote-apostrophe ()
  "Test apostrophes in text."
  (should
   (equal
    "Here's a quote: \\quotation{I think, therefore I am}"
    (context-test-with-temp-customization-value
     org-context-export-quotes-alist
     '((primary-opening . "\\quotation{")
       (primary-closing . "}")
       (secondary-opening . "\\quote{")
       (secondary-closing . "}")
       (apostrophe . "'"))
     (context-test-with-temp-text
      "Here's a quote: \"I think, therefore I am\""
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/smart-quote-nested ()
  "Test nested quotes."
  (should
   (equal
    "Here\\TestApostrophes a nested quote: \\TestPrimary{Descartes says \\TestSecondary{I think therefore I am}}"
    (context-test-with-temp-customization-value
     org-context-export-quotes-alist
     '((primary-opening . "\\TestPrimary{")
       (primary-closing . "}")
       (secondary-opening . "\\TestSecondary{")
       (secondary-closing . "}")
       (apostrophe . "\\TestApostrophe"))
     (context-test-with-temp-text
     "Here's a nested quote: \"Descartes says 'I think therefore I am'\""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))



;;; Environments
;;;; Block quotes
(ert-deftest test-org-context/block-quote-1 ()
  "Test block quotes."
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
    (should
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
      document))
    (should (string-match-p def document))
    (should (string-match-p enumdef document))))
(ert-deftest test-org-context/block-quote-2 ()
  "Test block quotes with captions."
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
    (should
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
      document))
    (should (string-match-p def document))
    (should (string-match-p enumdef document))))
;;;; Example
(ert-deftest test-org-context/example-1 ()
  "Test simple example."
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
    (should
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
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/example-2 ()
  "Test named example."
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Fixed
(ert-deftest test-org-context/fixed ()
  "Test fixed width."
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
    (should
     (string-match-p
      (concat
       (regexp-quote "\\start")
       (regexp-quote name)
       "\\(.\\|\n\\)*"
       (regexp-quote "foo bar baz")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stop")
       (regexp-quote name))
      document))
    (should (string-match-p def document))))
;;;; Inline source
(ert-deftest test-org-context/inline-src-1 ()
  "Test inline source with default formatter"
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
    (should
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
      document))
    (should
     (string-match-p def document))))
(ert-deftest test-org-context/inline-src-2 ()
  "Inline src with vim syntax"
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Property Drawers
(ert-deftest test-org-context/property-drawers ()
  "Test property drawers."
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
    (should
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
      document))
    (should (string-match-p def document))
    (should (string-match-p def2 document))))
;;;; Description Items
(ert-deftest test-org-context/descriptions ()
  "Test description items"
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
    (should
     (string-match-p
      (concat
       (regexp-quote "\\starttestitemdescription{foo}")
       "\\(.\\|\n\\)*"
       (regexp-quote "bar")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stoptestitemdescription"))
      document))

    (should (string-match-p rand document))))
;;;; Block source
(ert-deftest test-org-context/block-source-1 ()
  "Test block source."
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
    (should
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
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/block-source-2 ()
  "Test block source."
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
    (should
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
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/block-source-3 ()
  "Test block source."
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
    (should
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
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/block-source-4 ()
  "Test block source."
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\start%sPython" name))
       "\\(.\\|\n\\)*"
       (regexp-quote "print(\"Hello, world!\")")
       "\\(.\\|\n\\)*"
       (regexp-quote (format "\\stop%s" name)))
      document))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\definevimtyping")
       "[^[]*"
       (regexp-quote (format "[%sPython]" name)))
      document))))
(ert-deftest test-org-context/block-source-5 ()
  "Test block source"
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\start%sBarlang" name))
       "\\(.\\|\n\\)*"
       (regexp-quote "print(\"Hello, world!\")")
       "\\(.\\|\n\\)*"
       (regexp-quote (format "\\stop%s" name)))
      document))
    (should
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
;;;;; Verses
(ert-deftest test-org-context/verses-1 ()
  "Test verses"
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
    (should
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
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/verses-2 ()
  "Test verses."
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Drawers
(ert-deftest test-org-context/drawers ()
  "Test drawers."
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Inline tasks
(ert-deftest test-org-context/inline-task ()
  "Test inline tasks."
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Planning
(ert-deftest test-org-context/planning-1 ()
  "Test planning."
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\%s" name))
       "[[:space:]]*"
       (context-test-build-ConTeXt-argument-regex
        (list (cons "DeadlineString" org-deadline-string)
              (cons "DeadlineTime" "TEST TIMESTAMP 2004"))))
      document))

    (should (string-match-p def document))))
(ert-deftest test-org-context/planning-2 ()
  "Test planning."
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\%s" name))
       "[[:space:]]*"
       (context-test-build-ConTeXt-argument-regex
        (list (cons "ScheduledString" org-scheduled-string)
              (cons "ScheduledTime" "TEST TIMESTAMP 2004"))))
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/planning-3 ()
  "Test planning."
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\%s" name))
       "[[:space:]]*"
       (context-test-build-ConTeXt-argument-regex
        (list (cons "ClosedString" org-closed-string)
              (cons "ClosedTime" "TEST TIMESTAMP 2004"))))
      document))
    (should (string-match-p def document))))
(ert-deftest test-org-context/planning-4 ()
  "Test planning."
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
    (should
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
      document))
    (should (string-match-p def document))))
;;;; Clock
(ert-deftest test-org-context/clock ()
  "Test clocks."
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
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\%s" name))
       "[^[]*"
       (regexp-quote "[")
       "[^]]*"
       (regexp-quote "2021")
       "[^]]*"
       (regexp-quote "]"))
      document))
    (should (string-match-p def document))))

;;; Markup Functions
(ert-deftest test-org-context/bold ()
  "Test bold."
 (should
   (equal
    "\\testbold{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'bold org-context-text-markup-alist) "\\testbold{%s}")
     (context-test-with-temp-text
      "*foo*"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/code-markup ()
  "Test code markup."
  (should
   (equal
    "\\testcode{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'code org-context-text-markup-alist) "\\testcode{%s}")
     (context-test-with-temp-text
      "~bar~"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/italic ()
  "Test italic."
  (should
   (equal
    "\\testitalic{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'italic org-context-text-markup-alist) "\\testitalic{%s}")
     (context-test-with-temp-text
      "/foo/"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/paragraph ()
  "Test paragraph markup."
  (should
   (equal
    "\\testpara{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'paragraph org-context-text-markup-alist) "\\testpara{%s}")
     (context-test-with-temp-text
      "foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/strikethrough ()
  "Test strikethrough."
  (should
   (equal
    "\\teststrike{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'strike-through org-context-text-markup-alist) "\\teststrike{%s}")
     (context-test-with-temp-text
      "+bar+"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/subscript ()
  "Test subscript."
  (should
   (equal
    "foo\\testsubscript{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'subscript org-context-text-markup-alist) "\\testsubscript{%s}")
     (context-test-with-temp-text
      "foo_bar"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/superscript ()
  "Test superscript."
  (should
   (equal
    "foo\\testsuperscript{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'superscript org-context-text-markup-alist) "\\testsuperscript{%s}")
     (context-test-with-temp-text
      "foo^bar"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/underline ()
  "Test underline."
  (should
   (equal
    "\\testunderline{foo}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'underline org-context-text-markup-alist) "\\testunderline{%s}")
     (context-test-with-temp-text
      "_foo_"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/verbatim ()
  "Test verbatim"
  (should
   (equal
    "\\testverbatim{bar}"
    (context-test-with-temp-modification
     org-context-text-markup-alist
     (setf (alist-get 'verbatim org-context-text-markup-alist) "\\testverbatim{%s}")
     (context-test-with-temp-text
      "=bar="
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/verb ()
  "Test verb."
  (should nil))

;;; Items
(ert-deftest test-org-context/bullet-off ()
  "Test bullet-off command."
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
    (should
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
      document))
    (should (string-match-p rand document))))
(ert-deftest test-org-context/bullet-on ()
  "Test bullet on."
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
    (should
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
      document))
    (should (string-match-p rand document))))
(ert-deftest test-org-context/bullet-trans ()
  "Test bullet trans command."
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
    (should
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
      document))
    (should (string-match-p rand document))))

;;; Empty environment
(ert-deftest test-org-context-export-empty ()
  "Test exporting nothing. No content with empty preset puts nothing in body"
  (should
   (equal
    ""
    (context-test-with-temp-text ""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Headlines
(setq test-org-context-inside-headline-regex
      "title={\\\\TestOrgHeadline[\s\n]*\\[Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
      test-org-context-headline-command '("TestOrgHeadline" . "\\def\\TestOrgHeadline#1[#2]{%
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
}")
      test-org-context-headline-name "Headline 1")
(ert-deftest test-org-context/headline-trivial ()
  "Trivial headline match."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
            (format test-org-context-inside-headline-regex test-org-context-headline-name test-org-context-headline-name test-org-context-headline-name test-org-context-headline-name))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text
      "* Headline 1"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-notnumbered ()
  "Non-numbered headlines."
  (should
   (string-match-p
    (format "\\\\startsubject\\[%s\\][\s\n]*\\\\stopsubject"
            (format test-org-context-inside-headline-regex test-org-context-headline-name test-org-context-headline-name test-org-context-headline-name test-org-context-headline-name))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text
      "* Headline 1\n:PROPERTIES:\n:UNNUMBERED:\n:END:"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-escape-chars ()
  "Headline with special chars in it."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
            (let ((replacement
                   (regexp-quote
                    "Headline `\\lettertilde !@\\#\\$\\%^&*()_-=+\\[\\{\\}\\]\\|\\letterbackslash :;\"'<,>.?/")))
              (format
               test-org-context-inside-headline-regex
               replacement
               replacement
               replacement
               replacement)))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text "* Headline `~!@#$%^&*()_-=+[{}]|\\:;\"'<,>.?/"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-nested ()
  "Nested headlines should wrap each other."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\startsubsection\\[%s\\][\s\n]*\\\\stopsubsection[\s\n]*\\\\stopsection"
            (format test-org-context-inside-headline-regex "Headline 1" "Headline 1" "Headline 1" "Headline 1")
            (format test-org-context-inside-headline-regex "Headline 2" "Headline 2" "Headline 2" "Headline 2"))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text "* Headline 1\n** Headline 2"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-alt-text ()
  "Alt text should show up."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
            (format test-org-context-inside-headline-regex test-org-context-headline-name "alt" "alt" "alt"))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text "* Headline 1\n:PROPERTIES:\n:ALT_TITLE: alt\n:END:"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-todo ()
  "Todo keywords should get passed to the headline."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
            (format "title={\\\\TestOrgHeadline[\s\n]*\\[Todo={TODO},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                    test-org-context-headline-name
                    test-org-context-headline-name
                    test-org-context-headline-name
                    test-org-context-headline-name))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text "* TODO Headline 1"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-tags ()
  "Tags should get passed to the headline as arguments."
  (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\TestOrgHeadline[\s\n]*\\[Text={%s},[\s\n]*Tags={tag1:tag2}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      test-org-context-headline-name
                      test-org-context-headline-name
                      test-org-context-headline-name
                      test-org-context-headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       test-org-context-headline-command
       (context-test-with-temp-text "* Headline 1 :tag1:tag2:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-priority ()
  "Priority should get passed to the headline command."
  (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\TestOrgHeadline[\s\n]*\\[Priority={A},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      test-org-context-headline-name
                      test-org-context-headline-name
                      test-org-context-headline-name
                      test-org-context-headline-name))
      (context-test-with-temp-customization-value
       org-context-headline-command
       test-org-context-headline-command
       (context-test-with-temp-text "* [#A] Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty" :with-priority t))))))))

;;; Keywords
(ert-deftest test-org-context/keyword-context ()
  "Test the CONTEXT keyword."
  (should
   (equal
    "abc"
    (context-test-with-temp-text
     "#+CONTEXT: abc"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-index ()
  "Test the INDEX keyword."
  (should
   (equal
    "\\index{foo}"
    (context-test-with-temp-text
     "#+INDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-cindex ()
  "Test the CINDEX keyword."
  (should
   (equal
    "\\OrgConcept{foo}"
    (context-test-with-temp-text "#+CINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-findex ()
  "Test the FINDEX keyword."
  (should
   (equal
    "\\OrgFunction{foo}"
    (context-test-with-temp-text
     "#+FINDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-kindex ()
  "Test the KINDEX keyword."
  (should
   (equal
    "\\OrgKeystroke{foo}"
    (context-test-with-temp-text
     "#+KINDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-pindex ()
  "Test the PINDEX keyword."
  (should
   (equal
    "\\OrgProgram{foo}"
    (context-test-with-temp-text
     "#+PINDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-tindex ()
  "Test the TINDEX keyword."
  (should
   (equal
    "\\OrgDataType{foo}"
    (context-test-with-temp-text
     "#+TINDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-vindex ()
  "Test the VINDEX keyword."
  (should
   (equal
    "\\OrgVariable{foo}"
    (context-test-with-temp-text
     "#+VINDEX: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-tables ()
  "Test placing list of tables."
  (should
   (equal
    "\\placelistoftables[criterium=all]"
    (context-test-with-temp-text
     "#+TOC: tables"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-figures ()
  "Test placing list of figures."
  (should
   (equal
    "\\placelistoffigures[criterium=all]"
    (context-test-with-temp-text
     "#+TOC: figures"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-equations ()
  "Test placing list of equations."
  (should
   (equal
    "\\placelist[formula][criterium=all]"
    (context-test-with-temp-text
     "#+TOC: equations"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-definitions ()
  "Test placing the standard index."
  (should
   (equal
    "\\placeindex"
    (context-test-with-temp-text
     "#+TOC: definitions"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-cp ()
  "Test placing list of concepts."
  (should
   (equal
    "\\placeregister[OrgConcept]"
    (context-test-with-temp-text
     "#+TOC: cp"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-fn ()
  "Test placing list of orgfunctions."
  (should
   (equal
    "\\placeregister[OrgFunction]"
    (context-test-with-temp-text
     "#+TOC: fn"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-ky ()
  "Test placing list of keystrokes."
  (should
   (equal
    "\\placeregister[OrgKeystroke]"
    (context-test-with-temp-text
     "#+TOC: ky"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-pg ()
  "Test placing list of programs."
  (should
   (equal
    "\\placeregister[OrgProgram]"
    (context-test-with-temp-text "#+TOC: pg"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-tp ()
  "Test placing list of datatypes."
  (should
   (equal
    "\\placeregister[OrgDataType]"
    (context-test-with-temp-text "#+TOC: tp"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-vr ()
  "Test placing list of variables."
  (should
   (equal
    "\\placeregister[OrgVariable]"
    (context-test-with-temp-text
     "#+TOC: vr"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-headline-plain ()
  "Test placing plain toc"
  (should
   (equal
    "\\placecontent[]"
    (context-test-with-temp-text
     "#+TOC: headlines"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))) )
(ert-deftest test-org-context/keyword-toc-headline-limited ()
  "Test placing toc limited to depth."
  (should
   (equal
    "\\placecontent[list={section,subject,subsection,subsubject}]"
    (context-test-with-temp-text
     "#+TOC: headlines 2"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-headlines-local ()
  "Test placing table of contents with local keyword."
  (should
   (equal
    "\\placecontent[criterium=local,]"
    (context-test-with-temp-text
     "#+TOC: headlines local"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-listings ()
  "Place list of listings."
  (should
   (equal
    "\\placelist[OrgListingEmpty][criterium=all]"
    (context-test-with-temp-text
     "#+TOC: listings"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-verses ()
  "Test placing list of verses."
  (should
   (equal
    "\\placelist[OrgVerseEnumerateEmpty][criterium=all]"
    (context-test-with-temp-text
     "#+TOC: verses"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-quotes ()
  "Test placing list of quotes."
  (should
   (equal
    "\\placelist[OrgBlockQuoteEnumEmpty][criterium=all]"
    (context-test-with-temp-text
     "#+TOC: quotes"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-examples ()
  "Test placing list of examples."
  (should
   (equal
    "\\placelist[OrgExampleEnumerationEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: examples"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-bibliography ()
  "Test placing bibliography"
  ;; TODO
  (should nil))

;;; Document Keywords

(ert-deftest test-org-context/keyword-attention ()
  "Test the ATTENTION keyword."
  (should
   (string-match-p
    (regexp-quote "letter:attention={bob}")
    (context-test-with-temp-text
     "#+ATTENTION: bob"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-closing ()
  "Test the CLOSING keyword."
  (should
   (string-match-p
    (regexp-quote "letter:closing={Sincerely, Bob}")
    (context-test-with-temp-text
     "#+CLOSING: Sincerely, Bob"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-context-header ()
  "Test the CONTEXT_HEADER keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     "#+CONTEXT_HEADER: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-context-header-extra ()
  "Test the CONTEXT_HEADER_EXTRA keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     "#+CONTEXT_HEADER_EXTRA: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-date ()
  "Test the DATE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:date={foo}")
    (context-test-with-temp-text
     "#+DATE: foo"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-description ()
  "Test the DESCRIPTION keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:description={foo}")
    (context-test-with-temp-text
     "#+DESCRIPTION: foo"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-from-address-doc ()
  "Test the FROM_ADDRESS keyword."
  (should
   (string-match-p
    (regexp-quote "letter:fromaddress={foo}")
    (context-test-with-temp-text
     "#+FROM_ADDRESS: foo"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-from-address-cust ()
  "Test the FROM_ADDRESS variable in customization."
  (should
   (string-match-p
    (regexp-quote "letter:fromaddress={foo}")
    (context-test-with-temp-customization-value
     org-context-from-address
     "foo"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-keywords ()
  "Test the KEYWORDS document keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:keywords={foo bar baz}")
    (context-test-with-temp-text
     "#+KEYWORDS: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-language ()
  "Test the LANGUAGE keyword"
  (let ((content
         (context-test-with-temp-text
          "#+LANGUAGE: grobnatch"
          (org-export-as 'context nil nil nil '(:context-preset "empty")))))
    (should
     (string-match-p
      (regexp-quote "metadata:language={grobnatch}")
      content))
    (should
     (string-match-p
      (regexp-quote "\\language[grobnatch]")
      content))))
(ert-deftest test-org-context/keyword-location-doc ()
  "Test the LOCATION keyword in the document."
  (should
   (string-match-p
    (regexp-quote "letter:location={grokistan}")
    (context-test-with-temp-text
     "#+LOCATION: grokistan"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-location-cust ()
  "Test the LOCATION keyword set with a customization value."
  (should
   (string-match-p
    (regexp-quote "letter:location={grokistan}")
    (context-test-with-temp-customization-value
     org-context-location
     "grokistan"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-opening-doc ()
  "Test the OPENING keyword in the document body."
  (should
   (string-match-p
    (regexp-quote "letter:opening={biz buz}")
    (context-test-with-temp-text
     "#+OPENING: biz buz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-opening-cust ()
  "Test the OPENING keyword set with a customization value."
  (should
   (string-match-p
    (regexp-quote "letter:opening={biz buz}")
    (context-test-with-temp-customization-value
     org-context-opening
     "biz buz"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-phonenumber-doc ()
  "Test the PHONE_NUMBER keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:phonenumber={314 159 2653}")
    (context-test-with-temp-text
     "#+PHONE_NUMBER: 314 159 2653"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-phonenumber-cust ()
  "Test the PHONE_NUMBER keyword set with customization variable."
  (should
   (string-match-p
    (regexp-quote "metadata:phonenumber={314 159 2653}")
    (context-test-with-temp-customization-value
     org-context-phone-number
     "314 159 2653"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-place-doc ()
  "Test the PLACE keyword in the document."
  (should
   (string-match-p
    (regexp-quote "letter:place={Arlen Texas}")
    (context-test-with-temp-text
     "#+PLACE: Arlen Texas"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-place-cust ()
  "Test the PLACE keyword set with customization."
  (should
   (string-match-p
    (regexp-quote "letter:place={Arlen Texas}")
    (context-test-with-temp-customization-value
     org-context-place
     "Arlen Texas"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-signature-doc ()
  "Test the SIGNATURE keyword in the document."
  (should
   (string-match-p
    (regexp-quote "letter:signature={buz buz}")
    (context-test-with-temp-text
     "#+SIGNATURE: buz buz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-signature-cust ()
  "Test the SIGNATURE keyword set with customization values."
  (should
   (string-match-p
    (regexp-quote "letter:signature={buz buz}")
    (context-test-with-temp-customization-value
     org-context-signature
     "buz buz"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-subject ()
  "Test the SUBJECT keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subject={vorpal swords}")
    (context-test-with-temp-text
     "#+SUBJECT: vorpal swords"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-subtitle ()
  "Test the SUBTITLE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subtitle={frumorious bandersnatches}")
    (context-test-with-temp-text
     "#+SUBTITLE: frumorious bandersnatches"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-to-address ()
  "Test the TO_ADDRESS keyword."
  (should
   (string-match-p
    (regexp-quote "letter:toaddress={the palace}")
    (context-test-with-temp-text
     "#+TO_ADDRESS: the palace"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-to-name ()
  "Test the TO_NAME keyword."
  (should
   (string-match-p
    (regexp-quote "letter:toname={bob}")
    (context-test-with-temp-text
     "#+TO_NAME: bob"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-url-doc ()
  "Test the URL keyword in the document."
  (should
   (string-match-p
    (regexp-quote "metadata:url={beware the jabberwock}")
    (context-test-with-temp-text
     "#+URL: beware the jabberwock"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-url-cust ()
  "Test the URL keyword in customization variables."
  (should
   (string-match-p
    (regexp-quote "metadata:url={beware the jabberwock}")
    (context-test-with-temp-customization-value
     org-context-url
     "beware the jabberwock"
     (context-test-with-temp-text
      ""
      (org-export-as 'context nil nil nil '(:context-preset "empty")))))))


;;; Images
(ert-deftest test-org-context/image-simple ()
  "Test exporting simple image."
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
      )))))
(ert-deftest test-org-context/image-caption ()
  "Test image with caption."
  (should
   (equal
    "\\startplacefigure[title={A cat},
   location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+CAPTION: A cat\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-wrap ()
  "Test image with wrap position."
  (should
   (equal
    "\\startplacefigure[location={here,left}]
\\externalfigure[./images/cat.jpg][width={0.48\\hsize}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float wrap\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-sideways ()
  "Test image with sideways position."
  (should
   (equal
    "\\startplacefigure[location={page,90}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float sideways\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-multicolumn ()
  "Test image with multicolumn position."
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr\\makeupwidth - 1em\\relax}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float multicolumn\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-placement ()
  "Test image with specific placement."
  (should
   (equal
    "\\startplacefigure[location={backspace}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :placement backspace\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-width-local ()
  "Test image with specified width."
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={2in}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :width 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-height-local ()
  "Test image with specified height."
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][height={2in}]
\\stopplacefigure"
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :height 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-width-height-cust ()
  "Test image options set with customization values."
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
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/image-height-cust ()
  "Test image height set in customization."
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
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))


;;; Links
(ert-deftest test-org-context/link-plain ()
  "Test plain links."
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (context-test-with-temp-text
     "[[http://orgmode.org]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-bare ()
  "Test bare link with not surrounding braces."
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (context-test-with-temp-text
     "http://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-with-desc ()
  "Test links with descriptions."
  (should
   (equal
    "\\goto{Some Description}[url(http://orgmode.org)]"
    (context-test-with-temp-text
     "[[http://orgmode.org][Some Description]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-https ()
  "Test an https link"
  (should
   (equal
    "\\goto{\\hyphenatedurl{https://orgmode.org}}[url(https://orgmode.org)]"
    (context-test-with-temp-text
     "https://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-doi ()
  "Test a doi link."
  (should
   (equal
    "\\goto{\\hyphenatedurl{doi:10.1000/182}}[url(doi:10.1000/182)]"
    (context-test-with-temp-text
     "doi:10.1000/182"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-ftp ()
  "Test ftp link."
  (should
   (equal
    "\\goto{\\hyphenatedurl{ftp:orgmode.org}}[url(ftp:orgmode.org)]"
    (context-test-with-temp-text
     "ftp:orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/link-other ()
  "Test a link to an unsupported type."
  (should
   (equal
    "\\goto{\\hyphenatedurl{projects.org}}[url(projects.org)]"
    (context-test-with-temp-text
     "attachment:projects.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))


;;; Labels
(ert-deftest test-org-context/label-center-none ()
  "Test no label on center"
  (should
   (string-match-p
    (regexp-quote "\\startalignment")
    (context-test-with-temp-text
     "#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/label-center-some ()
  "Test labelling center block."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\reference[org")
     "[0-9a-f]+"
     (regexp-quote "]{foo}")
     "\\(.\\|\n\\)*"
     (regexp-quote
     "\\startalignment"))
    (context-test-with-temp-text
     "#+NAME: foo\n#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))


;;; Tables
(ert-deftest test-org-context/table-empty ()
  "Test empty table structure."
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
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
;;;; Location
(ert-deftest test-org-context/table-loc-cust-none ()
  "Test table location disabled in customization."
  (should-not
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
(ert-deftest test-org-context/table-loc-cust-some ()
  "Table location enabled in customization."
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
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/table-loc-doc ()
  "Table location set in document keywords."
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
    (context-test-with-temp-text
     "#+TABLE_LOCATION: there\n
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/table-loc-attr ()
  "Table location in ATTR_CONTEXT"
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
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :location there
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/table-loc-attr-doc ()
  "Test table location in ATTR_CONTEXT and TABLE_LOCATION"
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
    (context-test-with-temp-text
     "#+TABLE_LOCATION: here
#+ATTR_CONTEXT: :location there
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
;;;; Header
(ert-deftest test-org-context/table-head-repeat-cust-some ()
  "Test table header repeat with customization."
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
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-head-repeat-cust-none ()
  "Test table header disabled in customization."
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
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/table-head-doc-none ()
  "Test turning off header repeat in document keywords."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "header={")
     "[^}]*"
     "repeat")
    (context-test-with-temp-customization-value
     org-context-table-head
     "repeat"
     (context-test-with-temp-text
      "#+TABLE_HEAD: nil
| foo |
|-----|
| bar |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-head-attr-none ()
  "Turn on header repeat in ATTR_CONTEXT."
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
    (context-test-with-temp-customization-value
     org-context-table-head
     ""
     (context-test-with-temp-text
     "
#+ATTR_CONTEXT: :header repeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-head-attr-doc-on ()
  "Test override header document setting in ATTR_CONTEXT."
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
    (context-test-with-temp-text
     "#+TABLE_HEAD: nil
#+ATTR_CONTEXT: :header repeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/table-head-attr-doc-off ()
  "Test override header document setting in ATTR_CONTEXT."
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
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
;;;; Footer
(ert-deftest test-org-context/table-foot-cust-some ()
  "Test table footer with customization enabled."
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
                      '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-foot-cust-none ()
  "Test table footer customization disabled."
  (should-not
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
(ert-deftest test-org-context/table-foot-doc-some ()
  "Turn on footer repeat in document keywords."
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
    (context-test-with-temp-customization-value
     org-context-table-foot
     ""
     (context-test-with-temp-text
      "#+TABLE_FOOT: repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-foot-attr-some ()
  "Test turn on footer repeat in ATTR_CONTEXT"
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
    (context-test-with-temp-customization-value
     org-context-table-foot
     ""
     (context-test-with-temp-text "#+ATTR_CONTEXT: :footer repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-foot-attr-doc ()
  "Test override footer document setting in ATTR_CONTEXT"
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
    (context-test-with-temp-customization-value
     org-context-table-foot
     ""
     (context-test-with-temp-text
      "#+TABLE_FOOT: nil
#+ATTR_CONTEXT: :footer repeat
| foo |
|-----|
| bar |
|-----|
| baz |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-foot-attr-doc-2 ()
  "Test override document setting in ATTR_CONTEXT."
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
    (context-test-with-temp-customization-value
     org-context-table-foot
     ""
     (context-test-with-temp-text "#+TABLE_FOOT: repeat
#+ATTR_CONTEXT: :footer norepeat
| foo |
|-----|
| bar |
|-----|
| baz |"
                                  (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
;;;; Table option
(ert-deftest test-org-context/table-option-cust-some ()
  "Table option configured in customization."
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
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-option-cust-none ()
  "Test table option not configured."
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
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/table-option-doc-some ()
  "Test table option in document keyword."
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
     "foo"
     (context-test-with-temp-text
     "#+TABLE_OPTION: bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-option-attr-some ()
  "Test table option in ATTR_CONTEXT"
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
     "foo"
     (context-test-with-temp-text
      "#+ATTR_CONTEXT: :option bar
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-option-attr-doc ()
  "Test table option configured in both doc keyword and ATTR_CONTEXT."
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
    (context-test-with-temp-customization-value
     org-context-table-option
     "buz"
     (context-test-with-temp-text
      "#+TABLE_OPTION: bar
#+ATTR_CONTEXT: :option baz
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
;;;; Table style
(ert-deftest test-org-context/table-style-doc-some ()
  "Test table style configured in document keywords."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "baz"
     (context-test-with-temp-text
      "#+TABLE_STYLE: bar

| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-style-attr-some ()
  "Test table style configured in ATTR_CONTEXT"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "baz"
     (context-test-with-temp-text
      "#+ATTR_CONTEXT: :table-style bar
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-style-attr-doc ()
  "Test table style configured in document keyword and ATTR_CONTEXT."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[baz]"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "buz"
     (context-test-with-temp-text
      "#+TABLE_STYLE: bar
#+ATTR_CONTEXT: :table-style baz
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-style-cust-some ()
  "Test table style configured in customization."
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
                       '(:context-preset "empty"))))))))
;;;; Table float style
(ert-deftest test-org-context/table-float-doc-some ()
  "Test table style configured in document keywords."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "buz"
     (context-test-with-temp-text
      "#+TABLE_FLOAT: bar

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-float-attr-some ()
  "Test table float style configured in ATTR_CONTEXT."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-customization-value
     org-context-table-style
     "baz"
     (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float-style bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-float-cust-some ()
  "Test table float style configured in customization."
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
     (context-test-with-temp-text
      "| foo |"
      (org-trim
       (org-export-as 'context nil nil t
                      '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-float-attr-doc ()
  "Test float style configured in document keywords and ATTR_CONTEXT."
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
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
;;;; Table split
(ert-deftest test-org-context/table-split-cust-some ()
  "Test table split configured with customization value."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "split[^}]*"))
      nil nil t)
     "[[:space:]]*"
     (regexp-quote "\\startxtable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("split" . "yes"))
      nil nil t))
    (context-test-with-temp-customization-value
     org-context-table-split
     "yes"
     (context-test-with-temp-text "| foo |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-split-cust-none ()
  "Test table split disabled with customization value."
  (should-not
   (string-match-p
     (concat
      (regexp-quote "\\startplacetable")
      "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "split"))))
     (context-test-with-temp-customization-value
      org-context-table-split
      "no"
      (context-test-with-temp-text "| foo |"
        (org-trim
         (org-export-as 'context nil nil t
                        '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-split-doc-none ()
  "Test disable table-split in document keywords."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "split"))))
    (context-test-with-temp-customization-value
     org-context-table-split
     "yes"
     (context-test-with-temp-text
      "#+TABLE_SPLIT: no

| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-split-doc-special ()
  "Custom split keywords are passed to startxtable."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("split" . "no"))
      nil nil t))
    (context-test-with-temp-customization-value
     org-context-table-split
     "yes"
     (context-test-with-temp-text
     "#+TABLE_SPlIT: no

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))

(ert-deftest test-org-context/table-split-attr-none ()
  "Test table split disabled in ATTR_CONTEXT."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "split"))
      nil nill t))
    (context-test-with-temp-customization-value
     org-context-table-split
     "yes"
     (context-test-with-temp-text
      "#+ATTR_CONTEXT: :split no
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("split" . "no"))))
    (context-test-with-temp-text "#+ATTR_CONTEXT: :split no
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/table-split-attr-doc ()
  "Test table split configured in doc keyword and ATTR_CONTEXT."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "split[^}]*"))
      nil nil t)
     "[[:space:]]*"
     (regexp-quote "\\startxtable")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("split" . "yes"))
      nil nil t))
    (context-test-with-temp-text "#+TABLE_SPLIT: no
#+ATTR_CONTEXT: :split yes
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Document Structure
(ert-deftest test-org-context/document-structure-1 ()
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
