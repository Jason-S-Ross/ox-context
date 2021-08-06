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
;; TODO Don't need to use `context-test-with-temp-customization-value'.
;;      Just use let binding!


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
  `(let ((,varname ,value))
     ,@body))
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
    (let ((org-context-export-quotes-alist
           '((primary-opening . "\\quotation{")
             (primary-closing . "}")
             (secondary-opening . "\\quote{")
             (secondary-closing . "}")
             (apostrophe . "'")))
          (org-export-with-smart-quotes t))
      (context-test-with-temp-text
       "Quote: \"Cogito ergo sum\" - Descartes"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/smart-quote-basic-plist ()
  "Test simple smart quotes."
  (should
   (equal
    "Quote: \\testquotation{Cogito ergo sum} - Descartes"
    (let ((org-export-with-smart-quotes t))
      (context-test-with-temp-text
       "Quote: \"Cogito ergo sum\" - Descartes"
       (org-trim
        (org-export-as
         'context nil nil t
         '(:context-preset "empty"
           :context-export-quotes-alist
           ((primary-opening . "\\testquotation{")
            (primary-closing . "}")
            (secondary-opening . "\\testquote{")
            (secondary-closing . "}")
            (apostrophe . "'"))))))))))
(ert-deftest test-org-context/smart-quote-apostrophe ()
  "Test apostrophes in text."
  (should
   (equal
    "Here's a quote: \\quotation{I think, therefore I am}"
    (let ((org-export-with-smart-quotes t))
      (context-test-with-temp-customization-value
       org-context-export-quotes-alist
       '((primary-opening . "\\quotation{")
         (primary-closing . "}")
         (secondary-opening . "\\quote{")
         (secondary-closing . "}")
         (apostrophe . "'"))
       (context-test-with-temp-text
        "Here's a quote: \"I think, therefore I am\""
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/smart-quote-nested ()
  "Test nested quotes."
  (should
   (equal
    "Here\\TestApostrophes a nested quote: \\TestPrimary{Descartes says \\TestSecondary{I think therefore I am}}"
    (let ((org-export-with-smart-quotes t))
      (context-test-with-temp-customization-value
       org-context-export-quotes-alist
       '((primary-opening . "\\TestPrimary{")
         (primary-closing . "}")
         (secondary-opening . "\\TestSecondary{")
         (secondary-closing . "}")
         (apostrophe . "\\TestApostrophe"))
       (context-test-with-temp-text
        "Here's a nested quote: \"Descartes says 'I think therefore I am'\""
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))



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
       "[[:space:]]*"
       (context-test-build-ConTeXt-argument-regex
        '(("title" . "foo")
          ("reference" . "org[0-9a-f]+")))
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
(ert-deftest test-org-context/block-quote-3 ()
  "Test block quotes."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+BEGIN_QUOTE
foo bar baz
#+END_QUOTE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-enumerate-blockquote-empty-environment (cons enumname enumdef)
              :context-blockquote-environment (cons name def)))))))
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
(ert-deftest test-org-context/block-quote-4 ()
  "Test block quotes with captions."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+CAPTION: foo
#+BEGIN_QUOTE
foo bar baz
#+END_QUOTE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-blockquote-environment (cons name def)
              :context-enumerate-blockquote-environment (cons enumname enumdef)))))))
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
;;;; Dynamic Blocks
(ert-deftest test-org-context/dynamic-block ()
  "Test dynamic blocks."
  (should
   (equal
    "foo bar baz"
    (context-test-with-temp-text
     "#+BEGIN: foo :bar 1 :baz 2
foo bar baz
#+END:"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
;;;; Entities
(ert-deftest test-org-context/entity ()
  "Test dynamic blocks."
  (should
   (equal
    "À"
    (context-test-with-temp-text
     "\\Agrave"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))

;;;; Export block
(ert-deftest test-org-context/export-context ()
  "Test exporting context."
  (should
   (equal
    "foo"
    (context-test-with-temp-text
     "#+BEGIN_EXPORT context
foo
#+END_EXPORT"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
(ert-deftest test-org-context/export-tex ()
  "Test exporting context."
  (should
   (equal
    "foo"
    (context-test-with-temp-text
     "#+BEGIN_EXPORT tex
foo
#+END_EXPORT"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
(ert-deftest test-org-context/export-metapost ()
  "Test exporting context."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startMPcode")
     "[[:space:]]*"
     "foo"
     "[[:space:]]*"
     (regexp-quote "\\stopMPcode"))
    (context-test-with-temp-text
     "#+BEGIN_EXPORT metapost
foo
#+END_EXPORT"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
;;;; Export snippet
(ert-deftest test-org-context/export-snippet ()
  "Test exporting snippet."
  (should
   (equal
    "foo"
    (context-test-with-temp-text
     "@@context:foo@@"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
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
(ert-deftest test-org-context/example-3 ()
  "Test simple example."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-example-environment (cons name def)
              :context-enumerate-example-empty-environment (cons enumname enumdef)))))))
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
(ert-deftest test-org-context/example-4 ()
  "Test named example."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+CAPTION: foo
#+BEGIN_EXAMPLE
foo bar baz
#+END_EXAMPLE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-example-environment (cons name def)
              :context-enumerate-example-environment (cons enumname enumdef)))))))
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
(ert-deftest test-org-context/fixed-plist ()
  "Test fixed width."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           ": foo bar baz"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list :context-fixed-environment (cons name def)))))))
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
;;;; Footnote
(ert-deftest test-org-context/footnote-simple ()
  "Test exporting footnote."
  (let* ((content
          (context-test-with-temp-text
           "Footnotes: [fn:: l1 ]"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))
         (matches
          (string-match
           (concat
            (regexp-quote "Footnotes: \\note[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "\\footnotetext[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "{l1}")
            )
           content)))
    (should
     (equal
      (match-string 1 content)
      (match-string 2 content)))))
(ert-deftest test-org-context/footnote-nested ()
  "Test exporting footnote."
  (let* ((content
          (context-test-with-temp-text
           "Footnotes: [fn:: l1 [fn:: l2 [fn:: l3]]]"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))
         (matches
          (string-match
           (concat
            (regexp-quote "Footnotes: \\note[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "\\footnotetext[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "{l1 \\note[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]}%")
            "[[:space:]]*"
            (regexp-quote "\\footnotetext[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "{l2 \\note[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]}%")
            "[[:space:]]*"
            (regexp-quote "\\footnotetext[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "{l3}")
            )
           content)))
    (should
     (equal
      (match-string 1 content)
      (match-string 2 content)))
    (should
     (equal
      (match-string 3 content)
      (match-string 4 content)))
    (should
     (equal
      (match-string 5 content)
      (match-string 6 content)))))
(ert-deftest test-org-context/footnote-table ()
  "Test exporting footnote."
  (let* ((content
          (context-test-with-temp-text
           "| foo [fn:: l1 ] |"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))
         (matches
          (string-match
           (concat
            (regexp-quote "foo \\note[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "\\footnotetext[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]")
            "[[:space:]]*"
            (regexp-quote "{l1}")
            )
           content)))
    (should
     (equal
      (match-string 1 content)
      (match-string 2 content)))))
;;;; Horizontal Rule
(ert-deftest test-org-context/hrule ()
  "Test horizontal rules."
  (should
   (equal
    "\\textrule"
    (context-test-with-temp-text
     "-----"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
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
(ert-deftest test-org-context/inline-src-3 ()
  "Test inline source with default formatter"
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "src_python[:exports code]{print(\"Hello, world!\")}"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-inline-source-environment (cons name def)
              :context-syntax-engine 'default))))))
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
(ert-deftest test-org-context/inline-src-4 ()
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
              (org-export-as
               'context nil nil nil
               (list
                :context-inline-source-environment (cons name def)
                :context-syntax-engine 'vim))))))))
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
;;;; Itemized List
(ert-deftest test-org-context/items-simple ()
  "Test plain flat lists."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startitemize")
     "[[:space:]]*"
     (regexp-quote "\\item A")
     "[[:space:]]*"
     (regexp-quote "\\item B")
     "[[:space:]]*"
     (regexp-quote "\\item C")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     )
    (context-test-with-temp-text
     "- A
- B
- C"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
(ert-deftest test-org-context/items-alpha ()
  "Test alphabetized flat lists."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startitemize[a]")
     "[[:space:]]*"
     (regexp-quote "\\item foo")
     "[[:space:]]*"
     (regexp-quote "\\item bar")
     "[[:space:]]*"
     (regexp-quote "\\item baz")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     )
    (let ((org-list-allow-alphabetical t))
      (context-test-with-temp-text
       "a. foo
b. bar
c. baz"

       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty"))))))))
(ert-deftest test-org-context/items-num ()
  "Test alphabetized flat lists."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startitemize[n]")
     "[[:space:]]*"
     (regexp-quote "\\item foo")
     "[[:space:]]*"
     (regexp-quote "\\item bar")
     "[[:space:]]*"
     (regexp-quote "\\item baz")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     )
    (context-test-with-temp-text
"1. foo
2. bar
3. baz"

     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
(ert-deftest test-org-context/items-nested ()
  "Test alphabetized flat lists."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startitemize[n]")
     "[[:space:]]*"
     (regexp-quote "\\item foo")
     "[[:space:]]*"
     (regexp-quote "\\startitemize")
     "[[:space:]]*"
     (regexp-quote "\\item e1")
     "[[:space:]]*"
     (regexp-quote "\\item e2")
     "[[:space:]]*"
     (regexp-quote "\\startitemize[n]")
     "[[:space:]]*"
     (regexp-quote "\\item e2.1")
     "[[:space:]]*"
     (regexp-quote "\\item e2.2")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     "[[:space:]]*"
     (regexp-quote "\\item bar")
     "[[:space:]]*"
     (regexp-quote "\\item baz")
     "[[:space:]]*"
     (regexp-quote "\\stopitemize")
     )
    (context-test-with-temp-text
"1. foo
  - e1
  - e2
    1. e2.1
    2. e2.2
2. bar
3. baz"

     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
;;;; LaTeX Fragment
(ert-deftest test-org-context/latex-fragment-dddollar-unnumbered ()
  "Test double-dollar equations with no numbering."
  (let ((content
         (context-test-with-temp-text
          "$$foo$$"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      content))
    (should-not
     (string-match-p
      (regexp-quote "placeformula")
      content))))
(ert-deftest test-org-context/latex-fragment-ddollar-numbered-doc ()
  "Test numbering double-dollar equations in document options."
  (let ((content
         (context-test-with-temp-text
          "#+OPTIONS: numeq:t
$$foo$$"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\placeformula")
       "[[:space:]]*"
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      content))))
(ert-deftest test-org-context/latex-fragment-ddollar-numbered-plist ()
  "Test numbering double-dollar equations in plist."
  (let ((content
         (context-test-with-temp-text
          "$$foo$$"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations t))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\placeformula")
       "[[:space:]]*"
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      content))))
(ert-deftest test-org-context/latex-fragment-ddollar-numbered-cust ()
  "Test numbering double-dollar equations with customization variables."
  (let ((content
         (context-test-with-temp-customization-value
          org-context-number-equations
          t
          (context-test-with-temp-text
           "$$foo$$"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\placeformula")
       "[[:space:]]*"
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      content))))
(ert-deftest test-org-context/latex-fragment-bracket-numbered-doc ()
  "Test matching bracket-delimited equations."
  (let ((content
         (context-test-with-temp-text
          "#+OPTIONS: numeq:t
\\[foo\\]"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\placeformula")
       "[[:space:]]*"
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      content))))
(ert-deftest test-org-context/latex-fragment-paren-doc ()
  "Test matching parenthesis-delimited equations."
  (let ((content
         (context-test-with-temp-text
          "\\(foo\\)"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should (string-match-p (concat (regexp-quote "\\m{foo}")) content))))
(ert-deftest test-org-context/latex-fragment-dollar-doc ()
  "Test matching single-dollar equations."
  (let ((content
         (context-test-with-temp-text
          "$foo$"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should (string-match-p (concat (regexp-quote "\\m{foo}")) content))))

(ert-deftest test-org-context/latex-fragment-line-break ()
  "Test citations."
  (let ((content
         (context-test-with-temp-text
          "foo\\\\
bar"
          (org-trim
           (org-export-as 'context nil nil t
                          '(:context-preset "empty"
                            :context-number-equations nil))))))
    (should (string-match-p (concat (regexp-quote "foo\\crlf\nbar")) content))))
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
(ert-deftest test-org-context/property-drawers-plist ()
  "Test property drawers."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (name2 (format "%i" (random)))
         (def2 (format "%i" (random)))
         (key (format "%i" (random)))
         (val (format "%i" (random)))
         (document
          (context-test-with-temp-text
           (format "* Foo
:PROPERTIES:
:%s: %s
:END:" key val)
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-property-drawer-environment (cons name def)
              :context-node-property-command (cons name2 def2)
              :with-properties t))))))
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
(ert-deftest test-org-context/descriptions-plist ()
  "Test description items"
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "- foo :: bar"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list :context-description-command (cons name def)))))))
    (should
     (string-match-p
      (concat
       (regexp-quote (format "\\start%s{foo}" name))
       "\\(.\\|\n\\)*"
       (regexp-quote "bar")
       "\\(.\\|\n\\)*"
       (regexp-quote (format "\\stop%s" name)))
      document))
    (should (string-match-p def document))))
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
(ert-deftest test-org-context/block-source-6 ()
  "Test block source plist environemnt"
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+CAPTION: foo
#+BEGIN_SRC python
print(\"Hello, world!\")
#+END_SRC"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list :context-block-source-environment (cons name def)
                   :context-enumerate-listing-environment (cons enumname enumdef)
                   :context-syntax-engine 'default))))))
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
(ert-deftest test-org-context/block-source-7 ()
  "Test block source."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+BEGIN_SRC python
print(\"Hello, world!\")
#+END_SRC"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list :context-block-source-environment (cons name def)
                   :context-enumerate-listing-empty-environment (cons enumname enumdef)
                   :context-syntax-engine 'vim))))))
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
(ert-deftest test-org-context/block-source-8 ()
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
              "#+BEGIN_SRC foolang
print(\"Hello, world!\")
#+END_SRC"
              (org-trim
               (org-export-as 'context nil nil nil
                              '(:context-preset "empty"
                                :context-highlighted-langs (("foolang" . "barlang"))))))
             )))
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
(ert-deftest test-org-context/block-source-9 ()
  "Test block source"
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-customization-value
             org-context-block-source-environment
             (cons name def)
             (context-test-with-temp-text
              "#+BEGIN_SRC foolang
print(\"Hello, world!\")
#+END_SRC"
              (org-trim
               (org-export-as
                'context nil nil nil
                '(:context-preset "empty"
                  :context-syntax-engine vim
                  :context-vim-langs-alist
                  (("foolang" :vim-name "bazlang" :context-name "Barlang")))))))))
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
(ert-deftest test-org-context/block-source-links-1 ()
  "Test block source links with links retained."
  (let* ((document
          (context-test-with-temp-text
           "
#+BEGIN_SRC javascript
    (ref:JavaRef)
#+END_SRC

Here's a link to [[(JavaRef)]]
"
           (org-trim
            (org-export-as
             'context nil nil t
             '(:context-preset "empty")))))
         (match
          (string-match
           (concat
            (regexp-quote "/BTEX\\reference[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]{JavaRef}\\inright{JavaRef}/ETEX")
            "\\(?:.\\|\n\\)*"
            (regexp-quote "Here's a link to \\goto{JavaRef}[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]"))
           document)))
    (should match)
    (should
     (equal
      (match-string 1 document)
      (match-string 2 document)))))
(ert-deftest test-org-context/block-source-links-2 ()
  "Test block source links with links removed."
  (let* ((document
          (context-test-with-temp-text
           "
#+BEGIN_SRC javascript -r
    (ref:JavaRef)
#+END_SRC

Here's a link to [[(JavaRef)]]
"
           (org-trim
            (org-export-as
             'context nil nil t
             '(:context-preset "empty")))))
         (match
          (string-match
           (concat
            (regexp-quote "/BTEX\\reference[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]{1}/ETEX")
            "\\(?:.\\|\n\\)*"
            (regexp-quote "Here's a link to \\goto{\\ref[default][")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]}[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]")
            )
           document)))
    (should match)
    (should
     (equal
      (match-string 1 document)
      (match-string 2 document)))
    (should
     (equal
      (match-string 1 document)
      (match-string 3 document)))))
(ert-deftest test-org-context/block-source-links-3 ()
  "Test block source links with links retained."
  (let* ((document
          (context-test-with-temp-text
           "
#+BEGIN_SRC javascript
    (ref:JavaRef)
#+END_SRC

Here's a link to [[(JavaRef)]]
"
           (org-trim
            (org-export-as
             'context nil nil t
             '(:context-preset "empty"
               :context-source-label "\\Foo{%s}")))))
         (match
          (string-match
           (concat
            (regexp-quote "/BTEX\\reference[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]{JavaRef}\\Foo{JavaRef}/ETEX")
            "\\(?:.\\|\n\\)*"
            (regexp-quote "Here's a link to \\goto{JavaRef}[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote ":JavaRef]"))
           document)))
    (should match)
    (should
     (equal
      (match-string 1 document)
      (match-string 2 document)))))
(ert-deftest test-org-context/block-source-linenum-on-default ()
  "Test block source line numbering."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startOrgBlkSrc[")
     "[^]]*"
     (regexp-quote "numbering={line}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text
     "#+BEGIN_SRC javascript -n
foo
#+END_SRC"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-syntax-engine default
         :context-block-source-environment ("OrgBlkSrc" . ""))))))))
(ert-deftest test-org-context/block-source-linenum-on-vim ()
  "Test block source line numbering."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startOrgBlkSrcJavascript")
     (context-test-build-ConTeXt-argument-regex
      '(("numbering" . "yes"))
      nil nil t))
    (context-test-with-temp-text
     "
#+BEGIN_SRC javascript -n
foo
#+END_SRC"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-syntax-engine vim
         :context-preset "empty"
         :context-block-source-environment ("OrgBlkSrc" . ""))))))))
(ert-deftest test-org-context/block-source-linenum-off ()
  "Test block source line numbering."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\startOrgBlkSrc[")
     "[^]]*"
     (regexp-quote "numbering={line}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text
     "#+BEGIN_SRC javascript
foo
#+END_SRC"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-syntax-engine default
         :context-block-source-environment ("OrgBlkSrc" . ""))))))))
(ert-deftest test-org-context/block-source-linenum-default-continue ()
  "Test block source line numbering."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\stopOrgBlkSrc[")
     "[[:space:]]*"
     (regexp-quote "\\startOrgBlkSrc")
     (context-test-build-ConTeXt-argument-regex
      '(("numbering" . "line")
        ("start" . "2")))
     "[[:space:]]*"
     "[^]]*"
     (regexp-quote "numbering={line}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text
     "#+BEGIN_SRC javascript
foo
bar
#+END_SRC
#+BEGIN_SRC javascript +n
foo
#+END_SRC
"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-syntax-engine default
         :context-block-source-environment ("OrgBlkSrc" . ""))))))))
(ert-deftest test-org-context/block-source-linenum-vim-continue ()
  "Test block source line numbering."
  (should-not
   (string-match-p
    (concat
     (regexp-quote "\\stopOrgBlkSrc[")
     "[[:space:]]*"
     (regexp-quote "\\startOrgBlkSrc")
     (context-test-build-ConTeXt-argument-regex
      '(("numbering" . "yes")
        ("numberstart" . "2")))
     "[[:space:]]*"
     "[^]]*"
     (regexp-quote "numbering={line}")
     "[^]]*"
     (regexp-quote "]"))
    (context-test-with-temp-text
     "#+BEGIN_SRC javascript
foo
bar
#+END_SRC
#+BEGIN_SRC javascript +n
foo
#+END_SRC
"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-syntax-engine vim
         :context-block-source-environment ("OrgBlkSrc" . ""))))))))

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
(ert-deftest test-org-context/verses-3 ()
  "Test verses"
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+BEGIN_VERSE
foo bar baz
#+END_VERSE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-verse-environment (cons name def)
              :context-enumerate-verse-empty-environment (cons enumname enumdef)))))))
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
(ert-deftest test-org-context/verses-4 ()
  "Test verses."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (enumname (format "%i" (random)))
         (enumdef (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "#+CAPTION: foo
#+BEGIN_VERSE
foo bar baz
#+END_VERSE"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-verse-environment (cons name def)
              :context-enumerate-verse-environment (cons enumname enumdef)))))))
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
(ert-deftest test-org-context/drawers-plist ()
  "Test drawers."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           ":MyDrawer:
foo bar baz
:END:"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-drawer-command (cons name def)
              :context-format-drawer-function
              (lambda
                (name contents info)
                (let ((formatter
                       (car (plist-get info :context-drawer-command))))
                  (format "\\TEST%s{%s}{%s}" formatter name contents)))))))))
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
(ert-deftest test-org-context/inline-task-plist ()
  "Test inline tasks."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "*************** TODO [#A] my task :tag1:tag2:
foo bar baz
*************** END"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-format-inlinetask-function
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
                    (concat title "\\hairline" contents "\\hairline"))))
              :context-inlinetask-command (cons name def)))))))
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
(ert-deftest test-org-context/planning-plist ()
  "Test planning."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (plan-string (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "*** TODO write article about the Earth for the Guide
DEADLINE: <2004-02-29 Sun>"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-planning-command (cons name def)
              :with-planning t
              :context-format-timestamp-function
              (lambda (timestamp)
                (format-time-string
                 "TEST TIMESTAMP %Y"
                 (org-timestamp-to-time timestamp)))))))))
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
(ert-deftest test-org-context/clock-plist ()
  "Test clocks."
  (let* ((name (format "%i" (random)))
         (def (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "CLOCK: [2021-01-15 Fri 16:58   ]"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-clock-command (cons name def)
              :context-format-clock-function
              (lambda (timestamp info)
                (let ((formatter
                       (car (plist-get info :context-clock-command)))
                      )
                  (format "\\%s[%s]"
                          formatter
                          (format-time-string
                           "%Y"
                           (org-timestamp-to-time timestamp)))))
              :with-clocks t))))))
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
(ert-deftest test-org-context/markup-bold ()
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
(ert-deftest test-org-context/markup-code-markup ()
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
(ert-deftest test-org-context/markup-italic ()
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
(ert-deftest test-org-context/markup-paragraph ()
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
(ert-deftest test-org-context/markup-strikethrough ()
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
(ert-deftest test-org-context/markup-subscript ()
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
(ert-deftest test-org-context/markup-superscript ()
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
(ert-deftest test-org-context/markup-underline ()
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
(ert-deftest test-org-context/markup-verbatim ()
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
(ert-deftest test-org-context/markup-bold-plist ()
  "Test bold."
 (should
   (equal
    "\\testbold{foo}"
    (context-test-with-temp-text
      "*foo*"
      (org-trim
       (org-export-as
        'context nil nil t
        (list
         :context-preset "empty"
         :context-text-markup-alist
         '((bold . "\\testbold{%s}")))))))))
(ert-deftest test-org-context/markup-bold-italic-plist ()
  "Test bold italic text."
 (should
   (equal
    "\\testbold{\\testbolditalic{foo} bar}"
    (context-test-with-temp-text
      "*/foo/ bar*"
      (org-trim
       (org-export-as
        'context nil nil t
        (list
         :context-preset "empty"
         :context-text-markup-alist
         '((bold . "\\testbold{%s}")
           (bold-italic . "\\testbolditalic{%s}")))))))))
(ert-deftest test-org-context/markup-italic-bold-plist ()
  "Test italic bold text."
 (should
   (equal
    "\\testitalic{\\testbolditalic{foo} bar}"
    (context-test-with-temp-text
      "/*foo* bar/"
      (org-trim
       (org-export-as
        'context nil nil t
        (list
         :context-preset "empty"
         :context-text-markup-alist
         '((italic . "\\testitalic{%s}")
           (bold-italic . "\\testbolditalic{%s}")))))))))

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
(ert-deftest test-org-context/bullet-off-plist ()
  "Test bullet-off command."
  (let* ((rand (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "- [ ] Item 1"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-bullet-off-command (cons "TestItemOff" rand)))))))
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
(ert-deftest test-org-context/bullet-on-plist ()
  "Test bullet on."
  (let* ((rand (format "%i" (random)))
         (document
          (context-test-with-temp-text
           "- [X] Item 1"
           (org-trim
            (org-export-as
             'context nil nil nil
             (list
              :context-bullet-on-command (cons "TestItemOn" rand)))))))
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
(ert-deftest test-org-context/bullet-trans-plist ()
  "Test bullet trans command."
  (let* ((rand (format "%i" (random)))
         (document
          (context-test-with-temp-text
            "- [-] Item 1"
            (org-trim
             (org-export-as
              'context nil nil nil
              (list
               :context-bullet-trans-command (cons "TestItemTrans" rand)))))))
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
      test-org-context-headline-command
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
}")
      test-org-context-headline-name "Headline 1")
(defun test-org-context-format-headline-function
    (todo todo-type priority text tags info)
        (format "TestTodo: %s, TestTodoType: %s, TestPriority: %s, TestText: %s, TestTags: %s"
                todo todo-type priority
                text (mapconcat #'identity tags " ")))
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
(ert-deftest test-org-context/headline-trivial-plist ()
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
      (org-trim
       (org-export-as
        'context nil nil t
        (list :context-headline-command test-org-context-headline-command))))))))
(ert-deftest test-org-context/headline-cust-func-cust ()
  "Test headline with customized format-headline-function."
  (should
   (string-match-p
    (regexp-quote
     "title={TestTodo: TODO, TestTodoType: todo, TestPriority: A, TestText: Headline 1, TestTags: tag1 tag2}")
    (context-test-with-temp-customization-value
     org-context-format-headline-function
     'test-org-context-format-headline-function
     test-org-context-headline-command
     (context-test-with-temp-text
      "* TODO [#A] Headline 1 :tag1:tag2:"
      (org-trim
       (org-export-as
        'context nil nil t
        (list :context-headline-command test-org-context-headline-command
              :with-priority t))))))))
(ert-deftest test-org-context/headline-cust-func-plist ()
  "Test headline with customized format-headline-function."
  (should
   (string-match-p
    (regexp-quote
     "title={TestTodo: TODO, TestTodoType: todo, TestPriority: A, TestText: Headline 1, TestTags: tag1 tag2}")
    (context-test-with-temp-text
      "* TODO [#A] Headline 1 :tag1:tag2:"
      (org-trim
       (org-export-as
        'context nil nil t
        (list :context-headline-command test-org-context-headline-command
              :with-priority t
              :context-format-headline-function 'test-org-context-format-headline-function)))))))
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
                    "Headline `\\lettertilde !@\\#\\$\\%^&*()_-=+\\[\\{\\}\\]\\|\\letterbackslash :;\"'<,>.?/"))
                  (listing (regexp-quote "Headline `~!@^&*()_-=+[{}]:;\"'<,>.?/")))
              (format
               test-org-context-inside-headline-regex
               replacement
               listing
               listing
               listing)))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text
      "* Headline `~!@#$%^&*()_-=+[{}]|\\:;\"'<,>.?/"
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
(ert-deftest test-org-context/headline-withtarget ()
  "Trivial headline match."
  (should
   (string-match-p
    (let ((regex (concat
                (regexp-quote "Foo \\reference[org")
                "[0-9a-f]+"
                (regexp-quote "]{bar}"))))
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format
               test-org-context-inside-headline-regex
               regex
               "Foo"
               "Foo"
               "Foo")))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text
      "* Foo <<bar>>"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/headline-withstyle ()
  "Trivial headline match."
  (should
   (string-match-p
    (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
            (format
             test-org-context-inside-headline-regex
             ".*"
             "Foo bar baz biz buz ab bc verbatim code"
             "Foo bar baz biz buz ab bc verbatim code"
             "Foo bar baz biz buz ab bc verbatim code"))
    (context-test-with-temp-customization-value
     org-context-headline-command
     test-org-context-headline-command
     (context-test-with-temp-text
      "* Foo *bar* /baz/ _biz_ +buz+ a_b b^c =verbatim= ~code~"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))

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
(ert-deftest test-org-context/keyword-index-custom ()
  "Test an index from `org-context-texinfo-indices-alist'."
  (should
   (equal
    "\\TestFoo{foo}"
    (context-test-with-temp-customization-value
     org-context-texinfo-indices-alist
     '(("foo" . (:keyword "FOOINDEX" :command "TestFoo")))
     (context-test-with-temp-text "#+FOOINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
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
(ert-deftest test-org-context/keyword-toc-custom ()
  "Test placing list from `org-context-texinfo-indices-alist'."
  (should
   (equal
    "\\placeregister[TestFoo]"
    (context-test-with-temp-customization-value
     org-context-texinfo-indices-alist
     '(("foo" . (:keyword "FOOINDEX" :command "TestFoo")))
     (context-test-with-temp-text
     "#+TOC: foo"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
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
   (string-match-p
    (regexp-quote
     "\\placecontent[list={section,subsection,subject,subsubject}]")
    (context-test-with-temp-text
     "* Foo
** Bar
*** Baz
**** Biz
***** Baz
* Foo
:PROPERTIES:
:UNNUMBERED: t
:END:
** Bar
:PROPERTIES:
:UNNUMBERED: t
:END:
*** Baz
:PROPERTIES:
:UNNUMBERED: t
:END:
**** Biz
:PROPERTIES:
:UNNUMBERED: t
:END:
***** Baz
:PROPERTIES:
:UNNUMBERED: t
:END:

#+TOC: headlines 2"
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
    "\\placelist[OrgListingEnumEmpty][criterium=all]"
    (context-test-with-temp-text
     "#+TOC: listings"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-verses ()
  "Test placing list of verses."
  (should
   (equal
    "\\placelist[OrgVerseEnumEmpty][criterium=all]"
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
    "\\placelist[OrgExampleEnumEmpty][criterium=all]"
    (context-test-with-temp-text "#+TOC: examples"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/keyword-toc-custom-plist ()
  "Test placing custom list."
  (should
   (equal
    "\\placelist[Foo]"
    (context-test-with-temp-text "#+TOC: foo"
      (org-trim (org-export-as
                 'context nil nil t
                 '(:context-preset "empty"
                   :context-toc-command-alist (("foo" . "\\placelist[Foo]")))))))))
(ert-deftest test-org-context/keyword-toc-custom-cust ()
  "Test placing custom list."
  (should
   (equal
    "\\placelist[Foo]"
    (context-test-with-temp-customization-value
     org-context-toc-command-alist
     '(("foo" . "\\placelist[Foo]"))
     (context-test-with-temp-text "#+TOC: foo"
      (org-trim (org-export-as
                 'context nil nil t
                 '(:context-preset "empty"))))))))

;;; Document Keywords
(ert-deftest test-org-context/keyword-context-header ()
  "Test the CONTEXT_HEADER keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     "#+CONTEXT_HEADER: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-context-header-plist ()
  "Test the CONTEXT_HEADER keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:context-header "foo bar baz"))))))
(ert-deftest test-org-context/keyword-context-header-extra ()
  "Test the CONTEXT_HEADER_EXTRA keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     "#+CONTEXT_HEADER_EXTRA: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-context-header-extra-plist ()
  "Test the CONTEXT_HEADER_EXTRA keyword."
  (should
   (string-match-p
    (regexp-quote "foo bar baz")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:context-header-extra "foo bar baz"))))))
(ert-deftest test-org-context/keyword-date ()
  "Test the DATE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:date={foo}")
    (context-test-with-temp-text
     "#+DATE: foo"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-date-plist ()
  "Test the DATE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:date={foo}")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:date "foo"))))))
(ert-deftest test-org-context/keyword-description ()
  "Test the DESCRIPTION keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:description={foo}")
    (context-test-with-temp-text
     "#+DESCRIPTION: foo"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-description-plist ()
  "Test the DESCRIPTION keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:description={foo}")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:description "foo"))))))
(ert-deftest test-org-context/keyword-keywords ()
  "Test the KEYWORDS document keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:keywords={foo bar baz}")
    (context-test-with-temp-text
     "#+KEYWORDS: foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-keywords-plist ()
  "Test the KEYWORDS document keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:keywords={foo bar baz}")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:keywords "foo bar baz"))))))
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
(ert-deftest test-org-context/keyword-subject ()
  "Test the SUBJECT keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subject={vorpal swords}")
    (context-test-with-temp-text
     "#+SUBJECT: vorpal swords"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-subject-plist ()
  "Test the SUBJECT keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subject={vorpal swords}")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:subject "vorpal swords"))))))
(ert-deftest test-org-context/keyword-subtitle ()
  "Test the SUBTITLE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subtitle={frumorious bandersnatches}")
    (context-test-with-temp-text
     "#+SUBTITLE: frumorious bandersnatches"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/keyword-subtitle-plist ()
  "Test the SUBTITLE keyword."
  (should
   (string-match-p
    (regexp-quote "metadata:subtitle={frumorious bandersnatches}")
    (context-test-with-temp-text
     ""
     (org-export-as 'context nil nil nil '(:subtitle "frumorious bandersnatches"))))))


;;; Images
(ert-deftest test-org-context/image-simple ()
  "Test exporting simple image."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacefigure")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "TestLocation")))
     "[[:space:]]*"
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth")))
     "[[:space:]]*"
     (regexp-quote "\\stopplacefigure"))
    (context-test-with-temp-customization-value
     org-context-float-default-placement
     "TestLocation"
     (context-test-with-temp-customization-value
      org-context-image-default-height
      nil
      (context-test-with-temp-customization-value
       org-context-image-default-width
       '((t . "TestWidth"))
       (context-test-with-temp-text
        "[[./images/cat.jpg]]"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))))
(ert-deftest test-org-context/image-simple-plist ()
  "Test exporting simple image."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacefigure")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "TestLocation")))
     "[[:space:]]*"
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth")))
     "[[:space:]]*"
     (regexp-quote "\\stopplacefigure"))
    (context-test-with-temp-text
     "[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-height nil
         :context-image-default-width ((t . "TestWidth"))
         :context-float-default-placement "TestLocation")))))))
(ert-deftest test-org-context/image-table ()
  "Test image in table."
  (let ((document
         (context-test-with-temp-text
          "| [[./images/cat.jpg]] |"
          (org-trim
           (org-export-as
            'context nil nil t
            '(:context-preset "empty"))))))
    (should
     (string-match-p
      (regexp-quote "\\externalfigure[./images/cat.jpg]")
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\startplacefigure")
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\stopplacefigure")
      document))))
(ert-deftest test-org-context/image-caption ()
  "Test image with caption."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacefigure")
     (context-test-build-ConTeXt-argument-regex
      '(("title" . "A cat"))
      nil nil t))
    (context-test-with-temp-text
     "#+CAPTION: A cat\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-wrap ()
  "Test image with wrap position."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacefigure")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "here,left"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float wrap\n[[./images/cat.jpg]]"
     (let ((org-context-float-default-placement "left"))
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/image-sideways ()
  "Test image with sideways position."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacefigure")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("location" . "page,90,"))))
    (context-test-with-temp-customization-value
     org-context-float-default-placement
     ""
     (context-test-with-temp-text
      "#+ATTR_CONTEXT: :float sideways\n[[./images/cat.jpg]]"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/image-multicolumn ()
  "Test image with multicolumn position."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      (list (cons "width" (regexp-quote "\\dimexpr\\makeupwidth - 1em\\relax")))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float multicolumn\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-placement-doc ()
  "Test image with specific placement."
  (should
   (string-match-p
    (regexp-quote "\\startplacefigure[location={backspace}]")
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :placement backspace\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-placement-cust ()
  "Test image with specific placement."
  (should
   (string-match-p
    (regexp-quote "\\startplacefigure[location={backspace}]")
    (context-test-with-temp-customization-value
     org-context-float-default-placement
     "backspace"
     (context-test-with-temp-text
      "[[./images/cat.jpg]]"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/image-placement-plist ()
  "Test image with specific placement."
  (should
   (string-match-p
    (regexp-quote "\\startplacefigure[location={backspace}]")
    (context-test-with-temp-text
     "[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-float-default-placement "backspace")))))))
(ert-deftest test-org-context/image-width-local ()
  "Test image with specified width."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "2in"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :width 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-height-local ()
  "Test image with specified height."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("height" . "2in"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :height 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-width-height-cust ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth")
        ("height" . "TestHeight"))))
    (context-test-with-temp-customization-value
     org-context-image-default-width
     '((t . "TestWidth"))
     (context-test-with-temp-customization-value
      org-context-image-default-height '((t . "TestHeight"))
      (context-test-with-temp-text
       "[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/image-width-height-plist ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth")
        ("height" . "TestHeight"))))
    (context-test-with-temp-text
     "[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-height ((t . "TestHeight"))
         :context-image-default-width ((t .  "TestWidth")))))))))
(ert-deftest test-org-context/image-width-wrap ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float wrap
[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-width ((wrap .  "TestWidth")))))))))
(ert-deftest test-org-context/image-width-sideways ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float sideways
[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-height nil
         :context-image-default-width ((t . "Foo")
                                       (sideways .  "TestWidth")))))))))
(ert-deftest test-org-context/image-width-multicolumn ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("width" . "TestWidth"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :float multicolumn
[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-height nil
         :context-image-default-width ((t . "Foo")
                                       (multicolumn .  "TestWidth")))))))))
(ert-deftest test-org-context/image-height-cust ()
  "Test image height set in customization."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("height" . "TestHeight"))))
    (context-test-with-temp-customization-value
     org-context-image-default-width
     '((t . "TestWidth"))
     (context-test-with-temp-customization-value
      org-context-image-default-height '((t . "TestHeight"))
      (context-test-with-temp-text
       "#+ATTR_CONTEXT: :width nil
[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))))
(ert-deftest test-org-context/image-rules-cust ()
  "Test image rules set in customization."
  (should
   (string-match-p
    (regexp-quote "\\externalfigure[./images/cat.foo]")
    (context-test-with-temp-customization-value
     org-context-inline-image-rules
     `(("file" . ,(rx "."
                   "foo"
                   eos)))
     ""
     (context-test-with-temp-text
      "[[./images/cat.foo]]"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/image-rules-plist ()
  "Test image rules set in customization."
  (should
   (string-match-p
    (regexp-quote "\\externalfigure[./images/cat.foo]")
    (context-test-with-temp-customization-value
     org-context-image-default-height '((t . "TestHeight"))
     (context-test-with-temp-text
      "[[./images/cat.foo]]"
      (org-trim
       (org-export-as
        'context nil nil t
        (list :context-inline-image-rules
              `(("file" . ,(rx "." "foo" eos)))))))))))
(ert-deftest test-org-context/image-options-cust ()
  "Test image options set in customization."
  (should
   (string-match-p
   (concat
    (regexp-quote "\\externalfigure[./images/cat.jpg]")
    "[[:space:]]*"
    (regexp-quote "[")
    "[^]]*"
    (regexp-quote "TestOption=Foo")
    "[^]]*"
    (regexp-quote "]"))
    (context-test-with-temp-customization-value
     org-context-image-default-option "TestOption=Foo"
     (context-test-with-temp-text
      "[[./images/cat.jpg]]"
      (org-trim
       (org-export-as
        'context nil nil t)))))))
(ert-deftest test-org-context/image-options-plist ()
  "Test image options set in customization."
  (should
   (string-match-p
   (concat
    (regexp-quote "\\externalfigure[./images/cat.jpg]")
    "[[:space:]]*"
    (regexp-quote "[")
    "[^]]*"
    (regexp-quote "TestOption=Foo")
    "[^]]*"
    (regexp-quote "]"))
    (context-test-with-temp-text
      "[[./images/cat.jpg]]"
      (org-trim
       (org-export-as
        'context nil nil t
        '(:context-image-default-option "TestOption=Foo")))))))
(ert-deftest test-org-context/image-options-opt ()
  "Test image options set in customization."
  (should
   (string-match-p
   (concat
    (regexp-quote "\\externalfigure[./images/cat.jpg]")
    "[[:space:]]*"
    (regexp-quote "[")
    "[^]]*"
    (regexp-quote "TestOption=Foo")
    "[^]]*"
    (regexp-quote "]"))
    (context-test-with-temp-text
      "#+ATTR_CONTEXT: :options TestOption=Foo
[[./images/cat.jpg]]"
      (org-trim (org-export-as 'context nil nil t))))))
(ert-deftest test-org-context/image-options-bracketed ()
  "Test image options set in customization."
  (should
   (string-match-p
   (concat
    (regexp-quote "\\externalfigure[./images/cat.jpg]")
    "[[:space:]]*"
    (regexp-quote "[")
    "[^]]*"
    (regexp-quote "TestOption=Foo")
    "[^]]*"
    (regexp-quote "]"))
    (context-test-with-temp-text
      "#+ATTR_CONTEXT: :options [TestOption=Foo]
[[./images/cat.jpg]]"
      (org-trim (org-export-as 'context nil nil t))))))
(ert-deftest test-org-context/image-scale-local ()
  "Test image with specified scale."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("scale" . "2"))))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :scale 2\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/image-width-height-scale-plist ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("scale" . "TestScale"))))
    (context-test-with-temp-text
     "[[./images/cat.jpg]]"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-image-default-scale ((t . "TestScale"))
         :context-image-default-height ((t . "TestHeight"))
         :context-image-default-width ((t .  "TestWidth")))))))))
(ert-deftest test-org-context/image-width-height-scale-cust ()
  "Test image options set with customization values."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\externalfigure[./images/cat.jpg]")
     (context-test-build-ConTeXt-argument-regex
      '(("scale" . "TestScale"))))
    (context-test-with-temp-customization-value
     org-context-image-default-scale
     '((t . "TestScale"))
     (context-test-with-temp-customization-value
     org-context-image-default-width
     '((t . "TestWidth"))
     (context-test-with-temp-customization-value
      org-context-image-default-height '((t . "TestHeight"))
      (context-test-with-temp-text
       "[[./images/cat.jpg]]"
       (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))))

;;; LaTeX Environment
(ert-deftest test-org-context/latex-environment-align-trivial ()
  "Test transcoding an align environment with no rows.."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplaceformula")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("reference" . "eq:org[0-9a-f]+")))
     "[[:space:]]*"
     (regexp-quote "\\startformula")
     "[[:space:]]*"
     (regexp-quote "foo")
     "[[:space:]]*"
     (regexp-quote "\\stopformula")
     "[[:space:]]*"
     (regexp-quote "\\stopplaceformula"))
    (context-test-with-temp-text
     "\\begin{align}
foo
\\end{align}"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/latex-environment-align-multiple-rows ()
  "Test transcoding an align environment with multiple rows."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplaceformula")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("reference" . "eq:org[0-9a-f]+")))
     "[[:space:]]*"
     (regexp-quote "\\startformula")
     "[[:space:]]*"
     (regexp-quote "\\startalign")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "foo")
     "[[:space:]]*"
     (regexp-quote "\\NR[+]")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "bar")
     "[[:space:]]*"
     (regexp-quote "\\NR[+]")
     "[[:space:]]*"
     (regexp-quote "\\stopalign")
     "[[:space:]]*"
     (regexp-quote "\\stopformula")
     "[[:space:]]*"
     (regexp-quote "\\stopplaceformula"))
    (context-test-with-temp-text
     "\\begin{align}
foo\\\\
bar
\\end{align}"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/latex-environment-align-multiple-cols ()
  "Test transcoding an align environment with multiple rows and columns."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplaceformula")
     "[[:space:]]*"
     (context-test-build-ConTeXt-argument-regex
      '(("reference" . "eq:org[0-9a-f]+")))
     "[[:space:]]*"
     (regexp-quote "\\startformula")
     "[[:space:]]*"
     (regexp-quote "\\startalign")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "foo")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "bar")
     "[[:space:]]*"
     (regexp-quote "\\NR[+]")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "baz")
     "[[:space:]]*"
     (regexp-quote "\\NC")
     "[[:space:]]*"
     (regexp-quote "buz")
     "[[:space:]]*"
     (regexp-quote "\\NR[+]")
     "[[:space:]]*"
     (regexp-quote "\\stopalign")
     "[[:space:]]*"
     (regexp-quote "\\stopformula")
     "[[:space:]]*"
     (regexp-quote "\\stopplaceformula"))
    (context-test-with-temp-text
     "\\begin{align}
foo & bar \\\\
baz & buz
\\end{align}"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))
(ert-deftest test-org-context/latex-environment-align*-trivial ()
  "Test transcoding an align* environment with no rows.."
  (let ((document (context-test-with-temp-text
                   "\\begin{align*}
foo
\\end{align*}"
                   (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       (regexp-quote "foo")
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\startplaceformula")
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\stopplaceformula")
      document))))
(ert-deftest test-org-context/latex-environment-align*-multiple-rows ()
  "Test transcoding an align* environment with multiple rows."
  (let ((document (context-test-with-temp-text
                   "\\begin{align*}
foo\\\\
bar
\\end{align*}"
                   (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       (regexp-quote "\\startalign")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "foo")
       "[[:space:]]*"
       (regexp-quote "\\NR[+]")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "bar")
       "[[:space:]]*"
       (regexp-quote "\\NR[+]")
       "[[:space:]]*"
       (regexp-quote "\\stopalign")
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\startplaceformula")
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\stopplaceformula")
      document))))
(ert-deftest test-org-context/latex-environment-align*-multiple-cols ()
  "Test transcoding an align* environment with multiple rows and columns."
  (let ((document (context-test-with-temp-text
                   "\\begin{align*}
foo & bar \\\\
baz & buz
\\end{align*}"
                   (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\startformula")
       "[[:space:]]*"
       (regexp-quote "\\startalign")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "foo")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "bar")
       "[[:space:]]*"
       (regexp-quote "\\NR[+]")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "baz")
       "[[:space:]]*"
       (regexp-quote "\\NC")
       "[[:space:]]*"
       (regexp-quote "buz")
       "[[:space:]]*"
       (regexp-quote "\\NR[+]")
       "[[:space:]]*"
       (regexp-quote "\\stopalign")
       "[[:space:]]*"
       (regexp-quote "\\stopformula"))
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\startplaceformula")
      document))
    (should-not
     (string-match-p
      (regexp-quote "\\stopplaceformula")
      document))))

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
    "attachment:projects.org"
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

;;; Special Blocks
(ert-deftest test-org-context/special-block-notitle ()
  "Test special block exports."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startFOO")
     "[[:space:]]*"
     (regexp-quote "foo bar baz")
     "[[:space:]]*"
     (regexp-quote "\\stopFOO"))
    (context-test-with-temp-text
     "#+BEGIN_FOO
foo bar baz
#+END_FOO"
     (org-export-as
      'context nil nil t '(:context-preset "empty"))))) )
(ert-deftest test-org-context/special-block-meta ()
  "Test special block exports."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startFOO[foo=bar]")
     "[[:space:]]*"
     (regexp-quote "foo bar baz")
     "[[:space:]]*"
     (regexp-quote "\\stopFOO"))
    (context-test-with-temp-text
     "#+ATTR_CONTEXT: :options foo=bar
#+BEGIN_FOO
foo bar baz
#+END_FOO"
     (org-export-as
      'context nil nil t '(:context-preset "empty"))))))

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
(ert-deftest test-org-context/table-loc-cust-plist ()
  "Table location enabled in customization."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[")
     "[^]]*"
     (regexp-quote "location={bar}"))
    (context-test-with-temp-text
       " | foo |"
       (org-trim
        (org-export-as
         'context nil nil t
         '(:context-preset "empty"
           :context-table-location "bar"
           :context-table-split "")))))))
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
(ert-deftest test-org-context/table-head-repeat-cust-plist ()
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
    (context-test-with-temp-text
      "| foo |
|-----|
| bar |"
      (org-trim
       (org-export-as
        'context nil nil t
        '(:context-preset "empty"
          :context-table-header "repeat")))))))
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
(ert-deftest test-org-context/table-foot-cust-plist ()
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
    (context-test-with-temp-text
     "| foo |
|-----|
| bar |
|-----|
| baz |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-footer "repeat")))))))
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
(ert-deftest test-org-context/table-option-cust-plist ()
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
    (context-test-with-temp-text
     "| foo |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-option "bar")))))))
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
(ert-deftest test-org-context/table-style-plist ()
  "Test table style configured in document keywords."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (context-test-with-temp-text
     "#+TABLE_STYLE: bar

| foo |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-style "baz")))))))
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
(ert-deftest test-org-context/table-float-plist-some ()
  "Test table float style configured in customization."
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]")
     "[\s\n]*"
     (regexp-quote "\\startxtable"))
    (context-test-with-temp-text
     "| foo |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-float-style "bar")))))))
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
(ert-deftest test-org-context/table-split-plist-some ()
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
    (context-test-with-temp-text
     "| foo |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-split "yes")))))))
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
      nil nil t))
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
      '(("split" . "no"))
      nil nil t))
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
;;;; Table cell styles
(setq test-org-context--basic-table
"#+ATTR_CONTEXT: :footer repeat
| N   | N^2 | N^3 | N^4 | sqrt(n) | sqrt[4](N) |
| foo | bar | baz | baz | buz     | foo        |
| foo | bar | baz | baz | buz     | foo        |
| foo | bar | baz | baz | buz     | foo        |
|-----+-----+-----+-----+---------+------------|
| /   | <   |     | >   | <       | >          |
| 1   | 1   | 1   | 1   | 1       | 1          |
| 1   | 1   | 1   | 1   | 1       | 1          |
| 1   | 1   | 1   | 1   | 1       | 1          |
|-----+-----+-----+-----+---------+------------|
| ab  | de  | ag  | ce  | fe      | dd         |
| ab  | de  | ag  | ce  | fe      | dd         |
|-----+-----+-----+-----+---------+------------|
| ab  | cd  | ef  | gh  | ij      | kl         |
| 1   | 1   | 1   | 1   | 1       | 1          |
| 1   | 1   | 1   | 1   | 1       | 1          |
| ab  | cd  | ef  | gh  | ij      | kl         |
")
(ert-deftest test-org-context/table-body-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablebody")
     "[[:space:]]*"
     (regexp-quote "[TestBody]"))
    (context-test-with-temp-customization-value
     org-context-table-body-style
     "TestBody"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-body-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablebody")
     "[[:space:]]*"
     (regexp-quote "[TestBody]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim (org-export-as
                'context nil nil t
                '(:context-preset "empty"
                  :context-table-body-style "TestBody")))))))
(ert-deftest test-org-context/table-bottomleft-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestBottomLeft]"))
    (context-test-with-temp-customization-value
     org-context-table-bottomleft-style
     "TestBottomLeft"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-bottomleft-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestBottomLeft]"))
    (context-test-with-temp-text
      test-org-context--basic-table
      (org-trim
       (org-export-as
        'context nil nil t
        '(:context-preset "empty"
          :context-table-bottomleft-style "TestBottomLeft")))))))
(ert-deftest test-org-context/table-bottomright-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestBottomRight]"))
    (context-test-with-temp-customization-value
     org-context-table-bottomright-style
     "TestBottomRight"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-bottomright-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestBottomRight]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-bottomright-style "TestBottomRight")))))))
(ert-deftest test-org-context/table-bottomrow-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestBottomRow]"))
    (context-test-with-temp-customization-value
     org-context-table-bottomrow-style
     "TestBottomRow"
     (context-test-with-temp-text
      "| abc |
| 123 |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-bottomrow-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestBottomRow]"))
    (context-test-with-temp-text
     "| abc |
| 123 |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-bottomrow-style "TestBottomRow")))))))
(ert-deftest test-org-context/table-colgroup-end-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestColgroupEnd]"))
    (context-test-with-temp-customization-value
     org-context-table-colgroup-end-style
     "TestColgroupEnd"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-colgroup-end-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestColgroupEnd]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-colgroup-end-style "TestColgroupEnd")))))))
(ert-deftest test-org-context/table-colgroup-start-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestColgroupStart]"))
    (context-test-with-temp-customization-value
     org-context-table-colgroup-start-style
     "TestColgroupStart"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-colgroup-start-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestColgroupStart]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-colgroup-start-style "TestColgroupStart")))))))
(ert-deftest test-org-context/table-footer-bottom-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterBottomStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-footer-bottom-style
     "TestFooterBottomStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-footer-bottom-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterBottomStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-footer-bottom-style "TestFooterBottomStyle")))))))
(ert-deftest test-org-context/table-footer-mid-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterMidStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-footer-mid-style
     "TestFooterMidStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-footer-mid-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterMidStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-footer-mid-style "TestFooterMidStyle")))))))
(ert-deftest test-org-context/table-footer-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablefoot")
     "[[:space:]]*"
     (regexp-quote "[TestFooterStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-footer-style
     "TestFooterStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-footer-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablefoot")
     "[[:space:]]*"
     (regexp-quote "[TestFooterStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-footer-style "TestFooterStyle")))))))
(ert-deftest test-org-context/table-footer-top-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterTopStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-footer-top-style
     "TestFooterTopStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-footer-top-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestFooterTopStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-footer-top-style "TestFooterTopStyle")))))))
(ert-deftest test-org-context/table-header-bottom-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderBottomStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-header-bottom-style
     "TestHeaderBottomStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-header-bottom-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderBottomStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-header-bottom-style "TestHeaderBottomStyle")))))))
(ert-deftest test-org-context/table-header-mid-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderMidStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-header-mid-style
     "TestHeaderMidStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-header-mid-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderMidStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-header-mid-style "TestHeaderMidStyle")))))))
(ert-deftest test-org-context/table-header-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablehead")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-header-style
     "TestHeaderStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-header-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtablehead")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-header-style "TestHeaderStyle")))))))
(ert-deftest test-org-context/table-header-top-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderTopStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-header-top-style
     "TestHeaderTopStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-header-top-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestHeaderTopStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-header-top-style "TestHeaderTopStyle")))))))
(ert-deftest test-org-context/table-leftcolstyle-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestLeftcolStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-leftcol-style
     "TestLeftcolStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-leftcolstyle-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestLeftcolStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-leftcol-style "TestLeftcolStyle")))))))
(ert-deftest test-org-context/table-rightcol-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestRightcolStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-rightcol-style
     "TestRightcolStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-rightcol-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestRightcolStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-rightcol-style "TestRightcolStyle")))))))
(ert-deftest test-org-context/table-rowgroup-start-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestRowgroupStartStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-rowgroup-start-style
     "TestRowgroupStartStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-rowgroup-start-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestRowgroupStartStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-rowgroup-start-style "TestRowgroupStartStyle")))))))
(ert-deftest test-org-context/table-rowgroup-end-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestRowgroupEndStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-rowgroup-end-style
     "TestRowgroupEndStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-rowgroup-end-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestRowgroupEndStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-rowgroup-end-style "TestRowgroupEndStyle")))))))
(ert-deftest test-org-context/table-topleft-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestTopleftStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-topleft-style
     "TestTopleftStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-topleft-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestTopleftStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-topleft-style "TestTopleftStyle")))))))
(ert-deftest test-org-context/table-topright-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestToprightStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-topright-style
     "TestToprightStyle"
     (context-test-with-temp-text test-org-context--basic-table
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-topright-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxcell")
     "[[:space:]]*"
     (regexp-quote "[TestToprightStyle]"))
    (context-test-with-temp-text
     test-org-context--basic-table
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-topright-style "TestToprightStyle")))))))
(ert-deftest test-org-context/table-toprow-style-cust ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestToprowStyle]"))
    (context-test-with-temp-customization-value
     org-context-table-toprow-style
     "TestToprowStyle"
     (context-test-with-temp-text
      "| abc |
| 123 |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))))
(ert-deftest test-org-context/table-toprow-style-plist ()
  "Test table body style override"
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxrow")
     "[[:space:]]*"
     (regexp-quote "[TestToprowStyle]"))
    (context-test-with-temp-text
     "| abc |
| 123 |"
     (org-trim
      (org-export-as
       'context nil nil t
       '(:context-preset "empty"
         :context-table-toprow-style "TestToprowStyle")))))))
;;;; Table approximate layout
(ert-deftest test-org-context/table-layout ()
  "Test complete table structure."
  (let ((content
         (context-test-with-temp-text
          test-org-context--basic-table
          (org-trim
           (org-export-as
            'context nil nil t
            '(:context-preset "empty"
              :context-table-body-style "TestBodyStyle"
              :context-table-bottomleft-style "TestBottomleftStyle"
              :context-table-bottomright-style  "TestBottomrightStyle"
              :context-table-bottomrow-style "TestBottomRowStyle"
              :context-table-colgroup-end-style  "TestColgroupEndStyle"
              :context-table-colgroup-start-style "TestColgroupStartStyle"
              :context-table-footer-bottom-style "TestFooterBottomStyle"
              :context-table-footer-mid-style "TestFooterMidStyle"
              :context-table-footer-style "TestFooterStyle"
              :context-table-footer-top-style "TestFooterTopStyle"
              :context-table-header-bottom-style "TestHeaderBottomStyle"
              :context-table-header-mid-style "TestHeaderMidStyle"
              :context-table-header-style "TestHeaderStyle"
              :context-table-header-top-style "TestHeaderTopStyle"
              :context-table-leftcol-style "TestLeftcolStyle"
              :context-table-rightcol-style "TestRightcolStyle"
              :context-table-rowgroup-end-style "TestRowgroupEndStyle"
              :context-table-rowgroup-start-style "TestRowgroupStartStyle"
              :context-table-topleft-style "TestTopleftStyle"
              :context-table-topright-style "TestToprightStyle"
              :context-table-toprow-style "TestToprowStyle"))))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\startplacetable")
       "[[:space:]]*"
       (regexp-quote "[")
       "[^]]*"
       (regexp-quote "]")
       "[[:space:]]*"
       (regexp-quote "\\startxtable")
       "[[:space:]]*"
       (regexp-quote "[")
       "[^]]*"
       (regexp-quote "]")
       "[[:space:]]*"
       (regexp-quote "\\startxtablehead")
       "[[:space:]]*"
       (regexp-quote "[TestHeaderStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestHeaderTopStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestTopleftStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupEndStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestToprightStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       "\\("
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestHeaderMidStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestLeftcolStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupEndStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestRightcolStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\stopxrow")
       "\\)+"
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestHeaderBottomStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestLeftcolStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupEndStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestRightcolStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\stopxtablehead")
       "[[:space:]]*"

       (regexp-quote "\\startxtablebody")
       "[[:space:]]*"
       (regexp-quote "[TestBodyStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestRowgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestRowgroupEndStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\stopxtablebody")
       "[[:space:]]*"

       (regexp-quote "\\startxtablefoot")
       "[[:space:]]*"
       (regexp-quote "[TestFooterStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestFooterTopStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestFooterMidStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"

       "\\(.\\|\n\\)*"
       (regexp-quote "\\startxrow")
       "[[:space:]]*"
       (regexp-quote "[TestFooterBottomStyle]")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestBottomleftStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupEndStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestColgroupStartStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\startxcell")
       "[[:space:]]*"
       (regexp-quote "[TestBottomrightStyle]")
       "\\(.\\|\n\\)*"
       (regexp-quote "\\stopxcell")
       "[[:space:]]*"
       (regexp-quote "\\stopxrow")
       "[[:space:]]*"
       (regexp-quote "\\stopxtablefoot")
       "[[:space:]]*"
       (regexp-quote "\\stopxtable")
       "[[:space:]]*"
       (regexp-quote "\\stopplacetable"))
      content))
    (should-not
     (string-match-p
      (concat
       (regexp-quote "\\startxtablebody")
       "\\(.\\|\n\\)"
       (regexp-quote "\\startxtablebody"))
      content))))
;;; Radio Targets
(ert-deftest testo-org-context/radio-target ()
  "Test radio targets."
  (let* ((content
          (context-test-with-temp-text
           "<<<target>>>
foo
target"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))
         (reference
          (progn
            (string-match
             (concat
              (regexp-quote "\\reference[")
              "\\(org[0-9a-f]+\\)"
              (regexp-quote "]"))
             content)
            (match-string 1 content))))
    (should
     (string-match-p
      (concat
       (regexp-quote "\\reference[")
       reference
       (regexp-quote "]")
       (regexp-quote "{target}")
       "[[:space:]]*target"
       "[[:space:]]*"
       "foo"
       "[[:space:]]*"
       (regexp-quote "\\goto{target}[")
       reference
       (regexp-quote "]"))
      content))))
;;; Statistics Cookie
(ert-deftest testo-org-context/statistics-cookie ()
  "Test statistics cookie."
  (should
   (equal
    "[123\\%]"
    (context-test-with-temp-text
     "[123%]"
     (org-trim
      (org-export-as 'context nil nil t
                     '(:context-preset "empty")))))))
;;; Target
(ert-deftest testo-org-context/target ()
  "Test targets."
  (let* ((document
          (context-test-with-temp-text
           "<<target>>
foo bar [[target]]"
           (org-trim
            (org-export-as 'context nil nil t
                           '(:context-preset "empty")))))
         (match
          (string-match
           (concat
            (regexp-quote "\\reference[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]{target}")
            "[[:space:]]*"
            (regexp-quote "foo bar \\goto{\\ref[default][")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]}[")
            "\\(org[0-9a-f]+\\)"
            (regexp-quote "]"))
           document)))
    (should match)
    (should
     (equal (match-string 1 document)
            (match-string 2 document)))
    (should
     (equal (match-string 1 document)
            (match-string 3 document)))))
;;; Document Structure
(ert-deftest test-org-context/document-structure-1 ()
  "Test that document structure matches what we expect generally."
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
    (should (string-match-p "A Test Snippet" document))
    (should
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
      document))))
(ert-deftest test-org-context/document-structure-2 ()
  "Test that document structure matches what we expect generally."
  (let ((document
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
           (org-export-as
            'context nil nil nil
            '(:context-snippets (("TestSnippet" . "A Test Snippet"))
              :context-presets (("TestPreset" .
                                 (:literal "Test Literal"
                                  :template "TestFrame"
                                  :snippets ("TestSnippet"))))
              :context-preset "TestPreset"
              :context-inner-templates (("TestFrame" . "
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
              :context-texinfo-indices (("foo" . (:keyword "FOOINDEX" :command "TestFooIndex")))))))))
    (should
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
      document))
     (should (string-match-p "A Test Snippet" document))))
(ert-deftest test-org-context/document-structure-snippets-doc ()
  "Test that snippets are included in the document."
  (let ((document
         (context-test-with-temp-text
          "#+CONTEXT_SNIPPET: TestSnippet "
          (org-trim
           (org-export-as
            'context nil nil nil
            '(:context-snippets (("TestSnippet" . "A Test Snippet"))))))))
    (should
     (string-match-p "A Test Snippet" document))))
;;; Table of Contents
(ert-deftest test-org-context/table-of-contents-off ()
  "Turning the table of contents off."
  (should-not
   (string-match-p
    (regexp-quote "\\placecontent")
    (context-test-with-temp-text
     "#+OPTIONS: toc:nil
* First

** Second"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/table-of-contents-on ()
  "Turning the table of contents on."
  (should
   (string-match-p
    (regexp-quote "\\placecontent")
    (context-test-with-temp-text
     "#+OPTIONS: toc:1
* First

** Second"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/table-of-contents-empty ()
  "Requirethat table of contents does not appear if there are no headlines."
  (should-not
   (string-match-p
    (regexp-quote "\\placecontent")
    (context-test-with-temp-text
     "#+OPTIONS: toc:1
foo bar baz"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(ert-deftest test-org-context/table-of-contents-levels ()
  "Test the correct number of levels appear in the table of contents."
  (should
   (string-match-p
    (regexp-quote "\\setupcombinedlist[content][list={section,subsection}]")
    (context-test-with-temp-text
     "#+OPTIONS: toc:2
* First

** Second

*** Third

**** Fourth"
     (org-export-as 'context nil nil nil '(:context-preset "empty")))))
  (should
   (string-match-p
    (regexp-quote "\\setupcombinedlist[content][list={section}]")
    (context-test-with-temp-text
     "#+OPTIONS: toc:1
* First

** Second

*** Third

**** Fourth"
     (org-export-as 'context nil nil nil '(:context-preset "empty"))))))
(provide 'test-ox-context)
;;; test-ox-context ends here
