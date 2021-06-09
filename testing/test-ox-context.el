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

(load "~/Projects/org-mode/testing/org-test.el")

;;; Protecting text
(ert-deftest test-org-context-clean-text ()
  "Test smart quotations"
  ;; Protected names
  (should
   (equal
    "testing names of \\TeX{}, \\LaTeX{}, and \\ConTeXt{}. tex, latex, and context shouldn't match."
    (org-test-with-temp-text "testing names of TeX, LaTeX, and ConTeXt. tex, latex, and context shouldn't match."
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Special characters in normal text
  (should
   (equal
    "`\\lettertilde !@\\#\\$\\%^&*()-_=+\\[\\{\\}\\]\\letterbackslash \\|;:'\",<.>/?"
    (org-test-with-temp-text "`~!@#$%^&*()-_=+[{}]\\|;:'\",<.>/?"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Smart Quotation
(ert-deftest test-org-context-smart-quote ()
  "Test smart quotations"
  ;; Basic
  (should
   (equal
    "Quote: \\quotation{Cogito ergo sum} - Descartes"
    (org-test-with-temp-text "Quote: \"Cogito ergo sum\" - Descartes"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Apostrophe
  (should
   (equal
    "Here's a quote: \\quotation{I think, therefore I am}"
    (org-test-with-temp-text "Here's a quote: \"I think, therefore I am\""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "Here's a nested quote: \\quotation{Descartes says \\quote{I think therefore I am}}"
    (org-test-with-temp-text "Here's a nested quote: \"Descartes says 'I think therefore I am'\""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Transcode Functions
(ert-deftest test-org-context-export-markup ()
  "Test inline formatting"
  ;; Bold
  (should
   (equal
    "\\bold{foo}"
    (org-test-with-temp-text "*foo*"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Italic
  (should
   (equal
    "\\italic{foo}"
    (org-test-with-temp-text "/foo/"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Underline
  (should
   (equal
    "\\underbar{foo}"
    (org-test-with-temp-text "/foo/"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Empty environment
(ert-deftest test-org-context-export-empty ()
  "Test exporting nothing. No content with empty preset puts nothing in body"
  (should
   (equal
    ""
    (org-test-with-temp-text ""
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty")))))))

;;; Headlines
(ert-deftest test-org-context-export-headline ()
  "Test `org-context-headline'"
  (let ((inside-headline-regex
         "title={\\\\OrgHeadline[\s\n]*\\[Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}")
        (headline-name "Headline 1"))

    ;; Trivial case
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format inside-headline-regex headline-name headline-name headline-name headline-name))
      (org-test-with-temp-text "* Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; Not-numbered
    (should
     (string-match-p
      (format "\\\\startsubject\\[%s\\][\s\n]*\\\\stopsubject"
              (format inside-headline-regex headline-name headline-name headline-name headline-name))
      (org-test-with-temp-text "* Headline 1\n:PROPERTIES:\n:UNNUMBERED:\n:END:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
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
      (org-test-with-temp-text "* Headline `~!@#$%^&*()_-=+[{}]|\\:;\"'<,>.?/"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; Nested headlines should wrap each other
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\startsubsection\\[%s\\][\s\n]*\\\\stopsubsection[\s\n]*\\\\stopsection"
              (format inside-headline-regex "Headline 1" "Headline 1" "Headline 1" "Headline 1")
              (format inside-headline-regex "Headline 2" "Headline 2" "Headline 2" "Headline 2"))
      (org-test-with-temp-text "* Headline 1\n** Headline 2"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; Alt text for title
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format inside-headline-regex headline-name "alt" "alt" "alt"))
      (org-test-with-temp-text "* Headline 1\n:PROPERTIES:\n:ALT_TITLE: alt\n:END:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; todo keywords
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\OrgHeadline[\s\n]*\\[Todo={TODO},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (org-test-with-temp-text "* TODO Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; Tags
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\OrgHeadline[\s\n]*\\[Text={%s},[\s\n]*Tags={tag1:tag2}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (org-test-with-temp-text "* Headline 1 :tag1:tag2:"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
    ;; Priority
    (should
     (string-match-p
      (format "\\\\startsection\\[%s\\][\s\n]*\\\\stopsection"
              (format "title={\\\\OrgHeadline[\s\n]*\\[Priority={A},[\s\n]*Text={%s}\\]},[\s\n]*list={%s},[\s\n]*marking={%s},[\s\n]*bookmark={%s},[\s\n]*reference={sec:org[a-f0-9]+}"
                      headline-name
                      headline-name
                      headline-name
                      headline-name))
      (org-test-with-temp-text "* [#A] Headline 1"
        (org-trim (org-export-as 'context nil nil t '(:context-preset "empty" :with-priority t))))))))

;;; Keywords
(ert-deftest test-org-context-export-keywords ()
  "Test keywords."
  (should
   (equal
    "abc"
    (org-test-with-temp-text "#+CONTEXT: abc"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\index{foo}"
    (org-test-with-temp-text "#+INDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgConcept{foo}"
    (org-test-with-temp-text "#+CINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgFunction{foo}"
    (org-test-with-temp-text "#+FINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgKeystroke{foo}"
    (org-test-with-temp-text "#+KINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgProgram{foo}"
    (org-test-with-temp-text "#+PINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgDataType{foo}"
    (org-test-with-temp-text "#+TINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\OrgVariable{foo}"
    (org-test-with-temp-text "#+VINDEX: foo"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelistoftables[criterium=all]"
    (org-test-with-temp-text "#+TOC: tables"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelistoffigures[criterium=all]"
    (org-test-with-temp-text "#+TOC: figures"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[formula][criterium=all]"
    (org-test-with-temp-text "#+TOC: equations"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeindex"
    (org-test-with-temp-text "#+TOC: definitions"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgConcept]"
    (org-test-with-temp-text "#+TOC: cp"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgFunction]"
    (org-test-with-temp-text "#+TOC: fn"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgKeystroke]"
    (org-test-with-temp-text "#+TOC: ky"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgProgram]"
    (org-test-with-temp-text "#+TOC: pg"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgDataType]"
    (org-test-with-temp-text "#+TOC: tp"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placeregister[OrgVariable]"
    (org-test-with-temp-text "#+TOC: vr"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[]"
    (org-test-with-temp-text "#+TOC: headlines"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[list={section,subject,subsection,subsubject}]"
    (org-test-with-temp-text "#+TOC: headlines 2"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placecontent[criterium=local,]"
    (org-test-with-temp-text "#+TOC: headlines local"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgListingEmpty][criterium=all]"
    (org-test-with-temp-text "#+TOC: listings"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgVerseEnumerateEmpty][criterium=all]"
    (org-test-with-temp-text "#+TOC: verses"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgBlockQuoteEnumEmpty][criterium=all]"
    (org-test-with-temp-text "#+TOC: quotes"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  (should
   (equal
    "\\placelist[OrgExampleEnumerationEmpty][criterium=all]"
    (org-test-with-temp-text "#+TOC: examples"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; TODO test bibliography
  )

;;; Images
(ert-deftest test-org-context-export-images ()
  "Test exporting links."
  ;; Simple case
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (org-test-with-temp-text "[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; With Caption
  (should
   (equal
    "\\startplacefigure[title={A cat},
   location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (org-test-with-temp-text "#+CAPTION: A cat\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; wrap
  (should
   (equal
    "\\startplacefigure[location={here,left}]
\\externalfigure[./images/cat.jpg][width={0.48\\hsize}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :float wrap\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; sideways
  (should
   (equal
    "\\startplacefigure[location={page,90}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :float sideways\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; multicolumn
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr\\makeupwidth - 1em\\relax}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :float multicolumn\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Placement
  (should
   (equal
    "\\startplacefigure[location={backspace}]
\\externalfigure[./images/cat.jpg][width={\\dimexpr \\hsize - 1em \\relax}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :placement backspace\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; width
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][width={2in}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :width 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; height
  (should
   (equal
    "\\startplacefigure[location={Here}]
\\externalfigure[./images/cat.jpg][height={2in}]
\\stopplacefigure"
    (org-test-with-temp-text "#+ATTR_CONTEXT: :height 2in\n[[./images/cat.jpg]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

;;; Links
(ert-deftest test-org-context-export-links ()
  "Test exporting links."
  ;; plain link
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (org-test-with-temp-text "[[http://orgmode.org]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; plain link
  (should
   (equal
    "\\goto{\\hyphenatedurl{http://orgmode.org}}[url(http://orgmode.org)]"
    (org-test-with-temp-text "http://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Link with description
  (should
   (equal
    "\\goto{Some Description}[url(http://orgmode.org)]"
    (org-test-with-temp-text "[[http://orgmode.org][Some Description]]"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; https
  (should
   (equal
    "\\goto{\\hyphenatedurl{https://orgmode.org}}[url(https://orgmode.org)]"
    (org-test-with-temp-text "https://orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; doi
  (should
   (equal
    "\\goto{\\hyphenatedurl{doi:10.1000/182}}[url(doi:10.1000/182)]"
    (org-test-with-temp-text "doi:10.1000/182"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; ftp
  (should
   (equal
    "\\goto{\\hyphenatedurl{ftp:orgmode.org}}[url(ftp:orgmode.org)]"
    (org-test-with-temp-text "ftp:orgmode.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; other
  (should
   (equal
    "\\goto{\\hyphenatedurl{projects.org}}[url(projects.org)]"
    (org-test-with-temp-text "attachment:projects.org"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

;;; Labels
(ert-deftest test-org-context-export-labels ()
  "When things should have labels or not."
  ;; No label on center
  (should
   (string-match-p
    (regexp-quote "\\startalignment")
    (org-test-with-temp-text "#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Center with name
  (should
   (string-match-p
    (concat
     (regexp-quote "\\reference[org")
     "[0-9a-f]+"
     (regexp-quote "]{foo}")
     (regexp-quote
     "\\startalignment"))
    (org-test-with-temp-text "#+NAME: foo\n#+BEGIN_CENTER\nfoo bar baz\n#+END_CENTER"
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
    (org-test-with-temp-text "| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
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
    (org-test-with-temp-text "#+TABLE_LOCATION: there\n
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
    (org-test-with-temp-text "#+ATTR_CONTEXT: :location there
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
    (org-test-with-temp-text "#+TABLE_LOCATION: here
#+ATTR_CONTEXT: :location there
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table header repeat by default
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
     (regexp-quote "\\startxtablehead")
    )
    (org-test-with-temp-text "| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
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
    (org-test-with-temp-text "#+TABLE_HEAD: nil
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
    (org-test-with-temp-text "#+ATTR_CONTEXT: :header repeat
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
    (org-test-with-temp-text "#+TABLE_HEAD: nil
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
    (org-test-with-temp-text "#+TABLE_HEAD: repeat
#+ATTR_CONTEXT: :header norepeat
| foo |
|-----|
| bar |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table footer not repeat by default
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
     (org-test-with-temp-text "| foo |
|-----|
| bar |
|-----|
| baz |"
       (org-trim
        (org-export-as 'context nil nil t
                       '(:context-preset "empty")))))))
  ;; Table footer not present by default
  (should
   (not
    (string-match-p
     (regexp-quote "\\startxtablefoot")
     (org-test-with-temp-text "| foo |
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
    (org-test-with-temp-text "#+TABLE_FOOT: repeat
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
    (org-test-with-temp-text "#+ATTR_CONTEXT: :footer repeat
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
    (org-test-with-temp-text "#+TABLE_FOOT: nil
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
    (org-test-with-temp-text "#+TABLE_FOOT: repeat
#+ATTR_CONTEXT: :footer norepeat
| foo |
|-----|
| bar |
|-----|
| baz |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
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
    (org-test-with-temp-text "#+TABLE_OPTION: bar
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
    (org-test-with-temp-text "#+ATTR_CONTEXT: :option bar
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
    (org-test-with-temp-text "#+TABLE_OPTION: bar
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
    (org-test-with-temp-text "#+TABLE_STYLE: bar

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (org-test-with-temp-text "#+ATTR_CONTEXT: :table-style bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table style in both
  (should
   (string-match-p
   (concat
     (regexp-quote "\\startxtable")
     "[\s\n]*"
     (regexp-quote "[baz]"))
    (org-test-with-temp-text "#+TABLE_STYLE: bar
#+ATTR_CONTEXT: :table-style baz
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table float style in document keywords
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (org-test-with-temp-text "#+TABLE_FLOAT: bar

| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table float style in ATTR_CONTEXT
  (should
   (string-match-p
    (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[bar]"))
    (org-test-with-temp-text "#+ATTR_CONTEXT: :float-style bar
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table float style in both
  (should
   (string-match-p
   (concat
     (regexp-quote "\\startplacetable")
     "[\s\n]*"
     (regexp-quote "[baz]"))
    (org-test-with-temp-text "#+TABLE_FLOAT: bar
#+ATTR_CONTEXT: :float-style baz
| foo |"
     (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  ;; Table split by default
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
    (org-test-with-temp-text "| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
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
     (org-test-with-temp-text "#+TABLE_SPLIT: no

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
    (org-test-with-temp-text "#+TABLE_SPlIT: no

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
     (org-test-with-temp-text "#+ATTR_CONTEXT: :split no
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
    (org-test-with-temp-text "#+ATTR_CONTEXT: :split no
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
    (org-test-with-temp-text "#+TABLE_SPLIT: no
#+ATTR_CONTEXT: :split yes
| foo |"
      (org-trim (org-export-as 'context nil nil t '(:context-preset "empty"))))))
  )

(provide 'test-ox-context)
;;; test-ox-context ends here
