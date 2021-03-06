;;; ox-context --- Org exporter for ConTeXt
;; Copyright (C) 2021 Jason Ross
;; Author: Jason Ross <jasonross1024 at gmail dot com>

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; TODO Set indentation level for content
;;
;; TODO `org-context--add-reference' needs to be checked against all environments
;;
;; TODO abstract?
;;
;; TODO How should "hidden" enumerations be handled?
;; In the html exporter, explicitley enumerated items like verses
;; have a number that can clash with the number of the item.
;; https://orgmode.org/list/55ec0cbb-eebf-0d49-b182-372407c8c84c@gmail.com/T/#u
;;
;;; Commentary:

;;; Code:

;;; Dependencies
(require 'cl-lib)
(require 'ox)
(require 'seq)
(require 'subr-x)
(require 'context)

;;; Define Back-end
(org-export-define-backend 'context
  '((bold . org-context-bold)
    (center-block . org-context-center-block)
    (clock . org-context-clock)
    (code . org-context-code)
    (drawer . org-context-drawer)
    (dynamic-block . org-context-dynamic-block)
    (entity . org-context-entity)
    (example-block . org-context-example-block)
    (export-block . org-context-export-block)
    (export-snippet . org-context-export-snippet)
    (fixed-width . org-context-fixed-width)
    (footnote-reference . org-context-footnote-reference)
    (headline . org-context-headline)
    (horizontal-rule . org-context-horizontal-rule)
    (inline-src-block . org-context-inline-src-block)
    (inlinetask . org-context-inlinetask)
    (inner-template . org-context-inner-template)
    (italic . org-context-italic)
    (item . org-context-item)
    (keyword . org-context-keyword)
    (latex-environment . org-context-latex-environment)
    (latex-fragment . org-context-latex-fragment)
    (line-break . org-context-line-break)
    (link . org-context-link)
    (node-property . org-context-node-property)
    (paragraph . org-context-paragraph)
    (plain-list . org-context-plain-list)
    (plain-text . org-context-plain-text)
    (planning . org-context-planning)
    (property-drawer . org-context-property-drawer)
    (quote-block . org-context-quote-block)
    (radio-target . org-context-radio-target)
    (section . org-context-section)
    (special-block . org-context-special-block)
    (src-block . org-context-src-block)
    (statistics-cookie . org-context-statistics-cookie)
    (strike-through . org-context-strike-through)
    (subscript . org-context-subscript)
    (superscript . org-context-superscript)
    (table . org-context-table)
    (table-cell . org-context-table-cell)
    (table-row . org-context-table-row)
    (target . org-context-target)
    (template . org-context-template)
    (timestamp . org-context-timestamp)
    (underline . org-context-underline)
    (verbatim . org-context-verbatim)
    (verse-block . org-context-verse-block)
    ;;;; Pseudo objects and elements.
    (latex-math-block . org-context-math-block))
  :menu-entry
  '(?C "Export to ConTeXt"
       ((?c "As ConTeXt file" org-context-export-to-context)
        (?C "As ConTeXt buffer" org-context-export-as-context)
        (?p "As PDF file" org-context-export-to-pdf)
        (?o "As PDF file and open"
            (lambda (a s v b)
              (if a (org-context-export-to-pdf t s v b)
                (org-open-file (org-context-export-to-pdf s v b)))))))
 :filters-alist '((:filter-options . org-context-math-block-options-filter)
                  (:filter-paragraph . org-context-clean-invalid-line-breaks)
                  (:filter-parse-tree org-context-math-block-tree-filter)
                  (:filter-verse-block . org-context-clean-invalid-line-breaks))
 :options-alist '((:attention "ATTENTION" nil nil newline)
                  (:closing "CLOSING" nil org-context-closing parse)
                  (:context-block-source-environment nil nil org-context-block-source-environment)
                  (:context-blockquote-environment nil nil org-context-blockquote-environment)
                  (:context-bullet-off-command nil nil org-context-bullet-off-command)
                  (:context-bullet-on-command nil nil org-context-bullet-on-command)
                  (:context-bullet-trans-command nil nil org-context-bullet-trans-command)
                  (:context-clock-command nil nil org-context-clock-command)
                  (:context-description-command nil nil org-context-description-command)
                  (:context-drawer-command nil nil org-context-drawer-command)
                  (:context-enumerate-blockquote-empty-environment nil nil org-context-enumerate-blockquote-empty-environment)
                  (:context-enumerate-blockquote-environment nil nil org-context-enumerate-blockquote-environment)
                  (:context-enumerate-example-empty-environment nil nil org-context-enumerate-example-empty-environment)
                  (:context-enumerate-example-environment nil nil org-context-enumerate-example-environment)
                  (:context-enumerate-listing-empty-environment nil nil org-context-enumerate-listing-empty-environment)
                  (:context-enumerate-listing-environment nil nil org-context-enumerate-listing-environment)
                  (:context-enumerate-verse-empty-environment nil nil org-context-enumerate-verse-empty-environment)
                  (:context-enumerate-verse-environment nil nil org-context-enumerate-verse-environment)
                  (:context-example-environment nil nil org-context-example-environment)
                  (:context-export-quotes-alist nil nil org-context-export-quotes-alist)
                  (:context-fixed-environment nil nil org-context-fixed-environment)
                  (:context-float-default-placement nil nil org-context-float-default-placement)
                  (:context-format-clock-function nil nil org-context-format-clock-function)
                  (:context-format-drawer-function nil nil org-context-format-drawer-function)
                  (:context-format-headline-function nil nil org-context-format-headline-function)
                  (:context-format-inlinetask-function nil nil org-context-format-inlinetask-function)
                  (:context-format-timestamp-function nil nil org-context-format-timestamp-function)
                  (:context-header "CONTEXT_HEADER" nil nil newline)
                  (:context-header-extra "CONTEXT_HEADER_EXTRA" nil nil newline)
                  (:context-headline-command nil nil org-context-headline-command)
                  (:context-highlighted-langs nil nil org-context-highlighted-langs)
                  (:context-image-default-height nil nil org-context-image-default-height)
                  (:context-image-default-option nil nil org-context-image-default-option)
                  (:context-image-default-width nil nil org-context-image-default-width)
                  (:context-inline-image-rules nil nil org-context-inline-image-rules)
                  (:context-inline-source-environment nil nil org-context-inline-source-environment)
                  (:context-inlinetask-command nil nil org-context-inlinetask-command)
                  (:context-inner-template "CONTEXT_TEMPLATE" nil org-context-default-inner-template t)
                  (:context-inner-templates nil nil org-context-inner-templates-alist)
                  (:context-node-property-command nil nil org-context-node-property-command)
                  (:context-number-equations nil "numeq" org-context-number-equations)
                  (:context-planning-command nil nil org-context-planning-command)
                  (:context-preset "CONTEXT_PRESET" nil org-context-default-preset t)
                  (:context-presets nil nil org-context-presets-alist)
                  (:context-property-drawer-environment nil nil org-context-property-drawer-environment)
                  (:context-snippet "CONTEXT_SNIPPET" nil nil split)
                  (:context-snippets nil nil org-context-snippets-alist)
                  (:context-syntax-engine nil "syntax" org-context-syntax-engine)
                  (:context-table-body-style nil nil org-context-table-body-style)
                  (:context-table-bottomleft-style nil nil org-context-table-bottomleft-style)
                  (:context-table-bottomright-style nil nil org-context-table-bottomright-style)
                  (:context-table-bottomrow-style nil nil org-context-table-bottomrow-style)
                  (:context-table-colgroup-end-style nil nil org-context-table-colgroup-end-style)
                  (:context-table-colgroup-start-style nil nil org-context-table-colgroup-start-style)
                  (:context-table-footer-bottom-style nil nil org-context-table-footer-bottom-style)
                  (:context-table-footer-mid-style nil nil org-context-table-footer-mid-style)
                  (:context-table-footer-style nil nil org-context-table-footer-style)
                  (:context-table-footer-top-style nil nil org-context-table-footer-top-style)
                  (:context-table-header-bottom-style nil nil org-context-table-header-bottom-style)
                  (:context-table-header-mid-style nil nil org-context-table-header-mid-style)
                  (:context-table-header-style nil nil org-context-table-header-style)
                  (:context-table-header-top-style nil nil org-context-table-header-top-style)
                  (:context-table-leftcol-style nil nil org-context-table-leftcol-style)
                  (:context-table-rightcol-style nil nil org-context-table-rightcol-style)
                  (:context-table-rowgroup-end-style nil nil org-context-table-rowgroup-end-style)
                  (:context-table-rowgroup-start-style nil nil org-context-table-rowgroup-start-style)
                  (:context-table-topleft-style nil nil org-context-table-topleft-style)
                  (:context-table-topright-style nil nil org-context-table-topright-style)
                  (:context-table-toprow-style nil nil org-context-table-toprow-style)
                  (:context-table-use-footer nil "tablefoot" org-context-table-use-footer)
                  (:context-text-markup-alist nil nil org-context-text-markup-alist)
                  (:context-title-command nil nil org-context-title-command)
                  (:context-title-contents-command nil nil org-context-title-contents-command)
                  (:context-titlepage-environment nil nil org-context-titlepage-environment)
                  (:context-verse-environment nil nil org-context-verse-environment)
                  (:context-vim-langs-alist nil nil org-context-vim-langs-alist)
                  (:date "DATE" nil "\\currentdate" parse)
                  (:description "DESCRIPTION" nil nil parse)
                  (:from-address "FROM_ADDRESS" nil org-context-from-address newline)
                  (:from-logo "FROM_LOGO" nil org-context-from-logo)
                  (:keywords "KEYWORDS" nil nil parse)
                  (:location "LOCATION" nil org-context-location)
                  (:opening "OPENING" nil org-context-opening parse)
                  (:phone-number "PHONE_NUMBER" nil org-context-phone-number)
                  (:place "PLACE" nil org-context-place)
                  (:signature "SIGNATURE" nil org-context-closing parse)
                  (:subject "SUBJECT" nil nil parse)
                  (:subtitle "SUBTITLE" nil nil parse)
                  (:to-address "TO_ADDRESS" nil nil newline)
                  (:to-name "TO_NAME" nil nil newline)
                  (:url "URL" nil org-context-url)
                  (:with-backaddress nil "backaddress" org-context-use-backaddress)
                  (:with-email nil "email" org-context-use-email)
                  (:with-foldmarks nil "foldmarks" org-context-use-foldmarks)
                  (:with-from-logo nil "from-logo" org-context-use-from-logo)
                  (:with-phone nil "phone" org-context-use-phone)
                  (:with-place nil "place" org-context-use-place)
                  (:with-url nil "url" org-context-use-url)))

;;; Constants

;; TODO Many of these constants appear as options. Is this ideomatic? It makes
;; sense for creating derived templates but it incurs a performance penalty.
;; The advantage of having constants linked to the `options-alist' is that
;; they are exposed when creating a derived template but not exposed as part
;; of normal configuration.

(defconst org-context-export-quotes-alist
  '((primary-opening . "\\quotation{")
    (primary-closing . "}")
    (secondary-opening . "\\quote{")
    (secondary-closing . "}")
    (apostrophe . "'")))

(defconst org-context-latex-math-environments-re
  (format
   "\\`[ \t]*\\\\begin{%s\\*?}"
   (regexp-opt
    '("equation" "eqnarray" "math" "displaymath"
      "align"  "gather" "multline" "flalign"  "alignat"
      "xalignat" "xxalignat"
      "subequations"
      ;; breqn
      "dmath" "dseries" "dgroup" "darray"
      ;; empheq
      "empheq")))
  "Regexp of LaTeX math environments.")

(defconst org-context-table-body-style "OrgTableBody"
  "The default style name for the body row group in tables.")

(defconst org-context-table-bottomleft-style "OrgTableBottomLeftCell"
  "The default style name for the bottom left cell in tables.")

(defconst org-context-table-bottomright-style "OrgTableBottomRightCell"
  "The default style name for the bottom right cell in tables.")

(defconst org-context-table-bottomrow-style "OrgTableBottomRow"
  "The default style name for the bottom row in tables.")

(defconst org-context-table-colgroup-end-style "OrgTableColGroupEnd"
  "The default style name for columns ending column groups in tables.")

(defconst org-context-table-colgroup-start-style "OrgTableColGroupStart"
  "The default style name for columns starting column groups in tables.")

(defconst org-context-table-footer-bottom-style "OrgTableFooterBottom"
  "The default style name for the bottom row in the footer row group in tables.")

(defconst org-context-table-footer-mid-style "OrgTableFooterMid"
  "The default style name for footer rows where the footer is only one row.")

(defconst org-context-table-footer-style "OrgTableFooter"
  "The default style name for the footer row group in tables.")

(defconst org-context-table-footer-top-style "OrgTableFooterTop"
  "The default style name for the top row in the footer row group in tables.")

(defconst org-context-table-header-bottom-style "OrgTableHeaderBottom"
  "The default style name for the bottom row in the header row group in tables.")

(defconst org-context-table-header-mid-style "OrgTableHeaderMid"
  "The default style name for header rows where the header is only one row.")

(defconst org-context-table-header-style "OrgTableHeader"
  "The default style name for the header row group in tables.")

(defconst org-context-table-header-top-style "OrgTableHeaderTop"
  "The default style name for the top row in the header row group in tables.")

(defconst org-context-table-leftcol-style "OrgTableLeftCol"
  "The default style name for the left column in tables.")

(defconst org-context-table-rightcol-style "OrgTableRightCol"
  "The default style name for the right column in tables.")

(defconst org-context-table-rowgroup-start-style "OrgTableRowGroupStart"
  "The default style name for rows starting row groups in tables.")

(defconst org-context-table-rowgroup-end-style "OrgTableRowGroupEnd"
  "The default style name for rows ending row groups in tables.")

(defconst org-context-table-topleft-style "OrgTableTopLeftCell"
  "The default style name for the top left cell in tables.")

(defconst org-context-table-topright-style "OrgTableTopRightCell"
  "The default style name for the top right cell in tables.")

(defconst org-context-table-toprow-style "OrgTableTopRow"
  "The default style name for the top row in tables.")

;;; User configuration variables

(defgroup org-export-context nil
  "Options for exporting to ConTeXt."
  :tag "Org ConTeXt"
  :group 'org-export)

;;;; ConTeXt environments

;;;;; Element Environments

;; TODO Documentations for these environment defs
;; These environments wrap block elements to provide the core implementation.

(defcustom org-context-blockquote-environment
  '("OrgBlockQuote" . "\\definenarrower[OrgBlockQuote][left=2em,right=2em]")
  "The environment name of the block quote environment.

If nil, block quotes aren't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-example-environment
  '("OrgExample" . "\\definetyping[OrgExample][escape=yes]")
  "The environment name of the example environment.

Cons list of NAME, DEF. If NAME is nil, examples are delimited
in a typing environment. If DEF is nil, a default typing environment
called NAME is created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-fixed-environment
  '("OrgFixed" . "\\definetextbackground
  [OrgFixedBackground]
  [backgroundcolor=white,
   framecolor=black,
   location=always]
\\definetyping
  [OrgFixed]
  [before={\\starttextbackground[OrgFixedBackground]},
   after={\\stoptextbackground}]")
  "The environment name of the fixed-width environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-property-drawer-environment
  '("OrgPropDrawer" . "\\definestartstop[OrgPropDrawer]
  [before={\\startframedtext[frame=on,width=broad]},
   after={\\stopframedtext}]")
  "The environment name of the property drawer environment.

If nil, examples are enclosed in \"\\startframedtext\" / \"\\stopframedtext\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-inline-source-environment
  '("OrgInlineSrc" . "\\definetype[OrgInlineSrc]")
  "The environment name of the inline source environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-block-source-environment
  '("OrgBlkSrc" . "\\definetyping[OrgBlkSrc][escape=yes]")
  "The environment name of the block source environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-titlepage-environment
  '("OrgTitlePage" . "\\definestartstop[OrgTitlePage]")
  "The environment name that wraps title pages.

If nil, title pages aren't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-verse-environment
  '("OrgVerse" . "\\definelines[OrgVerse]")
  "The environment name of the verse environment.

If nil, verses aren't delimited."
  :group 'org-export-context
  :type '(cons string string))


;;;;; Enumeration environments

;; These environments wrap around element environments to allow them
;; to be enumerated in listings.

(defcustom org-context-enumerate-blockquote-empty-environment
  '("OrgBlockQuoteEnumEmpty" . "\\defineenumeration
  [OrgBlockQuoteEnumEmpty]
  [alternative=empty]")
  "The enumeration of the unlabelled blockquote environment.

Shares a counter with `org-context-enumerate-blockquote-environment'.

If nil, block quotes are not wrapped in an enumeration"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-blockquote-environment
  '("OrgBlockQuoteEnum" . "\\defineenumeration
  [OrgBlockQuoteEnum]
  [OrgBlockQuoteEnumEmpty]
  [title=yes,
   text=Quote,
   alternative=top]")
  "The enumeration of the blockquote environment.

Shares a counter with `org-context-enumerate-blockquote-empty-environment'.

If nil, block quotes are not wrapped in an enumeration"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-example-empty-environment
  '("OrgExampleEnumerationEmpty" . "\\defineenumeration
  [OrgExampleEnumerationEmpty]
  [alternative=empty]")
  "The enumeration of the unlabelled example environment.

Shares a counter with `org-context-enumerate-example-environment'.

If nil, examples are not wrapped in an enumeration"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-example-environment
  '("OrgExampleEnumeration" . "\\startuseMPgraphic{mpos:exampleback}
  begingroup; save lftbdry; save rtbdry; save lftx; save parframe;
  for i=1 upto nofmultipars :
      path lftbdry, rtbdry, parframe;
      numeric lftx;
      lftx := xpart(point 0 of leftboundary Page) - 32pt;
      rtbdry := rightboundary multipars[i];
      lftbdry := (lftx, ypart (point 0 of rtbdry))
          -- (lftx, ypart (point 1 of rtbdry));
      parframe := point 0 of lftbdry
          --point 1 of lftbdry
          --point 1 of rtbdry
          --point 0 of rtbdry
          --cycle;
      draw parframe;
  endfor;
\\stopuseMPgraphic
\\definetextbackground
  [OrgExampleBackground]
  [frame=off, mp={mpos:exampleback}, location=always]
\\defineenumeration
  [OrgExampleEnumeration]
  [OrgExampleEnumerationEmpty]
  [title=yes,
   text=Example,
   headalign=middle,
   style=\\tfx,
   titlestyle=\\tf,
   titleleft={\\bold{:}\\space},
   titleright=,
   titledistance=0em,
   before={\\starttextbackground[OrgExampleBackground]},
   after={\\stoptextbackground},
   alternative=top]")
  "The enumeration to wrap examples in.

Shares a counter with `org-context-enumerate-example-empty-environment'

If nil, examples are not wrapped in an enumeration"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-listing-empty-environment
  '("OrgListingEmpty" . "\\defineenumeration
  [OrgListingEmpty]
  [alternative=empty]")
  "The enumeration for unlabelled listings.

Shares a counter with `org-context-enumerate-listing-environment'.

If null, listings are not enumerated."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-listing-environment
  '("OrgListing" . "\\startuseMPgraphic{mpos:listingback}
  begingroup; save lftbdry; save rtbdry; save lftx; save parframe;
  for i=1 upto nofmultipars :
      path lftbdry, rtbdry, parframe;
      numeric lftx;
      lftx := xpart(point 0 of leftboundary Page) - 32pt;
      rtbdry := rightboundary multipars[i];
      lftbdry := (lftx, ypart (point 0 of rtbdry))
          -- (lftx, ypart (point 1 of rtbdry));
      parframe := point 0 of lftbdry
          --point 1 of lftbdry
          --point 1 of rtbdry
          --point 0 of rtbdry
          --cycle;
      draw parframe;
  endfor;
\\stopuseMPgraphic
\\definetextbackground
  [OrgListingBackground]
  [frame=off, mp={mpos:listingback}, location=always]
\\defineenumeration
  [OrgListing]
  [OrgListingEmpty]
  [title=yes,
   text=Listing,
   headalign=middle,
   titlestyle=\\tf,
   style=\\tfx,
   titleleft={\\bold{:}\\space},
   titleright=,
   titledistance=0em,
   before={\\starttextbackground[OrgListingBackground]},
   after={\\stoptextbackground},
   alternative=top]")
  "The enumeration for listings.

Shares a counter with `org-context-enumerate-listing-empty-environment'.

If null, listings are not enumerated."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-verse-empty-environment
  '("OrgVerseEnumerateEmpty" . "\\defineenumeration
  [OrgVerseEnumerateEmpty]
  [alternative=empty]")
  "The environment name that wraps verses to list them.

If nil, verses aren't enumerated."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-enumerate-verse-environment '("OrgVerseEnumerate" . "
\\defineenumeration
  [OrgVerseEnumerate]
  [OrgVerseEnumerateEmpty]
  [title=yes,
   text=Verse,
   alternative=top]")
  "The environment name that wraps verses to list them.

If nil, verses aren't enumerated."
  :group 'org-export-context
  :type '(cons string string))

;;;; ConTeXt commands

;; These commands provide names and implementations of Org elements
;; in ConTeXt.

(defcustom org-context-bullet-off-command
  '("OrgItemOff" . "\\define\\OrgItemOff{\\square}")
  "The name of the command that creates bullets for uncompleted items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-bullet-on-command
  '("OrgItemOn" . "\\define\\OrgItemOn{\\boxplus}")
  "The name of the command that creates bullets for completed items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-bullet-trans-command
  '("OrgItemTrans" . "\\define\\OrgItemTrans{\\boxtimes}")
  "The name of the command that creates bullets for partially completed items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-clock-command
  '("OrgClock" . "\\def\\OrgClock#1[#2]{%
  \\getparameters
    [OrgClock]
    [y=,
     m=,
     d=,
     H=,
     M=,
     I=,
     S=,
     #2]
  \\doifnot{\\OrgClocky}{}{%
    \\date[year=\\OrgClocky,month=\\OrgClockm,day=\\OrgClockd]
          [year, --, mm, --, dd]}%
  \\doifnot{\\OrgClockH}{}{T\\OrgClockH:\\OrgClockM%
  \\doifnot{\\OrgClockS}{}{:\\OrgClockS}}
}")
  "The name of the command that formats clocks.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-description-command
  '("OrgDesc" . "\\definedescription[OrgDesc]")
  "The command name to be used for Org description items.

If nil, \"\\description\" is used"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-drawer-command
  '("OrgDrawer" . "\\define[2]\\OrgDrawer{#2}")
  "The name of the command that formats drawers.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-headline-command
  '("OrgHeadline" . "\\def\\OrgHeadline#1[#2]{%
  \\getparameters
    [OrgHeadline]
    [Todo=,
     TodoType=,
     Priority=,
     Text=,
     Tags=,
     #2]
  \\doifnot{\\OrgHeadlineTodo}{}{{\\sansbold{\\smallcaps{\\OrgHeadlineTodo}}\\space}}%
  \\doifnot{\\OrgHeadlinePriority}{}{{\\inframed{\\OrgHeadlinePriority}\\space}}%
  \\OrgHeadlineText%
  \\doifnot{\\OrgHeadlineTags}{}{{\\hfill\\tt\\OrgHeadlineTags}}%
}")
  "The name of the command that formats headlines.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-inlinetask-command
  '("OrgInlineTask" . "\\def\\OrgInlineTask#1[#2]{%
  \\getparameters
    [OrgInlineTask]
    [Todo=,
     TodoType=,
     Priority=,
     Title=,
     Tags=,
     Contents=,
     #2]
  \\blank[big]
  \\startframedtext[align=normal, location=middle, width=0.6\\hsize]
  \\startalignment[middle]
  \\doifnot{\\OrgInlineTaskTodo}{}{\\sansbold{\\smallcaps{\\OrgInlineTaskTodo}} }%
  \\doifnot{\\OrgInlineTaskPriority}{}{\\inframed{\\OrgInlineTaskPriority} }%
  \\OrgInlineTaskTitle %
  \\doifnot{\\OrgInlineTaskTags}{}{{\\crlf\\tt\\OrgInlineTaskTags} }%
  \\crlf%
  \\textrule
  \\stopalignment
  \\OrgInlineTaskContents
  \\stopframedtext
  \\blank[big]
}")
  "The name of the command that formats inline tasks.

Receives the following keyword arguments:

Todo
TodoType
Priority
Title
Tags
Contents

If nil, returns a basic command with only the title and contents"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-node-property-command
  '("OrgNodeProp" . "\\def\\OrgNodeProp#1[#2]{%
  \\getparameters
    [OrgNodeProp]
    [key=,
     value=,
     #2]%
{\\tt \\OrgNodePropkey: \\OrgNodePropvalue}\\crlf}")
  "The name of the command that formats drawers.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-planning-command
  '("OrgPlanning" . "\\def\\OrgPlanning#1[#2]{%
  \\getparameters
    [OrgPlanning]
    [ClosedString=,
     ClosedTime=,
     DeadlineString=,
     DeadlineTime=,
     ScheduledString=,
     ScheduledTime=,
     #2]
  \\doifnot{\\OrgPlanningClosedString}{}{\\OrgPlanningClosedString\\space}
  \\doifnot{\\OrgPlanningClosedTime}{}{\\OrgPlanningClosedTime\\space}
  \\doifnot{\\OrgPlanningDeadlineString}{}{\\OrgPlanningDeadlineString\\space}
  \\doifnot{\\OrgPlanningDeadlineTime}{}{\\OrgPlanningDeadlineTime\\space}
  \\doifnot{\\OrgPlanningScheduledString}{}{\\OrgPlanningScheduledString\\space}
  \\doifnot{\\OrgPlanningScheduledTime}{}{\\OrgPlanningScheduledTime\\space}
  \\crlf
}")
  "The name of the command that formats planning items.

If nil, just returns a plain text time stamp and label.

Receives the following keyword arguments:

ClosedString
ClosedTime
DeadlineString
DeadlineTime
ScheduledString
ScheduledTime"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-title-command
  '("OrgMakeTitle" . "\\define\\OrgMakeTitle{}")
  "The name of the command that creates the document title.

If nil, the document title command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-title-contents-command
  '("OrgTitleContents" . "\\define\\OrgTitleContents{%
  {\\tfc Contents}
}")
  "The name of the command that titles the table of contents.

If nil, the table of contents title command isn't created."
  :group 'org-export-context
  :type '(cons string string))

;;;; Element Configuration

;; These settings configure elements in Org.


(defcustom org-context-closing ""
  "Letter's closing, as a string.
This option can also be set with the CLOSING keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-default-inner-template
  "empty"
  "The default inner template to use in documents.
See `org-context-inner-templates-alist'"
  :group 'org-export-context
  :type 'string)

(defcustom org-context-default-preset "empty"
  "A preamble with no style settings for the document elements."
  :group 'org-export-context
  :type '(string :tag "ConTeXt preset"))

(defcustom org-context-float-default-placement "left"
  "Default placement for floats."
  :group 'org-export-context
  :type '(choice
          (const "split")
          (const "always")
          (const "left")
          (const "right")
          (const "inner")
          (const "outer")
          (const "backspace")
          (const "cutspace")
          (const "inleft")
          (const "inright")
          (const "inmargin")
          (const "leftmargin")
          (const "rightmargin")
          (const "leftedge")
          (const "rightedge")
          (const "innermargin")
          (const "outermargin")
          (const "inneredge")
          (const "outeredge")
          (const "text")
          (const "opposite")
          (const "reset")
          (const "height")
          (const "depth")
          (const "line")
          (const "+line")
          (const "-line")
          (const "halfline")
          (const "grid")
          (const "high")
          (const "low")
          (const "fit")
          (const "90")
          (const "180")
          (const "270")
          (const "nonumber")
          (const "none")
          (const "local")
          (const "here")
          (const "force")
          (const "margin")
          (const "hang")
          (const "+hang")
          (const "-hang")
          (const "hanging")
          (const "tall")
          (const "both")
          (const "middle")
          (const "offset")
          (const "top")
          (const "bottom")
          (const "auto")
          (const "page")
          (const "leftpage")
          (const "rightpage")
          (const "somewhere")
          (const "effective")
          (const "header")
          (const "footer")
          (const "tblr")
          (const "lrtb")
          (const "tbrl")
          (const "rltb")
          (const "fxtb")
          (const "btlr")
          (const "lrbt")
          (const "btrl")
          (const "rlbt")
          (const "fxbt")
          (const "fixd"))
  :safe #'stringp)

(defcustom org-context-format-clock-function
  'org-context-format-clock-default-function
  "Function called to format a clock in ConTeXt code.

The function should take two parameters:

TIMESTAMP   the org timestamp
INFO        plist containing context information

The function should return the string to be exported."
  :group 'org-export-context
  :type 'function)

(defcustom org-context-format-drawer-function
  'org-context-format-drawer-default-function
  "Function called to format a drawer in ConTeXt code.

The function must accept two parameters:
  NAME      the drawer name, like \"LOGBOOK\"
  CONTENTS  the contents of the drawer.
  INFO      plist containing contextual information.

The function should return the string to be exported."
  :group 'org-export-context
  :type 'function)

(defcustom org-context-format-headline-function
  'org-context-format-headline-default-function
  "Function for formatting the headline's text.

This function will be called with six arguments:
TODO      the todo keyword (string or nil)
TODO-TYPE the type of todo (symbol: `todo', `done', nil)
PRIORITY  the priority of the headline (integer or nil)
TEXT      the main headline text (string)
TAGS      the tags (list of strings or nil)
INFO      the export options (plist)

The function result will be used in the section format string."
  :group 'org-export-latex
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'function)

(defcustom org-context-format-inlinetask-function
  'org-context-format-inlinetask-default-function
  "Function called to format an inlinetask in LaTeX code.

The function must accept seven parameters:
  TODO      the todo keyword (string or nil)
  TODO-TYPE the todo type (symbol: `todo', `done', nil)
  PRIORITY  the inlinetask priority (integer or nil)
  NAME      the inlinetask name (string)
  TAGS      the inlinetask tags (list of strings or nil)
  CONTENTS  the contents of the inlinetask (string or nil)
  INFO      the export options (plist)

The function should return the string to be exported."
  :group 'org-export-context
  :type 'function)

(defcustom org-context-format-timestamp-function
  'org-context-format-timestamp-default-function
  "Function called to format a timestamp in ConTeXt code.

The function should take one parameter, TIMESTAMP,
which is an Org timestamp object.

The function should return the string to be exported."
  :group 'org-export-context
  :type 'function)

(defcustom org-context-from-address ""
  "Sender's address, as a string.
This option can also be set with one or more FROM_ADDRESS
keywords."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-from-logo ""
  "Commands for inserting the sender's logo, e. g., \\externalfigure[logo.pdf].
This option can also be set with the FROM_LOGO keyword."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-highlighted-langs
  '(("metapost" "mp")
    ("c++" "cpp")
    ("c#" "cs"))
  "Alist mapping languages to their counterpart in ConTeXt.
ConTeXt only supports a couple of languages out-of-the-box
so this is a short list."
  :group 'org-export-context
  :type '(repeat
          (list
           (symbol :tag "Major mode      ")
           (symbol :tag "ConTeXt language"))))

(defcustom org-context-image-default-height ""
  "Default height for images."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-image-default-option ""
  "Default option for images."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-image-default-width "\\dimexpr \\hsize - 1em \\relax"
  "Default width for images."
  ;; TODO This ought to be a ConTeXt command
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-inline-image-rules
  `(("file" . ,(rx "."
                   (or "pdf" "jpeg" "jpg" "png" "ps" "eps" "tikz" "pgf" "svg")
                   eos)))
  "Rules characterizing image files that can be inlined into ConTeXt.

A rule consists in an association whose key is the type of link
to consider, and value is a regexp that will be matched against
link's path."
  :group 'org-export-latex
  :package-version '(Org . "9.4")
  :type '(alist :key-type (string :tag "Type")
                :value-type (regexp :tag "Path")))

(defcustom org-context-inner-templates-alist
  '(("empty" . "%t
%f
%c
%a
%b")
    ("article" . "\\startfrontmatter
%t
%f
\\stopfrontmatter

\\startbodymatter
%c
\\stopbodymatter

\\startappendices
%a
\\stopappendices

\\startbackmatter
%b
\\stopbackmatter")
    ("letter" . "\\startfrontmatter
%t
%f
\\stopfrontmatter
\\startbodymatter
%c
\\stopbodymatter

\\startappendices
%a
\\stopappendices

\\startbackmatter
%b
\\stopbackmatter")
    ("report" . "\\startfrontmatter
\\startstandardmakeup
%t
\\stopstandardmakeup
%f
\\stopfrontmatter

\\startbodymatter
%c
\\stopbodymatter

\\startappendices
%a
\\stopappendices
\\startbackmatter
%b
\\stopbackmatter"))
  "Alist of ConTeXt document body templates.
First element is the name of the template. Second element is
a format specification string.
String keys are as follows:

?t: The document title command
?f: Sections tagged :frontmatter: or with the property :FRONTMATTER:
?c: Normal sections
?a: Sections tagged :appendix: or with the property :APPENDIX:
?b: Sections tagged :backmatter: or with the property :BACKMATTER:"
  :group 'org-export-context
  :type '(repeat
          (cons (string :tag "Template Name")
                (string :tag "Template Contents"))))

(defcustom org-context-location ""
  "Sender's extension field, as a string.

This option can also be set with the LOCATION keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-logfiles-extensions
  '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out"
    "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "tuc")
  "The list of file extensions to consider as ConTeXt logfiles.
The logfiles will be removed if `org-context-remove-logfiles' is
non-nil."
  :group 'org-export-context
  :type '(repeat (string :tag "Extension")))

(defcustom org-context-number-equations nil
  "Non-nil means insert a \\placeformula line before all formulas to allow numbering."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-opening ""
  "Letter's opening, as a string.

This option can also be set with the OPENING keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-pdf-process
  '("context %f")
  "Commands to process a ConTeXt file to a PDF file.

This is a list of strings, each of them will be given to the
shell as a command.  %f in the command will be replaced by the
relative file name, %F by the absolute file name, %b by the file
base name (i.e. without directory and extension parts), %o by the
base directory of the file, %O by the absolute file name of the
output file, %context is the ConTeXt compiler (see
`org-context-compiler').

Alternatively, this may be a Lisp function that does the
processing, so you could use this to apply the machinery of
AUCTeX or the Emacs LaTeX mode.  This function should accept the
file name as its single argument."
  :group 'org-export-pdf
  :type '(repeat (string :tag "Command")))

(defcustom org-context-phone-number ""
  "Sender's phone number, as a string.
This option can also be set with the PHONE_NUMBER keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-place ""
  "Place from which the letter is sent, as a string.
This option can also be set with the PLACE keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-presets-alist
  '(("empty" . (""))
    ("article" . ("\\setupwhitespace[big]"
                  "layout-article"
                  "description-article"
                  "quote-article"
                  "verse-article"
                  "table-article"
                  "title-article"
                  "sectioning-article"
                  "page-numbering-article"
                  "setup-grid"))
    "layout-article"
    ("report" . ("\\setupwhitespace[big]"
                 "description-article"
                 "quote-article"
                 "verse-article"
                 "table-article"
                 "title-report"
                 "headlines-report"
                 "page-numbering-article"
                 "setup-grid"))
    ("letter" . ("\\setupwhitespace[big]
\\usemodule[letter]"
                 "setup-letter"
                 "setup-grid")))
  ;; TODO update doc
  "Alist of ConTeXt preamble presets."
  :group 'org-export-context
  :type '(repeat
          (cons
           (string :tag "Preset Name")
           (plist
            :tag "Presets"
            :value-type
            (cons
             :tag "Location Settings"
             (string :tag "Literal Text")
             (repeat (string :tag "Snippets")))
            :key-type
            (choice
             (const :tag "Preamble Location" :preamble )
             (const :tag "After \\starttext" :starttext )
             (const :tag "Before \\stoptext" :stoptext ))))))

(defcustom org-context-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
By default, logfiles are files with these extensions: .aux, .idx,
.log, .out, .toc, .nav, .snm and .vrb.  To define the set of
logfiles to remove, set `org-context-logfiles-extensions'."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-signature ""
  "Signature, as a string.
This option can also be set with the SIGNATURE keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-snippets-alist
  '(;; Margin setup for article style
    ("layout-article" . "\\setuplayout[
   backspace=103pt,
   topspace=92pt,
   header=12pt,
   headerdistance=25pt,
   width=middle,
   height=middle]")
    ;; US letter paper
    ("paper-letter" . "\\setuppapersize[letter]")
    ;; LaTeX-style tables
    ("table-article" . "\\setupxtable
  [split=yes,
   header=repeat,
   footer=repeat,
   leftframe=off,
   rightframe=off,
   topframe=off,
   bottomframe=off,
   loffset=1em,
   roffset=1em,
   stretch=on]
\\setupxtable
  [OrgTableHeader]
  [toffset=1ex,
   foregroundstyle=bold,
   topframe=on,
   bottomframe=on]
\\setupxtable[OrgTableFooter][OrgTableHeader][]
\\setupxtable
  [OrgTableHeaderTop]
  [OrgTableHeader]
  [bottomframe=off]
\\setupxtable
  [OrgTableFooterTop]
  [OrgTableFooter]
  [bottomframe=off]
\\setupxtable
  [OrgTableHeaderBottom]
  [OrgTableHeader]
  [topframe=off]
\\setupxtable
  [OrgTableFooterBottom]
  [OrgTableFooter]
  [topframe=off]
\\setupxtable
  [OrgTableHeaderMid]
  [OrgTableHeader]
  [topframe=off,bottomframe=off]
\\setupxtable
  [OrgTableFooterMid]
  [OrgTableFooter]
  [topframe=off,bottomframe=off]
\\setupxtable
  [OrgTableTopRow]
  [topframe=on]
\\setupxtable
  [OrgTableRowGroupStart]
  [topframe=on]
\\setupxtable
  [OrgTableRowGroupEnd]
  [bottomframe=on]
\\setupxtable
  [OrgTableColGroupStart]
  [leftframe=on]
\\setupxtable
  [OrgTableColGroupEnd]
  [rightframe=on]
\\setupxtable
  [OrgTableBottomRow]
  [bottomframe=on]
")
    ;; Indented quote blocks
    ("quote-article" . "\\defineblank[QuoteSkip][1ex]
\\setupstartstop
  [OrgBlockQuote]
  [style=slanted,
   before={\\blank[QuoteSkip]
      \\setupnarrower[left=1em, right=1em]
      \\startnarrower[left, right]
      \\noindent},
   after={\\stopnarrower
      \\blank[QuoteSkip]
      \\indenting[next]}]")
    ;; Indented verse blocks with spaces preserved
    ("verse-article" . "\\defineblank[VerseSkip][1ex]
\\setuplines
  [OrgVerse]
  [before={\\blank[VerseSkip]
      \\setupnarrower[left=1em, right=1em]
      \\startnarrower[left, right]},
   after={\\stopnarrower
      \\blank[VerseSkip]},
   space=on]")
    ;; LaTeX-style descriptions
    ("description-article" . "\\setupdescription
  [OrgDesc]
  [headstyle=bold,
   style=normal,
   align=flushleft,
   alternative=hanging,
   width=broad,
   margin=1cm]")
    ;; LaTeX article style title setup
    ("title-article" . "\\setuphead[title][align=middle]
\\define\\OrgMakeTitle{%
  \\startalignment[center]
   \\blank[force,2*big]
   \\title{\\documentvariable{metadata:title}}
   \\doifnot{\\documentvariable{metadata:subtitle}}{}{
     \\blank[force,1*big]
     \\tfa \\documentvariable{metadata:subtitle}}
   \\doifelse{\\documentvariable{metadata:author}}{}{
   \\blank[3*medium]
   {\\tfa \\documentvariable{metadata:email}}
   }{
      \\blank[3*medium]
      {\\tfa \\documentvariable{metadata:author}}
   }
   \\blank[2*medium]
   {\\tfa \\documentvariable{metadata:date}}
   \\blank[3*medium]
  \\stopalignment}")
    ;; LaTeX report style title setup
    ("title-report" . "\\setuphead[title][align=middle]
\\define\\OrgMakeTitle{%
  \\startstandardmakeup[page=yes]
  \\startalignment[center]
   \\blank[force,2*big]
    \\title{\\documentvariable{metadata:title}}
   \\doifnot{\\documentvariable{metadata:subtitle}}{}{
     \\blank[force,1*big]
     \\tfa \\documentvariable{metadata:subtitle}}
   \\doifelse{\\documentvariable{metadata:author}}{}{
   \\blank[3*medium]
   {\\tfa \\documentvariable{metadata:email}}
   }{
      \\blank[3*medium]
      {\\tfa \\documentvariable{metadata:author}}
   }
   \\blank[2*medium]
   {\\tfa \\documentvariable{metadata:date}}
   \\blank[3*medium]
  \\stopalignment
  \\stopstandardmakeup}")
    ;; Report title setuphead
    ;; LaTeX Report-style Headlines
    ("headlines-report" . "\\definehead[subsubsubsection][subsubsection]
\\definehead[subsubsection][subsection]
\\definehead[subsection][section]
\\definehead[section][chapter]
\\definehead[subsubsubsubsubject][subsubsubsubject]
\\definehead[subsubsubsubject][subsubsubject]
\\definehead[subsubsubject][subsubject]
\\definehead[subsubject][subject]
\\definehead[subject][title]
\\setuphead
  [subject,section]
  [before={\\startstandardmakeup[
        headerstate=normal, footerstate=normal, pagestate=start]},
    after={\\stopstandardmakeup}]")
    ;; A simple message
    ("hello" . "% Hello, World!")
    ;; Title on same page as body
    ("sectioning-article" . "\\setupsectionblock[frontpart][page=no]
\\setupsectionblock[bodypart][page=no]")
    ("page-numbering-article" . "\\setuppagenumbering[location=footer,middle]")
    ;; Syntax highlighting. Note that overriding pscolor overrides
    ;; the default so no further action is needed
    ("colors-pigmints" . "% Syntax highlighting that may superficially resemble Pygments
\\startcolorscheme[pscolor]
  \\definesyntaxgroup
    [Comment]
    [style=italic,color={x=408080}]
  \\definesyntaxgroup
    [Constant]
    [color={x=008000}]
  \\definesyntaxgroup
    [Error]
    [style=bold,color={x=D2413A}]
  \\definesyntaxgroup
     [Ignore]
  \\definesyntaxgroup
    [Identifier]
    []
  \\definesyntaxgroup
    [PreProc]
    [color={x=BC7A00}]
  \\definesyntaxgroup
    [Statement]
    [style=bold,color={x=AA22FF}]
  % Don't Know
  \\definesyntaxgroup
    [Special]
    [color={h=BA2121}]
  \\definesyntaxgroup
    [Todo]
    [color={h=800000},
      command=\\vimtodoframed]
  \\definesyntaxgroup
    [Type]
    [color={h=B00040}]
  \\definesyntaxgroup
    [Underlined]
    [color={h=6a5acd},
      command=\\underbar]
  \\setups{vim-minor-groups}
  \\definesyntaxgroup
    [StorageClass]
    [color={h=666666}]
  \\definesyntaxgroup
    [Number]
    [color={h=666666}]
  \\definesyntaxgroup
    [Operator]
    [color={h=666666}, style=bold]
  \\definesyntaxgroup
    [Keyword]
    [color={h=008000}, style=bold]
  \\definesyntaxgroup
    [Conditional]
    [Keyword]
  \\definesyntaxgroup
    [Repeat]
    [Keyword]
  \\definesyntaxgroup
    [Include]
    [Keyword]
  \\definesyntaxgroup
    [Label]
    [color={h=B00040}, style=bold]
  \\definesyntaxgroup
    [Function]
    [color={h=0000ff}]
  \\definesyntaxgroup
    [Macro]
    [Function]
  \\definesyntaxgroup
    [String]
    [color={x=BA2121}]
\\stopcolorscheme")
    ("setup-grid" . "\\setuplayout[grid=both]
\\setupformulae[grid=both]")
    ("setup-letter" . "\\setupletter[
  fromname={\\documentvariable{metadata:author}},
  fromaddress={\\documentvariable{letter:fromaddress}},
  subject={\\documentvariable{metadata:subject}},
  closing={\\documentvariable{letter:closing}},
  signature={\\documentvariable{letter:signature}},
  toname={\\documentvariable{letter:toname}},
  toaddress={\\documentvariable{letter:toaddress}},
  attention={\\documentvariable{letter:attention}},
  opening={\\documentvariable{letter:opening}},
  fromphone={\\documentvariable{metadata:phonenumber}},
  fromurl={\\documentvariable{metadata:url}}]
\\setupletterlayer
  [topmark,botmark,cutmark]
  [state=\\documentvariable{letter:foldmarks}]
\\setupletterlayer
  [backaddress]
  [state=\\documentvariable{letter:withbackaddress}]"))
  "Alist of snippet names and associated text.
These snippets will be inserted into the document preamble when
calling `org-context-make-template'. These snippets are also
available for use in presets. See also `:context-presets'"
  :group 'org-export-context
  :type `(repeat
          (cons
           (string :tag "Snippet Name")
           (string :tag "Snippet Value"))))

(defcustom org-context-syntax-engine
  'default
  "Option for the syntax engine."
  :tag "Default Syntax Engine"
  :group 'org-export-context
  :type '(choice (const :tag "Vim" vim)
                 (const :tag "Default" default)))

(defcustom org-context-table-use-footer ""
  "If \"repeat\", footer rows will be repeated on all pages."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-text-markup-alist
  '((bold ."\\bold{%s}")
    (code . "\\type{%s}")
    (italic . "\\italic{%s}")
    (paragraph . "%s")
    (protectedtexttt . "\\type{%s}")
    (strike-through . "\\inframed[frame=off]{\\overstrike{%s}}")
    (subscript . "\\low{%s}")
    (superscript . "\\high{%s}")
    (underline . "\\underbar{%s}")
    (verbatim . "\\type{%s}")
    (verb . "\\type{%s}"))
  "Alist of ConTeXt expressions to convert text markup."
  :group 'org-export-context
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options
  '(bold
    code
    italic
    paragraph
    protectedtexttt
    strike-through
    subscript
    superscript
    underline
    verbatim
    verb))

(defcustom org-context-url ""
  "Sender's URL, e. g., the URL of her homepage.
This option can also be set with the URL keyword."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-use-backaddress nil
  "Non-nil prints return address in line above to address.
This option can also be set with the OPTIONS keyword, e.g.:
\"backaddress:t\"."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-use-email nil
  "Non-nil prints sender's email address.
This option can also be set with the OPTIONS keyword, e.g.:
\"email:t\"."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-use-foldmarks t
  "Configure appearance of folding marks.

When t, activate default folding marks.  When nil, do not insert
folding marks at all."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-use-from-logo nil
  "Non-nil prints sender's FROM_LOGO.
This option can also be set with the OPTIONS keyword, e.g.:
\"from-logo:t\"."
  :group 'org-export-context
  :type 'boolean
  :safe #'booleanp)

(defcustom org-context-use-phone nil
  "Non-nil prints sender's phone number.
This option can also be set with the OPTIONS keyword, e.g.:
\"phone:t\"."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-use-place t
  "Non-nil prints the letter's place next to the date.
This option can also be set with the OPTIONS keyword, e.g.:
\"place:nil\"."
  :group 'org-export-context
  :type 'boolean)

(defcustom org-context-use-url nil
  "Non-nil prints sender's URL.
This option can also be set with the OPTIONS keyword, e.g.:
\"url:t\"."
  :group 'org-export-context
  :type 'boolean
  :safe #'booleanp)

(defcustom org-context-vim-langs-alist
  (list (list "c++" :vim-name "cpp" :context-name "Cpp")
        (list "c#" :vim-name "cs" :context-name "CSharp")
        (list "vba" :vim-name "basic" :context-name "VBA")
        (list "bash" :vim-name "sh" :context-name "Bash"))
  "Alist mapping Org language names to their counterparts in Vim and ConTeXt."
  :group 'org-export-context
  :type '(repeat
          (list
           (string :tag "Org Language Name")
           (string :tag "Vim Language Name")
           (string :tag "ConTeXt Language Name"))))


;;; Filters

(defun org-context-clean-invalid-line-breaks (data _backend _info)
  "Remove invalid line breaks from raw text.
DATA is the data to strip."
  (replace-regexp-in-string
   "\\(\\\\stop[A-Za-z0-9*]+\\|^\\)[ \t]*\\\\\\\\[ \t]*$"
   "\\1"
   data))

(defun org-context-math-block-options-filter (info _backend)
  "Filter math blocks prior to parsing.
INFO is a plist containing contextual information."
  (dolist (prop '(:author :date :title) info)
    (plist-put info prop
               (org-context--wrap-latex-math-block (plist-get info prop) info))))

(defun org-context-math-block-tree-filter (tree _backend info)
  "Wrap math blocks in TREE prior to parsing.
INFO is a plist containing contextual information."
  (org-context--wrap-latex-math-block tree info))


;;; Internal functions

(defun org-context--add-reference (element contents info)
  "Add a reference label to CONTENTS.
INFO is a plist containing contextual information.
ELEMENT is the entity to add a reference label to."
  (concat (org-context--get-reference element info)
          contents))

(defun org-context--caption/label-string (element info)
  "Return caption and label ConTeXt string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-context--add-reference'."
  (let* ((main (org-export-get-caption element)))
    (org-export-data main info)))

(defun org-context--enumerated-block
    (ent contents info env-kw wrap-kw wrap-empty-kw &optional inner-args)
  "Helper function to wrap blocks in the correct environent.
ENT is the entity to wrap. CONTENTS is the block contents. INFO
is a plist holding contextual information. ENV-KW is the keyword
identifying the environment to place the contents into (see
`options-alist'). WRAP-KW is the keyword identifying the wrapper
environment to enumerate the contents in (see `options-alist').
WRAP-EMPTY-KW is the keyword identifying the wrapper environment
to use if no caption is specified (used to keep numbering
synchronized; see `options-alist'). INNER-ARGS is an alist of
arguments to add to the inner environment."
  ;; TODO Don't wrap in an enumerate if caption is nil (?)
  ;; This is related to https://orgmode.org/list/55ec0cbb-eebf-0d49-b182-372407c8c84c@gmail.com/T/#u
  (let* ((caption
          (org-trim
           (org-export-data
            (or (org-export-get-caption ent t)
                (org-export-get-caption ent))
            info)))
         (enumerate-environment
          (org-string-nw-p
           (car
            (plist-get info (if (org-string-nw-p caption) wrap-kw wrap-empty-kw)))))
         (environment
          (org-string-nw-p
           (car
            (plist-get info env-kw))))
         (label (org-context--label ent info t))
         (args (org-context--format-arguments
                (list
                 (cons "title" caption)
                 (cons "reference" label)))))
    (concat
     (if enumerate-environment
         (format "\\start%s\n  [%s]" enumerate-environment args)
       (org-context--get-reference ent info))
     (format "\n\\start%s%s\n" environment
             (if inner-args (format "[%s]" inner-args) ""))
     contents
     (format "\\stop%s\n" environment)
     (when enumerate-environment
       (format "\\stop%s" enumerate-environment)))))

(defun org-context--format-arguments (arguments &optional oneline)
  "Format ARGUMENTS into a ConTeXt argument string.
ARGUMENTS is an alist of string, string pairs. For instance,
given '((\"key1\" . \"val1\") (\"key2\" . \"val2\")) returns
\"[key1=val1, key2=val2] or similar. If ONELINE is not nil,
formats all the arguments on one line (can be helpful in
verbatim environments)."
  (mapconcat
   (lambda (kv)
     (let ((key (car kv))
           (val (cdr kv)))
       (format "%s={%s}" key val)))
   (seq-filter (lambda (s) (org-string-nw-p (cdr s))) arguments)
   (if oneline "," ",\n   ")))

(defun org-context--get-reference (element info)
  "Gets a label for ELEMENT.
INFO is the current export state, as
a plist."
  (let ((label (org-context--label element info t))
        (name (org-export-get-node-property :name element))
        (value (org-export-get-node-property :value element)))
    (if (or name value)
        (format "\\reference[%s]{%s}\n" label (or name value))
      (format "\\reference[%s]{}\n" label))))

(defun org-context--get-vim-lang-info (src-block info)
  "Get a plist containing langauge information for vim higlighting.
INFO is a plist that acts as a communication channel. SRC-BLOCK
is the code block to get information for.

Language translation info is cached in the INFO plist so that
typing environments can be defined in the template."
  (let ((org-lang (org-element-property :language src-block)))
    (if (org-string-nw-p org-lang)
      (let* ((lang-cache
              (or (plist-get info :context-languages-used-cache)
                  (let ((hash (make-hash-table :test #'equal)))
                    (plist-put info :context-languages-used-cache hash)
                    hash))))
        (or (gethash org-lang lang-cache)
            (puthash org-lang
                     (let ((lang-info
                            (or
                             (cdr
                              (assoc org-lang
                                     (plist-get info :context-vim-langs-alist)))
                             (list
                              :vim-name (downcase org-lang)
                              :context-name (capitalize org-lang)))))
                       (list
                        :vim-lang
                        (plist-get lang-info :vim-name)
                        :context-inline-name
                        (concat
                         (or
                          (org-string-nw-p
                           (car
                            (plist-get info :context-inline-source-environment)))
                          "")
                         (plist-get lang-info :context-name))
                        :context-block-name
                        (concat
                         (or
                          (org-string-nw-p
                           (car
                            (plist-get info :context-block-source-environment)))
                          "")
                         (plist-get lang-info :context-name))))
                     lang-cache))))))

(defun org-context--get-builtin-lang-name (src-block info)
  "Gets the ConTeXt name of a language from its Org name.
SRC-BLOCK is the code block to get the name of. INFO is a plist
containing contextual information."
  (let* ((org-lang (org-element-property :language src-block)))
    (and
     org-lang
     (or (cadr (assoc org-lang
                      (plist-get info :context-highlighted-langs)))
         (downcase org-lang)))))

(defun org-context--label (datum info &optional force full)
  "Return an appropriate label for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

Return nil if element DATUM has no NAME or VALUE affiliated
keyword or no CUSTOM_ID property, unless FORCE is non-nil.  In
this case always return a unique label.

Eventually, if FULL is non-nil, wrap label within \"\\label{}\"."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (cl-case type
	     ((headline inlinetask) :CUSTOM_ID)
	     (target :value)
	     (otherwise :name))
	   datum))
	 (label
	  (and (or user-label force)
	       (concat (pcase type
                   (`headline "sec:")
                   (`table "tab:")
                   (`latex-environment
                    (and (string-match-p
                          org-context-latex-math-environments-re
                          (org-element-property :value datum))
                         "eq:"))
                   (`latex-matrices "eq:")
                   (`paragraph
                    (and (org-element-property :caption datum)
                         "fig:"))
                   (_ nil))
                 (org-export-get-reference datum info)))))
    (cond ((not full) label)
          (label
           (format
            "\\pagereference[%s]%s"
            label
            (if (eq type 'target) "" "\n")))
          (t ""))))

(defun org-context--get-coderef-label (ref parent info)
  "Create a reference label for REF.
PARENT is the parent code block or example block referred to by REF.
See `org-context--find-coderef-parent' for finding that element.
INFO is a plist containing contextual information."
  (format "%s:%s" (org-context--label parent info t) ref))

(defun org-context--find-coderef-parent (ref info)
  "Resolve a code reference REF and return the element in which it appears.

INFO is a plist used as a communication channel.

This function is used in place of `org-export-resolve-coderef'
because ConTeXt provides semantics for line number references
already. Therefore, we just need a globally unique identifier for
the coderef."

  (or
   (org-element-map (plist-get info :parse-tree) '(example-block src-block)
     (lambda (el)
       (with-temp-buffer
         (insert (org-trim (org-element-property :value el)))
         (let* ((label-fmt (or (org-element-property :label-fmt el)
                               org-coderef-label-format))
                (ref-re (org-src-coderef-regexp label-fmt ref)))
           ;; Element containing ref is found. Always return ref.
           (when (re-search-backward ref-re nil t) el))))
     info 'first-match)
   (signal 'org-link-broken (list ref))))

(defun org-context--protect-text (text)
  "Protect special characters in string TEXT and return it."
  (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" text))

(defun org-context--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel. See
`org-context-text-markup-alist' for details"
  ;; TODO Handle special cases like the LaTeX backend
  (let ((fmt (cdr (assq markup (plist-get info :context-text-markup-alist)))))
    (cl-case fmt
      ;; No format string: Return raw text.
      ((nil) text)
      (verb
       (format "\\type{%s}" text))
      (t (format fmt text)))))

(defun org-context--wrap-env (ent contents info env-key &optional default)
  "Wraps content in an environment with a label.
ENT is the entity to wrap in an environment.
CONTENTS is the contents of the entity to wrap.
INFO is a plist containing contextual information.
ENV-KEY is a keyword from `:options-alist'.
DEFAULT is the default environment if the environment
in ENV-KEY is not implemented.
Environment is looked up from the info plist."
  (let* ((prog-env-name (car (plist-get info env-key)))
         (env-name (or (org-string-nw-p prog-env-name) default)))
    (org-context--add-reference
     ent
     (if env-name
         (format "\\start%s\n%s\\stop%s" env-name contents env-name)
       contents)
     info)))

(defun org-context--wrap-latex-math-block (data info)
  "Merge continuous math objects in a pseudo-object container.
DATA is a parse tree or a secondary string. INFO is a plist
containing export options. Modify DATA by side-effect and return it."
  (let
      ((valid-object-p
        ;; Non-nill when OBJECT can be added to a latex math block
        (lambda (object)
          (pcase (org-element-type object)
            (`entity (org-element-property :latex-math-p object))
            (`latex-fragment
             (let
                 ((value (org-element-property :value object)))
               (or (string-prefix-p "\\(" value)
                   (string-match-p "\\`\\$[^$]" value))))))))
    (org-element-map
     data
     '(entity latex-fragment)
     (lambda (object)
       (when
           (and
            (not
             (eq
              (org-element-type
               (org-element-property :parent object))
              'latex-math-block))
            (funcall valid-object-p object))
         (let
             ((math-block (list 'latex-math-block nil))
              (next-elements (org-export-get-next-element object info t))
              (last object))
           ;; Wrap MATH-BLOCK around OBJECT in DATA.
           (org-element-insert-before math-block object)
           (org-element-extract-element object)
           (org-element-adopt-elements math-block object)
           (when (zerop (or (org-element-property :post-blank object) 0))
             ;; MATH-BLOCK swallows consecutive math objects.
             (catch 'exit
               (dolist (next next-elements)
                 (unless (funcall valid-object-p next) (throw 'exit nil))
                 (org-element-extract-element next)
                 (org-element-adopt-elements math-block next)
                 ;; Eschew the case: \beta$x$ -> \(\betax\)
                 (org-element-put-property last :post-blank 1)
                 (setq last next)
                 (when (> (or (org-element-property :post-blank next) 0) 0)
                   (throw 'exit nil)))))
           (org-element-put-property
            math-block :post-blank (org-element-property :post-blank last)))))
     info nil '(latex-latex-math-block) t)
    data))

;;; Transcode Functions

;;;; Bold

(defun org-context-bold (_bold contents info)
  "Transcode BOLD from Org to ConTeXt.
CONTENTS is the text with bold markup. INFO is a plist holding
contextual information."
  (org-context--text-markup contents 'bold info))

;;;; Center Block

(defun org-context-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-context--add-reference
   center-block (format "\\startalignment[middle]\n%s\\stopalignment" contents) info))

;;;; Clock

(defun org-context-format-clock-default-function (timestamp info)
  "Format a timestamp in ConTeXt format.
TIMESTAMP is an Org timestamp. INFO is a plist containing
contextual information."
  (let* ((time (org-timestamp-to-time timestamp))
         (args
          (list
           (cons "y" (format-time-string "%Y" time))
           (cons "m" (format-time-string "%m" time))
           (cons "d" (format-time-string "%d" time))
           (cons "H" (format-time-string "%H" time))
           (cons "M" (format-time-string "%M" time))
           (cons "I" (format-time-string "%I" time))
           (cons "S" (format-time-string "%S" time))))
         (formatter
          (org-string-nw-p
           (car (plist-get info :context-clock-command)))))
    (if formatter
        (format "\\%s[%s]" formatter (org-context--format-arguments args))
      (format-time-string "%FT%T%z" time))))

(defun org-context-clock (clock _contents info)
  "Transcode a CLOCK element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((timestamp (org-element-property :value clock))
        (formatter (plist-get info :context-format-clock-function)))
    (funcall formatter timestamp info)))

;;;; Code

(defun org-context-code (code _contents info)
  "Transcode CODE from Org to ConTeXt.
INFO is a plist containing contextual information."
  (org-context--text-markup (org-element-property :value code) 'code info))

;;;; Drawer

(defun org-context-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ConTeXt.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (funcall (plist-get info :context-format-drawer-function)
                          name contents info)))
    (org-context--add-reference drawer output info)))

(defun org-context-format-drawer-default-function (name contents info)
  "Format a drawer using the default.
NAME is the name of the drawer. CONTENTS is the contents of the drawer.
INFO is a plist containing contextual information."
  (let ((formatter
         (org-string-nw-p
          (car (plist-get info :context-drawer-command)))))
    (if formatter
        (format "\\%s{%s}{%s}" formatter name contents)
      (format "%s\\hairline %s" name contents))))

;;;; Dynamic Block

(defun org-context-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-context--add-reference dynamic-block contents info))

;;;; Entity

(defun org-context-entity (entity _contennts info)
  "Transcode an ENTITY object from Org to ConTeXt.
CONTENTS are the definition itself. INFO is a plist
holding contextual information."
  (org-element-property :context entity))

;;;; Example Block

(defun org-context-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let* ((contents (org-context--preprocess-source-block example-block info))
         (num-start (org-export-get-loc example-block info))
         (num-start-str
          (when (and num-start (> num-start 0))
            (number-to-string (+ 1 num-start))))
         (args
          (org-string-nw-p
           (org-context--format-arguments
            (list
             (cons "start" num-start-str)
             (cons "numbering" (when num-start "line")))))))
    (when contents
      (org-context--enumerated-block
       example-block
       (org-context--preprocess-source-block example-block info)
       info
       :context-example-environment
       :context-enumerate-example-environment
       :context-enumerate-example-empty-environment
       args))))

;;;; Export Block

(defun org-context-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((type (org-element-property :type export-block))
        (value (org-element-property :value export-block)))
    (cond ((member type '("CONTEXT" "TEX"))
           (org-remove-indentation value))
          ((member type '("METAPOST"))
           (format "\\startMPcode\n%s\\stopMPcode"
                   (org-remove-indentation value))))))

;;;; Export Snippet

(defun org-context-export-snippet (export-snippet _contents _info)
  "Transcode an EXPORT-SNIPPET object from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'context)
    (org-element-property :value export-snippet)))

;;;; Fixed Width

(defun org-context-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element from Org to LaTeX.
CONTENTS is nil. INFO is a plist holding contextual information."
  (org-context--wrap-env
   fixed-width
   (org-remove-indentation (org-element-property :value fixed-width))
   info
   :context-fixed-environment
   "typing"))

;;;; Footnote Reference

(defun org-context-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;;
  ;; Use `org-export-collect-elements' to collect all
  ;; footnotes in the document
  (let* ((label (org-element-property :label footnote-reference))
         (footnote-definition
          (org-export-get-footnote-definition footnote-reference info))
         (reference-label (org-context--label footnote-definition info t))
         (contents (org-trim (org-export-data footnote-definition info)))
         (insidep (org-element-lineage footnote-reference
                                       '(footnote-reference
                                         footnote-definition))))
    (concat
     (format "\\note[%s]" reference-label)
     (when (not insidep)
       (concat "\n"
               (format "\\footnotetext[%s]{%s}%%\n" reference-label contents)
               (org-context--delayed-footnotes-definitions footnote-definition info))))))

(defun org-context--delayed-footnotes-definitions (element info)
  "Return footnotes definitions in ELEMENT as a string.

INFO is a plist used as a communication channel.

Footnotes definitions are returned within \"\\footnotetext{}\"
commands. This is done to make the handling of footnotes more
uniform."
  (mapconcat
   (lambda (ref)
     (let ((def (org-export-get-footnote-definition ref info)))
       (format "\\footnotetext[%s]{%s}%%\n"
	       (org-trim (org-context--label def info t))
	       (org-trim (org-export-data def info)))))
   ;; Find every footnote reference in ELEMENT.
   (letrec ((all-refs nil)
            (search-refs
             (lambda (data)
               ;; Return a list of all footnote references never seen
               ;; before in DATA.
               (org-element-map data 'footnote-reference
                 (lambda (ref)
                   (when (org-export-footnote-first-reference-p ref info)
                     (push ref all-refs)
                     (when (eq (org-element-property :type ref) 'standard)
                       (funcall search-refs
                                (org-export-get-footnote-definition ref info)))))
                 info)
               (reverse all-refs))))
     (funcall search-refs element))
   ""))

;;;; Headline

(defun org-context-headline (headline contents info)
  "Transcode a HEADLINE element from Org to ConTeXt.
CONTENTS is the content of the section. INFO is a plist
containing contextual information."
  ;; TODO Handle category from `org-export-get-category'
  ;; TODO Handle node property from `org-export-get-node-property'
  (let* ((level (org-export-get-relative-level headline info))
         (numberedp (org-export-numbered-headline-p headline info))
         (text (org-export-data (org-element-property :title headline) info))
         (alt-title (or (org-export-get-node-property :ALT_TITLE headline) text))
         ;; TODO Handle description metadata
         ;; (description (org-export-get-node-property :DESCRIPTION headline))
         (todo
          (and (plist-get info :with-todo-keywords)
               (let ((todo (org-element-property :todo-keyword headline)))
                 (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority-num (org-element-property :priority headline))
         (priority (and (plist-get info :with-priority)
                        priority-num
                        (string priority-num)))
         (full-text (funcall (plist-get info :context-format-headline-function)
                             todo todo-type priority text tags info))
         (notoc (org-export-excluded-from-toc-p headline info))
         (headline-name
          (let ((hname (org-context--get-headline-command numberedp level)))
            (if notoc
                (let* ((notoc-heading-cache
                        (or (plist-get info :context-notoc-headline-cache)
                            (let ((hash (make-hash-table :test #'equal)))
                              (plist-put info :context-notoc-headline-cache hash) hash)))
                       (notoc-name
                        (or (gethash hname notoc-heading-cache)
                            (puthash hname
                                     (format "%sNoToc" hname)
                                     notoc-heading-cache))))
                  notoc-name)
              hname)))
         (headertemplate (format "\n\\start%s" headline-name))
         (footercommand (format "\n\\stop%s" headline-name))
         (headline-label (org-context--label headline info t ))
         (frontmatterp
          (or (member "frontmatter" tags)
              (org-export-get-node-property :FRONTMATTER headline)))
         (backmatterp
          (or (member "backmatter" tags)
              (org-export-get-node-property :BACKMATTER headline)))
         (appendixp
          (or (member "appendix" tags)
              (org-export-get-node-property :APPENDIX headline)))
         (headline-args
          (org-context--format-arguments
           (list
            (cons "title" full-text)
            (cons "list" alt-title)
            (cons "marking" alt-title)
            (cons "bookmark" alt-title)
            (cons "reference" headline-label))))
         (result (concat
                  headertemplate
                  (format "[%s]" headline-args)
                  "\n\n"
                  contents
                  footercommand)))
    ;; Special sections are stuck in the plist somewhere else
    ;; for later rendering
    (cond
     (backmatterp
      (let ((backmatter-sections
             (plist-get info :context-backmatter-sections)))
        (plist-put info :context-backmatter-sections
                   (cons result backmatter-sections))
        nil))
     (frontmatterp
      (let ((frontmatter-sections
             (plist-get info :context-frontmatter-sections)))
        (plist-put info :context-frontmatter-sections
                   (cons result frontmatter-sections))
        nil))
     (appendixp
      (let ((appendix-sections
             (plist-get info :context-appendix-sections)))
        (plist-put info :context-appendix-sections
                   (cons result appendix-sections))
        nil))
     (t result))))

(defun org-context-format-headline-default-function
    (todo todo-type priority text tags info)
  "Default format function for a headline.
TODO is the actual text of the TODO keyword.
TODO-TYPE is the type of the todo.
PRIORITY is the priority of the item.
TEXT is the text of the headline.
TAGS is a list of tags associated with the headline.
INFO is a plist containing contextual information.
See `org-context-format-headline-function' for details."
  (let ((formatter (org-string-nw-p (car (plist-get info :context-headline-command)))))
    (if formatter
        (format
         "\\%s
   [%s]"
         formatter
         (org-context--format-arguments
          (list
           (cons "Todo" todo)
           (cons "TodoType" todo-type)
           (cons "Priority" priority)
           (cons "Text" text)
           (cons "Tags" (mapconcat #'org-context--protect-text tags ":")))))
      text)))

(defun org-context--get-all-headline-commands (max-depth)
  "Get a concatenated listing of every headline command up to MAX-DEPTH."
  (concat
   (mapconcat
    (lambda (depth)
      (concat
       (org-context--get-headline-command t depth)
       ","
       (org-context--get-headline-command nil depth)))
    (number-sequence 1 max-depth)
    ",")))

(defun org-context--get-headline-command (numberedp level)
  "Create a headline name with the correct depth.
If NUMBEREDP, gets a numbered command. LEVEL is the level of headline
command to get."
  (concat
   (apply 'concat (make-list (+ level (- 1)) "sub"))
   (if numberedp "section" "subject")))

;;;; Horizontal Rule

(defun org-context-horizontal-rule (horizontal-rule _contents info)
  "Transcode a HORIZONTAL-RULE object from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  ;; TODO accept attr_context
  (let ((attr (org-export-read-attribute :attr_latex horizontal-rule))
        (prev (org-export-get-previous-element horizontal-rule info)))
    (concat
     ;; Make sure the rule doesn't start at the end of the current
     ;; line
     (when (and prev
                (let ((prev-blank (org-element-property :post-blank prev)))
                  (or (not prev-blank) (zerop prev-blank))))
       "\n")
     ;; TODO get width and thickness from attr_context
     (org-context--add-reference
      horizontal-rule
      "\\textrule"
      info))))

;;;; Inline Src Block

(defun org-context-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((engine (plist-get info :context-syntax-engine))
        (code (org-export-format-code-default inline-src-block info)))
    (pcase engine
      ('vim (org-context--highlight-inline-src-vim
             inline-src-block code info))
      (_ (org-context--highlight-inline-src-builtin
          inline-src-block code info)))))

(defun org-context--highlight-inline-src-builtin (src-block code info)
  "Wraps a source block in the builtin environment for ConTeXt source code.
Use this if you don't have Vim.

SRC-BLOCK is the code object to transcode.
CODE is the preprocessed contents of the code block.
INFO is a plist holding contextual information."
  (when code
    (let* ((lang (org-context--get-builtin-lang-name src-block info))
           (env-name
            (or
             (org-string-nw-p
              (car (plist-get info :context-inline-source-environment)))
             "type")))
      (format "\\%s%s{%s}"
              env-name
              (if (org-string-nw-p lang)
                  (format "[option=%s]" lang)
                "")
              code))))

(defun org-context--highlight-inline-src-vim (src-block code info)
  "Wraps a source block in a vimtyping environment.
This requires you have Vim installed and the t-vim module for
ConTeXt. SRC-BLOCK is the entity to wrap. CODE is the contents of
the entity. INFO is a plist containing contextual information."
  (let ((org-lang (org-element-property :language src-block)))
    (if (org-string-nw-p org-lang)
      (let* ((lang-info (org-context--get-vim-lang-info src-block info))
             (context-name (plist-get lang-info :context-inline-name)))
        (format "\\inline%s{%s}" context-name code)))))

;;;; Inlinetask

(defun org-context-inlinetask (inlinetask contents info)
  "Transcode an INLINETASK element from Org to ConTeXt.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let* ((title (org-export-data (org-element-property :title inlinetask) info))
         (todo (and (plist-get info :with-todo-keywords)
                    (let ((todo (org-element-property :todo-keyword inlinetask)))
                      (and todo (org-export-data todo info)))))
         (todo-type (org-element-property :todo-type inlinetask))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags inlinetask info)))
         (priority-num (org-element-property :priority inlinetask))
         (priority (and (plist-get info :with-priority)
                        priority-num
                        (make-string 1 priority-num)))
         (label (org-context--label inlinetask info))
         (format-func (plist-get info :context-format-inlinetask-function)))
    (funcall format-func
             todo todo-type priority title tags contents info)))

(defun org-context-format-inlinetask-default-function
    (todo todo-type priority title tags contents info)
  "Default format function for inlinetasks.
TODO is the actual text of the TODO keyword.
TODO-TYPE is the type of the todo.
PRIORITY is the priority of the item.
TITLE is the text of the headline.
TAGS is a list of tags associated with the headline.
CONTENTS is the contents of the task.
INFO is a plist containing contextual information.
See `org-context-format-inlinetask-function' for details."

  ;; TODO Strip surrounding colons from tags...
  ;; Or decide not to. LaTeX leaves the colons; html strips them
  (let ((format-command
         (org-string-nw-p (car (plist-get info :context-inlinetask-command)))))
    (if format-command
        (format
         "\\%s
  [%s]"
         format-command
         (org-context--format-arguments
          (list
           (cons "Todo" todo)
           (cons "TodoType" todo-type)
           (cons "Priority" priority)
           (cons "Title" title)
           (cons "Tags" (org-make-tag-string (mapcar #'org-context--protect-text tags)))
           (cons "Contents" contents))))
      (concat title "\\hairline" contents "\\hairline"))))

;;;; Inner Template

(defun org-context-inner-template (contents info)
  "Return body of document string after ConTeXt conversion.
CONTENTS is the transcoded contents string. INFO is a plist
containing contextual information."
  (let* ((templates (plist-get info :context-inner-templates))
         (template-name (plist-get info :context-inner-template))
         (titlepagecommand (car (plist-get info :context-titlepage-environment)))
         (titlecommand (org-string-nw-p (car (plist-get info :context-title-command))))
         (toccommand (car (plist-get info :context-title-contents-command)))
         (template (cdr (assoc template-name templates)))
         (num-sections (length (org-export-collect-headlines info)))
         (frontmatter-sections
          (mapconcat
           'identity
           (reverse (plist-get info :context-frontmatter-sections))
           "\n\n"))
         (backmatter-sections
          (mapconcat
           'identity
           (reverse (plist-get info :context-backmatter-sections))
           "\n\n"))
         (appendix-sections
          (mapconcat
           'identity
           (reverse (plist-get info :context-appendix-sections))
           "\n\n"))
         (title
          (concat
           (when (org-string-nw-p titlepagecommand)
             (format "\\start%s\n" titlepagecommand))
           (when (org-string-nw-p titlecommand)
             (format "\\%s\n" titlecommand))
           (when (and (plist-get info :with-toc)
                      (> num-sections 0))
             (concat (when (org-string-nw-p toccommand)
                       (format "\\%s\n" toccommand))
                     "\n\\placecontent\n"))
           (when (org-string-nw-p titlepagecommand)
             (format "\\stop%s\n" titlepagecommand)))))
    (format-spec
     template
     (list (cons ?t title)
           (cons ?f frontmatter-sections)
           (cons ?c contents)
           (cons ?a appendix-sections)
           (cons ?b backmatter-sections)))))

;;;; Italic

(defun org-context-italic (_italic contents info)
  "Transcode CONTENTS from Org to ConTeXt.
INFO is a plist containing contextual information."
  (org-context--text-markup contents 'italic info))

;;;; Item

(defun org-context-item (item contents info)
  "Transcode an ITEM element from Org to ConTeXt.
CONTENTS is the contents of the item. INFO is a plist containing
contextual information."
  ;; TODO Handle alphabetical items where `org-list-allow-alphabetical' is set
  (let ((tag (let ((tag (org-element-property :tag item)))
               (and tag (org-export-data tag info))))
        (checkbox (cl-case (org-element-property :checkbox item)
                    (on (format "\\%s" (car (plist-get info :context-bullet-on-command))))
                    (off (format "\\%s" (car (plist-get info :context-bullet-off-command))))
                    (trans (format "\\%s" (car (plist-get info :context-bullet-trans-command)))))))
    (if (eq (org-element-property :type (org-export-get-parent item))
            'descriptive)
        (let ((descrcommand (car (plist-get info :context-description-command))))
          (format "\\start%s{%s} %s\n\\stop%s"
                  descrcommand
                  (if (org-string-nw-p checkbox)
                      (format "%s\\space\\space %s" checkbox tag)
                    tag)
                  (org-trim (or contents ""))
                  descrcommand))
      (if (org-string-nw-p checkbox)
          (format "\\sym{%s} %s" checkbox contents)
        (format "\\item %s" (org-trim (or contents "")))))))

;;;; Keyword

(defun org-context-keyword (keyword _contents info)
  "Transcode a KEYWORD element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((string= key "CONTEXT") value)
     ((string= key "INDEX") (format "\\index{%s}" value))
     ((string= key "TOC")
      (let ((case-fold-search t))
        (pcase value
          ((pred (string-match-p "\\<tables\\>")) "\\placelistoftables[criterium=all]")
          ((pred (string-match-p "\\<figures\\>")) "\\placelistoffigures[criterium=all]")
          ((pred (string-match-p "\\<equations\\>")) "\\placelist[formula][criterium=all]")
          ((pred (string-match-p "\\<references\\>")) "\\placelistofpublications[criterium=all]")
          ((pred (string-match-p "\\<definitions\\>")) "\\placeindex")
          ((pred (string-match-p "\\<headlines\\>"))
           (let* ((localp (string-match-p "\\<local\\>" value))
                  (parent (org-element-lineage keyword '(headline)))
                  (depth
                   (if (string-match "\\<[0-9]+\\>" value)
                       (string-to-number (match-string 0 value))
                     0))
                  (level (+ depth
                            (if (not (and localp parent)) 0
                              (org-export-get-relative-level parent info))))
                  (levelstring (if (> level 0)
                                   (format "list={%s}"
                                           (org-context--get-all-headline-commands level))
                                 "")))
             (if localp (format  "\\placecontent[criterium=local,%s]" levelstring)
               (format  "\\placecontent[%s]" levelstring))))
          ((or
            (and (pred (string-match-p "\\<listings\\>"))
                 (let kw :context-enumerate-listing-empty-environment))
            (and (pred (string-match-p "\\<verses\\>"))
                 (let kw :context-enumerate-verse-empty-environment))
            (and (pred (string-match-p "\\<quotes\\>"))
                 (let kw :context-enumerate-blockquote-empty-environment))
            (and (pred (string-match-p "\\<examples\\>"))
                 (let kw :context-enumerate-example-empty-environment))
            (let kw nil))
           (let ((env
                  (org-string-nw-p
                   (car
                    (plist-get info kw)))))
             (if env (format "\\placelist[%s][criterium=all]" env)
               ""))))))

     ((string= key "BIBLIOGRAPHY")
      ;; TODO
      ;;
      ;; ox-bibtex supports the following syntax
      ;;
      ;; bibfilename stylename optional-options
      ;;
      ;; Full filepaths are also possible
      ;;
      ;; Stylename can be nil in which case no style is used
      ;;
      ;; Options are defined in bibtex2html manual
      ;;
      ;; limit:t limits to only entries cited in the document
      (let ((file (org-context--get-bib-file keyword)))
        (plist-put info :context-bib-command
                   (format "\\usebtxdataset[%s]" file))
        nil)))))

(defun org-context--get-bib-file (keyword)
  "Return bibliography file as a string.
KEYWORD is a \"BIBLIOGRAPHY\" keyword. If no file is found,
return nil instead."
  (let ((value (org-element-property :value keyword)))
    (and value
         (string-match "\\(\\S-+\\)[ \t]+\\(\\S-+\\)\\(.*\\)" value)
         (concat (match-string 1 value) ".bib"))))

;;;; Latex Enviroment

(defun org-context-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; TODO handle associated metadata
  (when (plist-get info :with-latex)
    (let* ((value (org-remove-indentation
                   (org-element-property :value latex-environment)))
           (environment-name
            (org-context--latex-environment-name latex-environment))
           (environment-contents
            (org-context--latex-environment-contents
             latex-environment))
           (numberedp
            (not (string-match "\\*$" environment-name)))
           (type (org-context--environment-type latex-environment))
           (label (org-context--label latex-environment info t))
           (caption (org-context--caption/label-string latex-environment info))
           (args (org-context--format-arguments
                  (list
                   ;; (cons "title" caption)
                   (cons "reference" label))))
           )
      ;; TODO 'table 'src-block
      (pcase type
        ('math
         ;; TODO equaton eqnarray math displaymath
         ;; gather multline flalign alignat
         ;; xalginat xxalignat
         ;; subequations brequn
         ;; dmath dseries dgroup darray
         ;; empheq
         (concat
          (when numberedp
            (format "\\startplaceformula\n  [%s]\n" args))
          "\\startformula\n"
          (pcase environment-name
            ((or "align" "align*")
             (org-context--transcode-align environment-contents))
            (_ environment-contents))
          "\\stopformula"
          (when numberedp "\\stopplaceformula"))
         )
        (_ value)))))

(defun org-context--environment-type (latex-environment)
  "Return the TYPE of LATEX-ENVIRONMENT.

The TYPE is determined from the actual latex environment."
  (let* ((latex-begin-re "\\\\begin{\\([A-Za-z0-9*]+\\)}")
         (value (org-remove-indentation
                 (org-element-property :value latex-environment)))
         (env (or (and (string-match latex-begin-re value)
                       (match-string 1 value))
                  "")))
    (cond
     ((string-match-p org-context-latex-math-environments-re value) 'math)
     ((string-match-p
       (eval-when-compile
         (regexp-opt '("table" "longtable" "tabular" "tabu" "longtabu")))
       env)
      'table)
     ((string-match-p "figure" env) 'image)
     ((string-match-p
       (eval-when-compile
         (regexp-opt '("lstlisting" "listing" "verbatim" "minted")))
       env)
      'src-block)
     (t 'special-block))))

(defun org-context--latex-environment-contents (latex-environment)
  "Return the contents of LATEX-ENVIRONMENT."
  (let* ((latex-env-re "\\\\begin{\\([A-Za-z0-9*]+\\)}\\(\\(?:.*\n\\)*\\)\\\\end{\\1}")
         (value (org-remove-indentation
                 (org-element-property :value latex-environment)))
         (match (string-match latex-env-re value))
         (env-contents (match-string 2 value)))
    env-contents))

(defun org-context--latex-environment-name (latex-environment)
  "Return the NAME of LATEX-ENVIRONMENT.

The TYPE is determined from the actual latex environment."
  (let* ((latex-begin-re "\\\\begin{\\([A-Za-z0-9*]+\\)}")
         (value (org-remove-indentation
                 (org-element-property :value latex-environment)))
         (env (or (and (string-match latex-begin-re value)
                       (match-string 1 value))
                  "")))
    env))

(defun org-context--transcode-align (align-environment)
  "Transcode an ALIGN-ENVIRONMENT from org to ConTeXt."
  (concat
   "\\startalign\n"
   (mapconcat
   (lambda (math-row)
     (concat
      "\\NC "
      ;; Strip surrounding whitespace
      (replace-regexp-in-string
       "\\`[ \t\n]*"
       ""
       (replace-regexp-in-string
        "[ \t\n]*\\'"
        ""
        (replace-regexp-in-string "[^\\]&" " \\\\NC " math-row)))))
   (seq-filter 'org-string-nw-p
               (split-string align-environment "\\\\\\\\"))
   " \\NR[+]\n")
   " \\NR[+]\n\\stopalign\n"))

;;;; Latex Fragment

(defun org-context-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((value (org-element-property :value latex-fragment)))
    ;; Trim math markers since the fragment is enclosed within
    ;; a latex-math-block object anyway.
    (cond ((string-match-p "\\`\\$[^$]" value) (substring value 1 -1))
          ((string-prefix-p "\\(" value) (substring value 2 -2))
          ((or
            (string-prefix-p "\\[" value)
            (string-prefix-p "$$" value))
           (concat
            (when (plist-get info :context-number-equations)
              "\\placeformula\n")
            (format "\\startformula\n%s\n\\stopformula"
                    (substring value 2 -2))))
          ((org-context--citation-p latex-fragment)
           (format "\\cite[%s]"
                   (org-context--get-citation-key latex-fragment)))
          (t value))))

(defun org-context--citation-p (object)
  "Non-nil when OBJECT is a citation."
  (cl-case (org-element-type object)
    (link (equal (org-element-property :type object) "cite"))
    (latex-fragment
     (string-match "\\`\\\\cite{" (org-element-property :value object)))))

(defun org-context--get-citation-key (citation)
  "Return key for a given citation, as a string.
CITATION is a `latex-fragment' or `link' type object satisfying
to `org-bibtex-citation-p' predicate."
  (if (eq (org-element-type citation) 'link)
      (org-element-property :path citation)
    (let ((value (org-element-property :value citation)))
      (and (string-match "\\`\\\\cite{" value)
           (substring value (match-end 0) -1)))))

;;;; Line Break

(defun org-context-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\crlf\n")

;;;; Link

(defun org-context-link (link desc info)
  "Transcode a LINK object from Org to ConTeXt.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information. See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (desc (and (not (string= desc "")) desc))
         (imagep (org-export-inline-image-p
                  link
                  (plist-get info :context-inline-image-rules)))
         (path (org-context--protect-text
                (pcase type
                  ((or "http" "https" "ftp" "mailto" "doi")
                   (concat type ":" raw-path))
                  ("file"
                   (org-export-file-uri raw-path))
                  (_
                   raw-path)))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'latex info))
     ;; Image file.
     (imagep (org-context--inline-image link info))
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination)
            desc
          (format "\\goto{%s}[%s]"
                  desc
                  (org-export-get-reference destination info)))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination
             (if (string= type "fuzzy")
                 (org-export-resolve-fuzzy-link link info 'latex-matrices)
               (org-export-resolve-id-link link info))))
        (cl-case (org-element-type destination)
          ;; Id link points to an external file
          (plain-text
           (if desc
               (format "\\goto{%s}[url(%s)]" desc destination)
             (format "\\goto{\\hyphenatedurl{%s}}[url(%s)]" destination destination)))
          ;; Fuzzy link points nowhere
          ((nil)
           (format "\\hyphenatedurl{%s}" (org-element-property :raw-link link)))
          ;; LINK points to a headline.  If headlines are numbered
          ;; and the link has no description, display headline's
          ;; number.  Otherwise, display description or headline's
          ;; title.
          (otherwise
           (let ((label (org-context--label destination info t)))
             (if (not desc)
                 (format "\\goto{\\ref[default][%s]}[%s]" label label)
               (format "\\goto{%s}[%s]" desc label)))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let* ((code-block (org-context--find-coderef-parent raw-path info))
             (ref-label (org-context--get-coderef-label
                         raw-path code-block info))
             (linenum (org-export-get-loc code-block info))
             (retain-labels (org-element-property :retain-labels code-block)))
        (cond ((and linenum (not retain-labels))
               (format "\\inline{ }[%s]" ref-label))
              ((not retain-labels)
               (format "\\goto{\\ref[default][%s]}[%s]" ref-label ref-label))
              (t (format "\\goto{%s}[%s]" path ref-label)))))
     ;; External link with a description part.
     ((and path desc) (format "\\goto{%s}[url(%s)]" desc path))
     ;; External link without a description part.
     (path (format "\\goto{\\hyphenatedurl{%s}}[url(%s)]" path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\hyphenatedurl{%s}" desc)))))

(defun org-context--inline-image (link info)
  "Return the ConTeXt code for an inline image.
LINK is the link pointing to the inline image. INFO is a plist
used as a communication channel."
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                   (expand-file-name raw-path))))
         (filetype (file-name-extension path))
         (attr-latex (org-export-read-attribute :attr_latex parent))
         (attr-context (org-export-read-attribute :attr_context parent))
         ;; Context takes precedence over latex
         (attr (or attr-context attr-latex))
         (caption (org-context--caption/label-string parent info))
         (label (org-context--label parent info ))
         (float (let ((float (plist-get attr :float)))
                  (cond ((string= float "wrap") 'wrap)
                        ((string= float "sideways") 'sideways)
                        ((string= float "multicolumn") 'multicolumn))))
         (width (cond ((plist-get attr :width))
                      ((plist-get attr :height) "")
                      ;; TODO Give this some config somehow
                      ((eq float 'multicolumn) "\\dimexpr\\makeupwidth - 1em\\relax")
                      ((eq float 'wrap) "0.48\\hsize")
                      (t (plist-get info :context-image-default-width))))
         (height (cond ((plist-get attr :height))
                       ((or (plist-get attr :width)
                            (memq float '(figure wrap))) "")
                       (t (plist-get info :context-image-default-height))))
         (placement (plist-get attr :placement))
         image-code
         options-list)
    ;; TODO tikz and pgf
    ;; tikz graphics seem to be more trouble than they're worth.
    ;; A lot of the markup has to be stripped to get a conversion.
    ;; TODO Add scale, or width and height to options
    (when (org-string-nw-p width)
      (setq options-list (add-to-list 'options-list (cons "width" width))))
    (when (org-string-nw-p height)
      (setq options-list (add-to-list 'options-list (cons "height" height))))
    (setq image-code
          (format "\\externalfigure[%s][%s]"
                  path
                  (org-context--format-arguments options-list)))
    (let (env-options
          location-options)
      (pcase float
        (`wrap (progn
                 (add-to-list
                  'location-options
                  (or placement (plist-get info :context-float-default-placement)))
                 (or placement (add-to-list 'location-options "here"))))
        (`sideways (progn (add-to-list 'location-options "90")
                          (add-to-list 'location-options "page")))
        (_ (progn
             (add-to-list 'location-options (or placement "Here")))))
      (add-to-list 'env-options
                   (cons "location" (mapconcat 'identity location-options ",")))
      (add-to-list 'env-options
                   (cons "reference" label))
      (when (org-string-nw-p caption)
        (add-to-list 'env-options (cons "title" caption)))
      (format
       "\\startplacefigure[%s]
%s
\\stopplacefigure"
       (org-context--format-arguments env-options)
       image-code))))

;;;; Math Block

(defun org-context-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to ConTeXt.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "\\m{%s}" (org-trim contents))))

;;;; Node Property

(defun org-context-node-property (node-property _contents info)
  "Transcode a NODE-PROPERTY element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((command
         (org-string-nw-p
          (car
           (plist-get info :context-node-property-command))))
        (key (org-element-property :key node-property))
        (value (org-element-property :value node-property)))
    (if command
        (let ((args (org-context--format-arguments
                     (list (cons "key" key) (cons "value" value)))))
          (format "\\%s[%s]" command args))
      (format "%s:%s" key value))))

;;;; Paragraph

(defun org-context-paragraph (_paragraph contents info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (org-context--text-markup contents 'paragraph info))

;;;; Plain List

(defun org-context-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ContTeXt.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (org-export-read-attribute :attr_latex plain-list))
         (bullet
          (org-element-property
           :bullet
           (car (org-element-map plain-list 'item #'identity t))))
         (alphap (string-match-p "\\`[a-zA-Z][.)]" bullet))
         (upperp (and alphap
                      (let ((case-fold-search nil))
                        (string-match-p "\\`[[:upper:]]" bullet))))
         (open-command
          (cond ((eq type 'ordered)
                 (format  "\\startitemize[%s]\n"
                          (if alphap (if upperp "A" "a") "n")))
                ((eq type 'descriptive) "")
                (t "\\startitemize\n")))
         (close-command
          (if (eq type 'descriptive)
              ""
            "\\stopitemize")))
    (org-context--add-reference
     plain-list
     (concat
      open-command
      contents
      close-command)
     info)))

;;;; Plain Text

(defun org-context-plain-text (text info)
  "Transcode a TEXT string from Org to ConTeXt.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let* ((specialp (plist-get info :with-special-strings))
	 (output
	  ;; Turn LaTeX into \LaTeX{} and TeX into \TeX{}.
	  (let ((case-fold-search nil))
	    (replace-regexp-in-string
	     "\\<\\(?:\\(?:La\\)?TeX\\)\\|\\(?:ConTeXt\\)\\>" "\\\\\\&{}"
	     ;; Protect ^, ~, %, #, &, $, _, { and }.  Also protect \.
	     ;; However, if special strings are used, be careful not
	     ;; to protect "\" in "\-" constructs.
	     (replace-regexp-in-string
	      (concat "[%$#&{}_~^]\\|\\\\" (and specialp "\\([^-]\\|$\\)"))
	      (lambda (m)
		(cl-case (string-to-char m)
		  (?\\ "$\\\\backslash$\\1")
		  (?~ "\\\\textasciitilde{}")
		  (?^ "\\\\^{}")
		  (t "\\\\\\&")))
	      text)))))
    (when (plist-get info :with-smart-quotes)
      (setq output (org-context--format-quote output info text)))
    ;; Convert special strings.
    (when specialp
      (setq output (replace-regexp-in-string "\\.\\.\\." "\\\\ldots{}" output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output (replace-regexp-in-string
		    "\\(?:[ \t]*\\\\\\\\\\)?[ \t]*\n" "\\\\\n" output nil t)))
    ;; Return value.
    output))

;;;; Planning

(defun org-context-planning (planning _contents info)
  "Transcode a PLANNING element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((closed (org-element-property :closed planning))
        (deadline (org-element-property :deadline planning))
        (scheduled (org-element-property :scheduled planning))
        (formatter (plist-get info :context-format-timestamp-function))
        (command-name (org-string-nw-p
                       (car (plist-get info :context-planning-command)))))
    (if command-name
        (concat (format "\\%s[" command-name)
                (when closed
                  (concat
                   (format "\nClosedString={%s}," org-closed-string)
                   (format "\nClosedTime={%s}," (funcall formatter closed))))
                (when deadline
                  (concat
                   (format "\nDeadlineString={%s}," org-deadline-string)
                   (format "\nDeadlineTime={%s}," (funcall formatter deadline))))
                (when scheduled
                  (concat
                   (format "\nScheduledString={%s}," org-scheduled-string)
                   (format "\nScheduledTime={%s}," (funcall formatter scheduled))))
                "]")
      (concat
       (when closed (concat org-closed-string (funcall formatter closed)))
       (when deadline (concat org-deadline-string (funcall formatter deadline)))
       (when scheduled (concat org-scheduled-string (funcall formatter scheduled)))))))

;;;; Property Drawer

(defun org-context-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (when (org-string-nw-p contents)
    (org-context--wrap-env
     property-drawer
     contents
     info
     :context-property-drawer-environment)))

;;;; Quote Block

(defun org-context-quote-block (quote-block contents info)
  "Transcodes a QUOTE-BLOCK element from Org to ConTeXt.
CONTENTS is the contents of the block. INFO is a plist containing
contextual information."
  (when (org-string-nw-p contents)
    (org-context--enumerated-block
     quote-block contents info
     :context-blockquote-environment
     :context-enumerate-blockquote-environment
     :context-enumerate-blockquote-empty-environment)))

(defun org-context--format-quote (text info original)
  "Wraps quoted text in `\\quote{}' constructs.
ConTeXt provides facilities for multilingual quoting so
no need to reimplement. TEXT is the text to quote.
INFO is a plist containing contextual information.
ORIGINAL is the original unfiltered text."
  (let ((quote-status
         (copy-sequence (org-export--smart-quote-status (or original text) info))))
    (replace-regexp-in-string
     "['\"]"
     (lambda (match)
       (cdr (assq (pop quote-status)
                  (plist-get info :context-export-quotes-alist)))
)
     text nil t)))

;;;; Radio Target

(defun org-context-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ConTeXt.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\reference[%s]{%s} %s"
          (org-export-get-reference radio-target info)
          text
          text))

;;;; Section

(defun org-context-section (section contents info)
  "Transcode a SECTION element from Org to ConTeXt.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

;;;; Special Block

(defun org-context-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block))
        (opt (org-export-read-attribute :attr_context special-block :options))
        (caption (org-context--caption/label-string special-block info)))
    (concat (format "\\start%s[%s]\n" type (or opt ""))
            contents
            "\\crlf"
            caption
            (format "\\stop%s" type))))

;;;; Src Block

(defun org-context-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let* ((caption (org-trim
                 (org-export-data
                  (or (org-export-get-caption src-block t)
                      (org-export-get-caption src-block))
                  info)))
         (environment
          (car
           (plist-get info
                      (if caption
                          :context-enumerate-listing-environment
                        :context-enumerate-listing-empty-environment))))
         (label (org-context--label src-block info t))
         (code (org-context--preprocess-source-block src-block info)))
    (let ((engine (plist-get info :context-syntax-engine))
          (args (org-context--format-arguments
                 (list
                  (cons "location" "force,split")
                  (cons "title" caption)
                  (cons "reference" label)))))
      (concat
       (format "\\start%s
  [%s]\n"
               environment args)
       (pcase engine
         ('vim (org-context--highlight-src-block-vim
                src-block code info))
         (_ (org-context--highlight-src-block-builtin
             src-block code info)))
       "\n"
       (format "\\stop%s" environment)))))

(defun org-context--preprocess-source-block (src-block info)
  "Format SRC-BLOCK with inline refs.
INFO is a plist containing contextual information."
  (let* ((code-info (org-export-unravel-code src-block))
         (code (car code-info))
         (refs (cdr code-info))
         (num-start (org-export-get-loc src-block info))
         (retain-labels (org-element-property :retain-labels src-block))
         (line-num 0)
         (reffed-code
          (mapconcat
           (lambda (line)
             (cl-incf line-num)
             (let* ((line-ref (assoc line-num refs))
                    (ref (cdr line-ref))
                    (ref-label (org-context--get-coderef-label
                                ref src-block info)))
               (concat
                line
                (when ref
                  (format "    /BTEX%s/ETEX"
                   (concat
                    (cond ((and num-start (not retain-labels))
                           (format "\\someline[%s]" ref-label))
                          ((not retain-labels)
                           (format "\\reference[%s]{%d}" ref-label line-num))
                          (t (format "\\reference[%s]{%s}" ref-label ref)))
                      ;; TODO Provide configuration for visible code labels
                      (when retain-labels
                        (format "\\inright{%s}" ref))))))))
           (split-string code "\n")
           "\n")))
    reffed-code))

(defun org-context--highlight-src-block-builtin (src-block code info)
  "Wraps a source block in the builtin environment for ConTeXt source code.
Use this if you don't have Vim.

SRC-BLOCK is the code object to transcode. CODE is the
preprocessed contents of the code block. INFO is a plist holding
contextual information."
  (let* ((lang (org-context--get-builtin-lang-name src-block info))
         (env-name
          (or
           (org-string-nw-p
            (car (plist-get info :context-block-source-environment)))
           "typing"))
         (num-start (org-export-get-loc src-block info))
         (num-start-str
          (when (and num-start (> num-start 0))
            (number-to-string (+ 1 num-start))))
         (args
          (org-string-nw-p
           (org-context--format-arguments
            (list (cons "start" num-start-str)
                  (cons "numbering" (when num-start "line"))
                  (cons "option" lang))
            t))))
    (format "\\start%s%s\n%s\n\\stop%s"
            env-name
            (if args (format "[%s]" args) "")
            code
            env-name)))

(defun org-context--highlight-src-block-vim (src-block code info)
  "Wraps a source block in a vimtyping environment.
This requires you have Vim installed and the t-vim module for
ConTeXt. SRC-BLOCK is the entity to wrap. CODE is the contents of
the entity. INFO is a plist containing contextual information."
  (let* ((lang-info (org-context--get-vim-lang-info src-block info))
         (context-name (plist-get lang-info :context-block-name))
         (num-start (org-export-get-loc src-block info))
         (num-start-str
          (when (and num-start (> num-start 0))
            (number-to-string (+ 1 num-start))))
         (args
          (org-string-nw-p
           (org-context--format-arguments
            (list (cons "numberstart" num-start-str)
                  (cons "numbering" (when num-start "yes")))
            t))))
    (if context-name
        (format "\\start%s%s\n%s\n\\stop%s"
                context-name
                (if args (format "[%s]" args) "")
                code
                context-name)
      (format "\\starttyping\n%s\\stoptying" code))))

;;;; Statistics Cookie

(defun org-context-statistics-cookie (statistics-cookie _contents _info)
  "Transcode a STATISTICS-COOKIE object from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (replace-regexp-in-string
   "%" "\\%" (org-element-property :value statistics-cookie) nil t))

;;;; Strike-Through

(defun org-context-strike-through (_strike-through contents info)
  "Transcode STRIKE_THROUGH from Org to ConTeXt.
CONTENTS is the contents to strike out. INFO is a plist contextual information."
  (org-context--text-markup contents 'strike-through info))

;;;; Subscript

(defun org-context-subscript (_subscript contents info)
  "Transcode a SUBSCRIPT from Org to ConTeXt.
CONTENTS is the content to subscript. INFO is a plist containing
contextual information."
  (org-context--text-markup contents 'subscript info))

;;;; Superscript

(defun org-context-superscript (_superscript contents info)
  "Transcode a SUPERSCRIPT from Org to ConTeXt.
CONTENTS is the content to subscript. INFO is a plist containing
contextual information."
  (org-context--text-markup contents 'superscript info))

;;;; Table Cell

(defun org-context-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL from Org to ConTeXt.
CONTENTS is the cell contents. INFO is a plist used as
a communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (table-row (org-export-get-parent table-cell))
         (alignment (org-export-table-cell-alignment table-cell info))
         (attr (org-export-read-attribute :attr_context table))
         (first-row-p (not (org-export-get-previous-element table-row info)))
         (last-row-p (not (org-export-get-next-element table-row info)))
         (first-col-p (not (org-export-get-previous-element table-cell info)))
         (last-col-p (not (org-export-get-next-element table-cell info)))
         (alignment (org-export-table-cell-alignment table-cell info))
         (starts-colgroup-p (org-export-table-cell-starts-colgroup-p table-cell info))
         (ends-colgroup-p (org-export-table-cell-ends-colgroup-p table-cell info))
         (first-col-style (or (plist-get attr :w)
                             (org-string-nw-p
                              (plist-get info :context-table-leftcol-style))))
         (last-col-style (or (plist-get attr :e)
                            (org-string-nw-p
                             (plist-get info :context-table-rightcol-style))))
         (top-left-style (or (plist-get attr :nw)
                            (org-string-nw-p
                             (plist-get info :context-table-topleft-style))))
         (top-right-style (or (plist-get attr :ne)
                             (org-string-nw-p
                              (plist-get info :context-table-topright-style))))
         (bottom-left-style (or (plist-get attr :sw)
                               (org-string-nw-p
                                (plist-get info :context-table-bottomleft-style))))
         (bottom-right-style (or (plist-get attr :se)
                                (org-string-nw-p
                                 (plist-get info :context-table-bottomright-style))))
         (starts-colgroup-style (or (plist-get attr :cgs)
                                    (org-string-nw-p
                                     (plist-get info :context-table-colgroup-start-style))))
         (ends-colgroup-style (or (plist-get attr :cge)
                                    (org-string-nw-p
                                     (plist-get info :context-table-colgroup-end-style))))
         (suffix
          (cond ((and first-row-p first-col-p top-left-style) (format "[%s]" top-left-style))
                ((and first-row-p last-col-p top-right-style) (format "[%s]" top-right-style))
                ((and last-row-p first-col-p bottom-left-style) (format "[%s]" bottom-left-style))
                ((and last-row-p last-col-p bottom-right-style) (format "[%s]" bottom-right-style))
                ((and first-col-p first-col-style) (format "[%s]" first-col-style))
                ((and last-col-p last-col-style) (format "[%s]" last-col-style))
                ((and starts-colgroup-p starts-colgroup-style)
                 (format "[%s]" starts-colgroup-style))
                ((and ends-colgroup-p ends-colgroup-style)
                 (format "[%s]" ends-colgroup-style))
                (t "")))
         ;; TODO Consider not applying alignment to contents if alignment not specified
         (alignspec (pcase alignment
                      ('left "\\startalignment[flushleft] %s \\stopalignment")
                      ('right "\\startalignment[flushright] %s \\stopalignment")
                      ('center "\\startalignment[middle] %s \\stopalignment"))))
    (concat
     (format "\\startxcell%s " suffix)
     (when contents (format alignspec contents))
     " \\stopxcell\n")))

;;;; Table Row

(defun org-context-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ConTeXt.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  ;; TODO Allow the user to enable or disable different parts.
  ;; Parts include head, next, body, foot
  (let* ((table (org-export-get-parent-table table-row))
         (attr (org-export-read-attribute :attr_context table))
         (dimensions (org-export-table-dimensions table info))
         (row-num (or (org-export-table-row-number table-row info) 0))
         (row-group-num (or (org-export-table-row-group table-row info) 0))
         (headerp (org-export-table-row-in-header-p table-row info))
         (footerp
          (let* ((last-row-num (org-export-get-parent-element
                                (org-export-get-table-cell-at
                                 (cons (- (car dimensions) 1) (- (cdr dimensions) 1))
                                 table info)))
                 (last-row-group-num (org-export-table-row-group last-row-num info))
                 (table-has-footer-p
                  ;; Table has a footer if it has 3 or more row groups and footer
                  ;; is selected
                  (and (> last-row-group-num 2)
                       (or (plist-member attr :f)
                           (org-string-nw-p
                            (plist-get info :context-table-use-footer)))))
                 (last-row-group-p (and row-group-num (= row-group-num last-row-group-num))))
            (and last-row-group-p table-has-footer-p)))
         (header-style (or (plist-get attr :h)
                           (org-string-nw-p
                            (plist-get info :context-table-header-style))))
         (footer-style (or (plist-get attr :f)
                           (org-string-nw-p
                            (plist-get info :context-table-footer-style))))
         (body-style (or (plist-get attr :b)
                           (org-string-nw-p
                            (plist-get info :context-table-body-style))))
         (header-mid-row-style
          (or (plist-get attr :ho)
              (org-string-nw-p (plist-get info :context-table-header-mid-style))))
         (footer-mid-row-style
          (or (plist-get attr :fo)
              (org-string-nw-p (plist-get info :context-table-footer-mid-style))))
         (header-top-row-style
          (or (plist-get attr :ht)
              (org-string-nw-p (plist-get info :context-table-header-top-style))))
         (footer-top-row-style
          (or (plist-get attr :ft)
              (org-string-nw-p (plist-get info :context-table-footer-top-style))))
         (header-bottom-row-style
          (or (plist-get attr :hb)
              (org-string-nw-p (plist-get info :context-table-header-bottom-style))))
         (footer-bottom-row-style
          (or (plist-get attr :hb)
              (org-string-nw-p (plist-get info :context-table-footer-bottom-style))))

         (row-group-start-style
          (or (plist-get attr :rgs)
              (org-string-nw-p
               (plist-get info :context-table-rowgroup-start-style))))
         (row-group-end-style
          (or (plist-get attr :rge)
              (org-string-nw-p
               (plist-get info :context-table-rowgroup-end-style))))
         (first-row-style
          (or
           (or (plist-get attr :n)
               (org-string-nw-p
                (plist-get info :context-table-toprow-style)))
           row-group-start-style))
         (last-row-style
          (or
           (or (plist-get attr :s)
               (org-string-nw-p
                (plist-get info :context-table-bottomrow-style)))
           row-group-end-style))
         (first-row-p (= row-num 0))
         (last-row-p (= row-num (- (car dimensions) 1)))
         (starts-row-group-p (org-export-table-row-starts-rowgroup-p table-row info))
         (ends-row-group-p (org-export-table-row-ends-rowgroup-p table-row info))
         (wrappedcontents
          (when contents
            (format "\\startxrow%s\n%s\\stopxrow\n"
                    ;; TODO The order of this should maybe be configurable
                    (cond ((and headerp
                                (org-export-table-row-starts-header-p table-row info)
                                (org-export-table-row-ends-header-p table-row info))
                           "")
                          ((and headerp
                                (org-export-table-row-starts-header-p table-row info)
                                header-top-row-style)
                           (format "[%s]" header-top-row-style))
                          ((and headerp
                                (org-export-table-row-ends-header-p table-row info)
                                header-bottom-row-style)
                           (format "[%s]" header-bottom-row-style))
                          ((and headerp header-mid-row-style)
                           (format "[%s]" header-mid-row-style))
                          ;; footer
                          ((and footerp
                                (org-export-table-row-starts-rowgroup-p table-row info)
                                (org-export-table-row-ends-rowgroup-p table-row info))
                           "")
                          ((and footerp
                                (org-export-table-row-starts-rowgroup-p table-row info)
                                footer-top-row-style)
                           (format "[%s]" footer-top-row-style))
                          ((and footerp
                                (org-export-table-row-ends-rowgroup-p table-row info)
                                footer-bottom-row-style)
                           (format "[%s]" footer-bottom-row-style))
                          ((and footerp footer-mid-row-style)
                           (format "[%s]" footer-mid-row-style))
                          ((and first-row-p first-row-style)
                           (format "[%s]" first-row-style))
                          ((and last-row-p last-row-style)
                           (format "[%s]" last-row-style))
                          ((and ends-row-group-p row-group-end-style)
                           (format "[%s]" row-group-end-style))
                          ((and starts-row-group-p row-group-start-style)
                           (format "[%s]" row-group-start-style))
                          (t ""))
                    contents)))
         (group-tags
          (cond
           (headerp
            (list "\\startxtablehead%s\n" header-style "\\stopxtablehead"))
           (footerp
            (list "\\startxtablefoot%s\n" footer-style "\\stopxtablefoot"))
           (t (list "\\startxtablebody%s\n" body-style "\\stopxtablebody")))))
    (concat (and starts-row-group-p
                 (format (nth 0 group-tags)
                         (if (org-string-nw-p (nth 1 group-tags))
                             (format "[%s]" (nth 1 group-tags))
                           "")))
            wrappedcontents
            (and ends-row-group-p (nth 2 group-tags)))))

;;;; Table

(defun org-context-table (table contents info)
  "Return appropriate ConTeXt code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."

  (let* ((attr (org-export-read-attribute :attr_context table))
         (caption (org-context--caption/label-string table info))
         (label (org-context--label table info t))
         (location (or (plist-get attr :location) "force,here"))
         (header (or (plist-get attr :header)
                     ;; TODO add `:org-context-header' option
                     (plist-get info :org-context-header)))
         (footer (or (plist-get attr :footer)
                     ;; TODO add `:org-context-footer' option
                     (plist-get info :org-context-footer)))
         (option (or (plist-get attr :option)
                     ;; TODO add `:org-context-table-option' option
                     (plist-get info :org-context-table-option)))
         (table-style (or (plist-get attr :table-style)
                    ;; TODO add `org-context-table-style'
                    (plist-get info :org-context-table-style)))
         (float-style (or (plist-get attr :float-style)
                          ;; TODO add `org-context-table-float-style'
                          (plist-get info :org-context-table-float-style)))
         (split (plist-get attr :split))
         (location-string (concat (when split "split,") location))
         (float-args (org-context--format-arguments
                      (list
                       (cons "location" location-string)
                       (cons "title" caption)
                       (cons "reference" label))))
         (table-args (org-context--format-arguments
                      (list
                       (cons "split" (when split "yes"))
                       (cons "header" (when (string= header "repeat") "repeat"))
                       (cons "footer" (when (string= footer "repeat") "repeat"))
                       (cons "option" option))))
         (first-row (org-element-map table 'table-row
                      (lambda (row)
                        (and (eq (org-element-property :type row) 'standard) row))
                      info 'first-match))
         (cells
          (org-element-map first-row 'table-cell 'identity))
         (widths
          (mapcar (lambda (cell)
                    (let* ((raw-width (org-export-table-cell-width cell info)))
                      (if raw-width (format "[width=%.2fem]" raw-width) "")))
                  cells)))
    (concat
     (format
      "\\startplacetable%s%s
\\startxtable%s%s
%s"
      (if (org-string-nw-p float-style) (format "\n[%s]" float-style) "")
      (if (org-string-nw-p float-args) (format "\n[%s]" float-args) "")
      (if (org-string-nw-p table-style) (format "\n[%s]" table-style) "")
      (if (org-string-nw-p table-args) (format "\n[%s]" table-args) "")
      contents)
     (when (cl-some 'org-string-nw-p widths)
       (concat
        "\\startxrow[empty=yes,offset=-1pt,height=0pt]\n"
        (mapconcat (lambda (w)
                     (if w
                         (format "\\startxcell%s\\stopxcell" w)
                       "\\startxcell\\stopxcell"))
                   widths
                   "\n")
        "\n\\stopxrow"))
     "\\stopxtable
\\stopplacetable\n"
     )))

;;;; Target

(defun org-context-target (target _contents info)
  "Transcode a TARGET object from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  ;; TODO This should refer to the section header name,
  ;; not the plain text of the section.
  (format "\\reference[%s]{%s}" (org-context--label target info)
          (org-export-get-node-property :value target)))

;;;; Template

(defun org-context-template (contents info)
  "Return complete document string after ConTeXt conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding the export options."
  (let* ((time-stamp (plist-get info :time-stamp-file))
         (header-lines (list (plist-get info :context-header)))
         (with-toc (plist-get info :with-toc))
         (headline-levels (plist-get info :headline-levels))
         (with-section-numbers (plist-get info :section-numbers))
         (metadata (org-context--list-metadata info))
         (header-extra-lines (list (plist-get info :context-header-extra)))
         (preset-name (plist-get info :context-preset))
         (preset-data (cdr (assoc preset-name (plist-get info :context-presets))))
         (preset-header-string (car preset-data))
         (preset-header-snippets
          (org-context--get-snippet-text info (cdr preset-data)))
         (user-snippets (org-context--get-snippet-text info (plist-get info :context-snippet)))
         (command-defs
          (let ((deflist
                  (list
                   (list
                    :context-blockquote-environment
                    "% blockquote environment")
                   (list
                    :context-block-source-environment
                    "% Create the block source environment")
                   (list
                    :context-bullet-on-command
                    "% Define on bullet command")
                   (list
                    :context-bullet-off-command
                    "% Define off bullet command")
                   (list
                    :context-bullet-trans-command
                    "% Define incomplete bullet command")
                   (list
                    :context-description-command
                    "% LaTeX-style descriptive enumerations")
                   (list
                    :context-enumerate-blockquote-empty-environment
                    "% Unlabelled blockquote enumeration environment")
                   (list
                    :context-enumerate-blockquote-environment
                    "% blockquote enumeration environment")
                   (list
                    :context-enumerate-example-empty-environment
                    "% Create the unlabelled example enumeration environment")
                   (list
                    :context-enumerate-example-environment
                    "% Create the example enumeration environment")
                   (list
                    :context-enumerate-listing-empty-environment
                    "% Create the unlabelled listings enumeration environment")
                   (list
                    :context-enumerate-listing-environment
                    "% Create the listings enumeration environment")
                   (list
                    :context-enumerate-verse-empty-environment
                    "% Create the unlabelled verse enumeration environment")
                   (list
                    :context-enumerate-verse-environment
                    "% Create a verse enumeration environment")
                   (list
                    :context-example-environment
                    "% Create the example environment")
                   (list
                    :context-fixed-environment
                    "% Create the fixed width environment")
                   (list
                    :context-planning-command
                    "% Define a basic planning command")
                   (list
                    :context-inline-source-environment
                    "% Create the inline source environment")
                   (list
                    :context-inlinetask-command
                    "% Define a basic inline task command")
                   (list
                    :context-headline-command
                    "% Define a basic headline command")
                   (list
                    :context-clock-command
                    "% Define a basic clock command")
                   (list
                    :context-drawer-command
                    "% Define a basic drawer command")
                   (list
                    :context-node-property-command
                    "% Define a command for node properties in drawers")
                   (list
                    :context-property-drawer-environment
                    "% Create a property drawer style")
                   (list
                    :context-title-command
                    "% Create an empty title command to be overridden by user")
                   (list
                    :context-title-contents-command
                    "% Create a TOC header command")
                   (list
                    :context-titlepage-environment
                    "% Create the title page style")
                   (list
                    :context-verse-environment
                    "% Create a verse style"))))
            (mapconcat
             (lambda (args)
               (let* ((kw (nth 0 args))
                      (comment (nth 1 args))
                      (nameimpl (plist-get info kw))
                      (impl (cdr nameimpl)))
                 (concat
                  comment
                  "\n"
                  (when (org-string-nw-p impl) impl))))
             deflist
             "\n")))
         (table-defs
          (mapconcat
           'identity
           (seq-filter
            'identity
            (delete-dups
             (mapcar
              (lambda
                (kw)
                (let ((style (plist-get info kw)))
                  (when (org-string-nw-p style) (format "\\setupxtable[%s][]" style))))
              (list :context-table-toprow-style
                    :context-table-bottomrow-style
                    :context-table-leftcol-style
                    :context-table-rightcol-style
                    :context-table-topleft-style
                    :context-table-topright-style
                    :context-table-bottomleft-style
                    :context-table-bottomright-style
                    :context-table-header-style
                    :context-table-footer-style
                    :context-table-header-top-style
                    :context-table-footer-top-style
                    :context-table-header-bottom-style
                    :context-table-footer-bottom-style
                    :context-table-header-mid-style
                    :context-table-footer-mid-style
                    :context-table-body-style
                    :context-table-rowgroup-start-style
                    :context-table-rowgroup-end-style
                    :context-table-colgroup-start-style
                    :context-table-colgroup-end-style))))
           "\n"))
         (unnumbered-headline-commands
          (let* ((notoc-headline-cache (plist-get info :context-notoc-headline-cache))
                 (notoc-headline-keys
                  (when notoc-headline-cache
                    (hash-table-keys notoc-headline-cache))))
            (mapconcat
             (lambda (key)
               (let ((val (gethash key notoc-headline-cache)))
                 (format "\\definehead[%s][%s]" val key)))
             notoc-headline-keys
             "\n")))
         (vimp (eq (plist-get info :context-syntax-engine) 'vim))
         (vim-lang-hash (when vimp
                          (plist-get info :context-languages-used-cache)))
         (vim-langs
          (when (and vimp vim-lang-hash)
            (mapconcat
             (lambda (key)
               (let* ((lang-info (gethash key vim-lang-hash))
                      (vim-lang (plist-get lang-info :vim-lang))
                      (context-inline-name (plist-get lang-info :context-inline-name))
                      (context-block-name (plist-get lang-info :context-block-name))
                      (def-template "\\definevimtyping[%s]\n  [syntax=%s,escape=command]\n"))
                 (concat
                  (format def-template
                          context-inline-name vim-lang)
                  (format def-template
                         context-block-name vim-lang))))
             (hash-table-keys vim-lang-hash)
             "\n")))
         (bib-place (plist-get info :context-bib-command)))
    (concat
     (and time-stamp
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     (when vimp "\n\\usemodule[vim]\n")
     "\n"
     unnumbered-headline-commands
     (when bib-place (format "\n%s\n" bib-place))
   "
%===============================================================================
% From CONTEXT_HEADER
%===============================================================================
"
   (mapconcat #'org-element-normalize-string
              header-lines
              "")

   "
%===============================================================================
% Table of Contents Configuration
%===============================================================================
"
   (when (and with-toc (not with-section-numbers))
     "% Add internal numbering to unnumbered sections so they can be included in TOC
\\setuphead[subject]
          [incrementnumber=yes,
            number=no]
\\setuphead[subsubject]
          [incrementnumber=yes,
            number=no]
\\setuphead[subsubsubject]
          [incrementnumber=yes,
            number=no]
\\setuphead[subsubsubsubject]
          [incrementnumber=yes,
            number=no]
")
   (when (and (wholenump headline-levels)
            (/= headline-levels 0))
     (format "\\setupcombinedlist[content][list={%s}]\n"
             (org-context--get-all-headline-commands headline-levels)))
   "
%===============================================================================
% Document Metadata
%===============================================================================
"
   (format "\\setupdocument[%s]\n"
           (org-context--format-arguments metadata))
   (format "\\language[%s]" (cdr (assoc "metadata:language" metadata)))
   "
%===============================================================================
% Define Environments and Commands
%===============================================================================

% Turn on interaction to make links work
\\setupinteraction[state=start]
"
   vim-langs
   "\n"
   command-defs
   "\n"
   table-defs
   "
%===============================================================================
% Preset Commands
%===============================================================================
"
   (concat preset-header-string "\n"
           (mapconcat 'identity preset-header-snippets "\n"))
   "
%===============================================================================
% Snippet Commands
%===============================================================================
"
   (mapconcat 'identity
              user-snippets
              "\n")
   "
%===============================================================================
% Commands from CONTEXT_HEADER_EXTRA
%===============================================================================
"
   (mapconcat #'org-element-normalize-string
              header-extra-lines
              "\n\n")
   "
%===============================================================================
% Document Body
%===============================================================================
\\starttext
\\placebookmarks
"
   contents
   "\\stoptext\n")))

(defun org-context--list-metadata (info)
  "Create a format-spec for document meta-data.
INFO is a plist used as a communication channel."
  (list
    (cons "metadata:author" (org-export-data (plist-get info :author) info))
    (cons "metadata:title" (org-export-data (plist-get info :title) info))
    (cons "metadata:email" (org-export-data (plist-get info :email) info))
    (cons "metadata:subtitle" (org-export-data (plist-get info :subtitle) info))
    (cons "metadata:keywords" (org-export-data (org-context--wrap-latex-math-block
                                                (plist-get info :keywords) info)
                                               info))
    (cons "metadata:description" (org-export-data (org-context--wrap-latex-math-block
                                                   (plist-get info :description) info)
                                                  info))
    (cons "metadata:creator" (plist-get info :creator))
    (cons "metadata:language" (plist-get info :language))
    (cons "Lang" (capitalize (plist-get info :language)))
    (cons "metadata:date" (org-export-data (org-export-get-date info) info))
    (cons "letter:fromaddress" (org-export-data (plist-get info :from-address) info))
    (cons "metadata:phonenumber" (org-export-data (plist-get info :phone-number) info))
    (cons "metadata:url" (org-export-data (plist-get info :url) info))
    (cons "letter:toaddress" (org-export-data (plist-get info :to-address) info))
    (cons "letter:toname" (org-export-data (plist-get info :to-name) info))
    (cons "letter:attention" (org-export-data (plist-get info :attention) info))
    (cons "location" (org-export-data (plist-get info :place) info))
    (cons "letter:location" (org-export-data (plist-get info :location) info))
    (cons "metadata:subject" (org-export-data (plist-get info :subject) info))
    (cons "letter:opening" (org-export-data (plist-get info :opening) info))
    (cons "letter:closing" (org-export-data (plist-get info :closing) info))
    (cons "letter:signature" (org-export-data (plist-get info :signature) info))
    (cons "letter:foldmarks" (if (plist-get info :with-foldmarks) "start" "stop"))
    (cons "letter:withbackaddress" (if (plist-get info :with-backaddress) "start" "stop"))))

(defun org-context--get-snippet-text (info snippet-names)
  "Return snippets given a list of SNIPPET NAMES.
SNIPPET-NAMES is a list of snippet names to look up.
INFO is a plist used as a communication channel."
  (mapcar
   (lambda (snippet-name)
     (cdr (assoc
           snippet-name
           (plist-get info :context-snippets))))
   snippet-names))
 
;;;; Timestamp

(defun org-context-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (funcall (plist-get info :context-format-timestamp-function) timestamp))

(defun org-context-format-timestamp-default-function (timestamp)
  "Transcode a TIMESTAMP from Org to ConTeXt."
  (let* ((time (org-timestamp-to-time timestamp))
         (year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (day (format-time-string "%d")))
    (format "\\date[d=%s,m=%s,y=%s]" day month year)))

;;;; Underline

(defun org-context-underline (_underline contents info)
  "Transcode UNDERLINE from Org to ConTeXt.
CONTENTS is the content to underline. INFO is a plist containing
contextual information."
  (org-context--text-markup contents 'underline info))

;;;; Verbatim

(defun org-context-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to ConTeXt.
CONTENTS is the content to mark up. INFO is a plist containing
contextual information."
  (org-context--text-markup
   (org-element-property :value verbatim) 'verbatim info))

;;;; Verse Block

(defun org-context-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ConTeXt.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p contents)
    (org-context--enumerated-block
     verse-block contents info
     :context-verse-environment
     :context-enumerate-verse-environment
     :context-enumerate-verse-empty-environment)))

;;; End-user functions

(defun org-context--collect-warnings (buffer)
  "Collect some warnings from \"pdflatex\" command output.
BUFFER is the buffer containing output.  Return collected
warnings types as a string, `error' if a ConTeXt error was
encountered or nil if there was none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[ \t]*This is .*?TeX.*?Version" nil t)
        (if (re-search-forward "^!" nil t) 'error
          (let ((case-fold-search t)
                (warnings ""))
            (dolist (warning org-latex-known-warnings)
              (when (save-excursion (re-search-forward (car warning) nil t))
                (setq warnings (concat warnings " " (cdr warning)))))
            (org-string-nw-p (org-trim warnings))))))))

;;;###autoload
(defun org-context-export-as-context
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a ConTeXt buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\starttext\" and \"\\stoptext\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONTEXT Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'context "*Org CONTEXT Export*"
    async subtreep visible-only body-only ext-plist (lambda () (ConTeXt-mode))))

;;;###autoload
(defun org-context-export-to-context
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a ConTeXt file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\starttext\" and \"\\stoptext\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((file (org-export-output-file-name ".mkiv" subtreep)))
    (org-export-to-file 'context file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-context-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to ConTeXt then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\starttext\" and \"\\stoptext\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".mkiv" subtreep)))
    (org-export-to-file 'context outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-context-compile file)))))

(defun org-context-compile (texfile &optional snippet)
  "Compile a ConTeXt file.

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-context-pdf-process',
which see.  Output is redirected to \"*Org PDF ConTeXt Output*\"
buffer.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not remove log files.

Return PDF file name or raise an error if it couldn't be
produced."
  (unless snippet (message "Processing ConTeXt file %s..." texfile))
  (let* (;; TODO bibtex compiler options?
         (process org-context-pdf-process)
         ;; TODO bibtex spec?
         (log-buf-name "*Org PDF ConTeXt Output*")
         (log-buf (and (not snippet) (get-buffer-create log-buf-name)))
         (outfile (org-compile-file texfile process "pdf"
                                    (format "See %S for details" log-buf-name)
                                    log-buf )))
    (unless snippet
      (when org-context-remove-logfiles
        (mapc #'delete-file
              (directory-files
               (file-name-directory outfile)
               t
               (format "%s\\(?:\\(?:%s\\)\\|\\(?:%s\\)\\)"
                       (regexp-quote (file-name-base outfile))
                       (concat "\\(?:\\.[0-9]+\\)?\\."
                               (regexp-opt org-context-logfiles-extensions))
                       "-temp-[[:alnum:]]+-[0-9]+\\.vimout")
               t)))
      ;; LaTeX warnings should be close enough to ConTeXt warnings
      (let ((warnings (org-context--collect-warnings log-buf)))
        (message (concat "PDF file produced"
                         (cond
                          ((eq warnings 'error) " with errors.")
                          (warnings (concat " with warnings: " warnings))
                          (t "."))))))
    ;; Return output file name.
    outfile))

(provide 'ox-context)
;;; ox-context.el ends here
