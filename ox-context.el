;; Custom exporter for ConTeXt
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

(require 'cl-lib)
(require 'ox-latex)
(org-export-define-derived-backend
    'context 'latex
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
                  (:filter-parse-tree org-context-math-block-tree-filter
                                      ;;org-context-matrices-tree-filter
                                      ;; org-context-image-link-filter
                                      )
                  (:filter-verse-block . org-context-clean-invalid-line-breaks))
 :options-alist '((:context-float-default-placement nil nil org-context-float-default-placement)
                  (:context-format-clock-function nil nil org-context-format-clock-function)
                  (:context-format-drawer-function nil nil org-context-format-drawer-function)
                  (:context-format-inlinetask-function nil nil org-context-format-inlinetask-function)
                  (:context-format-headline-function nil nil org-context-format-headline-function)
                  (:context-format-timestamp-function nil nil org-context-format-timestamp-function)
                  (:context-header "CONTEXT_HEADER" nil nil newline)
                  (:context-description-command nil nil org-context-description-command)
                  (:context-blockquote-environment nil nil org-context-blockquote-environment)
                  (:context-example-environment nil nil org-context-example-environment)
                  (:context-fixed-environment nil nil org-context-fixed-environment)
                  (:context-inline-source-environment nil nil org-context-inline-source-environment)
                  (:context-block-source-environment nil nil org-context-block-source-environment)
                  (:context-titlepage-environment nil nil org-context-titlepage-environment)
                  (:context-verse-environment nil nil org-context-verse-environment)
                  (:context-property-drawer-environment nil nil org-context-property-drawer-environment)
                  (:context-body-environment nil nil org-context-body-environment)
                  (:context-title-command nil nil org-context-title-command)
                  (:context-title-contents-command nil nil org-context-title-contents-command)
                  (:context-bullet-on-command nil nil org-context-bullet-on-command)
                  (:context-bullet-off-command nil nil org-context-bullet-off-command)
                  (:context-bullet-trans-command nil nil org-context-bullet-trans-command)
                  (:context-planning-command nil nil org-context-planning-command)
                  (:context-inline-task-command nil nil org-context-inline-task-command)
                  (:context-headline-command nil nil org-context-headline-command)
                  (:context-clock-command nil nil org-context-clock-command)
                  (:context-drawer-command nil nil org-context-drawer-command)
                  (:context-image-default-scale nil nil org-context-image-default-scale)
                  (:context-image-default-height nil nil org-context-image-default-height)
                  (:context-image-default-width nil nil org-context-image-default-width)
                  (:context-image-default-option nil nil org-context-image-default-option)
                  (:context-snippet "CONTEXT_SNIPPET" nil nil split)
                  (:context-snippets nil nil org-context-snippets-alist)
                  (:context-table-leftcol-style nil nil org-context-table-leftcol-style)
                  (:context-table-rightcol-style nil nil org-context-table-rightcol-style)
                  (:context-table-toprow-style nil nil org-context-table-toprow-style)
                  (:context-table-bottomrow-style nil nil org-context-table-bottomrow-style)
                  (:context-table-topleft-style nil nil org-context-table-topleft-style)
                  (:context-table-topright-style nil nil org-context-table-topright-style)
                  (:context-table-bottomleft-style nil nil org-context-table-bottomleft-style)
                  (:context-table-bottomright-style nil nil org-context-table-bottomright-style)
                  (:context-table-header-top-style nil nil org-context-table-header-top-style)
                  (:context-table-footer-top-style nil nil org-context-table-footer-top-style)
                  (:context-table-header-style nil nil org-context-table-header-style)
                  (:context-table-footer-style nil nil org-context-table-footer-style)
                  (:context-table-header-bottom-style nil nil org-context-table-header-bottom-style)
                  (:context-table-footer-bottom-style nil nil org-context-table-footer-bottom-style)
                  (:context-table-header-mid-style nil nil org-context-table-header-mid-style)
                  (:context-table-footer-mid-style nil nil org-context-table-footer-mid-style)
                  (:context-table-body-style nil nil org-context-table-body-style)
                  (:context-table-rowgroup-start-style nil nil org-context-table-rowgroup-start-style)
                  (:context-table-rowgroup-end-style nil nil org-context-table-rowgroup-end-style)
                  (:context-table-colgroup-start-style nil nil org-context-table-colgroup-start-style)
                  (:context-table-colgroup-end-style nil nil org-context-table-colgroup-end-style)
                  (:context-table-use-footer nil "tablefoot" org-context-table-use-footer)
                  (:context-preset "CONTEXT_PRESET" nil org-context-default-preset t)
                  (:context-number-equations nil "numeq" org-context-number-equations)
                  (:context-presets nil nil org-context-presets-alist)
                  (:context-header-extra "CONTEXT_HEADER_EXTRA" nil nil newline)
                  (:context-syntax-engine nil "syntax" org-context-syntax-engine)
                  (:context-highlighted-langs nil nil org-context-highlighted-langs)
                  (:context-vim-langs nil nil org-context-vim-langs)
                  (:context-vim-color-scheme nil "color-scheme" org-context-vim-color-scheme)
                  (:context-vim-colors nil nil org-context-vim-colors)
                  (:context-text-markup-alist nil nil org-context-text-markup-alist)
                  (:context-export-quotes-alist nil nil org-context-export-quotes-alist)
                  (:description "DESCRIPTION" nil nil parse)
                  (:keywords "KEYWORDS" nil nil parse)
                  (:subtitle "SUBTITLE" nil nil parse)
                  (:date "DATE" nil "\\currentdate" parse)
                  (:from-address "FROM_ADDRESS" nil org-context-from-address newline)
                  (:phone-number "PHONE_NUMBER" nil org-context-phone-number)
                  (:url "URL" nil org-context-url)
                  (:from-logo "FROM_LOGO" nil org-context-from-logo)
                  (:to-address "TO_ADDRESS" nil nil newline)
                  (:to-name "TO_NAME" nil nil newline)
                  (:attention "ATTENTION" nil nil newline)
                  (:place "PLACE" nil org-context-place)
                  (:location "LOCATION" nil org-context-location)
                  (:subject "SUBJECT" nil nil parse)
                  (:opening "OPENING" nil org-context-opening parse)
                  (:closing "CLOSING" nil org-context-closing parse)
                  (:signature "SIGNATURE" nil org-context-closing parse)
                  (:with-backaddress nil "backaddress" org-context-use-backaddress)
                  (:with-email nil "email" org-context-use-email)
                  (:with-foldmarks nil "foldmarks" org-context-use-foldmarks)
                  (:with-phone nil "phone" org-context-use-phone)
                  (:with-url nil "url" org-context-use-url)
                  (:with-from-logo nil "from-logo" org-context-use-from-logo)
                  (:with-place nil "place" org-context-use-place))
 :translate-alist '((bold . org-context-bold)
                    (center-block . org-context-center-block)
                    (code . org-context-code)
                    (clock . org-context-clock)
                    (drawer . org-context-drawer)
                    (entity . org-context-entity)
                    (example-block . org-context-example-block)
                    (export-block . org-context-export-block)
                    (fixed-width . org-context-fixed-width)
                    ;;(footnote-definition . org-context-footnote-definition)
                    (footnote-reference . org-context-footnote-reference)
                    (headline . org-context-headline)
                    (horizontal-rule . org-context-horizontal-rule)
                    (inline-src-block . org-context-inline-src-block)
                    (inlinetask . org-context-inlinetask)
                    (italic . org-context-italic)
                    (item . org-context-item)
                    (keyword . org-context-keyword)
                    (latex-environment . org-context-latex-environment)
                    (latex-fragment . org-context-latex-fragment)
                    (line-break . org-context-line-break)
                    (link . org-context-link)
                    (paragraph . org-context-paragraph)
                    (plain-list . org-context-plain-list)
                    (plain-text . org-context-plain-text)

                    (planning . org-context-planning)
                    (property-drawer . org-context-property-drawer)
                    (quote-block . org-context-quote-block)
                    (radio-target . org-context-radio-target)
                    (section . org-context-section)
                    (src-block . org-context-src-block)
                    (special-block . org-context-special-block)
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
                    (latex-math-block . org-context-math-block)
                    ;;(latex-matrices . org-context-matrices)
                    ))



(defgroup org-export-context nil
  "Options for exporting to ConTeXt."
  :tag "Org ConTeXt"
  :group 'org-export)

(defcustom org-context-description-command (cons "OrgDesc" "")
  "The command name to be used for Org description items.

If nil, \"\\description\" is used"
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-blockquote-environment (cons "OrgBlockQuote" "")
  "The environment name of the block quote environment.

If nil, block quotes aren't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-example-environment (cons "OrgExample" "")
  "The environment name of the example environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-fixed-environment (cons "OrgFixed" "")
  "The environment name of the fixed-width environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-inline-source-environment (cons "OrgInlineSrc" "")
  "The environment name of the inline source environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-block-source-environment (cons "OrgBlkSrc" "")
  "The environment name of the block source environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-titlepage-environment (cons "OrgTitlePage" "")
  "The environment name that wraps title pages.

If nil, title pages aren't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-verse-environment (cons "OrgVerse" "")
  "The environment name of the verse environment.

If nil, verses aren't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-property-drawer-environment (cons "OrgBlkSrc" "")
  "The environment name of the property drawer environment.

If nil, examples are enclosed in \"\\starttyping\" / \"\\stoptying\""
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-body-environment (cons  "OrgBody" "")
  "The environment name that wraps the document body.

If nil, the document body isn't delimited."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-title-command (cons "OrgMakeTitle" "\\define\\OrgMakeTitle{}")
  "The name of the command that creates the document title.

If nil, the document title command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-title-contents-command (cons "OrgTitleContents" "\\define\\OrgTitleContents{%
  {\\tfc Contents}
}")
  "The name of the command that titles the table of contents.

If nil, the table of contents title command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-bullet-on-command (cons "OrgItemOn"  "\\define\\OrgItemOn{\\boxplus}")
  "The name of the command that creates bullets for completed items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-bullet-off-command (cons "OrgItemOff" "\\define\\OrgItemOff{\\square}")
  "The name of the command that creates bullets for uncompleted items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-bullet-trans-command (cons "OrgItemTrans" "\\define\\OrgItemTrans{\\boxtimes}")
  "The name of the command that creates bullets for partially completed items.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-planning-command (cons "OrgPlanning" "\\def\\OrgPlanning#1[#2]{%
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
}")
  "The name of the command that formats planning items.

If nil, just returns a plain text time stamp and label.

Receives the following keyword arguments:

ClosedString
ClosedTime
DeadlineString
DeadlineTime
ScheduledString
ScheduledTime "
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-inline-task-command (cons  "OrgInlineTask" "\\def\\OrgInlineTask#1[#2]{%
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
  \\startframedtext[align=normal, location=middle, width=0.6\\textwidth]
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

(defcustom org-context-headline-command (cons "OrgHeadline" "\\def\\OrgHeadline#1[#2]{%
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
  \\doifnot{\\OrgHeadlineTags}{}{{\\space\\tt\\OrgHeadlineTags}}%
}")
  "The name of the command that formats headlines.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-clock-command (cons "OrgClock" "\\def\\OrgClock#1[#2]{%
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

(defcustom org-context-drawer-command (cons "OrgDrawer" "\\define[2]\\OrgDrawer{#2}")
  "The name of the command that formats drawers.

If nil, the command isn't created."
  :group 'org-export-context
  :type '(cons string string))

(defcustom org-context-closing ""
  "Letter's closing, as a string.
This option can also be set with the CLOSING keyword."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-default-preset "empty"
  "A preamble with no style settings for the document elements."
  :group 'org-export-context
  :type '(string :tag "ConTeXt preset"))

(defconst org-context-export-quotes-alist
  '((primary-opening . "\\quotation{")
    (primary-closing . "}")
    (secondary-opening . "\\quote{")
    (secondary-closing . "}")
    (apostrophe . "'")))

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

(defcustom org-context-syntax-engine
  'vim
  "Option for the syntax engine."
  :tag "Default Syntax Engine"
  :group 'org-export-context
  :type '(choice (const :tag "Vim" vim)
                 (const :tag "Default" default)))

(defcustom org-context-highlighted-langs
  '((metapost "mp"))
  "Alist mapping languages to their counterpart in
ConTeXt. ConTeXt only supports a couple of languages
out-of-the-box so this is a short list."
  :group 'org-export-context
  :type '(repeat
          (list
           (symbol :tag "Major mode      ")
           (symbol :tag "ConTeXt language"))))

(defcustom org-context-vim-langs
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

(defcustom org-context-vim-color-scheme
  'pigmints
  "The name of the default color scheme to use from `org-context-vim-colors'."
  :group 'org-export-context
  :type 'string)

(defcustom org-context-vim-colors
  (list (cons 'pscolor "")
   (cons 'pigmints "% Syntax highlighting based on the Pygments default style
\\startcolorscheme[pigmints]
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
    [Conditional]
    [color={h=008000}, style=bold]
  \\definesyntaxgroup
    [Repeat]
    [color={h=008000}, style=bold]
  % Don't know
  \\definesyntaxgroup
    [Label]
    [color={h=B00040}, style=bold]
  \\definesyntaxgroup
    [Keyword]
    [color={h=008000}, style=bold]
  \\definesyntaxgroup
    [Function]
    [color={h=0000ff}]
  \\definesyntaxgroup
    [Macro]
    [color={h=0000ff}]
  \\definesyntaxgroup
    [String]
    [color={x=BA2121}]
\\stopcolorscheme"))
  "Color Scheme as defined by the t-vim context library.
Cons of NAME, DEFINITION where NAME is the name of the scheme
to use and DEFINITION is the code to add to the preamble to define
the scheme."
  :group 'org-export-context
  :type '(repeat
          (cons
           (symbol :tag "Scheme Name")
           (string :tag "Scheme Definition"))))

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

(defcustom org-context-image-default-scale ""
  "Default scale for images.
Scale overrides width and height."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-image-default-width ""
  "Default width for images."
  :group 'org-export-context
  :type 'string
  :safe #'stringp)

(defcustom org-context-location ""
  "Sender's extension field, as a string.

This option can also be set with the LOCATION keyword. "
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
  "Non-nil means insert a \\placeformula line before all formulas
to allow numbering."
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
  '(("empty"
     :preamble ("")
     :starttext ("")
     :stoptext (""))
    ("article"
     :preamble
     ("\\setupwhitespace[big]"
      "layout-article"
      "description-article"
      "quote-article"
      "verse-article"
      "table-article"
      "title-article"
      "sectioning-article"
      "page-numbering-article"))
    ("report"
     :preamble
     ("\\setupwhitespace[big]"
      "layout-article"
      "description-article"
      "quote-article"
      "verse-article"
      "table-article"
      "title-report"
      "headlines-report"
      "page-numbering-article"))
    ("letter"
     :preamble
     ("\\setupwhitespace[big]
\\usemodule[letter]
\\setupletter[
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
  [state=\\documentvariable{letter:withbackaddress}]")
     :starttext ("\\startletter")
     :stoptext ("\\stopletter")))
  ;; TODO update doc
  "Alist of ConTeXt preamble presets. "
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
    ("page-numbering-article" . "\\setuppagenumbering[location=footer,middle]"))
  "Alist of snippet names and associated text. These snippets will be
inserted into the document preamble when calling `org-context-make-template'.
These snippets are also available for use in presets.
See also `:context-presets'"
  :group 'org-export-context
  :type `(repeat
          (cons
           (string :tag "Snippet Name")
           (string :tag "Snippet Value"))))


(defconst org-context-table-leftcol-style "OrgTableLeftCol"
  "The default style name for the left column in tables.")

(defconst org-context-table-rightcol-style "OrgTableRightCol"
  "The default style name for the right column in tables.")

(defconst org-context-table-toprow-style "OrgTableTopRow"
  "The default style name for the top row in tables.")

(defconst org-context-table-bottomrow-style "OrgTableBottomRow"
  "The default style name for the bottom row in tables.")

(defconst org-context-table-topleft-style "OrgTableTopLeftCell"
  "The default style name for the top left cell in tables.")

(defconst org-context-table-topright-style "OrgTableTopRightCell"
  "The default style name for the top right cell in tables.")

(defconst org-context-table-bottomleft-style "OrgTableBottomLeftCell"
  "The default style name for the bottom left cell in tables.")

(defconst org-context-table-bottomright-style "OrgTableBottomRightCell"
  "The default style name for the bottom right cell in tables.")

(defconst org-context-table-header-style "OrgTableHeader"
  "The default style name for the header row group in tables.")

(defconst org-context-table-footer-style "OrgTableFooter"
  "The default style name for the footer row group in tables.")

(defconst org-context-table-header-top-style "OrgTableHeaderTop"
  "The default style name for the top row in the header row group in tables.")

(defconst org-context-table-footer-top-style "OrgTableFooterTop"
  "The default style name for the top row in the footer row group in tables.")

(defconst org-context-table-header-bottom-style "OrgTableHeaderBottom"
  "The default style name for the bottom row in the header row group in tables.")

(defconst org-context-table-footer-bottom-style "OrgTableFooterBottom"
  "The default style name for the bottom row in the footer row group in tables.")

(defconst org-context-table-header-mid-style "OrgTableHeaderMid"
  "The default style name for header rows where the header is only one row.")

(defconst org-context-table-footer-mid-style "OrgTableFooterMid"
  "The default style name for footer rows where the footer is only one row.")

(defconst org-context-table-body-style "OrgTableBody"
  "The default style name for the body row group in tables.")

(defconst org-context-table-rowgroup-start-style "OrgTableRowGroupStart"
  "The default style name for rows starting row groups in tables.")

(defconst org-context-table-rowgroup-end-style "OrgTableRowGroupEnd"
  "The default style name for rows ending row groups in tables.")

(defconst org-context-table-colgroup-start-style "OrgTableColGroupStart"
  "The default style name for columns starting column groups in tables.")

(defconst org-context-table-colgroup-end-style "OrgTableColGroupEnd"
  "The default style name for columns ending column groups in tables.")

(defcustom org-context-table-use-footer ""
  "If \"repeat\", footer rows will be repeated on all pages. "
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
folding marks at all. "
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

;;; Filters
(defun org-context-math-block-options-filter (info _backend)
  ;; Annotating because I'm a big dummy.
  ;; Assign to INFO the result of iterating over '(:author :date :title)
  (dolist (prop '(:author :date :title) info)
    ;; Dumb comment
    ;; Info is a plist. Prop is one of :author :date :title
    ;; For each of those values, set its key in INFO to the results of
    ;; org-context--wrap-latex-math-block on its old value
    (plist-put info prop
               (org-context--wrap-latex-math-block (plist-get info prop) info))))

(defun org-context-clean-invalid-line-breaks (data _backend _info)
  (replace-regexp-in-string
   "\\(\\\\stop[A-Za-z0-9*]+\\|^\\)[ \t]*\\\\\\\\[ \t]*$"
   "\\1"
   data))

(defun org-context-math-block-tree-filter (tree _backend info)
  (org-context--wrap-latex-math-block tree info))

;; (defun org-context-matrices-tree-filter (tree _backend info)
;;   (org-context--wrap-latex-matrices tree info))

;; (defun org-context-image-link-filter (data _backend info)
;;   (org-export-insert-image-links data info org-context-inline-image-rules))


;;;; Pseudo Object: LaTeX Math Block

;; `latex-math-block' objects have the following property:
;; `:post-blank'.

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
    (cons "metadata:description" (org-export-data (org-latex--wrap-latex-math-block
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
  "Returns snippets given a list of snippet names.
SNIPPET_NAMES is a list of snippet names to look up.
INFO is a plist used as a communication channel."
  (mapcar
   (lambda (snippet-name)
     (cdr (assoc
           snippet-name
           (plist-get info :context-snippets))))
   snippet-names))

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
         (preset-header-data (plist-get preset-data :preamble))
         (preset-header-string (car preset-header-data))
         (preset-header-snippets
          (org-context--get-snippet-text info (cdr preset-header-data)))
         (starttext-extra-lines (list (plist-get info :context-starttext)))
         (preset-starttext-data (plist-get preset-data :starttext))
         (preset-starttext-string (car preset-starttext-data))
         (preset-starttext-snippets
          (org-context--get-snippet-text info preset-starttext-data))
         (stoptext-extra-lines (list (plist-get info :context-stoptext)))
         (preset-stoptext-data (plist-get preset-data :stoptext))
         (preset-stoptext-string (car preset-stoptext-data))
         (preset-stoptext-snippets
          (org-context--get-snippet-text info preset-stoptext-data))
         (user-snippets (org-context--get-snippet-text info (plist-get info :context-snippet)))
         (titlepagecommand (car (plist-get info :context-titlepage-environment)))
         (bodyenvname (car (plist-get info :context-body-environment)))
         (titlecommand (org-string-nw-p (car (plist-get info :context-title-command))))
         (toccommand (car (plist-get info :context-title-contents-command)))
         (environment-defs
          (let ((deflist
                  (list
                   (list
                    :context-description-command
                    "% LaTeX-style descriptive enumerations"
                    "\\definedescription[%s]")
                   (list
                    :context-blockquote-environment
                    "% blockquote environment"
                    "\\definestartstop[%s]")
                   (list
                    :context-example-environment
                    "% Create the example environment"
                    "\\definetyping[%s]")
                   (list
                    :context-fixed-environment
                    "% Create the fixed width environment"
                    "\\definetyping[%s]")
                   (list
                    :context-inline-source-environment
                    "% Create the inline source environment"
                    "\\definetyping[%s]")
                   (list
                    :context-block-source-environment
                    "% Create the block source environment"
                    "\\definetyping[%s]")
                   (list
                    :context-titlepage-environment
                    "% Create the title page style"
                    "\\definestartstop[%s]")
                   (list
                    :context-verse-environment
                    "% Create a verse style"
                    "\\definelines[%s]")
                   (list
                    :context-property-drawer-environment
                    "% Create a property drawer style"
                    "\\definetyping[%s]")
                   (list
                    :context-body-environment
                    "% Create a body style"
                    "\\definestartstop[%s]"))))
            (mapconcat
             (lambda
               (args)
               (let* ((kw (nth 0 args))
                      (comment (nth 1 args))
                      (templ (nth 2 args))
                      (nameimpl (plist-get info kw))
                      (name (car nameimpl))
                      (impl (cdr nameimpl)))
                 (concat
                  comment
                  "\n"
                  (when (org-string-nw-p name) (format templ name))
                  "\n"
                  (when (org-string-nw-p impl) impl))))
             deflist
             "\n")))
         (command-defs
          (let ((deflist
                  (list
                   (list
                    :context-title-command
                    "% Create an empty title command to be overridden by user")
                   (list
                    :context-title-contents-command
                    "% Create a TOC header command")
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
                    :context-planning-command
                    "% Define a basic planning command")
                   (list
                    :context-inline-task-command
                    "% Define a basic inline task command")
                   (list
                    :context-headline-command
                    "% Define a basic headline command")
                   (list
                    :context-clock-command
                    "% Define a basic clock command")
                   (list
                    :context-drawer-command
                    "% Define a basic drawer command"))))
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
         ;; TODO Allow user to pick the color in OPTIONS
         (vim-scheme-name (when vimp
                            (plist-get info :context-vim-color-scheme)))
         (vim-lang-colors (when vimp
                            (assoc vim-scheme-name
                                   (plist-get info :context-vim-colors))))
         (vim-langs
          (when (and vimp vim-lang-hash)
            (mapconcat
             (lambda (key)
               (let* ((lang-info (gethash key vim-lang-hash))
                      (vim-lang (plist-get lang-info 'vim-lang))
                      (context-name (plist-get lang-info 'context-name)))
                 (format "\\definevimtyping[%s]\n  [syntax=%s,\n   alternative=%s]"
                         context-name vim-lang (symbol-name (car vim-lang-colors)))))
             (hash-table-keys vim-lang-hash)
             "\n"))))
    (concat
     (and time-stamp
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     (when vimp
       (concat
        "\n\\usemodule[vim]\n"
        (cdr vim-lang-colors)))
     "\n"
     unnumbered-headline-commands
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
   environment-defs
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
\\startfrontmatter
"
   (when (org-string-nw-p titlepagecommand)
     (format "\\start%s\n" titlepagecommand))
   (when (org-string-nw-p titlecommand)
     (format "\\%s\n" titlecommand))
   (when
       (plist-get info :with-toc)
     (concat
      (when (org-string-nw-p toccommand)
        (format "\\%s\n" toccommand))
      "\n\\placecontent\n"))
   (when (org-string-nw-p titlepagecommand)
     (format "\\stop%s\n" titlepagecommand))
   "
\\stopfrontmatter
\\startbodymatter
"
   (when (org-string-nw-p bodyenvname)
     (format "\\start%s\n" bodyenvname))
   (concat preset-starttext-string "\n"
           (mapconcat 'identity preset-starttext-snippets "\n"))

   contents

   (concat preset-stoptext-string "\n"
           (mapconcat 'identity preset-stoptext-snippets "\n"))
"\n"
   (when (org-string-nw-p bodyenvname)
     (format "\\stop%s\n" bodyenvname))
   "\\stopbodymatter
\\stoptext\n")))

;;; Internal functions

(defun org-context-format-headline-default-function
    (todo todo-type priority text tags info)
  "Default format function for a headline.
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
           (cons "Tags" (mapconcat #'org-latex--protect-text tags ":")))))
      text)))

(defun org-context--format-arguments (arguments)
  "Formats ARGUMENTS into a ConTeXt argument string.
ARGUMENTS is an alist of string, string pairs. For instance,
given '((\"key1\" . \"val1\") (\"key2\" . \"val2\")) returns
\"[key1=val1, key2=val2] or similar."
  (mapconcat
   (lambda (kv)
     (let ((key (car kv))
           (val (cdr kv)))
       (format "%s={%s}" key val)))
   (seq-filter (lambda (s) (org-string-nw-p (cdr s))) arguments)
   ",\n"))

(defun org-context--wrap-label (element output info)
  "Wrap label associated to ELEMENT around OUTPUT, if appropriate.
INFO is the current export state, as a plist.  This function
should not be used for floats.  See
`org-context--caption/label-string'."
  (if (not (and (org-string-nw-p output) (org-element-property :name element)))
      output
    (concat (format "\\pagereference[%s]"
                    (org-context--label element info))
            output)))

(defun org-context--caption/label-string (element info)
  "Return caption and label ConTeXt string for ELEMENT.

INFO is a plist holding contextual information.  If there's no
caption nor label, return the empty string.

For non-floats, see `org-context--wrap-label'."
  ;; TODO This whole function needs a lot of work.
  (let* ((label (org-context--label element info nil t))
	 (main (org-export-get-caption element))
	 (attr (or (org-export-read-attribute :attr_context element)
          (org-export-read-attribute :attr_latex element)))
	 (type (org-element-type element))
	 (nonfloat (or (and (plist-member attr :float)
			    (not (plist-get attr :float))
			    main)
		       (and (eq type 'src-block)
			    (not (plist-get attr :float))
			    (null (plist-get info :latex-listings)))))
	 (short (org-export-get-caption element t))
	 (caption-from-attr-latex (plist-get attr :caption)))
    (cond
     ((org-string-nw-p caption-from-attr-latex)
      (concat caption-from-attr-latex "\n"))
     ((and (not main) (equal label "")) "")
     ((not main) label)
     ;; Option caption format with short name.
     (t(org-export-data main info)))))

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
                          org-latex-math-environments-re
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

(defun org-context--inline-image (link info)
  "Return the ConTeXt code for an inline image.
LINK is the link pointing to the inline image. INFO is a plist
used as a communication channel."
  ;; TODO handle svg graphics with built-in converter
  (let* ((parent (org-export-get-parent-element link))
         (path (let ((raw-path (org-element-property :path link)))
                 (if (not (file-name-absolute-p raw-path)) raw-path
                   (expand-file-name raw-path))))
         (filetype (file-name-extension path))
         ;; OK to use the latex function here
         (caption-above-p (org-latex--caption-above-p link info))
         (attr-latex (org-export-read-attribute :attr_latex parent))
         (attr-context (org-export-read-attribute :attr_context parent))
         ;; Context takes precedence over latex
         (attr (or attr-context attr-latex))
         (caption (org-context--caption/label-string parent info))
         (label (org-context--label parent info ))
         (float (let ((float (plist-get attr :float)))
                  (cond ((string= float "wrap") 'wrap)
                        ((string= float "sideways") 'sideways)
                        ((string= float "multicolumn") 'multicolumn)
                        ((and (plist-member attr :float) (not float)) 'nonfloat)
                        ((or float
                             (org-element-property :caption parent)
                             (org-string-nw-p (plist-get attr :caption)))
                         'figure)
                        (t 'nonfloat))))
         ;; TODO convert from LaTeX placement options to ConTeXt placement options
         (placement (plist-get attr-context :placement))
         (center
          (cond
           ;; If this is an image link, do not center
           ((eq 'link (org-element-type (org-export-get-parent link))) nil)
           ((plist-member attr :center) (plist-get attr :center))
           ;; TODO: Do we need an option for this or can we just have it
           ;; be in CONTEXT_HEADER_EXTRA?
           (t (plist-get info :latex-images-centered))))
         ;; TODO: Figure out what this is about
         (comment-include (if (plist-get attr :comment-include) "%" ""))
         ;; It is possible to specify the scale or width and height in
         ;; the ATTR_LATEX line, and also via default variables.
         (scale (cond ((eq float 'wrap) "")
                      ((plist-get attr :scale))
                      ;; TODO: Can we eliminate this option with
                      ;; CONTEXT_HEADER_EXTRA?
                      (t (plist-get info :context-image-default-scale))))
         (width (cond ((org-string-nw-p scale) "")
                      ((plist-get attr :width))
                      ((plist-get attr :height) "")
                      ;; TODO Is this a reasonable size? Shouldn't this
                      ;; be configurable?
                      ((eq float 'wrap) "0.48\\textwidth")
                      ;; TODO Can we eliminate this option with
                      ;; CONTEXT_HEADER_EXTRA?
                      (t (plist-get info :context-image-default-width))))
         (height (cond ((org-string-nw-p scale) "")
                       ((plist-get attr :height))
                       ((or (plist-get attr :width)
                            (memq float '(figure wrap))) "")
                       ;; TODO Can we eliminate this option with
                       ;; CONTEXT_HEADER_EXTRA?
                       (t (plist-get info :context-image-default-height))))
         ;; TODO format options compatible with ConTeXt
         (options (let ((opt (or (plist-get attr :options)
                                 (plist-get info :context-image-default-option))))
                    ;; Strip braces
                    (if (not (string-match "\\`\\[\\(.*\\)\\]\\'" opt)) opt
                      (match-string 1 opt))))
         image-code
         options-list)
    ;; TODO tikz and pdf
    (when (not (member filetype '("tikz" "pgf")))
      ;; Add scale, or width and height to options
      (if (org-string-nw-p scale)
          ;; TODO check scale format
          (setq options-list (add-to-list 'options-list (cons "scale" scale)))
        (when (org-string-nw-p width)
          (setq options-list (add-to-list 'options-list (cons "maxwidth" width))))
        (when (org-string-nw-p height)
          (setq options-list (add-to-list 'options-list (cons "maxheight" height)))))
      (let ((search-option (org-element-property :search-option link)))
        ;; TODO
        )
      (setq image-code
            (format "\\externalfigure[%s][%s]"
                    path
                    (concat
                     (when (org-string-nw-p options) (format "%s,\n" options))
                     (org-context--format-arguments options-list))))
      (let (env-options
            location-options)
        (when (and center (not (plist-member attr :float)))
          (add-to-list 'location-options "middle"))
        (pcase float
          (`wrap (add-to-list
                  'location-options
                  (or placement (plist-get info :context-float-default-placement))))
          (`sideways (progn (add-to-list 'location-options "90")
                            (add-to-list 'location-options "page")))
          ;; TODO I don't know if this even works in LaTeX
          ;;(`multicolumn "orgmulticolumnfigure")
          ;; TODO What do we do with figure?
          (_ (when placement (add-to-list 'location-options placement))))
        ;;(if (not (eq float 'sideways))
        ;;    (add-to-list 'location-options "here"))
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
         ;; TODO include comments
         ;; TODO allow caption placement
         image-code)))))



(defun org-context--wrap-env (ent contents info env-key default)
  "Wraps content in an environment with a label.
Environment is looked up from the info plist."
  (let* ((prog-env-name (car (plist-get info env-key)))
         (env-name (or (org-string-nw-p prog-env-name) default)))
    (org-context--wrap-label
     ent
     (if env-name
         (format "\\start%s\n%s\\stop%s" env-name contents env-name)
       contents)
     info)))

;;; Transcode Functions

;;;; Bold

(defun org-context-bold (_bold contents info)
  "Transcode BOLD from Org to ConTeXt.
CONTENTS is the text with bold markup. INFO is a plist holding
contextual information."
  (org-context--text-markup contents 'bold info))

(defun org-context-center-block (center-block contents info)
  "Transcode a CENTER-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the center block.  INFO is a plist
holding contextual information."
  (org-context--wrap-label
   center-block (format "\\startalignment[middle]\n%s\\stopalignment" contents) info))

(defun org-context-format-clock-default-function (timestamp info)
  "Formats a timestamp in ConTeXt format"
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
      format-time-string "%FT%T%z" time)))

(defun org-context-clock (clock _contents info)
  "Transcode a CLOCK element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((timestamp (org-element-property :value clock))
        (formatter (plist-get info :context-format-clock-function)))
    (funcall formatter timestamp info)))

(defun org-context-code (code contents info)
  "Transcode CODE from Org to ConTeXt"
  (org-context--text-markup (org-element-property :value code) 'code info))

(defun org-context-format-drawer-default-function (name contents info)
  (let ((formatter
         (org-string-nw-p
          (car (plist-get info :context-drawer-command)))))
    (if formatter
        (format "\\%s{%s}{%s}" formatter name contents)
      (format "%s\\hairline %s" name contents))))

(defun org-context-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ConTeXt.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (funcall (plist-get info :context-format-drawer-function)
                          name contents info)))
    (org-context--wrap-label drawer output info)))

(defun org-context-dynamic-block (dynamic-block contents info)
  "Transcode a DYNAMIC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.  See `org-export-data'."
  (org-context--wrap-label dynamic-block contents info))

(defun org-context-entity (entity _contennts info)
  "Transcode an ENTITY object from Org to ConTeXt.
CONTENTS are the definition itself. INFO is a plist
holding contextual information."
  (org-element-property :context entity))

(defun org-context-example-block (example-block _contennts info)
  "Transcode an EXAMPLE-BLOCK element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (when (org-string-nw-p (org-element-property :value example-block))
    (org-context--wrap-env
     example-block
     (org-export-format-code-default example-block info)
     info
     :context-example-environment
     "typing")))

(defun org-context-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block) '("CONTEXT" "TEX"))
    (org-remove-indentation (org-element-property :value export-block))))

(defun org-context-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WDITH element from Org to LaTeX.
CONTENTS is nil. INFO is a plist holding contextual information."
  (org-context--wrap-env
   fixed-width
   (org-remove-indentation (org-element-property :value fixed-width))
   info
   :context-fixed-environment
   "typing"))

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

(defun org-context-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; TODO Handle the case where the first appearance of a footnote
  ;; is inside of another footnote. This could possibly be solved
  ;; by using \footnotetext. This could also be a problem with
  ;; my ConTeXt version
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
        (cond
         ((string-match-p "\\<tables\\>" value) "\\placelistoftables")
         ((string-match-p "\\<figures\\>" value) "\\placelistoffigures")
         ((string-match-p "\\<headlines\\>" value)
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
              (format  "\\placecontent[%s]" levelstring))))))))))

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
                  (plist-get info :latex-inline-image-rules)))
         (path (org-latex--protect-text
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
      (format (org-export-get-coderef-format path desc)
              ;; Resolve with RAW-PATH since PATH could be tainted
              ;; with `org-context--protect-text' call above.
              (org-export-resolve-coderef raw-path info)))
     ;; External link with a description part.
     ((and path desc) (format "\\goto{%s}[url(%s)]" desc path))
     ;; External link without a description part.
     (path (format "\\goto{\\hyphenatedurl{%s}}[url(%s)]" path path))
     ;; No path, only description.  Try to do something useful.
     (t (format "\\hyphenatedurl{%s}" desc)))))

(defun org-context--get-all-headline-commands (max-depth)
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
  "Creates a headline name with the correct depth."
  (concat
   (apply 'concat (make-list (+ level (- 1)) "sub"))
   (if numberedp "section" "subject")))

(defun org-context-headline (headline contents info)
  "Transcodes a HEADLINE element from Org to ConTeXt."
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
         (headertemplate (format "\\start%s" headline-name))
         (footercommand (format "\n\n\\stop%s" headline-name))
         (headline-label (org-context--label headline info t ))
         (headline-args
          (org-context--format-arguments
           (list
            (cons "title" full-text)
            (cons "list" alt-title)
            (cons "marking" alt-title)
            (cons "bookmark" alt-title)
            (cons "reference" headline-label)))))
    ;; Use a special heading command to exclude this from the TOC
    (concat
     "\n\n"
     headertemplate
     (format "[%s]" headline-args)
     "\n\n"
     contents
     footercommand)))

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
     ;; TODO get width and thickness from attr_latex
     (org-context--wrap-label
      horizontal-rule
      "\\textrule"
      info))))

(defun org-context--highlight-src-builtin (src-block info typ)
  "Wraps a source block in the builtin environment for ConTeXt source
code. Use this if you don't have Vim.

SRC-BLOCK is the code object to transcode.
INFO is a plist holding contextual information.
TYP is one of \"'inline\" or \"'block\""
  (let ((code (org-string-nw-p (org-element-property :value src-block))))
    (when code
      (let* ((org-lang (org-element-property :language src-block))
             (lang (and
                    org-lang
                    (or (cadr (assq (intern org-lang)
                                    (plist-get info :context-highlighted-langs)))
                        (downcase org-lang))))
             (env-name (or
                        (org-string-nw-p
                         (car (plist-get info :context-block-source-environment)))
                        "typing")))
        (format "\\start%s%s\n%s\\stop%s"
                env-name
                (if (org-string-nw-p lang)
                    (format "[option=%s]" lang)
                  "")
                (pcase typ
                  ('block (org-export-format-code-default src-block info))
                  ('inline ((org-element-property :value src-block))))
                env-name)))))

(defun org-context--highlight-src-vim (src-block info typ)
  "Wraps a source block in a vimtyping environment. This requires you
have Vim installed and the t-vim module for ConTeXt."
  (let ((org-lang (org-element-property :language src-block))
        (code (pcase typ
                ('block (org-export-format-code-default src-block info))
                ('inline (org-element-property :value src-block)))))
    (if (org-string-nw-p org-lang)
      (let* (;; Add a hash table of language names and vimtyping names
             (lang-cache
              (or (plist-get info :context-languages-used-cache)
                  (let ((hash (make-hash-table :test #'equal)))
                    (plist-put info :context-languages-used-cache hash)
                    hash)))
             ;; Optional namespacing prefix to set org elements apart
             (lang-info
              (or (gethash org-lang lang-cache)
                  (puthash org-lang
                           (let ((lang-info
                                  (or
                                   (cdr
                                    (assoc org-lang
                                           (plist-get info :context-vim-langs)))
                                   (list
                                    :vim-name (downcase org-lang)
                                    :context-name (capitalize org-lang)))))
                             (list
                              'vim-lang
                              (plist-get lang-info :vim-name)
                              'context-name
                              (concat
                               (or (org-string-nw-p
                                    (car (plist-get info :context-block-source-environment)))
                                   "")
                               (plist-get lang-info :context-name))
                              ))
                           lang-cache)))
             (context-name (plist-get lang-info 'context-name)))
        (format "\\start%s\n%s\\stop%s"
                context-name
                code
                context-name))
      (format "\\starttyping\n%s\\stoptying" code))))

(defun org-context-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((engine (plist-get info :context-syntax-engine)))
    (pcase engine
      ('vim (org-context--highlight-src-vim inline-src-block info 'inline))
      (_ (org-context--highlight-src-builtin inline-src-block info 'inline)))))

(defun org-context-inlinetask (inlinetask contents info)
  "Transcode an INLNETASK element from Org to ConTeXt.
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
See `org-context-format-inlinetask-function' for details."
  (let ((format-command
         (org-string-nw-p (car (plist-get info :context-inline-task-command)))))
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
           (cons "Tags" (org-make-tag-string (mapcar #'org-latex--protect-text tags)))
           (cons "Contents" contents))))
      (concat title "\\hairline" contents "\\hairline"))))

(defun org-context-italic (_italic contents info)
  "Transcode ITALIC from Org to ConTeXt"
  (org-context--text-markup contents 'italic info))

(defun org-context-item (item contents info)
  "Transcode and ITEM element from Org to ConTeXt"
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
                  (org-trim contents)
                  descrcommand))
      (if (org-string-nw-p checkbox)
          (format "\\sym{%s} %s" checkbox contents)
        (format "\\item %s" (org-trim contents))))))

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

(defun org-context--latex-environment-contents (latex-environment)
  "Returns the CONTENTS of LATEX-ENVIRONMENT."
  (let* ((latex-env-re "\\\\begin{\\([A-Za-z0-9*]+\\)}\\(\\(?:.*\n\\)*\\)\\\\end{\\1}")
         (value (org-remove-indentation
                 (org-element-property :value latex-environment)))
         (match (string-match latex-env-re value))
         (env-contents (match-string 2 value)))
    env-contents))

(defun org-context--transcode-align (align-environment)
  "Transcode an ALIGN-ENVIRONMENT from org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (concat
   "\\startalign\n\\NC "
   (replace-regexp-in-string
    "\\\\\\\\" "\\\\NR\n\\\\NC "
    (replace-regexp-in-string "[^\\]&" " \\\\NC " align-environment))
   "\\stopalign\n"))

(defun org-context-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (plist-get info :with-latex)
    (let* ((value (org-remove-indentation
                   (org-element-property :value latex-environment)))
           (environment-name (org-context--latex-environment-name latex-environment))
           (environment-contents
            (org-context--latex-environment-contents
             latex-environment))
           (numberedp
            (not (string-match "\\*$" environment-name)))
           (type (org-latex--environment-type latex-environment))
           (caption (if (eq type 'math)
                        (org-latex--label latex-environment info nil t)
                      (org-latex--caption/label-string latex-environment info)))
           (caption-above-p
            (memq type (append (plist-get info :latex-caption-above) '(math)))))
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
          (when numberedp "\\placeformula\n")
          "\\startformula\n"
          (pcase environment-name
            ("align" (org-context--transcode-align environment-contents))
            ("align*" (org-context--transcode-align environment-contents))
            (_ environment-contents))
          "\\stopformula"))
        (_ value)))))

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
                    (substring value 2 -2)))
           )
          (t value))))

(defun org-context-line-break (_line-break _contents _info)
  "Transcode a LINE-BREAK object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  "\\crlf\n")

(defun org-context-paragraph (_paragraph contents info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (org-context--text-markup contents 'paragraph info))

(defun org-context-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ContTeXt.
CONTENTS is the contents of the list. INFO is a plist holding
contextual information."
  (let* ((type (org-element-property :type plain-list))
         (attr (org-export-read-attribute :attr_latex plain-list))
         (open-command
          (cond ((eq type 'ordered) "\\startitemize[n]\n")
                ((eq type 'descriptive) "")
                (t "\\startitemize\n")))
         (close-command
          (if (eq type 'descriptive)
              ""
            "\\stopitemize")))
    (org-context--wrap-label
     plain-list
     (concat
      open-command
      contents
      close-command)
     info)))

(defun org-context--format-quote (text info original)
  "Wraps quoted text in `\\quote{}' constructs. ConTeXt provides
facilities for multilingual quoting so no need to reimplement"
  (let ((quote-status
         (copy-sequence (org-export--smart-quote-status (or original text) info))))
    (replace-regexp-in-string
     "['\"]"
     (lambda (match)
       (cdr (assq (pop quote-status)
                  (plist-get info :context-export-quotes-alist)))
)
     text nil t)))

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

(defun org-context-property-drawer (property-drawer contents info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (org-context--wrap-env
   property-drawer
   contents
   info
   :context-property-drawer-environment
   "typing"))

(defun org-context-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to ConTeXt.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "\\m{%s}" (org-trim contents))))

(defun org-context-quote-block (quote-block contents info)
  "Transcodes a QUOTE-BLOCK element from Org to ConTeXt."
  (org-context--wrap-env
   quote-block
   contents
   info
   :context-blockquote-environment
   nil))

(defun org-context-radio-target (radio-target text info)
  "Transcode a RADIO-TARGET object from Org to ConTeXt.
TEXT is the text of the target.  INFO is a plist holding
contextual information."
  (format "\\reference[%s]{%s} %s"
          (org-export-get-reference radio-target info)
          text
          text))

(defun org-context-strike-through (_strike-through contents info)
  "Transcode STRIKE_THROUGH from Org to ConTeXt"
  (org-context--text-markup contents 'strike-through info))

(defun org-context-subscript (_subscript contents info)
  "Transcode a SUBSCRIPT from Org to ConTeXt"
  (org-context--text-markup contents 'subscript info))

(defun org-context-superscript (_superscript contents info)
  "Transcode a SUPERSCRIPT from Org to ConTeXt"
  (org-context--text-markup contents 'superscript info))

(defun org-context-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the block. INFO is a plist
holding contextual information."
  (let ((type (org-element-property :type special-block))
        (opt (org-export-read-attribute :attr_latex special-block :options))
        (caption (org-context--caption/label-string special-block info))
        (caption-above-p (org-latex--caption-above-p special-block info)))
    (concat (format "\\start%s[%s]\n" type (or opt ""))
            (and caption-above-p caption)
            contents
            (and (not caption-above-p) caption)
            (format "\\stop%s" type))))

(defun org-context-section (section contents info)
  "Transcode a SECTION element from Org to ConTeXt.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

(defun org-context-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  ;; TODO caption
  ;; TODO caption-above-p
  ;; TODO label
  ;; TODO custom-environment
  ;; TODO num-start
  ;; TODO retain labels
  ;; TODO attributes
  ;; TODO float
  (let ((engine (plist-get info :context-syntax-engine)))
    (pcase engine
      ('vim (org-context--highlight-src-vim src-block info 'block))
      (_ (org-context--highlight-src-builtin src-block info 'block)))))

(defun org-context-table (table contents info)
  "Return appropriate ConTeXt code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."

  (let* ((attr (org-export-read-attribute :attr_context table))
         (caption (org-context--caption/label-string table info))
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
                       (cons "title" caption))))
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

(defun org-context-target (target _contents info)
  "Transcode a TARGET object from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "\\pagereference[%s]" (org-context--label target info)))

(defun org-context-format-timestamp-default-function (timestamp)
  (let* ((time (org-timestamp-to-time timestamp))
         (year (format-time-string "%Y" time))
         (month (format-time-string "%m" time))
         (day (format-time-string "%d")))
    (format "\\date[d=%s,m=%s,y=%s]" day month year)))

(defun org-context-timestamp (timestamp _contents info)
  "Transcode a TIMESTAMP object from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (funcall (plist-get info :context-format-timestamp-function) timestamp))

(defun org-context-underline (_underline contents info)
  "Transcode UNDERLINE from Org to ConTeXt"
  (org-context--text-markup contents 'underline info))

(defun org-context-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to ConTeXt"
  (org-context--text-markup
   (org-element-property :value verbatim) 'verbatim info))

(defun org-context-verse-block (verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to ConTeXt.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (org-context--wrap-env
   verse-block
   contents
   info
   :context-verse-environment
   nil))

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
               (concat (regexp-quote (file-name-base outfile))
                       "\\(?:\\.[0-9]+\\)?\\."
                       (regexp-opt org-context-logfiles-extensions))
               t)))
      ;; LaTeX warnings should be close enough to ConTeXt warnings
      (let ((warnings (org-latex--collect-warnings log-buf)))
        (message (concat "PDF file produced"
                         (cond
                          ((eq warnings 'error) " with errors.")
                          (warnings (concat " with warnings: " warnings))
                          (t "."))))))
    ;; Return output file name.
    outfile))

(provide 'ox-context)
