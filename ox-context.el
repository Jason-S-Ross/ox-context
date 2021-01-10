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
       ((?c "As ConTeXt file" org-context-export-to-context)))
 :filters-alist '((:filter-options . org-context-math-block-options-filter)
                  (:filter-paragraph . org-context-clean-invalid-line-breaks)
                  (:filter-parse-tree org-context-math-block-tree-filter
                                      ;;org-context-matrices-tree-filter
                                      ;; org-context-image-link-filter
                                      )
                  (:filter-verse-block . org-context-clean-invalid-line-breaks))
 :options-alist '((:context-format-headline-function nil nil org-context-format-headline-function)
                  (:context-header "CONTEXT_HEADER" nil nil newline)
                  (:context-header-extra "CONTEXT_HEADER_EXTRA" nil nil newline)
                  (:context-highlighted-langs nil nil org-context-highlighted-langs)
                  (:context-text-markup-alist nil nil org-context-text-markup-alist)
                  (:context-toc-command nil nil org-context-toc-command)
                  (:date "DATE" nil "\\currentdate" parse))
 :translate-alist '((bold . org-context-bold)
                    (center-block . org-context-center-block)
                    (code . org-context-code)
                    (entity . org-context-entity)
                    (example-block . org-context-example-block)
                    (export-block . org-context-export-block)
                    (fixed-width . org-context-fixed-width)
                    ;;(footnote-definition . org-context-footnote-definition)
                    (footnote-reference . org-context-footnote-reference)
                    (headline . org-context-headline)
                    (horizontal-rule . org-context-horizontal-rule)
                    (inline-src-block . org-context-inline-src-block)
                    (italic . org-context-italic)
                    (item . org-context-item)
                    (latex-environment . org-context-latex-environment)
                    (latex-fragment . org-context-latex-fragment)
                    (line-break . org-context-line-break)
                    (link . org-context-link)
                    (paragraph . org-context-paragraph)
                    (plain-list . org-context-plain-list)
                    ;;(plain-text . org-context-plain-text)
                    (quote-block . org-context-quote-block)
                    (src-block . org-context-src-block)
                    (special-block . org-context-special-block)
                    (strike-through . org-context-strike-through)
                    (subscript . org-context-subscript)
                    (superscript . org-context-superscript)
                    (table . org-context-table)
                    (table-cell . org-context-table-cell)
                    (table-row . org-context-table-row)
                    ;;(target . org-context-target)
                    (template . org-context-template)
                    ;;(timestamp . org-context-timestamp)
                    (underline . org-context-underline)
                    (verbatim . org-context-verbatim)
                    (verse-block . org-context-verse-block)
                    ;;;; Pseudo objects and elements.
                    (latex-math-block . org-context-math-block)
                    ;;(latex-matrices . org-context-matrices)
                    ))


(defcustom org-context-toc-command "\\completecontent"
  "ConTeXt command to set the table of contents."
  :group 'org-export-context
  :type 'string)

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
;;;; Text Markup

(defcustom org-context-text-markup-alist
  '((bold ."\\bold{%s}")
    (code . "\\type{%s}")
    (fixed-width . "\\startOrgFixed\n%s\n\\stopOrgFixed")
    (italic . "\\italic{%s}")
    (paragraph . "\n\\startOrgParagraph\n%s\n\\stopOrgParagraph")
    (property-drawer . "\n\startOrgPropertyDrawer\n%s\n\\stopOrgPropertyDrawer")
    (protectedtexttt . "\\type{%s}")
    (quotation . "\\startOrgBlockQuote\n%s\n\\stopOrgBlockQuote")
    (strike-through . "\\inframed[frame=off]{\\overstrike{%s}}")
    (subscript . "\\low{%s}")
    (superscript . "\\high{%s}")
    (underline . "\\underbar{%s}")
    (verbatim . "\\type{%s}")
    (verb . "\\type{%s}")
    (verse . "\\startOrgVerse\n%s\n\\stopOrgVerse"))
  "Alist of ConTeXt expressions to convert text markup."
  :group 'org-export-context
  :version "26.1"
  :package-version '(Org . "8.3")
  :type 'alist
  :options '(bold code italic strike-through underline verbatim))

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


(defun org-context--format-spec (info)
  "Create a format-spec for document meta-data.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t . ,(org-export-data (plist-get info :title) info))
    (?s . ,(org-export-data (plist-get info :subtitle) info))
    (?k . ,(org-export-data (org-context--wrap-latex-math-block
                             (plist-get info :keywords) info)
                            info))
    (?d . ,(org-export-data (org-latex--wrap-latex-math-block
                             (plist-get info :description) info)
                            info))
    (?c . ,(plist-get info :creator))
    (?l . ,(plist-get info :language))
    (?L . ,(capitalize (plist-get info :language)))
    (?D . ,(org-export-get-date info))))

(defun org-context--make-title (info &optional template)
  "Return a formatted ConTeXt title."
  " \\startalignment[center]
  \\blank[force,2*big]
  \\title{\\getvariable{org}{title}}
  \\blank[3*medium]
  {\\tfa \\getvariable{org}{name}}
  \\blank[3*medium]
  {\\mono \\getvariable{org}{email}}
  \\blank[2*medium]
  {\\tfa \\getvariable{org}{date}}
  \\blank[3*medium]
  \\stopalignment")

(defun org-context-make-preamble (info &optional template)
  "Return a formatted ConTeXt preamble.
INFO is a plist used as a communication channel. Optional
argument TEMPLATE, when non-nil, is the header template string,
as expected by `org-splice-context-header'."
  (concat
   "\n\n%From CONTEXT_HEADER\n"
   (mapconcat #'org-element-normalize-string
              (list (plist-get info :context-header))
              "")
   "

% Turn on interaction to make links work
\\setupinteraction[state=start]
% LaTeX-style descriptive enumerations
\\definedescription[OrgDesc]
% blockquote environment
\\definestartstop[OrgBlockQuote]
% Create the example environment
\\definetyping[OrgExample]
% Create the fixed width environment
\\definestartstop[OrgFixed]
% Create the inline source environment
\\definetyping[OrgInlineSrc]
% Create the block source environment
\\definetyping[OrgBlkSrc]
% Create the table header style
\\definextable[OrgTableHeader]
% Create the title page style
\\definemakeup[OrgTitlePage]
% Create a verse style
\\definelines[OrgVerse]
% Create a property drawer style
\\definestartstop[OrgPropertyDrawer]
% Create a paragraph style
\\definestartstop[OrgParagraph]
% Create a body style
\\definestartstop[OrgBody]

% From CONTEXT_HEADER_EXTRA
"
   (mapconcat #'org-element-normalize-string
              (list (plist-get info :context-header-extra))
              "\n\n")
   ))


(defun org-context-template (contents info)
  "Return complete document string after ConTeXt conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding the export options."
  (let ((title (org-export-data (plist-get info :title) info))
        (spec (org-context--format-spec info)))
    (concat
     ;; Time-stamp.
     (and (plist-get info :time-stamp-file)
          (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; Document class and packages.
     (org-context-make-preamble info)
     "\n% Table of Contents \n"
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (cond
         ((eq sec-num 1) "\\setupcombinedlist[content][list={chapter}]\n")
         ((eq sec-num 2) "\\setupcombinedlist[content][list={section}]\n")
         ((eq sec-num 3) "\\setupcombinedlist[content][list={subsection}]\n")
         ((eq sec-num 4) "\\setupcombinedlist[content][list={subsubsection}]\n")
         (t "\\setupcombinedlist[content]\n")))
     "\n% Org Document Variables\n"
     ;; Author.
     (let
         ((author
           (and (plist-get info :with-author)
                (let ((auth (plist-get info :author)))
                  (and auth (org-export-data auth info))))))
       (format "\\setvariable{org}{author}{%s}\n" author))
     (let
          ((email (plist-get info :email)))
       (format "\\setvariable{org}{email}{%s}\n" email))
     (let
         ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\setvariable{org}{date}{%s}\n" (org-export-data date info)))
     (format "\\setvariable{org}{title}{%s}\n" title)
     "\n% Document Start\n\n\n"
     "\\starttext
\\placebookmarks
\\startfrontmatter
\\startOrgTitlePagemakeup\n"
     (org-context--make-title info)
     (plist-get info :context-toc-command)
     "\n\\stopOrgTitlePagemakeup
\\stopfrontmatter
\\startbodymatter
\\startOrgBody\n"
     contents
     "\n\\stopOrgBody
\\stopbodymatter
\\stoptext\n")))

;;; Internal functions

(defun org-context-format-headline-default-function
    (todo _todo-type priority text tags _info)
  "Default format function for a headline.
See `org-latex-format-headline-function' for details."
  (concat
   (and todo (format "\\sansbold{%s} " todo))
   (and priority (format "\\framed{\\#%c} " priority))
   text
   (and tags
        (format "\\hfill{}{\\tt %s} "
                (mapconcat #'org-latex--protect-text tags ":")))))


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
	 (attr (org-export-read-attribute :attr_latex element))
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
	       (if (and user-label (plist-get info :latex-prefer-user-labels))
		   user-label
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
			 (org-export-get-reference datum info))))))
    (cond ((not full) label)
	  (label (format "\\pagereference[%s]%s"
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
         (attr (org-export-read-attribute :attr_latex parent))
         (caption (org-context--caption/label-string parent info))
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
         (placement
          (let ((place (plist-get attr :placement)))
            (cond
             (place (format %s place))
             ;; TODO: Get correct style for wrapping
             ((eq float 'wrap) "{l}{0.5\\textwidth}")
             ((eq float 'figure)
              ;; TODO: Get correct style for figure mode. Do we even need this?
              ;; Check the ConTeXt options for default figure placement
              (format "[%s]" (plist-get info :latex-default-figure-position)))
              (t ""))))
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
                       (t (plist-get info :latex-image-default-scale))))
          (width (cond ((org-string-nw-p scale) "")
                       ((plist-get attr :width))
                       ((plist-get attr :height) "")
                       ;; TODO Is this a reasonable size? Shouldn't this
                       ;; be configurable?
                       ((eq float 'wrap) "0.48\\textwidth")
                       ;; TODO Can we eliminate this option with
                       ;; CONTEXT_HEADER_EXTRA?
                       (t  "0.9\\textwidth")))
          (height (cond ((org-string-nw-p scale) "")
                        ((plist-get attr :height))
                        ((or (plist-get attr :width)
                             (memq float '(figure wrap))) "")
                        ;; TODO Can we eliminate this option with
                        ;; CONTEXT_HEADER_EXTRA?
                        (t (plist-get info nil))))
          ;; TODO format options compatible with ConTeXt
          (options (let ((opt (or (plist-get attr :options)
                                  (plist-get info :latex-image-default-option))))
                     (if (not (string-match "\\`\\[\\(.*\\)\\]\\'" opt))
                         opt
                       (match-string 1 opt))))
          image-code)
         ;; We can't handle tikz and pgf so don't even try
         (when (not (member filetype '("tikz" "pgf")))
           ;; Add scale, or width and height to options
           (if (org-string-nw-p scale)
               ;; TODO check scale format
               (setq options (concat options ",scale=" scale))
             (when (org-string-nw-p width) (setq options (concat options ",width=" width)))
             (when (org-string-nw-p height) (setq options (concat options ",height=" height))))
           (let ((search-option (org-element-property :search-option link)))
             ;; TODO
             )
           (setq options
                 (cond ((not (org-string-nw-p options)) "")
                       ((string-prefix-p "," options)
                        (substring options 1))
                       (t options)))
           (setq image-code
                 (format "\\externalfigure[%s][%s]" path options))
           (let ((floatname
                  (pcase float
                    ;;;; TODO
                    ;;(`wrap "orgwrapfigure")
                    ;;;; TODO
                    ;;(`sideways "orgsidewaysfigure")
                    ;;;; TODO
                    ;;(`multicolumn "orgmulticolumnfigure")
                    ;;(`figure "orgfigure")
                    (_ "figure"))))
             (format
              "\\startplace%s%s
%s
\\stopplace%s"
              floatname
              (format
               "[%s]"
               (concat
                (if center "location={force,middle}" "location={force}")
                (when (org-string-nw-p caption) (format ",title={%s}" caption))))
              ;; TODO include comments
              ;; TODO allow caption placement
              image-code
              floatname)))))


(defun org-context--org-table (table contents info)
  "Return appropriate ConTeXt code for an Org table.

TABLE is the table type element to transcode.  CONTENTS is its
contents, as a string.  INFO is a plist used as a communication
channel.

This function assumes TABLE has `org' as its `:type' property and
`table' as its `:mode' attribute."
  ;; TODO
  (concat
   "\\startplacetable\n\\startxtable\n"
   contents
   "\n\\stopxtable\n\\stopplacetable\n"))


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

(defun org-context-code (code contents info)
  "Transcode CODE from Org to ConTeXt"
  (org-context--text-markup (org-element-property :value code) 'code info))

(defun org-context-drawer (drawer contents info)
  "Transcode a DRAWER element from Org to ConTeXt.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* ((name (org-element-property :drawer-name drawer))
         (output (funcall (plist-get info :context-format-drawer-function)
                          name contents)))
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
    (org-context--wrap-label
     example-block
     (format "\\startOrgExample\n%s\\stopOrgExample"
             (org-export-format-code-default example-block info))
     info)))

(defun org-context-export-block (export-block _contents _info)
  "Transcode a EXPORT-BLOCK element from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (when (member (org-element-property :type export-block) '("CONTEXT" "TEX"))
    (org-remove-indentation (org-element-property :value export-block))))

(defun org-context-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WDITH element from Org to LaTeX.
CONTENTS is nil. INFO is a plist holding contextual information."
  (org-context--wrap-label
   fixed-width
   (org-context--text-markup
    (org-remove-indentation
     (org-element-property :value fixed-width))
    'fixed-width
    info)
   info))

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
          (format "\\about{%s}[%s]"
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
          (headline
           (let ((label (org-context--label destination info t)))
             (if (and (not desc)
                      (org-export-numbered-headline-p destination info))
                 (format "\\about[%s]" label)
               (format "\\goto{%s}[%s]"
                       (or desc
                           (org-export-data
                            (org-element-property :title destination) info)
                           label)))))
          (otherwise
           (let ((ref (org-context--label destination info t)))
             (if (not desc)
                 (format "\\about[%s]" ref)
               (format "\\goto{%s}[%s]" desc ref)))))))
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

(defun org-context-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to ConTeXt.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; TODO Handle the case where the first appearance of a footnote
  ;; is inside of another footnote. This could possibly be solved
  ;; by using \footnotetext. This could also be a problem with
  ;; my ConTeXt version
  (let* ((label (org-element-property :label footnote-reference))
         (footnote-definition
          (org-export-get-footnote-definition footnote-reference info))
         (reference-label (org-latex--label footnote-definition info t))
         (contents (org-trim (org-export-data footnote-definition info))))
    (cond
     ;; Footnote has already been defined
     ((not (org-export-footnote-first-reference-p footnote-reference info))
      (format "\\note[%s]" reference-label))
     ;; Otherwise create it
     (t (format "\\footnote[%s]{%s}" reference-label contents)))))

(defun org-context-headline (headline contents info)
  "Transcodes a HEADLINE element from Org to ConTeXt."
  (let* ((level (org-export-get-relative-level headline info))
         (numberedp (org-export-numbered-headline-p headline info))
         (text (org-export-data (org-element-property :title headline) info))
         (todo
          (and (plist-get info :with-todo-keywords)
               (let ((todo (org-element-property :todo-keyword headline)))
                 (and todo (org-export-data todo info)))))
         (todo-type (and todo (org-element-property :todo-type headline)))
         (tags (and (plist-get info :with-tags)
                    (org-export-get-tags headline info)))
         (priority (and (plist-get info :with-priority)
                        (org-element-property :priority headline)))
         (full-text (funcall (plist-get info :context-format-headline-function)
                             todo todo-type priority text tags info))
         (headertemplate
          (concat
           "\\"
           (apply 'concat (make-list (+ level (- 1)) "sub"))
           (if numberedp "section" "subject")))
         (headline-label (org-context--label headline info t )))
    (concat
     headertemplate
     (format
      "[reference={%s},\n  title={%s},\n  list={%s},\n  marking={%s},\n bookmark={%s}]"
      headline-label
      full-text
      text
      text
      text)
     "\n"
     contents)))

(defun org-context-horizontal-rule (horizontal-rule _contents info)
  "Transcode a HORIZONTAL-RULE object from Org to ConTeXt.
CONTENTS is nil. INFO is a plist holding contextual information."
  (let ((attr (org-export-read-attribute :attr_latex horizontal-rule))
        (prev (org-export-get-previous-element horizontal-rule info)))
    (concat
     ;; Make sure the rule doesn't start at the end of the current
     ;; line
     (when (and prev
                (let ((prev-blank (org-element-property :post-blank prev)))
                  (or (not prev-blank) (zerop prev-blank))))
       "\n")
     (org-context--wrap-label
      horizontal-rule
      "\\textrule"
      info))))

(defun org-context-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to ConTeXt.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (let ((code (org-element-property :value inline-src-block)))
    (let* ((org-lang (org-element-propery :language inline-src-block))
           (lang (or (cadr
                      (assq (intern org-lang)
                            (plist-get info :context-highlighted-langs)))
                     (downcase org-lang))))
      (format "\\startOrgInlineSrc[option=%s] %s \\stopOrgInlineSrc " lang code))))

(defun org-context-italic (_italic contents info)
  "Transcode ITALIC from Org to ConTeXt"
  (org-context--text-markup contents 'italic info))

(defun org-context-item (item contents info)
  "Transcode and ITEM element from Org to ConTeXt"
  (let ((tag (let ((tag (org-element-property :tag item)))
               (and tag (org-export-data tag info)))))
    (if (eq
         (org-element-property :type (org-export-get-parent item))
         'descriptive)
        (format "\\startOrgDesc{%s} %s\n\\stopOrgDesc" tag (org-trim contents))
      (format "\\item %s" (org-trim contents)))))

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
          "\\startformula\n"
          (pcase environment-name
            ("align" (org-context--transcode-align environment-contents))
            ("align*" (org-context--transcode-align environment-contents))
            (_ environment-contents))
          "\\stopformula"))
        (_ value)))))

(defun org-context-latex-fragment (latex-fragment _contents _info)
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
           (format "\\startformula\n%s\n\\stopformula"
                   (substring value 2 -2)))
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
         (env (plist-get attr :environment))
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

(defun org-latex-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to LaTeX.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (org-context--text-markup contents 'property-drawer info)))

(defun org-context-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to ConTeXt.
CONTENTS is a string.  INFO is a plist used as a communication
channel."
  (when (org-string-nw-p contents)
    (format "\\m{%s}" (org-trim contents))))

(defun org-context-quote-block (quote-block contents info)
  "Transcodes a QUOTE-BLOCK element from Org to ConTeXt."
  (org-context--wrap-label
   quote-block (org-context--text-markup contents 'quotation info) info))

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

(defun org-context-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item. INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((org-lang (org-element-property :language src-block))
           (lang (or (cadr (assq (intern org-lang)
                                 (plist-get info :context-highlighted-langs)))
                     (downcase org-lang))))
      (cond
       ((not lang) (format "\\startOrgBlkSrc\n%s\\stopOrgBlkSrc"
                           (org-export-format-code-default src-block info)))
       (t (format "\\startOrgBlkSrc[option=%s]\n%s\\stopOrgBlkSrc"
                  lang
                  (org-export-format-code-default src-block info)))))))

(defun org-context-table (table contents info)
  "Transcode a TABLE element from Org to ConTeXt.
CONTENTS is the contents of the table. INFO is a plist holding
contextual information."
  (org-context--org-table table contents info))



(defun org-context-table-cell (table-cell contents info)
  "Transcode a TABLE-CELL from Org to ConTeXt.
CONTENTS is the cell contents. INFO is a plist used as
a communication channel."
  (concat
   "\\startxcell "
   ;; TODO
   contents
   " \\stopxcell\n"))

(defun org-context-table-row (table-row contents info)
  "Transcode a TABLE-ROW element from Org to ConTeXt.
CONTENTS is the contents of the row.  INFO is a plist used as
a communication channel."
  (let ((firstrowp (not (org-export-get-previous-element table-row info)))
        (wrappedcontents (concat "\\startxrow\n" contents "\\stopxrow")))
    (if firstrowp
        (concat "\\startxtablehead[OrgTableHeader]\n" wrappedcontents "\n\\stopxtablehead")
      wrappedcontents)))

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
  (org-context--wrap-label
   verse-block
   (org-context--text-markup contents 'verse info)
   info))

;;;###autoload
(defun org-context-export-to-context
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer as a ConTeXt document (.mkiv)"
  (interactive)
  (let ((file (org-export-output-file-name ".mkiv" subtreep)))
    (org-export-to-file 'context file
      async subtreep visible-only body-only ext-plist)))
