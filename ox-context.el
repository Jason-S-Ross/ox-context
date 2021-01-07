;; Custom exporter for ConTeXt

(require 'cl-lib)
(require 'ox-latex)
(org-export-define-derived-backend
    'context 'latex
  :menu-entry
  '(?C 1
       ((?c "As ConTeXt file" org-context-export-to-context)))
 :filters-alist '((:filter-options . org-context-math-block-options-filter)
                  (:filter-paragraph . org-context-clean-invalid-line-breaks)
                  (:filter-parse-tree org-context-math-block-tree-filter
                                      ;;org-context-matrices-tree-filter
                                      ;; org-context-image-link-filter
                                      )
                  (:filter-verse-block . org-context-clean-invalid-line-breaks))
 :options-alist '((:context-text-markup-alist nil nil org-context-text-markup-alist)
                  (:context-toc-command nil nil org-context-toc-command)
                  (:context-header "CONTEXT_HEADER" nil nil newline)
                  (:context-header-extra "CONTEXT_HEADER_EXTRA" nil nil newline)
                  (:context-highlighted-langs nil nil org-context-highlighted-langs))
 :translate-alist '((bold . org-context-bold)
                    ;;(center-block org-context-center-block)
                    (code . org-context-code)
                    ;;(fixed-width . org-context-fixed-width)
                    ;;(footnote-definition . org-context-footnote-definition)
                    ;;(footnote-reference . org-context-footnote-reference)
                    (headline . org-context-headline)
                    ;;(horizontal-rule . org-context-horizontal-rule)
                    (inline-src-block . org-context-context-src-block)
                    (italic . org-context-italic)
                    (item . org-context-item)
                    ;;(latex-environment . org-context-environment)
                    ;;(line-break . org-context-line-break)
                    ;;(link . org-context-link)
                    ;;(paragraph . org-context-paragraph)
                    (plain-list . org-context-plain-list)
                    ;;(plain-text . org-context-plain-text)
                    (quote-block . org-context-quote-block)
                    ;;(section . org-context-section)
                    (src-block . org-context-src-block)
                    (special-block . org-context-special-block)
                    (strike-through . org-context-strike-through)
                    (subscript . org-context-subscript)
                    (superscript . org-context-superscript)
                    ;;(table . org-context-table)
                    ;;(table-cell . org-context-table-cell)
                    ;;(table-row . org-context-table-row)
                    ;;(target . org-context-target)
                    (template . org-context-template)
                    ;;(timestamp . org-context-timestamp)
                    (underline . org-context-underline)
                    (verbatim . org-context-verbatim)
                    ;;(verse-block . org-context-verse-block)
                    ;;;; Pseudo objects and elements.
                    ;;(latex-math-block . org-context-math-block)
                    ;;(latex-matrices . org-context-matrices)
                    ))


(defcustom org-context-toc-command "\\completecontent"
  "ConTeXt command to set the table of contents."
  :group 'org-export-context
  :type 'string)

;;;; Text Markup

(defcustom org-context-text-markup-alist
  '((bold ."\\bold{%s}")
    (code . "\\type{%s}")
    (italic . "\\italic{%s}")
    (strike-through . "\\inframed[frame=off]{\\overstrike{%s}}")
    (subscript . "\\low{%s}")
    (superscript . "\\high{%s}")
    (underline . "\\underbar{%s}")
    (verbatim . "\\type{%s}")
    (verb . "\\type}%s")
    (protectedtexttt . "\\type{%s}")
    (quotation . "\\startblockquote\n%s\n\\stopblockquote"))
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


(defun org-context-make-preamble (info &optional template)
  "Return a formatted ConTeXt preamble.
INFO is a plist used as a communication channel. Optional
argument TEMPLATE, when non-nil, is the header template string,
as expected by `org-splice-context-header'."
  (concat
   (mapconcat #'org-element-normalize-string
              (list (plist-get info :context-header))
              "")
   "
\\unprotect
\\def\\doctitle#1{\\gdef\\@title{#1}}
\\def\\author#1{\\gdef\\@author{#1}}
\\def\\email#1{\\gdef\\@email{#1}}
\\def\\date#1{\\gdef\\@date{#1}}
\\date{\\currentdate}  % Default to today unless specified otherwise.

\\def\\maketitle{%
  \\startalignment[center]
    \\blank[force,2*big]
      {\\tfd \\@title}
    \\blank[3*medium]
      {\\tfa \\@author}
    \\blank[3*medium]
      {\\mono \\@email}
    \\blank[2*medium]
      {\\tfa \\@date}
    \\blank[3*medium]
  \\stopalignment}
\\protect

% LaTeX-style descriptive enumerations
\\definedescription[desc][
headstyle=bold, style=normal, align=flushleft,
alternative=hanging, width=broad, margin=1cm
]

% blockquote environment
\\setupdelimitedtext
  [blockquote]
  [style=\\slx,
   before={\\setupinterlinespace[line=2.4ex]}]

% Allow LaTeX-style urls
\\def\\href#1#2{\\useURL[#2][{#2}][][{#1}]\\goto{\\url[#2]}[url(#1)]}
\\setupurl
  [color=blue
   style=\\tf]
"
   (mapconcat #'org-element-normalize-string
              (list (plist-get info :context-header-extra))
              "")
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
     ;; Possibly limit depth for headline numbering.
     (let ((sec-num (plist-get info :section-numbers)))
       (cond
         ((eq sec-num 1) "\\setupcombinedlist[content][list={chapter}]\n")
         ((eq sec-num 2) "\\setupcombinedlist[content][list={chapter,section}]\n")
         ((eq sec-num 3) "\\setupcombinedlist[content][list={chapter,section,subsection}]\n")
         ((eq sec-num 4) "\\setupcombinedlist[content][list={chapter,section,subsection,subsubsection}]\n")
         (t "\\setupcombinedlist[content]\n")))
     ;; Author.
     (let
         ((author
           (and (plist-get info :with-author)
                (let ((auth (plist-get info :author)))
                  (and auth (org-export-data auth info))))))
       (format "\\author{%s}\n" author))
     (let
          ((email (plist-get info :email)))
       (format "\\email{%s}\n" email))
     (let
         ((date (and (plist-get info :with-date) (org-export-get-date info))))
       (format "\\date{%s}\n" date))
     (format "\\doctitle{%s}\n" title)
     "\\starttext\n"
     "\\maketitle\n"
     (plist-get info :context-toc-command)
     contents
     "\\stoptext\n"
     )))

;;; Internal functions


(defun org-context--text-markup (text markup info)
  "Format TEXT depending on MARKUP text markup.
INFO is a plist used as a communication channel. See
`org-context-text-markup-alist' for details"
  (let ((fmt (cdr (assq markup (plist-get info :context-text-markup-alist)))))
    (cl-case fmt
      ;; No format string: Return raw text.
      ((nil) text)
      (verb
       (format "\\type{%s}" text))
      (t (format fmt text)))))

(defun org-context--label (datum info &optional force full)
  "Return an appropriate label for DATUM
DATUM is an element or a `target' type object. INFO is the
current export state, as a plist.

Return nil if element DATUM has no NAME or VALUE affiliated
keyword or no CUSTOM_ID property, unless FORCE is non-nil. In
this case always return a unique label.

Eventually, if FULL is non-nil, wrap label within \"\\label{}\"."
  "")

;;; Transcode Functions

;;;; Bold

(defun org-context-bold (_bold contents info)
  "Transcode BOLD from Org to ConTeXt.
CONTENTS is the text with bold markup. INFO is a plist holding
contextual information."
  (org-context--text-markup contents 'bold info))

(defun org-context-code (code contents info)
  "Transcode CODE from Org to ConTeXt"
  (org-context--text-markup (org-element-property :value code) 'code info))

(defun org-context-headline (headline contents info)
  "Transcodes a HEADLINE element from Org to ConTeXt."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (class (plist-get info :latex-class))
           (class-sectioning (assoc class (plist-get info :latex-classes)))
           (numberedp (org-export-numbered-headline-p headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (text (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (todo-type (and todo (org-element-property :todo-type headline)))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list (concat "    " (org-make-tag-string tag-list))))))
           (priority (and (plist-get info :with-priority)
                          (org-element-property :priority headline)))
           (full-text (funcall (plist-get info :latex-format-headline-function)
                               todo todo-type priority text tags info))
           (headline-label (org-context--label headline info t t))
           (section-back-end
            (org-export-create-backend
             :parent 'latex
             :transcoders '((underline . (lambda (o c i) format ("\\underline{%s}" c))))))
           (section-fmt
            (let ((sec (nth (1+ level) class-sectioning)))
              (cond
               ((not sec) nil)
               ((stringp sec) (concat sec "\n%s"))
               ((not (consp (cdr sec)))
                (concat (funcall (if numberedp #'car #'cdr) sec) "\n%s"))
               ((= (length sec) 2)
                (when numberedp (concat (car sec) "\n%s" (nth 1 sec))))
               ((= (length sec) 4)
                (if numberedp (concat (car sec) "\n%s" (nth 1 sec))
                  (concat (nth 2 sec) "\n%s" (nth 3 sec)))))))
           (pre-blanks
            (make-string (org-element-property :pre-blank headline) ?\n)))
      (let ((opt-title
             (funcall (plist-get info :latex-format-headline-function)
                      todo todo-type priority
                      (org-export-data-with-backend
                       (org-export-get-alt-title headline info)
                       section-back-end info)
                      (and (eq (plist-get info :with-tags) t) tags)
                      info)))
        (format section-fmt full-text
                (concat headline-label pre-blanks contents))))))

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
      (format "\\starttyping[option=%s] %s \\stoptyping " lang code))))

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
        (format "\\desc{%s} %s" tag (org-trim contents))
      (format "\\item %s" (org-trim contents))))
  )

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
    (concat
     open-command
     contents
     close-command)))

(defun org-context-quote-block (quote-block contents info)
  "Transcodes a QUOTE-BLOCK element from Org to ConTeXt."
  ;; TODO Wrap a label around quotes
  (org-context--text-markup contents 'quotation info))

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
  ;; TODO `org-latex--caption/label-string' needs to be
  ;; replaced.
  (let ((type (org-element-property :type special-block))
        (opt (org-export-read-attribute :attr_latex special-block :options))
        (caption (org-latex--caption/label-string special-block info))
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
       ((not lang) (format "\\starttyping\n%s\\stoptyping"
                           (org-export-format-code-default src-block info)))
       (t (format "\\starttyping[option=%s]\n%s\\stoptyping"
                  lang
                  (org-export-format-code-default src-block info)))))))

(defun org-context-underline (_underline contents info)
  "Transcode UNDERLINE from Org to ConTeXt"
  (org-context--text-markup contents 'underline info))

(defun org-context-verbatim (verbatim _contents info)
  "Transcode a VERBATIM object from Org to ConTeXt"
  (org-context--text-markup
   (org-element-property :value verbatim) 'verbatim info))

;;;###autoload
(defun org-context-export-to-context
    (&optional async subtreep visible-only body-only ext-plist)
  "Export the current buffer as a ConTeXt document (.mkiv)"
  (interactive)
  (let ((file (org-export-output-file-name ".mkiv" subtreep)))
    (org-export-to-file 'context file
      async subtreep visible-only body-only ext-plist)))
