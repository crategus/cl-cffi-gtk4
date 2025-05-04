;;;; Unicode Character Database
;;;;
;;;; 2025-05-04

(in-package :gtk4-example)

(defun get-unicode-category (category)
  (or (cdr (assoc category
                  '((:cc . "Other, Control (Cc)")
                    (:cf . "Other, Format (Cf)")
                    (:cn . "Other, Not Assigned (Cn)")
                    (:co . "Other, Private Use (Co)")
                    (:cs . "Other, Surrogate (Cs)")
                    (:ll . "Letter, Lowercase (Ll)")
                    (:lm . "Letter, Modifier (Lm)")
                    (:lo . "Letter, Other (Lo)")
                    (:lt . "Letter, Titlecase (Lt)")
                    (:lu . "Letter, Uppercase (Lu)")
                    (:mc . "Mark, Spacing (Mc)")
                    (:me . "Mark, Enclosing (Me)")
                    (:mn . "Mark, Nonspacing (Mn)")
                    (:nd . "Number, Decimal (Nd)")
                    (:nl . "Number, Letter (Nl)")
                    (:no . "Number, Other (No)")
                    (:pc . "Punctuation, Connector (Pc)")
                    (:pd . "Punctuation, Dash (Pd)")
                    (:pe . "Punctuation, Close (Pe)")
                    (:pf . "Punctuation, Final quote (Pf)")
                    (:pi . "Punctuation, Initial quote (Pi)")
                    (:po . "Punctuation, Other (Po)")
                    (:ps . "Punctuation, Open (Ps)")
                    (:sc . "Symbol, Currency (Sc)")
                    (:sk . "Symbol, Modifier (Sk)")
                    (:sm . "Symbol, Math (Sm)")
                    (:so . "Symbol, Other (So)")
                    (:zl . "Separator, Line (Zl)")
                    (:zp . "Separator, Paragraph (Zp)")
                    (:zs . "Separator, Space (Zs)"))))
      ""))

(defun get-unicode-bidi-class (bidiclass)
  (or (cdr (assoc bidiclass
                  '((:l . "Left-to-Right")
                    (:r . "Right-to-Left")
                    (:al . "Arabic Letter")
                    (:en . "European Number")
                    (:es . "European Separator")
                    (:et . "European Number Terminator")
                    (:an . "Arabic Number")
                    (:cs . "Common Number Separator")
                    (:nsm . "Nonspacing Mark")
                    (:bn . "Boundary Neutral")
                    (:b . "Paragraph Separator")
                    (:s . "Segment Separator")
                    (:ws . "Whitespace")
                    (:on . "Other Neutrals")
                    (:lre . "Left-to-Right Embedding")
                    (:lro . "Left-to-Right Override")
                    (:rle . "Right-to-Left Embedding")
                    (:rlo . "Right-to-Left Override")
                    (:pdf . "Pop Directional Format")
                    (:lri . "Left-to-right Isolate")
                    (:rli . "Right-to-left Isolate")
                    (:fsi . "First Strong Isolate")
                    (:pdi . "Pop Directional Isolate"))))
      ""))

(defun get-unicode-breaktype (breaktype)
  (or (cdr (assoc breaktype
                  '((:bk . "Mandatory Break (BK)")
                    (:cr . "Carriage Return (CR)")
                    (:lf . "Line Feed (LF)")
                    (:cm . "Attached Characters and Combining Marks (CM)")
                    (:sg . "Surrogates (SG)")
                    (:zw . "Zero Width Space (ZW)")
                    (:in . "Inseparable (IN)")
                    (:gl . "Non-breaking (Glue) (GL)")
                    (:cb . "Contingent Break Opportunity (CB)")
                    (:sp . "Space (SP)")
                    (:ba . "Break Opportunity After (BA)")
                    (:bb . "Break Opportunity Before (BB)")
                    (:b2 . "Break Opportunity Before and After (B2)")
                    (:hy . "Hyphen (HY)")
                    (:ns . "Nonstarter (NS)")
                    (:op . "Opening Punctuation (OP)")
                    (:cl . "Closing Punctuation (CL)")
                    (:qu . "Ambiguous Quotation (QU)")
                    (:ex . "Exclamation/Interrogation (EX)")
                    (:id . "Ideographic (ID)")
                    (:nu . "Numeric (NU)")
                    (:is . "Infix Separator (Numeric) (IS)")
                    (:sy . "Symbols Allowing Break After (SY)")
                    (:al . "Ordinary Alphabetic and Symbol Characters (AL)")
                    (:pr . "Prefix (Numeric) (PR)")
                    (:po . "Postfix (Numeric) (PO)")
                    (:sa . "Complex Content Dependent (South East Asian) (SA)")
                    (:ai . "Ambiguous (Alphabetic or Ideographic) (AI)")
                    (:xx . "Unknown (XX)")
                    (:nl . "Next Line (NL)")
                    (:wj . "Word Joiner (WJ)")
                    (:jl . "Hangul L Jamo (JL)")
                    (:jv . "Hangul V Jamo (JV)")
                    (:jt . "Hangul T Jamo (JT)")
                    (:h2 . "Hangul LV Syllable (H2)")
                    (:h3 . "Hangul LVT Syllable (H3)")
                    (:cp . "Closing Parenthesis (CP)")
                    (:cj . "Conditional Japanese Starter (CJ)")
                    (:hl . "Hebrew Letter (HL)")
                    (:ri . "Regional Indicator (RI)")
                    (:eb . "Emoji Base (EB)")
                    (:em . "Emoji Modifier (EM)")
                    (:zwj . "Zero Width Joiner (ZWJ)")
                    (:ak . "Aksara (AK)")
                    (:ap . "Aksara Pre-Base (AP)")
                    (:as . "Aksara Start (AS)")
                    (:vf . "Virama Final (VF)")
                    (:vi . "Virama (VI)"))))
      ""))

(gobject:define-gobject-subclass "UcdItem" ucd-item
  (:superclass g:object
   :export t
   :interfaces ())
  ((codepoint
    ucd-item-codepoint
    "codepoint" "guint" t t)
   (name
    ucd-item-name
    "name" "gchararray" t t)))

(defun ucd-item-new (codepoint name)
 (make-instance 'ucd-item
                :codepoint codepoint
                :name name))

(defun ucd-model-new ()
  (let* ((bytes (g:resources-lookup-data "/listview-ucd-data/ucdnames.data"))
         (variant (g:variant-new-from-bytes "a(us)" bytes t))
         (iter (g:variant-iter-new variant))
         (store (g:list-store-new "GObject")))
    (iter (for value = (g:variant-iter-next-value iter))
          (while value)
          (cffi:with-foreign-objects ((u :int) (name :string))
            (cffi:foreign-funcall-varargs "g_variant_get"
                                          (:pointer value
                                           :string "(u&s)")
                                          :pointer u
                                          :pointer name)
            (unless (= 0 (cffi:mem-ref u :int))
              (g:list-store-append store
                                   (ucd-item-new (cffi:mem-ref u :int)
                                                 (cffi:mem-ref name :string))))
            (g:variant-unref value)))
    (g:variant-iter-free iter)
    store))

(defun create-ucd-view (label)
  (let* ((model (ucd-model-new))
         (selection (make-instance 'gtk:single-selection
                                   :model model
                                   :autoselect t
                                   :can-unselet nil))
         (columnview (make-instance 'gtk:column-view
                                    :model selection
                                    :show-column-separators t)))
    (when label
      (g:signal-connect selection "notify::selected"
          (lambda (selection pspec)
            (declare (ignore pspec))
            (let* ((item (gtk:single-selection-selected-item selection))
                   (codepoint (ucd-item-codepoint item))
                   (ch (code-char codepoint))
                   (text ""))
              (when (graphic-char-p ch)
                (setf text (format nil "~a" ch)))
              (setf (gtk:label-label label) text)))))
    ;; First column for "Codepoint"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (gtk:column-view-column-new "Codepoint" factory)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (setf (gtk:list-item-child listitem)
                  (make-instance 'gtk:inscription
                                 :text ""
                                 :xalign 0.5))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item  (gtk:list-item-item listitem))
                   (codepoint (ucd-item-codepoint item)))
              (setf (gtk:inscription-text label)
                    (format nil "0x~6,'0x" codepoint)))))
      (gtk:column-view-append-column columnview column))
    ;; Second column for "Char"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (gtk:column-view-column-new "Char" factory)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (setf (gtk:list-item-child listitem) (gtk:label-new ""))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item  (gtk:list-item-item listitem))
                   (ch (code-char (ucd-item-codepoint item))))
              (if (graphic-char-p ch)
                  (setf (gtk:label-label label) (format nil "~a" ch))
                  (setf (gtk:label-label label) "")))))
      (gtk:column-view-append-column columnview column))
    ;; Third column for "Name"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (make-instance 'gtk:column-view-column
                                  :title "Name"
                                  :factory factory
                                  :resizable t)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 0
                                        :ellipsize :end
                                        :width-chars 20)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item (gtk:list-item-item listitem))
                   (name (ucd-item-name item)))
              (setf (gtk:label-label label) name))))
      (gtk:column-view-append-column columnview column))
    #+sbcl ; Needs SB-UNICODE package
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (make-instance 'gtk:column-view-column
                                  :title "Script"
                                  :factory factory
                                  :resizable t)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 0
                                        :ellipsize :end
                                        :width-chars 20)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item (gtk:list-item-item  listitem))
                   (codepoint (ucd-item-codepoint item))
                   (script (sb-unicode:script (code-char codepoint)))
                   (text (format nil "~a"script)))
              (setf (gtk:label-label label) text))))
      (gtk:column-view-append-column columnview column))
    #+sbcl ; Needs SB-UNICODE package
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (make-instance 'gtk:column-view-column
                                  :title "Category"
                                  :factory factory
                                  :resizable t)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 0
                                        :ellipsize :end
                                        :width-chars 20)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item (gtk:list-item-item  listitem))
                   (codepoint (ucd-item-codepoint item))
                   (category (sb-unicode:general-category (code-char codepoint)))
                   (text (get-unicode-category category)))
              (setf (gtk:label-label label) text))))
      (gtk:column-view-append-column columnview column))
    #+sbcl ; Needs SB-UNICODE package
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (make-instance 'gtk:column-view-column
                                  :title "Bidi Class"
                                  :factory factory
                                  :resizable t)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 0
                                        :ellipsize :end
                                        :width-chars 20)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item (gtk:list-item-item  listitem))
                   (codepoint (ucd-item-codepoint item))
                   (bidiclass (sb-unicode:bidi-class (code-char codepoint)))
                   (text (get-unicode-bidi-class bidiclass)))
              (setf (gtk:label-label label) text))))
      (gtk:column-view-append-column columnview column))
    #+sbcl ; Needs SB-UNICODE package
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (make-instance 'gtk:column-view-column
                                  :title "Line Break Type"
                                  :factory factory
                                  :resizable t)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:label
                                        :label ""
                                        :xalign 0
                                        :ellipsize :end
                                        :width-chars 20)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item (gtk:list-item-item  listitem))
                   (codepoint (ucd-item-codepoint item))
                   (breaktype (sb-unicode:line-break-class (code-char codepoint)))
                   (text (get-unicode-breaktype breaktype)))
              (setf (gtk:label-label label) text))))
      (gtk:column-view-append-column columnview column))
    columnview))

(defun do-column-view-ucd (&optional application)
  (let* ((path (glib-sys:sys-path "resource/dialog-unicode.ui"))
         (builder (gtk:builder-new-from-file path))
         (dialog (gtk:builder-object builder "dialog"))
         (provider (gtk:css-provider-new))
         (box (make-instance 'gtk:box
                             :orientation :horizontal))
         (label (make-instance 'gtk:label
                               :hexpand t
                               :width-chars 2))
         (listview (create-ucd-view label))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child listview
                                  :propagate-natural-width t))
         (window (make-instance 'gtk:window
                               :title "Unicode Character Database"
                               :application application
                               :child box
                               :default-width 800
                               :default-height 600)))
    (gtk:widget-add-css-class label "enormous")
    (gtk:css-provider-load-from-string provider
                                       "label.enormous { font-size: 80px; }")
    (gtk:widget-add-provider label provider 800)
    (g:signal-connect listview "activate"
        (lambda (columnview pos)
          (let* ((model (gtk:column-view-model columnview))
                 (item (g:list-model-item model pos))
                 (codepoint (ucd-item-codepoint item))
                 (ch (code-char codepoint)))
            ;; Fill labels of the dialog
            (setf (gtk:label-label (gtk:builder-object builder "codepoint"))
                  (format nil "0x~x" codepoint))
            (setf (gtk:label-label (gtk:builder-object builder "char"))
                  (format nil "~a" ch))
            (setf (gtk:label-label (gtk:builder-object builder "category"))
                  (get-unicode-category (sb-unicode:general-category ch)))
            (setf (gtk:label-label (gtk:builder-object builder "bidi_class"))
                  (get-unicode-bidi-class (sb-unicode:bidi-class ch)))
            (setf (gtk:label-label (gtk:builder-object builder "combining_class"))
                  (format nil "~a" (sb-unicode:combining-class ch)))
            (setf (gtk:label-label (gtk:builder-object builder "script"))
                  (format nil "~a" (sb-unicode:script ch)))
            (setf (gtk:label-label (gtk:builder-object builder "grapheme_break"))
                  (get-unicode-breaktype (sb-unicode:grapheme-break-class ch)))
            (setf (gtk:label-label (gtk:builder-object builder "word_break"))
                  (get-unicode-breaktype (sb-unicode:word-break-class ch)))
            (setf (gtk:label-label (gtk:builder-object builder "sentence_break"))
                  (get-unicode-breaktype (sb-unicode:sentence-break-class ch)))
            (setf (gtk:label-label (gtk:builder-object builder "line_break"))
                  (get-unicode-breaktype (sb-unicode:line-break-class ch)))
            (gtk:window-present dialog))))
    ;; Pack the box
    (gtk:box-append box label)
    (gtk:box-prepend box scrolled)
    ;; Present the window
    (gtk:window-present window)))
