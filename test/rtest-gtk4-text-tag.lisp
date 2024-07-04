(in-package :gtk-test)

(def-suite gtk-text-tag :in gtk-suite)
(in-suite gtk-text-tag)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextTag

(test gtk-text-tag-class
  ;; Check type
  (is (g:type-is-object "GtkTextTag"))
  ;; Check registered name
  (is (eq 'gtk:text-tag
          (glib:symbol-for-gtype "GtkTextTag")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextTag")
          (g:gtype (cffi:foreign-funcall "gtk_text_tag_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTextTag")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkTextTag")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkTextTag")))
  ;; Check properties
  (is (equal '("accumulative-margin" "allow-breaks" "allow-breaks-set"
               "background" "background-full-height"
               "background-full-height-set" "background-rgba" "background-set"
               "direction" "editable" "editable-set" "fallback" "fallback-set"
               "family" "family-set" "font" "font-desc" "font-features"
               "font-features-set" "foreground" "foreground-rgba"
               "foreground-set" "indent" "indent-set" "insert-hyphens"
               "insert-hyphens-set" "invisible" "invisible-set" "justification"
               "justification-set" "language" "language-set" "left-margin"
               "left-margin-set" "letter-spacing" "letter-spacing-set"
               "line-height" "line-height-set" "name" "overline" "overline-rgba"
               "overline-rgba-set" "overline-set" "paragraph-background"
               "paragraph-background-rgba" "paragraph-background-set"
               "pixels-above-lines" "pixels-above-lines-set"
               "pixels-below-lines" "pixels-below-lines-set"
               "pixels-inside-wrap" "pixels-inside-wrap-set" "right-margin"
               "right-margin-set" "rise" "rise-set" "scale" "scale-set"
               "sentence" "sentence-set" "show-spaces" "show-spaces-set" "size"
               "size-points" "size-set" "stretch" "stretch-set" "strikethrough"
               "strikethrough-rgba" "strikethrough-rgba-set" "strikethrough-set"
               "style" "style-set" "tabs" "tabs-set" "text-transform"
               "text-transform-set" "underline" "underline-rgba"
               "underline-rgba-set" "underline-set" "variant" "variant-set"
               "weight" "weight-set" "word" "word-set" "wrap-mode"
               "wrap-mode-set")
             (gtk-test:list-properties "GtkTextTag")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkTextTag")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTextTag" GTK-TEXT-TAG
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_text_tag_get_type")
                               ((ACCUMULATIVE-MARGIN
                                 GTK-TEXT-TAG-ACCUMULATIVE-MARGIN
                                 "accumulative-margin" "gboolean" T T)
                                (ALLOW-BREAKS GTK-TEXT-TAG-ALLOW-BREAKS
                                 "allow-breaks" "gboolean" T T)
                                (ALLOW-BREAKS-SET GTK-TEXT-TAG-ALLOW-BREAKS-SET
                                 "allow-breaks-set" "gboolean" T T)
                                (BACKGROUND GTK-TEXT-TAG-BACKGROUND
                                 "background" "gchararray" NIL T)
                                (BACKGROUND-FULL-HEIGHT
                                 GTK-TEXT-TAG-BACKGROUND-FULL-HEIGHT
                                 "background-full-height" "gboolean" T T)
                                (BACKGROUND-FULL-HEIGHT-SET
                                 GTK-TEXT-TAG-BACKGROUND-FULL-HEIGHT-SET
                                 "background-full-height-set" "gboolean" T T)
                                (BACKGROUND-RGBA GTK-TEXT-TAG-BACKGROUND-RGBA
                                 "background-rgba" "GdkRGBA" T T)
                                (BACKGROUND-SET GTK-TEXT-TAG-BACKGROUND-SET
                                 "background-set" "gboolean" T T)
                                (DIRECTION GTK-TEXT-TAG-DIRECTION "direction"
                                 "GtkTextDirection" T T)
                                (EDITABLE GTK-TEXT-TAG-EDITABLE "editable"
                                 "gboolean" T T)
                                (EDITABLE-SET GTK-TEXT-TAG-EDITABLE-SET
                                 "editable-set" "gboolean" T T)
                                (FALLBACK GTK-TEXT-TAG-FALLBACK "fallback"
                                 "gboolean" T T)
                                (FALLBACK-SET GTK-TEXT-TAG-FALLBACK-SET
                                 "fallback-set" "gboolean" T T)
                                (FAMILY GTK-TEXT-TAG-FAMILY "family"
                                 "gchararray" T T)
                                (FAMILY-SET GTK-TEXT-TAG-FAMILY-SET
                                 "family-set" "gboolean" T T)
                                (FONT GTK-TEXT-TAG-FONT "font" "gchararray" T
                                 T)
                                (FONT-DESC GTK-TEXT-TAG-FONT-DESC "font-desc"
                                 "PangoFontDescription" T T)
                                (FONT-FEATURES GTK-TEXT-TAG-FONT-FEATURES
                                 "font-features" "gchararray" T T)
                                (FONT-FEATURES-SET
                                 GTK-TEXT-TAG-FONT-FEATURES-SET
                                 "font-features-set" "gboolean" T T)
                                (FOREGROUND GTK-TEXT-TAG-FOREGROUND
                                 "foreground" "gchararray" NIL T)
                                (FOREGROUND-RGBA GTK-TEXT-TAG-FOREGROUND-RGBA
                                 "foreground-rgba" "GdkRGBA" T T)
                                (FOREGROUND-SET GTK-TEXT-TAG-FOREGROUND-SET
                                 "foreground-set" "gboolean" T T)
                                (INDENT GTK-TEXT-TAG-INDENT "indent" "gint" T
                                 T)
                                (INDENT-SET GTK-TEXT-TAG-INDENT-SET
                                 "indent-set" "gboolean" T T)
                                (INSERT-HYPHENS GTK-TEXT-TAG-INSERT-HYPHENS
                                 "insert-hyphens" "gboolean" T T)
                                (INSERT-HYPHENS-SET
                                 GTK-TEXT-TAG-INSERT-HYPHENS-SET
                                 "insert-hyphens-set" "gboolean" T T)
                                (INVISIBLE GTK-TEXT-TAG-INVISIBLE "invisible"
                                 "gboolean" T T)
                                (INVISIBLE-SET GTK-TEXT-TAG-INVISIBLE-SET
                                 "invisible-set" "gboolean" T T)
                                (JUSTIFICATION GTK-TEXT-TAG-JUSTIFICATION
                                 "justification" "GtkJustification" T T)
                                (JUSTIFICATION-SET
                                 GTK-TEXT-TAG-JUSTIFICATION-SET
                                 "justification-set" "gboolean" T T)
                                (LANGUAGE GTK-TEXT-TAG-LANGUAGE "language"
                                 "gchararray" T T)
                                (LANGUAGE-SET GTK-TEXT-TAG-LANGUAGE-SET
                                 "language-set" "gboolean" T T)
                                (LEFT-MARGIN GTK-TEXT-TAG-LEFT-MARGIN
                                 "left-margin" "gint" T T)
                                (LEFT-MARGIN-SET GTK-TEXT-TAG-LEFT-MARGIN-SET
                                 "left-margin-set" "gboolean" T T)
                                (LETTER-SPACING GTK-TEXT-TAG-LETTER-SPACING
                                 "letter-spacing" "gint" T T)
                                (LETTER-SPACING-SET
                                 GTK-TEXT-TAG-LETTER-SPACING-SET
                                 "letter-spacing-set" "gboolean" T T)
                                (LINE-HEIGHT GTK-TEXT-TAG-LINE-HEIGHT
                                 "line-height" "gfloat" T T)
                                (LINE-HEIGHT-SET GTK-TEXT-TAG-LINE-HEIGHT-SET
                                 "line-height-set" "gboolean" T T)
                                (NAME GTK-TEXT-TAG-NAME "name" "gchararray" T
                                 NIL)
                                (OVERLINE GTK-TEXT-TAG-OVERLINE "overline"
                                 "PangoOverline" T T)
                                (OVERLINE-RGBA GTK-TEXT-TAG-OVERLINE-RGBA
                                 "overline-rgba" "GdkRGBA" T T)
                                (OVERLINE-RGBA-SET
                                 GTK-TEXT-TAG-OVERLINE-RGBA-SET
                                 "overline-rgba-set" "gboolean" T T)
                                (OVERLINE-SET GTK-TEXT-TAG-OVERLINE-SET
                                 "overline-set" "gboolean" T T)
                                (PARAGRAPH-BACKGROUND
                                 GTK-TEXT-TAG-PARAGRAPH-BACKGROUND
                                 "paragraph-background" "gchararray" NIL T)
                                (PARAGRAPH-BACKGROUND-RGBA
                                 GTK-TEXT-TAG-PARAGRAPH-BACKGROUND-RGBA
                                 "paragraph-background-rgba" "GdkRGBA" T T)
                                (PARAGRAPH-BACKGROUND-SET
                                 GTK-TEXT-TAG-PARAGRAPH-BACKGROUND-SET
                                 "paragraph-background-set" "gboolean" T T)
                                (PIXELS-ABOVE-LINES
                                 GTK-TEXT-TAG-PIXELS-ABOVE-LINES
                                 "pixels-above-lines" "gint" T T)
                                (PIXELS-ABOVE-LINES-SET
                                 GTK-TEXT-TAG-PIXELS-ABOVE-LINES-SET
                                 "pixels-above-lines-set" "gboolean" T T)
                                (PIXELS-BELOW-LINES
                                 GTK-TEXT-TAG-PIXELS-BELOW-LINES
                                 "pixels-below-lines" "gint" T T)
                                (PIXELS-BELOW-LINES-SET
                                 GTK-TEXT-TAG-PIXELS-BELOW-LINES-SET
                                 "pixels-below-lines-set" "gboolean" T T)
                                (PIXELS-INSIDE-WRAP
                                 GTK-TEXT-TAG-PIXELS-INSIDE-WRAP
                                 "pixels-inside-wrap" "gint" T T)
                                (PIXELS-INSIDE-WRAP-SET
                                 GTK-TEXT-TAG-PIXELS-INSIDE-WRAP-SET
                                 "pixels-inside-wrap-set" "gboolean" T T)
                                (RIGHT-MARGIN GTK-TEXT-TAG-RIGHT-MARGIN
                                 "right-margin" "gint" T T)
                                (RIGHT-MARGIN-SET GTK-TEXT-TAG-RIGHT-MARGIN-SET
                                 "right-margin-set" "gboolean" T T)
                                (RISE GTK-TEXT-TAG-RISE "rise" "gint" T T)
                                (RISE-SET GTK-TEXT-TAG-RISE-SET "rise-set"
                                 "gboolean" T T)
                                (SCALE GTK-TEXT-TAG-SCALE "scale" "gdouble" T
                                 T)
                                (SCALE-SET GTK-TEXT-TAG-SCALE-SET "scale-set"
                                 "gboolean" T T)
                                (SENTENCE GTK-TEXT-TAG-SENTENCE "sentence"
                                 "gboolean" T T)
                                (SENTENCE-SET GTK-TEXT-TAG-SENTENCE-SET
                                 "sentence-set" "gboolean" T T)
                                (SHOW-SPACES GTK-TEXT-TAG-SHOW-SPACES
                                 "show-spaces" "PangoShowFlags" T T)
                                (SHOW-SPACES-SET GTK-TEXT-TAG-SHOW-SPACES-SET
                                 "show-spaces-set" "gboolean" T T)
                                (SIZE GTK-TEXT-TAG-SIZE "size" "gint" T T)
                                (SIZE-POINTS GTK-TEXT-TAG-SIZE-POINTS
                                 "size-points" "gdouble" T T)
                                (SIZE-SET GTK-TEXT-TAG-SIZE-SET "size-set"
                                 "gboolean" T T)
                                (STRETCH GTK-TEXT-TAG-STRETCH "stretch"
                                 "PangoStretch" T T)
                                (STRETCH-SET GTK-TEXT-TAG-STRETCH-SET
                                 "stretch-set" "gboolean" T T)
                                (STRIKETHROUGH GTK-TEXT-TAG-STRIKETHROUGH
                                 "strikethrough" "gboolean" T T)
                                (STRIKETHROUGH-RGBA
                                 GTK-TEXT-TAG-STRIKETHROUGH-RGBA
                                 "strikethrough-rgba" "GdkRGBA" T T)
                                (STRIKETHROUGH-RGBA-SET
                                 GTK-TEXT-TAG-STRIKETHROUGH-RGBA-SET
                                 "strikethrough-rgba-set" "gboolean" T T)
                                (STRIKETHROUGH-SET
                                 GTK-TEXT-TAG-STRIKETHROUGH-SET
                                 "strikethrough-set" "gboolean" T T)
                                (STYLE GTK-TEXT-TAG-STYLE "style" "PangoStyle"
                                 T T)
                                (STYLE-SET GTK-TEXT-TAG-STYLE-SET "style-set"
                                 "gboolean" T T)
                                (TABS GTK-TEXT-TAG-TABS "tabs" "PangoTabArray"
                                 T T)
                                (TABS-SET GTK-TEXT-TAG-TABS-SET "tabs-set"
                                 "gboolean" T T)
                                (TEXT-TRANSFORM GTK-TEXT-TAG-TEXT-TRANSFORM
                                 "text-transform" "PangoTextTransform" T T)
                                (TEXT-TRANSFORM-SET
                                 GTK-TEXT-TAG-TEXT-TRANSFORM-SET
                                 "text-transform-set" "gboolean" T T)
                                (UNDERLINE GTK-TEXT-TAG-UNDERLINE "underline"
                                 "PangoUnderline" T T)
                                (UNDERLINE-RGBA GTK-TEXT-TAG-UNDERLINE-RGBA
                                 "underline-rgba" "GdkRGBA" T T)
                                (UNDERLINE-RGBA-SET
                                 GTK-TEXT-TAG-UNDERLINE-RGBA-SET
                                 "underline-rgba-set" "gboolean" T T)
                                (UNDERLINE-SET GTK-TEXT-TAG-UNDERLINE-SET
                                 "underline-set" "gboolean" T T)
                                (VARIANT GTK-TEXT-TAG-VARIANT "variant"
                                 "PangoVariant" T T)
                                (VARIANT-SET GTK-TEXT-TAG-VARIANT-SET
                                 "variant-set" "gboolean" T T)
                                (WEIGHT GTK-TEXT-TAG-WEIGHT "weight" "gint" T
                                 T)
                                (WEIGHT-SET GTK-TEXT-TAG-WEIGHT-SET
                                 "weight-set" "gboolean" T T)
                                (WORD GTK-TEXT-TAG-WORD "word" "gboolean" T T)
                                (WORD-SET GTK-TEXT-TAG-WORD-SET "word-set"
                                 "gboolean" T T)
                                (WRAP-MODE GTK-TEXT-TAG-WRAP-MODE "wrap-mode"
                                 "GtkWrapMode" T T)
                                (WRAP-MODE-SET GTK-TEXT-TAG-WRAP-MODE-SET
                                 "wrap-mode-set" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkTextTag"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-text-tag-properties
  (let ((tag (make-instance 'gtk:text-tag)))
    (is-false (gtk:text-tag-accumulative-margin tag))
    (is-true (gtk:text-tag-allow-breaks tag))
    (is-false (gtk:text-tag-allow-breaks-set tag))
    (signals (error) (gtk:text-tag-background tag))
    (is-false (gtk:text-tag-background-full-height tag))
    (is-false (gtk:text-tag-background-full-height-set tag))
    (is-false (gtk:text-tag-background-rgba tag))
    (is-false (gtk:text-tag-background-set tag))
    (is (eq :none (gtk:text-tag-direction tag)))
    (is-true (gtk:text-tag-editable tag))
    (is-false (gtk:text-tag-editable-set tag))
    (is-true (gtk:text-tag-fallback tag))
    (is-false (gtk:text-tag-fallback-set tag))
    (is-false (gtk:text-tag-family tag))
    (is-false (gtk:text-tag-family-set tag))
    (is (string= "Normal" (gtk:text-tag-font tag)))
    (is (typep (gtk:text-tag-font-desc tag) 'pango:font-description))
    (is-false (gtk:text-tag-font-features tag))
    (is-false (gtk:text-tag-font-features-set tag))
    (signals (error) (gtk:text-tag-foreground tag))
    (is-false (gtk:text-tag-foreground-rgba tag))
    (is-false (gtk:text-tag-foreground-set tag))
    (is (= 0 (gtk:text-tag-indent tag)))
    (is-false (gtk:text-tag-indent-set tag))
    (is-true (gtk:text-tag-insert-hyphens tag))
    (is-false (gtk:text-tag-insert-hyphens-set tag))
    (is-false (gtk:text-tag-invisible tag))
    (is-false (gtk:text-tag-invisible-set tag))
    (is (eq :left (gtk:text-tag-justification tag)))
    (is-false (gtk:text-tag-justification-set tag))
    (is (string= "de-de" (gtk:text-tag-language tag)))
    (is-false (gtk:text-tag-language-set tag))
    (is (= 0 (gtk:text-tag-left-margin tag)))
    (is-false (gtk:text-tag-left-margin-set tag))
    (is (= 0 (gtk:text-tag-letter-spacing tag)))
    (is-false (gtk:text-tag-letter-spacing-set tag))
    (is (= 0.0 (gtk:text-tag-line-height tag)))
    (is-false (gtk:text-tag-line-height-set tag))
    (is-false (gtk:text-tag-name tag))
    (is (eq :none (gtk:text-tag-overline tag)))
    (is-false (gtk:text-tag-overline-rgba tag))
    (is-false (gtk:text-tag-overline-set tag))
    (signals (error) (gtk:text-tag-paragraph-background tag))
    (is-false (gtk:text-tag-paragraph-background-rgba tag))
    (is-false (gtk:text-tag-paragraph-background-set tag))
    (is (= 0 (gtk:text-tag-pixels-above-lines tag)))
    (is-false (gtk:text-tag-pixels-above-lines-set tag))
    (is (= 0 (gtk:text-tag-pixels-below-lines tag)))
    (is-false (gtk:text-tag-pixels-below-lines-set tag))
    (is (= 0 (gtk:text-tag-pixels-inside-wrap tag)))
    (is-false (gtk:text-tag-pixels-inside-wrap-set tag))
    (is (= 0 (gtk:text-tag-right-margin tag)))
    (is-false (gtk:text-tag-right-margin-set tag))
    (is (= 0 (gtk:text-tag-rise tag)))
    (is-false (gtk:text-tag-rise-set tag))
    (is (= 1.0d0 (gtk:text-tag-scale tag)))
    (is-false (gtk:text-tag-scale-set tag))
    (is-false (gtk:text-tag-sentence tag))
    (is-false (gtk:text-tag-sentence-set tag))
    (is-false (gtk:text-tag-show-spaces tag))
    (is-false (gtk:text-tag-show-spaces-set tag))
    (is (= 0 (gtk:text-tag-size tag)))
    (is (= 0.0d0 (gtk:text-tag-size-points tag)))
    (is-false (gtk:text-tag-size-set tag))
    (is (eq :normal (gtk:text-tag-stretch tag)))
    (is-false (gtk:text-tag-stretch-set tag))
    (is-false (gtk:text-tag-strikethrough tag))
    (is-false (gtk:text-tag-strikethrough-rgba tag))
    (is-false (gtk:text-tag-strikethrough-rgba-set tag))
    (is-false (gtk:text-tag-strikethrough-set tag))
    (is (eq :normal (gtk:text-tag-style tag)))
    (is-false (gtk:text-tag-style-set tag))
    (is-false (gtk:text-tag-tabs tag))
    (is-false (gtk:text-tag-tabs-set tag))
    (is (eq :none (gtk:text-tag-text-transform tag)))
    (is-false (gtk:text-tag-text-transform-set tag))
    (is (eq :none (gtk:text-tag-underline tag)))
    (is-false (gtk:text-tag-underline-rgba tag))
    (is-false (gtk:text-tag-underline-rgba-set tag))
    (is-false (gtk:text-tag-underline-set tag))
    (is (eq :normal (gtk:text-tag-variant tag)))
    (is-false (gtk:text-tag-variant-set tag))
    (is (= 400 (gtk:text-tag-weight tag)))
    (is-false (gtk:text-tag-weight-set tag))
    (is-false (gtk:text-tag-word tag))
    (is-false (gtk:text-tag-word-set tag))
    (is (eq :none (gtk:text-tag-wrap-mode tag)))
    (is-false (gtk:text-tag-wrap-mode-set tag))))

;;;     accumulative-margin

(test gtk-text-tag-accumulative-margin
  (let ((tag (gtk:text-tag-new "margin" :accumulative-margin t)))
    (is-true (gtk:text-tag-accumulative-margin tag))
    (is-false (setf (gtk:text-tag-accumulative-margin tag) nil))
    (is-false (gtk:text-tag-accumulative-margin tag))))

;;;     allow-breaks
;;;     allow-breaks-set

(test gtk-text-tag-allow-breaks
  (let ((tag (gtk:text-tag-new "allow-breaks" :allow-breaks t)))
    (is-true (gtk:text-tag-allow-breaks tag))
    (is-true (gtk:text-tag-allow-breaks-set tag))
    (is-false (setf (gtk:text-tag-allow-breaks tag) nil))
    (is-true (gtk:text-tag-allow-breaks-set tag))))

;;;     background
;;;     background-rgba
;;;     background-set

(test gtk-text-tag-background
  (let ((tag (gtk:text-tag-new "background" :background "red")))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1.0 :alpha 1.0)
                        (gtk:text-tag-background-rgba tag)))
    (is-true (gtk:text-tag-background-set tag))))

;;;     background-full-height
;;;     background-full-height-set

(test gtk-text-tag-background-full-height
  (let ((tag (gtk:text-tag-new "background" :background-full-height t)))
    (is-true (gtk:text-tag-background-full-height tag))
    (is-true (gtk:text-tag-background-full-height-set tag))))

;;;     direction
;;;     editable
;;;     editable-set
;;;     fallback
;;;     fallback-set
;;;     family
;;;     family-set
;;;     font
;;;     font-desc
;;;     font-features
;;;     font-features-set
;;;     foreground
;;;     foreground-rgba
;;;     foreground-set
;;;     indent
;;;     indent-set
;;;     insert-hyphens
;;;     insert-hyphens-set
;;;     invisible
;;;     invisible-set
;;;     justification
;;;     justification-set
;;;     language
;;;     language-set
;;;     left-margin
;;;     left-margin-set
;;;     letter-spacing
;;;     letter-spacing-set
;;;     line-height                                        Since 4.6
;;;     line-height-set                                    Since 4.6
;;;     name
;;;     overline
;;;     overline-rgba
;;;     overline-set
;;;     paragraph-background
;;;     paragraph-background-rgba
;;;     paragraph-background-set
;;;     pixels-above-lines
;;;     pixels-above-lines-set
;;;     pixels-below-lines
;;;     pixels-below-lines-set
;;;     pixels-inside-wrap
;;;     pixels-inside-wrap-set
;;;     right-margin
;;;     right-margin-set
;;;     rise
;;;     rise-set
;;;     scale
;;;     scale-set
;;;     sentence                                           Since 4.6
;;;     sentence-set                                       Since 4.6
;;;     show-spaces
;;;     show-spaces-set
;;;     size
;;;     size-points
;;;     size-set
;;;     stretch
;;;     stretch-set
;;;     strikethrough
;;;     strikethrough-rgba
;;;     strikethrough-rgba-set
;;;     strikethrough-set
;;;     style
;;;     style-set
;;;     tabs
;;;     tabs-set
;;;     text-transform                                     Since 4,6
;;;     text-transform-set                                 Since 4.6
;;;     underline
;;;     underline-rgba
;;;     underline-rgba-set
;;;     underline-set
;;;     variant
;;;     variant-set
;;;     weight
;;;     weight-set
;;;     word                                               Since 4.6
;;;     word-set                                           Since 4.6
;;;     wrap-mode
;;;     wrap-mode-set

;;; ---  Functions -------------------------------------------------------------

;;;     gtk_text_tag_new

(test gtk-text-tag-new
  (is (typep (gtk:text-tag-new nil) 'gtk:text-tag))
  (is (typep (gtk:text-tag-new "name") 'gtk:text-tag))
  (is (typep (gtk:text-tag-new nil :indent 10 :direction :ltr) 'gtk:text-tag)))

;;;     gtk_text_tag_get_priority
;;;     gtk_text_tag_set_priority

(test gtk-text-tag-priority
  (let ((table (gtk:text-tag-table-new)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "direction"
                                                       :direction :ltr)))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "monospace"
                                                       :family "monospace")))
    (is-true (gtk:text-tag-table-add table
                                     (gtk:text-tag-new "indent"
                                                       :indent 10)))

    (is (= 0 (gtk:text-tag-priority
                 (gtk:text-tag-table-lookup table "direction"))))
    (is (= 1 (gtk:text-tag-priority
                 (gtk:text-tag-table-lookup table "monospace"))))
    (is (= 2 (gtk:text-tag-priority
                 (gtk:text-tag-table-lookup table "indent"))))

    (let ((tag (gtk:text-tag-table-lookup table "direction")))
      (is (= 1 (setf (gtk:text-tag-priority tag) 1)))
      (is (= 1 (gtk:text-tag-priority
                   (gtk:text-tag-table-lookup table "direction"))))
      (is (= 0 (gtk:text-tag-priority
                   (gtk:text-tag-table-lookup table "monospace"))))
      (is (= 2 (gtk:text-tag-priority
                   (gtk:text-tag-table-lookup table "indent")))))))

;;;     gtk_text_tag_changed

(test gtk:text-tag-changed
  (let ((table (gtk:text-tag-table-new))
        (tag (gtk:text-tag-new "indent" :indent 10))
        (message nil))

    (g:signal-connect table "tag-changed"
                      (lambda (table tag size-changed)
                        (declare (ignore table size-changed))
                        (push (gtk:text-tag-name tag) message)))

    (is-true (gtk:text-tag-table-add table tag))
    (is-false (gtk:text-tag-changed tag t))
    (is (equal '("indent") message))
    (is (= 20 (setf (gtk:text-tag-indent tag) 20)))
    (is (equal '("indent" "indent") message))))

;;; 2024-7-2
