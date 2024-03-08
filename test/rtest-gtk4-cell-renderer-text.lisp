(in-package :gtk-test)

(def-suite gtk-cell-renderer-text :in gtk-suite)
(in-suite gtk-cell-renderer-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCellRendererText

(test gtk-cell-renderer-text-class
  ;; Type check
  (is (g:type-is-object "GtkCellRendererText"))
  ;; Check the registered name
  (is (eq 'gtk:cell-renderer-text
          (glib:symbol-for-gtype "GtkCellRendererText")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkCellRendererText")
          (g:gtype (cffi:foreign-funcall "gtk_cell_renderer_text_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkCellRenderer")
          (g:type-parent "GtkCellRendererText")))
  ;; Check the children
  (is (equal '("GtkCellRendererAccel" "GtkCellRendererCombo"
               "GtkCellRendererSpin")
             (list-children "GtkCellRendererText")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkCellRendererText")))
  ;; Check the properties
  (is (equal '("align-set" "alignment" "attributes" "background"
               "background-rgba" "background-set" "editable" "editable-set"
               "ellipsize" "ellipsize-set" "family" "family-set" "font"
               "font-desc" "foreground" "foreground-rgba" "foreground-set"
               "language" "language-set" "markup" "max-width-chars"
               "placeholder-text" "rise" "rise-set" "scale" "scale-set"
               "single-paragraph-mode" "size" "size-points" "size-set" "stretch"
               "stretch-set" "strikethrough" "strikethrough-set" "style"
               "style-set" "text" "underline" "underline-set" "variant"
               "variant-set" "weight" "weight-set" "width-chars" "wrap-mode"
               "wrap-width")
             (list-properties "GtkCellRendererText")))
  ;; Check the signals
  (is (equal '("edited")
             (list-signals "GtkCellRendererText")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCellRendererText" GTK-CELL-RENDERER-TEXT
                               (:SUPERCLASS GTK-CELL-RENDERER :EXPORT T
                                :INTERFACES NIL :TYPE-INITIALIZER
                                "gtk_cell_renderer_text_get_type")
                               ((ALIGN-SET GTK-CELL-RENDERER-TEXT-ALIGN-SET
                                 "align-set" "gboolean" T T)
                                (ALIGNMENT GTK-CELL-RENDERER-TEXT-ALIGNMENT
                                 "alignment" "PangoAlignment" T T)
                                (ATTRIBUTES GTK-CELL-RENDERER-TEXT-ATTRIBUTES
                                 "attributes" "PangoAttrList" T T)
                                (BACKGROUND GTK-CELL-RENDERER-TEXT-BACKGROUND
                                 "background" "gchararray" NIL T)
                                (BACKGROUND-RGBA
                                 GTK-CELL-RENDERER-TEXT-BACKGROUND-RGBA
                                 "background-rgba" "GdkRGBA" T T)
                                (BACKGROUND-SET
                                 GTK-CELL-RENDERER-TEXT-BACKGROUND-SET
                                 "background-set" "gboolean" T T)
                                (EDITABLE GTK-CELL-RENDERER-TEXT-EDITABLE
                                 "editable" "gboolean" T T)
                                (EDITABLE-SET
                                 GTK-CELL-RENDERER-TEXT-EDITABLE-SET
                                 "editable-set" "gboolean" T T)
                                (ELLIPSIZE GTK-CELL-RENDERER-TEXT-ELLIPSIZE
                                 "ellipsize" "PangoEllipsizeMode" T T)
                                (ELLIPSIZE-SET
                                 GTK-CELL-RENDERER-TEXT-ELLIPSIZE-SET
                                 "ellipsize-set" "gboolean" T T)
                                (FAMILY GTK-CELL-RENDERER-TEXT-FAMILY "family"
                                 "gchararray" T T)
                                (FAMILY-SET GTK-CELL-RENDERER-TEXT-FAMILY-SET
                                 "family-set" "gboolean" T T)
                                (FONT GTK-CELL-RENDERER-TEXT-FONT "font"
                                 "gchararray" T T)
                                (FONT-DESC GTK-CELL-RENDERER-TEXT-FONT-DESC
                                 "font-desc" "PangoFontDescription" T T)
                                (FOREGROUND GTK-CELL-RENDERER-TEXT-FOREGROUND
                                 "foreground" "gchararray" NIL T)
                                (FOREGROUND-RGBA
                                 GTK-CELL-RENDERER-TEXT-FOREGROUND-RGBA
                                 "foreground-rgba" "GdkRGBA" T T)
                                (FOREGROUND-SET
                                 GTK-CELL-RENDERER-TEXT-FOREGROUND-SET
                                 "foreground-set" "gboolean" T T)
                                (LANGUAGE GTK-CELL-RENDERER-TEXT-LANGUAGE
                                 "language" "gchararray" T T)
                                (LANGUAGE-SET
                                 GTK-CELL-RENDERER-TEXT-LANGUAGE-SET
                                 "language-set" "gboolean" T T)
                                (MARKUP GTK-CELL-RENDERER-TEXT-MARKUP "markup"
                                 "gchararray" NIL T)
                                (MAX-WIDTH-CHARS
                                 GTK-CELL-RENDERER-TEXT-MAX-WIDTH-CHARS
                                 "max-width-chars" "gint" T T)
                                (PLACEHOLDER-TEXT
                                 GTK-CELL-RENDERER-TEXT-PLACEHOLDER-TEXT
                                 "placeholder-text" "gchararray" T T)
                                (RISE GTK-CELL-RENDERER-TEXT-RISE "rise" "gint"
                                 T T)
                                (RISE-SET GTK-CELL-RENDERER-TEXT-RISE-SET
                                 "rise-set" "gboolean" T T)
                                (SCALE GTK-CELL-RENDERER-TEXT-SCALE "scale"
                                 "gdouble" T T)
                                (SCALE-SET GTK-CELL-RENDERER-TEXT-SCALE-SET
                                 "scale-set" "gboolean" T T)
                                (SINGLE-PARAGRAPH-MODE
                                 GTK-CELL-RENDERER-TEXT-SINGLE-PARAGRAPH-MODE
                                 "single-paragraph-mode" "gboolean" T T)
                                (SIZE GTK-CELL-RENDERER-TEXT-SIZE "size" "gint"
                                 T T)
                                (SIZE-POINTS GTK-CELL-RENDERER-TEXT-SIZE-POINTS
                                 "size-points" "gdouble" T T)
                                (SIZE-SET GTK-CELL-RENDERER-TEXT-SIZE-SET
                                 "size-set" "gboolean" T T)
                                (STRETCH GTK-CELL-RENDERER-TEXT-STRETCH
                                 "stretch" "PangoStretch" T T)
                                (STRETCH-SET GTK-CELL-RENDERER-TEXT-STRETCH-SET
                                 "stretch-set" "gboolean" T T)
                                (STRIKETHROUGH
                                 GTK-CELL-RENDERER-TEXT-STRIKETHROUGH
                                 "strikethrough" "gboolean" T T)
                                (STRIKETHROUGH-SET
                                 GTK-CELL-RENDERER-TEXT-STRIKETHROUGH-SET
                                 "strikethrough-set" "gboolean" T T)
                                (STYLE GTK-CELL-RENDERER-TEXT-STYLE "style"
                                 "PangoStyle" T T)
                                (STYLE-SET GTK-CELL-RENDERER-TEXT-STYLE-SET
                                 "style-set" "gboolean" T T)
                                (TEXT GTK-CELL-RENDERER-TEXT-TEXT "text"
                                 "gchararray" T T)
                                (UNDERLINE GTK-CELL-RENDERER-TEXT-UNDERLINE
                                 "underline" "PangoUnderline" T T)
                                (UNDERLINE-SET
                                 GTK-CELL-RENDERER-TEXT-UNDERLINE-SET
                                 "underline-set" "gboolean" T T)
                                (VARIANT GTK-CELL-RENDERER-TEXT-VARIANT
                                 "variant" "PangoVariant" T T)
                                (VARIANT-SET GTK-CELL-RENDERER-TEXT-VARIANT-SET
                                 "variant-set" "gboolean" T T)
                                (WEIGHT GTK-CELL-RENDERER-TEXT-WEIGHT "weight"
                                 "gint" T T)
                                (WEIGHT-SET GTK-CELL-RENDERER-TEXT-WEIGHT-SET
                                 "weight-set" "gboolean" T T)
                                (WIDTH-CHARS GTK-CELL-RENDERER-TEXT-WIDTH-CHARS
                                 "width-chars" "gint" T T)
                                (WRAP-MODE GTK-CELL-RENDERER-TEXT-WRAP-MODE
                                 "wrap-mode" "PangoWrapMode" T T)
                                (WRAP-WIDTH GTK-CELL-RENDERER-TEXT-WRAP-WIDTH
                                 "wrap-width" "gint" T T)))
             (gobject:get-g-type-definition "GtkCellRendererText"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-cell-renderer-text-properties
  (let ((renderer (make-instance 'gtk:cell-renderer-text)))
    (is-false (gtk:cell-renderer-text-align-set renderer))
    (is (eq :left (gtk:cell-renderer-text-alignment renderer)))
    (is-false (gtk:cell-renderer-text-attributes renderer))
    ;; background is not readable
    (signals (error) (gtk:cell-renderer-text-background renderer))
    (is (string= "red"
                 (setf (gtk:cell-renderer-text-background renderer) "red")))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1)
                        (gtk:cell-renderer-text-background-rgba renderer)))
    (is-true (gtk:cell-renderer-text-background-set renderer))
    (is-false (gtk:cell-renderer-text-editable renderer))
    (is-false (gtk:cell-renderer-text-editable-set renderer))
    (is (eq :none (gtk:cell-renderer-text-ellipsize renderer)))
    (is-false (gtk:cell-renderer-text-ellipsize-set renderer))
    (is-false (gtk:cell-renderer-text-family renderer))
    (is-false (gtk:cell-renderer-text-family-set renderer))
    (is (string= "Normal" (gtk:cell-renderer-text-font renderer)))
    (is (typep (gtk:cell-renderer-text-font-desc renderer)
               'pango:font-description))
    ;; foreground is not readable
    (signals (error) (gtk:cell-renderer-text-foreground renderer))
    (is (string= "red"
                 (setf (gtk:cell-renderer-text-foreground renderer) "red")))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1 :green 0 :blue 0 :alpha 1)
                        (gtk:cell-renderer-text-foreground-rgba renderer)))
    (is-true (gtk:cell-renderer-text-foreground-set renderer))
    (is-false (gtk:cell-renderer-text-language renderer))
    (is-false (gtk:cell-renderer-text-language-set renderer))
    ;; markup is not readable
    (signals (error) (gtk:cell-renderer-text-markup renderer))
    (is (= -1 (gtk:cell-renderer-text-max-width-chars renderer)))
    (is-false (gtk:cell-renderer-text-placeholder-text renderer))
    (is (= 0 (gtk:cell-renderer-text-rise renderer)))
    (is-false (gtk:cell-renderer-text-rise-set renderer))
    (is (= 1.0d0 (gtk:cell-renderer-text-scale renderer)))
    (is-false (gtk:cell-renderer-text-scale-set renderer))
    (is-false (gtk:cell-renderer-text-single-paragraph-mode renderer))
    (is (= 0 (gtk:cell-renderer-text-size renderer)))
    (is (= 0.0d0 (gtk:cell-renderer-text-size-points renderer)))
    (is-false (gtk:cell-renderer-text-size-set renderer))
    (is (eq :normal (gtk:cell-renderer-text-stretch renderer)))
    (is-false (gtk:cell-renderer-text-stretch-set renderer))
    (is-false (gtk:cell-renderer-text-strikethrough renderer))
    (is-false (gtk:cell-renderer-text-strikethrough-set renderer))
    (is (eq :normal (gtk:cell-renderer-text-style renderer)))
    (is-false (gtk:cell-renderer-text-style-set renderer))
    (is-false (gtk:cell-renderer-text-text renderer))
    (is (eq :none (gtk:cell-renderer-text-underline renderer)))
    (is-false (gtk:cell-renderer-text-underline-set renderer))
    (is (eq :normal (gtk:cell-renderer-text-variant renderer)))
    (is-false (gtk:cell-renderer-text-variant-set renderer))
    (is (= 400 (gtk:cell-renderer-text-weight renderer)))
    (is-false (gtk:cell-renderer-text-weight-set renderer))
    (is (= -1 (gtk:cell-renderer-text-width-chars renderer)))
    (is (eq :char (gtk:cell-renderer-text-wrap-mode renderer)))
    (is (= -1 (gtk:cell-renderer-text-wrap-width renderer)))))

;;; --- Signals ----------------------------------------------------------------

;;;     edited

;;; --- Functions --------------------------------------------------------------

;;;     gtk_cell_renderer_text_new

(test gtk-cell-renderer-text-new
  (is (typep (gtk:cell-renderer-text-new) 'gtk:cell-renderer-text)))

;;;     gtk_cell_renderer_text_set_fixed_height_from_font

;;; 2024-2-21
