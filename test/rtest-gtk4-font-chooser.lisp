(in-package :gtk-test)

(def-suite gtk-font-chooser :in gtk-suite)
(in-suite gtk-font-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserLevel

(test gtk-font-chooser-level
  ;; Check type
  (is (g:type-is-flags "GtkFontChooserLevel"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser-level
          (glib:symbol-for-gtype "GtkFontChooserLevel")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooserLevel")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_level_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_FONT_CHOOSER_LEVEL_FAMILY" "GTK_FONT_CHOOSER_LEVEL_STYLE"
               "GTK_FONT_CHOOSER_LEVEL_SIZE" "GTK_FONT_CHOOSER_LEVEL_VARIATIONS"
               "GTK_FONT_CHOOSER_LEVEL_FEATURES")
             (gtk-test:list-flags-item-name "GtkFontChooserLevel")))
  ;; Check values
  (is (equal '(0 1 2 4 8)
             (gtk-test:list-flags-item-value "GtkFontChooserLevel")))
  ;; Check nick names
  (is (equal '("family" "style" "size" "variations" "features")
             (gtk-test:list-flags-item-nick "GtkFontChooserLevel")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GtkFontChooserLevel"
                                      GTK-FONT-CHOOSER-LEVEL
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "gtk_font_chooser_level_get_type")
                                      (:FAMILY 0)
                                      (:STYLE 1)
                                      (:SIZE 2)
                                      (:VARIATIONS 4)
                                      (:FEATURES 8))
             (gobject:get-g-type-definition "GtkFontChooserLevel"))))

;;;     GtkFontChooser

(test gtk-font-chooser-interface
  ;; Check type
  (is (g:type-is-interface "GtkFontChooser"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser
          (glib:symbol-for-gtype "GtkFontChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooser")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (gtk-test:list-interface-prerequisites "GtkFontChooser")))
  ;; Check interface properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (gtk-test:list-interface-properties "GtkFontChooser")))
  ;; Check interface signals
  (is (equal '("font-activated")
             (gtk-test:list-signals "GtkFontChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkFontChooser" GTK-FONT-CHOOSER
                            (:EXPORT T :TYPE-INITIALIZER
                             "gtk_font_chooser_get_type")
                            (FONT GTK-FONT-CHOOSER-FONT "font" "gchararray" T
                             T)
                            (FONT-DESC GTK-FONT-CHOOSER-FONT-DESC "font-desc"
                             "PangoFontDescription" T T)
                            (FONT-FEATURES GTK-FONT-CHOOSER-FONT-FEATURES
                             "font-features" "gchararray" T NIL)
                            (LANGUAGE GTK-FONT-CHOOSER-LANGUAGE "language"
                             "gchararray" T T)
                            (LEVEL GTK-FONT-CHOOSER-LEVEL "level"
                             "GtkFontChooserLevel" T T)
                            (PREVIEW-TEXT GTK-FONT-CHOOSER-PREVIEW-TEXT
                             "preview-text" "gchararray" T T)
                            (SHOW-PREVIEW-ENTRY
                             GTK-FONT-CHOOSER-SHOW-PREVIEW-ENTRY
                             "show-preview-entry" "gboolean" T T))
             (gobject:get-g-type-definition "GtkFontChooser"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-chooser-properties
  (let ((*gtk-warn-deprecated* nil))
    (let ((chooser (make-instance 'gtk:font-chooser-dialog)))
      (is-false (gtk:font-chooser-font chooser))
      (is-false (gtk:font-chooser-font-desc chooser))
      (is (string= "" (gtk:font-chooser-font-features chooser)))
      (is (string= "de-de" (gtk:font-chooser-language chooser)))
      (is (equal '(:style :size) (gtk:font-chooser-level chooser)))
      #-windows
      (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                   (gtk:font-chooser-preview-text chooser)))
      #+windows
      (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                   (gtk:font-chooser-preview-text chooser)))
      (is-true (gtk:font-chooser-show-preview-entry chooser)))))

;;; --- Signals ----------------------------------------------------------------

;;;     font-activated

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_get_font_family
;;;     gtk_font_chooser_get_font_face
;;;     gtk_font_chooser_get_font_size
;;;     GtkFontFilterFunc
;;;     gtk_font_chooser_set_filter_func
;;;     gtk_font_chooser_set_font_map
;;;     gtk_font_chooser_get_font_map

;;; 2024-5-22
