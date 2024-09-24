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
             (glib-test:list-flags-item-names "GtkFontChooserLevel")))
  ;; Check values
  (is (equal '(0 1 2 4 8)
             (glib-test:list-flags-item-values "GtkFontChooserLevel")))
  ;; Check nick names
  (is (equal '("family" "style" "size" "variations" "features")
             (glib-test:list-flags-item-nicks "GtkFontChooserLevel")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkFontChooserLevel"
                                     GTK:FONT-CHOOSER-LEVEL
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_font_chooser_level_get_type")
                                     (:FAMILY 0)
                                     (:STYLE 1)
                                     (:SIZE 2)
                                     (:VARIATIONS 4)
                                     (:FEATURES 8))
             (gobject:get-gtype-definition "GtkFontChooserLevel"))))

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
             (glib-test:list-interface-prerequisites "GtkFontChooser")))
  ;; Check interface properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (glib-test:list-interface-properties "GtkFontChooser")))
  ;; Check interface signals
  (is (equal '("font-activated")
             (glib-test:list-signals "GtkFontChooser")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkFontChooser" GTK:FONT-CHOOSER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_font_chooser_get_type")
                       (FONT FONT-CHOOSER-FONT "font" "gchararray" T T)
                       (FONT-DESC FONT-CHOOSER-FONT-DESC
                        "font-desc" "PangoFontDescription" T T)
                       (FONT-FEATURES FONT-CHOOSER-FONT-FEATURES
                        "font-features" "gchararray" T NIL)
                       (LANGUAGE FONT-CHOOSER-LANGUAGE
                        "language" "gchararray" T T)
                       (LEVEL FONT-CHOOSER-LEVEL
                        "level" "GtkFontChooserLevel" T T)
                       (PREVIEW-TEXT FONT-CHOOSER-PREVIEW-TEXT
                        "preview-text" "gchararray" T T)
                       (SHOW-PREVIEW-ENTRY FONT-CHOOSER-SHOW-PREVIEW-ENTRY
                        "show-preview-entry" "gboolean" T T))
             (gobject:get-gtype-definition "GtkFontChooser"))))

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

;;; 2024-9-20
