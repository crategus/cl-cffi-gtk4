(in-package :gtk-test)

(def-suite gtk-font-dialog-button :in gtk-suite)
(in-suite gtk-font-dialog-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontLevel

(test gtk-font-level
  ;; Check the type
  (is (g:type-is-enum "GtkFontLevel"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontLevel")
          (g:gtype (cffi:foreign-funcall "gtk_font_level_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:font-level
          (glib:symbol-for-gtype "GtkFontLevel")))
  ;; Check the names
  (is (equal '("GTK_FONT_LEVEL_FAMILY" "GTK_FONT_LEVEL_FACE"
               "GTK_FONT_LEVEL_FONT" "GTK_FONT_LEVEL_FEATURES")
             (list-enum-item-name "GtkFontLevel")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkFontLevel")))
  ;; Check the nick names
  (is (equal '("family" "face" "font" "features")
             (list-enum-item-nick "GtkFontLevel")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkFontLevel" GTK-FONT-LEVEL
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_font_level_get_type")
                                     (:FAMILY 0)
                                     (:FACE 1)
                                     (:FONT 2)
                                     (:FEATURES 3))
             (gobject:get-g-type-definition "GtkFontLevel"))))

;;;     GtkFontDialogButton

(test gtk-font-dialog-button-class
  ;; Type check
  (is (g:type-is-object "GtkFontDialogButton"))
  ;; Check the registered name
  (is (eq 'gtk:font-dialog-button
          (glib:symbol-for-gtype "GtkFontDialogButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontDialogButton")
          (g:gtype (cffi:foreign-funcall "gtk_font_dialog_button_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFontDialogButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontDialogButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkFontDialogButton")))
  ;; Check the properties
  (is (equal '("dialog" "font-desc" "font-features" "language" "level"
               "use-font" "use-size")
             (list-properties "GtkFontDialogButton")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontDialogButton")))
  ;; CSS name
  (is (string= "fontbutton"
               (gtk:widget-class-css-name "GtkFontDialogButton")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:font-dialog-button))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkFontDialogButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontDialogButton"
                                             GTK-FONT-DIALOG-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_font_dialog_button_get_type")
                               ((DIALOG GTK-FONT-DIALOG-BUTTON-DIALOG "dialog"
                                 "GtkFontDialog" T T)
                                (FONT-DESC GTK-FONT-DIALOG-BUTTON-FONT-DESC
                                 "font-desc" "PangoFontDescription" T T)
                                (FONT-FEATURES
                                 GTK-FONT-DIALOG-BUTTON-FONT-FEATURES
                                 "font-features" "gchararray" T T)
                                (LANGUAGE GTK-FONT-DIALOG-BUTTON-LANGUAGE
                                 "language" "PangoLanguage" T T)
                                (LEVEL GTK-FONT-DIALOG-BUTTON-LEVEL "level"
                                 "GtkFontLevel" T T)
                                (USE-FONT GTK-FONT-DIALOG-BUTTON-USE-FONT
                                 "use-font" "gboolean" T T)
                                (USE-SIZE GTK-FONT-DIALOG-BUTTON-USE-SIZE
                                 "use-size" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkFontDialogButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     dialog
;;;     font-desc
;;;     font-features
;;;     language
;;;     level
;;;     use-font
;;;     use-size

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_dialog_button_new

;;; --- 2023-10-19 -------------------------------------------------------------
