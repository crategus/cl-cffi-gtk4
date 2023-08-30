(in-package :gtk-test)

(def-suite gtk-font-chooser-dialog :in gtk-suite)
(in-suite gtk-font-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserDialog

(test gtk-font-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkFontChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser-dialog
          (glib:symbol-for-gtype "GtkFontChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkFontChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontChooserDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkFontChooser")
             (list-interfaces "GtkFontChooserDialog")))
  ;; Check the properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry")
             (list-properties "GtkFontChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontChooserDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkFontChooserDialog")))
  #-windows
  (is (string=
"fontchooser:dir(ltr)
"
               (print-style-context "GtkFontChooserWidget" :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontChooserDialog"
                                     GTK-FONT-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkFontChooser" "GtkNative" "GtkRoot"
                         "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_font_chooser_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFontChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_dialog_new ()

(test gtk-font-chooser-dialog-new
  (is (typep (gtk:font-chooser-dialog-new "title" nil)
             'gtk:font-chooser-dialog)))

;;; --- 2023-8-28 --------------------------------------------------------------
