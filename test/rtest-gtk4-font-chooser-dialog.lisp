(in-package :gtk-test)

(def-suite gtk-font-chooser-dialog :in gtk-suite)
(in-suite gtk-font-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserDialog

(test gtk-font-chooser-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Check type
    (is (g:type-is-object "GtkFontChooserDialog"))
    ;; Check registered name
    (is (eq 'gtk:font-chooser-dialog
            (glib:symbol-for-gtype "GtkFontChooserDialog")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkFontChooserDialog")
            (g:gtype (cffi:foreign-funcall "gtk_font_chooser_dialog_get_type"
                                           :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkFontChooserDialog")))
    ;; Check children
    (is (equal '()
               (list-children "GtkFontChooserDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot" "GtkFontChooser")
               (list-interfaces "GtkFontChooserDialog")))
    ;; Check properties
    (is (equal '("font" "font-desc" "font-features" "language" "level"
                 "preview-text" "show-preview-entry")
               (list-properties "GtkFontChooserDialog")))
    ;; Check signals
    (is (equal '()
               (list-signals "GtkFontChooserDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkFontChooserDialog")))
    ;; Check accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkFontChooserDialog")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontChooserDialog"
                                       GTK-FONT-CHOOSER-DIALOG
                         (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkFontChooser" "GtkNative" "GtkRoot"
                           "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_font_chooser_dialog_get_type")
                         NIL)
               (gobject:get-g-type-definition "GtkFontChooserDialog")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_dialog_new ()

(test gtk-font-chooser-dialog-new
  (let ((*gtk-warn-deprecated* nil))
    (is (typep (gtk:font-chooser-dialog-new "title" nil)
               'gtk:font-chooser-dialog))))

;;; 2024-5-22
