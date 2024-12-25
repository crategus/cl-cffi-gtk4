(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-deprecated)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test gtk-file-chooser-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Check type
    (is (g:type-is-object "GtkFileChooserDialog"))
    ;; Check registered name
    (is (eq 'gtk:file-chooser-dialog
            (glib:symbol-for-gtype "GtkFileChooserDialog")))
    ;; Check type initializer
    (is (eq (g:gtype "GtkFileChooserDialog")
            (g:gtype (cffi:foreign-funcall "gtk_file_chooser_dialog_get_type"
                                           :size))))
    ;; Check parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkFileChooserDialog")))
    ;; Check children
    (is (equal '()
               (glib-test:list-children "GtkFileChooserDialog")))
    ;; Check interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot" "GtkFileChooser")
               (glib-test:list-interfaces "GtkFileChooserDialog")))
    ;; Check properties
    (is (equal '("action" "create-folders" "filter" "filters" "select-multiple"
                 "shortcut-folders")
               (glib-test:list-properties "GtkFileChooserDialog")))
    ;; Check signals
    (is (equal '()
               (glib-test:list-signals "GtkFileChooserDialog")))
    ;; Check CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkFileChooserDialog")))
    ;; Check accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkFileChooserDialog")))
    ;; Check class definition
    (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFileChooserDialog"
                                        GTK:FILE-CHOOSER-DIALOG
                         (:SUPERCLASS GTK:DIALOG
                          :EXPORT T
                          :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkFileChooser" "GtkNative" "GtkRoot"
                           "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                         NIL)
               (gobject:get-gtype-definition "GtkFileChooserDialog")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_dialog_new

;;; 2024-9-20
