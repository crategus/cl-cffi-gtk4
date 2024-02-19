(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-suite)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test gtk-file-chooser-dialog-class
  (let ((*gtk-warn-deprecated* nil))
    ;; Type check
    (is (g:type-is-object "GtkFileChooserDialog"))
    ;; Check the registered name
    (is (eq 'gtk:file-chooser-dialog
            (glib:symbol-for-gtype "GtkFileChooserDialog")))
    ;; Check the type initializer
    (is (eq (g:gtype "GtkFileChooserDialog")
            (g:gtype (cffi:foreign-funcall "gtk_file_chooser_dialog_get_type"
                                           :size))))
    ;; Check the parent
    (is (eq (g:gtype "GtkDialog")
            (g:type-parent "GtkFileChooserDialog")))
    ;; Check the children
    (is (equal '()
               (list-children "GtkFileChooserDialog")))
    ;; Check the interfaces
    (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                 "GtkNative" "GtkShortcutManager" "GtkRoot" "GtkFileChooser")
               (list-interfaces "GtkFileChooserDialog")))
    ;; Check the properties
    (is (equal '("action" "create-folders" "filter" "filters" "select-multiple"
                 "shortcut-folders")
               (list-properties "GtkFileChooserDialog")))
    ;; Check the signals
    (is (equal '()
               (list-signals "GtkFileChooserDialog")))
    ;; CSS name
    (is (string= "window"
                 (gtk:widget-class-css-name "GtkFileChooserDialog")))
    ;; CSS classes
    #-windows
    (is (equal '("background" "csd" "dialog" "filechooser")
               (gtk:widget-css-classes
                   (make-instance 'gtk:file-chooser-dialog))))
    #+windows
    (is (equal '("background" "dialog" "filechooser")
               (gtk:widget-css-classes
                   (make-instance 'gtk:file-chooser-dialog))))
    ;; Accessible role
    (is (eq :dialog (gtk:widget-class-accessible-role "GtkFileChooserDialog")))
    ;; Check the class definition
    (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserDialog"
                                       GTK-FILE-CHOOSER-DIALOG
                         (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                          ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                           "GtkFileChooser" "GtkNative" "GtkRoot"
                           "GtkShortcutManager")
                          :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                         NIL)
               (gobject:get-g-type-definition "GtkFileChooserDialog")))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_dialog_new

;;; 2024-1-9
