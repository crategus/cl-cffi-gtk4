(in-package :gtk-test)

(def-suite gtk-file-chooser-dialog :in gtk-suite)
(in-suite gtk-file-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFileChooserDialog

(test file-chooser-dialog-class
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
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkFileChooser")
             (list-interfaces "GtkFileChooserDialog")))
  ;; Check the properties
  (is (equal '("action" "create-folders" "filter" "filters" "select-multiple"
               "shortcut-folders")
             (list-properties "GtkFileChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFileChooserDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkFileChooserDialog")))
  #-windows
  (is (string=
"[window.background.csd.dialog:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:file-chooser-dialog))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFileChooserDialog"
                                     GTK-FILE-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkFileChooser" "GtkNative" "GtkRoot"
                         "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_file_chooser_dialog_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFileChooserDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_file_chooser_dialog_new

;;; --- 2023-5-29 --------------------------------------------------------------
