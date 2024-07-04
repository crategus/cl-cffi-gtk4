(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-suite)
(in-suite gtk-color-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserDialog

(test gtk-color-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkColorChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:color-chooser-dialog
          (glib:symbol-for-gtype "GtkColorChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_dialog_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorChooserDialog")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkColorChooserDialog")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkColorChooser")
             (gtk-test:list-interfaces "GtkColorChooserDialog")))
  ;; Check properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (gtk-test:list-properties "GtkColorChooserDialog")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkColorChooserDialog")))
  ;; Check CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorChooserDialog"
                                     GTK-COLOR-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkColorChooser"
                         "GtkConstraintTarget" "GtkNative" "GtkRoot"
                         "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_color_chooser_dialog_get_type")
                       ((SHOW-EDITOR GTK-COLOR-CHOOSER-DIALOG-SHOW-EDITOR
                         "show-editor" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

(test gtk-color-chooser-dialog-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (dialog (make-instance 'gtk:color-chooser-dialog)))
    (is-false (gtk:color-chooser-dialog-show-editor dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_dialog_new

(test gtk-color-chooser-dialog-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:color-chooser-dialog-new "title" nil)
               'gtk:color-chooser-dialog))))

;;; 2024-5-22
