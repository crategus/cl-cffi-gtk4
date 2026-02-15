(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-deprecated)
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
             (glib-test:list-children "GtkColorChooserDialog")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkColorChooser")
             (glib-test:list-interfaces "GtkColorChooserDialog")))
  ;; Check properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (glib-test:list-properties "GtkColorChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColorChooserDialog")))
  ;; Check CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColorChooserDialog"
                                      GTK:COLOR-CHOOSER-DIALOG
                      (:SUPERCLASS GTK:DIALOG
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkColorChooser"
                        "GtkConstraintTarget" "GtkNative" "GtkRoot"
                        "GtkShortcutManager")
                       :TYPE-INITIALIZER "gtk_color_chooser_dialog_get_type")
                      ((SHOW-EDITOR COLOR-CHOOSER-DIALOG-SHOW-EDITOR
                        "show-editor" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkColorChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

(test gtk-color-chooser-dialog-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (is (typep (setf dialog
                       (make-instance 'gtk:color-chooser-dialog))
                 'gtk:color-chooser-dialog))
      (is-false (gtk:color-chooser-dialog-show-editor dialog))
      (is-false (gtk:window-destroy dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_dialog_new

(test gtk-color-chooser-dialog-new
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (is (typep (setf dialog
                       (gtk:color-chooser-dialog-new "title" nil))
                 'gtk:color-chooser-dialog))
      (is-false (gtk:window-destroy dialog)))))

;;; 2024-12-24
