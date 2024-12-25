(in-package :gtk-test)

(def-suite gtk-app-chooser-dialog :in gtk-deprecated)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test gtk-app-chooser-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check registered name
  (is (eq 'gtk:app-chooser-dialog
          (glib:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkAppChooserDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkAppChooserDialog")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkAppChooser")
             (glib-test:list-interfaces "GtkAppChooserDialog")))
  ;; Check properties
  (is (equal '("content-type" "gfile" "heading")
             (glib-test:list-properties "GtkAppChooserDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkAppChooserDialog")))
  ;; Check CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkAppChooserDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkAppChooserDialog"
                                      GTK:APP-CHOOSER-DIALOG
                      (:SUPERCLASS GTK:DIALOG
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAppChooser" "GtkBuildable"
                        "GtkConstraintTarget" "GtkNative" "GtkRoot"
                        "GtkShortcutManager")
                       :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                      ((GFILE APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T NIL)
                       (HEADING APP-CHOOSER-DIALOG-HEADING
                        "heading" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-dialog-properties
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (dialog)
      (is (typep (setf dialog
                       (make-instance 'gtk:app-chooser-dialog))
                 'gtk:app-chooser-dialog))
      (is-false (gtk:app-chooser-dialog-gfile dialog))
      (is-false (gtk:app-chooser-dialog-heading dialog))
      (is-false (gtk:window-destroy dialog)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget

;;; 2024-12-24
