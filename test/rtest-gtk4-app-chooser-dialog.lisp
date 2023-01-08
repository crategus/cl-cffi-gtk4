(in-package :gtk-test)

(def-suite gtk-adjustment :in gtk-suite)
(in-suite gtk-adjustment)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test app-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:widget
          (gobject:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDialog")
          (g:gtype (foreign-funcall "gtk_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkAppChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserDialog")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkAppChooserDialog")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkAppChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAppChooserDialog")))
  ;; CSS information
  (is (string= ""
               (gtk:widget-class-css-name "GtkAppChooserDialog")))
  (is (string= ""
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:app-chooser-dialog))
                   :none)))
  ;; Check the class definition
  (is (equal '()
             (gobject:get-g-type-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     gfile
;;;     heading

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget

;;; 2ß22-11-10
