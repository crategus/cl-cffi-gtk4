(in-package :gtk-test)

(def-suite gtk-app-chooser-dialog :in gtk-suite)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test app-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-dialog
          (gobject:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkAppChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkAppChooser")
             (list-interfaces "GtkAppChooserDialog")))
  ;; Check the properties
  (is (equal '("content-type" "gfile" "heading")
             (list-properties "GtkAppChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAppChooserDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkAppChooserDialog")))
  (is (string=
"[window.background.csd.dialog:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:app-chooser-dialog))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkAppChooserDialog" GTK-APP-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAppChooser" "GtkBuildable"
                         "GtkConstraintTarget" "GtkNative" "GtkRoot"
                         "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                       ((GFILE GTK-APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T
                         NIL)
                        (HEADING GTK-APP-CHOOSER-DIALOG-HEADING "heading"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     gfile
;;;     heading

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget

;;; --- 2023-3-18 --------------------------------------------------------------
