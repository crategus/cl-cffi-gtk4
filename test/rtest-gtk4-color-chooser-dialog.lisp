(in-package :gtk-test)

(def-suite gtk-color-chooser-dialog :in gtk-suite)
(in-suite gtk-color-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserDialog

(test color-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkColorChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser-dialog
          (glib:symbol-for-gtype "GtkColorChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkColorChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorChooserDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkColorChooser")
             (list-interfaces "GtkColorChooserDialog")))
  ;; Check the properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (list-properties "GtkColorChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorChooserDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkColorChooserDialog")))
  #-windows
  (is (string=
"[window.background.csd.dialog:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:color-chooser-dialog))
                   :none)))
  ;; Check the class definition
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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_dialog_new

;;; --- 2023-5-29 --------------------------------------------------------------
