(in-package :gtk-test)

(def-suite gtk-color-chooser-widget :in gtk-suite)
(in-suite gtk-color-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserWidget

(test gtk-color-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkColorChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:color-chooser-widget
          (glib:symbol-for-gtype "GtkColorChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkColorChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColorChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkColorChooserWidget")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkColorChooser")
             (list-interfaces "GtkColorChooserWidget")))
  ;; Check the properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (list-properties "GtkColorChooserWidget")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkColorChooserWidget")))
  ;; CSS name
  (is (string= "colorchooser"
               (gtk:widget-class-css-name "GtkColorChooserWidget")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:color-chooser-widget))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkColorChooserWidget")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorChooserWidget"
                               GTK-COLOR-CHOOSER-WIDGET
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkColorChooser" "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_color_chooser_widget_get_type")
                               ((SHOW-EDITOR
                                 GTK-COLOR-CHOOSER-WIDGET-SHOW-EDITOR
                                 "show-editor" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

;;;     show-editor

(test gtk-color-chooser-widget-properties
  (let ((widget (make-instance 'gtk:color-chooser-widget)))
    (is-false (gtk:color-chooser-widget-show-editor widget))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_widget_new

(test gtk-color-chooser-widget-new
  (is (typep (gtk:color-chooser-widget-new) 'gtk:color-chooser-widget)))

;;; 2024-2-22
