(in-package :gtk-test)

(def-suite gtk-color-chooser-widget :in gtk-deprecated)
(in-suite gtk-color-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorChooserWidget

(test gtk-color-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkColorChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:color-chooser-widget
          (glib:symbol-for-gtype "GtkColorChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_color_chooser_widget_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkColorChooserWidget")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkColorChooserWidget")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkColorChooser")
             (glib-test:list-interfaces "GtkColorChooserWidget")))
  ;; Check properties
  (is (equal '("rgba" "show-editor" "use-alpha")
             (glib-test:list-properties "GtkColorChooserWidget")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkColorChooserWidget")))
  ;; Check CSS name
  (is (string= "colorchooser"
               (gtk:widget-class-css-name "GtkColorChooserWidget")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkColorChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkColorChooserWidget"
                                      GTK:COLOR-CHOOSER-WIDGET
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkColorChooser"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_color_chooser_widget_get_type")
                      ((SHOW-EDITOR COLOR-CHOOSER-WIDGET-SHOW-EDITOR
                        "show-editor" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkColorChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-color-chooser-widget-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (widget (make-instance 'gtk:color-chooser-widget)))
    (is-false (gtk:color-chooser-widget-show-editor widget))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_chooser_widget_new

(test gtk-color-chooser-widget-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:color-chooser-widget-new) 'gtk:color-chooser-widget))))

;;; 2024-9-20
