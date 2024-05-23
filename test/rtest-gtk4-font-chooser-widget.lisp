(in-package :gtk-test)

(def-suite gtk-font-chooser-widget :in gtk-suite)
(in-suite gtk-font-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserWidget

(test gtk-font-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkFontChooserWidget"))
  ;; Check registered name
  (is (eq 'gtk:font-chooser-widget
          (glib:symbol-for-gtype "GtkFontChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_widget_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFontChooserWidget")))
  ;; Check children
  (is (equal '()
             (list-children "GtkFontChooserWidget")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFontChooser")
             (list-interfaces "GtkFontChooserWidget")))
  ;; Check properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry" "tweak-action")
             (list-properties "GtkFontChooserWidget")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkFontChooserWidget")))
  ;; Check CSS name
  (is (string= "fontchooser"
               (gtk:widget-class-css-name "GtkFontChooserWidget")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkFontChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontChooserWidget"
                                             GTK-FONT-CHOOSER-WIDGET
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkFontChooser")
                                :TYPE-INITIALIZER
                                "gtk_font_chooser_widget_get_type")
                               ((TWEAK-ACTION
                                 GTK-FONT-CHOOSER-WIDGET-TWEAK-ACTION
                                 "tweak-action" "GAction" T NIL)))
             (gobject:get-g-type-definition "GtkFontChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-chooser-widget-properties
  (let* ((gtk-init:*gtk-warn-deprecated* nil)
         (widget (make-instance 'gtk:font-chooser-widget)))
    (is (typep (gtk:font-chooser-widget-tweak-action widget) 'g:action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_widget_new

(test gtk-font-chooser-widget-new
  (let* ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:font-chooser-widget-new) 'gtk:font-chooser-widget))))

;;; 2024-5-22
