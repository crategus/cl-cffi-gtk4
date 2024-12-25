(in-package :gtk-test)

(def-suite gtk-font-chooser-widget :in gtk-deprecated)
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
             (glib-test:list-children "GtkFontChooserWidget")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFontChooser")
             (glib-test:list-interfaces "GtkFontChooserWidget")))
  ;; Check properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry" "tweak-action")
             (glib-test:list-properties "GtkFontChooserWidget")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFontChooserWidget")))
  ;; Check CSS name
  (is (string= "fontchooser"
               (gtk:widget-class-css-name "GtkFontChooserWidget")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkFontChooserWidget")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontChooserWidget"
                                      GTK:FONT-CHOOSER-WIDGET
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkFontChooser")
                       :TYPE-INITIALIZER "gtk_font_chooser_widget_get_type")
                      ((TWEAK-ACTION FONT-CHOOSER-WIDGET-TWEAK-ACTION
                        "tweak-action" "GAction" T NIL)))
             (gobject:get-gtype-definition "GtkFontChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-font-chooser-widget-properties
  (when *first-run-testsuite*
    (let ((gtk-init:*gtk-warn-deprecated* nil))
      (glib-test:with-check-memory (widget :strong 1)
        (setf widget (make-instance 'gtk:font-chooser-widget))
        (is (typep (gtk:font-chooser-widget-tweak-action widget) 'g:action))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_widget_new

(test gtk-font-chooser-widget-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory ()
      (is (typep (gtk:font-chooser-widget-new) 'gtk:font-chooser-widget)))))

;;; 2024-12-24
