(in-package :gtk-test)

(def-suite gtk-font-chooser-widget :in gtk-suite)
(in-suite gtk-font-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontChooserWidget

(test gtk-font-chooser-widget-class
  ;; Type check
  (is (g:type-is-object "GtkFontChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:font-chooser-widget
          (glib:symbol-for-gtype "GtkFontChooserWidget")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_font_chooser_widget_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFontChooserWidget")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontChooserWidget")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkFontChooser")
             (list-interfaces "GtkFontChooserWidget")))
  ;; Check the properties
  (is (equal '("font" "font-desc" "font-features" "language" "level"
               "preview-text" "show-preview-entry" "tweak-action")
             (list-properties "GtkFontChooserWidget")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontChooserWidget")))
  ;; CSS name
  (is (string= "fontchooser"
               (gtk:widget-class-css-name "GtkFontChooserWidget")))
  ;; CSS style context
  (is (string=
"fontchooser:dir(ltr)
"
               (print-style-context "GtkFontChooserWidget" :none)))
  ;; Check the class definition
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

;;;     tweak-action

(test gtk-font-chooser-widget-properties
  (let ((widget (make-instance 'gtk:font-chooser-widget)))
    (is (typep (gtk:font-chooser-widget-tweak-action widget) 'g:action))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_chooser_widget_new

(test gtk-font-chooser-widget-new
  (is (typep (gtk:font-chooser-widget-new) 'gtk:font-chooser-widget)))

;;; --- 2023-8-28 --------------------------------------------------------------
