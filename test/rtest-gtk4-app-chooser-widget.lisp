(in-package :gtk-test)

(def-suite gtk-app-chooser-widget :in gtk-suite)
(in-suite gtk-app-chooser-widget)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserWidget

(test gtk-app-chooser-widget-class
  ;; Check type
  (is (g:type-is-object "GtkAppChooserWidget"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-widget
          (glib:symbol-for-gtype "GtkAppChooserWidget")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAppChooserWidget")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_widget_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkAppChooserWidget")))
  ;; Check children
  (is (equal '()
             (list-children "GtkAppChooserWidget")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAppChooser")
             (list-interfaces "GtkAppChooserWidget")))
  ;; Check properties
  (is (equal '("content-type" "default-text" "show-all" "show-default"
               "show-fallback" "show-other" "show-recommended")
             (list-properties "GtkAppChooserWidget")))
  ;; Check signals
  (is (equal '("application-activated" "application-selected")
             (list-signals "GtkAppChooserWidget")))
  ;; Check CSS name
  (is (string= "appchooser"
               (gtk:widget-class-css-name "GtkAppChooserWidget")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkAppChooserWidget")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAppChooserWidget"
                                             GTK-APP-CHOOSER-WIDGET
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkAppChooser" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_app_chooser_widget_get_type")
                               ((DEFAULT-TEXT
                                 GTK-APP-CHOOSER-WIDGET-DEFAULT-TEXT
                                 "default-text" "gchararray" T T)
                                (SHOW-ALL GTK-APP-CHOOSER-WIDGET-SHOW-ALL
                                 "show-all" "gboolean" T T)
                                (SHOW-DEFAULT
                                 GTK-APP-CHOOSER-WIDGET-SHOW-DEFAULT
                                 "show-default" "gboolean" T T)
                                (SHOW-FALLBACK
                                 GTK-APP-CHOOSER-WIDGET-SHOW-FALLBACK
                                 "show-fallback" "gboolean" T T)
                                (SHOW-OTHER GTK-APP-CHOOSER-WIDGET-SHOW-OTHER
                                 "show-other" "gboolean" T T)
                                (SHOW-RECOMMENDED
                                 GTK-APP-CHOOSER-WIDGET-SHOW-RECOMMENDED
                                 "show-recommended" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkAppChooserWidget"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-app-chooser-widget-properties
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (let ((widget (make-instance 'gtk:app-chooser-widget)))
      (is-false (gtk:app-chooser-widget-default-text widget))
      (is-false (gtk:app-chooser-widget-show-all widget))
      (is-false (gtk:app-chooser-widget-show-default widget))
      (is-false (gtk:app-chooser-widget-show-fallback widget))
      (is-false (gtk:app-chooser-widget-show-other widget))
      (is-true (gtk:app-chooser-widget-show-recommended widget)))))

;;; --- Signals ----------------------------------------------------------------

;;;     application-activated
;;;     application-selected

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_widget_new

(test gtk-app-chooser-widget-new
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (is (typep (gtk:app-chooser-widget-new "text/plain")
               'gtk:app-chooser-widget))))

;;; 2024-4-26
