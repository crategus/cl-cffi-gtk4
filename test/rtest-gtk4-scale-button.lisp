(in-package :gtk-test)

(def-suite gtk-scale-button :in gtk-suite)
(in-suite gtk-scale-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScaleButton

(test scale-button-class
  ;; Type check
  (is (g:type-is-object "GtkScaleButton"))
  ;; Check the registered name
  (is (eq 'gtk:scale-button
          (gobject:symbol-for-gtype "GtkScaleButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScaleButton")
          (g:gtype (foreign-funcall "gtk_scale_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScaleButton")))
  ;; Check the children
  (is (equal '("GtkVolumeButton")
             (list-children "GtkScaleButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkScaleButton")))
  ;; Check the properties
  (is (equal '("adjustment" "icons" "orientation" "value")
             (list-properties "GtkScaleButton")))
  ;; Check the signals
  (is (equal '("popdown" "popup" "value-changed")
             (list-signals "GtkScaleButton")))
  ;; CSS information
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkScaleButton")))
  (is (string=
"scalebutton.scale:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:scale-button))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkScaleButton" GTK-SCALE-BUTTON
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_scale_button_get_type")
                       ((ADJUSTMENT GTK-SCALE-BUTTON-ADJUSTMENT "adjustment"
                         "GtkAdjustment" T T)
                        (ICONS GTK-SCALE-BUTTON-ICONS "icons" "GStrv" T T)
                        (VALUE GTK-SCALE-BUTTON-VALUE "value" "gdouble" T T)))
             (gobject:get-g-type-definition "GtkScaleButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment
;;;     icons
;;;     value

;;; --- Signals ----------------------------------------------------------------

;;;     popdown
;;;     popup
;;;     value-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scale_button_new
;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button

;;; 2022-11-11
