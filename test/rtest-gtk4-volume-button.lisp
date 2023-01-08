(in-package :gtk-test)

(def-suite gtk-volume-button :in gtk-suite)
(in-suite gtk-volume-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkVolumeButton

(test volume-button-class
  ;; Type check
  (is (g:type-is-object "GtkVolumeButton"))
  ;; Check the registered name
  (is (eq 'gtk:volume-button
          (gobject:symbol-for-gtype "GtkVolumeButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkVolumeButton")
          (g:gtype (foreign-funcall "gtk_volume_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkScaleButton")
          (g:type-parent "GtkVolumeButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkVolumeButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkVolumeButton")))
  ;; Check the properties
  (is (equal '("use-symbolic")
             (list-properties "GtkVolumeButton")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkVolumeButton")))
  ;; CSS information
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkVolumeButton")))
  (is (string=
"scalebutton.scale:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:volume-button))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkVolumeButton" GTK-VOLUME-BUTTON
                       (:SUPERCLASS GTK-SCALE-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_volume_button_get_type")
                       ((USE-SYMBOLIC GTK-VOLUME-BUTTON-USE-SYMBOLIC
                         "use-symbolic" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkVolumeButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     use-symbolic

;;; --- Functions --------------------------------------------------------------

;;;     gtk_volume_button_new

;;; 2022-11-11
