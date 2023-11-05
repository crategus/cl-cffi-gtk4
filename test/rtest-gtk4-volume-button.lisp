(in-package :gtk-test)

(def-suite gtk-volume-button :in gtk-suite)
(in-suite gtk-volume-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkVolumeButton

(test gtk-volume-button-class
  ;; Type check
  (is (g:type-is-object "GtkVolumeButton"))
  ;; Check the registered name
  (is (eq 'gtk:volume-button
          (glib:symbol-for-gtype "GtkVolumeButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkVolumeButton")
          (g:gtype (cffi:foreign-funcall "gtk_volume_button_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkScaleButton")
          (g:type-parent "GtkVolumeButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkVolumeButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (list-interfaces "GtkVolumeButton")))
  ;; Check the properties
  (is (equal '("use-symbolic")
             (list-properties "GtkVolumeButton")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkVolumeButton")))
  ;; CSS name
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkVolumeButton")))
  ;; CSS classes
  (is (equal '("scale")
             (gtk:widget-css-classes (make-instance 'gtk:volume-button))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkVolumeButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkVolumeButton" GTK-VOLUME-BUTTON
                       (:SUPERCLASS GTK-SCALE-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_volume_button_get_type")
                       ((USE-SYMBOLIC GTK-VOLUME-BUTTON-USE-SYMBOLIC
                         "use-symbolic" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkVolumeButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-volume-button-properties
  (let ((button (make-instance 'gtk:volume-button)))
    (is-true (setf (gtk:volume-button-use-symbolic button) t))
    (is-true (gtk:volume-button-use-symbolic button))
    (is-false (setf (gtk:volume-button-use-symbolic button) nil))
    (is-false (gtk:volume-button-use-symbolic button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_volume_button_new

(test gtk-volume-button-new
  (let ((button (gtk:volume-button-new)))
    (is (typep button 'gtk:volume-button))
    (is (=  0.0d0 (gtk:adjustment-lower (gtk:scale-button-adjustment button))))
    (is (=  1.0d0 (gtk:adjustment-upper (gtk:scale-button-adjustment button))))
    (is (= 0.02d0 (gtk:adjustment-step-increment
                      (gtk:scale-button-adjustment button))))))

;;; --- 2023-11-4 --------------------------------------------------------------
