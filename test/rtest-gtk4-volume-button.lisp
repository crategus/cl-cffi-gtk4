(in-package :gtk-test)

(def-suite gtk-volume-button :in gtk-deprecated)
(in-suite gtk-volume-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkVolumeButton

(test gtk-volume-button-class
  ;; Check type
  (is (g:type-is-object "GtkVolumeButton"))
  ;; Check registered name
  (is (eq 'gtk:volume-button
          (glib:symbol-for-gtype "GtkVolumeButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkVolumeButton")
          (g:gtype (cffi:foreign-funcall "gtk_volume_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkScaleButton")
          (g:type-parent "GtkVolumeButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkVolumeButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAccessibleRange" "GtkOrientable")
             (glib-test:list-interfaces "GtkVolumeButton")))
  ;; Check properties
  (is (equal '("use-symbolic")
             (glib-test:list-properties "GtkVolumeButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkVolumeButton")))
  ;; Check CSS name
  (is (string= "scalebutton"
               (gtk:widget-class-css-name "GtkVolumeButton")))
  ;; Chekc accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkVolumeButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkVolumeButton" GTK:VOLUME-BUTTON
                      (:SUPERCLASS GTK:SCALE-BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                        "GtkConstraintTarget" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_volume_button_get_type")
                      ((USE-SYMBOLIC VOLUME-BUTTON-USE-SYMBOLIC
                        "use-symbolic" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkVolumeButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-volume-button-properties
  (glib-test:with-check-memory (button)
    (setf button (make-instance 'gtk:volume-button))
    (is-true (setf (gtk:volume-button-use-symbolic button) t))
    (is-true (gtk:volume-button-use-symbolic button))
    (is-false (setf (gtk:volume-button-use-symbolic button) nil))
    (is-false (gtk:volume-button-use-symbolic button))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_volume_button_new

(test gtk-volume-button-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory (button :strong 1)
      (setf button (gtk:volume-button-new))
      (is (typep button 'gtk:volume-button))
      (is (=  0.0d0 (gtk:adjustment-lower (gtk:scale-button-adjustment button))))
      (is (=  1.0d0 (gtk:adjustment-upper (gtk:scale-button-adjustment button))))
      (is (= 0.02d0 (gtk:adjustment-step-increment
                        (gtk:scale-button-adjustment button)))))))

;;; 2024-12-24
