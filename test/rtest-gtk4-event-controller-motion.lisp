(in-package :gtk-test)

(def-suite gtk-event-controller-motion :in gtk-suite)
(in-suite gtk-event-controller-motion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerMotion

(test event-controller-motion-class
  ;; Check type
  (is (g:type-is-object "GtkEventControllerMotion"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-motion
          (glib:symbol-for-gtype "GtkEventControllerMotion")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerMotion")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_motion_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerMotion")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkEventControllerMotion")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventControllerMotion")))
  ;; Check properties
  (is (equal '("contains-pointer" "is-pointer")
             (gtk-test:list-properties "GtkEventControllerMotion")))
  ;; Check signals
  (is (equal '("enter" "leave" "motion")
             (gtk-test:list-signals "GtkEventControllerMotion")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventControllerMotion"
                                     GTK-EVENT-CONTROLLER-MOTION
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_event_controller_motion_get_type")
                       ((CONTAINS-POINTER
                         GTK-EVENT-CONTROLLER-MOTION-CONTAINS-POINTER
                         "contains-pointer" "gboolean" T NIL)
                        (IS-POINTER GTK-EVENT-CONTROLLER-MOTION-IS-POINTER
                         "is-pointer" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkEventControllerMotion"))))

;;; --- Properties -------------------------------------------------------------

;;;     contains-pointer
;;;     is-pointer

;;; --- Signals ----------------------------------------------------------------

;;;     enter
;;;     leave
;;;     motion

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_motion_new

;;; 2024-7-4
