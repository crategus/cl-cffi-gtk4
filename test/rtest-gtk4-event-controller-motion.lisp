(in-package :gtk-test)

(def-suite gtk-event-controller-motion :in gtk-suite)
(in-suite gtk-event-controller-motion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerMotion

(test event-controller-motion-class
  ;; Type check
  (is (g:type-is-object "GtkEventControllerMotion"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller-motion
          (glib:symbol-for-gtype "GtkEventControllerMotion")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventControllerMotion")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_motion_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerMotion")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventControllerMotion")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventControllerMotion")))
  ;; Check the properties
  (is (equal '("contains-pointer" "is-pointer")
             (list-properties "GtkEventControllerMotion")))
  ;; Check the signals
  (is (equal '("enter" "leave" "motion")
             (list-signals "GtkEventControllerMotion")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventControllerMotion"
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

;;; --- 2023-5-29 --------------------------------------------------------------
