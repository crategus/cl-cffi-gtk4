(in-package :gtk-test)

(def-suite gtk-event-controller-motion :in gtk-suite)
(in-suite gtk-event-controller-motion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerMotion

(test gtk-event-controller-motion-class
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
             (glib-test:list-children "GtkEventControllerMotion")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventControllerMotion")))
  ;; Check properties
  (is (equal '("contains-pointer" "is-pointer")
             (glib-test:list-properties "GtkEventControllerMotion")))
  ;; Check signals
  (is (equal '("enter" "leave" "motion")
             (glib-test:list-signals "GtkEventControllerMotion")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventControllerMotion"
                                      GTK:EVENT-CONTROLLER-MOTION
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER
                        "gtk_event_controller_motion_get_type")
                       ((CONTAINS-POINTER
                         EVENT-CONTROLLER-MOTION-CONTAINS-POINTER
                         "contains-pointer" "gboolean" T NIL)
                        (IS-POINTER EVENT-CONTROLLER-MOTION-IS-POINTER
                         "is-pointer" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkEventControllerMotion"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-controller-motion-properties
  (let ((controller (make-instance 'gtk:event-controller-motion)))
    (is-false (gtk:event-controller-motion-contains-pointer controller))
    (is-false (gtk:event-controller-motion-is-pointer controller))))

;;; --- Signals ----------------------------------------------------------------

;;;     enter

(test gtk-event-controller-motion-enter-signal
  (let* ((name "enter")
         (gtype (g:gtype "GtkEventControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     leave

(test gtk-event-controller-motion-leave-signal
  (let* ((name "leave")
         (gtype (g:gtype "GtkEventControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     motion

(test gtk-event-controller-motion-motion-signal
  (let* ((name "motion")
         (gtype (g:gtype "GtkEventControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_motion_new

(test gtk-event-controller-motion-new
  (typep (gtk:event-controller-motion-new) 'gtk:event-controller-motion))

;;; 2024-9-20
