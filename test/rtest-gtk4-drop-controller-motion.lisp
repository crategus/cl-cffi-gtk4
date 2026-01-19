(in-package :gtk-test)

(def-suite gtk-drop-controller-motion :in gtk-drag-and-drop)
(in-suite gtk-drop-controller-motion)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDropControllerMotion

(test gtk-drop-controller-motion-class
  ;; Check type
  (is (g:type-is-object "GtkDropControllerMotion"))
  ;; Check registered name
  (is (eq 'gtk:drop-controller-motion
          (glib:symbol-for-gtype "GtkDropControllerMotion")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDropControllerMotion")
          (g:gtype (cffi:foreign-funcall "gtk_drop_controller_motion_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkDropControllerMotion")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDropControllerMotion")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkDropControllerMotion")))
  ;; Check properties
  (is (equal '("contains-pointer" "drop" "is-pointer")
             (glib-test:list-properties "GtkDropControllerMotion")))
  ;; Check signals
  (is (equal '("enter" "leave" "motion")
             (glib-test:list-signals "GtkDropControllerMotion")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDropControllerMotion"
                                      GTK:DROP-CONTROLLER-MOTION
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_drop_controller_motion_get_type")
                       ((CONTAINS-POINTER
                         DROP-CONTROLLER-MOTION-CONTAINS-POINTER
                         "contains-pointer" "gboolean" T NIL)
                        (DROP DROP-CONTROLLER-MOTION-DROP "drop" "GdkDrop" T
                         NIL)
                        (IS-POINTER DROP-CONTROLLER-MOTION-IS-POINTER
                         "is-pointer" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkDropControllerMotion"))))

;;; --- Signals ----------------------------------------------------------------

;;;     enter

(test gtk-drop-controller-motion-enter-signal
  (let* ((name "enter")
         (gtype (g:gtype "GtkDropControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     leave

(test gtk-drop-controller-motion-leave-signal
  (let* ((name "leave")
         (gtype (g:gtype "GtkDropControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     motion

(test gtk-drop-controller-motion-move-signal
  (let* ((name "motion")
         (gtype (g:gtype "GtkDropControllerMotion"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     contains-pointer
;;;     drop
;;;     is-pointer

(test gtk-drop-controller-motion-properties
  (glib-test:with-check-memory (controller)
    (is (typep (setf controller
                     (make-instance 'gtk:drop-controller-motion))
               'gtk:drop-controller-motion))
    (is-false (gtk:drop-controller-motion-contains-pointer controller))
    (is-false (gtk:drop-controller-motion-drop controller))
    (is-false (gtk:drop-controller-motion-is-pointer controller))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_controller_motion_new

(test gtk-drop-controller-motion-new
  (glib-test:with-check-memory (controller)
    (is (typep (setf controller
                     (gtk:drop-controller-motion-new))
               'gtk:drop-controller-motion))))

;;; 2026-01-17
