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
;;;     leave
;;;     move

;;; --- Properties -------------------------------------------------------------

;;;     contains-pointer
;;;     drop
;;;     is-pointer

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drop_controller_motion_new
;;;     gtk_drop_controller_motion_contains_pointer
;;;     gtk_drop_controller_motion_is_pointer
;;;     gtk_drop_controller_motion_get_drop

;;; 2024-11-2
