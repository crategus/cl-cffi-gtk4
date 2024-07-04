(in-package :gtk-test)

(def-suite gtk-event-controller-focus :in gtk-suite)
(in-suite gtk-event-controller-focus)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerFocus

(test event-controller-focus-class
  ;; Check type
  (is (g:type-is-object "GObject"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-focus
          (glib:symbol-for-gtype "GtkEventControllerFocus")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerFocus")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_focus_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerFocus")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkEventControllerFocus")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventControllerFocus")))
  ;; Check properties
  (is (equal '("contains-focus" "is-focus")
             (gtk-test:list-properties "GtkEventControllerFocus")))
  ;; Check signals
  (is (equal '("enter" "leave")
             (gtk-test:list-signals "GtkEventControllerFocus")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventControllerFocus"
                                     GTK-EVENT-CONTROLLER-FOCUS
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_event_controller_focus_get_type")
                       ((CONTAINS-FOCUS
                         GTK-EVENT-CONTROLLER-FOCUS-CONTAINS-FOCUS
                         "contains-focus" "gboolean" T NIL)
                        (IS-FOCUS GTK-EVENT-CONTROLLER-FOCUS-IS-FOCUS
                         "is-focus" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GtkEventControllerFocus"))))

;;; --- Properties -------------------------------------------------------------

;;;     contains-focus
;;;     is-focus

;;; --- Signals ----------------------------------------------------------------

;;;     enter
;;;     leave

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_focus_new

;;; 2024-7-4
