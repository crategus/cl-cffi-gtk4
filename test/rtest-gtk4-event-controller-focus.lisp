(in-package :gtk-test)

(def-suite gtk-event-controller-focus :in gtk-suite)
(in-suite gtk-event-controller-focus)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerFocus

(test event-controller-focus-class
  ;; Type check
  (is (g:type-is-object "GObject"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller-focus
          (glib:symbol-for-gtype "GtkEventControllerFocus")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventControllerFocus")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_focus_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerFocus")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventControllerFocus")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventControllerFocus")))
  ;; Check the properties
  (is (equal '("contains-focus" "is-focus")
             (list-properties "GtkEventControllerFocus")))
  ;; Check the signals
  (is (equal '("enter" "leave")
             (list-signals "GtkEventControllerFocus")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventControllerFocus"
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

;;; --- 2023-5-29 --------------------------------------------------------------
