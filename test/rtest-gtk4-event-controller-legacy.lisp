(in-package :gtk-test)

(def-suite gtk-event-controller-legacy :in gtk-suite)
(in-suite gtk-event-controller-legacy)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerLegacy

(test event-contoller-legacy-class
  ;; Type check
  (is (g:type-is-object "GtkEventControllerLegacy"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller-legacy
          (gobject:symbol-for-gtype "GtkEventControllerLegacy")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventControllerLegacy")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_legacy_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerLegacy")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventControllerLegacy")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventControllerLegacy")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkEventControllerLegacy")))
  ;; Check the signals
  (is (equal '("event")
             (list-signals "GtkEventControllerLegacy")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventControllerLegacy"
                                     GTK-EVENT-CONTROLLER-LEGACY
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_event_controller_legacy_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkEventControllerLegacy"))))

;;; --- Signals ----------------------------------------------------------------

;;;     event

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_legacy_new

;;; --- 2023-3-18 --------------------------------------------------------------
