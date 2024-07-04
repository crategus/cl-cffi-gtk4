(in-package :gtk-test)

(def-suite gtk-event-controller-legacy :in gtk-suite)
(in-suite gtk-event-controller-legacy)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerLegacy

(test event-contoller-legacy-class
  ;; Check type
  (is (g:type-is-object "GtkEventControllerLegacy"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-legacy
          (glib:symbol-for-gtype "GtkEventControllerLegacy")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerLegacy")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_legacy_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerLegacy")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkEventControllerLegacy")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventControllerLegacy")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkEventControllerLegacy")))
  ;; Check signals
  (is (equal '("event")
             (gtk-test:list-signals "GtkEventControllerLegacy")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventControllerLegacy"
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

;;; 2024-7-4
