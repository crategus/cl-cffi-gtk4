(in-package :gtk-test)

(def-suite gtk-event-controller-legacy :in gtk-event-handling)
(in-suite gtk-event-controller-legacy)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerLegacy

(test gtk-event-contoller-legacy-class
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
             (glib-test:list-children "GtkEventControllerLegacy")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventControllerLegacy")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkEventControllerLegacy")))
  ;; Check signals
  (is (equal '("event")
             (glib-test:list-signals "GtkEventControllerLegacy")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventControllerLegacy"
                                      GTK:EVENT-CONTROLLER-LEGACY
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER
                        "gtk_event_controller_legacy_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkEventControllerLegacy"))))

;;; --- Signals ----------------------------------------------------------------

;;;     event

(test gtk-event-controller-legacy-event-signal
  (let* ((name "event")
         (gtype (g:gtype "GtkEventControllerLegacy"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GdkEvent")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_legacy_new

(test gtk-event-controller-legacy-new
  (is (typep (gtk:event-controller-legacy-new) 'gtk:event-controller-legacy)))

;;; 2024-9-20
