(in-package :gtk-test)

(def-suite gtk-event-controller-key :in gtk-suite)
(in-suite gtk-event-controller-key)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerKey

(test event-controller-key-class
  ;; Check type
  (is (g:type-is-object "GtkEventControllerKey"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-key
          (glib:symbol-for-gtype "GtkEventControllerKey")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerKey")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_key_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerKey")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkEventControllerKey")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventControllerKey")))
  ;; Check properties
  (is (equal '()
             (gtk-test:list-properties "GtkEventControllerKey")))
  ;; Check signals
  (is (equal '("im-update" "key-pressed" "key-released" "modifiers")
             (gtk-test:list-signals "GtkEventControllerKey")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventControllerKey"
                                     GTK-EVENT-CONTROLLER-KEY
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_event_controller_key_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkEventControllerKey"))))

;;; --- Signals ----------------------------------------------------------------

;;;     im-update
;;;     key-pressed
;;;     key-released
;;;     modifiers

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_key_new
;;;     gtk_event_controller_key_set_im_context
;;;     gtk_event_controller_key_get_im_context
;;;     gtk_event_controller_key_forward
;;;     gtk_event_controller_key_get_group

;;; 2024-7-4
