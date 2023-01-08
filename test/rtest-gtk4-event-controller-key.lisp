(in-package :gtk-test)

(def-suite gtk-event-controller-key :in gtk-suite)
(in-suite gtk-event-controller-key)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerKey

(test event-controller-key-class
  ;; Type check
  (is (g:type-is-object "GtkEventControllerKey"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller-key
          (gobject:symbol-for-gtype "GtkEventControllerKey")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventControllerKey")
          (g:gtype (foreign-funcall "gtk_event_controller_key_get_type"
                                    :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerKey")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventControllerKey")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventControllerKey")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkEventControllerKey")))
  ;; Check the signals
  (is (equal '("im-update" "key-pressed" "key-released" "modifiers")
             (list-signals "GtkEventControllerKey")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventControllerKey"
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

;;; 2022-11-13
