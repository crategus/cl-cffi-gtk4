(in-package :gtk-test)

(def-suite gtk-event-controller-key :in gtk-event-handling)
(in-suite gtk-event-controller-key)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerKey

(test gtk-event-controller-key-class
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
             (glib-test:list-children "GtkEventControllerKey")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventControllerKey")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkEventControllerKey")))
  ;; Check signals
  (is (equal '("im-update" "key-pressed" "key-released" "modifiers")
             (glib-test:list-signals "GtkEventControllerKey")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventControllerKey"
                                      GTK:EVENT-CONTROLLER-KEY
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_event_controller_key_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkEventControllerKey"))))

;;; --- Signals ----------------------------------------------------------------

;;;     im-update

(test gtk-event-controller-key-im-update-signal
  (let* ((name "im-update")
         (gtype (g:gtype "GtkEventControllerKey"))
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

;;;     key-pressed

(test gtk-event-controller-key-key-pressed-signal
  (let* ((name "key-pressed")
         (gtype (g:gtype "GtkEventControllerKey"))
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
    (is (equal '("guint" "guint" "GdkModifierType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     key-released

(test gtk-event-controller-key-key-released-signal
  (let* ((name "key-released")
         (gtype (g:gtype "GtkEventControllerKey"))
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
    (is (equal '("guint" "guint" "GdkModifierType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     modifiers

(test gtk-event-controller-key-modifiers-signal
  (let* ((name "modifiers")
         (gtype (g:gtype "GtkEventControllerKey"))
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
    (is (equal '("GdkModifierType")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_key_new

(test gtk-event-controller-key-new
  (is (typep (gtk:event-controller-key-new) 'gtk:event-controller-key)))

;;;     gtk_event_controller_key_set_im_context
;;;     gtk_event_controller_key_get_im_context

(test gtk-event-controller-key-im-context
  (let ((controller (gtk:event-controller-key-new))
        (context (gtk:im-context-simple-new)))
    (is (eq context (setf (gtk:event-controller-key-im-context controller)
                          context)))
    (is (eq context (gtk:event-controller-key-im-context controller)))
    ;; Check memory management
    (is-false (setf (gtk:event-controller-key-im-context controller) nil))
    (is (= 1 (g:object-ref-count context)))
    (is (= 1 (g:object-ref-count controller)))))

;;;     gtk_event_controller_key_forward

;;;     gtk_event_controller_key_get_group

;; TODO: Causes a critical warning
;;     Gtk-CRITICAL : gtk_event_controller_key_get_group
;;                  : assertion 'controller->current_event != NULL' failed

#+nil
(test gtk-event-controller-key-group
  (let ((controller (gtk:event-controller-key-new)))
    (is (= 0 (gtk:event-controller-key-group controller)))))

;;; 2024-10-31
