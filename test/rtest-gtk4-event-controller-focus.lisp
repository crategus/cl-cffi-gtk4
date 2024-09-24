(in-package :gtk-test)

(def-suite gtk-event-controller-focus :in gtk-suite)
(in-suite gtk-event-controller-focus)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerFocus

(test gtk-event-controller-focus-class
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
             (glib-test:list-children "GtkEventControllerFocus")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventControllerFocus")))
  ;; Check properties
  (is (equal '("contains-focus" "is-focus")
             (glib-test:list-properties "GtkEventControllerFocus")))
  ;; Check signals
  (is (equal '("enter" "leave")
             (glib-test:list-signals "GtkEventControllerFocus")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventControllerFocus"
                                      GTK:EVENT-CONTROLLER-FOCUS
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_event_controller_focus_get_type")
                       ((CONTAINS-FOCUS EVENT-CONTROLLER-FOCUS-CONTAINS-FOCUS
                         "contains-focus" "gboolean" T NIL)
                        (IS-FOCUS EVENT-CONTROLLER-FOCUS-IS-FOCUS
                         "is-focus" "gboolean" T NIL)))
             (gobject:get-gtype-definition "GtkEventControllerFocus"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-controller-focus-properties
  (let ((controller (make-instance 'gtk:event-controller-focus)))
    (is-false (gtk:event-controller-focus-contains-focus controller))
    (is-false (gtk:event-controller-focus-is-focus controller))))

;;; --- Signals ----------------------------------------------------------------

;;;     enter

(test gtk-event-controller-focus-enter-signal
  (let* ((name "enter")
         (gtype (g:gtype "GtkEventControllerFocus"))
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

;;;     leave

(test gtk-event-controller-focus-leave-signal
  (let* ((name "leave")
         (gtype (g:gtype "GtkEventControllerFocus"))
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

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_focus_new

(test gtk-event-controller-focus-new
  (is (typep (gtk:event-controller-focus-new) 'gtk:event-controller-focus)))

;;; 2024-7-27
