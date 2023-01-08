(in-package :gtk-test)

(def-suite gtk-event-controller-scroll :in gtk-suite)
(in-suite gtk-event-controller-scroll)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerScrollFlags

;;;     GtkEventControllerScroll

(test event-controller-scroll-class
  ;; Type check
  (is (g:type-is-object "GtkEventControllerScroll"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller-scroll
          (gobject:symbol-for-gtype "GtkEventControllerScroll")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventControllerScroll")
          (g:gtype (foreign-funcall "gtk_event_controller_scroll_get_type"
                                    :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerScroll")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEventControllerScroll")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventControllerScroll")))
  ;; Check the properties
  (is (equal '("flags")
             (list-properties "GtkEventControllerScroll")))
  ;; Check the signals
  (is (equal '("decelerate" "scroll" "scroll-begin" "scroll-end")
             (list-signals "GtkEventControllerScroll")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkEventControllerScroll"
                                     GTK-EVENT-CONTROLLER-SCROLL
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER
                        "gtk_event_controller_scroll_get_type")
                       ((FLAGS GTK-EVENT-CONTROLLER-SCROLL-FLAGS "flags"
                         "GtkEventControllerScrollFlags" T T)))
             (gobject:get-g-type-definition "GtkEventControllerScroll"))))

;;; --- Properties -------------------------------------------------------------

;;;     flags

;;; --- Signals ----------------------------------------------------------------

;;;     decelerate
;;;     scroll
;;;     scroll-begin
;;;     scroll-end

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_scroll_new
;;;     gtk_event_controller_get_unit                      Since 4.8

;;; 2022-11-13
