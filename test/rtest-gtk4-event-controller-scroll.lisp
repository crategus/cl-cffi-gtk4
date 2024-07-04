(in-package :gtk-test)

(def-suite gtk-event-controller-scroll :in gtk-suite)
(in-suite gtk-event-controller-scroll)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerScrollFlags

;;;     GtkEventControllerScroll

(test event-controller-scroll-class
  ;; Check type
  (is (g:type-is-object "GtkEventControllerScroll"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-scroll
          (glib:symbol-for-gtype "GtkEventControllerScroll")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerScroll")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_scroll_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkEventControllerScroll")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkEventControllerScroll")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventControllerScroll")))
  ;; Check properties
  (is (equal '("flags")
             (gtk-test:list-properties "GtkEventControllerScroll")))
  ;; Check signals
  (is (equal '("decelerate" "scroll" "scroll-begin" "scroll-end")
             (gtk-test:list-signals "GtkEventControllerScroll")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventControllerScroll"
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

;;; 2024-7-4
