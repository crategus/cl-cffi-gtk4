(in-package :gtk-test)

(def-suite gtk-event-controller-scroll :in gtk-suite)
(in-suite gtk-event-controller-scroll)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEventControllerScrollFlags

(test gtk-event-controller-scroll-flags
  ;; Check type
  (is (g:type-is-flags "GtkEventControllerScrollFlags"))
  ;; Check registered name
  (is (eq 'gtk:event-controller-scroll-flags
          (glib:symbol-for-gtype "GtkEventControllerScrollFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventControllerScrollFlags")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_scroll_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_EVENT_CONTROLLER_SCROLL_NONE"
               "GTK_EVENT_CONTROLLER_SCROLL_VERTICAL"
               "GTK_EVENT_CONTROLLER_SCROLL_HORIZONTAL"
               "GTK_EVENT_CONTROLLER_SCROLL_DISCRETE"
               "GTK_EVENT_CONTROLLER_SCROLL_KINETIC"
               "GTK_EVENT_CONTROLLER_SCROLL_BOTH_AXES")
             (glib-test:list-flags-item-names "GtkEventControllerScrollFlags")))
  ;; Check values
  (is (equal '(0 1 2 4 8 3)
             (glib-test:list-flags-item-values "GtkEventControllerScrollFlags")))
  ;; Check nick names
  (is (equal '("none" "vertical" "horizontal" "discrete" "kinetic" "both-axes")
             (glib-test:list-flags-item-nicks "GtkEventControllerScrollFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkEventControllerScrollFlags"
                                     GTK:EVENT-CONTROLLER-SCROLL-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_event_controller_scroll_flags_get_type")
                       (:NONE 0)
                       (:VERTICAL 1)
                       (:HORIZONTAL 2)
                       (:DISCRETE 4)
                       (:KINETIC 8)
                       (:BOTH-AXES 3))
             (gobject:get-gtype-definition "GtkEventControllerScrollFlags"))))

;;;     GtkEventControllerScroll

(test gtk-event-controller-scroll-class
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
             (glib-test:list-children "GtkEventControllerScroll")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventControllerScroll")))
  ;; Check properties
  (is (equal '("flags")
             (glib-test:list-properties "GtkEventControllerScroll")))
  ;; Check signals
  (is (equal '("decelerate" "scroll" "scroll-begin" "scroll-end")
             (glib-test:list-signals "GtkEventControllerScroll")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventControllerScroll"
                                      GTK:EVENT-CONTROLLER-SCROLL
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER
                        "gtk_event_controller_scroll_get_type")
                       ((FLAGS EVENT-CONTROLLER-SCROLL-FLAGS
                         "flags" "GtkEventControllerScrollFlags" T T)))
             (gobject:get-gtype-definition "GtkEventControllerScroll"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-controller-scroll-properties
  (let ((controller (make-instance 'gtk:event-controller-scroll)))
    (is-false (gtk:event-controller-scroll-flags controller))))

;;; --- Signals ----------------------------------------------------------------

;;;     decelerate

(test gtk-event-controller-scroll-decelerate-signal
  (let* ((name "decelerate")
         (gtype (g:gtype "GtkEventControllerScroll"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     scroll

(test gtk-event-controller-scroll-scroll-signal
  (let* ((name "scroll")
         (gtype (g:gtype "GtkEventControllerScroll"))
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
    (is (equal '("gdouble" "gdouble")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     scroll-begin

(test gtk-event-controller-scroll-scroll-begin-signal
  (let* ((name "scroll-begin")
         (gtype (g:gtype "GtkEventControllerScroll"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     scroll-end

(test gtk-event-controller-scroll-scroll-end-signal
  (let* ((name "scroll-end")
         (gtype (g:gtype "GtkEventControllerScroll"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_scroll_new

(test gtk-event-controller-scroll-new
  (is (typep (gtk:event-controller-scroll-new :none) 'gtk:event-controller-scroll))
  (is (typep (gtk:event-controller-scroll-new :vertical) 'gtk:event-controller-scroll))
  (is (typep (gtk:event-controller-scroll-new :horizontal) 'gtk:event-controller-scroll))
  (is (typep (gtk:event-controller-scroll-new :discrete) 'gtk:event-controller-scroll))
  (is (typep (gtk:event-controller-scroll-new :kinetic) 'gtk:event-controller-scroll))
  (is (typep (gtk:event-controller-scroll-new :both-axes) 'gtk:event-controller-scroll)))

;;;     gtk_event_controller_get_unit                      Since 4.8

(test gtk-event-controller-scroll-unit
  (let ((controller (gtk:event-controller-scroll-new :both-axes)))
    (is (eq :wheel (gtk:event-controller-scroll-unit controller)))))

;;; 2024-9-20
