(in-package :gtk-test)

(def-suite gtk-event-controller :in gtk-suite)
(in-suite gtk-event-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPropagationPhase

(test propagation-phase
  ;; Check the type
  (is (g:type-is-enum "GtkPropagationPhase"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPropagationPhase")
          (g:gtype (cffi:foreign-funcall "gtk_propagation_phase_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:propagation-phase
          (glib:symbol-for-gtype "GtkPropagationPhase")))
  ;; Check the names
  (is (equal '("GTK_PHASE_NONE" "GTK_PHASE_CAPTURE" "GTK_PHASE_BUBBLE"
               "GTK_PHASE_TARGET")
             (list-enum-item-name "GtkPropagationPhase")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "GtkPropagationPhase")))
  ;; Check the nick names
  (is (equal '("none" "capture" "bubble" "target")
             (list-enum-item-nick "GtkPropagationPhase")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPropagationPhase"
                             GTK-PROPAGATION-PHASE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_propagation_phase_get_type")
                             (:NONE 0)
                             (:CAPTURE 1)
                             (:BUBBLE 2)
                             (:TARGET 3))
             (gobject:get-g-type-definition "GtkPropagationPhase"))))

;;;     GtkPropagationLimit

(test propagation-limit
  ;; Check the type
  (is (g:type-is-enum "GtkPropagationLimit"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPropagationLimit")
          (g:gtype (cffi:foreign-funcall "gtk_propagation_limit_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:propagation-limit
          (glib:symbol-for-gtype "GtkPropagationLimit")))
  ;; Check the names
  (is (equal '("GTK_LIMIT_NONE" "GTK_LIMIT_SAME_NATIVE")
             (list-enum-item-name "GtkPropagationLimit")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkPropagationLimit")))
  ;; Check the nick names
  (is (equal '("none" "same-native")
             (list-enum-item-nick "GtkPropagationLimit")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPropagationLimit"
                             GTK-PROPAGATION-LIMIT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_propagation_limit_get_type")
                             (:NONE 0)
                             (:SAME-NATIVE 1))
             (gobject:get-g-type-definition "GtkPropagationLimit"))))

;;;     GtkEventController

(test event-controller-class
  ;; Type check
  (is (g:type-is-object "GtkEventController"))
  ;; Check the registered name
  (is (eq 'gtk:event-controller
          (glib:symbol-for-gtype "GtkEventController")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEventController")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEventController")))
  ;; Check the children
  (is (equal '("GtkDropControllerMotion" "GtkDropTarget" "GtkDropTargetAsync"
               "GtkEventControllerFocus" "GtkEventControllerKey"
               "GtkEventControllerLegacy" "GtkEventControllerMotion"
               "GtkEventControllerScroll" "GtkGesture" "GtkPadController"
               "GtkShortcutController")
             (list-children "GtkEventController")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkEventController")))
  ;; Check the class properties
  (is (equal '("name" "propagation-limit" "propagation-phase" "widget")
             (list-properties "GtkEventController")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkEventController")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEventController" GTK-EVENT-CONTROLLER
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_event_controller_get_type")
                       ((NAME GTK-EVENT-CONTROLLER-NAME "name" "gchararray" T
                         T)
                        (PROPAGATION-LIMIT
                         GTK-EVENT-CONTROLLER-PROPAGATION-LIMIT
                         "propagation-limit" "GtkPropagationLimit" T T)
                        (PROPAGATION-PHASE
                         GTK-EVENT-CONTROLLER-PROPAGATION-PHASE
                         "propagation-phase" "GtkPropagationPhase" T T)
                        (WIDGET GTK-EVENT-CONTROLLER-WIDGET "widget"
                         "GtkWidget" T NIL)))
             (gobject:get-g-type-definition "GtkEventController"))))

;;; --- Properties -------------------------------------------------------------

;;;     name
;;;     propagation-limit
;;;     propagation-phase
;;;     widget

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_reset
;;;     gtk_event_controller_get_current_event
;;;     gtk_event_controller_get_current_event_device
;;;     gtk_event_controller_get_current_event_state
;;;     gtk_event_controller_get_current_event_time

;;; --- 2023-5-29 --------------------------------------------------------------
