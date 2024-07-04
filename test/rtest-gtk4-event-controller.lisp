(in-package :gtk-test)

(def-suite gtk-event-controller :in gtk-suite)
(in-suite gtk-event-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPropagationPhase

(test propagation-phase
  ;; Check type
  (is (g:type-is-enum "GtkPropagationPhase"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPropagationPhase")
          (g:gtype (cffi:foreign-funcall "gtk_propagation_phase_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:propagation-phase
          (glib:symbol-for-gtype "GtkPropagationPhase")))
  ;; Check names
  (is (equal '("GTK_PHASE_NONE" "GTK_PHASE_CAPTURE" "GTK_PHASE_BUBBLE"
               "GTK_PHASE_TARGET")
             (gtk-test:list-enum-item-name "GtkPropagationPhase")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (gtk-test:list-enum-item-value "GtkPropagationPhase")))
  ;; Check nick names
  (is (equal '("none" "capture" "bubble" "target")
             (gtk-test:list-enum-item-nick "GtkPropagationPhase")))
  ;; Check enum definition
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
  ;; Check type
  (is (g:type-is-enum "GtkPropagationLimit"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPropagationLimit")
          (g:gtype (cffi:foreign-funcall "gtk_propagation_limit_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:propagation-limit
          (glib:symbol-for-gtype "GtkPropagationLimit")))
  ;; Check names
  (is (equal '("GTK_LIMIT_NONE" "GTK_LIMIT_SAME_NATIVE")
             (gtk-test:list-enum-item-name "GtkPropagationLimit")))
  ;; Check values
  (is (equal '(0 1)
             (gtk-test:list-enum-item-value "GtkPropagationLimit")))
  ;; Check nick names
  (is (equal '("none" "same-native")
             (gtk-test:list-enum-item-nick "GtkPropagationLimit")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPropagationLimit"
                             GTK-PROPAGATION-LIMIT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_propagation_limit_get_type")
                             (:NONE 0)
                             (:SAME-NATIVE 1))
             (gobject:get-g-type-definition "GtkPropagationLimit"))))

;;;     GtkEventController

(test event-controller-class
  ;; Check type
  (is (g:type-is-object "GtkEventController"))
  ;; Check registered name
  (is (eq 'gtk:event-controller
          (glib:symbol-for-gtype "GtkEventController")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEventController")
          (g:gtype (cffi:foreign-funcall "gtk_event_controller_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkEventController")))
  ;; Check children
  (is (equal '("GtkDropControllerMotion" "GtkDropTarget" "GtkDropTargetAsync"
               "GtkEventControllerFocus" "GtkEventControllerKey"
               "GtkEventControllerLegacy" "GtkEventControllerMotion"
               "GtkEventControllerScroll" "GtkGesture" "GtkPadController"
               "GtkShortcutController")
             (gtk-test:list-children "GtkEventController")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkEventController")))
  ;; Check class properties
  (is (equal '("name" "propagation-limit" "propagation-phase" "widget")
             (gtk-test:list-properties "GtkEventController")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkEventController")))
  ;; Check class definition
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

;;; 2024-7-4
