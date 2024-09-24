(in-package :gtk-test)

(def-suite gtk-event-controller :in gtk-suite)
(in-suite gtk-event-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPropagationPhase

(test gtk-propagation-phase
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
             (glib-test:list-enum-item-names "GtkPropagationPhase")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPropagationPhase")))
  ;; Check nick names
  (is (equal '("none" "capture" "bubble" "target")
             (glib-test:list-enum-item-nicks "GtkPropagationPhase")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPropagationPhase" GTK:PROPAGATION-PHASE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_propagation_phase_get_type")
                       (:NONE 0)
                       (:CAPTURE 1)
                       (:BUBBLE 2)
                       (:TARGET 3))
             (gobject:get-gtype-definition "GtkPropagationPhase"))))

;;;     GtkPropagationLimit

(test gtk-propagation-limit
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
             (glib-test:list-enum-item-names "GtkPropagationLimit")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GtkPropagationLimit")))
  ;; Check nick names
  (is (equal '("none" "same-native")
             (glib-test:list-enum-item-nicks "GtkPropagationLimit")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPropagationLimit" GTK:PROPAGATION-LIMIT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_propagation_limit_get_type")
                       (:NONE 0)
                       (:SAME-NATIVE 1))
             (gobject:get-gtype-definition "GtkPropagationLimit"))))

;;;     GtkEventController

(test gtk-event-controller-class
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
             (glib-test:list-children "GtkEventController")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkEventController")))
  ;; Check class properties
  (is (equal '("name" "propagation-limit" "propagation-phase" "widget")
             (glib-test:list-properties "GtkEventController")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkEventController")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEventController" GTK:EVENT-CONTROLLER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_event_controller_get_type")
                       ((NAME EVENT-CONTROLLER-NAME "name" "gchararray" T T)
                        (PROPAGATION-LIMIT EVENT-CONTROLLER-PROPAGATION-LIMIT
                         "propagation-limit" "GtkPropagationLimit" T T)
                        (PROPAGATION-PHASE EVENT-CONTROLLER-PROPAGATION-PHASE
                         "propagation-phase" "GtkPropagationPhase" T T)
                        (WIDGET EVENT-CONTROLLER-WIDGET
                         "widget" "GtkWidget" T NIL)))
             (gobject:get-gtype-definition "GtkEventController"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-event-controller-properties.1
  (let ((controller (gtk:event-controller-key-new)))
    (is-false (gtk:event-controller-name controller))
    (is (eq :same-native (gtk:event-controller-propagation-limit controller)))
    (is (eq :bubble (gtk:event-controller-propagation-phase controller)))
    (is-false (gtk:event-controller-widget controller))))

(test gtk-event-controller-properties.2
  (let ((controller (gtk:event-controller-motion-new)))
    (is-false (gtk:event-controller-name controller))
    (is (eq :same-native (gtk:event-controller-propagation-limit controller)))
    (is (eq :bubble (gtk:event-controller-propagation-phase controller)))
    (is-false (gtk:event-controller-widget controller))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_event_controller_reset

(test gtk-event-controller-reset
  (let ((controller (gtk:event-controller-key-new)))
    (is-false (gtk:event-controller-reset controller))))

;;;     gtk_event_controller_get_current_event

(test gtk-event-controller-current-event
  (let ((controller (gtk:event-controller-key-new)))
    (is-false (gtk:event-controller-current-event controller))))

;;;     gtk_event_controller_get_current_event_device

(test gtk-event-controller-current-event-device
  (let ((controller (gtk:event-controller-key-new)))
    (is-false (gtk:event-controller-current-event-device controller))))

;;;     gtk_event_controller_get_current_event_state

(test gtk-event-controller-current-event-state
  (let ((controller (gtk:event-controller-key-new)))
    (is-false (gtk:event-controller-current-event-state controller))))

;;;     gtk_event_controller_get_current_event_time

(test gtk-event-controller-current-event-time
  (let ((controller (gtk:event-controller-key-new)))
    (is (= gdk:+current-time+
           (gtk:event-controller-current-event-time controller)))))

;;; 2024-9-20
