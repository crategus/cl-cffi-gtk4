(in-package :gtk-test)

(def-suite gtk-pad-controller :in gtk-suite)
(in-suite gtk-pad-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPadActionType

(test gtk-pad-action-type
  ;; Check type
  (is (g:type-is-enum "GtkPadActionType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPadActionType")
          (g:gtype (cffi:foreign-funcall "gtk_pad_action_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:pad-action-type
          (glib:symbol-for-gtype "GtkPadActionType")))
  ;; Check names
  (is (equal '("GTK_PAD_ACTION_BUTTON" "GTK_PAD_ACTION_RING"
               "GTK_PAD_ACTION_STRIP")
             (glib-test:list-enum-item-names "GtkPadActionType")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkPadActionType")))
  ;; Check nick names
  (is (equal '("button" "ring" "strip")
             (glib-test:list-enum-item-nicks "GtkPadActionType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPadActionType" GTK:PAD-ACTION-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_pad_action_type_get_type")
                                    (:BUTTON 0)
                                    (:RING 1)
                                    (:STRIP 2))
             (gobject:get-gtype-definition "GtkPadActionType"))))

;;;     GtkPadController

(test gtk-pad-controller-class
  ;; Check type
  (is (g:type-is-object "GtkPadController"))
  ;; Check registered name
  (is (eq 'gtk:pad-controller
          (glib:symbol-for-gtype "GtkPadController")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPadController")
          (g:gtype (cffi:foreign-funcall "gtk_pad_controller_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkPadController")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPadController")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPadController")))
  ;; Check properties
  (is (equal '("action-group" "pad")
             (glib-test:list-properties "GtkPadController")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPadController")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPadController" GTK:PAD-CONTROLLER
                       (:SUPERCLASS GTK:EVENT-CONTROLLER
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_pad_controller_get_type")
                       ((ACTION-GROUP PAD-CONTROLLER-ACTION-GROUP
                         "action-group" "GActionGroup" T NIL)
                        (PAD PAD-CONTROLLER-PAD "pad" "GdkDevice" T NIL)))
             (gobject:get-gtype-definition "GtkPadController"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-pad-controller-properties
  (let* ((group (g:simple-action-group-new))
         (controller (gtk:pad-controller-new group nil)))
    (is (typep (gtk:pad-controller-action-group controller)
               'g:simple-action-group))
    (is-false (gtk:pad-controller-pad controller))
    ;; Check memory management
    (is (= 2 (g:object-ref-count group)))
    (is (= 1 (g:object-ref-count controller)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_pad_controller_new

(test gtk-pad-controller-new
  (let ((group (g:simple-action-group-new)))
    (is (typep group 'g:simple-action-group))
    ;; Constructor
    (is (typep (gtk:pad-controller-new group nil) 'gtk:pad-controller))
    (is (= 1 (g:object-ref-count (gtk:pad-controller-new group nil))))
    ;; Make instance
    (is (typep (make-instance 'gtk:pad-controller) 'gtk:pad-controller))
    (is (= 1 (g:object-ref-count (make-instance 'gtk:pad-controller))))
    ;; New object
    (is (typep (g:object-new "GtkPadController") 'gtk:pad-controller))
    (is (= 1 (g:object-ref-count (g:object-new "GtkPadController"))))))

;;;     gtk_pad_controller_set_action_entries

(test gtk-pad-controller-set-action-entries
  (let* ((group (make-instance 'g:simple-action-group))
         (controller (gtk:pad-controller-new group nil))
         (entries '((:button 1 -1 "Action 1" "action1")
                    (:ring 1 -1 "Action 2" "action2")
                    (:strip 1 -1 "Action 3" "action3"))))
    (is-false (gtk:pad-controller-set-action-entries controller entries))
    ;; TODO: Does not return a GAction object. Why?
    (is-false (g:action-map-lookup-action group "action1"))
    (is-false (g:action-map-lookup-action group "action2"))
    (is-false (g:action-map-lookup-action group "action3"))))

;;;     gtk_pad_controller_set_action

(test gtk-pad-controller-set-action
  (let* ((group (make-instance 'g:simple-action-group))
         (controller (gtk:pad-controller-new group nil)))
    (is-false (gtk:pad-controller-set-action controller
                                             :button
                                             1
                                             -1
                                             "Action"
                                             "action"))
    ;; TODO: Does not return a GAction object. Why?
    (is-false (g:action-map-lookup-action group "action"))))

;;; 2024-9-20
