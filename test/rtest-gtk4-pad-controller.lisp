(in-package :gtk-test)

(def-suite gtk-pad-controller :in gtk-event-handling)
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
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((group 2) controller :strong 1)
      ;; Create group and controller
      (setf group (g:simple-action-group-new))
      (setf controller (gtk:pad-controller-new group nil))
      ;; Retrieve properties
      (is (eq group (gtk:pad-controller-action-group controller)))
      (is-false (gtk:pad-controller-pad controller)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_pad_controller_new

(test gtk-pad-controller-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((group 3) controller :strong 1)
      ;; Create group
      (setf group (g:simple-action-group-new))
      ;; Constructor
      (is (typep (setf controller
                       (gtk:pad-controller-new group)) 'gtk:pad-controller))
      (is (typep (setf controller
                       (gtk:pad-controller-new group nil)) 'gtk:pad-controller))
      ;; Make instance
      (is (typep (setf controller
                       (make-instance 'gtk:pad-controller)) 'gtk:pad-controller))
      ;; New object
      (is (typep (setf controller
                       (g:object-new "GtkPadController")) 'gtk:pad-controller)))))

;;;     gtk_pad_controller_set_action_entries

(test gtk-pad-controller-set-action-entries
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((group 2) controller :strong 1)
      (let ((entries '((:button 0 -1 "Action 1" "action1")
                       (:ring 0 -1 "Action 2" "action2")
                       (:strip 0 -1 "Action 3" "action3"))))
        ;; Create group and controller
        (setf group (make-instance 'g:simple-action-group))
        (setf controller (gtk:pad-controller-new group))
        ;; Add entries to controller
        (is-false (gtk:pad-controller-set-action-entries controller entries))
        ;; FIME: Does not return a GAction object. Why?
        (is-false (g:action-map-lookup-action group "action1"))
        (is-false (g:action-map-lookup-action group "action2"))
        (is-false (g:action-map-lookup-action group "action3"))))))

;;;     gtk_pad_controller_set_action

(test gtk-pad-controller-set-action
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((group 2) controller :strong 1)
      ;; Create group and controller
      (setf group (make-instance 'g:simple-action-group))
      (setf controller (gtk:pad-controller-new group))
      ;; Add action entry
      (is-false (gtk:pad-controller-set-action controller
                                               :button
                                               0
                                               -1
                                               "Action"
                                               "pad.action"))
      ;; FIXME: Does not return a GAction object. Why?
      (is-false (g:action-map-lookup-action group "pad.action")))))

;;; 2025-2-23
