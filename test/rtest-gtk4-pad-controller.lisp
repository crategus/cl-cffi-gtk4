(in-package :gtk-test)

(def-suite gtk-pad-controller :in gtk-suite)
(in-suite gtk-pad-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPadActionType

(test gtk-pad-action-type
  ;; Check the type
  (is (g:type-is-enum "GtkPadActionType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPadActionType")
          (g:gtype (cffi:foreign-funcall "gtk_pad_action_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:pad-action-type
          (glib:symbol-for-gtype "GtkPadActionType")))
  ;; Check the names
  (is (equal '("GTK_PAD_ACTION_BUTTON" "GTK_PAD_ACTION_RING"
               "GTK_PAD_ACTION_STRIP")
             (list-enum-item-name "GtkPadActionType")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "GtkPadActionType")))
  ;; Check the nick names
  (is (equal '("button" "ring" "strip")
             (list-enum-item-nick "GtkPadActionType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkPadActionType"
                             GTK-PAD-ACTION-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_pad_action_type_get_type")
                             (:BUTTON 0)
                             (:RING 1)
                             (:STRIP 2))
             (gobject:get-g-type-definition "GtkPadActionType"))))

;;;     GtkPadController

(test gtk-pad-controller-class
  ;; Type check
  (is (g:type-is-object "GtkPadController"))
  ;; Check the registered name
  (is (eq 'gtk:pad-controller
          (glib:symbol-for-gtype "GtkPadController")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPadController")
          (g:gtype (cffi:foreign-funcall "gtk_pad_controller_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkEventController")
          (g:type-parent "GtkPadController")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPadController")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkPadController")))
  ;; Check the properties
  (is (equal '("action-group" "pad")
             (list-properties "GtkPadController")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPadController")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPadController" GTK-PAD-CONTROLLER
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_pad_controller_get_type")
                       ((ACTION-GROUP GTK-PAD-CONTROLLER-ACTION-GROUP
                         "action-group" "GActionGroup" T NIL)
                        (PAD GTK-PAD-CONTROLLER-PAD "pad" "GdkDevice" T NIL)))
             (gobject:get-g-type-definition "GtkPadController"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-pad-controller-properties
  (let* ((group (g:simple-action-group-new))
         (controller (gtk:pad-controller-new group nil)))
    (is (typep (gtk:pad-controller-action-group controller)
               'g:simple-action-group))
    (is-false (gtk:pad-controller-pad controller))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_pad_controller_new

(test gtk-pad-controller-new
  (let ((group (g:simple-action-group-new)))
  (is (typep group 'g:simple-action-group))
  (is (typep (gtk:pad-controller-new group nil) 'gtk:pad-controller))))

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
    (is-false (g:action-map-lookup-action group "action3"))
))

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
    (is-false (g:action-map-lookup-action group "action"))
))

;;; --- 2023-11-1 --------------------------------------------------------------
