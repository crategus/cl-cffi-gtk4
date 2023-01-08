(in-package :gtk-test)

(def-suite gtk-pad-controller :in gtk-suite)
(in-suite gtk-pad-controller)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPadActionType
;;;     GtkPadActionEntry

;;;     GtkPadController

(test pad-controller-class
  ;; Type check
  (is (g:type-is-object "GtkPadController"))
  ;; Check the registered name
  (is (eq 'gtk:pad-controller
          (gobject:symbol-for-gtype "GtkPadController")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPadController")
          (g:gtype (foreign-funcall "gtk_pad_controller_get_type" :size))))
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
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPadController" GTK-PAD-CONTROLLER
                       (:SUPERCLASS GTK-EVENT-CONTROLLER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_pad_controller_get_type")
                       ((ACTION-GROUP GTK-PAD-CONTROLLER-ACTION-GROUP
                         "action-group" "GActionGroup" T NIL)
                        (PAD GTK-PAD-CONTROLLER-PAD "pad" "GdkDevice" T NIL)))
             (gobject:get-g-type-definition "GtkPadController"))))

;;; --- Properties -------------------------------------------------------------

;;;     action-group
;;;     pad

;;; --- Functions --------------------------------------------------------------

;;;     gtk_pad_controller_new
;;;     gtk_pad_controller_set_action_entries
;;;     gtk_pad_controller_set_action

;;; 2022-11-12
