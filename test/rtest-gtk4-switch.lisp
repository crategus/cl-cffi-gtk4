(in-package :gtk-test)

(def-suite gtk-switch :in gtk-suite)
(in-suite gtk-switch)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSwitch

(test gtk-switch-class
  ;; Type check
  (is (g:type-is-object "GtkSwitch"))
  ;; Check the registered name
  (is (eq 'gtk:switch
          (glib:symbol-for-gtype "GtkSwitch")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkSwitch")
          (g:gtype (cffi:foreign-funcall "gtk_switch_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSwitch")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkSwitch")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkSwitch")))
  ;; Check the properties
  (is (equal '("action-name" "action-target" "active" "state")
             (list-properties "GtkSwitch")))
  ;; Check the signals
  (is (equal '("activate" "state-set")
             (list-signals "GtkSwitch")))
  ;; CSS information
  (is (string= "switch"
               (gtk:widget-class-css-name "GtkSwitch")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkSwitch" GTK-SWITCH
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_switch_get_type")
                       ((ACTIVE GTK-SWITCH-ACTIVE "active" "gboolean" T T)
                        (STATE GTK-SWITCH-STATE "state" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkSwitch"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-switch-properties
  (let ((switch (make-instance 'gtk:switch)))
    (is-true (setf (gtk:switch-active switch) t))
    (is-true (gtk:switch-active switch))
    (is-true (setf (gtk:switch-state switch) t))
    (is-true (gtk:switch-active switch))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     state-set

;;; --- Functions --------------------------------------------------------------

;;;     gtk_switch_new

(test gtk-switch-new
  (is (typep (gtk:switch-new) 'gtk:switch)))

;;; --- 2023-5-29 --------------------------------------------------------------
