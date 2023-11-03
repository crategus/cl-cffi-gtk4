(in-package :gtk-test)

(def-suite gtk-stack-switcher :in gtk-suite)
(in-suite gtk-stack-switcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSwitcher

(test gtk-stack-switcher-class
  ;; Type check
  (is (g:type-is-object "GtkStackSwitcher"))
  ;; Check the registered name
  (is (eq 'gtk:stack-switcher
          (glib:symbol-for-gtype "GtkStackSwitcher")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStackSwitcher")
          (g:gtype (cffi:foreign-funcall "gtk_stack_switcher_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStackSwitcher")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStackSwitcher")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkStackSwitcher")))
  ;; Check the properties
  (is (equal '("orientation" "stack")
             (list-properties "GtkStackSwitcher")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkStackSwitcher")))
  ;; CSS name
  (is (string= "stackswitcher"
               (gtk:widget-class-css-name "GtkStackSwitcher")))
  ;; CSS classes
  (is (equal '("linked")
             (gtk:widget-css-classes (make-instance 'gtk:stack-switcher))))
  ;; Accessible role
  (is (eq :tab-list (gtk:widget-class-accessible-role "GtkStackSwitcher")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStackSwitcher"
                                             GTK-STACK-SWITCHER
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER
                                "gtk_stack_switcher_get_type")
                               ((STACK GTK-STACK-SWITCHER-STACK "stack"
                                 "GtkStack" T T)))
             (gobject:get-g-type-definition "GtkStackSwitcher"))))

;;; --- Properties -------------------------------------------------------------

;;;     stack

(test gtk-stack-switcher-stack
  (let ((switcher (make-instance 'gtk:stack-switcher)))
    (is-false (gtk:stack-switcher-stack switcher))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_switcher_new

(test gtk-stack-switcher-new
  (is (typep (gtk:stack-switcher-new) 'gtk:stack-switcher)))

;;; --- 2023-8-9 ---------------------------------------------------------------
