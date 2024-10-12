(in-package :gtk-test)

(def-suite gtk-stack-switcher :in gtk-suite)
(in-suite gtk-stack-switcher)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSwitcher

(test gtk-stack-switcher-class
  ;; Check type
  (is (g:type-is-object "GtkStackSwitcher"))
  ;; Check registered name
  (is (eq 'gtk:stack-switcher
          (glib:symbol-for-gtype "GtkStackSwitcher")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackSwitcher")
          (g:gtype (cffi:foreign-funcall "gtk_stack_switcher_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStackSwitcher")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkStackSwitcher")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkStackSwitcher")))
  ;; Check properties
  (is (equal '("orientation" "stack")
             (glib-test:list-properties "GtkStackSwitcher")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStackSwitcher")))
  ;; Check CSS name
  (is (string= "stackswitcher"
               (gtk:widget-class-css-name "GtkStackSwitcher")))
  ;; Check accessible role
  (is (eq :tab-list (gtk:widget-class-accessible-role "GtkStackSwitcher")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStackSwitcher" GTK:STACK-SWITCHER
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_stack_switcher_get_type")
                       ((STACK STACK-SWITCHER-STACK "stack" "GtkStack" T T)))
             (gobject:get-gtype-definition "GtkStackSwitcher"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-switcher-stack.1
  (let ((switcher (make-instance 'gtk:stack-switcher)))
    (is-false (gtk:stack-switcher-stack switcher))))

(test gtk-stack-switch-stack.2
  (let ((switcher (make-instance 'gtk:stack-switcher))
        (stack (make-instance 'gtk:stack)))
    (is (eq stack (setf (gtk:stack-switcher-stack switcher) stack)))
    (is (eq stack (gtk:stack-switcher-stack switcher)))
    (is-false (setf (gtk:stack-switcher-stack switcher) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_switcher_new

(test gtk-stack-switcher-new
  (is (typep (gtk:stack-switcher-new) 'gtk:stack-switcher)))

;;; 2024-10-9
