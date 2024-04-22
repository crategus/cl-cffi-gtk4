(in-package :gtk-test)

(def-suite gtk-stack-sidebar :in gtk-suite)
(in-suite gtk-stack-sidebar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSidebar

(test gtk-stack-sidebar-class
  ;; Check type
  (is (g:type-is-object "GtkStackSidebar"))
  ;; Check registered name
  (is (eq 'gtk:stack-sidebar
          (glib:symbol-for-gtype "GtkStackSidebar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkStackSidebar")
          (g:gtype (cffi:foreign-funcall "gtk_stack_sidebar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStackSidebar")))
  ;; Check children
  (is (equal '()
             (list-children "GtkStackSidebar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkStackSidebar")))
  ;; Check properties
  (is (equal '("stack")
             (list-properties "GtkStackSidebar")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkStackSidebar")))
  ;; Check CSS name
  (is (string= "stacksidebar"
               (gtk:widget-class-css-name "GtkStackSidebar")))
  ;; Check CSS classes
  (is (equal '("sidebar")
             (gtk:widget-css-classes (make-instance 'gtk:stack-sidebar))))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkStackSidebar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStackSidebar" GTK-STACK-SIDEBAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_stack_sidebar_get_type")
                               ((STACK GTK-STACK-SIDEBAR-STACK "stack"
                                 "GtkStack" T T)))
             (gobject:get-g-type-definition "GtkStackSidebar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-sidebar-stack.1
  (let ((sidebar (make-instance 'gtk:stack-sidebar)))
    (is-false (gtk:stack-sidebar-stack sidebar))))

(test gtk-stack-sidebar-stack.2
  (let ((sidebar (make-instance 'gtk:stack-sidebar))
        (stack (make-instance 'gtk:stack)))
    (is (eq stack (setf (gtk:stack-sidebar-stack sidebar) stack)))
    (is (eq stack (gtk:stack-sidebar-stack sidebar)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_sidebar_new

(test gtk-stack-sidebar-new
  (is (typep (gtk:stack-sidebar-new) 'gtk:stack-sidebar)))

;;; 2024-4-15
