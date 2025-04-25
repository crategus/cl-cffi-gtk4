(in-package :gtk-test)

(def-suite gtk-stack-sidebar :in gtk-layout-widgets)
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
             (glib-test:list-children "GtkStackSidebar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkStackSidebar")))
  ;; Check properties
  (is (equal '("stack")
             (glib-test:list-properties "GtkStackSidebar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkStackSidebar")))
  ;; Check CSS name
  (is (string= "stacksidebar"
               (gtk:widget-class-css-name "GtkStackSidebar")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkStackSidebar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkStackSidebar" GTK:STACK-SIDEBAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_stack_sidebar_get_type")
                      ((STACK STACK-SIDEBAR-STACK "stack" "GtkStack" T T)))
             (gobject:get-gtype-definition "GtkStackSidebar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-stack-sidebar-stack.1
  (glib-test:with-check-memory (sidebar)
    (is (typep (setf sidebar
                     (make-instance 'gtk:stack-sidebar)) 'gtk:stack-sidebar))
    (is-false (gtk:stack-sidebar-stack sidebar))))

(test gtk-stack-sidebar-stack.2
  (glib-test:with-check-memory (sidebar stack)
    (is (typep (setf sidebar
                     (make-instance 'gtk:stack-sidebar)) 'gtk:stack-sidebar))
    (is (typep (setf stack (make-instance 'gtk:stack)) 'gtk:stack))
    (is (eq stack (setf (gtk:stack-sidebar-stack sidebar) stack)))
    (is (eq stack (gtk:stack-sidebar-stack sidebar)))
    (is-false (setf (gtk:stack-sidebar-stack sidebar) nil))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_sidebar_new

(test gtk-stack-sidebar-new
  (glib-test:with-check-memory (sidebar)
    (is (typep (setf sidebar (gtk:stack-sidebar-new)) 'gtk:stack-sidebar))))

;;; 2025-4-23
