(in-package :gtk-test)

(def-suite gtk-stack-sidebar :in gtk-suite)
(in-suite gtk-stack-sidebar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkStackSidebar

(test gtk-stack-sidebar-class
  ;; Type check
  (is (g:type-is-object "GtkStackSidebar"))
  ;; Check the registered name
  (is (eq 'gtk:stack-sidebar
          (glib:symbol-for-gtype "GtkStackSidebar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkStackSidebar")
          (g:gtype (cffi:foreign-funcall "gtk_stack_sidebar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkStackSidebar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkStackSidebar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkStackSidebar")))
  ;; Check the properties
  (is (equal '("stack")
             (list-properties "GtkStackSidebar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkStackSidebar")))
  ;; CSS name
  (is (string= "stacksidebar"
               (gtk:widget-class-css-name "GtkStackSidebar")))
  ;; CSS style context
  (is (string=
"stacksidebar.sidebar:dir(ltr)
  scrolledwindow:dir(ltr)
    viewport:dir(ltr)
      list.navigation-sidebar:dir(ltr)
    scrollbar.bottom.horizontal:dir(ltr)
      range.horizontal:dir(ltr)
        trough:dir(ltr)
          slider:dir(ltr)
    scrollbar.right.vertical:dir(ltr)
      range.vertical:dir(ltr)
        trough:dir(ltr)
          slider:dir(ltr)
    overshoot.left:dir(ltr)
    undershoot.left:dir(ltr)
    overshoot.right:dir(ltr)
    undershoot.right:dir(ltr)
    overshoot.top:dir(ltr)
    undershoot.top:dir(ltr)
    overshoot.bottom:dir(ltr)
    undershoot.bottom:dir(ltr)
    junction:dir(ltr)
"
               (print-style-context "GtkStackSidebar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkStackSidebar" GTK-STACK-SIDEBAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_stack_sidebar_get_type")
                               ((STACK GTK-STACK-SIDEBAR-STACK "stack"
                                 "GtkStack" T T)))
             (gobject:get-g-type-definition "GtkStackSidebar"))))

;;; --- Properties -------------------------------------------------------------

;;;     stack

(test gtk-stack-sidebar-stack
  (let ((sidebar (make-instance 'gtk:stack-sidebar)))
    (is-false (gtk:stack-sidebar-stack sidebar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_stack_sidebar_new

(test gtk-stack-sidebar-new
  (is (typep (gtk:stack-sidebar-new) 'gtk:stack-sidebar)))

;;; --- 2023-8-9 ---------------------------------------------------------------
