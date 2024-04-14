(in-package :gtk-test)

(def-suite gtk-layout-child :in gtk-suite)
(in-suite gtk-layout-child)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLayoutChild

(test gtk-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:layout-child
          (glib:symbol-for-gtype "GtkLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_layout_child_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkLayoutChild")))
  ;; Check children
  (is (equal '("GtkConstraintLayoutChild" "GtkFixedLayoutChild"
               "GtkGridLayoutChild" "GtkOverlayLayoutChild")
             (list-children "GtkLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkLayoutChild")))
  ;; Check properties
  (is (equal '("child-widget" "layout-manager")
             (list-properties "GtkLayoutChild")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLayoutChild" GTK-LAYOUT-CHILD
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_layout_child_get_type")
                               ((CHILD-WIDGET GTK-LAYOUT-CHILD-CHILD-WIDGET
                                 "child-widget" "GtkWidget" T NIL)
                                (LAYOUT-MANAGER GTK-LAYOUT-CHILD-LAYOUT-MANAGER
                                 "layout-manager" "GtkLayoutManager" T NIL)))
             (gobject:get-g-type-definition "GtkLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

;; TODO: How do we get the gtk:layout-child object?!

;;;     child-widget
;;;     layout-manager

;;; 2024-4-12
