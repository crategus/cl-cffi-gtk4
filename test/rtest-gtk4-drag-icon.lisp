(in-package :gtk-test)

(def-suite gtk-drag-icon :in gtk-suite)
(in-suite gtk-drag-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDragIcon

(test gtk-drag-icon-class
  ;; Type check
  (is (g:type-is-object "GtkDragIcon"))
  ;; Check the registered name
  (is (eq 'gtk:drag-icon
          (glib:symbol-for-gtype "GtkDragIcon")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkDragIcon")
          (g:gtype (cffi:foreign-funcall "gtk_drag_icon_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkDragIcon")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkDragIcon")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkRoot")
             (list-interfaces "GtkDragIcon")))
  ;; Check the properties
  (is (equal '("child")
             (list-properties "GtkDragIcon")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkDragIcon")))
  ;; CSS name
  (is (string= "dnd"
               (gtk:widget-class-css-name "GtkDragIcon")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:drag-icon))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkDragIcon")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkDragIcon" GTK-DRAG-ICON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkNative" "GtkRoot")
                                :TYPE-INITIALIZER "gtk_drag_icon_get_type")
                               ((CHILD GTK-DRAG-ICON-CHILD "child" "GtkWidget"
                                 T T)))
             (gobject:get-g-type-definition "GtkDragIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_icon_get_for_drag
;;;     gtk_drag_icon_set_from_paintable
;;;     gtk_drag_icon_create_widget_for_value

;;; --- 2023-10-16 -------------------------------------------------------------
