(in-package :gtk-test)

(def-suite gtk-drag-icon :in gtk-drag-and-drop)
(in-suite gtk-drag-icon)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkDragIcon

(test gtk-drag-icon-class
  ;; Check type
  (is (g:type-is-object "GtkDragIcon"))
  ;; Check registered name
  (is (eq 'gtk:drag-icon
          (glib:symbol-for-gtype "GtkDragIcon")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkDragIcon")
          (g:gtype (cffi:foreign-funcall "gtk_drag_icon_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkDragIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkDragIcon")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkRoot")
             (glib-test:list-interfaces "GtkDragIcon")))
  ;; Check properties
  (is (equal '("child")
             (glib-test:list-properties "GtkDragIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkDragIcon")))
  ;; Check CSS name
  (is (string= "dnd"
               (gtk:widget-class-css-name "GtkDragIcon")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkDragIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkDragIcon" GTK:DRAG-ICON
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkNative" "GtkRoot")
                       :TYPE-INITIALIZER "gtk_drag_icon_get_type")
                      ((CHILD DRAG-ICON-CHILD "child" "GtkWidget" T T)))
             (gobject:get-gtype-definition "GtkDragIcon"))))

;;; --- Properties -------------------------------------------------------------

;;;     child

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_icon_get_for_drag
;;;     gtk_drag_icon_set_from_paintable
;;;     gtk_drag_icon_create_widget_for_value

;;; 2024-7-3
