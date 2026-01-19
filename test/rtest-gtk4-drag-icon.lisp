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

(test gtk-drag-icon-properties
  (glib-test:with-check-memory (icon)
    (is (typep (setf icon (make-instance 'gtk:drag-icon)) 'gtk:drag-icon))
    (is-false (gtk:drag-icon-child icon))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_drag_icon_get_for_drag
;;;     gtk_drag_icon_set_from_paintable

;;;     gtk_drag_icon_create_widget_for_value

(test gtk-drag-icon-create-widget-for-value
  (glib-test:with-check-memory (widget)
    (g:with-value (value "gchararray" "string")
      (is (typep (setf widget
                       (gtk:drag-icon-create-widget-for-value value)) 'gtk:label))
      (is (string= "string" (gtk:label-label widget))))))

;;; 2026-01-12
