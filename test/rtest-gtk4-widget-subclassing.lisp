(in-package :gtk-test)

(def-suite gtk-widget-subclassing :in gtk-suite)
(in-suite gtk-widget-subclassing)

(in-package :gtk)

(eval-when (:compile-toplevel :load-toplevel :execute)
(gobject:define-vtable ("GtkWidget" widget)
  (:skip parent-instance (:struct g:type-class))
  ;; Methods for the GtkWidget class
  (show (:void (widget (g:object widget))))
  (hide (:void (widget (g:object widget))))
  (map (:void (widget (g:object widget))))

))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'widget-show-impl)
  (export 'widget-hide-impl)
  (export 'widget-map-impl))

;;; ----------------------------------------------------------------------------

;; Define default methods for the virtual functions of the interface.
;; The default functions of the C library are not available, therefor we
;; define the equivalent functionality in Lisp.

(in-package :gtk-test)

(gobject:define-g-object-subclass "CanvasItem" canvas-item
  (:superclass gtk:widget
   :export t
   :interfaces ())
  nil)

;;; --- gtk:widget-show-impl ---------------------------------------------------

(defmethod gtk:widget-show-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-SHOW-IMPL"
         (g:type-name (g:type-from-instance widget))))

;;; --- gtk:widget-hide-impl ---------------------------------------------------

(defmethod gtk:widget-hide-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-HIDE-IMPL"
         (g:type-name (g:type-from-instance widget))))

;;; --- gtk:widget-map-impl ----------------------------------------------------

(defmethod gtk:widget-map-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-MAP-IMPL"
         (g:type-name (g:type-from-instance widget))))

