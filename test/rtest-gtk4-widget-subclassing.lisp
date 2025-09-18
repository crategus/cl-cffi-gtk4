(in-package :gtk-test)

(def-suite gtk-widget-subclassing :in gtk-suite)
(in-suite gtk-widget-subclassing)

(eval-when (:compile-toplevel :load-toplevel :execute)
(gobject:define-vtable ("CanvasItem" canvas-item)
  (:skip parent-instance (:struct gobject::object-class))
  ;; Methods for the GtkWidget class
  (show (:void (widget (g:object canvas-item))))
  (hide (:void (widget (g:object canvas-item))))
  (map (:void (widget (g:object canvas-item))))
  (:skip unmap :pointer)
  (:skip realize :pointer)
  (:skip unrealize :pointer)
  (:skip root :pointer)
  (:skip unroot :pointer)
  (:skip size-allocate :pointer)
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)
  (:skip measure :pointer)
  (:skip mnemonic-activate :pointer)
  (:skip grab-focus :pointer)
  (:skip focus :pointer)
  (:skip set-focus-child :pointer)
  (:skip move-focus :pointer)
  (:skip keynav-failed :pointer)
  (:skip query-tooltip :pointer)
  (:skip compute-expand :pointer)
  (:skip css-changed :pointer)
  (:skip system-settings-changed :pointer)

  (snapshot (:void (widget (g:object canvas-item))
                   (snapshot (g:object gtk:snapshot))))

  (:skip contains :pointer)
))

#+nil
(PROGN
 (CFFI:DEFCSTRUCT CANVAS-ITEM-VTABLE
   (PARENT-INSTANCE (:STRUCT GOBJECT::OBJECT-CLASS))
   (SHOW :POINTER)
   (HIDE :POINTER))
 (SETF (GOBJECT::GET-VTABLE-INFO "CanvasItem")
         (GOBJECT::MAKE-VTABLE-INFO :GTYPE-NAME "CanvasItem" :CSTRUCT-NAME
                                    'CANVAS-ITEM-VTABLE :METHODS
                                    (LIST
                                     (GOBJECT::MAKE-VTABLE-METHOD-INFO
                                      :SLOT-NAME 'SHOW :NAME
                                      'CANVAS-ITEM-SHOW-IMPL :RETURN-TYPE
                                      ':VOID :ARGS
                                      '((WIDGET (GOBJECT:OBJECT WIDGET)))
                                      :CALLBACK-NAME
                                      'CANVAS-ITEM-SHOW-CALLBACK)
                                     (GOBJECT::MAKE-VTABLE-METHOD-INFO
                                      :SLOT-NAME 'HIDE :NAME
                                      'CANVAS-ITEM-HIDE-IMPL :RETURN-TYPE
                                      ':VOID :ARGS
                                      '((WIDGET (GOBJECT:OBJECT WIDGET)))
                                      :CALLBACK-NAME
                                      'CANVAS-ITEM-HIDE-CALLBACK))))
 (CLOSER-MOP:DEFGENERIC CANVAS-ITEM-SHOW-IMPL
     (WIDGET))
 (GOBJECT::GLIB-DEFCALLBACK CANVAS-ITEM-SHOW-CALLBACK
     :VOID
     ((WIDGET (GOBJECT:OBJECT WIDGET)))
   (RESTART-CASE (CANVAS-ITEM-SHOW-IMPL WIDGET)
     (GOBJECT::RETURN-FROM-INTERFACE-METHOD-IMPLEMENTATION (GOBJECT::V)
      :INTERACTIVE (LAMBDA () (LIST (EVAL (READ)))) GOBJECT::V)))
 (CLOSER-MOP:DEFGENERIC CANVAS-ITEM-HIDE-IMPL
     (WIDGET))
 (GOBJECT::GLIB-DEFCALLBACK CANVAS-ITEM-HIDE-CALLBACK
     :VOID
     ((WIDGET (GOBJECT:OBJECT WIDGET)))
   (RESTART-CASE (CANVAS-ITEM-HIDE-IMPL WIDGET)
     (GOBJECT::RETURN-FROM-INTERFACE-METHOD-IMPLEMENTATION (GOBJECT::V)
      :INTERACTIVE (LAMBDA () (LIST (EVAL (READ)))) GOBJECT::V))))

#+nil
(defun install-vtable (gname)

    (let* ((class (g:type-class-ref gname))
           (vtable (get-vtable-info gname))
           (vtable-cstruct (when vtable (vtable-info-cstruct-name vtable))))

      (format t "         gname : ~a~%" gname)
      (format t "         class : ~a~%" class)
      (format t "        vtable : ~a~%" vtable)
      (format t "vtable-cstruct : ~a~%" vtable-cstruct)

      (when vtable
        (iter (for method in (vtable-info-methods vtable))
              (for cb = (cffi:get-callback
                            (vtable-method-info-callback-name method)))
              (for slot-name = (vtable-method-info-slot-name method))
              (setf (cffi:foreign-slot-value class
                                            `(:struct ,vtable-cstruct)
                                             slot-name)
                    cb)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'canvas-item-show-impl)
  (export 'canvas-item-hide-impl)
  (export 'canvas-item-snapshot-impl))
;  (export 'widget-map-impl))

;;; ----------------------------------------------------------------------------

;; Define default methods for the virtual functions of the interface.
;; The default functions of the C library are not available, therefor we
;; define the equivalent functionality in Lisp.

(in-package :gtk-test)

(gobject:define-gobject-subclass "CanvasItem" canvas-item
  (:superclass gtk:widget
   :export t
   :interfaces ())
  nil)

;;; --- gtk:widget-show-impl ---------------------------------------------------

(defmethod canvas-item-show-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-SHOW-IMPL"
         (g:type-name (g:type-from-instance widget))))

;;; --- gtk:widget-hide-impl ---------------------------------------------------

(defmethod canvas-item-hide-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-HIDE-IMPL"
         (g:type-name (g:type-from-instance widget))))

(defmethod canvas-item-map-impl ((widget canvas-item))
  (format t "in CANVAS-ITEM-HIDE-IMPL for ~a~%" widget)
  (gtk:widget-map widget))


(defmethod canvas-item-snapshot-impl ((widget canvas-item) snapshot)
  (declare (ignorable snapshot))
  (error "Widget of type ~a does not implement GTK:CANVAS-ITEM-SNAPSHOT-IMPL"
         (g:type-name (g:type-from-instance widget))))

;;; --- gtk:widget-map-impl ----------------------------------------------------

#+nil
(defmethod gtk:widget-map-impl ((widget canvas-item))
  (error "Widget of type ~a does not implement GTK:WIDGET-MAP-IMPL"
         (g:type-name (g:type-from-instance widget))))

;;; 2025-09-18

