(in-package :gtk-test)

(def-suite gdk-paintable-subclassing :in gdk-suite)
(in-suite gdk-paintable-subclassing)

;;; ----------------------------------------------------------------------------

(in-package :gdk)

#|
GdkPaintable #S(VTABLE-DESCRIPTION
                :TYPE-NAME GdkPaintable
                :CSTRUCT-NAME PAINTABLE-VTABLE
                :METHODS (#S(VTABLE-METHOD-INFO
                             :SLOT-NAME GET-FLAGS
                             :NAME PAINTABLE-GET-FLAGS-IMPL
                             :RETURN-TYPE PAINTABLE-FLAGS
                             :ARGS ((PAINTABLE (OBJECT PAINTABLE)))
                             :CALLBACK-NAME PAINTABLE-GET-FLAGS-CALLBACK
                             :IMPL-CALL NIL)
                          #S(VTABLE-METHOD-INFO
                             :SLOT-NAME GET-INTRINSIC-HEIGHT
                             :NAME PAINTABLE-GET-INTRINSIC-HEIGHT-IMPL
                             :RETURN-TYPE INT
                             :ARGS ((PAINTABLE (OBJECT PAINTABLE)))
                             :CALLBACK-NAME PAINTABLE-GET-INTRINSIC-HEIGHT-CALLBACK
                             :IMPL-CALL NIL)))

in GLIB-DEFCALLBACK (DEFCALLBACK PAINTABLE-GET-FLAGS-CALLBACK
                        PAINTABLE-FLAGS
                        ((PAINTABLE (OBJECT PAINTABLE)))
                      (RESTART-CASE (PROGN
                                     (FORMAT T ~&in IMPL-CALL~%)
                                     (PAINTABLE-GET-FLAGS-IMPL PAINTABLE))
                        (RETURN-FROM-INTERFACE-METHOD-IMPLEMENTATION (V)
                         INTERACTIVE (LAMBDA () (LIST (EVAL (READ)))) V)))

in GLIB-DEFCALLBACK (DEFCALLBACK PAINTABLE-GET-INTRINSIC-HEIGHT-CALLBACK
                        INT
                        ((PAINTABLE (OBJECT PAINTABLE)))
                      (RESTART-CASE (PAINTABLE-GET-INTRINSIC-HEIGHT-IMPL
                                     PAINTABLE)
                        (RETURN-FROM-INTERFACE-METHOD-IMPLEMENTATION (V)
                         INTERACTIVE (LAMBDA () (LIST (EVAL (READ)))) V)))
|#

#+nil
(gobject::define-vtable ("GdkPaintable" paintable)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the interface
  (:skip snapshot :pointer)
  (:skip get-current-image :pointer)
  (get-flags (paintable-flags (paintable g:object))
             :impl-call
             ((paintable)
              (paintable-get-flags-impl paintable)))
  (get-intrinsic-width (:int (paintable (g:object paintable))))
  (get-intrinsic-height (:int (paintable g:object)))
  (:skip get-intrinsic-aspect-ratio :pointer))

(defclass nuclear-icon (paintable)
  ((rotation :initform 0.0d0
             :accessor nuclear-icon-rotation))
  (:gname . "GdkNuclearIcon")
  (:metaclass gobject:gobject-class))

(gobject::register-object-type-implementation "GdkNuclearIcon"  ; name
                                              nuclear-icon      ; class
                                              "GObject"         ; parent
                                              ("GdkPaintable")  ; interfaces
                                              nil)              ; properties

(defmethod initialize-instance :after ((obj nuclear-icon) &rest initargs)
  ;; Set the slot values from initargs
  (iter (for (slot value) on initargs by #'cddr)
        (cond ((eq slot :rotation)
               (setf (nuclear-icon-rotation obj) value)))))

(defmethod paintable-get-flags-impl ((paintable nuclear-icon))
  (list :static-contents :static-size))

(defmethod paintable-get-intrinsic-width-impl ((paintable nuclear-icon))
  36)

(defmethod paintable-get-intrinsic-height-impl ((paintable nuclear-icon))
  12)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'nuclear-icon)
  (export 'nuclear-icon-rotation))

;;; ----------------------------------------------------------------------------

(in-package :gtk-test)

(test gdk-nuclear-icon-class
  ;; Type check
  (is (g:type-is-object "GdkNuclearIcon"))
  ;; Check the registered name
  (is (eq 'gdk:nuclear-icon
          (glib:symbol-for-gtype "GdkNuclearIcon")))
  ;; Check the type initializer
;  (is (eq (g:gtype "GObject")
;          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkNuclearIcon")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkNuclearIcon")))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (list-interfaces "GdkNuclearIcon")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GdkNuclearIcon")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkNuclearIcon")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkNuclearIcon" GDK-NUCLEAR-ICON
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GdkPaintable"))
                               NIL)
             (gobject:get-g-type-definition "GdkNuclearIcon"))))

(test gdk-nuclear-icon-new
  (let ((icon (make-instance 'gdk:nuclear-icon
                             :rotation 90)))
    ;; Check initialization and the accessor
    (is (=  90 (gdk:nuclear-icon-rotation icon)))
    (is (= 180 (setf (gdk:nuclear-icon-rotation icon) 180)))
    (is (= 180 (gdk:nuclear-icon-rotation icon)))
    ;; Call the virtual functions
    (is (equal '(:static-size :static-contents) (gdk:paintable-flags icon)))
    (is (= 36 (gdk:paintable-intrinsic-width icon)))
    (is (= 12 (gdk:paintable-intrinsic-height icon)))
    (is (= 3.0d0 (gdk:paintable-intrinsic-aspect-ratio icon)))
))

;;; --- 2023-10-28 -------------------------------------------------------------
