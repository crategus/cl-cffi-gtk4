(in-package :gtk-test)

(def-suite gdk-paintable-subclassing :in gdk-suite)
(in-suite gdk-paintable-subclassing)

;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; Subclass of the GObject class and the GdkPaintable interface

#+nil
(gobject:define-g-object-subclass "GdkNuclearIcon" nuclear-icon
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((rotation
    nuclear-icon-rotation
    "rotation" "gdouble" t t)))

;;; Define the virtual methodes for GdkNuclearIcon

#+nil
(defmethod paintable-get-flags-impl ((paintable nuclear-icon))
  (list :static-contents :static-size))

#+nil
(defmethod paintable-get-intrinsic-width-impl ((paintable nuclear-icon))
  36)

#+nil
(defmethod paintable-get-intrinsic-height-impl ((paintable nuclear-icon))
  12)

;; To get access to the symbols in the same file below.
#+nil
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'nuclear-icon)
  (export 'nuclear-icon-rotation))

;;; ----------------------------------------------------------------------------

(in-package :gtk-test)

#+nil
(test gdk-nuclear-icon-class
  ;; Check type
  (is (g:type-is-object "GdkNuclearIcon"))
  ;; Check registered name
  (is (eq 'gdk:nuclear-icon
          (glib:symbol-for-gtype "GdkNuclearIcon")))
  ;; We have no type initializer
;  (is (eq (g:gtype "GObject")
;          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkNuclearIcon")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkNuclearIcon")))
  ;; Check interfaces
  (is (equal '("GdkPaintable")
             (glib-test:list-interfaces "GdkNuclearIcon")))
  ;; Check properties
  (is (equal '("rotation")
             (glib-test:list-properties "GdkNuclearIcon")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkNuclearIcon")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkNuclearIcon" GDK-NUCLEAR-ICON
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GdkPaintable"))
                               ((ROTATION GDK-NUCLEAR-ICON-ROTATION "rotation"
                                 "gdouble" T T)))
             (gobject:get-g-type-definition "GdkNuclearIcon"))))

;; Some tests which demonstrate the functionality of the new Lisp subclass

;; Check initialization of a default value for the slot :rotation
#+nil
(test gdk-nuclear-icon-new.1
  (let ((icon (make-instance 'gdk:nuclear-icon)))
    (is (= 0.0d0 (gdk:nuclear-icon-rotation icon)))))

#+nil
(test gdk-nuclear-icon-new.2
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
    (is (= 3.0d0 (gdk:paintable-intrinsic-aspect-ratio icon)))))

#+nil
(test gdk-nuclear-icon-new.3
  (let ((icon (make-instance 'gdk:nuclear-icon
                             :rotation 90)))
    ;; Check g:object-property accessor
    (is (=  90 (g:object-property icon "rotation")))
    (is (= 180 (setf (g:object-property icon "rotation") 180)))
    (is (= 180 (g:object-property icon "rotation")))))

#+nil
(test gdk-nuclear-icon-new.4
  (let ((icon (g:object-new "GdkNuclearIcon" :rotation 90)))
    ;; Check g:object-property accessor
    (is (=  90 (g:object-property icon "rotation")))
    (is (= 180 (setf (g:object-property icon "rotation") 180)))
    (is (= 180 (g:object-property icon "rotation")))))

#+nil
(test gdk-nuclear-icon-notify-signal
  (let* ((message nil)
         (icon (make-instance 'gdk:nuclear-icon :rotation 90))
         (handler (g:signal-connect icon "notify::rotation"
                                    (lambda (object pspec)
                                      (declare (ignore object pspec))
                                      (setf message
                                            "Signal notify::rotation")))))
    (is (integerp handler))
    (is-false (g:object-notify icon "rotation"))
    (is (string= "Signal notify::rotation" message))))

;;; 2024-7-4
