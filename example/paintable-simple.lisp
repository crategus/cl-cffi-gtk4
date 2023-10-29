;;;; Simple Paintable
;;;;
;;;; GdkPaintable is an interface used by GTK for drawings of any sort that do
;;;; not require layouting or positioning.
;;;;
;;;; This demo code gives a simple example on how a paintable can be created.
;;;; Paintables can be used in many places inside GTK widgets, but the most
;;;; common usage is inside GtkImage and that's what we're going to do here.
;;;;
;;;; 2023-10-29

(in-package :gdk)

(defclass nuclear-icon (paintable)
  ((rotation :initform 0.0d0
             :accessor nuclear-icon-rotation))
  (:gname . "GdkNuclearIcon")
  (:metaclass gobject:gobject-class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'nuclear-icon))

(gobject:register-object-type-implementation "GdkNuclearIcon"  ; name
                                             nuclear-icon      ; class
                                             "GObject"         ; parent
                                             ("GdkPaintable")  ; interfaces
                                             nil)              ; properties

(defmethod initialize-instance :after ((obj nuclear-icon) &rest initargs)
  ;; Set the slot values from initargs
  (iter (for (slot value) on initargs by #'cddr)
        (cond ((eq slot :rotation)
               (setf (nuclear-icon-rotation obj) value)))))

;; This is the function that draws the actual icon. We make it a custom function
;; and define it in the paintable.h header so that it can be called from all the
;; other demos, too.
(defun nuclear-snapshot (snapshot foreground background width height rotation)
  (let ((size (min width height))
        (radius 0.3)
        (cr nil))
    (graphene:with-graphene-rects ((rect1 0 0 width height)
                                   (rect2 (/ (- width size) 2.0)
                                          (/ (- height size) 2.0)
                                          size size))
      (gtk:snapshot-append-color snapshot background rect1)
      (setf cr (gtk:snapshot-append-cairo snapshot rect2))
      (cairo-set-source-rgba cr foreground)
      (cairo:translate cr (/ width 2.0) (/ height 2.0))
      (cairo:scale cr size size)
      (cairo:rotate cr rotation)

      (cairo:arc cr 0 0 0.1 (- pi) pi)
      (cairo:fill cr)

      (setf (cairo:line-width cr) radius)
      (setf (cairo:dash cr 0.0) (list (/ (* radius pi) 3)))
      (cairo:arc cr 0 0 radius (- pi) pi)
      (cairo:stroke cr)
      (cairo:destroy cr))))

;; Here, we implement the functionality required by the GdkPaintable interface
(defmethod paintable-snapshot-impl ((paintable nuclear-icon)
                                    snapshot width height)
  (nuclear-snapshot snapshot
                    (rgba-new :red 0 :green 0 :blue 0 :alpha 1)
                    (rgba-new :red 0.9 :green 0.75 :blue 0.15 :alpha 1)
                    width
                    height
                    (nuclear-icon-rotation paintable)))

(defmethod paintable-get-flags-impl ((paintable nuclear-icon))
  (list :static-contents :static-size))

;;; ----------------------------------------------------------------------------

(in-package :gtk4-example)

(defun do-paintable-simple (&optional application)
  (let* ((nuclear (make-instance 'gdk:nuclear-icon
                                 :rotation 90))
         (image (gtk:image-new-from-paintable nuclear))
         (window (make-instance 'gtk:window
                                :application application
                                :child image
                                :title "Nuclear Icon"
                                :default-width 300
                                :default-height 200)))
    (gtk:window-present window)))
