;;;; Layout Manager Transition
;;;;
;;;; This demo shows a simple example of a custom layout manager and a widget
;;;; using it. The layout manager places the children of the widget in a grid
;;;; or a circle.
;;;;
;;;; The widget is animating the transition between the two layouts.
;;;;
;;;; Click to start the transition.
;;;;
;;;; 2025-09-22

(in-package :gtk4-example)

(gobject:define-gobject-subclass "DemoLayout" demo-layout
  (:superclass gtk:layout-manager
   :export t
   :interfaces ())
  ((position
    demo-layout-position
    "position" "gfloat" t t)
   (:cl
    pos
    :accessor demo-layout-pos
    :initform (make-array 16 :initial-element 0))))

(gobject:define-vtable ("DemoLayout" demo-layout)
  (:skip parent-instance (:struct gobject::object-class))
  ;; Install a virtual get-request-mode function
  (get-request-mode (gtk:size-request-mode
                     (manager (g:object gtk:layout-manager))
                     (widget (g:object gtk:widget))))
  ;; Install a virtual measure function
  (measure (:void (manager (g:object gtk:layout-manager))
                  (widget (g:object demo-child))
                  (orientation gtk:orientation)
                  (for-size :int)
                  (minimum (:pointer :int))
                  (natural (:pointer :int))
                  (minimum-baseline (:pointer :int))
                  (natural-baseline (:pointer :int))))
  ;; Install a virtual allocate function
  (allocate (:void (manager (g:object gtk:layout-manager))
                   (widget (g:object gtk:widget))
                   (width :int)
                   (height :int)
                   (baseline :int)))
  (:skip create-layout-child :pointer)
  (:skip root :pointer)
  (:skip unroot :pointer))

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (gobject::install-vtable "DemoLayout"))

(defmethod demo-layout-measure-impl
           ((layout demo-layout)
            widget orientation for-size
            minimum natural minimum-baseline natural-baseline)
  (declare (ignore for-size minimum-baseline natural-baseline))
  (let ((min-size 0) (nat-size 0))
    (iter (for child first (gtk:widget-first-child widget)
                     then (gtk:widget-next-sibling child))
          (while child)
          (when (gtk:widget-should-layout child)
            (multiple-value-bind (min nat min-baseline nat-baseline)
                (gtk:widget-measure child orientation -1)
              (declare (ignore min-baseline nat-baseline))
              (setf min-size (max min-size min))
              (setf nat-size (max nat-size nat)))))
    ;; Reserve enough space for arranging 16 children in a circle
    (setf (cffi:mem-ref minimum :int)
          (truncate (+ min-size (/ (* 16 min-size) pi))))
    (setf (cffi:mem-ref natural :int)
          (truncate (+ nat-size (/ (* 16 nat-size) pi))))))

(defmethod demo-layout-allocate-impl
           ((layout demo-layout) widget width height baseline)
  (declare (ignore baseline))
  (let ((t1 (demo-layout-position layout))
        (child-width 0)
        (child-height 0))
    (iter (for child first (gtk:widget-first-child widget)
                     then (gtk:widget-next-sibling child))
          (while child)
          (when (gtk:widget-should-layout child)
            (let ((min-size (gtk:widget-preferred-size child)))
              (setf child-width
                    (max child-width (gtk:requisition-width min-size)))
              (setf child-height
                    (max child-height (gtk:requisition-height min-size))))))
    (let ((x0 (/ width 2))
          (y0 (/ height 2))
          (r (/ (* 8 child-width) pi)))
      (iter (for child first (gtk:widget-first-child widget)
                       then (gtk:widget-next-sibling child))
            (for i from 0 to 15)
            (while child)
            (when (gtk:widget-should-layout child)
              (let* ((a (/ (* pi (aref (demo-layout-pos layout) i)) 8))
                     (min-size (gtk:widget-preferred-size child))
                     ;; The grid position of child
                     (gx (+ x0 (* (- (mod i 4) 2) child-width)))
                     (gy (+ y0 (* (- (truncate (/ i 4)) 2) child-height)))
                     ;; The circle position of child. Note that we
                     ;; are adjusting the position by half the child size
                     ;; to place the center of child on a centered circle.
                     ;; This assumes that the children don't use align flags
                     ;; or uneven margins that would shift the center.
                     (cx (+ x0
                            (* r (sin a))
                            (- (/ (gtk:requisition-width min-size) 2))))
                     (cy (+ y0
                            (* r (cos a))
                            (- (/ (gtk:requisition-height min-size) 2))))
                     ;; We interpolate between the two layouts according to
                     ;; the position value that has been set on the layout.
                     (x (round (+ (* t1 cx) (* (- 1 t1) gx))))
                     (y (round (+ (* t1 cy) (* (- 1 t1) gy))))
                     ;; Create the allocation
                     (allocation (gdk:rectangle-new :x x
                                                    :y y
                                                    :width child-width
                                                    :height child-height)))
                (gtk:widget-size-allocate child allocation -1)))))))

(defmethod demo-layout-get-request-mode-impl
           ((layout demo-layout) widget)
  (declare (ignore widget))
  :constant-size)

(defun demo-layout-init (layout)
  (dotimes (i 16)
    (setf (aref (demo-layout-pos layout) i) i)))

(defun demo-layout-shuffle (layout)
  (dotimes (i 16)
    (let ((j (random (1+ i)))
          (tmp (aref (demo-layout-pos layout) i)))
      (setf (aref (demo-layout-pos layout) i) (aref (demo-layout-pos layout) j))
      (setf (aref (demo-layout-pos layout) j) tmp))))

;;; ----------------------------------------------------------------------------

;; This is a trivial child widget just for demo purposes.
;; It draws a 32 x 32 square in fixed color.
(gobject:define-gobject-subclass "DemoChild" demo-child
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color
    demo-child-color
    "color" "GdkRGBA" t t)))

(gobject:define-vtable ("DemoChild" demo-child)
  (:skip parent-instance (:struct gobject::object-class))
  ;; Methods for the GtkWidget class
  (:skip show :pointer)
  (:skip hide :pointer)
  (:skip map :pointer)
  (:skip unmap :pointer)
  (:skip realize :pointer)
  (:skip unrealize :pointer)
  (:skip root :pointer)
  (:skip unroot :pointer)
  (:skip size-allocate :pointer)
  (:skip state-flags-changed :pointer)
  (:skip direction-changed :pointer)
  (:skip get-request-mode :pointer)
  ;; Install a virtual measure function
  (measure (:void (widget (g:object demo-child))
                  (orientation gtk:orientation)
                  (for-size :int)
                  (minimum (:pointer :int))
                  (natural (:pointer :int))
                  (minimum-baseline (:pointer :int))
                  (natural-baseline (:pointer :int))))
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
  ;; Install new virtual snapshot function
  (snapshot (:void (widget (g:object demo-child))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (gobject::install-vtable "DemoChild"))

(defmethod demo-child-snapshot-impl ((widget demo-child) snapshot)
  (let ((width (gtk:widget-width widget))
        (height (gtk:widget-height widget))
        (color (demo-child-color widget)))
    (when snapshot
      (graphene:with-rect (bounds 0 0 width height)
        (gtk:snapshot-append-color snapshot color bounds)))))

(defmethod demo-child-measure-impl ((widget demo-child)
                                    orientation
                                    for-size
                                    minimum
                                    natural
                                    minimum-baseline
                                    natural-baseline)
  (declare (ignore widget orientation for-size minimum-baseline natural-baseline))
  (setf (cffi:mem-ref minimum :int) 32)
  (setf (cffi:mem-ref natural :int) 32))

(defun demo-child-new (color)
  (make-instance 'demo-child
                 :tooltip-text color
                 :color (gdk:rgba-parse color)))

;;; ----------------------------------------------------------------------------

;; The widget is controlling the transition by calling the
;; DEMO-LAYOUT-POSITION function in a tick callback.

;; We take two seconds to go from one layout to the other
(defvar duration (* 2 internal-time-units-per-second))

(gobject:define-gobject-subclass "DemoWidget" demo-widget
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((backward                       ; whether we go 0 -> 1 or 1 -> 0
    demo-widget-backward
    "backward" "gboolean" t t)
   (start                          ; time the transition started
    demo-widget-start
    "start" "guint64" t t)
   (tickid                         ; tick callback
    demo-widget-tickid
    "tickid" "guint" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'demo-widget))) class data)
  (declare (ignorable subclass class data))
  (setf (gtk:widget-class-layout-manager-type "DemoWidget") "DemoLayout"))

(defun demo-widget-new ()
  (let ((widget (make-instance 'demo-widget)))
    (demo-layout-init (gtk:widget-layout-manager widget))
    widget))

(defun transition (widget clock)
  (let ((layout (gtk:widget-layout-manager widget))
        (now (gdk:frame-clock-frame-time clock)))
    (gtk:widget-queue-allocate widget)
    (if (demo-widget-backward widget)
        (setf (demo-layout-position layout)
              (- 1.0 (/ (- now (demo-widget-start widget)) duration)))
        (setf (demo-layout-position layout)
              (/ (- now (demo-widget-start widget)) duration)))
    (if (> (- now (demo-widget-start widget)) duration)
        (progn
          (setf (demo-widget-backward widget)
                (not (demo-widget-backward widget)))
          (setf (demo-layout-position layout)
                (if (demo-widget-backward widget)
                    1.0
                    0.0))
          (when (not (demo-widget-backward widget))
            (demo-layout-shuffle layout))
          (setf (demo-widget-tickid widget) 0)
          g:+source-remove+)
        g:+source-continue+)))

(defun clicked (widget)
  (when (= (demo-widget-tickid widget) 0)
    (let ((clock (gtk:widget-frame-clock widget)))
      (setf (demo-widget-start widget)
            (gdk:frame-clock-frame-time clock))
      (setf (demo-widget-tickid widget)
            (gtk:widget-add-tick-callback widget
                                          (lambda (widget clock)
                                            (transition widget clock)))))))

(defmethod initialize-instance :after ((obj demo-widget) &key)
  (let ((gesture (gtk:gesture-click-new)))
    (g:signal-connect gesture "pressed"
                      (lambda (gesture npress x y)
                        (declare (ignore gesture npress x y))
                        (clicked obj)))
    (gtk:widget-add-controller obj gesture)))

(defun demo-widget-add-child (widget child)
  (gtk:widget-set-parent child widget))

;;; ----------------------------------------------------------------------------

(defun do-layout-manager-transition (&optional application)
  (let*  ((colors (list "red" "orange" "yellow" "green"
                        "blue" "grey" "magenta" "lime"
                        "yellow" "firebrick" "aqua" "purple"
                        "tomato" "pink" "thistle" "maroon"))
          (vbox (make-instance 'gtk:box
                               :orientation :vertical
                               :margin-top 6
                               :margin-start 6
                               :margin-end 6
                               :spacing 24))
          (hbox (make-instance 'gtk:box
                               :orientation :horizontal))
          (widget (demo-widget-new))
          (window (make-instance 'gtk:window
                                 :application application
                                 :title "Layout Manger Transition"
                                 :child vbox
                                 :default-width 520
                                 :default-height 360)))
    ;; Create a horizontal box with colors
    (dolist (color colors)
      (gtk:box-append hbox (demo-child-new color)))
    ;; Create demo widget and fill it with children
    (dolist (color colors)
      (demo-widget-add-child widget
                             (make-instance 'demo-child
                                            :tooltip-text color
                                            :color (gdk:rgba-parse color)
                                            :margin-start 3
                                            :margin-end 3
                                            :margin-top 3
                                            :margin-bottom 3)))
    ;; Pack boxes and present window
    (gtk:box-append vbox hbox)
    (gtk:box-append vbox widget)
    (gtk:window-present window)))
