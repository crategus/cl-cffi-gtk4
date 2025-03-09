;;;; Frame Clock
;;;;
;;;; This demo is intentionally as simple as possible, to see what framerate the
;;;; windowing system can deliver on its own.
;;;;
;;;; It changes the color for each frame and draws an increasing rectangle.
;;;;
;;;; The example also shows how to subclass the <tt>gtk:widget</tt> class
;;;; and the implementation of a virtual <tt>snapshot</tt> function for the
;;;; <tt>color-widget</tt> subclass.
;;;;
;;;; 2025-3-1

(in-package :gtk4-example)

(defconstant spantime (* 3 1000000))

(gobject:define-gobject-subclass "ColorWidget" color-widget
  (:superclass gtk:widget
   :export t
   :interfaces ())
  ((color1
    color-widget-color1
    "color1" "GdkRGBA" t t)
   (color2
    color-widget-color2
    "color2" "GdkRGBA" t t)
   (abstime
    color-widget-abstime
    "abstime" "guint64" t t)
   (fractime
    color-widget-fractime
    "fractime" "gfloat" t t)))

(gobject:define-vtable ("ColorWidget" color-widget)
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
  ;; Install new virtual snapshot function
  (snapshot (:void (widget (g:object color-widget))
                   (snapshot (g:object gtk:snapshot))))
  (:skip contains :pointer))

(defmethod initialize-instance :after ((obj color-widget) &key)
  (format t "in INITIAlIZE-INSTANCE for ~a~%" obj)
  (gobject::install-vtable "ColorWidget")
  (setf (gtk:widget-hexpand obj) t)
  (setf (gtk:widget-vexpand obj) t)
  (setf (color-widget-color1 obj) (gdk:rgba-new :alpha 1.0))
  (setf (color-widget-color2 obj) (gdk:rgba-new :alpha 1.0))
  (let ((id (gtk:widget-add-tick-callback obj
                (lambda (widget clock)
                  (let ((clocktime (gdk:frame-clock-frame-time clock))
                        (abstime (color-widget-abstime obj))
                        (color2 (color-widget-color2 obj)))
                    (when (>= clocktime abstime)
                      ;; Adjust new abstime
                      (setf (color-widget-abstime widget)
                            (+ clocktime spantime))
                      (setf abstime (color-widget-abstime widget))
                      ;; Copy color2 to color1
                      (setf (color-widget-color1 widget) (gdk:rgba-copy color2))
                      ;; Generate a new random color2
                      (setf (gdk:rgba-red color2) (random 1.0))
                      (setf (gdk:rgba-green color2) (random 1.0))
                      (setf (gdk:rgba-blue color2) (random 1.0))
                      (format t "TICK-CALLBACK : ~a~%" color2))
                    ;; Store new fractime
                    (setf (color-widget-fractime widget)
                          (- 1.0 (/ (- abstime clocktime) spantime)))
                    ;; Redraw widget
                    (gtk:widget-queue-draw widget)
                     glib:+source-continue+)))))
    (g:object-set-data-full obj "tick-cb"
                            (lambda ()
                              (gtk:widget-remove-tick-callback obj id)))))

(defmethod color-widget-snapshot-impl ((widget color-widget) snapshot)
  (let ((w (gtk:widget-width widget))
        (h (gtk:widget-height widget))
        (color (gdk:rgba-new :alpha 1.0))
        (color1 (color-widget-color1 widget))
        (color2 (color-widget-color2 widget))
        (fractime (color-widget-fractime widget)))
    ;; Color is a fraction between color1 and color2
    (setf (gdk:rgba-red color)
          (+ (* (- 1.0 fractime) (gdk:rgba-red color1))
             (* fractime (gdk:rgba-red color2))))
    (setf (gdk:rgba-green color)
          (+ (* (- 1.0 fractime) (gdk:rgba-green color1))
             (* fractime (gdk:rgba-green color2))))
    (setf (gdk:rgba-blue color)
          (+ (* (- 1.0 fractime) (gdk:rgba-blue color1))
             (* fractime (gdk:rgba-blue color2))))
    ;; Draw widget
    (let ((builder (gsk:path-builder-new)))
      (graphene:with-rect (bounds 0 0 w h)
        ;; Fill background with color
        (gtk:snapshot-append-color snapshot color bounds)
        ;; Draw increasing white rectangle
        (graphene:rect-init bounds 0 0 (* fractime w) (* fractime h))
        (gsk:path-builder-add-rect builder bounds)
        (gtk:snapshot-append-stroke snapshot
                                    (gsk:path-builder-to-path builder)
                                    (gsk:stroke-new 1)
                                    (gdk:rgba-new :red 1.0 :green 1.0 :blue 1.0
                                                  :alpha 1.0))))))

(defun do-frame-clock (&optional application)
  (let*  ((builder (gtk:builder-new-from-resource "/frames/frame-clock.ui"))
          (window (gtk:builder-object builder "window"))
          (label (gtk:builder-object builder "fps"))
          (box (gtk:builder-object builder "box")))

    (setf (gtk:window-application window) application)

    (let ((id (g:timeout-add 500
                      (lambda ()
                        (let ((clock (gtk:widget-frame-clock label)))
                          (if clock
                              (let* ((fps (gdk:frame-clock-fps clock))
                                     (str (format nil "~,2f fps" fps)))
                                (setf (gtk:label-label label) str))
                              (setf (gtk:label-label label) ""))
                          glib:+source-continue+)))))

      (g:object-set-data-full label "tick-cb"
              (lambda ()
                (format t "remove timeout callback for label~%")
                (g:source-remove id))))

    (gtk:box-append box (make-instance 'color-widget))

    (gtk:window-present window)))
