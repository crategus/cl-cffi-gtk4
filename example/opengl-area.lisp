;;;; Open GL area
;;;;
;;;;
;;;; 2026-03-06

(in-package :gtk4-example)

(defun realize (widget)
  (format t "in REALIZE for ~a~%" widget))
(defun unrealize (widget)
  (format t "in UNREALIZE for ~a~%" widget))

(defun render (area context)
  (format t "in RENDER for ~a and ~a~%" area context)

  (gl:clear-color 0.2 0.3 0.3 1.0)
  (gl:clear :color-buffer)
  (gl:with-primitives :triangles
    (gl:color 1 0 0)
    (gl:vertex 0 0 0)
    (gl:color 0 1 0)
    (gl:vertex 0.5 1 0)
    (gl:color 0 0 1)
    (gl:vertex 1 0 0))
; (gl:flush)
)

(defun do-opengl-area (&optional application)
  (let* ((area (make-instance 'gtk:gl-area
                              :hexpand t
                              :vexpand t))
         (window (make-instance 'gtk:window
                                :application application
                                :child area
                                :title "Open GL Area"
                                :default-width 400
                                :default-height 300)))

    ;; We need to initialize and free GL resouces, so we can use the
    ;; realize and unrealize signals on the widget
    (g:signal-connect area "realize" #'realize)
    (g:signal-connect area "unrealize" #'unrealize)
    ;; The main "draw" call for GtkGLArea
    (g:signal-connect area "render" #'render)

    ;; Present the window
    (gtk:window-present window)))
