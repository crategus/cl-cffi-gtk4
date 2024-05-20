;;;; Drawing Area
;;;;
;;;; GtkDrawingArea is a blank area where you can draw custom displays of
;;;; various kinds.
;;;;
;;;; The "scribble" area is a bit more advanced, and shows how to handle events
;;;; such as button presses and mouse motion. Click the mouse and drag in the
;;;; scribble area to draw squiggles. Resize the window to clear the area.
;;;;
;;;; Last version: 2024-5-16

(in-package :gtk4-example)

(let ((surface nil))

  ;; Create a new surface of the appropriate size to store our scribbles
  (defun create-surface (widget)
    (format t "create-surface : ~a~%" surface)
    (when surface
      (cairo:destroy surface))
    (setf surface
          (cairo:image-surface-create :argb32
                                      (gtk:widget-width widget)
                                      (gtk:widget-height widget)))
    ;; Initialize the surface to white
    (cairo:with-context (context surface)
      (cairo:set-source-rgb context 1.0 1.0 1.0)
      (cairo:paint context)))

  (defun draw-brush (widget x y)
    (format t "draw-brush : ~a ~a~%" x y)
    (when (or (not surface)
              (not (= (cairo:image-surface-width surface)
                      (gtk:widget-width widget)))
              (not (= (cairo:image-surface-height surface)
                      (gtk:widget-height widget))))
    (create-surface widget))
    (let ((rect (gdk:rectangle-new :x (- (truncate x) 3)
                                   :y (- (truncate y) 3)
                                   :width 6
                                   :height 6)))
     (cairo:with-context (context surface)
       (gdk:cairo-rectangle context rect)
       (cairo:fill context)))
    (gtk:widget-queue-draw widget))

  (defun do-drawing-area-scribble (&optional application)
    (let* ((area (make-instance 'gtk:drawing-area
                                :content-width 300
                                :content-height 300))
           (frame (make-instance 'gtk:frame
                                 :child area))
           (window (make-instance 'gtk:window
                                  :application application
                                  :child frame
                                  :title "Scribble"
                                  :default-width 250))
           (drag (make-instance 'gtk:gesture-drag
                                :button 0)))
      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (when surface
                            (cairo:surface-destroy surface))
                          (setf surface nil)))
      (g:signal-connect area "resize"
                        (lambda (widget width height)
                          (declare (ignore width height))
                          (create-surface widget)))
      (gtk:drawing-area-set-draw-func area
              ;; Redraw the screen from the surface
              (lambda (area cr height width)
                (declare (ignore area width height))
                (cairo:set-source-surface cr surface 0 0)
                (cairo:paint cr)))
      (gtk:widget-add-controller area drag)
      ;; Signal handlers for drag and drop
      (let ((xstart 0.0d0) (ystart 0.0d0))
        (g:signal-connect drag "drag-begin"
                          (lambda (gesture x y)
                            (declare (ignore gesture))
                            (setf xstart x)
                            (setf ystart y)
                            (draw-brush area x y)))
        (g:signal-connect drag "drag-update"
                          (lambda (gesture xoffset yoffset)
                            (declare (ignore gesture))
                            (draw-brush area (+ xstart xoffset)
                                             (+ ystart yoffset))))
        (g:signal-connect drag "drag-end"
                          (lambda (gesture xoffset yoffset)
                            (declare (ignore gesture))
                            (draw-brush area (+ xstart xoffset)
                                             (+ ystart yoffset)))))
      ;; Present window
      (gtk:window-present window))))
