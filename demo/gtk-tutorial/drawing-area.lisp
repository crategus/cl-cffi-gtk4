(in-package :gtk4-tutorial)

(defun drawing-area ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.crategus.drawing-area"
                            :flags :none))
        (surface nil) (xstart 0) (ystart 0))
    (g:signal-connect app "activate"
        (lambda (application)
            (let* ((area (make-instance 'gtk:drawing-area
                                        :width-request 200
                                        :height-request 300))
                   (frame (make-instance 'gtk:frame
                                         :child area))
                   (window (make-instance 'gtk:application-window
                                          :application application
                                          :child frame
                                          :title "Drawing Area"))
                   (drag (make-instance 'gtk:gesture-drag
                                        :button 1))
                   (press (make-instance 'gtk:gesture-click
                                         :button 3)))
              (gtk:widget-add-controller area drag)
              (gtk:widget-add-controller area press)
              (g:signal-connect drag "drag-begin"
                  (lambda (gesture x y)
                    (declare (ignore gesture))
                    (let ((cr (cairo:create surface)))
                      (setf xstart x)
                      (setf ystart y)
                      (cairo:rectangle cr (- xstart 3) (- ystart 3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect drag "drag-update"
                  (lambda (gesture x y)
                    (declare (ignore gesture))
                    (let ((cr (cairo:create surface)))
                      (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect drag "drag-end"
                  (lambda (gesture x y)
                    (declare (ignore gesture))
                    (let ((cr (cairo:create surface)))
                      (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect press "pressed"
                  (lambda (gesture n x y)
                    (declare (ignore gesture n x y))
                    (let ((cr (cairo:create surface)))
                      (cairo:set-source-rgb cr 1 1 1)
                      (cairo:paint cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (gtk:drawing-area-set-draw-func area
                  (lambda (widget cr width height)
                    (declare (ignore widget width height))
                    (cairo:set-source-surface cr surface 0 0)
                    (cairo:paint cr)))
              (g:signal-connect area "resize"
                  (lambda (area width height)
                    (declare (ignore width height))
                    (when surface
                      (cairo:surface-destroy surface)
                      (setf surface nil))
                    (when (gtk:native-surface (gtk:widget-native area))
                      (setf surface
                            (gdk:surface-create-similar-surface
                                (gtk:native-surface (gtk:widget-native area))
                                :color
                                (gtk:widget-width area)
                                (gtk:widget-height area)))
                      (let ((cr (cairo:create surface)))
                            (cairo:set-source-rgb cr 1 1 1)
                            (cairo:paint cr)
                            (cairo:destroy cr)))))
              (gtk:widget-show window))))
    (g:application-run app nil)))
