(in-package :gtk4-tutorial)

(defun print-gesture-info (gesture)
  (let ((name (gtk:event-controller-name gesture))
        (event (gtk:event-controller-current-event gesture))
        (device (gtk:event-controller-current-event-device gesture))
        (state (gtk:event-controller-current-event-state gesture))
        (time (gtk:event-controller-current-event-time gesture)))
    (format t " GtkEventController~%")
    (format t "   name : ~a~%" name)
    (format t "   event : ~a~%" event)
    (format t "   device : ~a~%" device)
    (format t "   state : ~a~%" state)
    (format t "   time : ~a~%" time))
  (let* ((event (gtk:event-controller-current-event gesture))
         (type (gdk:event-event-type event))
         (surface (gdk:event-surface event))
         (device (gdk:event-device event))
         (device-tool (gdk:event-device-tool event))
         (time (gdk:event-time event))
         (display (gdk:event-display event))
         (seat (gdk:event-seat event))
         (sequence (gdk:event-event-sequence event))
         (state (gdk:event-modifier-state event))
         (position (multiple-value-list (gdk:event-position event)))
         (emulated (gdk:event-pointer-emulated event))
         (triggers (gdk:event-triggers-context-menu event))
         (button (gdk:button-event-button event)))
    (format t " GdkEvent~%")
    (format t "    type : ~a~%" type)
    (format t "    surface : ~a~%" surface)
    (format t "    device : ~a~%" device)
    (format t "    device-tool : ~a~%" device-tool)
    (format t "    time : ~a~%" time)
    (format t "    display : ~a~%" display)
    (format t "    seat : ~a~%" seat)
    (format t "    sequence : ~a~%" sequence)
    (format t "    state : ~a~%" state)
    (format t "    position : ~a~%" position)
    (format t "    emulated : ~a~%" emulated)
    (format t "    triggers : ~a~%" triggers)
    (format t "    button : ~a~%" button)))

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
                                        :name "GESTURE DRAG Controller"
                                        :button 1))
                   (press (make-instance 'gtk:gesture-click
                                         :name "GESTURE CLICK Controller"
                                         :button 3)))
              (gtk:widget-add-controller area drag)
              (gtk:widget-add-controller area press)
              (g:signal-connect drag "drag-begin"
                  (lambda (gesture x y)
                    (print-gesture-info gesture)
                    (let ((cr (cairo:create surface)))
                      (setf xstart x)
                      (setf ystart y)
                      (cairo:rectangle cr (- xstart 3) (- ystart 3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect drag "drag-update"
                  (lambda (gesture x y)
                    (print-gesture-info gesture)
                    (let ((cr (cairo:create surface)))
                      (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect drag "drag-end"
                  (lambda (gesture x y)
                    (print-gesture-info gesture)
                    (let ((cr (cairo:create surface)))
                      (cairo:rectangle cr (+ xstart x -3) (+ ystart y -3) 6 6)
                      (cairo:fill cr)
                      (cairo:destroy cr)
                      (gtk:widget-queue-draw area))))
              (g:signal-connect press "pressed"
                  (lambda (gesture n x y)
                    (declare (ignore n x y))
                    (print-gesture-info gesture)
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
