(in-package :gtk4-example)

(defun put-pixel (pixbuf x y red green blue alpha)
  (let ((n-channels (gdk:pixbuf-n-channels pixbuf))
        (rowstride (gdk:pixbuf-rowstride pixbuf))
        (pixels (gdk:pixbuf-pixels pixbuf)))
    ;; Add offset to the pointer pixels into the pixbuf
    (cffi:incf-pointer pixels
                       (+ (* (round y) rowstride) (* (round x) n-channels)))
    ;; Set the color of the point and the alpha value
    (setf (cffi:mem-aref pixels :uchar 0) red)
    (setf (cffi:mem-aref pixels :uchar 1) green)
    (setf (cffi:mem-aref pixels :uchar 2) blue)
    (setf (cffi:mem-aref pixels :uchar 3) alpha)))

#+nil
(defun put-random-pixels (pixbuf x y)
  (let ((width (gdk:pixbuf-width pixbuf))
        (height (gdk:pixbuf-height pixbuf)))

  (dotimes (count 500)

    (let ((xp (+ x (- (random 40) 20)))
          (yp (+ y (- (random 40) 20))))

    (when (< xp 0) (setf xp 0))
    (when (>= xp width) (setf xp width))
    (when (< yp 0) (setf yp 0))
    (when (>= yp height) (setf yp height))

    (put-pixel pixbuf xp yp 255 0 0 255)

  ))))

#+nil
(defun create-image ()
  (let* ((path (glib-sys:sys-path "resource/ducky.png"))
         (image (gtk:image-new-from-file path)))

    ;; Connect a signal to the event box
    (g-signal-connect event-box "button-press-event"
                      (lambda (box event)
                        ;; Print infos about the event
                        (format t "~&in Signal Handler BUTTON-PRESS-EVENT~%")
                        (format t "~a~%" (gtk-current-event))
                        (format t "~a~%" (gtk-current-event-time))
                        (format t "~a~%" (gtk-current-event-state))
                        (format t "~a~%" (gtk-current-event-device))
                        ;; Print position of mouse click
                        (format t "Event box clicked at : ~6,2f, ~6,2f~%"
                                  (gdk-event-button-x event)
                                  (gdk-event-button-y event))

                        (put-random-pixels (gtk-image-pixbuf image)
                                           (gdk-event-button-x event)
                                           (gdk-event-button-y event))

                        (gdk-window-invalidate-rect (gtk-widget-window box) nil t)

                        +gdk-event-stop+))
    ;; Add the image to the event box
    (gtk-container-add event-box image)
    ;; Return the event box with the image
    event-box))

#+nil
(defun do-scrolled-window (&optional (application nil))
  (let* ((path (glib-sys:sys-path "resource/ducky.png"))
         (picture (gtk:picture-new-for-filename path))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child picture
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :always))
         (window (make-instance 'gtk:window
                                :title "Example Scrolled Window"
                                :child scrolled
                                :application application
                                :width-request 350
                                :height-request 300)))
      ;; Make the picture unshrinkable.
      (setf (gtk:picture-can-shrink picture) nil)
      ;; Show the window with the scrolled and the picture
      (gtk:window-present window)))


(defun do-put-pixel (&optional application)
  (let* ((path (glib-sys:sys-path "resource/ducky.png"))
         (picture (gtk:picture-new-for-filename path))
         (window (make-instance 'gtk:window
                                :title "Gdk Pixbuf Put Pixel"
                                :child picture
                                :application application))
         (gesture (gtk:gesture-click-new)))

    (format t " DO-PUT-PIXEL~%")
    (format t "    : ~a~%" picture)
    (format t "    : ~a~%" (gtk:picture-paintable picture))
    (format t "    : ~a~%" (gdk:texture-format (gtk:picture-paintable picture)))

    (gtk:widget-add-controller picture gesture)

    (g:signal-connect gesture "pressed"
        (lambda (gesture n x y)
          (declare (ignore gesture n))
          (format t "Gesture clicked~%")
          (format t "  x : ~a ~a~%" x y)))

    (gtk:window-present window)))
