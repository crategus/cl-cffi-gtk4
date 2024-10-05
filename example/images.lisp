;;;; Image Widgets
;;;;
;;;; <tt>GtkImage</tt> is used to display an image. The image can be in a number
;;;; of formats. Typically, you load an image into a GdkPixbuf, then display the
;;;; pixbuf.
;;;;
;;;; This demo code shows some of the more obscure cases, in the simple case a
;;;; call to the <tt>gtk:image-new-from-file</tt> is all you need.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(let ((pixbuf-loader nil)
      (image-stream nil))

  (defun progressive-timeout (picture)
    (if image-stream
        (let* ((buffer (make-array 128 :element-type '(unsigned-byte 8)))
               (len (read-sequence buffer image-stream)))
          (if (= 0 len)
              ;; We have reached the end of the file.
              (progn
                (close image-stream)
                (setf image-stream nil)
                (gdk-pixbuf:pixbuf-loader-close pixbuf-loader)
                (setf pixbuf-loader nil)
                (return-from progressive-timeout g:+source-remove+))
              ;; Load the buffer into GdkPixbufLoader
              (progn
                (gdk-pixbuf:pixbuf-loader-write pixbuf-loader buffer 128))))
        (progn
          ;; Create the image stream and the GdkPixbufLoader
          (setf image-stream
                (open (glib-sys:sys-path "resource/alphatest.png")
                      :element-type '(unsigned-byte 8)))
          (when pixbuf-loader
            (gdk-pixbuf:pixbuf-loader-close pixbuf-loader)
            (setf pixbuf-loader nil))
          (setf pixbuf-loader (gdk-pixbuf:pixbuf-loader-new))

          (g:signal-connect pixbuf-loader "area-prepared"
             (lambda (loader)
               (let ((pixbuf (gdk-pixbuf:pixbuf-loader-pixbuf loader)))
                 (gdk-pixbuf:pixbuf-fill pixbuf #xaaaaaaff)
                 (gtk:picture-set-pixbuf picture pixbuf))))

          (g:signal-connect pixbuf-loader "area-updated"
             (lambda (loader x y width height)
               (declare (ignore x y width height))
               ;; We know the pixbuf inside the GtkImage has changed, but the
               ;; image itself does not know this. So give it a hint by setting
               ;; the pixbuf again. Queuing a redraw used to be sufficient, but
               ;; nowadays GtkImage uses GtkIconHelper which caches the pixbuf
               ;; state and will just redraw from the cache.
               (let ((pixbuf (gdk-pixbuf:pixbuf-loader-pixbuf loader)))
                 (gtk:picture-set-pixbuf picture nil)
                 (gtk:picture-set-pixbuf picture pixbuf))))))
    ;; Continue the GSource
    g:+source-continue+)

  (defun do-images (&optional application)
    (let* ((timeout nil)
           (vbox (make-instance 'gtk:box
                                :orientation :vertical
                                :spacing 6))
           (window (make-instance 'gtk:window
                                  :child vbox
                                  :application application
                                  :title "Various Image Widgets"
                                  :default-width 320)))

      (g:signal-connect window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Destroy the timeout source
                          (when timeout
                            (g:source-remove timeout)
                            (setf timeout nil))
                          ;; Close the GdkPixbufLoader object
                          (when pixbuf-loader
                            (gdk-pixbuf:pixbuf-loader-close pixbuf-loader)
                            (setf pixbuf-loader nil))
                          ;; Close open input stream
                          (when image-stream
                            (close image-stream)
                            (setf image-stream nil))))

      ;; Image new from a filename
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Image new from a filename</b>"))
             (image (gtk:image-new-from-file
                        (glib-sys:sys-path "resource/gtk-logo.png"))))
        (setf (gtk:image-icon-size image) :large)
        (gtk:box-append vbox label)
        (gtk:box-append vbox image))

      ;; Image new from a symbolic icon
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Symbolic themed icon</b>"))
             (gicon (g:themed-icon-new-with-default-fallbacks
                        "battery-caution-charging-symbolic"))
             (image (gtk:image-new-from-gicon gicon)))
        (setf (gtk:image-icon-size image) :large)
        (gtk:box-append vbox label)
        (gtk:box-append vbox image))

      ;; Animation new from a pixbuf
      ;; TODO: Does not work. We do not get the animation.
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Animation new from a pixbuf</b>"))
             (pixbuf (gdk:pixbuf-new-from-file
                         (glib-sys:sys-path "resource/spinner.gif")))
             (image (gtk:image-new-from-pixbuf pixbuf)))
        (setf (gtk:image-icon-size image) :large)
        (gtk:box-append vbox label)
        (gtk:box-append vbox image))

      ;; Progressive loading
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 18
                                   :justify :center
                                   :use-markup t
                                   :label
                                   (format nil
                                           "<b>Progressive image loading</b>~
                                           ~%Click Image to repeat loading")))
             ;; Create an empty image for now. The progressive loader will
             ;; create the pixbuf and fill it in.
             (picture (gtk:picture-new))
             (frame (make-instance 'gtk:frame
                                   :child picture
                                   :width-request 340
                                   :height-request 220))
             (gesture (make-instance 'gtk:gesture-click
                                     :propagation-phase :target))
        )
        ;; An event controller for the frame
        (gtk:widget-add-controller picture gesture)
        (g:signal-connect gesture "pressed"
            (lambda (gesture n-press x y)
              (declare (ignore n-press))
              (format t "Button pressed event at (~a, ~a)~%" x y)
              (gtk:picture-set-pixbuf picture nil)
              (setf timeout
                    (g:timeout-add 25
                                   (lambda () (progressive-timeout picture))))
              (gtk:gesture-set-state gesture :claimed)))

        ;; This is obviously totally contrived (we slow down loading on
        ;; purpose to show how incremental loading works). The real purpose
        ;; of incremental loading is the case where you are reading data from
        ;; a slow source such as the network. The timeout simply simulates a
        ;; slow data source by inserting pauses in the reading process.
        (setf timeout
              (g:timeout-add 25 (lambda () (progressive-timeout picture))))


#|
    // Assign a click listener
    let gesture = gtk::GestureClick::new();
    gesture.connect_released(|gesture, _, _, _| {
        gesture.set_state(gtk::EventSequenceState::Claimed);
        println!("Box pressed!");
    });
    gtk_box.add_controller(&gesture);
|#

#|
        ;; Restart loading the image from the file
        (g:signal-connect event-box "button-press-event"
           (lambda (widget event)
             (declare (ignore widget))
             (format t "Event Box clicked at (~,2f, ~,2f)~%"
                       (gdk-event-button-x event)
                       (gdk-event-button-y event))
             (gtk:image-clear image)
             (setf timeout
                   (g:timeout-add 100
                                  (lambda () (progressive-timeout image))))))
|#

        (gtk:box-append vbox label)
        (gtk:box-append vbox frame))

      (gtk:window-present window))))
