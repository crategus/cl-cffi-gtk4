;;;; Image Widgets
;;;;
;;;; <tt>GtkImage</tt> is used to display an image. The image can be in a number
;;;; of formats. Typically, you load an image into a GdkPixbuf, then display the
;;;; pixbuf.
;;;;
;;;; This demo code shows some of the more obscure cases, in the simple case a
;;;; call to the <tt>gtk:image-new-from-file</tt> is all you need.
;;;;
;;;; 2025-3-2

(in-package :gtk4-example)

;; TODO: Improve this implementation, do not use a global
(defvar *animation* nil)

;;; ----------------------------------------------------------------------------

(gobject:define-gobject-subclass "PixbufPaintable" pixbuf-paintable
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((resource
    pixbuf-paintable-resource
    "resource" "gchararray" t t)
   (animation
    pixbuf-paintable-animation
    "animation" "GdkPixbufAnimation" t t)
   (iter
    pixbuf-paintable-iter
    "iter" "GdkPixbufAnimationIter" t t)
   (timeout
    pixbuf-paintable-timeout
    "timeout" "guint64" t t)))

;; Here, we implement the functionality required by the GdkPaintable interface
(defmethod gdk:paintable-snapshot-impl
           ((paintable pixbuf-paintable) snapshot width height)
  (let ((iter (pixbuf-paintable-iter paintable)))
    (gdk:pixbuf-animation-iter-advance iter 0)
    (let* ((pixbuf (gdk:pixbuf-animation-iter-pixbuf iter))
           (texture (gdk:texture-new-for-pixbuf pixbuf)))
      (gdk:paintable-snapshot texture snapshot width height))))

(defmethod gdk:paintable-get-flags-impl ((paintable pixbuf-paintable))
  (list :static-size))

(defmethod gdk:paintable-get-intrinsic-width-impl
           ((paintable pixbuf-paintable))
  (let ((pixbuf (pixbuf-paintable-animation paintable)))
    (gdk:pixbuf-animation-width pixbuf)))

(defmethod gdk:paintable-get-intrinsic-height-impl
           ((paintable pixbuf-paintable))
  (let ((pixbuf (pixbuf-paintable-animation paintable)))
    (gdk:pixbuf-animation-height pixbuf)))

(defun delay-cb (paintable)
  (let* ((iter (pixbuf-paintable-iter paintable))
         (delay (gdk:pixbuf-animation-iter-delay-time iter))
         (timeout (pixbuf-paintable-timeout paintable)))
    (when timeout
      (g:source-remove timeout))
    (setf (pixbuf-paintable-timeout paintable)
          (g:timeout-add delay (lambda () (delay-cb paintable))))
    (gdk:paintable-invalidate-contents paintable)
    g:+source-remove+))

(defun pixbuf-paintable-new (resource)
  (let ((paintable (make-instance 'pixbuf-paintable))
        (pixbuf (gdk:pixbuf-animation-new-from-resource resource)))

    (setf *animation* paintable)

    (format t "   PIXBUF loaded from resource : ~a x ~a~%"
              (gdk:pixbuf-animation-width pixbuf)
              (gdk:pixbuf-animation-height pixbuf))

    (setf (pixbuf-paintable-resource paintable) resource)
    (setf (pixbuf-paintable-animation paintable) pixbuf)
    (setf (pixbuf-paintable-iter paintable)
          (gdk:pixbuf-animation-iter pixbuf 0))

    (let* ((iter (pixbuf-paintable-iter paintable))
           (delay (gdk:pixbuf-animation-iter-delay-time iter)))

      (setf (pixbuf-paintable-timeout paintable)
            (g:timeout-add delay (lambda () (delay-cb paintable))))
      (gdk:paintable-invalidate-contents paintable))

      paintable))

;;; ----------------------------------------------------------------------------

;; TODO: Improve the implementation to avoid this globals
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
                 (let ((texture (gdk:texture-new-for-pixbuf pixbuf)))
                   (setf (gtk:picture-paintable picture) texture)))))

          (g:signal-connect pixbuf-loader "area-updated"
             (lambda (loader x y width height)
               (declare (ignore x y width height))
               ;; We know the pixbuf inside the GtkImage has changed, but the
               ;; image itself does not know this. So give it a hint by setting
               ;; the pixbuf again. Queuing a redraw used to be sufficient, but
               ;; nowadays GtkImage uses GtkIconHelper which caches the pixbuf
               ;; state and will just redraw from the cache.
               (let* ((pixbuf (gdk-pixbuf:pixbuf-loader-pixbuf loader))
                      (texture (gdk:texture-new-for-pixbuf pixbuf)))
                 (setf (gtk:picture-paintable picture) nil)
                 (setf (gtk:picture-paintable picture) texture))))))
    ;; Continue the GSource
    g:+source-continue+)

  (defun do-images (&optional application)
    (let* ((timeout nil)
           (vbox (make-instance 'gtk:box
                                :orientation :vertical
                                :valign :start
                                :spacing 6))
           (window (make-instance 'gtk:window
                                  :child vbox
                                  :application application
                                  :title "Various Image Widgets"
                                  :default-width 400
                                  :default-height 700)))

      (g:signal-connect window "close-request"
                        (lambda (widget)
                          (declare (ignore widget))
                          ;; Destroy source for the animation
                          ;; TODO: Improve the implementation
                          (let ((id (pixbuf-paintable-timeout *animation*)))
                            (when id
                              (g:source-remove id)))
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

      ;; Image from a filename
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Image from a file</b>"))
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

      ;; Animation from a pixbuf
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :margin-bottom 6
                                   :use-markup t
                                   :label
                                   "<b>Animation from a pixbuf</b>"))
             (paintable (pixbuf-paintable-new "/images/floppybuddy.gif"))
             (picture (gtk:picture-new-for-paintable paintable)))
        (setf (gtk:widget-halign picture) :center)
        (setf (gtk:widget-valign picture) :center)
        (gtk:box-append vbox label)
        (gtk:box-append vbox picture))

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

      ;; Progressive loading
      (let* ((label (make-instance 'gtk:label
                                   :margin-top 9
                                   :justify :center
                                   :use-markup t
                                   :label
                                   (format nil
                                           "<b>Progressive image loading</b>~
                                           ~%Click Image to repeat loading")))
             ;; Create an empty image for now. The progressive loader will
             ;; create the pixbuf and fill it in
             (picture (gtk:picture-new))
             (frame (make-instance 'gtk:frame
                                   :child picture
                                   :width-request 340
                                   :height-request 220))
             (gesture (make-instance 'gtk:gesture-click
                                     :propagation-phase :target)))
        ;; An event controller for the frame
        (gtk:widget-add-controller picture gesture)
        (g:signal-connect gesture "pressed"
            (lambda (gesture n-press x y)
              (declare (ignore n-press))
              (format t "Button pressed event at (~a, ~a)~%" x y)
              (setf (gtk:picture-paintable picture) nil)
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
        (gtk:box-append vbox label)
        (gtk:box-append vbox frame))
      ;; Present the window
      (gtk:window-present window))))
