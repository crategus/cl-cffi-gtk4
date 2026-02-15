;;;; Video Player
;;;;
;;;; The <tt>gtk:video</tt> widget is a widget to show a
;;;; <tt>gtk:media-stream</tt> object with media controls.
;;;;
;;;; The controls are available separately as <tt>gtk:media-controls</tt>
;;;; widgets. If you just want to display a video without controls, you can
;;;; treat it like any other paintable and for example put it into a
;;;; <tt>gtk:picture</tt> widget.
;;;;
;;;; The <tt>gtk:video</tt> widget aims to cover use cases such as previews,
;;;; embedded animations, etc. It supports autoplay, looping, and simple media
;;;; controls. It does not have support for video overlays, multichannel audio,
;;;; device selection, or input. If you are writing a full-fledged video player,
;;;; you may want to use the <tt>gdk:paintable</tt> API and a media framework
;;;; such as <tt>Gstreamer</tt> directly.
;;;;
;;;; 2026-02-14

(in-package :gtk4-example)

(defun open-clicked-cb (button video)
  (let ((filters (g:list-store-new "GtkFileFilter"))
        (dialog (make-instance 'gtk:file-dialog
                               :title "Select a video")))
    ;; Set "All files" file filter
    (let ((filter (make-instance 'gtk:file-filter
                                 :name "All Files")))
      (gtk:file-filter-add-pattern filter "*")
      (g:list-store-append filters filter))
    ;; Set "Images" file filter
    (let ((filter (make-instance 'gtk:file-filter
                                 :name "Images")))
      (gtk:file-filter-add-mime-type filter "image/*")
      (g:list-store-append filters filter))
    ;; Set "Videos" file filter
    (let ((filter (make-instance 'gtk:file-filter
                                 :name "Videos")))
      (gtk:file-filter-add-mime-type filter "video/*")
      ;; Make this filter the default filter
      (setf (gtk:file-dialog-default-filter dialog) filter)
      (g:list-store-append filters filter))
    ;; Set list of filters
    (setf (gtk:file-dialog-filters dialog) filters)
    ;; Open file dialog
    (gtk:file-dialog-open dialog (gtk:widget-root button) nil
            (lambda (source result)
              (let ((file (gtk:file-dialog-open-finish source result)))
                (when file
                  (setf (gtk:video-file video) file)))))))

(defun logo-clicked-cb (button video)
  (declare (ignore button))
  (let* ((path (glib-sys:sys-path "resource/gtk-logo.webm"))
         (file (g:file-new-for-path path)))
    (setf (gtk:video-file video) file)))

(defun bbb-clicked-cb (button video)
  (declare (ignore button))
  (let* ((path "https://download.blender.org/peach/trailer/trailer_400p.ogg")
         (file (g:file-new-for-uri path)))
    (setf (gtk:video-file video) file)))

(defun toggle-fullscreen (window args)
  (declare (ignore args))
  (if (gtk:window-is-fullscreen window)
      (gtk:window-unfullscreen window)
      (gtk:window-fullscreen window))
  t)

(defun do-video-player (&optional application)
  (let* ((header (make-instance 'gtk:header-bar))
         (video (make-instance 'gtk:video
                               :autoplay t
                               :graphics-offload :enabled))
         (window (make-instance 'gtk:window
                                :application application
                                :child video
                                :title "Video Player"
                                :titlebar header
                                :default-width 600
                                :default-height 400))
         (controller (gtk:shortcut-controller-new)))
    ;; Open button
    (let ((button (gtk:button-new-with-mnemonic "_Open")))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (open-clicked-cb button video)))
      (gtk:header-bar-pack-start header button))
    ;; Logo button
    (let* ((path (glib-sys:sys-path "resource/gtk-logo-cursor.png"))
           (image (gtk:image-new-from-file path))
           (button (make-instance 'gtk:button
                                  :child image)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (logo-clicked-cb button video)))
      (setf (gtk:image-pixel-size image) 24)
      (gtk:header-bar-pack-start header button))
    ;; Big Buck Bunny button
    (let* ((path (glib-sys:sys-path "resource/big-buck-bunny.png"))
           (image (gtk:image-new-from-file path))
           (button (make-instance 'gtk:button
                                  :child image)))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (bbb-clicked-cb button video)))
      (setf (gtk:image-pixel-size image) 24)
      (gtk:header-bar-pack-start header button))
    ;; Fullscreen button
    (let ((button (gtk:button-new-from-icon-name "view-fullscreen-symbolic")))
      (g:signal-connect button "clicked"
                        (lambda (button)
                          (let ((window (gtk:widget-root button)))
                            (gtk:window-fullscreen window))))
      (gtk:header-bar-pack-end header button))
    ;; Add shortcut controller
    (gtk:widget-add-controller window controller)
    (gtk:shortcut-controller-add-shortcut controller
            (gtk:shortcut-new
                    (gtk:keyval-trigger-new (gdk:keyval-from-name "F11") 0)
                    (gtk:callback-action-new #'toggle-fullscreen)))
    ;; Set file for first video
    (gtk:video-set-filename video (glib-sys:sys-path "resource/gtk-logo.webm"))
    ;; Show the window
    (gtk:window-present window)))
