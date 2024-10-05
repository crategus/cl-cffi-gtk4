;;;; Example Video
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
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-video (&optional application)
  (let* ((video (make-instance 'gtk:video))
         (window (make-instance 'gtk:window
                                :application application
                                :child video
                                :title "Video Player"
                                :default-width 400
                                :default-height 300)))
    (gtk:video-set-filename video (glib-sys:sys-path "resource/gtk-logo.webm"))
    ;; Show the window
    (gtk:window-present window)))
