;;;; Example Video - 2022-9-8

(in-package :gtk4-example)

;; FIXME: We have no media modul installed.

(defun do-video (&optional application)
  (let* ((video (make-instance 'gtk:video))
         (window (make-instance 'gtk:window
                                :application application
                                :child video
                                :title "Video Player"
                                :default-width 400
                                :default-height 300)))

    (gtk:video-set-filename video (sys-path "resource/gtk-logo.webm"))

    ;; Show the window
    (gtk:widget-show window)))
