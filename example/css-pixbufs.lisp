;;;; Animated backgrounds - 2023-3-10
;;;;
;;;; This demo is done in honour of the Pixbufs demo further down.
;;;; It is done exclusively with CSS as the background of the window.

(in-package :gtk4-example)

(defun do-css-pixbufs (&optional application)
  (let* ((container (make-instance 'gtk:box
                                   :orientation :horizontal
                                   :halign :center
                                   :valign :center))
         ;; Create a toplevel window.
         (window (make-instance 'gtk:window
                                :application application
                                :child container
                                :title "CSS Pixbufs"
                                :default-height 420
                                :default-width 420))
         (provider (make-instance 'gtk:css-provider))
         (csspath (sys-path "resource/css-pixbufs.css")))
    ;; Load CSS from file into the provider
    (gtk:css-provider-load-from-path provider csspath)
    ;; Apply CSS to the widgets
    (apply-css-to-widget provider window)
    ;; Show the window.
    (gtk:widget-show window)))
