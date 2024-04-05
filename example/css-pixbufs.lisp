;;;; Animated backgrounds
;;;;
;;;; This demo is done in honour of the Pixbufs demo further down.
;;;; It is done exclusively with CSS as the background of the window.
;;;;
;;;; 2024-4-3

(in-package :gtk4-example)

(defun do-css-pixbufs (&optional application)
  (let* ((container (make-instance 'gtk:box
                                   :orientation :horizontal
                                   :halign :center
                                   :valign :center))
         ;; Create a toplevel window
         (window (make-instance 'gtk:window
                                :application application
                                :child container
                                :title "CSS Pixbufs"
                                :default-height 420
                                :default-width 420))
         (provider (make-instance 'gtk:css-provider))
         (display (gdk:display-default))
         (csspath (sys-path "resource/css-pixbufs.css")))
    (gtk:widget-add-css-class window "pixbufs")
    ;; Load CSS from file into the provider
    (gtk:css-provider-load-from-path provider csspath)
    ;; Apply CSS to the widgets
    (gtk:style-context-add-provider-for-display display provider)
    ;; Install destroy callback to remove provider from window
    (g:object-set-data-full window "provider"
        (lambda ()
          (gtk:style-context-remove-provider-for-display display provider)))
    ;; Show the window
    (gtk:window-present window)))
