;;;; Example Simple Window
;;;;
;;;; This example shows a very simple window. The window has the title "Window".
;;;; The window can be sized and moved.
;;;;
;;;; Last version: 2026-01-24

(in-package :gtk4-tutorial)

(defun window-simple ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.crategus.window-simple"
                            :flags :none)))
    (g:signal-connect app "activate"
                      (lambda (application)
                        (let ((window (make-instance 'gtk:application-window
                                                     :application application
                                                     :title "Window"
                                                     :resizable nil
                                                     :default-width 200
                                                     :default-height 200)))
                          (gtk:window-present window))))
    (g:application-run app nil)))
