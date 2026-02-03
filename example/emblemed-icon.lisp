;;;; Example Emblemed Icon
;;;;
;;;; The g:emblemed-icon class is an implementation of the g:icon interface that
;;;; supports adding an emblem to an icon. Adding multiple emblems to an icon is
;;;; ensured via the function g:emblemed-icon-add-emblem.
;;;;
;;;; Note that the g:emblemed-icon class allows no control over the position of
;;;; the emblems. See also the g:emblem class for more information.
;;;;
;;;; Last version: 2026-01-24

;; FIXME: The emblemed icon does not show the attached emblem!?

(in-package :gtk4-example)

(defun do-emblemed-icon (&optional application)
  (let* ((icon (make-instance 'g:themed-icon
                              :name "desktop"))
         (emblem (make-instance 'g:emblem
                                :icon (make-instance 'g:themed-icon
                                                     :name
                                                     "emblem-ok-symbolic")))
         (emblemed-icon (g:emblemed-icon-new icon emblem))
         (grid (make-instance 'gtk:grid
                              :margin-top 6
                              :margin-bottom 18
                              :margin-start 12
                              :margin-end 12
                              :column-spacing 24
                              :row-spacing 24))
         (window (make-instance 'gtk:window
                                :application application
                                :child grid
                                :title "Emblemed Icon"
                                :default-width 300
                                :default-height 200)))
    ;; Add the emblem to the icon
;   (g:emblemed-icon-add-emblem emblemed-icon emblem)
    ;; Pack and show the window
    (gtk:grid-attach grid (make-instance 'gtk:label
                                         :use-markup t
                                         :xalign 0.0
                                         :valign :center
                                         :label "<b>Icon</b>")
                          1 0 1 2)
    (gtk:grid-attach grid (make-instance 'gtk:image
                                         :gicon icon
                                         :icon-size :large)
                          3 1 1 1)
    (gtk:grid-attach grid (make-instance 'gtk:label
                                         :use-markup t
                                         :xalign 0.0
                                         :label "<b>Emblem</b>")
                          1 2 1 2)
    (gtk:grid-attach grid (make-instance 'gtk:image
                                         :gicon (g:emblem-icon emblem)
                                         :icon-size :large)
                          3 3 1 1)
    (gtk:grid-attach grid (make-instance 'gtk:label
                                         :use-markup t
                                         :xalign 0.0
                                         :label
                                         "<b>Emblemed Icon</b>")
                          1 4 1 2)
    (gtk:grid-attach grid (make-instance 'gtk:image
                                         :gicon emblemed-icon
                                         :icon-size :large)
                          3 5 1 1)
    (gtk:window-present window)))
