;;;; Level Bar
;;;;
;;;; The <tt>gtk:level-bar widget</tt> is a bar widget that can be used as a
;;;; level indicator. Typical use cases are displaying the strength of a
;;;; password, or showing the charge level of a battery.
;;;;
;;;; Use the <tt>gtk:level-bar-value</tt> function to set the current value, and
;;;; the <tt>gtk:level-bar-add-offset-value</tt> function to set the value
;;;; offsets at which the bar will be considered in a different state. GTK will
;;;; add a few offsets by default on the level bar: <tt>"low"</tt>,
;;;; <tt>"high"</tt> and <tt>"full"</tt>, with values 0.25, 0.75 and 1.0
;;;; respectively.
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defun create-level-bar (orientation mode)
  (let ((provider (gtk:css-provider-new))
        (levelbar (make-instance 'gtk:level-bar
                                  :orientation orientation
                                  :mode mode)))
    ;; Change values from 0 to 10
    (setf (gtk:level-bar-max-value levelbar) 10)
    ;; Add new offset value with name "empty"
    (gtk:level-bar-add-offset-value levelbar "empty" 2.0)
    ;; Adjust the standard offset values
    (gtk:level-bar-add-offset-value levelbar "low" 4.0)
    (gtk:level-bar-add-offset-value levelbar "high" 8.0)
    (gtk:level-bar-add-offset-value levelbar "full" 10.0)
    ;; CSS to change the color for the values
    (gtk:css-provider-load-from-string provider
                                       "levelbar.lbar block.filled.empty {
                                          background-color: red; }
                                        levelbar.lbar block.filled.low {
                                          background-color: orange; }
                                        levelbar.lbar block.filled.high {
                                          background-color: yellow; }
                                        levelbar.lbar block.filled.full {
                                          background-color: green; }")
    (gtk:widget-add-css-class levelbar "lbar")
    (gtk:widget-add-provider levelbar provider)
    ;; Return the new level bar
    levelbar))

(defun do-level-bar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Level bar"
                                :child vbox
                                :application application
                                :default-width 420
                                :default-height 240))
         (adj (make-instance 'gtk:adjustment
                             :value 0.0
                             :lower 0.0
                             :upper 10.0
                             :step-increment 0.1))
         (scale (make-instance 'gtk:scale
                               :orientation :horizontal
                               :digits 1
                               :value-pos :top
                               :draw-value nil
                               :adjustment adj))
         (levelbar1 (create-level-bar :horizontal :continuous))
         (levelbar2 (create-level-bar :horizontal :discrete)))
    ;; Bind adjustment value for the scale to the level bar values
    (g:object-bind-property adj "value" levelbar1 "value" :default)
    (g:object-bind-property adj "value" levelbar2 "value" :default)
    ;; Pack and show the widgets
    (gtk:box-append vbox (make-instance 'gtk:label
                                        :xalign 0.0
                                        :use-markup t
                                        :label "<b>Continuous mode</b>"))
    (gtk:box-append vbox levelbar1)
    (gtk:box-append vbox (make-instance 'gtk:label
                                        :xalign 0.0
                                        :use-markup t
                                        :label "<b>Discrete mode</b>"))
    (gtk:box-append vbox levelbar2)
    (gtk:box-append vbox (make-instance 'gtk:label
                                        :use-markup t
                                        :xalign 0.0
                                        :label "<b>Change value</b>"))
    (gtk:box-append vbox scale)
    (gtk:window-present window)))
