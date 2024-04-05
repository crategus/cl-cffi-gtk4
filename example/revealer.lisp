;;;; Revealer
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun do-revealer (&optional (application nil))
  (let* ((grid (make-instance 'gtk:grid
                              :column-homogeneous t
                              :row-homogeneous t
                              :column-spacing 12
                              :row-spacing 12
                              :margin-bottom 12
                              :margin-end 12
                              :margin-start 12
                              :margin-top 12))
         (window (make-instance 'gtk:window
                                :child grid
                                :title "Example Revealer"
                                :application application
                                :default-width 300
                                :default-height 300)))
    (gtk:grid-attach grid
                     (make-instance 'gtk:label
                                    :use-markup t
                                    :label "<b>Press a Button</b>")
                     2 2 1 1)
    (let* ((toggle (make-instance 'gtk:toggle-button :label "None"))
           (label (make-instance 'gtk:label
                                 :label "No transation"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :center
                                    :valign :center
                                    :transition-type :none
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 0 0 1 1)
      (gtk:grid-attach grid revealer 1 0 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Fade"))
           (label (make-instance 'gtk:label
                                :label "Fade in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :center
                                    :valign :center
                                    :transition-type :crossfade
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 4 4 1 1)
      (gtk:grid-attach grid revealer 3 4 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Right"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :start
                                    :hexpand t
                                    :transition-type :slide-right
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 3 2 1 1)
      (gtk:grid-attach grid revealer 4 2 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Down"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :valign :start
                                    :vexpand t
                                    :transition-type :slide-down
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle   2 3 1 1)
      (gtk:grid-attach grid revealer 2 4 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Left"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :end
                                    :hexpand t
                                    :transition-type :slide-left
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 1 2 1 1)
      (gtk:grid-attach grid revealer 0 2 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Up"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :valign :end
                                    :vexpand t
                                    :transition-type :slide-up
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle   2 1 1 1)
      (gtk:grid-attach grid revealer 2 0 1 1))
    (setf (gtk:widget-visible window) t)))

(defun do-revealer-swing (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :column-homogeneous t
                              :row-homogeneous t
                              :column-spacing 12
                              :row-spacing 12
                              :margin-bottom 12
                              :margin-end 12
                              :margin-start 12
                              :margin-top 12))
         (window (make-instance 'gtk:window
                                :child grid
                                :title "Example Revealer Swing"
                                :application application
                                :default-width 300
                                :default-height 300)))
    (gtk:grid-attach grid
                     (make-instance 'gtk:label
                                    :use-markup t
                                    :label "<b>Press a Button</b>")
                     2 2 1 1)
    (let* ((toggle (make-instance 'gtk:toggle-button :label "None"))
           (label (make-instance 'gtk:label
                                 :label "No transation"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :center
                                    :valign :center
                                    :transition-type :none
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 0 0 1 1)
      (gtk:grid-attach grid revealer 1 0 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Fade"))
           (label (make-instance 'gtk:label
                                :label "Fade in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :center
                                    :valign :center
                                    :transition-type :crossfade
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 4 4 1 1)
      (gtk:grid-attach grid revealer 3 4 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Right"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :start
                                    :hexpand t
                                    :transition-type :swing-right
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 3 2 1 1)
      (gtk:grid-attach grid revealer 4 2 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Down"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :valign :start
                                    :vexpand t
                                    :transition-type :swing-down
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle   2 3 1 1)
      (gtk:grid-attach grid revealer 2 4 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Left"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :halign :end
                                    :hexpand t
                                    :transition-type :swing-left
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle 1 2 1 1)
      (gtk:grid-attach grid revealer 0 2 1 1))
    (let* ((toggle (make-instance 'gtk:toggle-button :label "Up"))
           (label (make-instance 'gtk:label
                                 :label "Slide in the label"))
           (revealer (make-instance 'gtk:revealer
                                    :child label
                                    :valign :end
                                    :vexpand t
                                    :transition-type :swing-up
                                    :transition-duration 4000)))
      (g:object-bind-property toggle "active"
                              revealer "reveal-child"
                              :default)
      (gtk:grid-attach grid toggle   2 1 1 1)
      (gtk:grid-attach grid revealer 2 0 1 1))
    (gtk:window-present window) t))
