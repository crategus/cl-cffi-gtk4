;;;; Example Menu Button - 2023-3-25

(in-package :gtk4-example)

(defun do-menu-button (&optional application)
  (let* ((box (make-instance 'gtk:box
                             :orientation :vertical
                             :margin-top 12
                             :margin-bottom 12
                             :margin-start 12
                             :margin-end 12))
         (window (make-instance 'gtk:window
                                 :title "Menu Buttons"
                                 :application application
                                 :child box
                                 :resizable nil
                                 :default-width 240
                                 :default-height 210))
         (button nil))
    ;; Default Menu Button
    (gtk:box-append box (make-instance 'gtk:label
                                       :use-markup t
                                       :margin-top 6
                                       :margin-bottom 6
                                       :label
                                       "<b>Menu Button</b>"))
    (setf button
          (make-instance 'gtk:menu-button
                         :popover
                         (make-instance 'gtk:popover
                                        :child
                                        (make-instance 'gtk:label
                                                       :label "Popover"))))
    (gtk:box-append box button)
    ;; Menu Button with Label
    (gtk:box-append box (make-instance 'gtk:label
                                       :use-markup t
                                       :margin-top 12
                                       :margin-bottom 6
                                       :label
                                       "<b>Menu Button with Label</b>"))
    (setf button
          (make-instance 'gtk:menu-button
                         :label "Menu Button"
                         :popover
                         (make-instance 'gtk:popover
                                        :child
                                        (make-instance 'gtk:label
                                                       :label "Popover"))))
    (gtk:box-append box button)
    ;; Menu Button with Mnemonic
    (gtk:box-append box (make-instance 'gtk:label
                                       :use-markup t
                                       :margin-top 12
                                       :margin-bottom 6
                                       :label
                                       "<b>Menu Button with Mnemonic</b>"))
    (setf button
          (make-instance 'gtk:menu-button
                         :label "Menu _Button"
                         :use-underline t
                         :popover
                         (make-instance 'gtk:popover
                                        :child
                                        (make-instance 'gtk:label
                                                       :label "Popover"))))
    (gtk:box-append box button)
    ;; Menu Button with Icon
    (gtk:box-append box (make-instance 'gtk:label
                                       :use-markup t
                                       :margin-top 12
                                       :margin-bottom 6
                                       :label
                                       "<b>Menu Button with Icon</b>"))
    (setf button
          (make-instance 'gtk:menu-button
                         :icon-name "preferences-system"
                         :popover
                         (make-instance 'gtk:popover
                                        :child
                                        (make-instance 'gtk:label
                                                       :label "Popover"))))
    (gtk:box-append box button)
    ;; Show the window
    (gtk:widget-show window)))
