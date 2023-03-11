;;;; Example Spin Button - 2022-11-18

(in-package :gtk4-example)

;; TODO: Improve the layout.

(defun do-spin-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :spacing 6))
         (window (make-instance 'gtk:window
                                :title "Example Spin Button"
                                :child vbox
                                :application application
                                :default-width 300)))

    (multiple-value-bind
        (second minute hour date month year day daylight-p zone)
        (get-decoded-time)
      (declare (ignore second minute hour day daylight-p zone))

      ;; A label for the three spin buttons for the input of day, month, year.
      (gtk:box-append vbox
                      (make-instance 'gtk:label
                                     :label "<b>Not Accelerated</b>"
                                     :halign :start
                                     :margin-top 6
                                     :margin-bottom 3
                                     :use-markup t))

      (let ((hbox (make-instance 'gtk:box :orientation :horizontal)))
        ;; A vertical Box with a label and a spin button for a day.
        (let ((vbox (make-instance 'gtk:box :orientation :vertical))
              (spinner (make-instance 'gtk:spin-button
                                      :adjustment
                                      (make-instance 'gtk:adjustment
                                                     :value date
                                                     :lower 1.0
                                                     :upper 31.0
                                                     :step-increment 1.0
                                                     :page-increment 5.0
                                                     :page-size 0.0)
                                      :climb-rate 0
                                      :digits 0
                                      :wrap t)))
          ;; FIXME: The entry does not show the default value.
          ;; What is the problem? We set the value explicitly.
          (setf (gtk:editable-text spinner) (format nil "~d" date))

          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Day :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner)
          (gtk:box-append hbox vbox))

        ;; A vertical Box with a label and a spin button for the month.
        (let ((vbox (make-instance 'gtk:box :orientation :vertical))
              (spinner (make-instance 'gtk:spin-button
                                      :adjustment
                                      (make-instance 'gtk:adjustment
                                                     :value month
                                                     :lower 1.0
                                                     :upper 12.0
                                                     :step-increment 1.0
                                                     :page-increment 5.0
                                                     :page-size 0.0)
                                      :climb-rate 0
                                      :digits 0
                                      :wrap t)))

        ;; FIXME: The entry does not show the default value.
        ;; What is the problem? We set the value explicitly.
        (setf (gtk:editable-text spinner) (format nil "~d" month))

        (gtk:box-append vbox
                        (make-instance 'gtk:label
                                       :label "Month :"
                                       :xalign 0
                                       :yalign 0.5))
        (gtk:box-append vbox spinner)
        (gtk:box-append hbox vbox))

        ;; A vertival Box with a label and a spin button for the year.
        (let ((vbox (make-instance 'gtk:box :orientation :vertical))
              (spinner (make-instance 'gtk:spin-button
                                      :adjustment
                                      (make-instance 'gtk:adjustment
                                                     :value year
                                                     :lower 1998.0
                                                     :upper 2100.0
                                                     :step-increment 1.0
                                                     :page-increment 100.0
                                                     :page-size 0.0)
                                      :climb-rate 0
                                      :digits 0
                                      :wrap t)))

        ;; FIXME: The entry does not show the default value.
        ;; What is the problem? We set the value explicitly.
        (setf (gtk:editable-text spinner) (format nil "~d" year))

        (gtk:box-append vbox
                        (make-instance 'gtk:label
                                       :label "Year :"
                                       :xalign 0
                                       :yalign 0.5))
        (gtk:box-append vbox spinner)
        (gtk:box-append hbox vbox))
      ;; Place the hbox in the vbox
      (gtk:box-append vbox hbox)))

    ;; A label for the accelerated spin button.
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :label "<b>Accelerated</b>"
                                   :halign :start
                                   :margin-top 6
                                   :margin-bottom 3
                                   :use-markup t))

    ;; A vertical Box with for the accelarated spin button
    (let ((spinner1 (make-instance 'gtk:spin-button
                                   :adjustment
                                   (make-instance 'gtk:adjustment
                                                  :value 0.0
                                                  :lower -10000.0
                                                  :upper  10000.0
                                                  :step-increment 0.5
                                                  :page-increment 100.0
                                                  :page-size 0.0)
                                   :climb-rate 1.0
                                   :digits 2
                                   :wrap t))
          (spinner2 (make-instance 'gtk:spin-button
                                   :adjustment
                                   (make-instance 'gtk:adjustment
                                                  :value 2
                                                  :lower 1
                                                  :upper 5
                                                  :step-increment 1
                                                  :page-increment 1
                                                  :page-size 0)
                                   :climb-rate 0.0
                                   :digits 0
                                   :wrap t)))
      ;; Customize the appearance of the number
      (g:signal-connect spinner1 "output"
        (lambda (spin-button)
          (let ((value (gtk:adjustment-value
                         (gtk:spin-button-adjustment spin-button)))
                (digits (truncate
                          (gtk:adjustment-value
                            (gtk:spin-button-adjustment spinner2)))))
            (setf (gtk:editable-text spin-button)
                  (format nil "~@?" (format nil "~~,~d@f" digits) value)))))
       ;; Update number of digits if changed
       (g:signal-connect spinner2 "value-changed"
         (lambda (spin-button)
           (setf (gtk:spin-button-digits spinner1)
                 (gtk:spin-button-value-as-int spin-button))))
       ;; FIXME: The entry does not show the default value.
       ;; What is the problem? We set the value.
      (setf (gtk:editable-text spinner2) (format nil "~d" 2))

      (let ((hbox (make-instance 'gtk:box :orientation :horizontal)))
        ;; Put the accelarated spin button with a label in a vertical box.
        (let ((vbox (make-instance 'gtk:box :orientation :vertical)))
          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Value :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner1)
          (gtk:box-append hbox vbox))

        ;; Put the spin button for digits with a label in a vertical box.
        (let ((vbox (make-instance 'gtk:box :orientation :vertical)))
          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Digits :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner2)
          (gtk:box-append hbox vbox))

        (gtk:box-append vbox hbox))

      (let ((check (make-instance 'gtk:check-button
                                  :label "Snap to 0.5-ticks"
                                  :active t)))
        (g:signal-connect check "toggled"
           (lambda (widget)
             (setf (gtk:spin-button-snap-to-ticks spinner1)
                   (gtk:check-button-active widget))))
        (gtk:box-append vbox check))

      (let ((check (make-instance 'gtk:check-button
                                  :label "Numeric only input mode"
                                  :active t)))
        (g:signal-connect check "toggled"
           (lambda (widget)
             (setf (gtk:spin-button-numeric spinner1)
                   (gtk:check-button-active widget))))
        (gtk:box-append vbox check))

      (let ((label (make-instance 'gtk:label
                                  :label "0"
                                  :halign :start
                                  :margin-top 3
                                  :margin-bottom 3))
            (hbox (make-instance 'gtk:box :orientation :horizontal)))

        (let ((button (gtk:button-new-with-label "Value as Int")))
          (g:signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (setf (gtk:label-text label)
                     (format nil "~A"
                             (gtk:spin-button-value-as-int spinner1)))))
            (gtk:box-append hbox button))

        (let ((button (gtk:button-new-with-label "Value as Float")))
          (g:signal-connect button "clicked"
             (lambda (widget)
               (declare (ignore widget))
               (setf (gtk:label-text label)
                     (format nil "~A"
                             (gtk:spin-button-value spinner1)))))
          (gtk:box-append hbox button))

        ;; Get the value of the accelerated spin button.
        (gtk:box-append vbox
                        (make-instance 'gtk:label
                                       :label "<b>Get Value</b>"
                                       :halign :start
                                       :margin-top 3
                                       :margin-bottom 3
                                       :use-markup t))
        (gtk:box-append vbox label)
        (gtk:box-append vbox hbox)))

    (gtk:widget-show window)))
