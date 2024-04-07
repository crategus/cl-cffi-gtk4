;;;; Spin Button
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-spin-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :margin-top 18
                              :margin-bottom 18
                              :margin-start 18
                              :margin-end 18
                              :spacing 6))
         (window (make-instance 'gtk:window
                                :title "Spin Buttons"
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
      (let ((hbox (gtk:box-new :horizontal 6)))
        ;; A vertical Box with a label and a spin button for a day.
        (let ((vbox (gtk:box-new :vertical 3))
              (spinner (make-instance 'gtk:spin-button
                                      :text (format nil "~d" date)
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
          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Day :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner)
          (gtk:box-append hbox vbox))
        ;; A vertical Box with a label and a spin button for the month.
        (let ((vbox (gtk:box-new :vertical 3))
              (spinner (make-instance 'gtk:spin-button
                                      :text (format nil "~d" month)
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
        (gtk:box-append vbox
                        (make-instance 'gtk:label
                                       :label "Month :"
                                       :xalign 0
                                       :yalign 0.5))
        (gtk:box-append vbox spinner)
        (gtk:box-append hbox vbox))
        ;; A vertival Box with a label and a spin button for the year.
        (let ((vbox (gtk:box-new :vertical 3))
              (spinner (make-instance 'gtk:spin-button
                                      :text (format nil "~d" year)
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
                                                  :step-increment 0.01
                                                  :page-increment 100.0
                                                  :page-size 0.0)
                                   :climb-rate 1.0
                                   :digits 2
                                   :wrap t))
          (spinner2 (make-instance 'gtk:spin-button
                                   :text (format nil "~d" 2)
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
        (lambda (spinner)
          (let ((value (gtk:spin-button-value spinner))
                (digits (gtk:spin-button-value-as-int spinner2)))
            (setf (gtk:editable-text spinner)
                  (format nil "~@?" (format nil "~~,~d@f" digits) value)))))
       ;; Update number of digits and step-increment if changed
       (g:signal-connect spinner2 "value-changed"
         (lambda (spinner)
           (let* ((digits (gtk:spin-button-value-as-int spinner))
                  (increment (/ 1 (expt 10 digits))))
             (setf (gtk:spin-button-increments spinner1)
                   (list increment (* 10 increment))
                   (gtk:spin-button-digits spinner1)
                   digits))))
      (let ((hbox (gtk:box-new :horizontal 3)))
        ;; Put the accelarated spin button with a label in a vertical box.
        (let ((vbox (gtk:box-new :vertical 3)))
          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Value :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner1)
          (gtk:box-append hbox vbox))
        ;; Put the spin button for digits with a label in a vertical box.
        (let ((vbox (gtk:box-new :vertical 3)))
          (gtk:box-append vbox
                          (make-instance 'gtk:label
                                         :label "Digits :"
                                         :xalign 0
                                         :yalign 0.5))
          (gtk:box-append vbox spinner2)
          (gtk:box-append hbox vbox))
        (gtk:box-append vbox hbox))
      (let ((label (make-instance 'gtk:label
                                  :label "0"
                                  :halign :start
                                  :margin-top 3
                                  :margin-bottom 3))
            (hbox (gtk:box-new :horizontal 3)))
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
                                       :margin-top 6
                                       :margin-bottom 3
                                       :use-markup t))
        (gtk:box-append vbox label)
        (gtk:box-append vbox hbox)))
    (gtk:window-present window)))
