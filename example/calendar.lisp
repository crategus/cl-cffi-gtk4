;;;; Calendar Widget - 2023-8-24

(in-package :gtk4-example)

(defun do-calendar (&optional application)
    (let* ((calendar (make-instance 'gtk:calendar
                                    :margin-top 12
                                    :margin-bottom 12
                                    :margin-start 12
                                    :margin-end 12))
           (window (make-instance 'gtk:window
                                  :title "Calendar"
                                  :child calendar
                                  :application application)))
      ;; Connect a signal handler to print the selected day
      (g:signal-connect calendar "day-selected"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Selected: year ~A month ~A day ~A~%"
                                  (gtk:calendar-year calendar)
                                  (1+ (gtk:calendar-month calendar))
                                  (gtk:calendar-day calendar))))
      ;; Mark a day
      (gtk:calendar-mark-day calendar 6)
      (setf (gtk:widget-visible window) t)))
