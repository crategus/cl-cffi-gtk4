;;;; Calendar Widget
;;;;
;;;; The <tt>gtk:calendar</tt> widget displays a Gregorian calendar, one month
;;;; at a time. It can be created with the <tt>gtk:calendar-new</tt> function.
;;;;
;;;; The date that is currently displayed can be altered with the
;;;; <tt>gtk:calendar-select-day</tt> function.
;;;;
;;;; To place a visual marker on a particular day, use the
;;;; <tt>gtk:calendar-mark-day</tt> function and to remove the marker, the
;;;; <tt>gtk:calendar-unmark-day</tt> function. Alternative, all marks can be
;;;; cleared with the <tt>gtk:calendar-clear-marks</tt> function.
;;;;
;;;; The selected date can be retrieved from a <tt>gtk:calendar</tt> widget
;;;; using the <tt>gtk:calendar-date</tt> function.
;;;;
;;;; Users should be aware that, although the Gregorian calendar is the legal
;;;; calendar in most countries, it was adopted progressively between 1582 and
;;;; 1929. Display before these dates is likely to be historically incorrect.
;;;;
;;;; 2024-7-5

(in-package :gtk4-example)

(defun do-calendar (&optional application)
    (let* ((calendar (make-instance 'gtk:calendar))
           (window (make-instance 'gtk:window
                                  :title "Calendar"
                                  :child calendar
                                  :application application)))
      ;; Connect a signal handler to print the selected day in the title
      (g:signal-connect calendar "day-selected"
                        (lambda (widget)
                          (setf (gtk:window-title window)
                                (format nil "Calendar - ~a.~a.~a"
                                       (gtk:calendar-day widget)
                                       (1+ (gtk:calendar-month widget))
                                       (gtk:calendar-year widget)))))
      ;; Mark a day
      (gtk:calendar-mark-day calendar 6)
      (gtk:window-present window)))
