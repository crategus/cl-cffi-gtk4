;;;; Interactive Overlay
;;;;
;;;; Shows overlay widgets in static positions over a main widget. The overlayed
;;;; widgets can be interactive controls such as the entry in this example, or
;;;; just decorative, like the big orange label.
;;;;
;;;; Last version: 2024-10-2

(in-package :gtk4-example)

(defun do-overlay-interactive (&optional application)
  (let* ((grid (make-instance 'gtk:grid))
         (overlay (make-instance 'gtk:overlay
                                 :child grid))
         (window (make-instance 'gtk:window
                                :title "Interactive Overlay"
                                :child overlay
                                :application application
                                :default-width 500
                                :default-height 510))
         (entry (make-instance 'gtk:entry
                               :placeholder-text "Your number ..."))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :halign :center
                              :valign :center)))
    ;; Fill grid with buttons
    (dotimes (j 5)
      (dotimes (i 5)
        (let ((button (make-instance 'gtk:button
                                     :label
                                     (format nil "~a" (+ 1 i (* 5 j)))
                                     :hexpand t
                                     :vexpand t)))
          (g:signal-connect button "clicked"
                            (lambda (button)
                              (setf (gtk:editable-text entry)
                                    (gtk:button-label button))))
          (gtk:grid-attach grid button i j 1 1))))
    ;; Overlay label
    (let ((label (make-instance 'gtk:label
                                :use-markup t
                                :can-target nil
                                :valign :start
                                :margin-top 36
                                :label
                                "<span foreground='orange'
                                       weight='ultrabold'
                                       font='32'>Choose a Number</span>")))
      (gtk:overlay-add-overlay overlay label))
    ;; Pack entry in box and add the box as overlay
    (gtk:box-append vbox entry)
    (gtk:overlay-add-overlay overlay vbox)
    ;; Present window
    (gtk:window-present window)))
