;;;; Scale Widget
;;;;
;;;; The <tt>gtk:scale</tt> widget is a slider control used to select a numeric
;;;; value. To use it, you will probably want to investigate the methods on its
;;;; base <tt>gtk:range</tt> class, in addition to the methods for the
;;;; <tt>gtk:scale</tt> class itself. To set the value of a scale, you would
;;;; normally use the <tt>gtk:range-value</tt> function. To detect changes to the
;;;; value, you would normally use the <tt>"value-changed"</tt> signal.
;;;;
;;;; 2024-12-5

;; TODO:
;; 1. Remove deprecated GtkComboBoxText widget
;; 2. Remove unnesessary GtkBox widgets
;; 3. Improve the layout of the widgets

(in-package :gtk4-example)

(defun do-scale-widget (&optional application)
  (let* ((box1 (make-instance 'gtk:box
                              :orientation :horizontal
                              :homogeneous nil
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (box2 (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :spacing 12))
         (box3 (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :spacing 12))
         (window (make-instance 'gtk:window
                                :title "Scale Widgets"
                                :child box1
                                :application application))
         (adj1 (make-instance 'gtk:adjustment
                              :value 0.0
                              :lower 0.0
                              :upper 101.0
                              :step-increment 0.1
                              :page-increment 1.0
                              :page-size 1.0))
         (vscale (make-instance 'gtk:scale
                                :orientation :vertical
                                :digits 1
                                :value-pos :top
                                :draw-value t
                                :adjustment adj1))
         (hscale (make-instance 'gtk:scale
                                 :orientation :horizontal
                                 :digits 1
                                 :value-pos :top
                                 :draw-value t
                                 :width-request 200
                                 :height-request -1
                                 :adjustment adj1))
         (scrollbar (make-instance 'gtk:scrollbar
                                   :orientation :horizontal
                                   :adjustment adj1)))
    ;; Packing the global widgets hscale, vscale, and scrollbar
    (gtk:box-append box1 vscale)
    (gtk:box-append box1 box2)
    (gtk:box-append box2 hscale)
    (gtk:box-append box2 scrollbar)
    ;; Check button to control whether the value is displayed or not
    (let ((box (make-instance 'gtk:box
                              :orientation :horizontal
                              :homogeneous nil
                              :spacing 12))
          (button (make-instance 'gtk:check-button
                                 :label "Display value on scale widget"
                                 :active t)))
      (g:signal-connect button "toggled"
          (lambda (widget)
            (setf (gtk:scale-draw-value hscale)
                  (gtk:check-button-active widget))
            (setf (gtk:scale-draw-value vscale)
                  (gtk:check-button-active widget))))
      (gtk:box-append box button)
      (gtk:box-append box3 box))
    ;; Check button to show or clear marks
    (let ((box (make-instance 'gtk:box
                              :orientation :horizontal
                              :homogeneous nil
                              :spacing 12))
          (button (make-instance 'gtk:check-button
                                 :label "Show marks"
                                 :active nil)))
      (g:signal-connect button "toggled"
          (lambda (widget)
            (if (gtk:check-button-active widget)
                (let* ((adj (gtk:range-adjustment vscale))
                       (lower (gtk:adjustment-lower adj))
                       (upper (gtk:adjustment-upper adj)))
                  (iter (for i from lower to upper by 10)
                        (gtk:scale-add-mark vscale i :left nil)))
                (gtk:scale-clear-marks vscale))))
      (gtk:box-append box button)
      (gtk:box-append box3 box))
    ;; A ComboBox to change the position of the value.
    (let ((box (make-instance 'gtk:box
                              :orientation :horizontal
                              :homogeneous nil
                              :spacing 12))
          (combo (make-instance 'gtk:combo-box-text)))
      (gtk:combo-box-text-append-text combo "TOP")
      (gtk:combo-box-text-append-text combo "BOTTOM")
      (gtk:combo-box-text-append-text combo "LEFT")
      (gtk:combo-box-text-append-text combo "RIGHT")
      (setf (gtk:combo-box-active combo) 0)
      (g:signal-connect combo "changed"
          (lambda (widget)
            (let ((pos (gtk:combo-box-text-active-text widget)))
              (format t "type      : ~A~%"
                        (g:type-from-instance (g:object-pointer widget)))
              (format t "active is : ~A~%"
                        (gtk:combo-box-active widget))
              (setq pos (if pos (intern pos :keyword) :top))
              (setf (gtk:scale-value-pos hscale) pos)
              (setf (gtk:scale-value-pos vscale) pos))))
      (gtk:box-append box
                      (make-instance 'gtk:label
                                     :label "Scale value position"))
      (gtk:box-append box combo)
      (gtk:box-append box3 box))
    ;; Create a scale to change the digits of hscale and vscale.
    (let* ((box (make-instance 'gtk:box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 12))
           (adj (make-instance 'gtk:adjustment
                               :value 1.0
                               :lower 0.0
                               :upper 5.0
                               :step-increment 1.0
                               :page-increment 1.0
                               :page-size 0.0))
           (scale (make-instance 'gtk:scale
                                 :orientation :horizontal
                                 :hexpand t
                                 :digits 0
                                 :adjustment adj)))
      (g:signal-connect adj "value-changed"
          (lambda (adjustment)
            (setf (gtk:scale-digits hscale)
                  (truncate (gtk:adjustment-value adjustment)))
            (setf (gtk:scale-digits vscale)
                  (truncate (gtk:adjustment-value adjustment)))))
      (gtk:box-append box
                      (make-instance 'gtk:label
                                     :label "Scale Digits:"))
      (gtk:box-append box scale)
      (gtk:box-append box3 box))
    ;; Another hscale for adjusting the page size of the scrollbar
    (let* ((box (make-instance 'gtk:box
                               :orientation :horizontal
                               :homogeneous nil
                               :spacing 12))
           (adj (make-instance 'gtk:adjustment
                               :value 1.0
                               :lower 0.0
                               :upper 100.0
                               :step-increment 1.0
                               :page-increment 1.0
                               :page-size 0.0))
           (scale (make-instance 'gtk:scale
                                 :orientation :horizontal
                                 :hexpand t
                                 :digits 0
                                 :adjustment adj)))
      (g:signal-connect adj "value-changed"
          (lambda (adjustment)
            (setf (gtk:adjustment-page-size adj1)
                  (gtk:adjustment-page-size adjustment))
            (setf (gtk:adjustment-page-increment adj1)
                  (gtk:adjustment-page-increment adjustment))))
      (gtk:box-append box
                      (make-instance 'gtk:label
                                     :label "Scrollbar Page Size:"))
      (gtk:box-append box scale)
      (gtk:box-append box3 box))
    (gtk:box-append box2 box3)
    (gtk:window-present window)))
