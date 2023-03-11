;;;; Size Groups - 2022-7-29
;;;;
;;;; GtkSizeGroup provides a mechanism for grouping a number of widgets together
;;;; so they all request the same amount of space. This is typically useful when
;;;; you want a column of widgets to have the same size, but you cannot use a
;;;; GtkTable widget.
;;;;
;;;; Note that size groups only affect the amount of space requested, not the
;;;; size that the widgets finally receive. If you want the widgets in a
;;;; GtkSizeGroup to actually be the same size, you need to pack them in such a
;;;; way that they get the size they request and not more. For example, if you
;;;; are packing your widgets into a table, you would not include the GTK_FILL
;;;; flag.

(in-package :gtk4-example)

(defun create-combo-box-text (strings)
  (let ((combo (make-instance 'gtk:combo-box-text)))
    (dolist (text strings)
      (gtk:combo-box-text-append-text combo text))
    (setf (gtk:combo-box-active combo) 0)
    combo))

(defun do-size-group (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 6
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title "Size Groups"
                                :child vbox
                                :application application
                                :resizable nil))
         (group (make-instance 'gtk:size-group
                               :mode :horizontal)))
    (let* ((grid (make-instance 'gtk:grid
                                :margin-top 6
                                :margin-bottom 6
                                :margin-start 6
                                :margin-end 6
                                :row-spacing 6
                                :column-spacing 12))
           (frame (make-instance 'gtk:frame
                                 :label "Color Options"
                                 :child grid))
           (combo1 (create-combo-box-text '("Red" "Green" "Blue")))
           (label1 (make-instance 'gtk:label
                                  :label "_Foreground"
                                  :mnemonic-widget combo1
                                  :use-underline t
                                  :halign :start
                                  :valign :baseline
                                  :hexpand t))
           (combo2 (create-combo-box-text '("Red" "Green" "Blue")))
           (label2 (make-instance 'gtk:label
                                  :label "_Background"
                                  :mnemonic-widget combo2
                                  :use-underline t
                                  :halign :start
                                  :valign :baseline
                                  :hexpand t)))
      (gtk:box-append vbox frame)
      (gtk:grid-attach grid label1 0 0 1 1)
      (gtk:grid-attach grid combo1 1 0 1 1)
      (gtk:size-group-add-widget group combo1)
      (gtk:grid-attach grid label2 0 1 1 1)
      (gtk:grid-attach grid combo2 1 1 1 1)
      (gtk:size-group-add-widget group combo2))
    (let* ((grid (make-instance 'gtk:grid
                                :margin-top 6
                                :margin-bottom 6
                                :margin-start 6
                                :margin-end 6
                                :row-spacing 6
                                :column-spacing 12))
           (frame (make-instance 'gtk:frame
                                 :label "Line Options"
                                 :child grid))
           (combo1 (create-combo-box-text '("Solid" "Dashed" "Dotted")))
           (label1 (make-instance 'gtk:label
                                  :label "_Dashing"
                                  :mnemonic-widget combo1
                                  :use-underline t
                                  :halign :start
                                  :valign :baseline
                                  :hexpand t))
           (combo2 (create-combo-box-text '("Square" "Round" "Double Arrow")))
           (label2 (make-instance 'gtk:label
                                  :label "_Line Ends"
                                  :mnemonic-widget combo2
                                  :use-underline t
                                  :halign :start
                                  :valign :baseline
                                  :hexpand t)))
      (gtk:box-append vbox frame)
      (gtk:grid-attach grid label1 0 0 1 1)
      (gtk:grid-attach grid combo1 1 0 1 1)
      (gtk:size-group-add-widget group combo1)
      (gtk:grid-attach grid label2 0 1 1 1)
      (gtk:grid-attach grid combo2 1 1 1 1)
      (gtk:size-group-add-widget group combo2))
    (let ((button (make-instance 'gtk:check-button
                                 :label "_Enable grouping"
                                 :active t
                                 :use-underline t)))
      (g:signal-connect button "toggled"
                        (lambda (button)
                          (if (gtk:check-button-active button)
                              (setf (gtk:size-group-mode group) :horizontal)
                              (setf (gtk:size-group-mode group) :none))))
      (gtk:box-append vbox button))
    (gtk:widget-show window)))
