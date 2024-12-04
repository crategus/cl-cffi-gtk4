;;;; Box Append
;;;;
;;;; The <tt>gtk:box</tt> widget arranges child widgets into a single row or
;;;; column. Whether it is a row or column depends on the value of its
;;;; <tt>orientation</tt> property. Within the other dimension, all children are
;;;; allocated the same size. Of course, the <tt>halign</tt> and <tt>valign</tt>
;;;; properties can be used on the children to influence their allocation.
;;;;
;;;; Use repeated calls to the <tt>gtk:box-append</tt> function to pack widgets
;;;; into a box from start to end. Use the <tt>gtk:box-remove</tt> function to
;;;; remove widgets from the box. The <tt>gtk:box-insert-child-after</tt>
;;;; function can be used to add a child widget at a particular position.
;;;;
;;;; Use the <tt>gtk:box-homogeneous</tt> function to specify whether or not all
;;;; children of the box are forced to get the same amount of space and the
;;;; <tt>gtk:box-spacing</tt> function to determine how much space will be
;;;; minimally placed between all children in the box. Note that spacing is
;;;; added between the children.
;;;;
;;;; 2024-4-4

(in-package :gtk4-example)

(defun do-box-append (&optional (application nil))
  (flet ((make-box (homogeneous spacing)
           (let ((box (make-instance 'gtk:box
                                     :orientation :horizontal
                                     :baseline-position :top
                                     :homogeneous homogeneous
                                     :spacing spacing)))
             (gtk:box-append box (gtk:button-new-with-label "This"))
             (gtk:box-append box (gtk:button-new-with-label "is"))
             (gtk:box-append box (gtk:button-new-with-label "a"))
             (gtk:box-append box (gtk:button-new-with-label "button"))
             (gtk:box-append box (gtk:button-new-with-label "party"))
             (gtk:box-append box (gtk:button-new-with-label "."))
             box)))
    (let* ((vbox (make-instance 'gtk:box
                                :orientation :vertical
                                :spacing 12
                                :margin-bottom 12
                                :margin-end 12
                                :margin-start 12
                                :margin-top 12))
           (window (make-instance 'gtk:window
                                  :title "Box Append"
                                  :application application
                                  :child vbox)))
      ;; Non-homogenous box
      (gtk:box-append vbox
                      (make-instance 'gtk:label
                                     :use-markup t
                                     :label "<b>Non-homogeneous box</b>"
                                     :xalign 0))
      (gtk:box-append vbox (make-box nil 3))
      ;; Homogeneous box
      (gtk:box-append vbox
                      (make-instance 'gtk:label
                                     :use-markup t
                                     :label "<b>Homogeneous box</b>"
                                     :xalign 0))
      (gtk:box-append vbox (make-box t 3))
      ;; Homogeneous box more spacing
      (gtk:box-append vbox
                      (make-instance 'gtk:label
                                     :use-markup t
                                     :label
                                     "<b>Homogeneous box with more spacing</b>"
                                     :xalign 0))
      (gtk:box-append vbox (make-box t 24))
      (gtk:window-present window))))
