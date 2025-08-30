;;;; Label Attributes
;;;;
;;;; This example demonstrates how to use Pango attributes to style the text of
;;;; a label. The attributes are created with the PANGO:ATTRIBUTE-NEW function,
;;;; added to a Pango attribute list and set on the label with the
;;;; (SETF GTK:LABEL-ATTRIBUTES) function.
;;;;
;;;; The second label uses a Pango tab array to format a 3x3 table with aligned
;;;; entries. The Pango tab array is created with the
;;;; PANGO:TAB-ARRAY-NEW-WITH-POSITIONS function and set on the label with the
;;;; (SETF GTK:LABEL-TABS) function.
;;;;
;;;; Last updated: 2025-08-29

(in-package :gtk4-example)

(defun do-label-attributes (&optional application)
  (let* ((attrlist (pango:attr-list-new))
         (box (make-instance 'gtk:box
                             :orientation :vertical
                             :spacing 12
                             :margin-top 12
                             :margin-start 24
                             :margin-end 24
                             :margin-bottom 24))
         (window (make-instance 'gtk:window
                                :application application
                                :title "Label with Pango Attributes"
                                :child box
                                :default-width 380
                                :border-width 18)))
    ;; Label with Pango Attributes
    (let ((attrs (list (pango:attribute-new :weight 4 9 :bold)
                       (pango:attribute-new :style 11 15 :italic)
                       (pango:attribute-new :background 15 20 0 65000 0)
                       (pango:attribute-new :foreground 21 31 65000 0 0)))
          (label (make-instance 'gtk:label
                                :label "The label with Pango attributes.")))
      ;; Insert attributes on attibute list
      (dolist (attr attrs)
        (pango:attr-list-insert attrlist attr))
      ;; Set attributes on label
      (setf (gtk:label-attributes label) attrlist)
      ;; Append header label and label with attributes
      (gtk:box-append box (make-instance 'gtk:label
                                         :use-markup t
                                         :label "<b>Pango Attributes</b>"
                                         :xalign 0.0))
      (gtk:box-append box label))
    ;; Label with Pango tab array
    (let* ((tabs (pango:tab-array-new-with-positions 3 t
                                                     :left 0
                                                     :decimal 90
                                                     :right 180))
           (text (format nil
                   "one~c2.0~cthree~%four~c5.555~csix~%seven~c88.88~cnine"
                   #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
           (label (make-instance 'gtk:label
                                 :label text)))
      ;; Set character for decimal point
      (setf (pango:tab-array-decimal-point tabs 1) #\.)
      ;; Set Pango tab array on label
      (setf (gtk:label-tabs label) tabs)
      ;; Append header label and label with Pango tab array
      (gtk:box-append box (make-instance 'gtk:label
                                         :use-markup t
                                         :label "<b>Pango Tab Array</b>"
                                         :xalign 0.0))
      (gtk:box-append box label))
    ;; Present window
    (gtk:window-present window)))
