;;;; Label Attributes
;;;;
;;;; Last version: 2025-08-14

(in-package :gtk4-example)

(defparameter *text1* "The label with Pango attributes.")

(defun make-header (text)
  (make-instance 'gtk:label
                 :xalign 0
                 :margin-top 6
                 :use-markup t
                 :label (format nil "<b>~A</b>" text)))

(defun do-label-attributes (&optional application)
  (let* ((attrlist (pango:attr-list-new))
         (label (make-instance 'gtk:label
                               :label "The label with Pango attributes."
                               :margin-top 24
                               :margin-bottom 24
                               :margin-left 24
                               :margin-right 24))
         (window (make-instance 'gtk:window
                                :application application
                                :title "Label with Pango attributes"
                                :child label
                                :default-width 250
                                :border-width 18)))


    (let ((attr1 (pango:attr-weight-new :bold)))

      (setf (pango:attribute-start-index attr1) 5)
      (setf (pango:attribute-end-index attr1) 9)

      (format t "   attr : ~a~%" attr1)
      (format t "  start : ~a~%" (pango:attribute-start-index attr1))
      (format t "    end : ~a~%" (pango:attribute-end-index attr1))
      (format t "   type : ~a~%" (pango:attribute-type attr1))

      (pango:attr-list-insert attrlist attr1)

      (format t "    str : ~a~%" (pango:attr-list-to-string attrlist))

      (setf (gtk:label-attributes label) attrlist)
    )
    (gtk:window-present window)))
