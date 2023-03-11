;;;; Various Label - 2022-11-12

(in-package :gtk4-example)

(defparameter *text1* "This is a normal label.")
(defparameter *text2*
              (format nil "This is a multiline label.~%~
                           Second line.~%~
                           Third line."))
(defparameter *text3*
              (format nil
                      "This is a center justified~%~
                       multiline label.~%~
                       Third line."))
(defparameter *text4*
              (format nil
                      "This is a right justified~%~
                       multiline label.~%~
                       Third line."))

(defun make-header (text)
  (make-instance 'gtk:label
                 :xalign 0
                 :margin-top 6
                 :use-markup t
                 :label (format nil "<b>~A</b>" text)))

(defun do-label-various (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :margin-bottom 12
                              :margin-top 12
                              :margin-start 12
                              :margin-end 12
                              :spacing 24))
         (window (make-instance 'gtk:window
                                :application application
                                :title "Example Labels"
                                :child hbox
                                :default-width 250
                                :border-width 18))
         (vbox1 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6))
         (vbox2 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6)))
    ;; Create a Normal Label
    (gtk:box-append vbox1 (make-header "Normal Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label :label *text1*))
    ;; Create a Multi-line Label
    (gtk:box-append vbox1 (make-header "Multiline Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label :label *text2*))
    ;; Create a Left Justified Label
    (gtk:box-append vbox1 (make-header "Center Justified Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :justify :center
                                         :label *text3*))
    ;; Create a Right Justified Label
    (gtk:box-append vbox1 (make-header "Right Justified Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :justify :right
                                         :label *text4*))
    ;; Create an mnemonic label
    (gtk:box-append vbox1 (make-header "Mnemonic Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :use-underline t
                                         :label "With _Mnemonic"))
    ;; Create an selectable label
    (gtk:box-append vbox1 (make-header "Selectable Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :selectable t
                                         :label "This label is selectable"))
    ;; Create an label with markup
    (gtk:box-append vbox1 (make-header "Labels with markup"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :use-markup t
                                         :label "<small>Small Text</small>"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :use-markup t
                                         :label "<b>Bold Text</b>"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :use-markup t
                                         :label
                                         (format nil
                                         "Go to ~
                                         <a href=\"http://gtk.org/\">~
                                         GTK Website</a> ...")))
    ;; Create a Line wrapped label
    (gtk:box-append vbox2 (make-header "Line Wrapped Label"))
    (gtk:box-append vbox2 (make-instance 'gtk:label
                                         :wrap t
                                         :label *lorem-ipsum-short*))
    ;; Create a Filled and wrapped label
    (gtk:box-append vbox2 (make-header "Filled and Wrapped Label"))
    (gtk:box-append vbox2 (make-instance 'gtk:label
                                        :wrap t
                                        :justify :fill
                                        :label *lorem-ipsum-short*))
    ;; Put the boxes into the window and show the window
    (gtk:box-append hbox vbox1)
    (gtk:box-append hbox (gtk:separator-new :vertical))
    (gtk:box-append hbox vbox2)
    (gtk:widget-show window)))
