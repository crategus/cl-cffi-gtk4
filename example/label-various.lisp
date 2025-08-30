;;;; Various Label
;;;;
;;;; The <tt>gtk:label</tt> widget displays a small amount of text. As the name
;;;; implies, most labels are used to label another widget such as a
;;;; <tt>gtk:button</tt> widget.
;;;;
;;;; Labels may contain mnemonics and can be made selectable. Selectable labels
;;;; allow the user to copy the label contents to the clipboard.  To make it
;;;; easy to format text in a label, changing colors, fonts, and so on, label
;;;; text can be provided in a simple markup format.
;;;;
;;;; A label can contain any number of paragraphs, but will have performance
;;;; problems if it contains more than a small number. Paragraphs are separated
;;;; by newlines or other paragraph separators understood by Pango.
;;;;
;;;; GTK supports markup for clickable hyperlinks in addition to regular Pango
;;;; markup.
;;;;
;;;; Last updated: 2025-08-29

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
                                :title "Various Label"
                                :child hbox
                                :default-width 250
                                :border-width 18))
         (vbox1 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6))
         (vbox2 (make-instance 'gtk:box
                               :orientation :vertical
                               :spacing 6)))
    ;; Create Normal Label
    (gtk:box-append vbox1 (make-header "Normal Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label :label *text1*))
    ;; Create Multi-line Label
    (gtk:box-append vbox1 (make-header "Multiline Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label :label *text2*))
    ;; Create Left Justified Label
    (gtk:box-append vbox1 (make-header "Center Justified Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :justify :center
                                         :label *text3*))
    ;; Create Right Justified Label
    (gtk:box-append vbox1 (make-header "Right Justified Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :justify :right
                                         :label *text4*))
    ;; Create mnemonic label
    (gtk:box-append vbox1 (make-header "Mnemonic Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :use-underline t
                                         :label "With _Mnemonic"))
    ;; Create selectable label
    (gtk:box-append vbox1 (make-header "Selectable Label"))
    (gtk:box-append vbox1 (make-instance 'gtk:label
                                         :selectable t
                                         :label "This label is selectable"))
    ;; Create label with markup
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
    ;; Create line wrapped label
    (gtk:box-append vbox2 (make-header "Line Wrapped Label"))
    (gtk:box-append vbox2 (make-instance 'gtk:label
                                         :wrap t
                                         :label *lorem-ipsum-short*))
    ;; Create filled and wrapped label
    (gtk:box-append vbox2 (make-header "Filled and Wrapped Label"))
    (gtk:box-append vbox2 (make-instance 'gtk:label
                                        :wrap t
                                        :justify :fill
                                        :label *lorem-ipsum-short*))
    ;; Put boxes into the window and present the window
    (gtk:box-append hbox vbox1)
    (gtk:box-append hbox (gtk:separator-new :vertical))
    (gtk:box-append hbox vbox2)
    (gtk:window-present window)))
