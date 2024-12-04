;;;; Entry Buffer
;;;;
;;;; <tt>GtkEntryBuffer</tt> provides the text content in a <tt>GtkEntry</tt>.
;;;;
;;;; 2024-11-26

(in-package :gtk4-example)

(defun do-entry-buffer (&optional application)
  (let* ((hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :spacing 30
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title "Entry Buffer"
                                :child hbox
                                :application application
                                :default-width 360
                                :resizable nil))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 12))
        ;; The entry buffer for the entries of this example
        (buffer (make-instance 'gtk:entry-buffer)))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :label "<b>First Entry</b>"
                                   :halign :start
                                   :use-markup t))
    (gtk:box-append vbox
                    (make-instance 'gtk:entry
                                   :buffer buffer))
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :label "<b>Second Entry</b>"
                                   :halign :start
                                   :use-markup t))
    (gtk:box-append vbox
                    (make-instance 'gtk:entry
                                   :buffer buffer))
    (gtk:box-append hbox vbox)
    (gtk:box-append hbox
                    (make-instance 'gtk:label
                                   :valign :start
                                   :wrap t
                                   :label
                                   (format nil
                                           "Both entries have the same  entry ~
                                            buffer object. Typ in some text in ~
                                            one of the entries to see the ~
                                            effect.")))
    (gtk:window-present window)))
