;;;; Header Bar
;;;;
;;;; The <tt>GtkHeaderBar</tt> widget is a container that is suitable for
;;;; implementing window titlebars. One of its features is that it can position
;;;; a title, and optional subtitle, centered with regard to the full width,
;;;; regardless of variable-width content at the left or right. It is commonly
;;;; used with the <tt>gtk:window-titlebar</tt> function.
;;;;
;;;; 2025-2-16

(in-package :gtk4-example)

(defun do-header-bar (&optional application)
  (let* ((header (make-instance 'gtk:header-bar
                                :decoration-layout ":close"
                                :title-widget (gtk:label-new "Header Bar")))
         (window (make-instance 'gtk:window
                                :title "No title"
                                :application application
                                :titlebar header
                                :default-width 300
                                :default-height 180)))
    ;; Button at the right side of the header bar
    (gtk:header-bar-pack-end header
                             (make-instance 'gtk:button
                                            :icon-name "mail-send-receive"
                                            :tooltip-text "Check out"))
    ;; Box with two buttons at the left side of the header bar
    (let ((box (make-instance 'gtk:box
                              :orientation :horizontal)))
      (gtk:widget-add-css-class box "linked")
      (gtk:box-append box
                      (make-instance 'gtk:button
                                     :icon-name "go-previous"
                                     :tooltip-text "Back"))
      (gtk:box-append box
                      (make-instance 'gtk:button
                                     :icon-name "go-next"
                                     :tooltip-text "Forward"))
      (gtk:header-bar-pack-start header box))
    ;; Present window
    (gtk:window-present window)))
