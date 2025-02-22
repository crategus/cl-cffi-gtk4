;;;; Link button
;;;;
;;;; The <tt>gtk:link-button</tt> widget is a <tt>gtk:button</tt> widget with
;;;; a hyperlink, similar to the one used by web browsers, which triggers an
;;;; action when clicked. It is useful to show quick links to resources. A link
;;;; button is created by calling either the <tt>gtk:link-button-new</tt> or
;;;; <tt>gtk:link-button-new-with-label</tt> function. If using the former, the
;;;; URI you pass to the constructor is used as a label for the widget.
;;;;
;;;; The URI bound to a <tt>gtk:link-button</tt> widget can be set specifically
;;;; or retrieved using the <tt>gtk:link-button-uri</tt> function.
;;;;
;;;; By default, the <tt>gtk:link-button</tt> widget calls the
;;;; <tt>gtk:file-launcher-launch</tt> function when the button is clicked.
;;;; This behaviour can be overridden by connecting to the "activate-link"
;;;; signal and returning true from the signal handler.
;;;;
;;;; 2025-2-22

(in-package :gtk4-example)

(defun do-link-button (&optional application)
  (let* ((css "frame { border-style: solid;
                       border-radius: 0;
                       border-color: rgb(1,1,1); }" )
         (vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :row-spacing 6
                              :homogeneous t
                              :margin-top 12
                              :margin-bottom 24
                              :margin-start 48
                              :margin-end 48))
         (frame (make-instance 'gtk:frame
                               :child vbox))
         (window (make-instance 'gtk:window
                                :title "Link Buttons"
                                :child frame
                                :application application))
         (provider (gtk:css-provider-new))
         button)
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider css)
    (gtk:widget-add-provider frame provider)
    ;; Create and pack link buttons
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :use-markup t
                                   :label
                                   "<b>Link Button with URL</b>"))
    (gtk:box-append vbox
                    (setf button
                          (gtk:link-button-new "http://www.gtk.org/")))
    (setf (gtk:link-button-visited button) nil)
    (gtk:box-append vbox
                    (make-instance 'gtk:label
                                   :margin-top 12
                                   :use-markup t
                                   :label
                                   "<b>Link Button with Label</b>"))
    (gtk:box-append vbox
                    (setf button
                          (gtk:link-button-new-with-label "http://www.gtk.org/"
                                                          "Project WebSite")))
    (setf (gtk:link-button-visited button) nil)
    (gtk:window-present window)))
