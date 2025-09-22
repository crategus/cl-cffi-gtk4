;;;;  Text View Attributes
;;;;
;;;; This examples uses a tag to change the color for just one word in the
;;;; text view. CSS data is also loaded to change the font and the color
;;;; for the text view.
;;;;
;;;; 2025-09-22

(in-package :gtk4-example)

(defun do-text-view-attributes (&optional application)
  (let* ((textview (make-instance 'gtk:text-view
                                  ;; Change left margin throughout the widget
                                  :left-margin 24
                                  ;; Change top margin
                                  :top-margin 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child textview
                                :title "Text View Attributes"
                                :default-width 350
                                :default-height 200))
         (provider (gtk:css-provider-new))
         (buffer (gtk:text-view-buffer textview)))
    (setf (gtk:text-buffer-text buffer) "Hello, this is some text.")
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-string provider
                                       ".viewstyle textview {
                                          color : Green;
                                          font : 20px Purisa; }")
    (gtk:widget-add-css-class window "viewstyle")
    (gtk:widget-add-provider window provider)
    ;; Use a tag to change the color for just one part of the text view
    (let ((tag (gtk:text-buffer-create-tag buffer
                                           "blue_foreground"
                                           :foreground "blue"))
          (start (gtk:text-buffer-iter-at-offset buffer 7))
          (end (gtk:text-buffer-iter-at-offset buffer 12)))
      ;; Apply the tag to a region of the text in the buffer
      (gtk:text-buffer-apply-tag buffer tag start end))
    ;; Show the window
    (gtk:window-present window)))
