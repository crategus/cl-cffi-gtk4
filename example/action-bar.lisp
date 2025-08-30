;;;; Action Bar
;;;;
;;;; The <tt>gtk:action-bar</tt> widget is designed to present contextual
;;;; actions. It is expected to be displayed below the content and expand
;;;; horizontally to fill the area.
;;;;
;;;; It allows placing children at the start or the end. In addition, it
;;;; contains an internal centered box which is centered with respect to the
;;;; full width of the box, even if the children at either side take up
;;;; different amounts of space.
;;;;
;;;; 2025-08-30

(in-package :gtk4-example)

(defun do-action-bar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Action Bar"
                                :application application
                                :child vbox
                                :default-width 380
                                :default-height 180))
          (actionbar (make-instance 'gtk:action-bar)))
    ;; Pack start widgets
    (let ((image (gtk:image-new-from-icon-name "media-skip-backward")))
      (gtk:action-bar-pack-start actionbar
                                 (make-instance 'gtk:button
                                                :child image)))
    (let ((image (gtk:image-new-from-icon-name "media-playback-start")))
      (gtk:action-bar-pack-start actionbar
                                 (make-instance 'gtk:button
                                                :child image)))
    (let ((image (gtk:image-new-from-icon-name "media-skip-forward")))
      (gtk:action-bar-pack-start actionbar
                                 (make-instance 'gtk:button
                                                :child image)))
    ;; Pack end widgets
    (let ((image (gtk:image-new-from-icon-name "media-record")))
      (gtk:action-bar-pack-end actionbar
                               (make-instance 'gtk:button
                                              :child image)))
    (let ((image (gtk:image-new-from-icon-name "media-playback-stop")))
      (gtk:action-bar-pack-end actionbar
                               (make-instance 'gtk:button
                                              :child image)))
    ;; Set center widget
    (let ((image (gtk:image-new-from-icon-name "media-eject")))
      (setf (gtk:action-bar-center-widget actionbar)
            (make-instance 'gtk:button
                           :child image)))
    ;; Pack widgets and present window
    (gtk:box-append vbox (make-instance 'gtk:text-view
                                        :vexpand t))
    (gtk:box-append vbox actionbar)
    (gtk:window-present window)))
