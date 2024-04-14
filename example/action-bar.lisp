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
;;;; 2024-4-14

(in-package :gtk4-example)

(defun do-action-bar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Action Bar"
                                :application application
                                :child vbox
                                :default-width 320
                                :default-height 180))
          (actionbar (make-instance 'gtk:action-bar)))
    (let* ((icon (g:themed-icon-new "media-skip-backward"))
           (image (gtk:image-new-from-gicon icon))
           (button (make-instance 'gtk:button
                                  :child image)))
      (gtk:action-bar-pack-start actionbar button))
    (let* ((icon (g:themed-icon-new "media-playback-start"))
           (image (gtk:image-new-from-gicon icon))
           (button (make-instance 'gtk:button
                                  :child image)))
        (gtk:action-bar-pack-start actionbar button))
    (let* ((icon (g:themed-icon-new "media-skip-forward"))
           (image (gtk:image-new-from-gicon icon))
           (button (make-instance 'gtk:button
                                  :child image)))
      (gtk:action-bar-pack-start actionbar button))
      (gtk:box-append vbox (make-instance 'gtk:text-view
                                          :vexpand t))
      (gtk:box-append vbox actionbar)
      (gtk:window-present window)))
