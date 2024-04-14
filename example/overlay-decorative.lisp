;;;; Decorative Overlay
;;;;
;;;; Another example of an overlay with some decorative and some interactive
;;;; controls.
;;;;
;;;; 2024-4-13

(in-package :gtk4-example)

(defun do-overlay-decorative (&optional application)
  (let* ((view (make-instance 'gtk:text-view
                              :wrap-mode :word))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hscrollbar-policy :automatic
                                  :vscrollbar-policy :automatic))
         (overlay (make-instance 'gtk:overlay
                                 :child scrolled))
         (window (make-instance 'gtk:window
                                :title "Overlay Decorative"
                                :child overlay
                                :application application
                                :default-width 500
                                :default-height 510))
         (buffer (gtk:text-view-buffer view))
         (tag (gtk:text-buffer-create-tag buffer
                                          "top-margin"
                                          :pixels-above-lines 0))
         (adjustment (gtk:adjustment-new 0 0 200 1 1 0)))
    (g:signal-connect adjustment "value-changed"
        (lambda (adjustment)
          (let ((value (truncate (gtk:adjustment-value adjustment))))
            (setf (gtk:text-view-left-margin view) value
                  (gtk:text-view-right-margin view) value
                  (gtk:text-tag-pixels-above-lines tag) value))))
    (setf (gtk:text-buffer-text buffer) *lorem-ipsum-short*)
    (let* ((start (gtk:text-buffer-start-iter buffer))
           (end (gtk:text-iter-copy start)))
      (gtk:text-iter-move end :by :word)
      (gtk:text-buffer-apply-tag buffer tag start end))
    (let* ((path (sys-path "resource/decor1.png"))
           (image (gtk:picture-new-for-filename path)))
      (setf (gtk:widget-can-target image) nil
            (gtk:widget-halign image) :start
            (gtk:widget-valign image) :start)
      (gtk:overlay-add-overlay overlay image))
    (let* ((path (sys-path "resource/decor2.png"))
           (image (gtk:picture-new-for-filename path)))
      (setf (gtk:widget-can-target image) nil
            (gtk:widget-halign image) :end
            (gtk:widget-valign image) :end)
      (gtk:overlay-add-overlay overlay image))
    (let ((scale (make-instance 'gtk:scale
                                :orientation :horizontal
                                :adjustment adjustment
                                :draw-value nil
                                :width-request 120
                                :height-request -1
                                :margin-start 20
                                :margin-end 20
                                :margin-bottom 20
                                :halign :start
                                :valign :end
                                :tooltip-text "Control margin")))
      (gtk:overlay-add-overlay overlay scale)
      (setf (gtk:adjustment-value adjustment) 100))
    ;; Present window
    (gtk:window-present window)))
