;;;; Text View Search
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(defun do-text-view-search (&optional application)
  (flet ((find-next-match (textview text iter &key (direction :forward))
           (let ((buffer (gtk:text-view-buffer textview)))
             (multiple-value-bind (found start end)
                 (gtk:text-iter-search iter text :direction direction)
               (when found
                 (gtk:text-buffer-select-range buffer start end)
                 (gtk:text-buffer-create-mark buffer "laststart" start)
                 (let ((lastend (gtk:text-buffer-create-mark buffer
                                                             "lastend"
                                                             end)))
                   (gtk:text-view-scroll-mark-onscreen textview lastend)))))))
    (let* ((vbox (make-instance 'gtk:box
                                :orientation :vertical))
           (window (make-instance 'gtk:window
                                  :application application
                                  :child vbox
                                  :title "Text View Search"
                                  :default-width 350
                                  :default-height 250))
           (entry (make-instance 'gtk:search-entry))
           (toolbar (make-instance 'gtk:box
                                   :orientation :horizontal
                                   :spacing 3))
           (textview (make-instance 'gtk:text-view
                                    :wrap-mode :word
                                    :pixels-inside-wrap 3
                                    :top-margin 6
                                    :left-margin 6
                                    :right-margin 6))
           (scrolled (make-instance 'gtk:scrolled-window
                                    :child textview
                                    :vexpand t
                                    :hexpand t)))
      ;; Search and select the text in the text buffer
      (g:signal-connect entry "search-changed"
          (lambda (widget)
            (let* ((text (gtk:editable-text widget))
                   (buffer (gtk:text-view-buffer textview))
                   (iter (gtk:text-buffer-start-iter buffer)))
              (find-next-match textview text iter))))
      ;; Find the next match
      (g:signal-connect entry "next-match"
          (lambda (widget)
             (let* ((text (gtk:editable-text widget))
                    (buffer (gtk:text-view-buffer textview))
                    (lastend (gtk:text-buffer-mark buffer "lastend"))
                    (iter (gtk:text-buffer-iter-at-mark buffer lastend)))
               (when lastend
                 (find-next-match textview text iter)))))
      ;; Find the previous match
      (g:signal-connect entry "previous-match"
           (lambda (widget)
             (let* ((text (gtk:editable-text widget))
                    (buffer (gtk:text-view-buffer textview))
                    (laststart (gtk:text-buffer-mark buffer "laststart"))
                    (iter (gtk:text-buffer-iter-at-mark buffer laststart)))
               (when laststart
                 (find-next-match textview text iter :direction :backward)))))
      ;; Set some text into the text buffer
      (setf (gtk:text-buffer-text (gtk:text-view-buffer textview))
            *some-text*)
      ;; Pack and show the widgets
      (gtk:box-append toolbar entry)
      (gtk:box-append vbox toolbar)
      (gtk:box-append vbox scrolled)
      (gtk:window-present window))))
