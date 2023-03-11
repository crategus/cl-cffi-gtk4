;;;; Text View Search - 2022-11-18

(in-package :gtk4-example)

(defun find-next-match (textview text iter &key (direction :forward))
  (let ((buffer (gtk:text-view-buffer textview)))
    (multiple-value-bind (found start end)
        (gtk:text-iter-search iter text :direction direction)
      (when found
        (gtk:text-buffer-select-range buffer start end)
        (gtk:text-buffer-create-mark buffer "last-start" start)
        (let ((last-end (gtk:text-buffer-create-mark buffer "last-end" end)))
          (gtk:text-view-scroll-mark-onscreen textview last-end))))))

(defun do-text-view-search (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :application application
                                :child vbox
                                :title "Text View Find Next"
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
                  (last-end (gtk:text-buffer-mark buffer "last-end"))
                  (iter (gtk:text-buffer-iter-at-mark buffer last-end)))
             (when last-end
               (find-next-match textview text iter)))))
    ;; Find the previous match
    (g:signal-connect entry "previous-match"
        (lambda (widget)
           (let* ((text (gtk:editable-text widget))
                  (buffer (gtk:text-view-buffer textview))
                  (last-start (gtk:text-buffer-mark buffer "last-start"))
                  (iter (gtk:text-buffer-iter-at-mark buffer last-start)))
             (when last-start
               (find-next-match textview text iter :direction :backward)))))
    ;; Set some text into the text buffer
    (setf (gtk:text-buffer-text (gtk:text-view-buffer textview))
          *some-text*)
    ;; Pack and show the widgets
    (gtk:box-append toolbar entry)
    (gtk:box-append vbox toolbar)
    (gtk:box-append vbox scrolled)
    (gtk:widget-show window)))
