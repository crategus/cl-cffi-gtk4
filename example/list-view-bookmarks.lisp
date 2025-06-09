;;;; List View Bookmarks
;;;;
;;;; This demo uses the GtkListView widget to display the content of the
;;;; default bookmark file.
;;;;
;;;; 2025-06-08

(in-package :gtk4-example)

(defun do-list-view-bookmarks (&optional application)
  (let* ((factory (gtk:signal-list-item-factory-new))
         (model (gtk:bookmark-list-new nil "standard::*"))
         (listview (gtk:list-view-new (gtk:single-selection-new model) factory))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :vexpand t))
         (status (make-instance 'gtk:label
                                :label "... is loading"
                                :xalign 0.0
                                :margin-start 6
                                :margin-top 6
                                :margin-bottom 6))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Bookmarks"
                                :application application
                                :child vbox
                                :default-width 640
                                :default-height 320)))
    ;; Update status line
    (g:signal-connect model "notify::loading"
        (lambda (model pspec)
          (declare (ignore pspec))
          (if (gtk:bookmark-list-is-loading model)
              (setf (gtk:label-label status)
                    (format nil "... is loading"))
              (let ((n (gtk:bookmark-list-n-items model))
                    (filename (gtk:bookmark-list-filename model)))
                (setf (gtk:label-label status)
                      (format nil "~a items in ~a" n filename))))))
    (g:signal-connect factory "setup"
        (lambda (factory item)
          (declare (ignore factory))
          (let* ((box (gtk:box-new :horizontal 12))
                 (image (make-instance 'gtk:image
                                       :icon-size :large)))
            (gtk:box-append box image)
            (gtk:box-append box (gtk:label-new ""))
            (setf (gtk:list-item-child item) box))))
    (g:signal-connect factory "bind"
        (lambda (factory item)
          (declare (ignore factory))
          (let* ((image (gtk:widget-first-child (gtk:list-item-child item)))
                 (label (gtk:widget-next-sibling image))
                 (fileinfo (gtk:list-item-item item)))
            (gtk:image-set-from-gicon image (g:file-info-icon fileinfo))
            (setf (gtk:label-label label)
                  (g:file-info-name fileinfo)))))
    ;; Append scrolled window and status line to vertical box
    (gtk:box-append vbox scrolled)
    (gtk:box-append vbox status)
    ;; Set child widget of the scrolled window
    (setf (gtk:scrolled-window-child scrolled) listview)
    ;; Present window
    (gtk:window-present window)))
