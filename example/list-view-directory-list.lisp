;;;; List View Directory List
;;;;
;;;; This demo uses the GtkListView widget and the GtkDirectoryList object to
;;;; display a directory list.
;;;;
;;;; 2025-06-15

(in-package :gtk4-example)

(defun do-list-view-directory-list (&optional application)
  (let* ((path (glib-sys:sys-path ""))
         (model (gtk:directory-list-new "standard::*" (namestring path)))

         (factory (gtk:signal-list-item-factory-new))
         (listview (gtk:list-view-new (gtk:single-selection-new model) factory))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :vexpand t))
         (status (make-instance 'gtk:label
                                :label "... is loading"
                                :xalign 0.0))
         (actionbar (make-instance 'gtk:action-bar))
         (vbox (make-instance 'gtk:box
                              :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Directory List"
                                :application application
                                :child vbox
                                :default-width 640
                                :default-height 320)))
    ;; Update status label
    (g:signal-connect model "notify::loading"
        (lambda (model pspec)
          (declare (ignore pspec))
          (if (gtk:directory-list-is-loading model)
              (setf (gtk:label-label status)
                    (format nil "... is loading"))
              (let ((n (gtk:directory-list-n-items model))
                    (file (gtk:directory-list-file model)))
                (setf (gtk:label-label status)
                      (format nil "~a items in ~a" n (g:file-path file)))))))
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
    ;; Append scrolled window and action bar with status label to vertical box
    (gtk:box-append vbox scrolled)
    (gtk:action-bar-pack-start actionbar status)
    (gtk:box-append vbox actionbar)
    ;; Set child widget of the scrolled window
    (setf (gtk:scrolled-window-child scrolled) listview)
    ;; Present window
    (gtk:window-present window)))
