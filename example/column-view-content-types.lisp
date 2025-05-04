;;;; Content Types
;;;;
;;;; 2025-05-03

(in-package :gtk4-example)

(defun create-column-view-for-content-types ()
  (let* ((model (gtk:string-list-new (g:content-types-registered)))
         (selection (make-instance 'gtk:single-selection
                                   :model model
                                   :autoselect t
                                   :can-unselet nil))
         (columnview (make-instance 'gtk:column-view
                                    :model selection
                                    :show-column-separators t)))
    ;; First column for "Content type"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (gtk:column-view-column-new "Content type" factory)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
              (let ((box (gtk:box-new :horizontal 12))
                    (image (make-instance 'gtk:image
                                          :icon-size :large)))
                (gtk:box-append box image)
                (gtk:box-append box (make-instance 'gtk:inscription
                                                   :text ""
                                                   :min-chars 20
                                                   :xalign 0.0))
                (setf (gtk:list-item-child listitem) box))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((item  (gtk:list-item-item listitem))
                   (child (gtk:list-item-child listitem))
                   (image (gtk:widget-first-child child))
                   (label (gtk:widget-next-sibling image))
                   (ctype (gtk:string-object-string item)))
              (gtk:image-set-from-gicon image (g:content-type-icon ctype))
              (setf (gtk:inscription-text label) ctype))))
      (setf (gtk:column-view-column-resizable column) t)
      (gtk:column-view-append-column columnview column))
    ;; Second column for "Default Application"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (gtk:column-view-column-new "Default Application" factory)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((box (gtk:box-new :horizontal 12))
                   (image (make-instance 'gtk:image
                                         :icon-size :large)))
              (gtk:box-append box image)
              (gtk:box-append box (gtk:label-new ""))
            (setf (gtk:list-item-child listitem) box))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((item (gtk:column-view-cell-item listitem))
                   (child (gtk:list-item-child listitem))
                   (image (gtk:widget-first-child child))
                   (label (gtk:widget-next-sibling image))
                   (ctype (gtk:string-object-string item)))
              (g:app-info-default-for-type-async ctype nil nil
                  (lambda (source result)
                    (declare (ignore source))
                    (let* ((info (g:app-info-default-for-type-finish result)))
                      (when info
                        (gtk:image-set-from-gicon image (g:app-info-icon info))
                        (setf (gtk:label-label label)
                              (g:app-info-display-name info)))))))))
      (setf (gtk:column-view-column-resizable column) t)
      (gtk:column-view-append-column columnview column))
    ;; Third column for "Description"
    (let* ((factory (gtk:signal-list-item-factory-new))
           (column (gtk:column-view-column-new "Description" factory)))
      (g:signal-connect factory "setup"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let ((label (make-instance 'gtk:inscription
                                        :min-chars 30)))
              (setf (gtk:list-item-child listitem) label))))
      (g:signal-connect factory "bind"
          (lambda (factory listitem)
            (declare (ignore factory))
            (let* ((label (gtk:list-item-child listitem))
                   (item  (gtk:list-item-item listitem))
                   (ctype (gtk:string-object-string item)))
              (setf (gtk:inscription-text label)
                    (g:content-type-description ctype)))))
      (setf (gtk:column-view-column-expand column) t)
      (setf (gtk:column-view-column-resizable column) t)
      (gtk:column-view-append-column columnview column))
    columnview))

(defun do-column-view-content-types (&optional application)
  (let* ((columnview (create-column-view-for-content-types))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child columnview
                                  :propagate-natural-width t))
         (window (make-instance 'gtk:window
                                :title "Content Types"
                                :application application
                                :child scrolled
                                :default-width 800
                                :default-height 600)))
    ;; Present the window
    (gtk:window-present window)))
