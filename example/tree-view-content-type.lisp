;;;; Tree View Content Type
;;;;
;;;; 2024-4-6

(in-package :gtk4-example)

(let ((col-icon 0) (col-icon-name 1) (col-mime-type 2) (col-desc 3))

  (defun create-and-fill-model-content-type ()
    (let ((data (g:content-types-registered))
          (model (gtk:list-store-new "gchararray"
                                     "gchararray" "gchararray" "gchararray")))
;          (icon-theme (gtk:icon-theme-for-display (gdk:display-default))))
      (dolist (mime-type data)
        (let* ((description (g:content-type-description mime-type))
               (icon-name (g:content-type-generic-icon-name mime-type)))
;               (icon (gtk:icon-theme-lookup-icon icon-theme
;                                                 icon-name
;                                                 nil
;                                                 48
;                                                 1
;                                                 :none
;                                                 :none)))
          (gtk:list-store-set model (gtk:list-store-append model)
                                    icon-name
                                    icon-name
                                    mime-type
                                    description)))
      model))

  (defun create-view-and-model-content-type ()
    (let* ((model (create-and-fill-model-content-type))
           (view (gtk:tree-view-new-with-model model)))
     ;; First column displays icon and icon-name
     (let ((column (gtk:tree-view-column-new))
           (renderer (gtk:cell-renderer-pixbuf-new)))
       (setf (gtk:tree-view-column-title column) "Icon")
       (gtk:tree-view-column-pack-start column renderer :expand nil)
       (gtk:tree-view-column-set-attributes column
                                            renderer
                                            "icon-name"
                                            col-icon)
       (setf renderer (gtk:cell-renderer-text-new))
       (gtk:tree-view-column-pack-start column renderer :expand nil)
       (gtk:tree-view-column-set-attributes column
                                            renderer
                                            "text"
                                            col-icon-name)
        (gtk:tree-view-append-column view column))
      ;; Second column for the MIME Type
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "MIME Type"
                                                               renderer
                                                               "text"
                                                                col-mime-type)))
        (gtk:tree-view-append-column view column))
      ;; Third column for the description
      (let* ((renderer (gtk:cell-renderer-text-new))
             (column (gtk:tree-view-column-new-with-attributes "Descripton"
                                                               renderer
                                                               "text"
                                                               col-desc)))
        (gtk:tree-view-append-column view column))
      view))

  (defun do-tree-view-content-type (&optional application)
    (let* ((view (create-view-and-model-content-type))
           (scrolled (make-instance 'gtk:scrolled-window
                                    :child view))
           (window (make-instance 'gtk:window
                                  :title "Tree View Content Type"
                                  :child scrolled
                                  :application application
                                  :default-width 550
                                  :default-height 350)))
        (gtk:window-present window))))
