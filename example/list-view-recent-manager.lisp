;;;; Recent Manager
;;;;
;;;; This demo uses the GtkListView widget to display a list of recently used
;;;; files. It defines a new RECENT-OBJECT subclass to wrap the gtk:recent-info
;;;; instances to a class of type GObject.
;;;;
;;;; 2025-05-03

(in-package :gtk4-example)

;; A wrapper object that holds a GtkRecentInfo instance
(gobject:define-gobject-subclass "RecentObject" recent-object
  (:superclass g:object
   :export t
   :interfaces nil)
  ((item
    recent-object-item
    "item" "GtkRecentInfo" t t)))

;; This is the function that creates the GListModel that we need
(defun create-recentinfo-list ()
  (let ((store (g:list-store-new "RecentObject"))
        (items (gtk:recent-manager-items (gtk:recent-manager-default))))
    (dolist (item items)
      (g:list-store-append store (make-instance 'recent-object :item item)))
  store))

;; This function is called whenever an item in the list is activated
(defun activate-recentinfo-cb (builder dialog listview pos)
  (let* ((model (gtk:list-view-model listview))
         (recentinfo (recent-object-item (g:list-model-item model pos))))
    ;; Fill labels of the dialog
    (setf (gtk:label-label (gtk:builder-object builder "display_name"))
          (gtk:recent-info-display-name recentinfo))
    (setf (gtk:label-label (gtk:builder-object builder "uri"))
          (gtk:recent-info-uri recentinfo))
    (when (gtk:recent-info-description recentinfo)
      (setf (gtk:label-label (gtk:builder-object builder "description"))
            (gtk:recent-info-description recentinfo)))
    (setf (gtk:label-label (gtk:builder-object builder "mime_type"))
          (gtk:recent-info-mime-type recentinfo))
    (setf (gtk:label-label (gtk:builder-object builder "added"))
          (format nil "~a"
                  (local-time:universal-to-timestamp
                      (gtk:recent-info-added recentinfo))))
    (setf (gtk:label-label (gtk:builder-object builder "modified"))
          (format nil "~a"
                  (local-time:universal-to-timestamp
                      (gtk:recent-info-modified recentinfo))))
    (setf (gtk:label-label (gtk:builder-object builder "visited"))
          (format nil "~a"
                  (local-time:universal-to-timestamp
                      (gtk:recent-info-visited recentinfo))))
    (setf (gtk:label-label (gtk:builder-object builder "applications"))
          (format nil "~{~a~^, ~}"
                  (gtk:recent-info-applications recentinfo)))
    ;; Present dialog
    (gtk:window-present dialog)))

(defun do-list-view-recent-manager (&optional application)
  (let* ((path (glib-sys:sys-path "resource/dialog-recent-info.ui"))
         (builder (gtk:builder-new-from-file path))
         (dialog (gtk:builder-object builder "dialog"))
         (factory (gtk:signal-list-item-factory-new))
         (model (create-recentinfo-list))
         (listview (gtk:list-view-new (gtk:single-selection-new model) factory))
         (scrolled (make-instance 'gtk:scrolled-window))
         (window (make-instance 'gtk:window
                                :title "Recent Manager"
                                :application application
                                :child scrolled
                                :default-width 640
                                :default-height 320)))
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
                 (recentinfo (recent-object-item (gtk:list-item-item item))))
            (gtk:image-set-from-gicon image (gtk:recent-info-gicon recentinfo))
            (setf (gtk:label-label label)
                  (gtk:recent-info-short-name recentinfo)))))
    ;; We connect the activate signal here
    (g:signal-connect listview "activate"
                      (lambda (listview pos)
                        (activate-recentinfo-cb builder dialog listview pos)))
    ;; Set the child widget of the scrolled window
    (setf (gtk:scrolled-window-child scrolled) listview)
    (setf (gtk:window-transient-for dialog) window)
    ;; Set the window visible
    (gtk:window-present window)))
