;;;; Recent Manager
;;;;
;;;; This demo uses the GtkListView widget as a fancy application launcher.
;;;; It is also a very small introduction to listviews.
;;;;
;;;; 2025-4-1

(in-package :gtk4-example)

;; A wrapper object that holds a GtkRecentInfo instance
(gobject:define-gobject-subclass "RecentObject" recent-object
  (:superclass g:object
   :export t
   :interfaces nil)
  ((item
    recent-object-item
    "item" "GtkRecentInfo" t t)))

;; This function is called whenever an item in the list is activated. This is
;; the simple way to allow reacting to the Enter key or double-clicking on a
;; listitem. Of course, it is possible to use far more complex interactions by
;; turning off activation and adding buttons or other widgets in the setup
;; function above, but this is a simple demo, so we'll use the simple way.
(defun activate-recentinfo-cb (listview position)
  (format t "in ACTiVATE-RECENTINFO-CB~%")
  (let* ((model (gtk:list-view-model listview))
         (recentinfo (recent-object-item (g:list-model-item model position)))
         appname)

    (format t "display-name : ~a~%" (gtk:recent-info-display-name recentinfo))
    (format t "         uri : ~a~%" (gtk:recent-info-uri recentinfo))
    (format t " description : ~a~%" (gtk:recent-info-description recentinfo))
    (format t "   mime-type : ~a~%" (gtk:recent-info-mime-type recentinfo))
    (format t "       added : ~a~%"
            (local-time:universal-to-timestamp (gtk:recent-info-added recentinfo)))
    (format t "    modified : ~a~%"
            (local-time:universal-to-timestamp (gtk:recent-info-modified recentinfo)))
    (format t "     visited : ~a~%"
            (local-time:universal-to-timestamp (gtk:recent-info-visited recentinfo)))
    (format t "applications : ~a~%" (gtk:recent-info-applications recentinfo))
    (when (setf appname (first (gtk:recent-info-applications recentinfo)))
      (format t "        exec : ~a~%" (gtk:recent-info-application-info recentinfo appname)))
))

;; This is the function that creates the GListModel that we need. GTK list
;; widgets need a GListModel to display, as models support change notifications.
;; Unfortunately various older APIs do not provide list models, so we create
;; our own.
(defun create-recentinfo-list ()
  (let (;; We use a GListStore here, which is a simple array-like list
        ;; implementation for manual management. List models need to know what
        ;; type of data they provide, so we need to provide the type here. As
        ;; we want to do a list of applications, GAppInfo is the object we
        ;; provide.
        (store (g:list-store-new "RecentObject"))
        (items (gtk:recent-manager-items (gtk:recent-manager-default))))
    (dolist (item items)
      (g:list-store-append store (make-instance 'recent-object :item item)))
  store))

(defun do-list-view-recentinfo (&optional (application nil))
  (let* (;; The GtkListitemFactory is what is used to create GtkListItems
         ;; to display the data from the model. So it is absolutely necessary
         ;; to create one. We will use a GtkSignalListItemFactory because it is
         ;; the simplest one to use. Different ones are available for different
         ;; use cases. The most powerful one is GtkBuilderListItemFactory which
         ;; uses GtkBuilder UI files, so it requires little code.
         (factory (gtk:signal-list-item-factory-new))
         ;; And of course we need to set the data model. Here we call the
         ;; function we wrote above that gives us the list of applications.
         ;; Then we set it on the list widget. The list will now take items
         ;; from the model and use the factory to create as many listitems as
         ;; it needs to show itself to the user.
         (model (create-recentinfo-list))
         (listview nil)
         (scrolled (make-instance 'gtk:scrolled-window
                                  :margin-start 12
                                  :margin-top 12))
         (window (make-instance 'gtk:window
                                :title "Recent Manager"
                                :application application
                                :child scrolled
                                :default-width 640
                                :default-height 320
                                :margin-start 24
                                :margin-top 6
                                :margin-bottom 6)))
    ;; This is the handler we use for setting up new list items to display.
    ;; We add just an GtkImage and a GtkLabel here to display the icon and name
    ;; of the application, as this is just a simple demo.
    (g:signal-connect factory "setup"
        (lambda (factory item)
          (declare (ignore factory))
          (let* ((box (gtk:box-new :horizontal 12))
                 (image (make-instance 'gtk:image
                                       :icon-size :large)))
            (gtk:box-append box image)
            (gtk:box-append box (gtk:label-new ""))
            (setf (gtk:list-item-child item) box))))
    ;; Here we need to prepare the list item for displaying its item. We get
    ;; the list item already set up from the previous function, so we can
    ;; reuse the GtkImage widget we set up above. We get the item - which we
    ;; know is a GAppInfo because it comes out of the model we set up above,
    ;; grab its icon and display it.
    (g:signal-connect factory "bind"
        (lambda (factory item)
          (declare (ignore factory))
          (let* ((image (gtk:widget-first-child (gtk:list-item-child item)))
                 (label (gtk:widget-next-sibling image))
                 (recentinfo (recent-object-item (gtk:list-item-item item))))
            (gtk:image-set-from-gicon image (gtk:recent-info-gicon recentinfo))
            (setf (gtk:label-label label)
                  (gtk:recent-info-short-name recentinfo)))))
    ;; Create the list widget here.
    (setf listview
          (gtk:list-view-new (gtk:single-selection-new model) factory))
    ;; We connect the activate signal here. It's the function we defined
    ;; above for launching the selected application.
    (g:signal-connect listview "activate" #'activate-recentinfo-cb)
    ;; Set the child widget of the scrolled window
    (setf (gtk:scrolled-window-child scrolled) listview)
    ;; Set the window visible
    (gtk:window-present window)))
