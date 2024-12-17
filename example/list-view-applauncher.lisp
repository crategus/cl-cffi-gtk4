;;;; Application launcher
;;;;
;;;; This demo uses the GtkListView widget as a fancy application launcher.
;;;; It is also a very small introduction to listviews.
;;;;
;;;; 2023-11-25

(in-package :gtk4-example)

;; This function is called whenever an item in the list is activated. This is
;; the simple way to allow reacting to the Enter key or double-clicking on a
;; listitem. Of course, it is possible to use far more complex interactions by
;; turning off activation and adding buttons or other widgets in the setup
;; function above, but this is a simple demo, so we'll use the simple way.
(defun activate-cb (listview position)
  (let* ((model (gtk:list-view-model listview))
         (appinfo (g:list-model-item model position))
         (display (gtk:widget-display listview))
         (context (gdk:display-app-launch-context display)))
    (unless (g:app-info-launch appinfo nil context)
      ;; And because error handling is important, even a simple demo has it.
      ;; We display an error dialog that something went wrong.
      (let* ((message (format nil "Could not launch ~a"
                                  (g:app-info-display-name appinfo)))
             (dialog (make-instance 'gtk:alert-dialog
                                    :message message)))
          (gtk:alert-dialog-show dialog (gtk:widget-root listview))))))

;; This is the function that creates the GListModel that we need. GTK list
;; widgets need a GListModel to display, as models support change notifications.
;; Unfortunately various older APIs do not provide list models, so we create
;; our own.
(defun create-application-list ()
  (let (;; We use a GListStore here, which is a simple array-like list
        ;; implementation for manual management. List models need to know what
        ;; type of data they provide, so we need to provide the type here. As
        ;; we want to do a list of applications, GAppInfo is the object we
        ;; provide.
        (store (g:list-store-new "GAppInfo"))
        (apps (g:app-info-all)))
    (dolist (app apps)
      (g:list-store-append store app))
  store))

(defun do-list-view-applauncher (&optional (application nil))
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
         (model (create-application-list))
         (listview nil)
         (scrolled (make-instance 'gtk:scrolled-window
                                  :margin-start 12
                                  :margin-top 12))
         (window (make-instance 'gtk:window
                                :title "Application launcher"
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
                 (appinfo (gtk:list-item-item item)))
            (gtk:image-set-from-gicon image (g:app-info-icon appinfo))
            (setf (gtk:label-label label)
                  (g:app-info-display-name appinfo)))))
    ;; Create the list widget here.
    (setf listview
          (gtk:list-view-new (gtk:single-selection-new model) factory))
    ;; We connect the activate signal here. It's the function we defined
    ;; above for launching the selected application.
    (g:signal-connect listview "activate" #'activate-cb)
    ;; Set the child widget of the scrolled window
    (setf (gtk:scrolled-window-child scrolled) listview)
    ;; Set the window visible
    (gtk:window-present window)))
