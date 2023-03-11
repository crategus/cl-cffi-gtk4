(defpackage :gtk4-example
  (:use :cffi :split-sequence :common-lisp)
  (:import-from :gtk  #:+gtk-priority-application+
                      #:+gtk-priority-user+)
  (:import-from :gdk  #:+gdk-event-propagate+
                      #:+gdk-event-stop+)
  (:import-from :glib #:+g-source-continue+
                      #:+g-source-remove+)
  (:export #:run-example
           ;; Windows
           #:do-window-simple
           #:do-dialog-quick-message
           #:do-dialog-various
           #:do-message-dialog-simple
           #:do-message-dialog-simple2
           #:do-message-dialog-new
           #:do-message-dialog-new-with-markup
           #:do-message-dialog-with-markup
           #:do-message-dialog-with-secondary-markup
           #:do-assistant

           ;; Layout Containers
           #:do-box-append
           #:do-box-center
           #:do-grid-spacing
           #:do-revealer
           #:do-revealer-icon
           #:do-paned
           #:do-fixed

           ;; Display Widgets
           #:do-label-various
           #:do-images
           #:do-spinner
           #:do-info-bar
           #:do-progress-bar
           #:do-level-bar
           #:do-statusbar

           ;; Media Support
           #:do-video

           ;; Button and Toggle Widgets
           #:do-button-more
           #:do-toggle-button
           #:do-check-button
           #:do-link-button
           #:do-switch
           #:do-scale-button

           ;; Numeric/Text Data Entry
           #:do-entry
           #:do-entry-buffer
           #:do-entry-completion
           #:do-scale-widget
           #:do-spin-button

           ;; Multiline Text Editor
           #:do-text-view-simple
           #:do-text-view-attributes
           #:do-text-view-tags
           #:do-text-view-search
           #:do-text-view-tooltip

           ;; Tree, List and Icon Grid Widgets
           #:do-tree-view-simple
           #:do-tree-view-path
           #:do-tree-view-content-type
           #:do-icon-view

           ;; Menus, Combo Box, Toolbar
           #:do-combo-box
           #:do-combo-box-text

           ;; Selector Widgets and Dialogs
           #:do-color-button
           #:do-color-button-label
           #:do-color-chooser-dialog
           #:do-color-chooser-widget
           #:do-color-chooser-palette
           #:do-file-chooser-dialog
           #:do-font-button
           #:do-font-button-label

           ;; Ornaments
           #:do-frame
           #:do-frame-properties

           ;; Widgets for custom drawing
           #:do-drawing-area

           ;; Scrolling
           #:do-scrolled-window

           ;; Miscellaneous
           #:do-size-group
           #:do-event-controller

           ;; Theming in GTK
           #:do-css-accordion
           #:do-css-basics
           #:do-css-blendmodes
           #:do-css-multiplebgs
           #:do-css-pixbufs
           #:do-css-shadows
           ))

(in-package :gtk4-example)

(defvar *some-text*
        (format nil "One of the important things to remember about text in ~
                     GTK is that it is in the UTF-8 encoding. This means that ~
                     one character can be encoded as multiple bytes. Character ~
                     counts are usually referred to as offsets, while byte ~
                     counts are called indexes. If you confuse these two, ~
                     things will work fine with ASCII, but as soon as your ~
                     buffer contains multibyte characters, bad things will ~
                     happen."))

(defvar *lorem-ipsum-short*
        (format nil "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Nunc scelerisque aliquam dui id ullamcorper. Sed placerat felis sed aliquam ~
sodales. Cras et ultricies nulla. Nullam ipsum ante, gravida vel malesuada ac, ~
sollicitudin eu diam. Morbi pellentesque elit et odio hendrerit dignissim. ~
Maecenas sagittis auctor leo a dictum. Sed at auctor."))

;; Get the absolute filename of a file for a ASDF loadable package
(defun sys-path (filename &optional (package :gtk4-example))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

 ;; A wrapper to run an example
(defun run-example (func &optional (filename nil))
  (let ((resource (when filename
                        (g:resource-load (sys-path filename))))
        ;; Create an application
        (app (make-instance 'gtk:application
                            :application-id "com.crategus.run-example"
                            :resource-base-path (null-pointer)
                            :flags :none)))
    ;; Register the resources
    (when resource
      (g:resources-register resource))
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create an application window
          (funcall func application)))
    ;; Connect signal "shutdown"
    (g:signal-connect app "shutdown"
        (lambda (application)
          (declare (ignore application))
          ;; Unregister the resources
          (when resource
            (g:resources-unregister resource))))
    ;; Run the application
    (g:application-run app nil)))

;; Recursivly apply CSS to a widget an all child widgets
(defun apply-css-to-widget (provider widget)
  (gtk:style-context-add-provider (gtk:widget-style-context widget)
                                  provider
                                  +gtk-priority-user+)
  (do ((child (gtk:widget-first-child widget)
              (gtk:widget-next-sibling child)))
       ((not child))
    (apply-css-to-widget provider child)))

;;; 2022-11-11
