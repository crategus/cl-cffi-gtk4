(defpackage :gtk4-example
  (:use :cffi :iterate :split-sequence :common-lisp)
  (:import-from :gtk)
  (:import-from :gdk)
  (:import-from :glib)
  (:export #:run-example

           ;; Interface builder
           #:do-builder

           ;; Windows
           #:do-window-simple
           #:do-dialog-quick-message
           #:do-dialog-various
           #:create-message-dialog-simple
           #:create-message-dialog-simple2
           #:do-message-dialog-new
           #:do-message-dialog-new-with-markup
           #:do-message-dialog-with-markup
           #:do-message-dialog-with-secondary-markup
           #:create-alert-dialog
           #:do-assistant
           #:do-application-window

           ;; Layout Containers
           #:do-box-append
           #:do-box-center
           #:do-box-baseline
           #:do-box-baseline-vertical
           #:do-grid-spacing
           #:do-grid-buildable
           #:do-revealer
           #:do-revealer-icon
           #:do-list-box-complex
           #:do-list-box-controls
           #:do-flow-box
           #:do-stack
           #:do-stack-sidebar
           #:do-action-bar
           #:do-header-bar
           #:do-overlay-interactive
           #:do-overlay-decorative
           #:do-notebook
           #:do-paned
           #:create-expander
           #:do-aspect-frame
           #:do-fixed

           ;; Layout Managers
           #:do-layout-manager-transition
           #:do-fixed-layout
           #:do-fixed-layout-transformations
           #:do-constraint-simple
           #:do-constraint-interactive
           #:do-constraint-builder

           ;; Display Widgets
           #:do-label-various
           #:do-images
           #:do-spinner
           #:do-info-bar
           #:do-progress-bar
           #:do-level-bar
           #:do-statusbar
           #:do-calendar

           ;; List-based widgets
           #:do-list-view-applauncher
           #:do-list-view-recent-manager
           #:do-list-view-bookmarks
           #:do-list-view-directory-list
           #:do-column-view-content-types
           #:do-column-view-ucd
           #:do-grid-view-clocks
           #:do-grid-view-colors
           #:do-drop-down

           ;; Media Support
           #:do-video

           ;; Button and Toggle Widgets
           #:do-button-various
           #:do-toggle-button
           #:do-toggle-button-action
           #:do-check-button
           #:do-menu-button
           #:do-link-button
           #:do-scale-button
           #:do-switch

           ;; Numeric/Text Data Entry
           #:do-entry
           #:do-entry-buffer
           #:do-entry-completion
           #:do-password-entry
           #:do-scale-widget
           #:do-spin-button
           #:do-search-bar

           ;; Multiline Text Editor
           #:do-text-view-simple
           #:do-text-view-attributes
           #:do-text-view-tags
           #:do-text-view-search
           #:do-text-view-hypertext
           #:do-text-view-markup
           #:do-text-view-tabs
           #:do-text-view-multiple
           #:do-text-view-undo
           #:do-text-view-scroll

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

           #:create-file-chooser-dialog
           #:create-file-chooser-native

           #:create-file-dialog-open
           #:create-file-dialog-open-multiple
           #:create-file-dialog-save
           #:create-file-dialog-select-folder
           #:create-file-dialog-select-multiple-folders

           #:do-font-button
           #:do-font-button-label

           #:do-pickers

           ;; Ornaments
           #:do-frame
           #:do-frame-properties

           ;; Widgets for custom drawing
           #:do-drawing-area
           #:do-drawing-area-scribble

           ;; Scrolling
           #:do-scrolled-window

           ;; Printing
           #:create-page-setup-dialog

           ;; Data exchange, Clipboards, Drag and Drop
           #:do-clipboard
           #:do-drag-and-drop

           ;; Miscellaneous
           #:do-cursors
           #:do-size-group
           #:do-emblemed-icon
           #:do-event-controller
           #:do-widget-template

           ;; Theming in GTK
           #:do-css-accordion
           #:do-css-basics
           #:do-css-blendmodes
           #:do-css-multiplebgs
           #:do-css-pixbufs
           #:do-css-shadows

           ;; Subclassing
           #:do-sublclassing-figure

           ;;; Gdk examples
           #:do-app-launch-context
           #:do-app-launch-context-async
           #:do-frame-clock

           #:do-paintable-simple
           #:do-paintable-animation
           #:do-paintable-emblem
           ))

(in-package :gtk4-example)

(setf (glib-sys:get-current-package) "gtk4-example")

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

(defvar *lorem-ipsum-long*
        (format nil "Lorem ipsum dolor sit amet, consectetur adipiscing elit. ~
Morbi vitae condimentum ligula, vitae bibendum urna. Praesent vitae nisi ~
hendrerit lorem malesuada interdum vitae vitae massa. Integer elementum justo ~
nibh, non euismod odio tincidunt et. Praesent lobortis molestie mi quis ~
rhoncus. Interdum et malesuada fames ac ante ipsum primis in faucibus. ~
Curabitur luctus, tortor vel ornare aliquet, erat nulla tempus orci, ac ~
pulvinar velit turpis ac nulla. Orci varius natoque penatibus et magnis dis ~
parturient montes, nascetur ridiculus mus. Nam efficitur scelerisque erat. ~
Nunc nec viverra magna, eget consequat dui. Vestibulum vitae porttitor quam. ~
Fusce leo enim, molestie non sollicitudin sollicitudin, porta vel libero.

In hac habitasse platea dictumst. In ultricies nulla vel massa varius, eu ~
tempor metus condimentum. Duis nisl tortor, vestibulum ut auctor eu, tristique ~
lobortis libero. Nam congue volutpat leo a hendrerit. In ut purus ac risus ~
aliquet commodo in sit amet ante. Aenean sed tempus dolor. Aliquam a sagittis ~
metus. Donec eget urna eu justo fringilla tincidunt id et diam. Maecenas ~
ultrices pellentesque augue vitae rhoncus. Integer aliquet venenatis elit sed ~
lacinia. Praesent dui libero, aliquet imperdiet blandit ut, sollicitudin id ~
ipsum. Pellentesque venenatis vitae sem non fermentum. Ut orci libero, ~
interdum a pharetra at, mollis a mi.

Integer tempus cursus fringilla. Donec ornare fermentum nulla sed aliquet. ~
Mauris in velit metus. Quisque in diam id diam bibendum eleifend vitae id ~
tortor. Nulla condimentum ultricies ultrices. Nunc tincidunt, justo at blandit ~
condimentum, leo purus mollis orci, sed mollis dui metus eget eros. Mauris ~
quam nibh, laoreet eget arcu in, accumsan lacinia purus. Morbi aliquet nibh id ~
sem venenatis, vitae ultricies arcu laoreet."))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream
      (let ((str (make-string (file-length instream))))
        (read-sequence str instream)
        str))))

(defun window-draw-func (title drawfunc application
                         &optional (width 600) (height 600))
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                 :application application
                                 :child area
                                 :title title
                                 :default-width width
                                 :default-height height)))
    ;; Set the drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (declare (ignore widget))
          (funcall drawfunc cr width height)))
    ;; Show the window.
    (setf (gtk:widget-visible window) t)))

;; Wrapper to run an example
(defun run-example (func &optional (functype :window))
  ;; Load resources for the examples
  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     "gtk4-example.xml"
                                     :package "gtk4-example"
                                     :sourcedir "resource/"
                                     :verbose t))
    (let* (;; Create an application
           (app (make-instance 'gtk:application
                               :application-id "com.crategus.run-example"
                               :resource-base-path (null-pointer)))
           (entries (list (list "inspector"
                                (lambda (action parameter)
                                  (declare (ignore action parameter))
                                  (gtk:window-set-interactive-debugging t)))
                          (list "quit"
                                (lambda (action parameter)
                                  (declare (ignore action parameter))
                                  (let ((windows (gtk:application-windows app)))
                                    (dolist (window windows)
                                      (gtk:window-destroy window))))))))
      ;; Add actions entries for the application
      (g:action-map-add-action-entries app entries)
      ;; Add accelerators for the application
      (setf (gtk:application-accels-for-action app "app.quit") "<Control>q")
      (setf (gtk:application-accels-for-action app "app.inspector") "<Control>d")

      ;; Connect signal "activate" to the application
      (g:signal-connect app "activate"
          (lambda (application)
            (cond ((eq :window functype)
                   ;; Start example as application window
                   (funcall func application))
                  ((eq :dialog functype)
                   ;; Start example as transient for application window
                   (let ((window (make-instance 'gtk:application-window
                                                :application application
                                                :title "GTK Example"
                                                :default-width 600
                                                :default-height 400)))
                     (setf (gtk:widget-visible window) t)
                     (funcall func window)))
                  ((eq :drawfunc functype)
                   (window-draw-func "GTK Example" func application))
                  (t
                   (error "Type of function is unknown.")))))
      ;; Run the application
      (g:application-run app nil))))

;;; 2024-10-2
