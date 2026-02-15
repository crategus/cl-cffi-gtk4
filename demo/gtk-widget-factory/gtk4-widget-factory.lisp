;;; ----------------------------------------------------------------------------
;;; gtk4-widget-factory.lisp
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defpackage :gtk4-widget-factory
  (:use :iterate :common-lisp)
  (:export #:widget-factory))

(in-package :gtk4-widget-factory)

;; Global variable that holds the running WidgetFactory application
(defvar *factory* nil)

;;; ----------------------------------------------------------------------------

;; Subclass WidgetFactory from GtkApplication. This class stores references
;; to widgets that are necessary for implementing actions.

(gobject:define-gobject-subclass "WidgetFactory" widget-factory
  (:superclass gtk:application
   :export t
   :interfaces nil)
  ((window
    widget-factory-window
    "window" "GtkWindow" t t)
   (stack
    widget-factory-stack
    "stack" "GtkWidget" t t)
   (searchbar
    widget-factory-searchbar
    "searchbar" "GtkWidget" t t)
   (infobar
    widget-factory-infobar
    "infobar" "GtkWidget" t t)
   (selection-dialog
    widget-factory-selection-dialog
    "selection-dialog" "GtkWindow" t t)
   (selection-flowbox
    widget-factory-selection-flowbox
    "selection-flowbox" "GtkWidget" t t)
   (open-menubutton
    widget-factory-open-menubutton
    "open-menubutton" "GtkWidget" t t)
   (record-button
    widget-factory-record-button
    "record-button" "GtkWidget" t t)
   (lockbutton
    widget-factory-lockbutton
    "lockbutton" "GtkWidget" t t)))

(defun widget-factory-init (builder)
  (let ((window (gtk:builder-object builder "window"))
        (stack (gtk:builder-object builder "toplevel_stack"))
        (searchbar (gtk:builder-object builder "searchbar"))
        (infobar (gtk:builder-object builder "infobar"))
        (dialog (gtk:builder-object builder "selection_dialog"))
        (flowbox (gtk:builder-object builder "selection_flowbox"))
        (open-menubutton (gtk:builder-object builder "open_menubutton"))
        (record-button (gtk:builder-object builder "record_button"))
        (lockbutton (gtk:builder-object builder "lockbutton")))
    (setf (widget-factory-window *factory*) window)
    (setf (widget-factory-stack *factory*) stack)
    (setf (widget-factory-searchbar *factory*) searchbar)
    (setf (widget-factory-infobar *factory*) infobar)
    (setf (widget-factory-selection-dialog *factory*) dialog)
    (setf (widget-factory-selection-flowbox *factory*) flowbox)
    (setf (widget-factory-open-menubutton *factory*) open-menubutton)
    (setf (widget-factory-record-button *factory*) record-button)
    (setf (widget-factory-lockbutton *factory*) lockbutton)))

;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; GtkColorSwatch is an undocumented internal GTK4 widget. Its type initializer
;; is not exported. Therefore, this implementation is not fully functional. We
;; use this widget in the list box example on Page 3 of the Widget Factory.
;;
;; TODO:Consider replacing this implementation with a ColorSwatch implementation
;; from scratch.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (gobject:define-gobject "GtkColorSwatch" color-swatch
    (:superclass widget
     :export t
     :interfaces ())
    ((can-drag
      color-swatch-can-drag
      "can-drag" "gboolean" t t)
     (can-drop
      color-swatch-can-drop
      "can-drop" "gboolean" t t)
     (has-menu
      color-swatch-has-menu
      "has-menu" "gboolean" t t)
     (rgba
      color-swatch-rgba
      "rgba" "GdkRGBA" t t)
     (selectable
      color-swatch-selectable
      "selectable" "gboolean" t t)))

  (export 'color-swatch))

;;; ----------------------------------------------------------------------------

(in-package :gtk4-widget-factory)

;; Subclass WFTextView from GtkTextView. This class adds a background with
;; opacity to the text view. The value for the opacity is taken from a
;; GtkAdjustment object that is controlled by a GtkScale widget. This
;; implementation overrides the SNAPSHOT-LAYER virtual function.

(gobject:define-gobject-subclass "WFTextView" wf-text-view
  (:superclass gtk:text-view
   :export t
   :interfaces ())
  ((view
    wf-text-view-view
    "view" "GtkTextView" t t)
   (texture
    wf-text-view-texture
    "texture" "GdkTexture" t t)
   (adjustment
    wf-text-view-adjustment
    "adjustment" "GtkAdjustment" t t)))

(gobject:define-vtable ("WFTextView" wf-text-view)
  ;; Parent class
  (:skip parent-instance (:struct gtk:widget-class))
  ;; Virtual functions for GtkTextView class
  (:skip move-cursor :pointer)
  (:skip set-anchor :pointer)
  (:skip insert-at-cursor :pointer)
  (:skip delete-from-cursor :pointer)
  (:skip backspace :pointer)
  (:skip cut-clipboard :pointer)
  (:skip copy-clipboard :pointer)
  (:skip paste-clipboard :pointer)
  (:skip toggle-overwrite :pointer)
  (:skip create-buffer :pointer)
  ;; Install SNAPSHOT-LAYER virtual function
  (snapshot-layer (:void
                   (textview (g:object gtk:text-view))
                   (layer gtk:text-view-layer)
                   (snapshot (g:object gtk:snapshot))))
  (:skip extend-selection :pointer)
  (:skip insert-emoji :pointer))

(defmethod wf-text-view-snapshot-layer-impl
           ((textview wf-text-view) layer snapshot)
  (when (and (eq :below-text layer)
             (wf-text-view-texture textview))
    (let* ((texture (wf-text-view-texture textview))
           (width (gdk:texture-width texture))
           (height (gdk:texture-height texture))
           (scale (/ (gtk:widget-width textview) width))
           (value (gtk:adjustment-value (wf-text-view-adjustment textview)))
           (opacity (/ value 100.0)))
      (gtk:snapshot-push-opacity snapshot opacity)
      (gtk:snapshot-scale snapshot scale scale)
      (graphene:with-rect (bounds 0 0 width height)
        (gtk:snapshot-append-texture snapshot texture bounds))
      (gtk:snapshot-scale snapshot (/ 1.0 scale) (/ 1.0 scale))
      (gtk:snapshot-pop snapshot))))

(defun wf-text-view-set-background (textview filename resource-p)
  (setf (wf-text-view-texture textview) nil)
  (when filename
    (if resource-p
        (setf (wf-text-view-texture textview)
              (gdk:texture-new-from-resource filename))
        (setf (wf-text-view-texture textview)
              (gdk:texture-new-from-filename filename)))
    (gtk:widget-queue-draw textview)))

(defun wf-text-view-set-adjustment (textview adjustment)
  (setf (wf-text-view-adjustment textview) adjustment)
  (g:signal-connect adjustment "value-changed"
      (lambda (adjustment)
        (declare (ignore adjustment))
        (gtk:widget-queue-draw textview))))

;;; ----------------------------------------------------------------------------

;; Subclass WFPermission from GPermission. An instance of this class is passed
;; to the GtkLockButton widget, which controls the status of the lock button.

(gobject:define-gobject-subclass "WFPermission" wf-permission
  (:superclass g:permission
   :export t
   :interfaces nil)
  nil)

(gobject:define-vtable ("WFPermission" wf-permission)
  ;; Parent class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Virtual methods
  (acquire        (:boolean
                   (permission (g:object g:permission))
                   (cancellable (g:object g:cancellable))
                   (error :pointer)))
  (acquire-async  (:void
                   (permission (g:object g:permission))
                   (cancellable (g:object g:cancellable))
                   (callback :pointer)
                   (data :pointer)))
  (acquire-finish (:boolean
                   (permission (g:object g:permission))
                   (result :pointer)
                   (err :pointer)))
  (release        (:boolean
                   (permission (g:object g:permission))
                   (cancellable (g:object g:cancellable))
                   (err :pointer)))
  (release-async  (:void
                   (permission (g:object g:permission))
                   (cancellable (g:object g:cancellable))
                   (callback :pointer)
                   (data :pointer)))
  (release-finish (:boolean
                   (permission (g:object g:permission))
                   (result :pointer)
                   (err :pointer)))
  (:skip none :pointer))

(defun update-allowed (permission allowed)
  (g:permission-impl-update permission allowed t t)
  t)

(defmethod wf-permission-acquire-impl
           ((permission wf-permission) cancellable err)
  (declare (ignore cancellable err))
  (update-allowed permission t))

(defmethod wf-permission-acquire-async-impl
           ((permission wf-permission) cancellable callback data)
  (let* ((permission1 (gobject:object-pointer permission))
         ;; TODO: We must call the internal %task-new function, which allows to
         ;; pass the C pointers instead of Lisp objects. Is it possible to
         ;; improve the implementation to use Lisp objects?
         (task (gio::%task-new permission1 (cffi:null-pointer) callback data)))
    (g:task-return-boolean task (update-allowed permission t))))

(defmethod wf-permission-acquire-finish-impl
           ((permission wf-permission) result err)
  (declare (ignore err))
  (g:task-propagate-boolean result))

(defmethod wf-permission-release-impl
           ((permission wf-permission) cancellable err)
  (declare (ignore cancellable err))
  (update-allowed permission nil))

(defmethod wf-permission-release-async-impl
           ((permission wf-permission) cancellable callback data)
  (let* ((permission1 (gobject:object-pointer permission))
         ;; TODO: Again, a call to the internal %task-new function is necessary.
         (task (gio::%task-new permission1 (cffi:null-pointer) callback data)))
    (g:task-return-boolean task (update-allowed permission nil))))

(defmethod wf-permission-release-finish-impl
           ((permission wf-permission) result err)
  (declare (ignore err))
  (g:task-propagate-boolean result))

;;; ----------------------------------------------------------------------------

;; Track current active page

(let ((currentpage 0))

  (defun get-on-page ()
    currentpage)

  (defun (setf get-on-page) (page)
    (setf currentpage page))

  (defun is-on-page (page)
    (= page currentpage)))

;;; ----------------------------------------------------------------------------

;; Callbacks for actions in gear menu

;; Action win.busy
(defun get-busy (action parameter)
  (declare (ignore action parameter))
  (let* ((window (widget-factory-window *factory*))
         (cursor (gdk:cursor-new-from-name "wait" nil)))
    (g:application-mark-busy *factory*)
    (setf (gdk:surface-cursor (gtk:native-surface window)) cursor)
    (g:timeout-add 2500
                   (lambda ()
                     (let ((native (gtk:native-surface window)))
                       (setf (gtk:widget-sensitive window) t)
                       (setf (gdk:surface-cursor native) nil)
                       (g:application-unmark-busy *factory*)
                       g:+source-remove+)))
    (setf (gtk:widget-sensitive window) nil)))

;; Action win.fullscreen
(defun change-fullscreen (action state)
  (let ((window (widget-factory-window *factory*)))
    (setf (g:simple-action-state action) state)
    (if (g:variant-boolean state)
        (gtk:window-fullscreen window)
        (gtk:window-unfullscreen window))))

;; Action win.theme
(defun change-theme-state (action state)
  (let ((settings (gtk:settings-default))
        (str (g:variant-string state))
        (theme nil))
    (setf (g:simple-action-state action) state)
    (cond ((string= str "default")
           (setf theme "Default"))
          ((string= str "dark")
           (setf theme "Default-dark"))
          ((string= str "hc")
           (setf theme "Default-hc"))
          ((string= str "hc-dark")
           (setf theme "Default-hc-dark"))
          ((string= str "current")
           (gtk:settings-reset-property settings "gtk-theme-name")
           (gtk:settings-reset-property settings
                                        "gtk-application-prefer-dark-theme")))
    (when theme
      (setf (gtk:settings-gtk-application-prefer-dark-theme settings) nil)
      (setf (gtk:settings-gtk-theme-name settings) theme))))

;; Action win.transition
(defun change-transition-state (action state)
  (let ((stack (widget-factory-stack *factory*)))
    (setf (g:simple-action-state action) state)
    (setf (gtk:stack-transition-type stack)
          (if (g:variant-boolean state) :slide-left :none))))

;; Action app.inspector
(defun activate-inspector (action parameter)
  (declare (ignore action parameter))
  (gtk:window-set-interactive-debugging t))

;; Action app.shortcuts and accel <control>?
(defun activate-shortcuts-window (action parameter)
  (declare (ignore action parameter))
  (let* ((window (widget-factory-window *factory*))
         (button (widget-factory-open-menubutton *factory*)))
    (gtk:menu-button-popdown button)
    (gtk:widget-activate-action window "win.show-help-overlay")))

;; Action app.about and accel F1
(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (let ((parent (widget-factory-window *factory*))
        (menubutton (widget-factory-open-menubutton *factory*))
        (buildinfo (make-string-output-stream)))
    ;; Output version informations in string
    (gtk:cl-cffi-gtk-build-info buildinfo)
    ;; Close the popup menu button on page3
    (gtk:menu-button-popdown menubutton)
    ;; Create and show about dialog
    (gtk:show-about-dialog parent
                           :modal t
                           :program-name "Gtk Widget Factory"
                           :version "0.0.0"
                           :copyright "© 2024 Dieter Kaiser"
                           :licence-type :mit-x11
                           :website "http://github.com/crategus/cl-cffi-gtk4"
                           :comments
                           "Program to demonstrate GTK themes and widgets"
                           :authors '("Dieter Kaiser")
                           :logo-icon-name "org.gtk.WidgetFacory4.svg"
                           :title "About GTK Widget Factory"
                           :system-information
                           (get-output-stream-string buildinfo))))

;;; ----------------------------------------------------------------------------

;; Action app.quit and accel <control>q
(defun activate-quit (action parameter)
  (declare (ignore action parameter))
  (dolist (window (gtk:application-windows *factory*))
    (gtk:window-destroy window)))

;; Action app.open
(defun activate-open-file (action parameter)
  (declare (ignore action parameter))
  (let ((dialog (gtk:file-dialog-new))
        (parent (widget-factory-window *factory*)))
    (gtk:file-dialog-open dialog parent nil
            (lambda (source result)
              (let ((file (gtk:file-dialog-open-finish source result)))
                (when file
                  (format t "File selected : ~a~%"
                            (g:file-basename file))))))))

;; Action app.size
(defun select-action (action parameter)
  (let* ((message (format nil "Select action ~a value is ~a~%"
                              (g:action-name action)
                              (g:variant-string parameter)))
         (dialog (gtk:alert-dialog-new message)))
  (setf (g:action-state action) parameter)
  (gtk:alert-dialog-show dialog (gtk:application-active-window *factory*))))

;; General action
(defun activate-action (action parameter)
  (declare (ignore parameter))
  (let* ((message (format nil "Activate action ~a~%" (g:action-name action)))
         (dialog (gtk:alert-dialog-new message)))
    (gtk:alert-dialog-show dialog (gtk:application-active-window *factory*))))

;; General toggle action
(defun toggle-action (action parameter)
  (declare (ignore parameter))
  (let* ((state (g:action-state action))
         (message (format nil "Toggle action ~a to ~a~%"
                              (g:action-name action)
                              (not (g:variant-boolean state))))
         (dialog (gtk:alert-dialog-new message)))
    (setf (g:simple-action-state action)
          (g:variant-new-boolean (not (g:variant-boolean state))))
    (gtk:alert-dialog-show dialog (gtk:application-active-window *factory*))))

;;; ----------------------------------------------------------------------------

;; Callback function for action entries

;; Action win.dark and accel <control>d
(defun change-dark-state (action state)
  (let ((settings (gtk:settings-default)))
    (setf (g:simple-action-state action) state)
    (setf (g:object-property settings "gtk-application-prefer-dark-theme")
          (g:variant-boolean state))))

;; Action win.search and accel <conrol>f
(defun activate-search (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((searchbar (widget-factory-searchbar *factory*)))
      (setf (gtk:search-bar-search-mode-enabled searchbar) t))))

;; Action win.delete and accel Delete
(defun activate-delete (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((infobar (widget-factory-infobar *factory*)))
      (setf (gtk:widget-visible infobar) t))))

;; Action win.background and accel <control>b
(defun activate-background (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((dialog (widget-factory-selection-dialog *factory*))
          (flowbox (widget-factory-selection-flowbox *factory*)))
      (populate-flowbox flowbox)
      (setf (gtk:widget-visible dialog) t))))

;; Action win.open and accel <control>o
(defun activate-open (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-open-menubutton *factory*)))
      (g:signal-emit button "activate"))))

;; Action win.record and accel <control>r
(defun activate-record (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-record-button *factory*)))
      (g:signal-emit button "clicked"))))

;; Action win.lock and accel <control>l
(defun activate-lock (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-lockbutton *factory*)))
      (g:signal-emit button "clicked"))))

;; Action win.print
(defun activate-print (action parameter)
  (declare (ignore action parameter))
  (let ((window (widget-factory-window *factory*))
        (operation (make-instance 'gtk:print-operation
                                  :allow-async t
                                  :embed-page-setup t)))

    (g:signal-connect operation "begin-print"
            (lambda (operation context)
              (declare (ignore context))
              (setf (gtk:print-operation-n-pages operation) 1)))

    (g:signal-connect operation "draw-page"
            (lambda (operation context page)
              (declare (ignore operation page))
              (let* ((cr (gtk:print-context-cairo-context context))
                     (width (gtk:print-context-width context))
                     (snapshot (gtk:snapshot-new))
                     (paintable (gtk:widget-paintable-new window))
                     (aspect (gdk:paintable-intrinsic-aspect-ratio paintable))
                     (node nil))
                (gdk:paintable-snapshot paintable snapshot width (/ width aspect))
                (setf node (gtk:snapshot-to-node snapshot))
                (gsk:render-node-draw node cr)
                (gsk:render-node-unref node))))

    (g:signal-connect operation "done"
            (lambda (operation result)
              (declare (ignore operation))
              (cond ((eq :error result)
                     (format t "Printing failed~%"))
                    ((eq :cancel result)
                     (format t "Printing was canceled~%"))
                    (t
                     (format t "Printing done~%")))))

    (unless (eq :in-progress
                (gtk:print-operation-run operation :print-dialog window))
      (g:signal-emit operation "done"))))

;;; --- Functions for page 1 ---------------------------------------------------

;; Control pulse mode of "progressbar3" and "entry1" on page 1

(let ((pulse-time 250)
      (pulse-entry-mode 0)
      (pulse-entry-fraction 0))

  ;; Function called by the timeout handler
  (defun pulse-it (widget)
    (let (pulse-id)
      ;; Pulse the widget which is an entry or a progress bar
      (if (eq 'gtk:entry (type-of widget))
          (gtk:entry-progress-pulse widget)
          (gtk:progress-bar-pulse widget))
      ;; Set a timeout handler and store a destroy notify handler on the
      ;; property list
      (setf pulse-id (g:timeout-add pulse-time
                                    (lambda () (pulse-it widget))))
      (g:object-set-data-full widget
                              "pulse-id"
                              (lambda ()
                                (g:source-remove pulse-id)))
      ;; Remove the source
      glib:+source-remove+))

  (defun pulse-update (adjustment widget)
    (let ((value (gtk:adjustment-value adjustment))
          (pulse-id (g:object-data widget "pulse-id")))
      (setf pulse-entry-fraction (/ value 100))
      (setf pulse-time (truncate (+ 50 (* 4 value))))
      (if (= 100 value)
          (setf (g:object-data widget "pulse-id") nil)
          (when (and (null pulse-id)
                     (or (eq 'gtk:progress-bar (type-of widget))
                         (and (eq 'gtk:entry (type-of widget))
                              (= 3 (mod pulse-entry-mode 3)))))
            (setf pulse-id
                  (g:timeout-add pulse-time
                                 (lambda () (pulse-it widget))))
            (g:object-set-data-full widget
                                    "pulse-id"
                                    (lambda ()
                                      (g:source-remove pulse-id)))))))

  (defun on-entry-icon-release (entry pos)
    (when (eq :secondary pos)
      (setf pulse-entry-mode (1+ pulse-entry-mode))
      (cond ((= 0 (mod pulse-entry-mode 3))
             (setf (g:object-data entry "pulse-id") nil)
             (setf (gtk:entry-progress-fraction entry) 0.0d0))
            ((= 1 (mod pulse-entry-mode 3))
             (setf (gtk:entry-progress-fraction entry) pulse-entry-fraction))
            (t
             (when (< (- pulse-time 50) 400)
               (setf (gtk:entry-progress-pulse-step entry) 0.1d0)
               (pulse-it entry)))))))

;;; ----------------------------------------------------------------------------

;; Load texures for page 1

#+nil
(defun load-texture-thread (task source path)
  (declare (ignore source))
  (let ((texture (gdk:texture-new-from-resource path)))
    (g:task-return-pointer task (g:object-pointer texture))))

#+nil
(defun load-texture-done (source result)
  (let* ((pointer (g:task-propagate-pointer result))
         (texture (cffi:convert-from-foreign pointer 'g:object)))
    (setf (gtk:picture-paintable source) texture)))

#+nil
(defun load-texture-in-thread (picture path)
  (let ((task (g:task-new picture nil #'load-texture-done)))
    (g:task-run-in-thread task
                          (lambda (task source data cancellable)
                            (declare (ignore data cancellable))
                            (load-texture-thread task source path)))))

;; Load without using a thread
(defun load-texture-in-thread (picture path)
  (let ((texture (gdk:texture-new-from-resource path)))
    (setf (gtk:picture-paintable picture) texture)))

;;; ----------------------------------------------------------------------------

;; Callbacks for notebook with pictures on page 1

(defun on-picture-drag-prepare (source x y)
  (declare (ignore x y))
  (let ((picture (gtk:event-controller-widget source)))
    (g:with-value (value "GdkTexture" (gtk:picture-paintable picture))
      (gdk:content-provider-new-for-value value))))

(defun on-picture-drop (target value x y)
  (declare (ignore x y))
  (let ((picture (gtk:event-controller-widget target))
        (paintable (g:value-object value)))
    (setf (gtk:picture-paintable picture) paintable)
    gdk:+event-stop+))

;;; ----------------------------------------------------------------------------

;; Callbacks for GtkTextView on page 1

(defun toggle-format (action value textview)
  (let ((name (g:action-name action))
        (buffer (gtk:text-view-buffer textview)))
    ;; TODO: Using function g:action-change-state causes infinite loop
    (setf (g:simple-action-state action) value)
    (multiple-value-bind (start end)
        (gtk:text-buffer-selection-bounds buffer)
      (if (g:variant-boolean value)
          (gtk:text-buffer-apply-tag buffer name start end)
          (gtk:text-buffer-remove-tag buffer name start end)))))

(defun text-changed (buffer actions)
  (let ((bold (g:action-map-lookup-action actions "bold"))
        (italic (g:action-map-lookup-action actions "italic"))
        (underline (g:action-map-lookup-action actions "underline"))
        (has-selection (gtk:text-buffer-has-selection buffer))
        (tags (gtk:text-buffer-tag-table buffer)))

    (setf (g:simple-action-enabled bold) has-selection)
    (setf (g:simple-action-enabled italic) has-selection)
    (setf (g:simple-action-enabled underline) has-selection)

    (when has-selection
      (multiple-value-bind (start end)
          (gtk:text-buffer-selection-bounds buffer)
        (let ((boldtag (gtk:text-tag-table-lookup tags "bold"))
              (italictag (gtk:text-tag-table-lookup tags "italic"))
              (underlinetag (gtk:text-tag-table-lookup tags "underline"))
              (allbold t) (allitalic t) (allunderline t))

          (iter (for iter = start)
                (while (and iter (not (gtk:text-iter-equal iter end))))
                (setf allbold
                      (and allbold (gtk:text-iter-has-tag iter boldtag)))
                (setf allitalic
                      (and allitalic (gtk:text-iter-has-tag iter italictag)))
                (setf allunderline
                      (and allunderline
                           (gtk:text-iter-has-tag iter underlinetag)))
                (gtk:text-iter-move iter))

          (setf (g:simple-action-state bold)
                (g:variant-new-boolean allbold))
          (setf (g:simple-action-state italic)
                (g:variant-new-boolean allitalic))
          (setf (g:simple-action-state underline)
                (g:variant-new-boolean allunderline)))))))

;;; --- Functions for page 2 ---------------------------------------------------

(defun set-needs-attention (page needs-attention)
  (let ((stack (gtk:widget-parent page)))
    (setf (g:object-property (gtk:stack-page stack page) "needs-attention")
          needs-attention)))

(defun page-changed-cb (stack pspec)
  (declare (ignore pspec))
  (unless (gtk:widget-in-destruction stack)
    (let* ((name (gtk:stack-visible-child-name stack))
           (window (gtk:widget-ancestor stack "GtkApplicationWindow"))
           (help (gtk:application-window-help-overlay window)))
      (when help
        (setf (g:object-property help "view-name") name))
      (cond ((string= "page1" name)
             (setf (get-on-page) 1))
            ((string= "page2" name)
             (setf (get-on-page) 2))
            ((string= "page3" name)
             (setf (get-on-page) 3)
             (let ((page (gtk:stack-visible-child stack)))
               (set-needs-attention page nil)))))))

;;; ----------------------------------------------------------------------------

(defun add-background (flowbox filename texture resource-p)
  (let* ((child (gtk:picture-new-for-paintable texture)))
    (setf (gtk:widget-size-request child) '(110 70))
    (gtk:flow-box-insert flowbox child -1)
    (let ((child1 (gtk:widget-parent child)))
      (setf (g:object-data child1 "filename") filename)
      (setf (g:object-data child1 "is-resource") resource-p))))

(defun populate-flowbox (flowbox)
  (unless (g:object-data flowbox "populated")
    (setf (g:object-data flowbox "populated") t)
    (let* ((size (* 4 110 80))
           (data (cffi:foreign-alloc :uchar :count size :initial-element #xff))
           (bytes (g:bytes-new data size))
           (texture (gdk:memory-texture-new 110 80
                                            gdk:+memory-default+
                                            bytes
                                            (* 4 110)))
           (child (gtk:picture-new-for-paintable texture)))
      (gtk:widget-add-css-class child "frame")
      (gtk:flow-box-insert flowbox (g:object-ref child) -1))
    (let ((resources '("sunset.jpg" "portland-rose.jpg" "beach.jpg" "nyc.jpg")))
      (dolist (resource resources)
        (let* ((prefix "/com/crategus/gtk4-widget-factory/")
               (file (concatenate 'string prefix resource))
               (pixbuf (gdk:pixbuf-new-from-resource-at-scale file 110 110 t))
               (texture (gdk:texture-new-for-pixbuf pixbuf)))
          (add-background flowbox file texture t))))))

;;; ----------------------------------------------------------------------------

(defun update-buttons (iconview size)
  (let ((button1 (g:object-data iconview "increase_button"))
        (button2 (g:object-data iconview "decrease_button"))
        (button3 (g:object-data iconview "reset_button")))
    (setf (gtk:widget-sensitive button1) (not (eq size :large)))
    (setf (gtk:widget-sensitive button2) (not (eq size :normal)))
    (setf (gtk:widget-sensitive button3) (not (eq size :inherit)))))

(defun increase-icon-size (iconview)
  (let ((cell (first (gtk:cell-layout-cells iconview))))
    (setf (gtk:cell-renderer-pixbuf-icon-size cell) :large)
    (update-buttons iconview :large)
    (gtk:widget-queue-resize iconview)))

(defun decrease-icon-size (iconview)
  (let ((cell (first (gtk:cell-layout-cells iconview))))
    (setf (gtk:cell-renderer-pixbuf-icon-size cell) :normal)
    (update-buttons iconview :normal)
    (gtk:widget-queue-resize iconview)))

(defun reset-icon-size (iconview)
  (let ((cell (first (gtk:cell-layout-cells iconview))))
    (setf (gtk:cell-renderer-pixbuf-icon-size cell) :inherit)
    (update-buttons iconview :inherit)
    (gtk:widget-queue-resize iconview)))

;;; --- Functions for page 3 ---------------------------------------------------

;; TODO: Rework this implementation with a nice Lisp function

(defun populate-model (store)
  (let (iter parent0 parent1 parent2 parent3)
    (setf iter (gtk:tree-store-append store nil))
    (gtk:tree-store-set store iter "Charlemagne" "742" "814")
    (setf parent0 iter)
    (setf iter (gtk:tree-store-append store parent0))
    (gtk:tree-store-set store iter "Peppin the Short" "741" "768")
    (setf parent1 iter)
    (setf iter (gtk:tree-store-append store parent1))
    (gtk:tree-store-set store iter "Charles Martel" "688" "741")
    (setf parent2 iter)
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Pepin of Herstal" "635" "714")
    (setf parent3 iter)
    (setf iter (gtk:tree-store-append store parent3))
    (gtk:tree-store-set store iter "Ansegisel" "602 of 610" "mudered befor 779")
    (setf iter (gtk:tree-store-append store parent3))
    (gtk:tree-store-set store iter "Begga" "615" "693")
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Alpaida")
    (setf iter (gtk:tree-store-append store parent1))
    (gtk:tree-store-set store iter "Rotrude")
    (setf parent2 iter)
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Liévin de Trèves")
    (setf parent3 iter)
    (setf iter (gtk:tree-store-append store parent3))
    (gtk:tree-store-set store iter "Guérin")
    (setf iter (gtk:tree-store-append store parent3))
    (gtk:tree-store-set store iter "Gunza")
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Willigarde de Bavière")
    (setf iter (gtk:tree-store-append store parent0))
    (gtk:tree-store-set store iter "Bertrada of Laon" "710" "783")
    (setf parent1 iter)
    (setf iter (gtk:tree-store-append store parent1))
    (gtk:tree-store-set store iter "Caribert of Laon" "before 762")
    (setf parent2 iter)
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Unknown")
    (setf iter (gtk:tree-store-append store parent2))
    (gtk:tree-store-set store iter "Prüm" "ca. 670" "after 721")
    (setf iter (gtk:tree-store-append store parent1))
    (gtk:tree-store-set store iter "Gisele of Aquitaine")
    (setf iter (gtk:tree-store-append store nil))
    (gtk:tree-store-set store iter "" "" "" t)
    (setf iter (gtk:tree-store-append store nil))
    (gtk:tree-store-set store iter "Attila the Hun" "ca. 390" "453")))

;;; ----------------------------------------------------------------------------

(defun populate-colors (listbox chooser)
  (let ((colors (list (list "2.5" "#C8828C" "Red")
                      (list "5"   "#C98286")
                      (list "7.5" "#C9827F")
                      (list "10"  "#C98376")
                      (list "2.5" "#C8856D" "Red/Yellow")
                      (list "5" "#C58764")
                      (list "7.5" "#C1895E")
                      (list "10" "#BB8C56")
                      (list "2.5" "#B58F4F" "Yellow")
                      (list "5" "#AD924B")
                      (list "7.5" "#A79548")
                      (list "10" "#A09749")
                      (list "2.5" "#979A4E" "Yellow/Green")
                      (list "5" "#8D9C55")
                      (list "7.5" "#7F9F62")
                      (list "10" "#73A06E")
                      (list "2.5" "#65A27C" "Green")
                      (list "5" "#5CA386")
                      (list "7.5" "#57A38D")
                      (list "10" "#52A394")
                      (list "2.5" "#4EA39A" "Green/Blue")
                      (list "5" "#49A3A2")
                      (list "7.5" "#46A2AA")
                      (list "10" "#46A1B1")
                      (list "2.5" "#49A0B8" "Blue")
                      (list "5" "#529EBD")
                      (list "7.5" "#5D9CC1")
                      (list "10" "#689AC3")
                      (list "2.5" "#7597C5" "Blue/Purple")
                      (list "5" "#8095C6")
                      (list "7.5" "#8D91C6")
                      (list "10" "#988EC4")
                      (list "2.5" "#A08CC1" "Purple")
                      (list "5" "#A88ABD")
                      (list "7.5" "#B187B6")
                      (list "10" "#B786B0")
                      (list "2.5" "#BC84A9" "Purple/Red")
                      (list "5" "#C183A0")
                      (list "7.5" "#C48299")
                      (list "10" "#C68292"))))

    (gtk:list-box-set-header-func listbox
        (lambda (row before)
          (declare (ignore before))
          (let ((header (gtk:list-box-row-header row))
                (title (g:object-data row "title")))
            (when (and (not header) title)
              (let* ((title1 (format nil "<b>~a</b>" title))
                     (header (make-instance 'gtk:label
                                            :label title1
                                            :use-markup t
                                            :halign :start
                                            :margin-top 12
                                            :margin-start 6
                                            :margin-end 6
                                            :margin-bottom 6
                                            :visible t)))
                (setf (gtk:list-box-row-header row) header))))))

    (dolist (color colors)
      (let* ((box (gtk:box-new :horizontal))
             (rgba (gdk:rgba-parse (second color)))
             (row (gtk:box-new :horizontal 20))
             (label (make-instance 'gtk:label
                                   :label (first color)
                                   :halign :start
                                   :valign :center
                                   :margin-start 6
                                   :margin-end 6
                                   :margin-top 6
                                   :margin-bottom 6
                                   :hexpand t
                                   :xalign 0.0))
             (swatch (make-instance 'gtk:color-swatch
                                    :rgba rgba
                                    :selectable nil
                                    :can-focus nil
                                    :halign :end
                                    :valign :center
                                    :margin-start 6
                                    :margin-end 6
                                    :margin-top 6
                                    :margin-bottom 6
                                    :height-request 24)))

        (gtk:box-append box swatch)
        (gtk:box-append row label)
        (gtk:box-append row box)

        (gtk:list-box-insert listbox row -1)

        (setf row (gtk:widget-parent row))
        (setf (gtk:list-box-row-activatable row) nil)
        (setf (g:object-data row "color") (second color))
        (when (third color)
          (setf (g:object-data row "title") (third color)))))

    (g:signal-connect listbox "row-selected"
        (lambda (listbox row)
          (declare (ignore listbox))
          (let (rgba)
            (when row
              (let ((color (g:object-data row "color")))
                (when color
                  (when (setf rgba (gdk:rgba-parse color))
                    (let (;; Retrieve handler ID from association list
                          (handler-id (g:object-data chooser "notify-rgba-id")))
                      (g:signal-handler-block chooser handler-id)
                      (setf (gtk:color-chooser-rgba chooser) rgba)
                      (g:signal-handler-unblock chooser handler-id)))))))))

    (gtk:list-box-invalidate-headers listbox)

    (let ((scrolled (gtk:widget-ancestor listbox "GtkScrolledWindow")))
      (g:signal-connect scrolled "edge-overshot"
          (lambda (window pos)
            (declare (ignore window))
            (block overshot
              (let ((silver (g:object-data listbox "Silver"))
                    (gold (g:object-data listbox "Gold"))
                    color)

                (when (eq  :top pos)
                  (when silver
                    (gtk:list-box-remove listbox silver)
                    (setf (g:object-data listbox "Silver") nil))
                  (when gold
                    (gtk:list-box-remove listbox gold)
                    (setf (g:object-data listbox "Gold") nil))
                  (return-from overshot))

                (when gold (return-from overshot))
                (if silver
                    (setf color "Gold")
                    (setf color "Silver"))

                (let* ((row (gtk:box-new :horizontal 20))
                       (text (format nil "<b>~a</b>" color))
                       (label (make-instance 'gtk:label
                                             :label text
                                             :use-markup t
                                             :halign :start
                                             :valign :center
                                             :hexpand t
                                             :margin-start 6
                                             :margin-end 6
                                             :margin-top 6
                                             :margin-bottom 6
                                             :xalign 0.0))
                        (box (gtk:box-new :horizontal))
                        (rgba (gdk:rgba-parse color))
                        (swatch (make-instance 'gtk:color-swatch
                                               :rgba rgba
                                               :can-focus nil
                                               :selectable nil
                                               :halign :end
                                               :valign :center
                                               :margin-start 6
                                               :margin-end 6
                                               :margin-top 6
                                               :margin-bottom 6
                                               :height-request 24)))
                  (gtk:box-append box swatch)
                  (gtk:box-append row label)
                  (gtk:box-append row box)
                  (gtk:list-box-insert listbox row -1)

                  (setf row (gtk:widget-parent row))
                  (setf (gtk:list-box-row-activatable row) nil)
                  (setf (g:object-data listbox color) row)
                  (setf (g:object-data row "color") color)))))))))

;;; ----------------------------------------------------------------------------

;; Callbacks for Text View with toolbuttons on page 3

(defun handle-insert (button textview)
  (let ((id (gtk:buildable-buildable-id button))
        (buffer (gtk:text-view-buffer textview))
        text)
    (cond ((string= id "toolbutton1")
           (setf text "⌘"))
          ((string= id "toolbutton2")
           (setf text "⚽"))
          ((string= id "toolbutton3")
           (setf text "⤢"))
          ((string= id "toolbutton4")
           (setf text "☆"))
          (t
           (setf text "")))
    (gtk:text-buffer-insert buffer :cursor text)))

(defun handle-cutcopypaste (button textview)
  (let ((clipboard (gtk:widget-clipboard textview))
        (buffer (gtk:text-view-buffer textview))
        (id (gtk:buildable-buildable-id button)))
    (cond ((string= id "cutbutton")
           (gtk:text-buffer-cut-clipboard buffer clipboard t))
          ((string= id "copybutton")
           (gtk:text-buffer-copy-clipboard buffer clipboard))
          ((string= id "pastebutton")
           (gtk:text-buffer-paste-clipboard buffer clipboard :editable t))
          ((string= id "deletebutton")
           (gtk:text-buffer-delete-selection buffer :interactive t :editable t))
          (t
           (error "HANDLE-CUTCOPYPASTE: Should never be reached.")))))

(defun textbuffer-notify-selection (buffer button)
  (let ((id (gtk:buildable-buildable-id button))
        (has-selection (gtk:text-buffer-has-selection buffer)))
    (when (or (string= id "cutbutton")
              (string= id "copybutton")
              (string= id "deletebutton"))
      (setf (gtk:widget-sensitive button) has-selection))))

(defun clipboard-formats-notify (clipboard button)
  (let ((id (gtk:buildable-buildable-id button))
        (has-text (gdk:content-formats-contain-gtype
                      (gdk:clipboard-formats clipboard) "GtkTextBuffer")))
    (when (string= id "pastebutton")
      (setf (gtk:widget-sensitive button) has-text))))

;;; ----------------------------------------------------------------------------

;; Notebook with closable tabs on page 3

(defun tab-close-cb (page)
  (setf (gtk:widget-visible page) nil)
  (g:timeout-add 2500
                 (lambda ()
                   (setf (gtk:widget-visible page) t)
                   g:+source-remove+)))

;;; ----------------------------------------------------------------------------

(defun activate (application)
  (let* ((entries `(;; Actions for gear menu in menu bar
                    ("busy" ,#'get-busy nil nil nil)
                    ("fullscreen" nil nil "false" ,#'change-fullscreen)
                    ("theme" nil "s" "'current'" ,#'change-theme-state)
                    ("transition" nil nil "true" ,#'change-transition-state)
                    ;; Other actions
                    ("dark" nil nil "false" ,#'change-dark-state)
                    ("search" ,#'activate-search nil nil nil)
                    ("delete" ,#'activate-delete nil nil nil)
                    ("background" ,#'activate-background nil nil nil)
                    ("open" ,#'activate-open nil nil nil)
                    ("record" ,#'activate-record nil nil nil)
                    ("lock" ,#'activate-lock nil nil nil)
                    ("print" ,#'activate-print nil nil nil)))
         (accels1 `(;; Accels for actions in gear menu
                    ("win.fullscreen" "F11")
                    ("app.shortcuts" "<Control>question")
                    ("app.about" "F1")
                    ;; Other accels
                    ("app.quit" "<Control>q")
                    ("app.open-in" "<Control>n")
                    ("win.dark" "<Control>d")
                    ("win.search" "<Control>f")
                    ("win.background" "<Control>b")
                    ("win.open" "<Control>o")
                    ("win.record" "<Control>r")
                    ("win.lock" "<Control>l")))
         (accels2 `(("app.cut" ("<Control>x"))
                    ("app.copy" ("<Control>c"))
                    ("app.paste" ("<Control>v"))
                    ("win.delete" ("Delete"))))
         (controller (make-instance 'gtk:shortcut-controller
                                    :propagation-phase :bubble))
         ;; Create new GtkProvider
         (cssfile "/com/crategus/gtk4-widget-factory/widget-factory.css")
         (provider (gtk:css-provider-new))
         ;; Load GtkBuilder UI definition from resource
         (uifile "/com/crategus/gtk4-widget-factory/widget-factory.ui")
         (builder (gtk:builder-new-from-resource uifile))
         (window (gtk:builder-object builder "window")))
    ;; Store widgets from UI definition as application properties
    (widget-factory-init builder)
    ;; Load and apply CSS from resource
    (gtk:css-provider-load-from-resource provider cssfile)
    (gtk:style-context-add-provider-for-display (gdk:display-default) provider)
    ;; Add help overlay
    (let ((resource "/com/crategus/gtk4-widget-factory/gtk/help-overlay.ui"))
      (gtk:builder-add-from-resource builder resource))

    ;; Prepare application window
    (setf (gtk:window-icon-name window) "org.gtk.WidgetFactory4")
    (setf (gtk:window-application window) application)
    (gtk:widget-add-controller window controller)
    ;; Add action entries
    (g:action-map-add-action-entries window entries)
    ;; Set ACCELS1 for actions
    (iter (for (name accels) in accels1)
          (setf (gtk:application-accels-for-action application name)
                accels))
    ;; Set ACCELS2
    (iter (for (name accels) in accels2)
          (multiple-value-bind (key mods)
              (gtk:accelerator-parse (first accels))
            (let* ((trigger (gtk:keyval-trigger-new key mods))
                   (action (gtk:named-action-new name))
                   ;; Workaround. What is wrong?
                   (shortcut (gtk:shortcut-new trigger action)))
              (gtk:shortcut-controller-add-shortcut controller shortcut))))

    ;; Change transition speed in gear menu
    (let ((scale (gtk:builder-object builder "scale-transition-speed")))
      (g:signal-connect scale "value-changed"
              (lambda (range)
                (let ((stack (widget-factory-stack *factory*))
                      (value (round (gtk:range-value range))))
                  (setf (gtk:stack-transition-duration stack) value)))))

    ;; --- On Page 1 -----------------------------------------------------------

    ;; Configure entry and progress bar on page 1
    (let ((adjustment (gtk:builder-object builder "adjustment1"))
          (progressbar (gtk:builder-object builder "progressbar3"))
          (entry (gtk:builder-object builder "entry1")))
      (g:signal-connect adjustment "value-changed"
                        (lambda (adj)
                          (pulse-update adj progressbar)))
      (g:signal-connect adjustment "value-changed"
                        (lambda (adj)
                          (pulse-update adj entry)))
      (g:signal-connect entry "icon-release" #'on-entry-icon-release)
      ;; Initialize progressbar and entry
      (pulse-update adjustment progressbar)
      (pulse-update adjustment entry))

    ;; Progessbar 1 und 2 and adjustment 3 on page 1
    (let ((adjustment (gtk:builder-object builder "adjustment3"))
          (bar1 (gtk:builder-object builder "progressbar1"))
          (bar2 (gtk:builder-object builder "progressbar2")))
      (g:signal-connect adjustment "value-changed"
              (lambda (adj)
                (let ((frac (- (/ (gtk:adjustment-value adj)
                                  (gtk:adjustment-upper adj))
                               (gtk:adjustment-lower adj))))
                  (setf (gtk:progress-bar-fraction bar1) frac)
                  (setf (gtk:progress-bar-fraction bar2) frac)))))

    ;; Set GtkScaleFormatFunc callback functions
    (gtk:scale-set-format-value-func (gtk:builder-object builder "scale3")
            (lambda (scale value)
              (declare (ignore scale))
              (format nil "~,1f" value)))
    (gtk:scale-set-format-value-func (gtk:builder-object builder "scale4")
            (lambda (scale value)
              (declare (ignore scale value))
              " "))

    ;; Load pictures in tabs of notebook on page 1
    (load-texture-in-thread (gtk:builder-object builder "notebook_sunset")
                            "/com/crategus/gtk4-widget-factory/sunset.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_nyc")
                            "/com/crategus/gtk4-widget-factory/nyc.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_beach")
                            "/com/crategus/gtk4-widget-factory/beach.jpg")

    ;; Drag and drop for GtkNotebook on page 1
    (let ((source (gtk:builder-object builder "notebook-sunset-drag")))
      (g:signal-connect source "prepare" #'on-picture-drag-prepare))
    (let ((target (gtk:builder-object builder "notebook-sunset-drop")))
      (g:signal-connect target "drop" #'on-picture-drop))
    (let ((source (gtk:builder-object builder "notebook-nyc-drag")))
      (g:signal-connect source "prepare" #'on-picture-drag-prepare))
    (let ((target (gtk:builder-object builder "notebook-nyc-drop")))
      (g:signal-connect target "drop" #'on-picture-drop))
    (let ((source (gtk:builder-object builder "notebook-beach-drag")))
      (g:signal-connect source "prepare" #'on-picture-drag-prepare))
    (let ((target (gtk:builder-object builder "notebook-beach-drop")))
      (g:signal-connect target "drop" #'on-picture-drop))

    ;; Configure GtkTextView on page 1
    ;; This text view shares the text buffer with text view on page 2
    (let* ((actions (g:simple-action-group-new))
           (textview (gtk:builder-object builder "textview1"))
           (entries (list (list "bold" nil nil
                                "false"
                                #'(lambda (action value)
                                    (toggle-format action value textview)))
                          (list "italic" nil nil
                                "false"
                                #'(lambda (action value)
                                    (toggle-format action value textview)))
                          (list "underline" nil nil
                                "false"
                                #'(lambda (action value)
                                    (toggle-format action value textview))))))

      (g:action-map-add-action-entries actions entries)

      (setf (g:simple-action-enabled
                (g:action-map-lookup-action actions "bold")) nil)
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action actions "italic")) nil)
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action actions "underline")) nil)

      (gtk:widget-insert-action-group textview "format" actions)

      (let ((menu (g:menu-new))
            (item1 (g:menu-item-new "Bold" "format.bold"))
            (item2 (g:menu-item-new "Italic" "format.italic"))
            (item3 (g:menu-item-new "Underline" "format.underline")))
        (setf (g:menu-item-attribute-value item1 "s")
              (g:variant-new-string "format-text-bold-symbolic"))
        (setf (g:menu-item-attribute-value item2 "s")
              (g:variant-new-string "format-text-italic-symbolic"))
        (setf (g:menu-item-attribute-value item3 "s")
              (g:variant-new-string "format-text-underline-symbolic"))

        (g:menu-append-item menu item1)
        (g:menu-append-item menu item2)
        (g:menu-append-item menu item3)

        (setf (gtk:text-view-extra-menu textview) menu))

      (g:signal-connect (gtk:text-view-buffer textview) "changed"
                        (lambda (buffer)
                          (text-changed buffer actions)))
      (g:signal-connect (gtk:text-view-buffer textview) "mark-set"
                        (lambda (buffer location mark)
                          (declare (ignore location mark))
                          (text-changed buffer actions))))

    ;; --- On Page 2 -----------------------------------------------------------

    ;; Prepare statusbar for text view on page 2
    (let ((statusbar (gtk:builder-object builder "statusbar")))
      (gtk:statusbar-push statusbar 0 "All systems are operating normally.")
      (g:action-map-add-action window
                               (g:property-action-new "statusbar"
                                                      statusbar
                                                      "visible")))
    ;; Prepare toolbar for text view on page 2
    (let ((toolbar (gtk:builder-object builder "toolbar")))
      (g:action-map-add-action window
                               (g:property-action-new "toolbar"
                                                      toolbar
                                                      "visible")))

    ;; Signal handler for infobar on page 2
    (let ((infobar (gtk:builder-object builder "infobar")))
      (g:signal-connect infobar "response"
                        (lambda (infobar response)
                          (when (= -7 response)
                            (setf (gtk:widget-visible infobar) nil)))))

    ;; Show info on page 2
    (let ((adj (gtk:builder-object builder "adjustment2"))
          (page2reset (gtk:builder-object builder "page2reset"))
          (page2dismiss (gtk:builder-object builder "page2dismiss"))
          (page2note (gtk:builder-object builder "page2note"))
          (spinbutton (gtk:builder-object builder "verticalspin1")))
      ;; Set tooltip on spin button
      (setf (gtk:widget-tooltip-text spinbutton)
            (format nil "Change the value of the spin button.~%~
                         Reveals an info if the value is a multiple of three."))
      ;; Connect signal handlers
      (g:signal-connect page2reset "clicked"
          (lambda (button)
            (setf (gtk:adjustment-value adj) 50.0)
            (let ((ancestor (gtk:widget-ancestor button "GtkRevealer")))
              (setf (gtk:revealer-reveal-child ancestor) nil))))
      (g:signal-connect page2dismiss "clicked"
          (lambda (button)
            (let ((ancestor (gtk:widget-ancestor button "GtkRevealer")))
              (setf (gtk:revealer-reveal-child ancestor) nil))))
      (g:signal-connect adj "value-changed"
          (lambda (adjustment)
            (let ((value (truncate (gtk:adjustment-value adjustment)))
                  (ancestor (gtk:widget-ancestor page2note "GtkRevealer")))
              (when (= 0 (mod value 3))
                (setf (gtk:label-label page2note)
                      (format nil "~a is a multiple of 3" value)))
              (setf (gtk:revealer-reveal-child ancestor)
                    (= 0 (mod value 3)))))))

    ;; Set signal handler for "mic-button" on page 2
    (let ((button (gtk:builder-object builder "mic-button")))
      (g:signal-connect  button "value-changed"
          (lambda (button value)
            (declare (ignore value))  ; Should we use value?
            (let ((adj (gtk:scale-button-adjustment button))
                  (val (gtk:scale-button-value button))
                  str)
              (cond ((<= val (gtk:adjustment-lower adj))
                     (setf str "Muted"))
                    ((>= val (gtk:adjustment-upper adj))
                     (setf str "Full volume"))
                    (t
                     (let ((percent (+ (/ (* 100 val)
                                          (- (gtk:adjustment-upper adj)
                                             (gtk:adjustment-lower adj)))
                                       0.5)))
                       (setf percent (truncate percent))
                       (setf str (format nil "~2d %" percent)))))
              (setf (gtk:widget-tooltip-text button) str)))))

    ;; Inform Button on page 2
    (let ((dialog (gtk:builder-object builder "info_dialog"))
          (button (gtk:builder-object builder "info_dialog_button")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible dialog) t)))
      (g:signal-connect dialog "response"
          (lambda (dialog response)
            (declare (ignore response))
            (setf (gtk:widget-visible dialog) nil))))

    ;; Act Button on page 2
    (let ((dialog (gtk:builder-object builder "action_dialog"))
          (button (gtk:builder-object builder "action_dialog_button")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible dialog) t)))
      (g:signal-connect dialog "response"
          (lambda (dialog response)
            (declare (ignore response))
            (setf (gtk:widget-visible dialog) nil))))
    (let ((button (gtk:builder-object builder "act_action_dialog"))
          (stack (gtk:builder-object builder "toplevel_stack")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (g:timeout-add 1000
                           (lambda ()
                             (let ((page (gtk:stack-child-by-name stack
                                                                  "page3")))
                               (set-needs-attention page t)
                               g:+source-remove+)))))
      (g:signal-connect stack "notify::visible-child-name" #'page-changed-cb))

    ;; Configure button on page 2 and circular button on page 3
    (let ((dialog (gtk:builder-object builder "preference_dialog"))
          (button (gtk:builder-object builder "preference_dialog_button"))
          (details (gtk:builder-object builder "details_entry"))
          (more (gtk:builder-object builder "more_details_entry"))
          (switch (gtk:builder-object builder "mode_switch"))
          (scale (gtk:builder-object builder "level_scale"))
          (label (gtk:builder-object builder "error_label"))
          (circular (gtk:builder-object builder "circular_button")))
      (g:signal-connect dialog "response"
          (lambda (dialog response)
            (declare (ignore response))
            (setf (gtk:widget-visible dialog) nil)))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible dialog) t)))
      (g:signal-connect more "notify::text"
          (lambda (more pspec)
            (declare (ignore pspec))
            (if (and (> (length (gtk:editable-text more)) 0)
                     (= (length (gtk:editable-text details)) 0))
                (progn
                  (setf (gtk:widget-tooltip-text more)
                        "Must have details first")
                  (gtk:widget-add-css-class more "error"))
                (progn
                  (setf (gtk:widget-tooltip-text more) "")
                  (gtk:widget-remove-css-class more "error")))))
      ;; This duplicates code, do a better implementation
      (g:signal-connect details "notify::text"
          (lambda (details pspec)
            (declare (ignore pspec))
            (if (and (> (length (gtk:editable-text more)) 0)
                     (= (length (gtk:editable-text details)) 0))
                (progn
                  (setf (gtk:widget-tooltip-text more)
                        "Must have details first")
                  (gtk:widget-add-css-class more "error"))
                (progn
                  (setf (gtk:widget-tooltip-text more) "")
                  (gtk:widget-remove-css-class more "error")))))

      (g:signal-connect switch "state-set"
          (lambda (switch state)
            (if (or (not state) (> (gtk:range-value scale) 50))
                (progn
                  (setf (gtk:widget-visible label) nil)
                  (setf (gtk:switch-state switch) state))
                (setf (gtk:widget-visible label) t))
            gdk:+event-stop+))

      (g:signal-connect scale "value-changed"
          (lambda (range)
            (if (and (gtk:switch-active switch)
                     (not (gtk:switch-state switch))
                     (> (gtk:range-value range) 50))
                (progn
                  (setf (gtk:widget-visible label) nil)
                  (setf (gtk:switch-state switch) t))
                (if (and (gtk:switch-state switch)
                         (<= (gtk:range-value range) 50))
                    (setf (gtk:switch-state switch) nil)))))

      ;; Circular button is on page 3
      (g:signal-connect circular "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible dialog) t))))

    ;; Select button on page 2
    (let ((dialog (gtk:builder-object builder "selection_dialog"))
          (text3 (gtk:builder-object builder "text3"))
          (opacity (gtk:builder-object builder "opacity"))
          (button (gtk:builder-object builder "selection_dialog_button"))
          (flowbox (gtk:builder-object builder "selection_flowbox")))
      (g:signal-connect dialog "response"
          (lambda (dialog response)
            (setf (gtk:widget-visible dialog) nil)
            (when (= -5 response)
              (let* ((box (gtk:widget-first-child
                              (gtk:dialog-content-area dialog)))
                     (children (gtk:flow-box-selected-children box)))
                (when children
                  (let* ((child (first children))
                         (file (g:object-data child "filename"))
                         (resource-p (g:object-data child "is-resource")))
                    (wf-text-view-set-background text3 file resource-p)))))))
      (wf-text-view-set-adjustment text3 (gtk:range-adjustment opacity))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible dialog) t)))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (populate-flowbox flowbox))))

    ;; Listbox on page 2
    (let ((listbox (gtk:builder-object builder "listbox"))
          (switch (gtk:builder-object builder "listboxrow1switch"))
          (row3 (gtk:builder-object builder "listboxrow3"))
          (row7 (gtk:builder-object builder "listboxrow7"))
          (row8 (gtk:builder-object builder "listboxrow8"))
          (image (gtk:builder-object builder "listboxrow3image"))
          (infodialog (gtk:builder-object builder "info_dialog"))
          (actiondialog (gtk:builder-object builder "action_dialog"))
          (button (gtk:builder-object builder "listboxrow5button")))
      ;; Set data
      (setf (g:object-data row3 "image") image)
      (setf (g:object-data row7 "dialog") infodialog)
      (setf (g:object-data row8 "dialog") infodialog)

      (setf (gtk:widget-tooltip-text (gtk:builder-object builder "opacity"))
            (format nil
                    "Change the opacity of the background in the text view.~%~
                     First, select a picture for the background."))

      ;; Row activated
      (g:signal-connect listbox "row-activated"
          (lambda (listbox row)
            (declare (ignore listbox))
            (let ((image (g:object-data row "image"))
                  (dialog (g:object-data row "dialog")))
              (when image
                (if (> (gtk:widget-opacity image) 0)
                    (setf (gtk:widget-opacity image) 0)
                    (setf (gtk:widget-opacity image) 1)))
              (when dialog
                (gtk:window-present dialog)))))
      ;; Switch activated
      (g:signal-connect switch "notify::active"
          (lambda (switch pspec)
            (declare (ignore pspec))
            (if (gtk:switch-active switch)
                (setf (gtk:list-box-selection-mode listbox) :single)
                (setf (gtk:list-box-selection-mode listbox) :none))
            (setf (gtk:list-box-activate-on-single-click listbox)
                  (not (gtk:switch-active switch)))))
      ;; Button clicked
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (gtk:window-present actiondialog))))

    ;; Expander with icons on page 2
    (let ((iconview (gtk:builder-object builder "iconview1"))
          (increase (gtk:builder-object builder "increase_button"))
          (decrease (gtk:builder-object builder "decrease_button"))
          (reset (gtk:builder-object builder "reset_button")))
      (setf (g:object-data iconview "increase_button") increase)
      (setf (g:object-data iconview "decrease_button") decrease)
      (setf (g:object-data iconview "reset_button") reset)
      (update-buttons iconview :normal)
      (g:signal-connect reset "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (reset-icon-size iconview)))
      (g:signal-connect increase "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (increase-icon-size iconview)))
      (g:signal-connect decrease "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (decrease-icon-size iconview))))


    ;; --- On Page 3 -----------------------------------------------------------

    ;; Set signal handler for "record_button" on page 3
    (let ((button (gtk:builder-object builder "record_button")))
      (g:signal-connect button "toggled"
          (lambda (button)
              (if (gtk:toggle-button-active button)
                  (gtk:widget-remove-css-class button "destructive-action")
                  (gtk:widget-add-css-class button "destructive-action")))))

    ;; lockbox and lockbox on page 3
    (let ((lockbox (gtk:builder-object builder "lockbox"))
          (lockbutton (gtk:builder-object builder "lockbutton"))
          (permission (make-instance 'wf-permission))
          (actionopen (g:action-map-lookup-action window "open"))
          (actionrecord (g:action-map-lookup-action window "record")))
      (update-allowed permission t)
      (g:object-bind-property permission "allowed"
                              lockbox "sensitive"
                              :sync-create)
      (g:object-bind-property permission "allowed"
                              actionopen "enabled"
                              :sync-create)
      (g:object-bind-property permission "allowed"
                              actionrecord "enabled"
                              :sync-create)
      (setf (gtk:lock-button-permission lockbutton) permission))

    ;; Configure handler for GtkComboBox "page_combo" and buttons on page 3
    (let ((combo (gtk:builder-object builder "page_combo"))
          (from-button (gtk:builder-object builder "range_from_spin"))
          (to-button (gtk:builder-object builder "range_to_spin"))
          (print-button (gtk:builder-object builder "print_button")))
      (g:signal-connect combo "changed"
          (lambda (combo)
            (let ((active (gtk:combo-box-active combo)))
              (cond ((= 0 active)
                     (setf (gtk:widget-sensitive from-button) t)
                     (setf (gtk:widget-sensitive to-button) t)
                     (setf (gtk:widget-sensitive print-button) t))
                    ((= 1 active)
                     (setf (gtk:spin-button-value from-button) 1)
                     (setf (gtk:spin-button-value to-button) 99)
                     (setf (gtk:widget-sensitive from-button) nil)
                     (setf (gtk:widget-sensitive to-button) nil)
                     (setf (gtk:widget-sensitive print-button) t))
                    ((= 2 active)
                     (setf (gtk:spin-button-value from-button) 7)
                     (setf (gtk:spin-button-value to-button) 7)
                     (setf (gtk:widget-sensitive from-button) nil)
                     (setf (gtk:widget-sensitive to-button) nil)
                     (setf (gtk:widget-sensitive print-button) t))
                    ((= 4 active)
                     (setf (gtk:widget-sensitive from-button) nil)
                     (setf (gtk:widget-sensitive to-button) nil)
                     (setf (gtk:widget-sensitive print-button) nil))))))
      (g:signal-connect from-button "changed"
          (lambda (button)
            (let ((val1 (gtk:spin-button-value-as-int button))
                  (val2 (gtk:spin-button-value-as-int to-button)))
              (when (> val1 val2)
               (setf (gtk:spin-button-value to-button) val1)))))
      (g:signal-connect to-button "changed"
          (lambda (button)
            (let* ((val1 (gtk:spin-button-value-as-int from-button))
                   (val2 (gtk:spin-button-value-as-int button)))
              (when (> val1 val2)
                (setf (gtk:spin-button-value from-button) val2))))))

    ;; Configure tree view on page 3
    (let ((treeview (gtk:builder-object builder "charletree")))
      (populate-model (gtk:tree-view-model treeview))
      (gtk:tree-view-set-row-separator-func treeview
          (lambda (model iter)
            (first (gtk:tree-model-get model iter 3))))
      (gtk:tree-view-expand-all treeview))

    ;; Listbox for colors on page 3
    (let ((listbox (gtk:builder-object builder "munsell"))
          (chooser (gtk:builder-object builder "cchooser")))
      ;; Store handler ID on association list
      (setf (g:object-data chooser "notify-rgba-id")
            (g:signal-connect chooser "notify::rgba"
                              (lambda (chooser pspec)
                                (declare (ignore chooser pspec))
                                (gtk:list-box-select-row listbox nil))))
      (populate-colors listbox chooser))

    ;; Text view with toolbuttons on page 3
    (let* ((textview (gtk:builder-object builder "tooltextview"))
           (buffer (gtk:text-view-buffer textview))
           (clipboard (gtk:widget-clipboard textview))
           (toolbutton1 (gtk:builder-object builder "toolbutton1"))
           (toolbutton2 (gtk:builder-object builder "toolbutton2"))
           (toolbutton3 (gtk:builder-object builder "toolbutton3"))
           (toolbutton4 (gtk:builder-object builder "toolbutton4"))
           (cutbutton (gtk:builder-object builder "cutbutton"))
           (copybutton (gtk:builder-object builder "copybutton"))
           (pastebutton (gtk:builder-object builder "pastebutton"))
           (deletebutton (gtk:builder-object builder "deletebutton")))

      (g:signal-connect toolbutton1 "clicked"
                        (lambda (button)
                          (handle-insert button textview)))
      (g:signal-connect toolbutton2 "clicked"
                        (lambda (button)
                          (handle-insert button textview)))
      (g:signal-connect toolbutton3 "clicked"
                        (lambda (button)
                          (handle-insert button textview)))
      (g:signal-connect toolbutton4 "clicked"
                        (lambda (button)
                          (handle-insert button textview)))

      (g:signal-connect cutbutton "clicked"
                        (lambda (button)
                          (handle-cutcopypaste button textview)))
      (g:signal-connect copybutton "clicked"
                        (lambda (button)
                          (handle-cutcopypaste button textview)))
      (g:signal-connect pastebutton "clicked"
                        (lambda (button)
                          (handle-cutcopypaste button textview)))
      (g:signal-connect deletebutton "clicked"
                        (lambda (button)
                          (handle-cutcopypaste button textview)))

      ;; Check signal handlers in detail. Is this implementation correct?
      (g:signal-connect buffer "notify::has-selection"
                        (lambda (buffer pspec)
                          (declare (ignore pspec))
                          (textbuffer-notify-selection buffer cutbutton)))
      (g:signal-connect buffer "notify::has-selection"
                        (lambda (buffer pspec)
                          (declare (ignore pspec))
                          (textbuffer-notify-selection buffer copybutton)))
      (g:signal-connect buffer "notify::has-selection"
                        (lambda (buffer pspec)
                          (declare (ignore pspec))
                          (textbuffer-notify-selection buffer pastebutton)))
      (g:signal-connect buffer "notify::has-selection"
                        (lambda (buffer pspec)
                          (declare (ignore pspec))
                          (textbuffer-notify-selection buffer deletebutton)))

      (g:signal-connect clipboard "notify::formats"
                        (lambda (clipboard pspec)
                          (declare (ignore pspec))
                          (clipboard-formats-notify clipboard pastebutton))))

    ;; Notebook with closable tabs on page 3
    (let ((osd (gtk:builder-object builder "osd_frame"))
          (totem (gtk:builder-object builder "totem_like_osd"))
          (gesture (gtk:gesture-click-new))
          (page1 (gtk:builder-object builder "closable_page1"))
          (page2 (gtk:builder-object builder "closable_page2"))
          (button1 (gtk:builder-object builder "closable_page1_button"))
          (button2 (gtk:builder-object builder "closable_page2_button")))
      (setf (g:object-data osd "osd") totem)
      (gtk:widget-add-controller osd gesture)
      (g:signal-connect gesture "pressed"
          (lambda (gesture press x y)
            (declare (ignore gesture press x y))
            (let ((visible (gtk:widget-visible totem)))
              (setf (gtk:widget-visible totem) (not visible))
              gdk:+event-stop+)))

      (g:signal-connect button1 "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (tab-close-cb page1)))

      (g:signal-connect button2 "clicked"
                        (lambda (button)
                          (declare (ignore button))
                          (tab-close-cb page2))))

    ;; GtkEntry for age on page 3
    (let ((entry (gtk:builder-object builder "age-entry")))
      (g:signal-connect entry "notify::text"
          (lambda (entry pspec)
            (declare (ignore pspec))
            (let* ((text (gtk:editable-text entry))
                   (value (parse-integer text :junk-allowed t)))
              (if (and value (>= value 16) (< value 100))
                  (progn
                    (setf (gtk:widget-tooltip-text entry) "Valid age")
                    (gtk:widget-remove-css-class entry "error"))
                  (progn
                    (setf (gtk:widget-tooltip-text entry)
                          "Only age between 16 to 99 allowed")
                    (gtk:widget-add-css-class entry "error")))))))

    ;; GtkPopover on page 3
    (let ((popover (gtk:builder-object builder "open_popover"))
          (entry (gtk:builder-object builder "open_popover_entry"))
          (button (gtk:builder-object builder "open_popover_button")))
      (setf (gtk:popover-default-widget popover) button)
      (g:signal-connect entry "notify::text"
          (lambda (entry pspec)
            (declare (ignore pspec))
            (let ((text (gtk:editable-text entry)))
              (setf (gtk:widget-sensitive button) (> (length text) 0)))))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (setf (gtk:widget-visible popover) nil))))

    ;; Popover in Page 1 of Notebook on page 3
    (let* ((box (gtk:builder-object builder "box_for_context"))
           (model (gtk:builder-object builder "new_style_context_menu_model"))
           (popover (gtk:popover-menu-new-from-model model))
           (gesture (gtk:gesture-click-new)))
      (gtk:widget-set-parent popover box)
      (setf (gtk:popover-has-arrow popover) nil)
      (setf (gtk:event-controller-name gesture) "widget-factory-context-click")
      (setf (gtk:gesture-single-button gesture) gdk:+button-secondary+)
      (gtk:widget-add-controller box gesture)
      (g:signal-connect gesture "pressed"
          (lambda (gesture n x y)
            (declare (ignore gesture n))
            (let ((rect (gdk:rectangle-new :x (truncate x)
                                           :y (truncate y)
                                           :width 1
                                           :height 1)))
              (setf (gtk:popover-pointing-to popover) rect)
              (gtk:popover-popup popover)))))

    ;; Show the application window
    (gtk:window-present window)))

;;; ----------------------------------------------------------------------------

(defun local-options (application options)
  (declare (ignore application))
  (let ((status -1)) ; Default to continue processing
    (when (g:variant-dict-contains options "version")
      (format t "~&~a~%" (gtk:cl-cffi-gtk-build-info))
      ;; Set status to zero to exit the application
      (setf status 0))
    status))

;;; ----------------------------------------------------------------------------

(defun widget-factory (&rest argv)
  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     "gtk4-widget-factory.xml"
                                     :package "gtk4-widget-factory"))
    (unless (string= "GTK4 Widget Factory" (g:application-name))
      (setf (g:application-name) "GTK4 Widget Factory"))
    (let* ((gtk-init:*warn-deprecated* nil)
           (argv (cons (g:application-name)
                       (or argv (uiop:command-line-arguments))))
           (app (setf *factory*
                      (make-instance 'widget-factory
                                     :application-id
                                     "com.crategus.gtk4-widget-factory"
                                     :register-session t)))
           (entries (list ;; Actions for gear menu in menur bar
                          (list "inspector" #'activate-inspector)
                          (list "shortcuts" #'activate-shortcuts-window)
                          (list "about" #'activate-about)
                          ;; Actions for File menu on page 2
                          (list "new" #'activate-action)
                          (list "open" #'activate-open-file)
                          (list "save" #'activate-action)
                          (list "save-as" #'activate-action)
                          (list "quit" #'activate-quit)
                          ;; Actions or Edit menu on page 2
                          (list "cut" #'activate-action)
                          (list "copy" #'activate-action)
                          (list "paste" #'activate-action)
                          ;; Actions for dinner_menu on page 2
                          (list "main" nil "s" "'steak'")
                          (list "wine" nil nil "false")
                          (list "beer" nil nil "false")
                          (list "water" nil nil "true")
                          (list "dessert" nil "s" "'bars'")
                          (list "pay" nil "s")
                          ;; Actions for ... menu on page 3
                          (list "open-in" #'activate-action)
                          (list "open-tab" #'activate-action)
                          (list "pin" #'toggle-action nil "true")
                          (list "labels" #'activate-action)
                          (list "share" #'activate-action)
                          (list "size" #'select-action "s" "'medium'")
                          (list "berk" #'toggle-action nil "true")
                          (list "broni" #'toggle-action nil "true")
                          (list "drutt" #'toggle-action nil "true")
                          (list "upstairs" #'toggle-action nil "true")
                          ;; Actions for Options in popover on page 3
                          (list "open-window" #'activate-action)
                          (list "option-a" #'activate-action)
                          (list "option-b" #'activate-action)
                          (list "option-c" #'activate-action)
                          (list "option-d" #'activate-action)
                          ;; Other actions
                          (list "check-on" nil nil "true")
                          (list "check-off" nil nil "false")
                          (list "radio-x" nil "s" "'x'")
                          (list "check-on-disabled" nil nil "true")
                          (list "check-off-disabled" nil nil "false")
                          (list "radio-x-disabled" nil "s" "'x'"))))
      ;; Workaround to initialize GtkColorSwatch
      (make-instance 'gtk:color-chooser-widget)
      ;; Add actions to the application
      (g:action-map-add-action-entries app entries)
      ;; Disable some actions
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action app "wine")) nil)
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action app "check-on-disabled")) nil)
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action app "check-off-disabled")) nil)
      (setf (g:simple-action-enabled
                (g:action-map-lookup-action app "radio-x-disabled")) nil)
      ;; Add an option for the application to handle
      (g:application-add-main-option app "version"
                                         (char-code #\v)
                                         :none
                                         :none
                                         "Show program version"
                                         "")
      ;; Connect LOCAL-OPTIONS and ACTIVATE handlers to the application
      (g:signal-connect app "handle-local-options" #'local-options)
      (g:signal-connect app "activate" #'activate)
      ;; Run the application
      (g:application-run app argv))))

;;; --- End of file gtk4-widget-factory.lisp -----------------------------------
