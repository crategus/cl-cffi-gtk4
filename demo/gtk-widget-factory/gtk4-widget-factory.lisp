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
  (:export #:gtk4-widget-factory))

(in-package :gtk4-widget-factory)

;;; ----------------------------------------------------------------------------

;; Track current active page

(let ((currentpage 0))

  (defun get-on-page ()
    currentpage)

  (defun (setf get-on-page) (value)
    (setf currentpage value))

  (defun is-on-page (i)
    (= i currentpage)))

;;; ----------------------------------------------------------------------------

;; Subclass WidgetFactory from GtkApplication
;; The purpose of this class is to store references to widgets

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
    "lockbutton" "GtkWidget" t t)
   (range-from-spin
    widget-factory-range-from-spin
    "range-from-spin" "GtkWidget" t t)
   (range-to-spin
    widget-factory-range-to-spin
    "range-to-spin" "GtkWidget" t t)
   (print-button
    widget-factory-print-button
    "print-button" "GtkWidget" t t)))

;; Global variable to hold the running WidgetFactory application
(defvar factory nil)

;;; ----------------------------------------------------------------------------

;; Code to load texures in a thread

(defun load-texture-thread (task source path)
  (declare (ignore source))
  (let ((texture (gdk:texture-new-from-resource path)))
    (g:task-return-pointer task (g:object-pointer texture))))

(defun load-texture-done (source result)
  (let* ((pointer (g:task-propagate-pointer result))
         (texture (cffi:convert-from-foreign pointer 'g:object)))
    (setf (gtk:picture-paintable source) texture)))

(defun load-texture-in-thread (picture path)
  (let ((task (g:task-new picture nil #'load-texture-done)))
    (g:task-run-in-thread task
                          (lambda (task source data cancellable)
                            (declare (ignore data cancellable))
                            (load-texture-thread task source path)))))

;;; ----------------------------------------------------------------------------

;; Control the pulse mode of "progressbar3" and "entry1"

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

(gobject:define-gobject-subclass "MyTextView" my-text-view
  (:superclass gtk:text-view
   :export t
   :interfaces ())
  nil)

;;; ----------------------------------------------------------------------------

;; Action app.about and accel F1
(defun activate-about (action parameter)
  (declare (ignore action parameter))
  (let ((parent (widget-factory-window factory))
        (menubutton (widget-factory-open-menubutton factory))
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

;; Action app.shortcuts and accel <control>?
(defun activate-shortcuts-window (action parameter)
  (format t "in ACTIVATE-SHORTCUTS-WINDOW: ~a, ~a~%" action parameter)
  (let* ((window (widget-factory-window factory))
         (button (widget-factory-open-menubutton factory)))
    (gtk:menu-button-popdown button)
    (gtk:widget-activate-action window "win.show-help-overlay")))

;; Action app.quit and accel <control>q
(defun activate-quit (action parameter)
  (declare (ignore action parameter))
  (dolist (window (gtk:application-windows factory))
    (gtk:window-destroy window)))

;; Action app.inspector
(defun activate-inspector (action parameter)
  (format t "in ACTIVATE-INSPECTOR: ~a, ~a~%" action parameter)
  (gtk:window-set-interactive-debugging t))

;; FIXME: Does not work on Windows, check for Linux
(defun activate-open-file (action parameter)
  (declare (ignore action parameter))
  (format t "in ACTIVATE-OPEN-FILE~%")
  (let ((dialog (gtk:file-dialog-new))
        (parent (widget-factory-window factory)))
    (format t "call GTK:FILE-DIALOG-OPEN~%")
    (gtk:file-dialog-open dialog parent nil
            (lambda (source result)
              (format t "in GAsync callback~%")
              (let ((file (gtk:file-dialog-open-finish source result)))
                (when file
                  (format t "File selected : ~a~%"
                            (g:file-basename file))))))))

;; Action app.size
(defun select-action (action parameter)
  (format t "in SELECT-ACTION: ~a, ~a~%" action parameter))

;; General action
(defun activate-action (action parameter)
  (format t "in ACTIVATE-ACTION: ~a, ~a~%" action parameter))

;; General toggle action
(defun toggle-action (action parameter)
  (format t "in TOGGLE-ACTION: ~a, ~a~%" action parameter))

;;; ----------------------------------------------------------------------------

;; Callback function for action entries

(defun change-dark-state (action state)
  (let ((settings (gtk:settings-default)))
    (setf (g:simple-action-state action) state)
    (setf (g:object-property settings "gtk-application-prefer-dark-theme")
          (g:variant-boolean state))))

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

(defun change-fullscreen (action state)
  (let ((window (widget-factory-window factory)))
    (setf (g:simple-action-state action) state)
    (if (g:variant-boolean state)
        (gtk:window-fullscreen window)
        (gtk:window-unfullscreen window))))

(defun change-transition-state (action state)
  (let ((stack (widget-factory-stack factory)))
    (setf (g:simple-action-state action) state)
    (setf (gtk:stack-transition-type stack)
          (if (g:variant-boolean state) :slide-left :none))))

(defun get-busy (action parameter)
  (declare (ignore action parameter))
  (let* ((application factory)
         (window (widget-factory-window application))
         (cursor (gdk:cursor-new-from-name "wait" nil)))
    (g:application-mark-busy application)
    (setf (gdk:surface-cursor (gtk:native-surface window)) cursor)
    (g:timeout-add 2500
                   (lambda ()
                     (let ((native (gtk:native-surface window)))
                       (setf (gtk:widget-sensitive window) t)
                       (setf (gdk:surface-cursor native) nil)
                       (g:application-unmark-busy application)
                       g:+source-remove+)))
    (setf (gtk:widget-sensitive window) nil)))

(defun activate-search (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((searchbar (widget-factory-searchbar factory)))
      (setf (gtk:search-bar-search-mode-enabled searchbar) t))))

(defun activate-delete (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((infobar (widget-factory-infobar factory)))
      (setf (gtk:widget-visible infobar) t))))

(defun add-background (flowbox file texture)
  (declare (ignore file))
  (let ((child (gtk:picture-new-for-paintable texture)))
    (setf (gtk:widget-size-request child) '(110 70))
    (gtk:flow-box-insert flowbox child -1)
))

;; TODO: Implement a :default constant for GDK_MEMORY_DEFAUlT

#|
/**
 * GDK_MEMORY_DEFAULT:
 *
 * The default memory format used by GTK.
 *
 * This is the format provided by [method@Gdk.Texture.download].
 * It is equal to %CAIRO_FORMAT_ARGB32.
 *
 * Be aware that unlike the `GdkMemoryFormat` values, this format
 * is different for different endianness.
 */
#if G_BYTE_ORDER == G_LITTLE_ENDIAN
#define GDK_MEMORY_DEFAULT GDK_MEMORY_B8G8R8A8_PREMULTIPLIED
#elif G_BYTE_ORDER == G_BIG_ENDIAN
#define GDK_MEMORY_DEFAULT GDK_MEMORY_A8R8G8B8_PREMULTIPLIED
#else
#error "Unknown byte order for GDK_MEMORY_DEFAULT"
#endif
|#

(defun populate-flowbox (flowbox)
  (unless (g:object-data flowbox "populated")
    (format t " POPLATE-FLOWBOX with pictures~%")
    (setf (g:object-data flowbox "populated") t)
    (let* ((size (* 4 110 80))
           (data (cffi:foreign-alloc :uchar :count size :initial-element #xff))
           (bytes (g:bytes-new data size))
           (texture (gdk:memory-texture-new 110 70
                                            gdk:+memory-default+
                                            bytes
                                            (* 4 110)))
           (child (gtk:picture-new-for-paintable texture)))
      (gtk:widget-add-css-class child "frame")
      (gtk:flow-box-insert flowbox child -1))
    (let ((resources '("sunset.jpg" "portland-rose.jpg" "beach.jpg" "nyc.jpg")))
      (dolist (resource resources)
        (let* ((prefix "/com/crategus/gtk4-widget-factory/")
               (file (concatenate 'string prefix resource))
               (pixbuf (gdk:pixbuf-new-from-resource-at-scale file 110 110 t))
               (texture (gdk:texture-new-for-pixbuf pixbuf)))
          (add-background flowbox file texture))))))

(defun activate-background (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 2)
    (let ((dialog (widget-factory-selection-dialog factory))
          (flowbox (widget-factory-selection-flowbox factory)))
      (populate-flowbox flowbox)
      (setf (gtk:widget-visible dialog) t))))

;; Action win.open and accel <control>o
(defun activate-open (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-open-menubutton factory)))
      (g:signal-emit button "activate"))))

;; Action win.record and accel <control>r
(defun activate-record (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-record-button factory)))
      (g:signal-emit button "clicked"))))

;; Action win.lock and accel <control>l
(defun activate-lock (action parameter)
  (declare (ignore action parameter))
  (when (is-on-page 3)
    (let ((button (widget-factory-lockbutton factory)))
      (g:signal-emit button "clicked"))))

;; Action win.print
(defun activate-print (action parameter)
  (declare (ignore action parameter))
  (let ((window (widget-factory-window factory))
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

;;; ----------------------------------------------------------------------------

(defun set-needs-attention (page needs-attention)
  (let ((stack (gtk:widget-parent page)))
    (setf (g:object-property (gtk:stack-page stack page) "needs-attention")
          needs-attention)))

(defun page-changed-cb (stack pspec)
  (format t "in PAGE-CHANGED-CB:~%")
  (format t "    stack : ~a~%" stack)
  (format t "    pspec : ~a~%" pspec)
  (unless (gtk:widget-in-destruction stack)
    (let* ((name (gtk:stack-visible-child-name stack))
           (window (gtk:widget-ancestor stack "GtkApplicationWindow"))
           (help (gtk:application-window-help-overlay window)))

      (format t "    name : ~a~%" name)
      (format t "    window : ~a~%" window)
      (format t "    help : ~a~%" help)
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

;; Signal handlers for objects from the UI definition

(defun transition-speed-changed (range)
  (let ((stack (widget-factory-stack factory))
        (value (round (gtk:range-value range))))
    (format t "in TRANSITION-SPEED-CHANGED with ~a~%" value)
    (setf (gtk:stack-transition-duration stack) value)))

(defun on-picture-drag-prepare (source x y)
  (format t "  in ON-PICTURE-DRAG-PREPARE : ~a ~a ~a~%" source x y))

(defun on-picture-drop (target value x y)
  (format t "  in ON-PICTURE-DROP : ~a ~a ~a ~a~%" target value x y))

;; Signal handler "value-changed" for mic-button on page 2
(defun on-scale-button-value-changed (range)
  (format t "  in ON-SCALE-BUTTON-VALUE-CHANGED : ~a~%" range))

(defun on-record-button-toggled (toggle)
  (format t "  in ON-RECORD-BUTTON-TOGGLED : ~a~%" toggle))

(defun on-page-combo-changed (combo)
  (format t "  in ON-PAGE-COMBO-CHANGED : ~a~%" combo))

(defun on-range-from-changed (range)
  (format t "  in ON-RANGE-FROM-CHANGED : ~a~%" range))

(defun on-range-to-changed (range)
  (format t "  in ON-RANGE-TO-CHANGED : ~a~%" range))

(defun tab-close-cb (button)
  (format t "  in TAB-CLOSE-CB : ~a~%" button))

(defun osd-frame-pressed (gesture n x y)
  (format t "  in OSD-FRAME-PRESSED : ~a ~a ~a ~a~%" gesture n x y))

(defun age-entry-changed (widget pspec)
  (format t "  in AGE-ENTRY-CHANGED : ~a ~a~%" widget pspec))

(defun validate-more-details (widget pspec)
  (format t "  in VALIDATE-MORE-DETAILS : ~a ~a~%" widget pspec))

(defun level-scale-value-changed (range)
  (format t "  in LEVEL-SCALE-VALUE-CHANGED : ~a~%" range))

(defun mode-switch-state-set (widget state)
  (format t "  in MODE-SWITCH-STATE-SET : ~a ~a~%" widget state))

;;; ----------------------------------------------------------------------------

;; FIXME: The style classes defined in the UI defintion file are not applied.
;; This is a workaround to set the style classes on the widgets.

(defun set-styles-on-page1 (builder)
  (gtk:widget-add-css-class (gtk:builder-object builder "label-large-title")
                            "large-title")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-title1")
                            "title-1")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-title2")
                            "title-2")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-title3")
                            "title-3")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-title4")
                            "title-4")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-heading")
                            "heading")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-body")
                            "body")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-caption-heading")
                            "caption-heading")
  (gtk:widget-add-css-class (gtk:builder-object builder "label-caption")
                            "caption"))

;;; ----------------------------------------------------------------------------

;; FIXME: The grid on page1 is not filled correctly from the UI definition.
;; The initialization of the row number is not correct. Is there a bug in
;; GtkBuilder? This function fills the grid by hand.

(defun fill-grid-on-page1 (grid)
  ;; Six checkbuttons in the first column
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton"
                                       :active t) 0 0)
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton") 0 1)
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton"
                                       :inconsistent t) 0 2)
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton"
                                       :active t
                                       :sensitive nil) 0 3)
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton"
                                       :sensitive nil) 0 4)
  (gtk:grid-attach grid (make-instance 'gtk:check-button
                                       :label "checkbutton"
                                       :inconsistent t
                                       :sensitive nil) 0 5)
  ;; Six radiobuttons in the second column
  (let (group)
    (gtk:grid-attach grid (setf group
                                (make-instance 'gtk:check-button
                                               :label "radiobutton"
                                               :active t)) 1 0)
    (gtk:grid-attach grid (make-instance 'gtk:check-button
                                         :label "radiobutton"
                                         :group group
                                         :active nil) 1 1)
    (gtk:grid-attach grid (make-instance 'gtk:check-button
                                         :label "radiobutton"
                                         :group group
                                         :active nil
                                         :inconsistent t) 1 2)
    (gtk:grid-attach grid (setf group
                                (make-instance 'gtk:check-button
                                               :label "radiobutton"
                                               :active t
                                               :sensitive nil)) 1 3)
    (gtk:grid-attach grid (make-instance 'gtk:check-button
                                         :label "radiobutton"
                                         :group group
                                         :active nil
                                         :sensitive nil) 1 4)
    (gtk:grid-attach grid (make-instance 'gtk:check-button
                                         :label "radiobutton"
                                         :group group
                                         :active nil
                                         :inconsistent t
                                         :sensitive nil) 1 5))
  ;; Four spinner widgets
  (gtk:grid-attach grid (make-instance 'gtk:spinner
                                       :spinning t) 2 0)
  (gtk:grid-attach grid (make-instance 'gtk:spinner
                                       :spinning nil) 2 1)
  (gtk:grid-attach grid (make-instance 'gtk:spinner
                                       :spinning t
                                       :sensitive nil) 2 3)
  (gtk:grid-attach grid (make-instance 'gtk:spinner
                                       :spinning nil
                                       :sensitive nil) 2 4))

;;; ----------------------------------------------------------------------------

(defun activate (application)
  (let* ((entries `(("dark" nil nil "false" ,#'change-dark-state)
                    ("theme" nil "s" "'current'" ,#'change-theme-state)
                    ("transition" nil nil "true" ,#'change-transition-state)
                    ("search" ,#'activate-search nil nil nil)
                    ("delete" ,#'activate-delete nil nil nil)
                    ("busy" ,#'get-busy nil nil nil)
                    ("fullscreen" nil nil "false" ,#'change-fullscreen)
                    ("background" ,#'activate-background nil nil nil)
                    ("open" ,#'activate-open nil nil nil)
                    ("record" ,#'activate-record nil nil nil)
                    ("lock" ,#'activate-lock nil nil nil)
                    ("print" ,#'activate-print nil nil nil)))
         (accels1 `(("app.about" "F1")
                    ("app.shortcuts" "<Control>question")
                    ("app.quit" "<Control>q")
                    ("app.open-in" "<Control>n")
                    ("win.dark" "<Control>d")
                    ("win.search" "<Control>f")
                    ("win.background" "<Control>b")
                    ("win.open" "<Control>o")
                    ("win.record" "<Control>r")
                    ("win.lock" "<Control>l")
                    ("win.fullscreen" "F11")))
         (accels2 `(("app.cut" ("<Control>x"))
                    ("app.copy" ("<Control>c"))
                    ("app.paste" ("<Control>v"))
                    ("win.delete" ("Delete"))))
         ;; Create a new GtkProvider
         (cssfile "/com/crategus/gtk4-widget-factory/widget-factory.css")
         (provider (gtk:css-provider-new))
         ;; Create a new GtkBuilder
         (uifile "/com/crategus/gtk4-widget-factory/widget-factory.ui")
         (builder (gtk:builder-new)))

    ;; Load and apply CSS from resource
    (gtk:css-provider-load-from-resource provider cssfile)
    (gtk:style-context-add-provider-for-display (gdk:display-default) provider)

    ;; Load the GtkBuilder UI definition from resource
    (unless (gtk:builder-add-from-resource builder uifile)
      (error "Cannot load the UI definition resource file."))

    ;; FIXME: The shortcuts window is not shown. What is the problem?!
    (let ((resource "/com/crategus/gtk4-widget-factory/gtk/help-overlay.ui"))
      (gtk:builder-add-from-resource builder resource))

    ;; Load pictures in the tabs of the notebook on page 1
    (load-texture-in-thread (gtk:builder-object builder "notebook_sunset")
                            "/com/crategus/gtk4-widget-factory/sunset.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_nyc")
                            "/com/crategus/gtk4-widget-factory/nyc.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_beach")
                            "/com/crategus/gtk4-widget-factory/beach.jpg")

    (let* ((controller (make-instance 'gtk:shortcut-controller
                                      :propagation-phase :bubble))
           (window (gtk:builder-object builder "window"))
           (stack (gtk:builder-object builder "toplevel_stack"))
           (searchbar (gtk:builder-object builder "searchbar"))
           (infobar (gtk:builder-object builder "infobar"))
           (dialog (gtk:builder-object builder "selection_dialog"))
           (flowbox (gtk:builder-object builder "selection_flowbox"))
           (open-menubutton (gtk:builder-object builder "open_menubutton"))
           (record-button (gtk:builder-object builder "record_button"))
           (lockbutton (gtk:builder-object builder "lockbutton"))
           (statusbar (gtk:builder-object builder "statusbar"))
           (toolbar (gtk:builder-object builder "toolbar")))

      ;; Store widgets from UI definition as application properties
      (setf (widget-factory-window factory) window)
      (setf (widget-factory-stack factory) stack)
      (setf (widget-factory-searchbar factory) searchbar)
      (setf (widget-factory-infobar factory) infobar)
      (setf (widget-factory-selection-dialog factory) dialog)
      (setf (widget-factory-selection-flowbox factory) flowbox)
      (setf (widget-factory-open-menubutton factory) open-menubutton)
      (setf (widget-factory-record-button factory) record-button)
      (setf (widget-factory-lockbutton factory) lockbutton)

      (setf (widget-factory-range-from-spin factory)
            (gtk:builder-object builder "range_from_spin"))
      (setf (widget-factory-range-to-spin factory)
            (gtk:builder-object builder "range_to_spin"))
      (setf (widget-factory-print-button factory)
            (gtk:builder-object builder "print_button"))

      ;; Prepare the application window
      (setf (gtk:window-icon-name window) "org.gtk.WidgetFactory4")
      (setf (gtk:window-application window) application)
      (gtk:widget-add-controller window controller)
      ;; Add the action entries
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
                     (shortcut (gtk:shortcut-new trigger action)))
                (gtk:shortcut-controller-add-shortcut controller shortcut))))

      ;; Gear menu

      ;; FIXME: UI definition does not work. Check gtk:builder-cl-scope
      ;; implementation. Connect signal handler for change of the transition
      ;; speed by hand.
      (g:signal-connect (gtk:builder-object builder "scale-transition-speed")
                        "value-changed"
                        #'transition-speed-changed)

      ;; --- Various dialogs ---------------------------------------------------



      ;; --- On Page 1 ---------------------------------------------------------

      ;; FIXME: This is a workaround for setting style classes. The style
      ;; classes defined in the UI definition file are not applied.
      #+nil
      (set-styles-on-page1 builder)

      ;; FIXME: This is a workaround to fill the grid on page1
      #+nil
      (fill-grid-on-page1 (gtk:builder-object builder "page1-grid1"))

      ;; Connect adjustment for entry and progress bar widgets
      (let ((adjustment (gtk:builder-object builder "adjustment1"))
            (progressbar (gtk:builder-object builder "progressbar3"))
            (entry (gtk:builder-object builder "entry1")))
        (g:signal-connect adjustment "value-changed"
                          (lambda (adj)
                            (pulse-update adj progressbar)))
        (pulse-update adjustment progressbar)
        (g:signal-connect adjustment "value-changed"
                          (lambda (adj)
                            (pulse-update adj entry)))
        (pulse-update adjustment entry)
        (g:signal-connect entry "icon-release" #'on-entry-icon-release))

       ;; Set GtkScaleFormatFunc callback functions
       (gtk:scale-set-format-value-func (gtk:builder-object builder "scale3")
               (lambda (scale value)
                 (declare (ignore scale))
                 (format nil "~,1f" value)))
       (gtk:scale-set-format-value-func (gtk:builder-object builder "scale4")
               (lambda (scale value)
                 (declare (ignore scale value))
                 " "))

      ;; Drag and drop GtkNotebook notebook1
      ;; FIXME: Does not work as expected. What is wrong?
      #+nil
      (gobject:with-value (value "GObject")

        (let ((source (gtk:builder-object builder "notebook-sunset-drag")))
          (g:signal-connect source "prepare"
                  (lambda (source x y)
                    (declare (ignore x y))
                    (format t "in PREPARE for drag source~%")
                    (let ((picture (gtk:event-controller-widget source)))
                      (setf (g:value-object value) picture)
                      (gdk:content-provider-new-for-value value)))))

        (let ((target (gtk:builder-object builder "notebook-sunset-drop")))
          (g:signal-connect target "drop"
                  (lambda (dest value x y)
                    (declare (ignore x y))
                    (format t "in DROP for drop target~%")
                    (let ((picture (gtk:event-controller-widget dest))
                          (paintable (g:value-object value)))
                      (setf (gtk:picture-paintable picture) paintable)
                      t)))))


      ;; --- On Page 2 ---------------------------------------------------------

      ;; Prepare the statusbar on page 2 for the text view
      (gtk:statusbar-push statusbar 0 "All systems are operating normally.")
      (g:action-map-add-action window
                               (g:property-action-new "statusbar"
                                                      statusbar
                                                      "visible"))
      ;; Prepare the toolbar on page 2 for the text view
      (g:action-map-add-action window
                               (g:property-action-new "toolbar"
                                                      toolbar
                                                      "visible"))


      ;; Signal handler for infobar on page 2
      (g:signal-connect infobar "response"
                        (lambda (infobar response)
                          (when (= -7 response)
                            (setf (gtk:widget-visible infobar) nil))))

        ;; Show info on page 2
        (let ((adj (gtk:builder-object builder "adjustment2"))
              (page2reset (gtk:builder-object builder "page2reset"))
              (page2dismiss (gtk:builder-object builder "page2dismiss"))
              (page2note (gtk:builder-object builder "page2note")))
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
               (let ((value (gtk:adjustment-value adjustment))
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
      (let ((button (gtk:builder-object builder "act_action_dialog"))+
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
#|
  widget = (GtkWidget *)gtk_builder_get_object (builder, "act_action_dialog");
  stack = (GtkWidget *)gtk_builder_get_object (builder, "toplevel_stack");
  g_signal_connect (widget, "clicked", G_CALLBACK (action_dialog_button_clicked), stack);
  g_signal_connect (stack, "notify::visible-child-name", G_CALLBACK (page_changed_cb), NULL);
  page_changed_cb (stack, NULL, NULL);
|#

      ;; --- On Page 3 ---------------------------------------------------------

      ;; Set signal handler for "record_button" on page 3
      (let ((button (gtk:builder-object builder "record_button")))
        (g:signal-connect button "toggled"
                (lambda (button)
                    (if (gtk:toggle-button-active button)
                        (gtk:widget-remove-css-class button "destructive-action")
                        (gtk:widget-add-css-class button "destructive-action")))))

      ;; Set signal handler for GtkComboBox "page_combo" on page 3
      (let ((combo (gtk:builder-object builder "page_combo")))
        (g:signal-connect combo "changed"
                (lambda (combo)
                  (let ((from (widget-factory-range-from-spin factory))
                        (to (widget-factory-range-to-spin factory))
                        (print (widget-factory-print-button factory))
                        (active (gtk:combo-box-active combo)))
                    (cond ((= 0 active)
                           (setf (gtk:widget-sensitive from) t)
                           (setf (gtk:widget-sensitive to) t)
                           (setf (gtk:widget-sensitive print) t))
                          ((= 1 active)
                           (setf (gtk:spin-button-value from) 1)
                           (setf (gtk:spin-button-value to) 99)
                           (setf (gtk:widget-sensitive from) nil)
                           (setf (gtk:widget-sensitive to) nil)
                           (setf (gtk:widget-sensitive print) t))
                          ((= 2 active)
                           (setf (gtk:spin-button-value from) 7)
                           (setf (gtk:spin-button-value to) 7)
                           (setf (gtk:widget-sensitive from) nil)
                           (setf (gtk:widget-sensitive to) nil)
                           (setf (gtk:widget-sensitive print) t))
                          ((= 4 active)
                           (setf (gtk:widget-sensitive from) nil)
                           (setf (gtk:widget-sensitive to) nil)
                           (setf (gtk:widget-sensitive print) nil)))))))
      ;; Set signal handler for GtkSpinButton "range_from_spin" on page3
      (let ((spinbutton (widget-factory-range-from-spin factory)))
        (g:signal-connect spinbutton "changed"
                (lambda (button-from)
                  (let* ((button-to (widget-factory-range-to-spin factory))
                         (val1 (gtk:spin-button-value-as-int button-from))
                         (val2 (gtk:spin-button-value-as-int button-to)))
                    (when (> val1 val2)
                      (setf (gtk:spin-button-value button-to) val1))))))
      ;; Set signal handler for GtkSpinButton "range_to_spin" on page3
      (let ((spinbutton (widget-factory-range-to-spin factory)))
        (g:signal-connect spinbutton "changed"
                (lambda (button-to)
                  (let* ((button-from (widget-factory-range-from-spin factory))
                         (val1 (gtk:spin-button-value-as-int button-from))
                         (val2 (gtk:spin-button-value-as-int button-to)))
                    (when (> val1 val2)
                      (setf (gtk:spin-button-value button-from) val2))))))


      ;; Show the application window
      (gtk:window-present window))))

;;; ----------------------------------------------------------------------------

(defun local-options (application options)
  (declare (ignore application))
  (format t "Application in HANDLE-LOCAL-OPTIONS: ~a~%" options)
  (let ((status -1)) ; Default to continue processing
    (when (g:variant-dict-contains options "version")
      (format t "~%~a~%" (gtk:cl-cffi-gtk-build-info))
      ;; Set status to zero to exit the application
      (setf status 0))
    status))

;;; ----------------------------------------------------------------------------

(defun startup (application)
  (format t "Application in STARTUP: ~a~%" application))

(defun shutdown (application)
  (format t "Application in SHUTDOWN: ~a~%" application))

;;; ----------------------------------------------------------------------------

(defun gtk4-widget-factory (&rest argv)

  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     "gtk4-widget-factory.xml"
                                     :package "gtk4-widget-factory"))

    (unless (string= "GTK4 Widget Factory" (g:application-name))
      (setf (g:application-name) "GTK4 Widget Factory"))

    (let ((argv (cons (g:application-name)
                      (or argv (uiop:command-line-arguments))))
          (app (setf factory
                     (make-instance 'widget-factory
                                    :application-id
                                    "com.crategus.gtk4-widget-factory"
                                    :register-session t
                                    :resource-base-path (cffi:null-pointer))))

          (entries (list (list "about" #'activate-about)
                         (list "shortcuts" #'activate-shortcuts-window)
                         (list "quit" #'activate-quit)
                         (list "inspector" #'activate-inspector)
                         (list "main" nil "s" "'steak'")
                         (list "wine" nil nil "false")
                         (list "beer" nil nil "false")
                         (list "water" nil nil "true")
                         (list "dessert" nil "s" "'bars'")
                         (list "pay" nil "s")
                         (list "share" #'activate-action)
                         (list "labels" #'activate-action)
                         (list "new" #'activate-action)
                         (list "open" #'activate-open-file)
                         (list "open-in" #'activate-action)
                         (list "open-tab" #'activate-action)
                         (list "open-window" #'activate-action)
                         (list "save" #'activate-action)
                         (list "save-as" #'activate-action)
                         (list "cut" #'activate-action)
                         (list "copy" #'activate-action)
                         (list "paste" #'activate-action)
                         (list "pin" #'toggle-action nil "true")
                         (list "size" #'select-action "s" "'medium'")
                         (list "berk" #'toggle-action nil "true")
                         (list "broni" #'toggle-action nil "true")
                         (list "drutt" #'toggle-action nil "true")
                         (list "upstairs" #'toggle-action nil "true")
                         (list "option-a" #'activate-action)
                         (list "option-b" #'activate-action)
                         (list "option-c" #'activate-action)
                         (list "option-d" #'activate-action)
                         (list "check-on" nil nil "true")
                         (list "check-off" nil nil "false")
                         (list "radio-x" nil "s" "'x'")
                         (list "check-on-disabled" nil nil "true")
                         (list "check-off-disabled" nil nil "false")
                         (list "radio-x-disabled" nil "s" "'x'")
         )))

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

      ;; Add option to be handled by the application
      (g:application-add-main-option app "version"
                                         (char-code #\v)
                                         :none
                                         :none
                                         "Show program version"
                                         "")
      ;; Connect signal handlers to the application
      (g:signal-connect app "handle-local-options" #'local-options)
      (g:signal-connect app "activate" #'activate)
      (g:signal-connect app "startup" #'startup)
      (g:signal-connect app "shutdown" #'shutdown)
      ;; Run the application
      (g:application-run app argv))))

;;; --- End of file gtk4-widget-factory.lisp -----------------------------------
