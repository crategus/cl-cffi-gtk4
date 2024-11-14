;;; ----------------------------------------------------------------------------
;;; gtk4-widget-factory.lisp
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
  (:export :gtk4-widget-factory))

(in-package :gtk4-widget-factory)

(defvar *window* nil)
(defvar *page-stack* nil)

(defvar *currentpage* 0)
(defun on-page (i)
  (= i *currentpage*))

;;; ----------------------------------------------------------------------------

;; Code to load texures in a thread

(defun load-texture-thread (task source path)
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

;; The code to pulse a progress bar and an entry on page 1

(let ((pulse-time 250))

  ;; TODO: We need a cffi callback. Can we improve the implementation?!
  (cffi:defcallback remove-pulse :void ((data :pointer))
    (let ((id (glib:get-stable-pointer-value data)))
      (g:source-remove id)))

  (defun pulse-it (widget)
    ;; Select the type of widget and pulse it
    (cond ((eq (g:gtype "GtkEntry") (g:type-from-instance widget))
           (gtk:entry-progress-pulse widget))
          ((eq (g:gtype "GtkProgressBar") (g:type-from-instance widget))
           (gtk:progress-bar-pulse widget))
          (t
           (error "PULSE-IT: unknown type of widget")))
    ;; Create and set a new timeout source for the widget for the next pulse
    (g:object-set-data-full widget
                            "pulse-id"
                            (g:timeout-add pulse-time
                                           (lambda ()
                                             (pulse-it widget)))
                            (cffi:callback remove-pulse))
    ;; Remove this timeout source
    g:+source-remove+)

  (defun update-pulse-time (adjustment widget)
    (let* ((value (gtk:adjustment-value adjustment))
           (id (g:object-data widget "pulse-id"))
           (mode (g:object-data widget "pulse-mode")))
      ;; Global to the pulse time functions
      (setf pulse-time (truncate (+ 50 (* 4 value))))

      (cond ((= value 100)
             (setf (g:object-data widget "pulse-id") nil))
            ((< value 100)
             (when (and (not id)
                        (or (eq (g:gtype "GtkProgressBar")
                                (g:type-from-instance widget))
                            (= 2 (mod mode 3))))
               (g:object-set-data-full widget
                                       "pulse-id"
                                       (g:timeout-add pulse-time
                                                      (lambda ()
                                                        (pulse-it widget)))
                                       (cffi:callback remove-pulse))))))))

;;; ----------------------------------------------------------------------------

(defclass my-text-view (gtk:text-view)
  ((tv :accessor my-text-view-tv)
   (texture :accessor my-text-view-texture)
   (adjustment :accessor my-text-view-adjustment))
  (:gname . "MyTextView")
  (:metaclass gobject:gobject-class))

(gobject::register-object-type-implementation "MyTextView"    ; name
                                              my-text-view    ; class
                                              "GtkTextView"   ; parent
                                              nil             ; interfaces
                                              nil)            ; properties

;;; ----------------------------------------------------------------------------

(defun activate-about (action parameter)
  (format t "in ACTIVATE-ABOUT: ~a, ~a~%" action parameter))

(defun activate-shortcuts-window (action parameter)
  (format t "in ACTIVATE-SHORTCUTS-WINDOW: ~a, ~a~%" action parameter)
  (let* ((window *window*)
         (button (g:object-data window "open_menubutton")))
    (gtk:menu-button-popdown button)
    (gtk:widget-activate-action window "win.show-help-overlay")))

(defun activate-quit (action parameter)
  (format t "in ACTIVATE-QUIT: ~a, ~a~%" action parameter))


(defun activate-inspector (action parameter)
  (format t "in ACTIVATE-INSPECTOR: ~a, ~a~%" action parameter)
  (gtk:window-set-interactive-debugging t))


(defun activate-action (action parameter)
  (format t "in ACTIVATE-ACTION: ~a, ~a~%" action parameter))

(defun activate-open-file (action parameter)
  (format t "in ACTIVATE-OPEN-FILE: ~a, ~a~%" action parameter))

(defun toggle-action (action parameter)
  (format t "in TOGGLE-ACTION: ~a, ~a~%" action parameter))

(defun select-action (action parameter)
  (format t "in SELECT-ACTION: ~a, ~a~%" action parameter))

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
  (setf (g:simple-action-state action) state)
  (if (g:variant-boolean state)
      (gtk:window-fullscreen *window*)
      (gtk:window-unfullscreen *window*)))

(defun change-transition-state (action state)
  (let (transition)
    (setf (g:simple-action-state action) state)
    (if (g:variant-boolean state)
        (setf transition :crossfade)
        (setf transition :none))
    (setf (gtk:stack-transition-type *page-stack*) transition)))

(defun get-busy (action parameter)
  (declare (ignore action parameter))
  (let ((application (gtk:window-application *window*))
        (cursor (gdk:cursor-new-from-name "wait" nil)))
    (g:application-mark-busy application)
    (setf (gdk:surface-cursor (gtk:native-surface *window*)) cursor)
    (g:timeout-add 5000
                   (lambda ()
                     (let ((application (gtk:window-application *window*))
                           (native (gtk:native-surface *window*)))
                       (setf (gtk:widget-sensitive *window*) t)
                       (setf (gdk:surface-cursor native) nil)
                       (g:application-unmark-busy application)
                       g:+source-remove+)))
    (setf (gtk:widget-sensitive *window*) nil)))

(defun activate-search (action parameter)
  (declare (ignore action parameter))
  (when (on-page 2)
    (let ((searchbar (g:object-data *window* "searchbar")))
      (setf (gtk:search-bar-search-mode-enabled searchbar) t))))

(defun activate-delete (action parameter)
  (declare (ignore action parameter))
  (when (on-page 2)
    (let ((infobar (g:object-data *window* "infobar")))
      (setf (gtk:widget-visible infobar) t))))

(defun add-background (flowbox file texture)
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
           (texture (gdk:memory-texture-new 110 70 :B8G8R8A8-PREMULTIPLIED bytes (* 4 110)))
           (child (gtk:picture-new-for-paintable texture)))
      (gtk:widget-add-css-class child "frame")
      (gtk:flow-box-insert flowbox child -1))
    (let ((resources '("sunset.jpg" "portland-rose.jpg" "beach.jpg" "nyc.jpg")))
      (dolist (resource resources)
        (let* ((file (concatenate 'string "/org/gtk/WidgetFactory4/" resource))
               (pixbuf (gdk:pixbuf-new-from-resource-at-scale file 110 110 t))
               (texture (gdk:texture-new-for-pixbuf pixbuf)))
          (add-background flowbox file texture))))))

(defun activate-background (action parameter)
  (when (on-page 2)
    (let ((dialog (g:object-data *window* "selection_dialog"))
          (flowbox (g:object-data *window* "selection_flowbox")))
      (setf (gtk:widget-visible dialog) t)
      (populate-flowbox flowbox)
)))

(defun activate-open ()
)

(defun activate-record ()
)

(defun activate-lock ()
)

(defun activate-print ()
)

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
             (setf *currentpage* 1))
            ((string= "page2" name)
             (setf *currentpage* 2))
            ((string= "page3" name)
             (setf *currentpage* 3)
             (let ((page (gtk:stack-visible-child stack)))
               (set-needs-attention page nil)))))))

;;; ----------------------------------------------------------------------------

;; Signal handlers for objects from the UI definition

(defun transition-speed-changed (range)
  (let ((value (round (gtk:range-value range))))
    (setf (gtk:stack-transition-duration *page-stack*) value)))

(defun on-entry-icon-release (entry pos)
  (format t "  in ON-ENTRY-ICON-RELEASE : ~a ~a~%" entry pos))

(defun on-picture-drag-prepare (source x y)
  (format t "  in ON-PICTURE-DRAG-PREPATE : ~a ~a ~a~%" source x y))

(defun on-picture-drop (target value x y)
  (format t "  in ON-PICTURE-DROP : ~a ~a ~a ~a~%" target value x y))

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

(defun activate (application)
  ;; Load and apply CSS
  (let ((path "/org/gtk/WidgetFactory4/widget-factory.css")
        (provider (gtk:css-provider-new)))
    (gtk:css-provider-load-from-resource provider path)
    (gtk:style-context-add-provider-for-display (gdk:display-default) provider))

  (let* ((entries (list (list "dark" nil nil "false" #'change-dark-state)
                        (list "theme" nil "s" "'current'" #'change-theme-state)
                        (list "transition" nil nil "true"
                                                   #'change-transition-state)
                        (list "search" #'activate-search nil nil nil)
                        (list "delete" #'activate-delete nil nil nil)
                        (list "busy" #'get-busy nil nil nil)
                        (list "fullscreen" nil nil "false" #'change-fullscreen)
                        (list "background" #'activate-background nil nil nil)
                        (list "open" #'activate-open nil nil nil)
                        (list "record" #'activate-record nil nil nil)
                        (list "lock" #'activate-lock nil nil nil)
                        (list "print" #'activate-print nil nil nil)))
         (accels1 (list (list "app.about" "F1")
                        (list "app.shortcuts" "<Control>question")
                        (list "app.quit" "<Control>q")
                        (list "app.open-in" "<Control>n")
                        (list "win.dark" "<Control>d")
                        (list "win.search" "<Control>s")
                        (list "win.background" "<Control>b")
                        (list "win.open" "<Control>o")
                        (list "win.record" "<Control>r")
                        (list "win.lock" "<Control>l")
                        (list "win.fullscreen" "F11")))
         (accels2 (list (list "app.cut" (list "<Control>x"))
                        (list "app.copy" (list "<Control>c"))
                        (list "app.paste" (list "<Control>v"))
                        (list "win.delete" (list "Delete"))))
         ;; Create a new GtkBuilder
         (path "/org/gtk/WidgetFactory4/widget-factory.ui")
         (builder (gtk:builder-new)))
    ;; Load the GtkBuilder UI definition
    (unless (gtk:builder-add-from-resource builder path)
      (error "Cannot load the UI definition resource file."))

    ;; FIXME: The shortcuts window is not shown. What is the problem?!
    (gtk:builder-add-from-resource builder
                                   "/org/gtk/WidgetFactory4/gtk/help-overlay.ui")

    ;; Load pictures in the tabs of the notebook on page 1
    (load-texture-in-thread (gtk:builder-object builder "notebook_sunset")
                            "/org/gtk/WidgetFactory4/sunset.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_nyc")
                            "/org/gtk/WidgetFactory4/nyc.jpg")
    (load-texture-in-thread (gtk:builder-object builder "notebook_beach")
                            "/org/gtk/WidgetFactory4/beach.jpg")

    (let* ((controller (make-instance 'gtk:shortcut-controller
                                      :propagation-phase :bubble))
           (window (gtk:builder-object builder "window"))
           (statusbar (gtk:builder-object builder "statusbar"))
           (toolbar (gtk:builder-object builder "toolbar"))
           (stack (gtk:builder-object builder "toplevel_stack"))
           (infobar (gtk:builder-object builder "infobar"))
           (flowbox (gtk:builder-object builder "selection_flowbox")))

      ;; Prepare the application window
      (setf *window* window)
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

      ;; Prepare the statusbar on page 2 for the text view
      (gtk:statusbar-push statusbar 0 "All systems are operating normally.")
      (g:action-map-add-action window
                               (g:property-action-new "statusbar"
                                                      statusbar
                                                      "visible"))
      ;; Prepare the toolbar
      (g:action-map-add-action window
                               (g:property-action-new "toolbar"
                                                      toolbar
                                                      "visible"))

      ;; Connect adjustment for entry and progress bar widgets
      (let ((adj (gtk:builder-object builder "adjustment1"))
            (progressbar (gtk:builder-object builder "progressbar3"))
            (entry (gtk:builder-object builder "entry1")))
#|
        (g:signal-connect adj "value-changed"
                          (lambda (adjustment)
                            (update-pulse-time adjustment progressbar)))
        ;; Set initial pulse mode for the entry
        (setf (g:object-data entry "pulse-mode") 0)
        (g:signal-connect adj "value-changed"
                          (lambda (adjustment)
                            (update-pulse-time adjustment entry)))
        (g:signal-connect entry "icon-release"
            (lambda (entry pos)
              (when (eq :secondary pos)
                (let ((mode (g:object-data entry "pulse-mode")))
                  ;; Increase the pulse mode of the entry
                  (setf (g:object-data entry "pulse-mode") (+ 1 mode))
                  (cond ((= 0 (mod mode 3))
                         (setf (g:object-data entry "pulse-id") nil)
                         (setf (gtk:entry-progress-fraction entry) 0))
                        ((= 1 (mod mode 3))
                         (setf (gtk:entry-progress-fraction entry) 0.25))
                        ((= 2 (mod mode 3))
                         (setf (gtk:entry-progress-pulse-step entry) 0.1)
                         (pulse-it entry)))))))
|#
                         )
        ;; Show info on page 2
        (let ((adj (gtk:builder-object builder "adjustment2"))
              (page2reset (gtk:builder-object builder "page2reset"))
              (page2dismiss (gtk:builder-object builder "page2dismiss"))
              (page2note (gtk:builder-object builder "page2note")))
#|
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
                       (= 0 (mod value 3))))))
|#
                       )


;; "mic-button"
;;  gtk_builder_cscope_add_callback (scope, on_scale_button_value_changed);
;;  gtk_builder_cscope_add_callback (scope, on_record_button_toggled);
;;  gtk_builder_cscope_add_callback (scope, on_page_combo_changed);
;;  gtk_builder_cscope_add_callback (scope, on_range_from_changed);
;;  gtk_builder_cscope_add_callback (scope, on_range_to_changed);
;;  gtk_builder_cscope_add_callback (scope, on_picture_drag_prepare);
;;  gtk_builder_cscope_add_callback (scope, on_picture_drop);
;;  gtk_builder_cscope_add_callback (scope, tab_close_cb);
;;  gtk_builder_cscope_add_callback (scope, increase_icon_size);
;;  gtk_builder_cscope_add_callback (scope, decrease_icon_size);
;;  gtk_builder_cscope_add_callback (scope, osd_frame_pressed);
;;  gtk_builder_cscope_add_callback (scope, age_entry_changed);
;;  gtk_builder_cscope_add_callback (scope, validate_more_details);
;;  gtk_builder_cscope_add_callback (scope, mode_switch_state_set);
;;  gtk_builder_cscope_add_callback (scope, level_scale_value_changed);
;;  gtk_builder_cscope_add_callback (scope, transition_speed_changed);
;;  gtk_builder_cscope_add_callback (scope, reset_icon_size);




      (setf *page-stack* stack)
      (g:signal-connect stack "notify::visible-child-name" #'page-changed-cb)

      ;; Store the search bar on the property list of the application window
      (setf (g:object-data window "searchbar")
            (gtk:builder-object builder "searchbar"))

      ;; Store the info bar on the property list of the application window
      (setf (g:object-data window "infobar") infobar)
      (g:signal-connect infobar "response"
                        (lambda (infobar response)
                          (when (= -7 response)
                            (setf (gtk:widget-visible infobar) nil))))

      ;; Store the selection dialog on the property list of the application
      ;; window
      (setf (g:object-data window "selection_dialog")
            (gtk:builder-object builder "selection_dialog"))

#|
  dialog = (GtkWidget *)gtk_builder_get_object (builder, "selection_dialog");
  g_object_set_data (G_OBJECT (window), "selection_dialog", dialog);

  widget = (GtkWidget *)gtk_builder_get_object (builder, "text3");
  g_signal_connect (dialog, "response", G_CALLBACK (close_selection_dialog), widget);

  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "opacity");
  my_text_view_set_adjustment ((MyTextView *)widget, gtk_range_get_adjustment (GTK_RANGE (widget2)));
  widget = (GtkWidget *)gtk_builder_get_object (builder, "selection_dialog_button");
  g_signal_connect (widget, "clicked", G_CALLBACK (show_dialog), dialog);

  widget2 = (GtkWidget *)gtk_builder_get_object (builder, "selection_flowbox");
  g_object_set_data (G_OBJECT (window), "selection_flowbox", widget2);
  g_signal_connect_swapped (widget, "clicked", G_CALLBACK (populate_flowbox), widget2);
|#

      ;; Store the flowbox on the property list
      (setf (g:object-data window "selection_flowbox") flowbox)


      ;;  --- PAGE 3 -----------------------------------------------------------

      (setf (g:object-data window "open_menubutton")
            (gtk:builder-object builder "open_menubutton"))

      ;; -----------------------------------------------------------------------

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
          (app (make-instance 'gtk:application
                              :application-id
                              "com.crategus.gtk4-widget-factory"
                              :register-session t
                              :resource-base-path (cffi:null-pointer)))

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
