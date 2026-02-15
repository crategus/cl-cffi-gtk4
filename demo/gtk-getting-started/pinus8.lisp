;;; pinus8.lisp

(defpackage :pinus8
  (:use :common-lisp)
  (:export #:pinus
           #:search-text-changed
           #:visible-child-changed))

(in-package :pinus8)

(defvar *window* nil)

;;; ----------------------------------------------------------------------------

;; Implementation of the PREFS subclass

(gobject:define-gobject-subclass "Pinus8Prefs" prefs
  (:superclass gtk:dialog
   :export t
   :interfaces ())
  ((settings
    prefs-settings
    "settings" "GObject" t t)
   (font
    prefs-font
    "font" "GObject" t t)
   (transition
    prefs-transition
    "transition" "GObject" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'prefs))) cclass data)
  (let ((path "/com/crategus/pinus/prefs8.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus8Prefs" path)
    (gtk:widget-class-bind-template-child "Pinus8Prefs" "font")
    (gtk:widget-class-bind-template-child "Pinus8Prefs" "transition")))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'prefs))) instance data)
  (gtk:widget-init-template instance))

(defun string-to-font-desc (value variant)
  (let* ((str (g:variant-string variant))
         (desc (pango:font-description-from-string str)))
    (g:value-take-boxed value desc)
    t))

(defun font-desc-to-string (value vtype)
  (declare (ignore vtype))
  (let* ((desc (g:value-boxed value))
         (str (pango:font-description-to-string desc)))
    (g:variant-new-string str)))

(defun transition-to-pos (value variant)
  (let ((str (string-downcase (g:variant-string variant))))
    (cond ((string= "none" str)
           (g:value-set value 0 "guint"))
          ((string= "crossfade" str)
           (g:value-set value 1 "guint"))
          (t
           (g:value-set value 2 "guint")))
    t))

(defun pos-to-transition (gvalue vtype)
  (declare (ignore vtype))
  (let ((value (g:value-get gvalue "guint")))
    (cond ((= 0 value)
           (g:variant-new-string "none"))
          ((= 1 value)
           (g:variant-new-string "crossfade"))
          ((= 2 value)
           (g:variant-new-string "slide-left-right"))
          (t
           (error "POS-TO-TRANSITION")))))

(defun prefs-new (win)
  (let* ((gtk-init:*warn-deprecated* nil)
         (prefs (make-instance 'prefs
                               :transient-for win
                               :use-header-bar 1))
         (settings (g:settings-new "com.crategus.pinus"))
         (font (gtk:widget-template-child prefs "Pinus8Prefs" "font"))
         (transition (gtk:widget-template-child prefs "Pinus8Prefs" "transition")))
    ;; Store settings
    (setf (prefs-settings prefs) settings)
    ;; Store widgets from template
    (setf (prefs-font prefs) font)
    (setf (prefs-transition prefs) transition)
    ;; Set bindings
    (g:settings-bind-with-mapping settings "font"
                                  font "font-desc"
                                  :default
                                  #'string-to-font-desc
                                  #'font-desc-to-string)
    (g:settings-bind-with-mapping settings "transition"
                                  transition "selected"
                                  :default
                                  #'transition-to-pos
                                  #'pos-to-transition)
    prefs))

;;; ----------------------------------------------------------------------------

;; Implementation of the WINDOW subclass

(gobject:define-gobject-subclass "Pinus8Window" window
  (:superclass gtk:application-window
   :export t
   :interfaces ())
  ((stack
    window-stack
    "stack" "GObject" t t)
   (gears
    window-gears
    "gears" "GObject" t t)
   (settings
    window-settings
    "settings" "GObject" t t)
   (search
    window-search
    "search" "GObject" t t)
   (searchbar
    window-searchbar
    "searchbar" "GObject" t t)
   (searchentry
    window-searchentry
    "searchentry" "GObject" t t)
   (sidebar
    window-sidebar
    "sidebar" "GObject" t t)
   (words
    window-words
    "words" "GObject" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'window))) cclass data)
  (let ((path "/com/crategus/pinus/window8.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus8Window" path)
    ;; Set builder scope
    (gtk:widget-class-set-template-scope "Pinus8Window"
                                         (make-instance 'gtk:builder-cl-scope))
    ;; Bind objects from the template
    (gtk:widget-class-bind-template-child "Pinus8Window" "stack")
    (gtk:widget-class-bind-template-child "Pinus8Window" "gears")
    (gtk:widget-class-bind-template-child "Pinus8Window" "search")
    (gtk:widget-class-bind-template-child "Pinus8Window" "searchbar")
    (gtk:widget-class-bind-template-child "Pinus8Window" "searchentry")
    (gtk:widget-class-bind-template-child "Pinus8Window" "sidebar")
    (gtk:widget-class-bind-template-child "Pinus8Window" "words")))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'window))) instance data)
  (gtk:widget-init-template instance))

(defun window-new (app)
  (let* ((path "/com/crategus/pinus/gears-menu.ui")
         (builder (gtk:builder-new-from-resource path))
         (menu (gtk:builder-object builder "menu"))
         (window (g:object-new "Pinus8Window" :application app))
         (stack (gtk:widget-template-child window "Pinus8Window" "stack"))
         (gears (gtk:widget-template-child window "Pinus8Window" "gears"))
         (search (gtk:widget-template-child window "Pinus8Window" "search"))
         (searchbar (gtk:widget-template-child window "Pinus8Window" "searchbar"))
         (searchentry (gtk:widget-template-child window "Pinus8Window" "searchentry"))
         (sidebar (gtk:widget-template-child window "Pinus8Window" "sidebar"))
         (words (gtk:widget-template-child window "Pinus8Window" "words"))
         (settings (g:settings-new "com.crategus.pinus")))
    ;; Store window in global
    (setf *window* window)
    ;; Store widgets from template
    (setf (window-stack window) stack)
    (setf (window-gears window) gears)
    (setf (window-search window) search)
    (setf (window-searchbar window) searchbar)
    (setf (window-searchentry window) searchentry)
    (setf (window-sidebar window) sidebar)
    (setf (window-words window) words)
    ;; Set gears menu
    (setf (gtk:menu-button-menu-model gears) menu)
    ;; Store settings
    (setf (window-settings window) settings)
    ;; Set bindings
    (g:settings-bind settings "transition"
                     stack "transition-type"
                     :default)
    (g:settings-bind settings "show-words"
                     sidebar "reveal-child"
                     :default)
    (g:object-bind-property search "active"
                            searchbar "search-mode-enabled"
                            :bidirectional)
    (g:signal-connect sidebar "notify::reveal-child"
                      (lambda (sidebar pspec)
                        (declare (ignore sidebar pspec))
                        (update-words window)))
    ;; Create and set actions
    (let ((action (g:settings-create-action settings "show-words")))
      (g:action-map-add-action window action))
    ;; Return initialized window
    window))

(defun search-text-changed (entry)
  (let ((text (gtk:editable-text entry)))
    (unless (> 0 (length text))
      (let* ((tab (gtk:stack-visible-child (window-stack *window*)))
             (view (gtk:scrolled-window-child tab))
             (buffer (gtk:text-view-buffer view))
             (start (gtk:text-buffer-start-iter buffer)))
        (multiple-value-bind (match-start match-end)
            (gtk:text-iter-search start text :flags :case-insensitive)
          (when (and match-start match-end)
            (gtk:text-buffer-select-range buffer match-start match-end)
            (gtk:text-view-scroll-to-iter view match-start)))))))

(defun visible-child-changed (stack pspec)
  (declare (ignore pspec))
  (unless (gtk:widget-in-destruction stack)
    (setf (gtk:search-bar-search-mode-enabled (window-searchbar *window*)) nil)
    (update-words *window*)))

(defun find-word (button)
  (let ((word (gtk:button-label button)))
    (setf (gtk:editable-text (window-searchentry *window*)) word)))

(defun update-words (win)
  (let ((tab (gtk:stack-visible-child (window-stack win))))
    (when tab
      (let ((words (g:settings-strv (window-settings win) "words")))
        (gtk:list-box-remove-all (window-words win))
        (dolist (word words)
          (let* ((label (format nil "~a" (string-downcase word)))
                 (row (gtk:button-new-with-label label)))
            (g:signal-connect row "clicked" #'find-word)
            (gtk:list-box-append (window-words win) row)))))))

(defun window-open (win filename)
  (let* ((view (make-instance 'gtk:text-view
                              :editable nil
                              :cursor-visible nil))
         (buffer (gtk:text-view-buffer view))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hexpand t
                                  :vexpand t))
         (file (file-namestring filename)))
    (gtk:stack-add-titled (window-stack win) scrolled file file)
    ;; Load the file into the buffer
    (when (and filename (probe-file filename))
      (gtk:text-buffer-load-file buffer filename))
    (let* ((tag (gtk:text-buffer-create-tag buffer "font"))
           (start (gtk:text-buffer-start-iter buffer))
           (end (gtk:text-buffer-end-iter buffer)))
      (g:settings-bind (window-settings win) "font"
                       tag "font"
                       :default)
      (gtk:text-buffer-apply-tag buffer tag start end))
    ;; Make search button sensitive
    (setf (gtk:widget-sensitive (window-search win)) t)
    (update-words win)))

;;; ----------------------------------------------------------------------------

;; Implementation of the APPLICATION subclass

(gobject:define-gobject-subclass "Pinus8Application" application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(gobject:define-vtable ("Pinus8Application" application)
  ;; Parent class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Signals
  (startup (:void (application (g:object application))) :chained :before)
  (activate (:void (application (g:object application))))
  (open (:void
         (app (g:object application))
         (files :pointer)
         (nfiles :int)
         (hint :string)))
  (:skip command-line :pointer))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'application))) cclass data)
)

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'application))) instance data)
)

(defun application-new ()
  (make-instance 'application
                 :application-id "com.crategus.pinus8"
                 :flags :handles-open))

(defun application-activate (app)
  (let ((window (window-new app)))
    (gtk:window-present window)))

(defmethod application-activate-impl ((app application))
  (application-activate app))

(defun application-open (app files nfiles hint)
  (declare (ignore hint))
  (let* ((windows (gtk:application-windows app))
         (window (if windows (first windows) (window-new app))))
    (dotimes (i nfiles)
      (let* ((file (cffi:mem-aref files '(g:object g:file) i))
             (filename (g:file-get-parse-name file)))
        (window-open window filename)))
    (gtk:window-present window)))

(defmethod application-open-impl ((app application) files nfiles hint)
  (application-open app files nfiles hint))

(defun preferences-activated (action parameter application)
  (declare (ignore action parameter))
  (let* ((window (gtk:application-active-window application))
         (prefs (prefs-new window)))
    (gtk:window-present prefs)))

(defun quit-activated (action parameter application)
  (declare (ignore action parameter))
  (let ((windows (gtk:application-windows application)))
    (dolist (window windows)
      (gtk:window-destroy window))))

(defun application-startup (app)
  (let ((entries (list (list "preferences"
                             (lambda (action parameter)
                               (preferences-activated action parameter app)))
                       (list "quit"
                             (lambda (action parameter)
                               (quit-activated action parameter app))))))
    ;; Add actions entries for the application
    (g:action-map-add-action-entries app entries)
    (setf (gtk:application-accels-for-action app "app.quit") "<Control>q")))

(defmethod application-startup-impl ((app application))
  (application-startup app))

;;; ----------------------------------------------------------------------------

(defun pinus (&rest argv)
  (gio:with-resources ((resource (glib-sys:check-and-create-resources
                                         "pinus.gresource.xml"
                                         :sourcedir "resource/"
                                         :package "gtk4-getting-started"
                                         :verbose t)))
    (let ((argv (cons "Pinus 8"
                      (if argv argv (uiop:command-line-arguments))))
          (app (application-new)))
      (g:application-run app argv))))

;;; --- End of file pinus8.lisp ------------------------------------------------
