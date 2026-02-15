;;; pinus7.lisp

(defpackage :pinus7
  (:use :common-lisp)
  (:export #:pinus
           #:search-text-changed
           #:visible-child-changed))

(in-package :pinus7)

(defvar *window* nil)

;;; ----------------------------------------------------------------------------

;; Implementation of the PREFS subclass

(gobject:define-gobject-subclass "Pinus7Prefs" prefs
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
  (let ((path "/com/crategus/pinus/prefs7.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus7Prefs" path)
    (gtk:widget-class-bind-template-child "Pinus7Prefs" "font")
    (gtk:widget-class-bind-template-child "Pinus7Prefs" "transition")))

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
         (font (gtk:widget-template-child prefs "Pinus7Prefs" "font"))
         (transition (gtk:widget-template-child prefs "Pinus7Prefs" "transition")))
    ;; Store settings
    (setf (prefs-settings prefs) settings)
    ;; Store widget from template
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

(gobject:define-gobject-subclass "Pinus7Window" window
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
    "searchbar" "GObject" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'window))) cclass data)
  (let ((path "/com/crategus/pinus/window7.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus7Window" path)
    ;; Set builder scope
    (gtk:widget-class-set-template-scope "Pinus7Window"
                                         (make-instance 'gtk:builder-cl-scope))
    ;; Bind objects from the template
    (gtk:widget-class-bind-template-child "Pinus7Window" "stack")
    (gtk:widget-class-bind-template-child "Pinus7Window" "gears")
    (gtk:widget-class-bind-template-child "Pinus7Window" "search")
    (gtk:widget-class-bind-template-child "Pinus7Window" "searchbar")))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'window))) instance data)
  (gtk:widget-init-template instance))

(defun window-new (app)
  (let* ((path "/com/crategus/pinus/gears-menu.ui")
         (builder (gtk:builder-new-from-resource path))
         (menu (gtk:builder-object builder "menu"))
         (window (g:object-new "Pinus7Window" :application app))
         (stack (gtk:widget-template-child window "Pinus7Window" "stack"))
         (gears (gtk:widget-template-child window "Pinus7Window" "gears"))
         (search (gtk:widget-template-child window "Pinus7Window" "search"))
         (searchbar (gtk:widget-template-child window "Pinus7Window" "searchbar"))
         (settings (g:settings-new "com.crategus.pinus")))
    ;; Store window in global
    (setf *window* window)
    ;; Store widgets from template
    (setf (window-stack window) stack)
    (setf (window-gears window) gears)
    (setf (window-search window) search)
    (setf (window-searchbar window) searchbar)
    ;; Set gears menu
    (setf (gtk:menu-button-menu-model gears) menu)
    ;; Store settings
    (setf (window-settings window) settings)
    ;; Set bindings
    (g:settings-bind settings "transition"
                     stack "transition-type"
                     :default)
    (g:object-bind-property search "active"
                            searchbar "search-mode-enabled"
                            :bidirectional)
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
    (setf (gtk:search-bar-search-mode-enabled (window-searchbar *window*)) nil)))

(defun window-open (win filename)
  (let* ((view (make-instance 'gtk:text-view
                              :editable nil
                              :cursor-visible nil))
         (buffer (gtk:text-view-buffer view))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child view
                                  :hexpand t
                                  :vexpand t))
         (title (file-namestring filename)))
    (gtk:stack-add-titled (window-stack win) scrolled title title)
    ;; Load file into buffer
    (when (and filename (probe-file filename))
      (gtk:text-buffer-load-file buffer filename)
      (let* ((tag (gtk:text-buffer-create-tag buffer "font"))
             (start (gtk:text-buffer-start-iter buffer))
             (end (gtk:text-buffer-end-iter buffer)))
        (g:settings-bind (window-settings win) "font"
                         tag "font"
                         :default)
        (gtk:text-buffer-apply-tag buffer tag start end))
        ;; Make search button sensitive
        (setf (gtk:widget-sensitive (window-search win)) t))))

;;; ----------------------------------------------------------------------------

;; Implementation of the APPLICATION subclass

(gobject:define-gobject-subclass "Pinus7Application" application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(gobject:define-vtable ("Pinus7Application" application)
  ;; Parent class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Signals
  (startup (:void (application (g:object application))) :chained :before)
  (activate (:void (application (g:object application))))
  (open (:void (app (g:object application))
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
                 :application-id "com.crategus.pinus7"
                 :flags :handles-open))

(defun application-activate (app)
  (let ((window (window-new app)))
    (gtk:window-present window)))

(defmethod application-activate-impl ((app application))
  (application-activate app))

(defun application-open (app files nfiles hint)
  (declare (ignore hint))
  (let ((window (gtk:application-active-window app)))
    (unless window
      (setf window (window-new app)))
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
    (let ((argv (cons "Pinus 7"
                      (if argv argv (uiop:command-line-arguments))))
          (app (application-new)))
      (g:application-run app argv))))

;;; --- End of file pinus7.lisp ------------------------------------------------
