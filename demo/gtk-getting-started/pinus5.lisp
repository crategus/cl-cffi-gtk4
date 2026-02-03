;;; pinus5.lisp

(defpackage :pinus5
  (:use :common-lisp)
  (:export #:pinus))

(in-package :pinus5)

;;; ----------------------------------------------------------------------------

;; Implementation of the WINDOW subclass

(gobject:define-gobject-subclass "Pinus5Window" window
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
    "settings" "GObject" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'window))) cclass data)
  (let ((path "/com/crategus/pinus/window5.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus5Window" path)
    (gtk:widget-class-bind-template-child "Pinus5Window" "stack")
    (gtk:widget-class-bind-template-child "Pinus5Window" "gears")))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'window))) instance data)
  (gtk:widget-init-template instance))

(defun window-new (app)
  (let* ((path "/com/crategus/pinus/gears-menu.ui")
         (builder (gtk:builder-new-from-resource path))
         (menu (gtk:builder-object builder "menu"))
         (window (g:object-new "Pinus5Window" :application app))
         (stack (gtk:widget-template-child window "Pinus5Window" "stack"))
         (gears (gtk:widget-template-child window "Pinus5Window" "gears"))
         (settings (g:settings-new "com.crategus.pinus")))
    ;; Store widgets from template
    (setf (window-stack window) stack)
    (setf (window-gears window) gears)
    ;; Set gears menu
    (setf (gtk:menu-button-menu-model gears) menu)
    ;; Store settings
    (setf (window-settings window) settings)
    ;; Set bindings
    (g:settings-bind settings "transition"
                     stack "transition-type"
                     :default)
    ;; Return initialized window
    window))

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
        (gtk:text-buffer-apply-tag buffer tag start end)))))

;;; ----------------------------------------------------------------------------

;; Implementation of the APPLICATION subclass

(gobject:define-gobject-subclass "Pinus5Application" application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(gobject:define-vtable ("Pinus5Application" application)
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
                 :application-id "com.crategus.pinus5"
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
  (declare (ignore action parameter application))
)

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
    (let ((argv (cons "Pinus 5"
                      (if argv argv (uiop:command-line-arguments))))
          (app (application-new)))
      (g:application-run app argv))))

;;; --- End of file pinus5.lisp ------------------------------------------------
