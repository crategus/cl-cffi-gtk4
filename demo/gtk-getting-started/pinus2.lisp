;;; pinus2.lisp

(defpackage :pinus2
  (:use :common-lisp)
  (:export #:pinus))

(in-package :pinus2)

;;; ----------------------------------------------------------------------------

;; Implementation of the WINDOW subclass

(gobject:define-gobject-subclass "Pinus2Window" window
  (:superclass gtk:application-window
   :export t
   :interfaces ())
  ((stack
    window-stack
    "stack" "GObject" t t)))

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'window))) cclass data)
  (let ((path "/com/crategus/pinus/window2.ui"))
    (gtk:widget-class-set-template-from-resource "Pinus2Window" path)
    (gtk:widget-class-bind-template-child "Pinus2Window" "stack")))

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'window))) instance data)
  (gtk:widget-init-template instance))

(defun window-new (app)
  (let* ((window (make-instance 'window
                                :application app))
         (stack (gtk:widget-template-child window "Pinus2Window" "stack")))
    ;; Store stack widget from template
    (setf (window-stack window) stack)
    ;; Return initialized window
    window))

(defun window-open (win filename)
  (declare (ignore win filename))
)

;;; ----------------------------------------------------------------------------

;; Implementation of the APPLICATION subclass

(gobject:define-gobject-subclass "Pinus2Application" application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(gobject:define-vtable ("Pinus2Application" application)
  ;; Parent class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Signals
  (:skip startup :pointer)
  (activate (:void (application (g:object application))))
  (open (:void (application (g:object application))
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
                 :application-id "com.crategus.pinus2"
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

;;; ----------------------------------------------------------------------------

(defun pinus (&rest argv)
  (gio:with-resources ((resource (glib-sys:check-and-create-resources
                                         "pinus.gresource.xml"
                                         :sourcedir "resource/"
                                         :package "gtk4-getting-started"
                                         :verbose t)))
    (let ((argv (cons "Pinus 2"
                      (if argv argv (uiop:command-line-arguments))))
          (app (application-new)))
      (g:application-run app argv))))

;;; --- End of file pinus2.lisp ------------------------------------------------
