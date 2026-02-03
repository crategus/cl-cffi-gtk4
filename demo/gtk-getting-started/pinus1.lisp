;;; pinus1.lisp

(defpackage :pinus1
  (:use :common-lisp)
  (:export #:pinus))

(in-package :pinus1)

;;; ----------------------------------------------------------------------------

;; Implementation of the WINDOW subclass

(gobject:define-gobject-subclass "Pinus1Window" window
  (:superclass gtk:application-window
   :export t
   :interfaces ())
  nil)

(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'window))) cclass data)
)

(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'window))) instance data)
)

(defun window-new (app)
  (let ((window (make-instance 'window
                               :application app)))
    ;; Return initialized window
    window))

(defun window-open (win file)
  (declare (ignore win file))
)

;;; ----------------------------------------------------------------------------

;; Implementation of the APPLICATION subclass

(gobject:define-gobject-subclass "Pinus1Application" application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(gobject:define-vtable ("Pinus1Application" application)
  ;; Parent class
  (:skip parent-instance (:struct gobject:object-class))
  ;; Signals
  (:skip startup :pointer)
  (activate (:void (app (g:object application))))
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
                 :application-id "com.crategus.pinus1"
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
  (let ((argv (cons "Pinus 1"
                    (if argv argv (uiop:command-line-arguments))))
        (app (application-new)))
    (g:application-run app argv)))

;;; --- End of file pinus1.lisp ------------------------------------------------