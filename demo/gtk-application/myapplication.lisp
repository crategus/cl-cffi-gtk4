;;;; My Application - Getting started
;;;;
;;;; 2025-3-7

(in-package :gtk4-application)

;;; ----------------------------------------------------------------------------

(gobject:define-gobject-subclass "MyApplication" my-application
  (:superclass gtk:application
   :export t
   :interfaces ())
  nil)

(defmethod g:object-class-init :after
           ((subclass (eql (find-class 'my-application))) class data)
  (format t "in G:OBJECT-CLASS-INIT for ~a~%" class))

(defmethod g:object-instance-init :after
           ((class (eql (find-class 'my-application))) instance data)
  (format t "in G:OBJECT-INSTANCE-INIT for ~a~%" class))

(defmethod initialize-instance :after
           ((obj my-application) &key)
  (format t "in INITIALIZE-INSTANCE for ~a~%" obj))

;;; ----------------------------------------------------------------------------

(gobject:define-gobject-subclass "MyAppWindow" my-app-window
  (:superclass gtk:application-window
   :export t
   :interfaces nil)
  ((stack
    my-app-window-stack
    "stack" "GtkWidget" t t)
   (gears
    my-app-window-gears
    "gears" "GtkWidget" t t)
   (search
    my-app-window-search
    "search" "GtkWidget" t t)
   (searchbar
    my-app-window-searchbar
    "searchbar" "GtkWidget" t t)
   (sidebar
    my-app-window-sidebar
    "sidebar" "GtkWidget" t t)
   (words
    my-app-window-words
    "words" "GtkWidget" t t)
   (lines
    my-app-window-lines
    "lines" "GtkWidget" t t)
   (lines-label
    my-app-window-lines-label
    "lines-label" "GtkWidget" t t)
   (:cl
    template-p :initform nil
               :accessor my-app-window-template-p
               :allocation :class)))

(defmethod g:object-class-init :after
           ((subclass (eql (find-class 'my-app-window))) class data)
  (format t "in G:OBJECT-CLASS-INIT for ~a~%" class)

  ;; Load template from resource for application window
  (let ((path "/com/crategus/myapplication/my-app-window.ui"))
    (gtk:widget-class-set-template-from-resource "MyAppWindow" path)

    (gtk:widget-class-bind-template-child "MyAppWindow" "header")
    (gtk:widget-class-bind-template-child "MyAppWindow" "content_box")
    (gtk:widget-class-bind-template-child "MyAppWindow" "stack")
    (gtk:widget-class-bind-template-child "MyAppWindow" "gears")
  )
)

(defmethod g:object-instance-init :after
           ((class (eql (find-class 'my-app-window))) instance data)
  (format t "in G:OBJECT-INSTANCE-INIT for ~a~%" class))

(defmethod initialize-instance :after
           ((obj my-app-window) &key)
  (format t "in INITIAlIZE-INSTANCE for ~a~%" obj)

  (gtk:widget-init-template obj)

  (let* ((path "/com/crategus/myapplication/my-app-window-gears-menu.ui")
         (builder (gtk:builder-new-from-resource path))
         (menu (gtk:builder-object builder "menu")))

    (setf (my-app-window-gears obj)
          (gtk:widget-template-child obj "MyAppWindow" "gears"))
    (setf (gtk:menu-button-menu-model (my-app-window-gears obj)) menu)

))

;;; ----------------------------------------------------------------------------

(defun my-application (&rest argv)
  (g:with-resource (resource (glib-sys:check-and-create-resources
                                     "gtk4-application.xml"
                                     :package "gtk4-application"
                                     :sourcedir "resource/"))

    (let ((argv (cons "my-application" (or argv (uiop:command-line-arguments))))
          ;; Create the application
          (app (make-instance 'my-application
                              :application-id "com.crategus.my-application"
                              :flags :handles-open)))

      (g:application-register app)

      ;; Connect signal "startup" to the application
      (g:signal-connect app "startup"
          (lambda (application)
            (format t "in STARTUP for ~a~%" application)))

      ;; Connect signal "open" to the application
      (g:signal-connect app "open"
          (lambda (application files n-files hint)
            (declare (ignore hint))
            (format t "in OPEN for ~a~%" application)
            (dotimes (i n-files)
              (let* ((file (cffi:mem-aref files '(g:object g:file) i))
                     (filename (g:file-get-parse-name file)))
                (format t "   file : ~a~%" filename)))))

      ;; Connect signal "activate" to the application
      (g:signal-connect app "activate"
          (lambda (application)
            (let* ((window (make-instance 'my-app-window
                                          :application application
                                          :title "Application Window"
                                          :show-menubar t))
                   (box (gtk:widget-template-child window "MyAppWindow" "content_box"))
                   (header (gtk:widget-template-child window "MyAppWindow" "header"))
                   (stack (gtk:widget-template-child window "MyAppWindow" "stack"))
                   (gears (gtk:widget-template-child window "MyAppWindow" "gears"))
                  )

              (format t "in ACTIVATE for ~a~%" application)

              (format t "    box : ~a~%" box)
              (format t " header : ~a~%" header)
              (format t "  stack : ~a~%" stack)
              (format t "  gears : ~a~%" gears)

              ;; Present the application window
              (gtk:window-present window))))

      ;; Run the application
      (g:application-run app argv))))
