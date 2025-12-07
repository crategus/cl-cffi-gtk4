;;;; Widget from template
;;;;
;;;; Last version: 2025-12-07

(in-package :gtk4-example)

;; Subclass from GtkApplicationWindow
(gobject:define-gobject-subclass "MyAppWindow" my-app-window
  (:superclass gtk:application-window
   :export t
   :interfaces nil)
  nil)

;; Set template and bindings during class initialization
(defmethod gobject:object-class-init :after
           ((subclass (eql (find-class 'my-app-window))) class data)
  (declare (ignorable subclass class data))
  ;; Load template from file, this is a Lisp extension
  (let ((path (glib-sys:sys-path "resource/widget-template.ui")))
    (gtk:widget-class-set-template-from-file "MyAppWindow" path))
  ;; Set builder scope
  (gtk:widget-class-set-template-scope "MyAppWindow"
                                       (make-instance 'gtk:builder-cl-scope))
  ;; Bind objects from the template
  (gtk:widget-class-bind-template-child "MyAppWindow" "button1")
  (gtk:widget-class-bind-template-child "MyAppWindow" "button2")
  (gtk:widget-class-bind-template-child "MyAppWindow" "button3")
  (gtk:widget-class-bind-template-child "MyAppWindow" "button4"))

;; Init usage of template in instance initialization
(defmethod gobject:object-instance-init :after
           ((subclass (eql (find-class 'my-app-window))) instance data)
  (declare (ignorable subclass data))
  (gtk:widget-init-template instance))

;; Signal handlers for the buttons in the template
(defun button-clicked (button)
  (format t "  Button is clicked: ~a~%" button))
(defun button1-clicked (button)
  (format t "  Button 1 is clicked: ~a~%" button))
(defun button2-clicked (button)
  (format t "  Button 2 is clicked: ~a~%" button))

(defun do-widget-template (&optional application)
  (let* ((window (make-instance 'my-app-window
                                :application application
                                :title "Application Window"
                                :show-menubar t))
         ;; Get the buttons from the template
         (button1 (gtk:widget-template-child window "MyAppWindow" "button1"))
         (button2 (gtk:widget-template-child window "MyAppWindow" "button2"))
         (button3 (gtk:widget-template-child window "MyAppWindow" "button3"))
         (button4 (gtk:widget-template-child window "MyAppWindow" "button4")))
    ;; Emit signal clicked on the buttons
    (g:signal-emit button1 "clicked")
    (g:signal-emit button2 "clicked")
    (g:signal-emit button3 "clicked")
    (g:signal-emit button4 "clicked")
    ;; Present the application window
    (gtk:window-present window)))
