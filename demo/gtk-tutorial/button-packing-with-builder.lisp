;;;; Example Button packing with GtkBuilder
;;;;
;;;; Last version: 2026-01-24

(in-package :gtk4-tutorial)

(defparameter *ui*
"<?xml version='1.0' encoding='UTF-8'?>
<interface>
  <object id='window' class='GtkWindow'>
    <property name='title'>Button Packing</property>
    <child>
      <object id='grid' class='GtkGrid'>
        <child>
          <object id='button1' class='GtkButton'>
            <property name='label'>Button 1</property>
            <layout>
              <property name='column'>0</property>
              <property name='row'>0</property>
            </layout>
          </object>
        </child>
        <child>
          <object id='button2' class='GtkButton'>
            <property name='label'>Button 2</property>
            <layout>
              <property name='column'>1</property>
              <property name='row'>0</property>
            </layout>
          </object>
        </child>
        <child>
          <object id='quit' class='GtkButton'>
            <property name='label'>Quit</property>
            <layout>
              <property name='column'>0</property>
              <property name='row'>1</property>
              <property name='column-span'>2</property>
            </layout>
          </object>
        </child>
      </object>
    </child>
  </object>
</interface>")

(defun button-packing-with-builder ()
  (let ((app (make-instance 'gtk:application
                            :application-id "com.crategus.button-packing"
                            :flags :none)))
    (g:signal-connect app "activate"
            (lambda (application)
              (let* ((builder (gtk:builder-new-from-string *ui*))
                     (window (gtk:builder-object builder "window"))
                     (button1 (gtk:builder-object builder "button1"))
                     (button2 (gtk:builder-object builder "button2"))
                     (button3 (gtk:builder-object builder "quit")))
                (setf (gtk:window-application window) application)
                (g:signal-connect button1 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 1 clicked.~%")))
                (g:signal-connect button2 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (format t "Button 2 clicked.~%")))
                (g:signal-connect button3 "clicked"
                        (lambda (widget)
                          (declare (ignore widget))
                          (gtk:window-destroy window)))
                (gtk:window-present window))))
    (g:application-run app nil)))
