;;;; Example Entry - 2022-11-17

(in-package :gtk4-example)

(defun do-entry (&optional application)
  (let* ((handlerid nil)
         (vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child vbox
                                :title "Entry Widget"
                                :default-width 250
                                :default-height 120
                                :resizable nil))
         (hbox (make-instance 'gtk:box
                              :orientation :horizontal
                              :spacing 12))
         (buffer (make-instance 'gtk:entry-buffer
                                :text "Hello"))
         (entry (make-instance 'gtk:entry
                               :buffer buffer
                               :max-length 50))
         (pos (gtk:entry-text-length entry)))
   ;; Handler for the "insert-text" signal
   (setf handlerid
         (g:signal-connect entry "insert-text"
             (lambda (editable text length position)
               (declare (ignore length))
               (g:signal-handler-block editable handlerid)
               (gtk:editable-insert-text editable
                                         (string-upcase text)
                                         (mem-ref position :intptr))
               (g:signal-stop-emission-by-name editable "insert-text")
               (g:signal-handler-unblock editable handlerid))))
    ;; Insert more text, this emits the "insert-text" signal
    (gtk:editable-insert-text entry " world" pos)
    ;; Add check button to toggle Editable
    (let ((check (gtk:check-button-new-with-label "Editable")))
      (g:signal-connect check "toggled"
         (lambda (widget)
           (declare (ignore widget))
           (setf (gtk:editable-editable entry)
                 (gtk:check-button-active check))))
      (gtk:box-append hbox check))
    ;; Add check button to toggle Visible
    (let ((check (gtk:check-button-new-with-label "Visible")))
      (setf (gtk:check-button-active check) t)
      (g:signal-connect check "toggled"
         (lambda (widget)
           (declare (ignore widget))
           (setf (gtk:entry-visibility entry)
                 (gtk:check-button-active check))))
      (gtk:box-append hbox check))
    ;; Pack and show the widgets
    (gtk:box-append vbox entry)
    (gtk:box-append vbox hbox)
    (gtk:widget-show window)))
