;;;; Statusbar
;;;;
;;;; The <tt>gtk:statusbar</tt> widget is usually placed along the bottom of the
;;;; main <tt>gtk:window</tt> widget of the application. It may provide a
;;;; regular commentary of the status of the application as is usually the case
;;;; in a web browser, for example, or may be used to simply output a message
;;;; when the status changes, when an upload is complete in an FTP client, for
;;;; example.
;;;;
;;;; Statusbars in GTK maintain a stack of messages. The message at the top of
;;;; the stack of the statusbar is the one that will currently be displayed.
;;;;
;;;; 2024-4-5

(in-package :gtk4-example)

(defun do-statusbar (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :homogeneous nil
                              :spacing 3))
         (window (make-instance 'gtk:window
                                :title "Example Statusbar"
                                :child vbox
                                :application application
                                :default-width 300))
         (statusbar (make-instance 'gtk:statusbar))
         (id (gtk:statusbar-context-id statusbar "Example Statusbar"))
         (count 0))
    (gtk:box-append vbox statusbar)
    (let ((button (gtk:button-new-with-label "Push Item")))
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (setq count (+ 1 count))
           (gtk:statusbar-push statusbar id (format nil "Item ~A" count))))
      (gtk:box-append vbox button))
    (let ((button (gtk:button-new-with-label "Pop Item")))
      (g:signal-connect button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (gtk:statusbar-pop statusbar id)))
      (gtk:box-append vbox button))
    (gtk:window-present window)))
