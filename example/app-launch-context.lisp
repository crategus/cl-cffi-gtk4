;;;; GdkAppLaunchContext
;;;;
;;;; The <tt>gdk:app-launch-context</tt> object is an implementation of the
;;;; <tt>g:app-launch-context</tt> object that handles launching an application
;;;; in a graphical context. It provides startup notification and allows to
;;;; launch applications on a specific screen or workspace.
;;;;
;;;; 2025-1-4

(in-package :gtk4-example)

;; Synchronous version
(defun do-app-launch-context (&optional application)
  (let* ((uri "http://www.gtk.org")
         (label (format nil "Launch Website ~a" uri))
         (button (make-instance 'gtk:button
                                :label label))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Application Launch Context"
                                :default-width 400
                                :default-height 100)))
    (g:signal-connect button "clicked"
        (lambda (widget)
          (let* ((display (gtk:widget-display widget))
                 (context (gdk:display-app-launch-context display)))
            (g:signal-connect context "launched"
                (lambda (context info data)
                  (format t " in LAUNCHED signal handler~%")
                  (format t "   context : ~a~%" context)
                  (format t "      info : ~a~%" (g:app-info-name info))
                  (format t "      type : ~a~%" (g:type-from-instance info))
                  (format t "      data : ~a~%" (g:variant-print data))
                  (format t "     vtype : ~a~%" (g:variant-type-string data))))
            (gdk:app-launch-context-set-timestamp context gdk:+current-time+)
            (if (g:app-info-launch-default-for-uri uri context)
                (gtk:window-destroy window)
                (warn "Launching failed")))))
    (gtk:window-present window)))

;; Asynchronous version
(defun do-app-launch-context-async (&optional application)
  (let* ((uri "http://www.gtk.org")
         (label (format nil "Launch Website ~a" uri))
         (button (make-instance 'gtk:button
                                :label label))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Application Launch Context Async"
                                :default-width 400
                                :default-height 100)))
    (g:signal-connect button "clicked"
        (lambda (widget)
          (let* ((display (gtk:widget-display widget))
                 (context (gdk:display-app-launch-context display)))
            (g:app-info-launch-default-for-uri-async
                    uri
                    context
                    nil
                    (lambda (widget result)
                      (declare (ignore widget))
                      (if (g:app-info-launch-default-for-uri-finish result)
                          (gtk:window-destroy window)
                          (warn "Launching failed")))))))
    (gtk:window-present window)))
