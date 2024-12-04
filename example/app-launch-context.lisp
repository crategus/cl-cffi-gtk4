;;;; GdkAppLaunchContext
;;;;
;;;; The <tt>gdk:app-launch-context</tt> object is an implementation of the
;;;; <tt>g:app-launch-context</tt> object that handles launching an application
;;;; in a graphical context. It provides startup notification and allows to
;;;; launch applications on a specific screen or workspace.
;;;;
;;;; 2024-11-22

(in-package :gtk4-example)

(defun do-app-launch-context (&optional application)
  (let* ((button (make-instance 'gtk:button
                                :label "Launch Website http://www.gtk.org"))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Application Launch Context"
                                :default-width 300
                                :default-height 100)))
    (g:signal-connect button "clicked"
        (lambda (widget)
          (let* ((display (gtk:widget-display widget))
                 (context (gdk:display-app-launch-context display)))
            (g:signal-connect context "launch-failed"
                (lambda (context id)
                  (format t " in LAUNCH-FAILED signal handler~%")
                  (format t "   context : ~a~%" context)
                  (format t "   id : ~a~%" id)))
            (g:signal-connect context "launch-started"
                (lambda (context info data)
                  (format t " in LAUNCH-STARTED signal handler~%")
                  (format t "   context : ~a~%" context)
                  (format t "   info : ~a~%" info)
                  (format t "   type : ~a~%" (g:type-from-instance info))
                  (format t "   data : ~a~%" data)))
            (g:signal-connect context "launched"
                (lambda (context info data)
                  (format t " in LAUNCHED signal handler~%")
                  (format t "   context : ~a~%" context)
                  (format t "   info : ~a~%" info)
                  (format t "   data : ~a~%" data)))
            (gdk:app-launch-context-set-timestamp context 0)
            (if (g:app-info-launch-default-for-uri "http://www.gtk.org"
                                                       context)
                (gtk:window-destroy window)
                (format t "Launching failed.~%")))))
    (gtk:window-present window)))


(defun do-app-launch-context-async (&optional application)
  (let* ((button (make-instance 'gtk:button
                                :label "Launch Website http://www.gtk.org"))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Application Launch Context Async"
                                :default-width 300
                                :default-height 100)))
    (g:signal-connect button "clicked"
        (lambda (widget)
          (let* ((display (gtk:widget-display widget))
                 (context (gdk:display-app-launch-context display)))
            (g:app-info-launch-default-for-uri-async
                    "http://www.gtk.org"
                    context
                    nil
                    (lambda (widget result)
                      (declare (ignore widget))
                      (if (g:app-info-launch-default-for-uri-finish result)
                          (gtk:window-destroy window)
                          (format t "Launching failed.~%")))))))
    (gtk:window-present window)))
