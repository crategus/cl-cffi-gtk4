;;;; Example GdkAppLaunchContext - 2023-4-6

(in-package :gtk4-example)

#|
GdkAppLaunchContext *context;

context = gdk_display_get_app_launch_context (display);

gdk_app_launch_context_set_timestamp (gdk_event_get_time (event));

if (!g_app_info_launch_default_for_uri ("http://www.gtk.org", context, &error))
  g_warning ("Launching failed: %s\n", error->message);

g_object_unref (context);
|#

(defun do-app-launch-context (&optional application)

  (let* ((button (make-instance 'gtk:button
                                :label "Launch Website"))
         (window (make-instance 'gtk:window
                                :child button
                                :application application
                                :title "Application Launch Context"
                                :default-width 300
                                :default-height 100)))

    (g:signal-connect button "clicked"
        (lambda (widget)
          (declare (ignore widget))
          (format t "Button clicked.~%")

          (let* ((display (gdk:display-default))
                 (context (gdk:display-app-launch-context display)))

            (g:signal-connect context "launch-failed"
                (lambda (context id)
                  (format t " in LAUNCH-FAILED~%")
                  (format t "   context : ~a~%" context)
                  (format t "   id : ~a~%" id)))

            (g:signal-connect context "launch-started"
                (lambda (context info data)
                  (format t " in LAUNCH-STARTED~%")
                  (format t "   context : ~a~%" context)
                  (format t "   info : ~a~%" info)
                  (format t "   type : ~a~%" (g:type-from-instance info))
                  (format t "   data : ~a~%" data)))

            (g:signal-connect context "launch"
                (lambda (context info data)
                  (format t " in LAUNCH~%")
                  (format t "   context : ~a~%" context)
                  (format t "   info : ~a~%" info)
                  (format t "   data : ~a~%" data)))

            (gdk:app-launch-context-set-timestamp context 0)

            (unless (g:app-info-launch-default-for-uri "http://www.gtk.org"
                                                       context)
              (format t "Launching failed.~%"))

    )))

    (gtk:widget-show window)))

;;; --- 2023-4-6 ---------------------------------------------------------------
