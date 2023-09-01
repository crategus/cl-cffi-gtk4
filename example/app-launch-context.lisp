;;;; Example GdkAppLaunchContext - 2023-8-31

(in-package :gtk4-example)

;; TODO: This example launches a website with the 
;; G:APP-INFO-LAUNCH-DEFAULT-for-URI function. The example generates several
;; warnings:
;;
;; (sbcl:20005): GLib-GObject-CRITICAL **: ../../../gobject/gsignal.c:2523: 
;; signal 'launch' is invalid for instance '0x556060512380' of type 
;; 'GdkWaylandAppLaunchContext'
;;
;; Gtk-Message: 01:07:14.667: Not loading module "atk-bridge": The functionality
;; is provided by GTK natively. Please try to not load it.
;;
;; (chrome:22148): Gtk-WARNING **: GTK+ module 
;; /snap/chromium/2599/gnome-platform/usr/lib/gtk-2.0/modules/libcanberra-gtk-
;; module.so cannot be loaded.
;; GTK+ 2.x symbols detected. Using GTK+ 2.x and GTK+ 3 in the same process is 
;; not supported.
;;;
;; Gtk-Message: 01:07:14.780: Failed to load module "canberra-gtk-module"
;;
;; Wird in einer aktuellen Browsersitzung geöffnet.

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
              (format t "Launching failed.~%")))))
    (setf (gtk:widget-visible window) t)))
