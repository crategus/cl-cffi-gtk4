;;;; GdkMonitor
;;;;
;;;; The <b>gdk:monitor</b> objects represent the individual outputs that are
;;;; associated with a <b>gdk:display</b> object. The <b>gdk:display</b> object
;;;; keeps a <b>g:list-model</b> instance to enumerate and monitor monitors with
;;;; the <b>gdk:display-monitors</b> function. You can use the
;;;; <b>gdk:display-monitor-at-surface</b> function to find a particular
;;;; monitor.
;;;;
;;;; 2026-04-13

(in-package :gtk4-example)

(defun do-monitor (&optional application)
  (let* ((display (gdk:display-default))
         (monitors (gdk:display-monitors display))
         (stack (make-instance 'gtk:stack))
         (switcher (make-instance 'gtk:stack-switcher
                                  :stack stack))
         (actionbar (make-instance 'gtk:action-bar))
         (group (make-instance 'gtk:size-group
                               :mode :horizontal))
         (box (make-instance 'gtk:box
                             :orientation :vertical))
         (window (make-instance 'gtk:window
                                :title "Monitors"
                                :child box)))
    (setf (gtk:window-application window) application)
    (dolist (monitor monitors)
      (let ((connector (gdk:monitor-connector monitor))
            (page (make-instance 'gtk:grid
                                 :margin-top 12
                                 :margin-bottom 12
                                 :margin-start 12
                                 :margin-end 12
                                 :row-spacing 6
                                 :column-spacing 6
                                 :hexpand t)))
        ;; Handler for the "invalidate" signal
        (g:signal-connect monitor "invalidate"
                (lambda (monitor)
                  (let ((connector (gdk:monitor-connector monitor)))
                    (format t "~&Monitor ~a invalidate.~%" connector))))

        ;; Description
        (let* ((label (gdk:monitor-description monitor))
               (entry (make-instance 'gtk:entry
                                     :buffer (gtk:entry-buffer-new label)
                                     :width-request 190
                                     :editable nil
                                     :halign :end)))
          (gtk:grid-attach page (gtk:label-new "Description") 0 1)
          (gtk:grid-attach page entry 1 1 2)
          (gtk:size-group-add-widget group entry))
        ;; Manufacturer
        (let* ((label (gdk:monitor-manufacturer monitor))
               (entry (make-instance 'gtk:entry
                                     :buffer (gtk:entry-buffer-new label)
                                     :editable nil
                                     :halign :end)))
          (gtk:grid-attach page (gtk:label-new "Manufacturer") 0 2)
          (gtk:grid-attach page entry 1 2 2)
          (gtk:size-group-add-widget group entry))
        ;; Model
        (let* ((label (gdk:monitor-model monitor))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Model") 0 3)
          (gtk:grid-attach page entry 1 3 2)
          (gtk:size-group-add-widget group entry))
        ;; Refresh rate
        (let* ((label (format nil "~a mHz" (gdk:monitor-refresh-rate monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Refresh rate") 0 4)
          (gtk:grid-attach page entry 1 4 2)
          (gtk:size-group-add-widget group entry))
        ;; Geometry
        (let* ((geometry (gdk:monitor-geometry monitor))
               (width (gdk:rectangle-width geometry))
               (height (gdk:rectangle-height geometry))
               (label (format nil "~a x ~a pixel" width height))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Geometry") 0 5)
          (gtk:grid-attach page entry 1 5 2)
          (gtk:size-group-add-widget group entry))
        ;; Height mm
        (let* ((label (format nil "~a mm" (gdk:monitor-height-mm monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Height") 0 6)
          (gtk:grid-attach page entry 1 6 2)
          (gtk:size-group-add-widget group entry))
        ;; Width mm
        (let* ((label (format nil "~a mm" (gdk:monitor-width-mm monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Width:") 0 7)
          (gtk:grid-attach page entry 1 7 2)
          (gtk:size-group-add-widget group entry))
        ;; Scale
        (let* ((label (format nil "~a" (gdk:monitor-scale monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Scale") 0 8)
          (gtk:grid-attach page entry 1 8 2)
          (gtk:size-group-add-widget group entry))
        ;; Scale Factor
        (let* ((label (format nil "~a" (gdk:monitor-scale-factor monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Scale Factor") 0 9)
          (gtk:grid-attach page entry 1 9 2)
          (gtk:size-group-add-widget group entry))
        ;; Subpixel Layout
        (let* ((label (format nil "~a" (gdk:monitor-subpixel-layout monitor)))
               (entry (make-instance 'gtk:entry
                                     :editable nil
                                     :buffer (gtk:entry-buffer-new label))))
          (gtk:grid-attach page (gtk:label-new "Subpixel Layout") 0 10)
          (gtk:grid-attach page entry 1 10 2)
          (gtk:size-group-add-widget group entry))
        (gtk:stack-add-titled stack page connector connector)))
    (setf (gtk:stack-visible-child-name stack)
          (gdk:monitor-connector (first monitors)))
    ;; Button On monitor
    (let ((button (gtk:button-new-with-label "On monitor")))
      (g:signal-connect button "clicked"
          (lambda (button)
            (declare (ignore button))
            (let* ((surface (gtk:native-surface window))
                   (monitor (gdk:display-monitor-at-surface display surface)))
              (when monitor
                (setf (gtk:stack-visible-child-name stack)
                      (gdk:monitor-connector monitor))))))
      (gtk:action-bar-pack-start actionbar button))
    ;; Button Fullscreen On Monitor
    (let ((button (gtk:toggle-button-new-with-label "Fullscreen on monitor")))
      (g:signal-connect button "toggled"
          (lambda (button)
            (if (gtk:toggle-button-active button)
                (let ((connector (gtk:stack-visible-child-name stack)))
                  (dolist (monitor monitors)
                    (when (and (string= connector
                                        (gdk:monitor-connector monitor))
                               (gdk:monitor-is-valid monitor))
                      (setf (gtk:button-label button) "Unfullscreen")
                      (gtk:window-fullscreen-on-monitor window monitor))))
                (progn
                  (setf (gtk:button-label button) "Fullscreen on monitor")
                  (gtk:window-unfullscreen window)))))
      (setf (gtk:toggle-button-active button) nil)
      (gtk:action-bar-pack-start actionbar button))
    ;; Show window
    (gtk:box-append box switcher)
    (gtk:box-append box stack)
    (gtk:box-append box actionbar)
    (gtk:window-present window)))
