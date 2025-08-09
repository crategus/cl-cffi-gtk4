;;;; Transformations
;;;;
;;;; GtkFixed is a container that allows placing and transforming widgets
;;;; manually. This demo shows how to rotate and scale a child widget using a
;;;; transform.
;;;;
;;;; 2025-08-05

(in-package :gtk4-example)

(defun do-fixed-layout-transformations (&optional application)
  (let* ((start (get-internal-run-time))
         (child (make-instance 'gtk:label
                               :label "Crategus"))
         (fixed (make-instance 'gtk:fixed
                               :overflow :visible))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child fixed))
         (window (make-instance 'gtk:window
                                :title "Fixed Layout - Transformations"
                                :application application
                                :child scrolled
                                :default-width 400
                                :default-height 300)))
    (gtk:fixed-put fixed child 0 0)
    (gtk:widget-add-tick-callback fixed
        (lambda (widget clock)
          (declare (ignore clock))
          (let* ((now (get-internal-run-time))
                 (duration (/ (- now start) internal-time-units-per-second))
                 (width (gtk:widget-width widget))
                 (height (gtk:widget-height widget))
                 (child-width (gtk:widget-width child))
                 (child-height (gtk:widget-height child))
                 (angle (mod (* 90 duration) 360))
                 (scale (+ 2.0 (sin (* angle (/ pi 180)))))
                 transform)
            (graphene:with-point (p)
              (setf transform
                    (gsk:transform-translate
                      (gsk:transform-scale
                        (gsk:transform-rotate
                          (gsk:transform-translate
                            (gsk:transform-new)
                            (graphene:point-init p (/ width 2) (/ height 2)))
                          angle)
                        scale scale)
                      (graphene:point-init p (- (/ child-width 2))
                                             (- (/ child-height 2))))))
            (setf (gtk:fixed-child-transform fixed child) transform)
            glib:+source-continue+)))
    (gtk:window-present window)))
