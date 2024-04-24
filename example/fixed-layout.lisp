;;;; Fixed Layout
;;;;
;;;; The <tt>GtkFixed</tt> widget is a container that allows placing and
;;;; transforming widgets manually.
;;;;
;;;; 2024-4-19

(in-package :gtk4-example)

(defparameter *css-fixed-layout*
"frame.front {
  border: 2px solid white;
  background-color: rgba(228, 0, 0, 0.9);
}

frame.back {
  border: 2px solid white;
  background-color: rgba(228, 0, 0, 0.9);
}

frame.right {
  border: 2px solid white;
  background-color: rgba(127, 231, 25, 0.9);
}

frame.left {
  border: 2px solid white;
  background-color: rgba(127, 231, 25, 0.9);
}

frame.top {
  border: 2px solid white;
  background-color: rgba(114, 159, 207, 0.9);
}

frame.bottom {
  border: 2px solid white;
  background-color: rgba(114, 159, 207, 0.9);
}")

(defun create-cube ()
  (let* ((fixed (make-instance 'gtk:fixed
                               :overflow :visible))
         (faces '(

                  (RIGHT "right")
                  (TOP "top")
                  (FRONT "front")

                  (BACK "back")
                  (LEFT "left")
                  (BOTTOM "bottom")

                  ))
         (size 200)
         (w (/ size 2.0)) (h (/ size 2.0)) (d (/ size 2.0)) (p (* size 3.0)))
    (dolist (face faces)
      (let ((transform (gsk:transform-new))
            (frame (make-instance 'gtk:frame
                                  :width-request size
                                  :height-request size)))
        (gtk:widget-add-css-class frame (second face))
        (gtk:fixed-put fixed frame 0 0)
        (graphene:with-objects ((point graphene:point-t w h)
                                (point3d graphene:point3d-t))
          (setf transform (gsk:transform-translate transform point))
          (setf transform (gsk:transform-perspective transform p))
          (setf transform
                (gsk:transform-rotate-3d transform
                                         -30.0
                                         (graphene:vec3-x-axis)))
          (setf transform
                (gsk:transform-rotate-3d transform
                                         135.0
                                         (graphene:vec3-y-axis)))
          (setf transform
                (gsk:transform-translate-3d transform
                                            (graphene:point3d-init point3d
                                                                   0 0
                                                                   (/ size
                                                                      -6.0))))
          (cond ((eq 'front (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                0.0
                                                (graphene:vec3-y-axis))))
                ((eq 'back (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                -180.0
                                                (graphene:vec3-y-axis))))
                ((eq 'right (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                90.0
                                                (graphene:vec3-y-axis))))
                ((eq 'left (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                -90.0
                                                (graphene:vec3-y-axis))))
                ((eq 'bottom (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                90.0
                                                (graphene:vec3-x-axis))))
                ((eq 'top (first face))
                 (setf transform
                       (gsk:transform-rotate-3d transform
                                                -90.0
                                                (graphene:vec3-x-axis)))))
          (setf transform
                (gsk:transform-translate-3d transform
                                            (graphene:point3d-init point3d
                                                                   0 0 d)))
          (setf transform
                (gsk:transform-translate-3d transform
                                            (graphene:point3d-init point3d
                                                                   (- w)
                                                                   (- h)
                                                                   0)))
          (setf (gtk:fixed-child-transform fixed frame) transform))))
    fixed))

(defun do-fixed-layout (&optional application)
  (let* ((cube (create-cube))
         (fixed (make-instance 'gtk:fixed
                               :halign :center
                               :valign :center
                               :overflow :visible))
         (scrolled (make-instance 'gtk:scrolled-window
                                  :child fixed))
         (window (make-instance 'gtk:window
                                :child scrolled
                                :title "Fixed Layout"
                                :application application
                                :default-width 480
                                :default-height 480))
         (provider (gtk:css-provider-new)))
    ;; Load and apply CSS
    (gtk:css-provider-load-from-string provider *css-fixed-layout*)
    (gtk:widget-add-provider cube provider)
    ;; Put cube in the fixed and present window
    (gtk:fixed-put fixed cube 80 40)
    (gtk:window-present window)))
