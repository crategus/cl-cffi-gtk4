;;;; Knockout groups
;;;;
;;;;
;;;; 2026-02-17

(in-package :gtk4-example)

(defun draw-checks (cr x y width height)
  (let ((size 32)
        (grey (gdk:rgba-parse "grey"))
        (lightgrey (gdk:rgba-parse "lightgrey")))
    (cairo:rectangle cr x y width height)
    (gdk:cairo-set-source-rgba cr grey)
    (cairo:fill cr)
    (do ((j 0 (+ j size)))
        ((> j height))
      (do ((i 0 (+ i size)))
          ((> i width))
        (when (= 0 (mod (+ (/ i size) (/ j size)) 2))
          (cairo:rectangle cr i j size size))))
    (gdk:cairo-set-source-rgba cr lightgrey)
    (cairo:fill cr)))

(defun draw-oval (cr xcenter ycenter xradius yradius)
  (cairo:save cr)
  (cairo:translate cr xcenter ycenter)
  (cairo:scale cr 1.0 (/ yradius xradius))
  (cairo:move-to cr xradius 0.0)
  (cairo:arc cr 0 0 xradius 0 (* 2.0 pi))
  (cairo:close-path cr)
  (cairo:restore cr))

(defun draw-circles (cr xcenter ycenter radius alpha)
  (let ((radius1 (* radius (- (/ 2.0 3.0) 0.1))))
    ;; Red circle
    (cairo:set-source-rgba cr 1 0 0 alpha)
    (draw-oval cr
               (+ xcenter (* (/ radius 3.0) (cos (* 0.5 pi))))
               (- ycenter (* (/ radius 3.0) (sin (* 0.5 pi))))
               radius1
               radius1)
    (cairo:fill cr)
    ;; Green circle
    (cairo:set-source-rgba cr 0 1 0 alpha)
    (draw-oval cr
               (+ xcenter (* (/ radius 3.0) (cos (* (+ 0.5 2/3) pi))))
               (- ycenter (* (/ radius 3.0) (sin (* (+ 0.5 2/3) pi))))
               radius1
               radius1)
    (cairo:fill cr)
    ;; Blue circle
    (cairo:set-source-rgba cr 0 0 1 alpha)
    (draw-oval cr
               (+ xcenter (* (/ radius 3.0) (cos (* (+ 0.5 4/3) pi))))
               (- ycenter (* (/ radius 3.0) (sin (* (+ 0.5 4/3) pi))))
               radius1
               radius1)
    (cairo:fill cr)))

(defun draw-groups (area cr width height)
  (declare (ignore area))
  (let ((radius (- (* 0.5 (min width height)) 10))
        (xcenter (/ width 2.0))
        (ycenter (/ height 2.0))
        (overlay (cairo:surface-create-similar (cairo:target cr)
                                               :color-alpha width height))
        (punch (cairo:surface-create-similar (cairo:target cr)
                                             :alpha width height))
        (circles (cairo:surface-create-similar (cairo:target cr)
                                               :color-alpha width height)))
    ;; Draw background
    (draw-checks cr 0 0 width height)
    ;; Create context for overlay surface
    (cairo:with-context (overlay-context overlay)
      ;; Draw a black circle on the overlay
      (cairo:set-source-rgb overlay-context 0.0 0.0 0.0)
      (draw-oval overlay-context xcenter ycenter radius radius)
      (cairo:fill overlay-context)
      ;; Create context for punch surface
      (cairo:with-context (punch-context punch)
        ;; Draw circles
        (draw-circles punch-context xcenter ycenter radius 1.0))
      (setf (cairo:operator overlay-context) :dest-out)
      (cairo:set-source-surface overlay-context punch 0 0)
      (cairo:paint overlay-context)
      ;; Draw the circles again
      (cairo:with-context (circles-context circles)
        (setf (cairo:operator circles-context) :over)
        (draw-circles circles-context xcenter ycenter radius 0.5))
      ;; Paint the overlay surface
      (setf (cairo:operator overlay-context) :add)
      (cairo:set-source-surface overlay-context circles 0 0)
      (cairo:paint overlay-context))
    (cairo:set-source-surface cr overlay 0 0)
    (cairo:paint cr)
    ;; Destroy surfaces
    (cairo:surface-destroy overlay)
    (cairo:surface-destroy punch)
    (cairo:surface-destroy circles)))

(defun do-drawing-area-knockout-groups (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :application application
                                :child area
                                :title "Knockout Groups"
                                :default-width 400
                                :default-height 300)))
    ;; Set a drawing function
    (gtk:drawing-area-set-draw-func area #'draw-groups)
    ;; Present the window
    (gtk:window-present window)))
