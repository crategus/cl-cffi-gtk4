;;;; Animated Paintable
;;;;
;;;; <tt>GdkPaintable</tt> also allows paintables to change.
;;;;
;;;; This demo code gives an example of how this could work. It builds on the
;;;; previous simple example.
;;;;
;;;; Paintables can also change their size, this works similarly, but we will
;;;; not demonstrate this here as our icon does not have any size.
;;;;
;;;; 2024-4-6

(in-package :gdk)

;; Do a full rotation in 5 seconds. We will register the timeout for doing a
;; single step to be executed every 10ms, which means after 1000 steps 10s will
;; have elapsed.
(defconstant +max-progress+ 500)

(gobject:define-gobject-subclass "GdkNuclearAnimation" nuclear-animation
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"))
  ((drawbackground
    nuclear-animation-drawbackground
    "drawbackground" "gboolean" t t)
   ;; This variable stores the progress of our animation. We just count upwards
   ;; until we hit MAX_PROGRESS and then start from scratch.
   (progress
    nuclear-animation-progress
    "progress" "gdouble" t t)
   ;; This variable holds the ID of the timer that updates our progress
   ;; variable. We need to keep track of it so that we can remove it again.
   (sourceid
    nuclear-animation-sourceid
    "sourceid" "guint" t t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'nuclear-animation))

(defmethod initialize-instance :after ((obj nuclear-animation) &key)
  ;; Add a timer here that constantly updates our animations. We want to update
  ;; it often enough to guarantee a smooth animation. Ideally, we would attach
  ;; to  the frame clock, but because we do not have it available here, we just
  ;; use a regular timeout that hopefully triggers often enough to be smooth.
  (let ((sourceid (g:timeout-add 10
                    (lambda ()
                      (let ((progress (nuclear-animation-progress obj)))
                        (setf (nuclear-animation-progress obj)
                              ;; Add 1 to the progress and reset it when we
                              ;; have reached the maximum value. The animation
                              ;; will rotate by 360 degrees at MAX_PROGRESS so
                              ;; it will be identical to the original unrotated
                              ;; one.
                              (mod (+ 1 progress) +max-progress+))
                        ;; Now we need to tell all listeners that we've changed
                        ;; out contents so that they can redraw this paintable.
                        (paintable-invalidate-contents obj)
                        ;; We want this timeout function to be called
                        ;; repeatedly, so we return this value here. If this
                        ;; was a single-shot timeout, we could also return
                        ;; G_SOURCE_REMOVE here to get rid of it.
                        g:+source-continue+)))))
    (setf (nuclear-animation-sourceid obj) sourceid)
    ;; Remove the timeout we registered when constructing the object.
    (tg:finalize obj (lambda ()
                       (g:source-remove sourceid)))))

;;; ----------------------------------------------------------------------------

;; Again, we implement the functionality required by the GdkPaintable interface

(defmethod paintable-get-flags-impl ((paintable nuclear-animation))
  ;; This time, we cannot set the static contents flag because our animation
  ;; changes the contents. However, our size still doesn't change, so report
  ;; that flag.
  (list :static-size))

(defmethod paintable-get-current-image-impl ((paintable nuclear-animation))
  ;; For non-static paintables, this function needs to be implemented. It must
  ;; return a static paintable with the same contents as this one currently has.
  ;; Luckily we added the rotation property to the nuclear icon object
  ;; previously, so we can just return an instance of that one.
  (make-instance 'nuclear-icon
                 :rotation (/ (* 2 pi (nuclear-animation-progress paintable))
                           +max-progress+)))

(defmethod paintable-snapshot-impl ((paintable nuclear-animation)
                                    snapshot width height)
  ;; We call the function from the previous example here.
  (nuclear-snapshot snapshot
                    (rgba-new :red 0 :green 0 :blue 0 :alpha 1)
                    (if (nuclear-animation-drawbackground paintable)
                        ;; Yellow background
                        (rgba-new :red 0.9 :green 0.75 :blue 0.15 :alpha 1)
                        ;; Transparent background
                        (rgba-new :red 0 :green 0 :blue 0 :alpha 0))
                    width
                    height
                    (/ (* 2 pi (nuclear-animation-progress paintable))
                       +max-progress+)))

;;; ----------------------------------------------------------------------------

(in-package :gtk4-example)

(defun do-paintable-animation (&optional application)
  (let* ((nuclear (make-instance 'gdk:nuclear-animation
                                 :drawbackground t))
         (image (gtk:image-new-from-paintable nuclear))
         (window (make-instance 'gtk:window
                                :application application
                                :child image
                                :title "Nuclear Animation"
                                :default-width 300
                                :default-height 200)))
    (gtk:window-present window)))
