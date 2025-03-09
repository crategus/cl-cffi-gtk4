(in-package :gtk-test)

(def-suite gdk-frame-timings :in gdk-suite)
(in-suite gdk-frame-timings)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameTimings

(test gdk-frame-timings-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkFrameTimings"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFrameTimings")
          (g:gtype (cffi:foreign-funcall "gdk_frame_timings_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:frame-timings
          (glib:symbol-for-gtype "GdkFrameTimings"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_timings_get_frame_counter
;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time

(test gdk-frame-timings-properties
  (glib-test:with-check-memory (clock)
    (let ((window (make-instance 'gtk:window)))
      (is-false (gtk:widget-realize window))
      (is-true (gtk:widget-realized window))
      (is (typep (setf clock
                       (gtk:widget-frame-clock window)) 'gdk:frame-clock))
      ;; is NIL, can we create a test with a frame timings not NIL?
      (is-false (gdk:frame-clock-current-timings clock))
      ;; Destroy window
      (is-false (gtk:window-destroy window)))))

;;; 2025-3-1
