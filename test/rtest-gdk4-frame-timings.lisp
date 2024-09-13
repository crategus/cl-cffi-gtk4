(in-package :gtk-test)

(def-suite gdk-frame-timings :in gdk-suite)
(in-suite gdk-frame-timings)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFrameTimings

(test gtk-text-iter-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkTextIter"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkTextIter")
          (g:gtype (cffi:foreign-funcall "gtk_text_iter_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:text-iter
          (glib:symbol-for-gtype "GtkTextIter"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_frame_timings_get_frame_counter
;;;     gdk_frame_timings_get_complete
;;;     gdk_frame_timings_get_frame_time
;;;     gdk_frame_timings_get_presentation_time
;;;     gdk_frame_timings_get_refresh_interval
;;;     gdk_frame_timings_get_predicted_presentation_time

;;; 2024-7-12
