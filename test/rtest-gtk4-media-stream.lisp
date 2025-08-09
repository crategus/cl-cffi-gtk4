(in-package :gtk-test)

(def-suite gtk-media-stream :in gtk-media-support)
(in-suite gtk-media-stream)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaStream

(test gtk-media-stream-class
  ;; Check type
  (is (g:type-is-object "GtkMediaStream"))
  ;; Check registered name
  (is (eq 'gtk:media-stream
          (glib:symbol-for-gtype "GtkMediaStream")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMediaStream")
          (g:gtype (cffi:foreign-funcall "gtk_media_stream_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMediaStream")))
  ;; Check children
  (is (equal '("GtkMediaFile")
             (glib-test:list-children "GtkMediaStream")))
  ;; Check interfaces
  (is (equal '("GdkPaintable")
             (glib-test:list-interfaces "GtkMediaStream")))
  ;; Check class properties
  (is (equal '("duration" "ended" "error" "has-audio" "has-video" "loop" "muted"
               "playing" "prepared" "seekable" "seeking" "timestamp" "volume")
             (glib-test:list-properties "GtkMediaStream")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMediaStream")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMediaStream" GTK:MEDIA-STREAM
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GdkPaintable")
                       :TYPE-INITIALIZER "gtk_media_stream_get_type")
                      ((DURATION MEDIA-STREAM-DURATION "duration" "gint64" T
                        NIL)
                       (ENDED MEDIA-STREAM-ENDED "ended" "gboolean" T NIL)
                       (ERROR MEDIA-STREAM-ERROR "error" "GError" T NIL)
                       (HAS-AUDIO MEDIA-STREAM-HAS-AUDIO "has-audio"
                        "gboolean" T NIL)
                       (HAS-VIDEO MEDIA-STREAM-HAS-VIDEO "has-video"
                        "gboolean" T NIL)
                       (LOOP MEDIA-STREAM-LOOP "loop" "gboolean" T T)
                       (MUTED MEDIA-STREAM-MUTED "muted" "gboolean" T T)
                       (PLAYING MEDIA-STREAM-PLAYING "playing" "gboolean" T T)
                       (PREPARED MEDIA-STREAM-PREPARED "prepared" "gboolean"
                        T NIL)
                       (SEEKABLE MEDIA-STREAM-SEEKABLE "seekable" "gboolean"
                        T NIL)
                       (SEEKING MEDIA-STREAM-SEEKING "seeking" "gboolean" T
                        NIL)
                       (TIMESTAMP MEDIA-STREAM-TIMESTAMP "timestamp" "gint64"
                        T NIL)
                       (VOLUME MEDIA-STREAM-VOLUME "volume" "gdouble" T T)))

             (gobject:get-gtype-definition "GtkMediaStream"))))

;;; --- Properties -------------------------------------------------------------

;;;     duration
;;;     ended
;;;     error
;;;     has-audio
;;;     has-video
;;;     loop
;;;     muted
;;;     playing
;;;     prepared
;;;     seekable
;;;     seeking
;;;     timestamp
;;;     volume

(test gtk-media-stream-properties
  (glib-test:with-check-memory (video mediastream)
    (let ((path (glib-sys:sys-path "test/resource/gtk-logo.webm")))
      (is (typep (setf video (gtk:video-new-for-filename path)) 'gtk:video))
      (is (typep (setf mediastream
                       (gtk:video-media-stream video)) 'gtk:media-stream))
      (is (= 0 (gtk:media-stream-duration mediastream)))
      (is-false (gtk:media-stream-ended mediastream))
      (is-false (gtk:media-stream-error mediastream))
      (is-false (gtk:media-stream-has-audio mediastream))
      (is-false (gtk:media-stream-has-video mediastream))
      (is-false (gtk:media-stream-loop mediastream))
      (is-false (gtk:media-stream-muted mediastream))
      (is-false (gtk:media-stream-playing mediastream))
      (is-false (gtk:media-stream-prepared mediastream))
      (is-false (gtk:media-stream-seekable mediastream))
      (is-false (gtk:media-stream-seeking mediastream))
      (is (= 0 (gtk:media-stream-timestamp mediastream)))
      (is (= 1.0d0 (gtk:media-stream-volume mediastream)))
      ;; Remove references
      (is-false (setf (gtk:video-media-stream video) nil)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_stream_is_prepared
;;;     gtk_media_stream_play
;;;     gtk_media_stream_pause
;;;     gtk_media_stream_is_seekable
;;;     gtk_media_stream_is_seeking
;;;     gtk_media_stream_seek
;;;     gtk_media_stream_realize
;;;     gtk_media_stream_unrealize
;;;     gtk_media_stream_prepared
;;;     gtk_media_stream_unprepared
;;;     gtk_media_stream_update
;;;     gtk_media_stream_ended
;;;     gtk_media_stream_seek_success
;;;     gtk_media_stream_seek_failed
;;;     gtk_media_stream_gerror
;;;     gtk_media_stream_error
;;;     gtk_media_stream_error_valist

;;; 2025-08-05
