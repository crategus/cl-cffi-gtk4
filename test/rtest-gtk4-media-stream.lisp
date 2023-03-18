(in-package :gtk-test)

(def-suite gtk-media-stream :in gtk-suite)
(in-suite gtk-media-stream)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaStream

(test gtk-media-stream-class
  ;; Type check
  (is (g:type-is-object "GtkMediaStream"))
  ;; Check the registered name
  (is (eq 'gtk:media-stream
          (gobject:symbol-for-gtype "GtkMediaStream")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMediaStream")
          (g:gtype (cffi:foreign-funcall "gtk_media_stream_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkMediaStream")))
  ;; Check the children
  (is (equal '("GtkMediaFile")
             (list-children "GtkMediaStream")))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (list-interfaces "GtkMediaStream")))
  ;; Check the class properties
  (is (equal '("duration" "ended" "error" "has-audio" "has-video" "loop" "muted"
               "playing" "prepared" "seekable" "seeking" "timestamp" "volume")
             (list-properties "GtkMediaStream")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkMediaStream")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMediaStream" GTK-MEDIA-STREAM
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                        ("GdkPaintable") :TYPE-INITIALIZER
                        "gtk_media_stream_get_type")
                       ((DURATION GTK-MEDIA-STREAM-DURATION "duration" "gint64"
                         T NIL)
                        (ENDED GTK-MEDIA-STREAM-ENDED "ended" "gboolean" T NIL)
                        (ERROR GTK-MEDIA-STREAM-ERROR "error" "GError" T NIL)
                        (HAS-AUDIO GTK-MEDIA-STREAM-HAS-AUDIO "has-audio"
                         "gboolean" T NIL)
                        (HAS-VIDEO GTK-MEDIA-STREAM-HAS-VIDEO "has-video"
                         "gboolean" T NIL)
                        (LOOP GTK-MEDIA-STREAM-LOOP "loop" "gboolean" T T)
                        (MUTED GTK-MEDIA-STREAM-MUTED "muted" "gboolean" T T)
                        (PLAYING GTK-MEDIA-STREAM-PLAYING "playing" "gboolean"
                         T T)
                        (PREPARED GTK-MEDIA-STREAM-PREPARED "prepared"
                         "gboolean" T T)
                        (SEEKABLE GTK-MEDIA-STREAM-SEEKABLE "seekable"
                         "gboolean" T NIL)
                        (SEEKING GTK-MEDIA-STREAM-SEEKING "seeking" "gboolean"
                         T NIL)
                        (TIMESTAMP GTK-MEDIA-STREAM-TIMESTAMP "timestamp"
                         "gint64" T NIL)
                        (VOLUME GTK-MEDIA-STREAM-VOLUME "volume" "gdouble" T
                         T)))
             (get-g-type-definition "GtkMediaStream"))))

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

;;; --- 2023-3-18 --------------------------------------------------------------
