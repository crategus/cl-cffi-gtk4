(in-package :gtk-test)

(def-suite gtk-video :in gtk-suite)
(in-suite gtk-video)

;;; Types and Values

;;;     GtkVideo

(test gtk-video-class
  ;; Check type
  (is (g:type-is-object "GtkVideo"))
  ;; Check registered name
  (is (eq 'gtk:video
          (glib:symbol-for-gtype "GtkVideo")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkVideo")
          (g:gtype (cffi:foreign-funcall "gtk_video_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget") (g:type-parent "GtkVideo")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkVideo")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkVideo")))
  ;; Check class properties
  (is (equal '("autoplay" "file" "graphics-offload" "loop" "media-stream")
             (glib-test:list-properties "GtkVideo")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkVideo")))
  ;; Check CSS name
  (is (string= "video"
               (gtk:widget-class-css-name "GtkVideo")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkVideo")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkVideo" GTK:VIDEO
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_video_get_type")
                      ((AUTOPLAY VIDEO-AUTOPLAY "autoplay" "gboolean" T T)
                       (FILE VIDEO-FILE "file" "GFile" T T)
                       (GRAPHICS-OFFLOAD VIDEO-GRAPHICS-OFFLOAD
                        "graphics-offload" "GtkGraphicsOffloadEnabled" T T)
                       (LOOP VIDEO-LOOP "loop" "gboolean" T T)
                       (MEDIA-STREAM VIDEO-MEDIA-STREAM
                        "media-stream" "GtkMediaStream" T T)))
             (gobject:get-gtype-definition "GtkVideo"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-video-properties
  (let ((video (make-instance 'gtk:video)))
    (is-false (gtk:video-autoplay video))
    (is-false (gtk:video-file video))
    (is (eq :disabled (gtk:video-graphics-offload video)))
    (is-false (gtk:video-loop video))
    (is-false (gtk:video-media-stream video))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_video_new

(test gtk-video-new
  (is (typep (gtk:video-new) 'gtk:video)))

;;;     gtk_video_new_for_file

(test gtk-video-new-for-file
  (let* ((file (g:file-new-for-path (glib-sys:sys-path "test/gtk-logo.webm")))
         (video (gtk:video-new-for-file file)))
    (is (typep video 'gtk:video))
    (is (string= "gtk-logo.webm" (g:file-basename (gtk:video-file video))))))

;;;     gtk_video_new_for_filename

(test gtk-video-new-for-filename
  (let* ((filename (glib-sys:sys-path "test/gtk-logo.webm"))
         (video (gtk:video-new-for-filename filename)))
    (is (typep video 'gtk:video))
    (is (string= "gtk-logo.webm" (g:file-basename (gtk:video-file video))))))

;;;     gtk_video_new_for_media_stream

(test gtk-video-new-for-media-stream
  (let* ((filename (glib-sys:sys-path "test/gtk-logo.webm"))
         (stream (gtk:media-file-new-for-filename filename))
         (video (gtk:video-new-for-media-stream stream)))
      (is (typep video 'gtk:video))
      (is-false (gtk:video-file video))
      (is (typep (gtk:video-media-stream video) 'gtk:media-file))))

;;;     gtk_video_new_for_resource

(test gtk-video-new-for-resource
  (gio:with-g-resources (resource (glib-sys:sys-path
                                    "test/resource/rtest-resource.gresource"))
    (let* ((path "/com/crategus/test/gtk-logo.webm")
           (video (gtk:video-new-for-resource path)))
      (is (typep video 'gtk:video))
      (is (string= "gtk-logo.webm" (g:file-basename (gtk:video-file video)))))))

;;;     gtk_video_set_filename
;;;     gtk_video_set_resource

;;; 2024-9-19
