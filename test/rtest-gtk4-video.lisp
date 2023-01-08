(def-suite gtk-video :in gtk-suite)
(in-suite gtk-video)

;;; Types and Values

;;;     GtkVideo

(test gtk-video-class
  ;; Type check
  (is (g:type-is-object "GtkVideo"))
  ;; Check the registered name
  (is (eq 'gtk-video
          (gobject:symbol-for-gtype "GtkVideo")))
  ;; Check the type initializer
  (is (eq (gtype "GtkVideo")
          (gtype (foreign-funcall "gtk_video_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GtkWidget") (g-type-parent "GtkVideo")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkVideo"))))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (mapcar #'g-type-name (g-type-interfaces "GtkVideo"))))
  ;; Check the class properties
  (is (equal '("autoplay" "file" "loop" "media-stream")
             (list-class-property-names "GtkVideo")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkVideo"))
                   #'string<)))
  ;; CSS information
  (is (string= "video"
               (gtk-widget-class-css-name "GtkVideo")))
  (is (string=
"video:dir(ltr)
  overlay:dir(ltr)
    picture:dir(ltr)
    image.circular.large-icons.osd:dir(ltr)
    revealer:dir(ltr)
      controls.bottom.osd:dir(ltr)
        box.horizontal:disabled:dir(ltr)
          button.flat.image-button:disabled:dir(ltr)
            image:disabled:dir(ltr)
          box.horizontal:disabled:dir(ltr)
            label:disabled:dir(ltr)
            scale.horizontal:disabled:dir(ltr)
              trough:disabled:dir(ltr)
                highlight.top:disabled:dir(ltr)
                slider:disabled:dir(ltr)
            label:disabled:dir(ltr)
          scalebutton.scale:disabled:dir(ltr)
            button.flat.image-button.toggle:disabled:dir(ltr)
              image:disabled:dir(ltr)
            [popover.background.scale-popup:disabled:dir(ltr)]
              contents:disabled:dir(ltr)
                box.vertical:disabled:dir(ltr)
                  button.flat.image-button:disabled:dir(ltr)
                    image:disabled:dir(ltr)
                  scale.vertical:disabled:dir(ltr)
                    trough:disabled:dir(ltr)
                      highlight.bottom:disabled:dir(ltr)
                      slider:disabled:dir(ltr)
                  button.flat.image-button:disabled:dir(ltr)
                    image:disabled:dir(ltr)
              arrow:dir(ltr)
"
               (gtk-style-context-to-string
                   (gtk-widget-style-context (make-instance 'gtk-video))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkVideo" GTK-VIDEO
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_video_get_type")
                       ((AUTOPLAY GTK-VIDEO-AUTOPLAY "autoplay" "gboolean" T T)
                        (FILE GTK-VIDEO-FILE "file" "GFile" T T)
                        (LOOP GTK-VIDEO-LOOP "loop" "gboolean" T T)
                        (MEDIA-STREAM GTK-VIDEO-MEDIA-STREAM "media-stream"
                         "GtkMediaStream" T T)))
             (get-g-type-definition "GtkVideo"))))

;;; --- Properties -------------------------------------------------------------

;;;     autoplay
;;;     file
;;;     loop
;;;     media-stream

(test gtk-video-properties
  (let ((video (make-instance 'gtk-video)))
    (is-false (gtk-video-autoplay video))
    (is-false (gtk-video-file video))
    (is-false (gtk-video-loop video))
    (is-false (gtk-video-media-stream video))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_video_new
;;;     gtk_video_new_for_media_stream
;;;     gtk_video_new_for_file
;;;     gtk_video_new_for_filename
;;;     gtk_video_new_for_resource
;;;     gtk_video_get_media_stream
;;;     gtk_video_set_media_stream
;;;     gtk_video_set_filename
;;;     gtk_video_set_resource

;;; 2022-9-8
