(def-suite gtk-media-file :in gtk-suite)
(in-suite gtk-media-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaFile

(test gtk-media-file-class
  ;; Type check
  (is (g:type-is-object "GtkMediaFile"))
  ;; Check the registered name
  (is (eq 'gtk-media-file
          (gobject:symbol-for-gtype "GtkMediaFile")))
  ;; Check the type initializer
  (is (eq (gtype "GtkMediaFile")
          (gtype (foreign-funcall "gtk_media_file_get_type" :size))))
  ;; Check the parent
  (is (eq (gtype "GtkMediaStream")
          (g-type-parent "GtkMediaFile")))
  ;; Check the children
  (is (equal '("GtkNoMediaFile")
             (mapcar #'g-type-name (g-type-children "GtkMediaFile"))))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (mapcar #'g-type-name (g-type-interfaces "GtkMediaFile"))))
  ;; Check the class properties
  (is (equal '("file" "input-stream")
             (list-class-property-names "GtkMediaFile")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkMediaFile"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkMediaFile" GTK-MEDIA-FILE
                       (:SUPERCLASS GTK-MEDIA-STREAM :EXPORT T :INTERFACES
                        ("GdkPaintable") :TYPE-INITIALIZER
                        "gtk_media_file_get_type")
                       ((FILE GTK-MEDIA-FILE-FILE "file" "GFile" T T)
                        (INPUT-STREAM GTK-MEDIA-FILE-INPUT-STREAM
                         "input-stream" "GInputStream" T T)))
             (get-g-type-definition "GtkMediaFile"))))

;;; --- Properties -------------------------------------------------------------

;;;     file
;;;     input-stream

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_file_new
;;;     gtk_media_file_new_for_filename
;;;     gtk_media_file_new_for_resource
;;;     gtk_media_file_new_for_file
;;;     gtk_media_file_new_for_input_stream
;;;     gtk_media_file_clear
;;;     gtk_media_file_set_filename
;;;     gtk_media_file_set_resource
;;;     gtk_media_file_set_file
;;;     gtk_media_file_get_file
;;;     gtk_media_file_set_input_stream
;;;     gtk_media_file_get_input_stream

;;; 2022-9-8
