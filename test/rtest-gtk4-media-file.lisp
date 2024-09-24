(in-package :gtk-test)

(def-suite gtk-media-file :in gtk-suite)
(in-suite gtk-media-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaFile

(test gtk-media-file-class
  ;; Check type
  (is (g:type-is-object "GtkMediaFile"))
  ;; Check registered name
  (is (eq 'gtk:media-file
          (glib:symbol-for-gtype "GtkMediaFile")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMediaFile")
          (g:gtype (cffi:foreign-funcall "gtk_media_file_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkMediaStream")
          (g:type-parent "GtkMediaFile")))
  ;; Check children
  (is (equal '("GtkGstMediaFile" "GtkNoMediaFile")
             (glib-test:list-children "GtkMediaFile")))
  ;; Check interfaces
  (is (equal '("GdkPaintable")
             (glib-test:list-interfaces "GtkMediaFile")))
  ;; Check class properties
  (is (equal '("file" "input-stream")
             (glib-test:list-properties "GtkMediaFile")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkMediaFile")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkMediaFile" GTK:MEDIA-FILE
                       (:SUPERCLASS GTK:MEDIA-STREAM
                        :EXPORT T
                        :INTERFACES ("GdkPaintable")
                        :TYPE-INITIALIZER "gtk_media_file_get_type")
                       ((FILE MEDIA-FILE-FILE "file" "GFile" T T)
                        (INPUT-STREAM MEDIA-FILE-INPUT-STREAM
                         "input-stream" "GInputStream" T T)))
             (gobject:get-gtype-definition "GtkMediaFile"))))

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

;;; 2024-9-20
