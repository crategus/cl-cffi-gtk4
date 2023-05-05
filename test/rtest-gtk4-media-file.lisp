(in-package :gtk-test)

(def-suite gtk-media-file :in gtk-suite)
(in-suite gtk-media-file)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkMediaFile

(test gtk-media-file-class
  ;; Type check
  (is (g:type-is-object "GtkMediaFile"))
  ;; Check the registered name
  (is (eq 'gtk:media-file
          (gobject:symbol-for-gtype "GtkMediaFile")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkMediaFile")
          (g:gtype (cffi:foreign-funcall "gtk_media_file_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkMediaStream")
          (g:type-parent "GtkMediaFile")))
  ;; Check the children
  (is (equal '("GtkGstMediaFile" "GtkNoMediaFile")
             (list-children "GtkMediaFile")))
  ;; Check the interfaces
  (is (equal '("GdkPaintable")
             (list-interfaces "GtkMediaFile")))
  ;; Check the class properties
  (is (equal '("file" "input-stream")
             (list-properties "GtkMediaFile")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkMediaFile")))
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

;;; --- 2023-5-3 ---------------------------------------------------------------
