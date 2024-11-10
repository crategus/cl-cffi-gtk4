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

(test gtk-media-file-properties
  (let ((media (gtk:media-file-new)))
    (is-false (gtk:media-file-file media))
    ;; not implemented
;   (is-false (gtk:media-file-input-stream media))
    (is (= 1 (g:object-ref-count media)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_media_file_new

(test gtk-media-file-new
  (let (media)
    (is (typep (setf media (gtk:media-file-new)) 'gtk:media-file))
    (= 1 (g:object-ref-count media))))

;;;     gtk_media_file_new_for_file

(test gtk-media-file-new-for-file
  (let* ((path (glib-sys:sys-path "resource/gtk-logo.webm"))
         (file (g:file-new-for-path path))
         media)
    (is (typep (setf media
                     (gtk:media-file-new-for-file file)) 'gtk:media-file))
    (is-false (setf (gtk:media-file-file media) nil))
    (is (= 1 (g:object-ref-count file)))
    (is (= 1 (g:object-ref-count media)))))

;;;     gtk_media_file_new_for_filename

(test gtk-media-file-new-for-filename
  (let ((path (glib-sys:sys-path "resource/gtk-logo.webm"))
        media)
    (is (typep (setf media
                     (gtk:media-file-new-for-filename path)) 'gtk:media-file))
    (is (= 1 (g:object-ref-count media)))))

;;;     gtk_media_file_new_for_input_stream

;;;     gtk_media_file_new_for_resource

(test gtk-media-file-new-for-resource
  (let ((path "/com/crategus/test/gtk-logo.webm")
        media)
    (is (typep (setf media
                     (gtk:media-file-new-for-resource path)) 'gtk:media-file))
    (is (= 1 (g:object-ref-count media)))))

;;;     gtk_media_file_clear

(test gtk-media-file-clear
  (let* ((path (glib-sys:sys-path "resource/gtk-logo.webm"))
         (media (gtk:media-file-new-for-filename path)))
      (is (typep (gtk:media-file-file media) 'g:object))
      (is-false (gtk:media-file-clear media))
      (is-false (gtk:media-file-file media))
      (is (= 1 (g:object-ref-count media)))))

;;;     gtk_media_file_set_filename

(test gtk-media-file-set-filename
  (let ((path (glib-sys:sys-path "resource/gtk-logo.webm"))
        (media (gtk:media-file-new)))
    (is-false (gtk:media-file-file media))
    (is-false (gtk:media-file-set-filename media path))
    (is (string= "gtk-logo.webm"
                 (g:file-basename (gtk:media-file-file media))))
    (is-false (gtk:media-file-clear media))
    (is (= 1 (g:object-ref-count media)))))

;;;     gtk_media_file_set_resource

(test gtk-media-file-set-resource
  (let ((path "/com/crategus/test/gtk-logo.webm")
        (media (gtk:media-file-new)))
    (is-false (gtk:media-file-file media))
    (is-false (gtk:media-file-set-resource media path))
    (is (string= "gtk-logo.webm"
                 (g:file-basename (gtk:media-file-file media))))
    (is-false (gtk:media-file-clear media))
    (is (= 1 (g:object-ref-count media)))))

;;; 2024-10-31
