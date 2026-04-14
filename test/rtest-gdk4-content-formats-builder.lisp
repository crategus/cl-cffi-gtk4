(in-package :gtk-test)

(def-suite gdk-content-formats-builder :in gdk-suite)
(in-suite gdk-content-formats-builder)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkContentFormatsBuilder

(test gdk-content-formats-builder-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkContentFormatsBuilder"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkContentFormatsBuilder")
          (g:gtype (cffi:foreign-funcall "gdk_content_formats_builder_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:content-formats-builder
          (glib:symbol-for-gtype "GdkContentFormatsBuilder"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_content_formats_builder_new

(test gdk-content-formats-builder-new
  (let (builder)
    (is (typep (setf builder
                     (gdk:content-formats-builder-new)) 'gdk:content-formats-builder))
    (is (string= ""
                 (gdk:content-formats-to-string
                     (gdk:content-formats-builder-to-formats builder))))))

;;;     gdk_content_formats_builder_free_to_formats         not needed

;;;     gdk_content_formats_builder_add_formats

(test gdk-content-formats-builder-add-formats
  (let (builder formats)
    (is (typep (setf builder
                     (gdk:content-formats-builder-new)) 'gdk:content-formats-builder))
    (is (typep (setf formats
                     (gdk:content-formats-new "text/plain" "image/png"))
               'gdk:content-formats))
    (is-false (gdk:content-formats-builder-add-formats builder formats))
    (is (string= "text/plain image/png"
                 (gdk:content-formats-to-string
                     (gdk:content-formats-builder-to-formats builder))))))

;;;     gdk_content_formats_builder_add_gtype

(test gdk-content-formats-builder-add-gtype.1
  (let (builder)
    (is (typep (setf builder
                     (gdk:content-formats-builder-new)) 'gdk:content-formats-builder))
    (is-false (gdk:content-formats-builder-add-gtype builder "GdkRGBA"))
    (is-false (gdk:content-formats-builder-add-gtype builder "gchararray"))
    (is-false (gdk:content-formats-builder-add-gtype builder "GtkButton"))

    (is (string= "GdkRGBA gchararray GtkButton"
                 (gdk:content-formats-to-string
                     (gdk:content-formats-builder-to-formats builder))))))

(test gdk-content-formats-builder-add-gtype.2
  (let (builder)
    (is (typep (setf builder
                     (gdk:content-formats-builder-new)) 'gdk:content-formats-builder))
    (is-false (gdk:content-formats-builder-add-gtype builder "GdkRGBA"))
    (is-false (gdk:content-formats-builder-add-mime-type builder "text/plain"))
    (is-false (gdk:content-formats-builder-add-gtype builder "gchararray"))
    (is-false (gdk:content-formats-builder-add-mime-type builder "image/png"))
    (is-false (gdk:content-formats-builder-add-gtype builder "GtkButton"))
    ;; First GTypes and then the MIME types
    (is (string= "GdkRGBA gchararray GtkButton text/plain image/png"
                 (gdk:content-formats-to-string
                     (gdk:content-formats-builder-to-formats builder))))))

;;;     gdk_content_formats_builder_add_mime_type

(test gdk-content-formats-builder-add-mime-type
  (let (builder)
    (is (typep (setf builder
                     (gdk:content-formats-builder-new)) 'gdk:content-formats-builder))
    (is-false (gdk:content-formats-builder-add-mime-type builder "text/plain"))
    (is-false (gdk:content-formats-builder-add-mime-type builder "image/png"))
    (is (string= "text/plain image/png"
                 (gdk:content-formats-to-string
                     (gdk:content-formats-builder-to-formats builder))))))

;;;     gdk_content_formats_builder_ref                     not needed
;;;     gdk_content_formats_builder_unref                   not needed

;;;     gdk_content_formats_builder_to_formats

;;; 2026-03-31
