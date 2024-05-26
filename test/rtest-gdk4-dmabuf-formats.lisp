(in-package :gtk-test)

(def-suite gdk-dmabuf-formats :in gdk-suite)
(in-suite gdk-dmabuf-formats)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDmabufFormats

(test gdk-dmabuf-formats-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkDmabufFormats"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDmabufFormats")
          (g:gtype (cffi:foreign-funcall "gdk_dmabuf_formats_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:dmabuf-formats
          (glib:symbol-for-gtype "GdkDmabufFormats"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_dmabuf-formats-contains
;;;     gdk_dmabuf-formats-equal
;;;     gdk_dmabuf-formats-get_format
;;;     gdk_dmabuf-formats-get_n_formats
;;;     gdk_dmabuf-formats-ref
;;;     gdk_dmabuf-formats-unref

;;; 2024-5-26
