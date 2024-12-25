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

(test gdk-dmabuf-formats-contains
  (glib-test:with-check-memory (:strong 1)
    (let* ((display (gdk:display-default))
           (formats (gdk:display-dmabuf-formats display)))
      #-windows
      (is-true (gdk:dmabuf-formats-contains formats 538982482 72057594037927937))
      #+windows
      (is-false (gdk:dmabuf-formats-contains formats 538982482 72057594037927937)))))

;;;     gdk_dmabuf-formats-equal

(test gdk-dmabuf-formats-format
  (glib-test:with-check-memory ()
    (let ((display (gdk:display-default)))
      (is (gdk:dmabuf-formats-equal (gdk:display-dmabuf-formats display)
                                    (gdk:display-dmabuf-formats display))))))

;;;     gdk_dmabuf-formats-get_format

#-windows
(test gdk-dambuf-formats-format
  (glib-test:with-check-memory ()
    (let* ((display (gdk:display-default))
           (formats (gdk:display-dmabuf-formats display)))
      (is (equal '(538982482 72057594037927937)
                 (multiple-value-list (gdk:dmabuf-formats-format formats 0))))
      (is (equal '(538982482 72057594037927938)
                 (multiple-value-list (gdk:dmabuf-formats-format formats 1))))
      (is (equal '(538982482 72057594037927937)
                 (multiple-value-list (gdk:dmabuf-formats-format formats 2)))))))

;;;     gdk_dmabuf-formats-get_n_formats

(test gdk-dmabuf-formats-n-formats
  (glib-test:with-check-memory ()
    (let* ((display (gdk:display-default))
           (formats (gdk:display-dmabuf-formats display)))
      #-windows
      (is (= 271 (gdk:dmabuf-formats-n-formats formats)))
      #+windows
      (is (= 0 (gdk:dmabuf-formats-n-formats formats))))))

;;; 2024-12-20
