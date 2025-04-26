(in-package :gtk-test)

(def-suite gdk-texture-downloader :in gdk-suite)
(in-suite gdk-texture-downloader)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkTextureDownloader

(test gdk-texture-downloader-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkTextureDownloader"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkTextureDownloader")
          (g:gtype (cffi:foreign-funcall "gdk_texture_downloader_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gdk:texture-downloader
          (glib:symbol-for-gtype "GdkTextureDownloader"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_texture_downloader_new

(test gdk-texture-downloader-new
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((pixbuf 2) (texture 2) :strong 2)
      (let ((path (glib-sys:sys-path "test/resource/ducky.png"))
             downloader)
        (setf pixbuf (gdk:pixbuf-new-from-file path))
        (setf texture (gdk:texture-new-for-pixbuf pixbuf))
        (is (typep (setf downloader
                         (gdk:texture-downloader-new texture))
                   'gdk:texture-downloader))))))

;;;     gdk_texture_downloader_copy

;;;     gdk_texture_downloader_download_bytes
;;;     gdk_texture_downloader_download_into

;;;     gdk_texture_downloader_get_color_state
;;;     gdk_texture_downloader_set_color_state

(test gdk-texture-downloader-color-state
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((pixbuf 2) (texture 2) :strong 2)
      (let ((path (glib-sys:sys-path "test/resource/ducky.png"))
            downloader)
        (setf pixbuf (gdk:pixbuf-new-from-file path))
        (setf texture (gdk:texture-new-for-pixbuf pixbuf))
        (is (typep (setf downloader
                         (gdk:texture-downloader-new texture))
                   'gdk:texture-downloader))
        (is-false (gdk:color-state-equal
                       (gdk:color-state-rec2100-linear)
                       (gdk:texture-downloader-color-state downloader)))
        (is-false (gdk:color-state-equal
                       (gdk:color-state-rec2100-pq)
                       (gdk:texture-downloader-color-state downloader)))
        (is (gdk:color-state-equal (gdk:color-state-srgb)
                                   (gdk:texture-downloader-color-state downloader)))
        (is-false (gdk:color-state-equal
                       (gdk:color-state-srgb-linear)
                       (gdk:texture-downloader-color-state downloader)))
        (is (gdk:color-state-equal
                (gdk:color-state-srgb-linear)
                (setf (gdk:texture-downloader-color-state downloader)
                      (gdk:color-state-srgb-linear))))))))

;;;     gdk_texture_downloader_get_format
;;;     gdk_texture_downloader_set_format

(test gdk-texture-downloader-format
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((pixbuf 2) (texture 2) :strong 2)
      (let ((path (glib-sys:sys-path "test/resource/ducky.png"))
            downloader)
        (setf pixbuf (gdk:pixbuf-new-from-file path))
        (setf texture (gdk:texture-new-for-pixbuf pixbuf))
        (is (typep (setf downloader
                         (gdk:texture-downloader-new texture))
                   'gdk:texture-downloader))
        (is (eq :B8G8R8A8-PREMULTIPLIED (gdk:texture-downloader-format downloader)))
        (is (eq :B8G8R8 (setf (gdk:texture-downloader-format downloader) :B8G8R8)))
        (is (eq :B8G8R8 (gdk:texture-downloader-format downloader)))))))

;;;     gdk_texture_downloader_get_texture
;;;     gdk_texture_downloader_set_texture

(test gdk-texture-downloader-texture
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((pixbuf 2) (texture 2) :strong 2)
      (let ((path (glib-sys:sys-path "test/resource/ducky.png"))
            downloader)
        (setf pixbuf (gdk:pixbuf-new-from-file path))
        (setf texture (gdk:texture-new-for-pixbuf pixbuf))
        (is (typep (setf downloader
                         (gdk:texture-downloader-new texture))
                   'gdk:texture-downloader))
        (is (eq texture (gdk:texture-downloader-texture downloader)))
        (is (eq texture (setf (gdk:texture-downloader-texture downloader) texture)))
        (is (eq texture (gdk:texture-downloader-texture downloader)))))))

;;; 2025-4-26
