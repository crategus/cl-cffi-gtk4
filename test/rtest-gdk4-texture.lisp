(in-package :gtk-test)

(def-suite gdk-texture :in gdk-suite)
(in-suite gdk-texture)

;;; --- Types and Values -------------------------------------------------------

;;;     GDK_MEMORY_DEFAULT

;;;     GdkMemoryFormat

;;;     GdkTexture
;;;     GdkMemoryTexture
;;;     GdkGLTexture

;;; --- Properties -------------------------------------------------------------

;;;     height
;;;     width

;;; --- Functions --------------------------------------------------------------

;;;     gdk_texture_new_for_pixbuf

(test gdk-texture-new-for-pixbuf
  (let* ((pixbuf (gdk:pixbuf-new-from-file (sys-path "resource/ducky.png")))
         (texture (gdk:texture-new-for-pixbuf pixbuf)))
    (is (typep pixbuf 'gdk-pixbuf:pixbuf))
    (is (typep texture 'gdk:texture))
    (is (= 489 (gdk:texture-width texture)))
    (is (= 537 (gdk:texture-height texture)))))

;;;     gdk_texture_new_from_resource

(test gdk-texture-new-from-resource
  (gio:with-g-resources (resource (sys-path "resource/rtest-resource.gresource"))
    (let* ((path "/com/crategus/test/ducky.png")
           (texture (gdk:texture-new-from-resource path)))
      (is (typep texture 'gdk:texture))
      (is (= 489 (gdk:texture-width texture)))
      (is (= 537 (gdk:texture-height texture))))))

;;;     gdk_texture_new_from_file

;;;     gdk_texture_new_from_filename                      Since 4.6



;;;     gdk_texture_new_from_bytes                         Since 4.6

;;;     gdk_texture_download
;;;     gdk_texture_save_to_png
;;;     gdk_texture_save_to_png_bytes                      Since 4.6
;;;     gdk_texture_save_to_tiff                           Since 4.6
;;;     gdk_texture_save_to_tiff_bytes                     Since 4.6
;;;     gdk_texture_get_format                             Since 4.10

;;;     gdk_memory_texture_new
;;;     gdk_gl_texture_new
;;;     gdk_gl_texture_release

;;; --- 2023-4-28 --------------------------------------------------------------
