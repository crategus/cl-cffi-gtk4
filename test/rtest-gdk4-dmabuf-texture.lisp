(in-package :gtk-test)

(def-suite gdk-dmabuf-texture :in gdk-suite)
(in-suite gdk-dmabuf-texture)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDmabufTexture

(test gdk-dmabuf-texture-class
  ;; Check type
  (is (g:type-is-object "GdkDmabufTexture"))
  ;; Check registered name
  (is (eq 'gdk:dmabuf-texture
          (glib:symbol-for-gtype "GdkDmabufTexture")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDmabufTexture")
          (g:gtype (cffi:foreign-funcall "gdk_dmabuf_texture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkTexture")
          (g:type-parent "GdkDmabufTexture")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDmabufTexture")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GdkDmabufTexture")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkDmabufTexture")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDmabufTexture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDmabufTexture" GDK:DMABUF-TEXTURE
                       (:SUPERCLASS GDK:TEXTURE
                        :EXPORT T
                        :INTERFACES ("GIcon" "GLoadableIcon" "GdkPaintable")
                        :TYPE-INITIALIZER "gdk_dmabuf_texture_get_type")
                       NIL)
             (gobject:get-gtype-definition "GdkDmabufTexture"))))

;;;     GdkDmabufTextureBuilder

(test gdk-dmabuf-texture-builder-class
  ;; Check type
  (is (g:type-is-object "GdkDmabufTextureBuilder"))
  ;; Check registered name
  (is (eq 'gdk:dmabuf-texture-builder
          (glib:symbol-for-gtype "GdkDmabufTextureBuilder")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDmabufTextureBuilder")
          (g:gtype (cffi:foreign-funcall "gdk_dmabuf_texture_builder_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDmabufTextureBuilder")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDmabufTextureBuilder")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDmabufTextureBuilder")))
  ;; Check properties
  (is (equal '("color-state" "display" "fourcc" "height" "modifier" "n-planes"
               "premultiplied" "update-region" "update-texture" "width")
             (glib-test:list-properties "GdkDmabufTextureBuilder")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDmabufTextureBuilder")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDmabufTextureBuilder"
                                      GDK:DMABUF-TEXTURE-BUILDER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_dmabuf_texture_builder_get_type")
                       ((COLOR-STATE DMABUF-TEXTURE-BUILDER-COLOR-STATE
                         "color-state" "GdkColorState" T T)
                        (DISPLAY DMABUF-TEXTURE-BUILDER-DISPLAY "display"
                         "GdkDisplay" T T)
                        (FOURCC DMABUF-TEXTURE-BUILDER-FOURCC "fourcc" "guint"
                         T T)
                        (HEIGHT DMABUF-TEXTURE-BUILDER-HEIGHT "height" "guint"
                         T T)
                        (MODIFIER DMABUF-TEXTURE-BUILDER-MODIFIER "modifier"
                         "guint64" T T)
                        (N-PLANES DMABUF-TEXTURE-BUILDER-N-PLANES "n-planes"
                         "guint" T T)
                        (PREMULTIPLIED DMABUF-TEXTURE-BUILDER-PREMULTIPLIED
                         "premultiplied" "gboolean" T T)
                        (UPDATE-REGION DMABUF-TEXTURE-BUILDER-UPDATE-REGION
                         "update-region" "CairoRegion" T T)
                        (UPDATE-TEXTURE DMABUF-TEXTURE-BUILDER-UPDATE-TEXTURE
                         "update-texture" "GdkTexture" T T)
                        (WIDTH DMABUF-TEXTURE-BUILDER-WIDTH "width" "guint" T
                         T)))
             (gobject:get-gtype-definition "GdkDmabufTextureBuilder"))))

;;; --- Properties -------------------------------------------------------------

;;;     display
;;;     fourcc
;;;     height
;;;     modifier
;;;     n-planes
;;;     premultiplied
;;;     update-region
;;;     update-texture
;;;     width

(test gdk-dmabuf-texture-builder-properties
  (let ((builder (make-instance 'gdk:dmabuf-texture-builder)))
    (is (eq (gdk:display-default)
            (gdk:dmabuf-texture-builder-display builder)))
    (is (= 0 (gdk:dmabuf-texture-builder-fourcc builder)))
    (is (= 0 (gdk:dmabuf-texture-builder-height builder)))
    (is (= 0 (gdk:dmabuf-texture-builder-modifier builder)))
    (is (= 1 (gdk:dmabuf-texture-builder-n-planes builder)))
    (is-true (gdk:dmabuf-texture-builder-premultiplied builder))
    ;; FIXME: Does not work. We have no boxed type for CairoRegion
;   (is-false (gdk:dmabuf-texture-builder-update-region builder))
    (is-false (gdk:dmabuf-texture-builder-update-texture builder))
    (is (= 0 (gdk:dmabuf-texture-builder-width builder)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_dmabuf_texture_builder_new
;;;     gdk_dmabuf_texture_builder_build

;;;     gdk_dmabuf_texture_builder_get_fd
;;;     gdk_dmabuf_texture_builder_set_fd

;;;     gdk_dmabuf_texture_builder_get_offset
;;;     gdk_dmabuf_texture_builder_set_offset

;;;     gdk_dmabuf_texture_builder_get_stride
;;;     gdk_dmabuf_texture_builder_set_stride

;;; 2024-11-29
