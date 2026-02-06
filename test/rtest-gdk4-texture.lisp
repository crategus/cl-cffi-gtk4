(in-package :gtk-test)

(def-suite gdk-texture :in gdk-suite)
(in-suite gdk-texture)

;;; --- Types and Values -------------------------------------------------------

;;;     GDK_MEMORY_DEFAULT

(test gdk-memory-default
  (is (eq :B8G8R8A8-PREMULTIPLIED gdk:+memory-default+)))

;;;     GdkMemoryFormat

(test gdk-memory-format
  ;; Check type
  (is (g:type-is-enum "GdkMemoryFormat"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkMemoryFormat")
          (g:gtype (cffi:foreign-funcall "gdk_memory_format_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:memory-format
          (glib:symbol-for-gtype "GdkMemoryFormat")))
  ;; Check names
  (is (equal '("GDK_MEMORY_B8G8R8A8_PREMULTIPLIED"
               "GDK_MEMORY_A8R8G8B8_PREMULTIPLIED"
               "GDK_MEMORY_R8G8B8A8_PREMULTIPLIED" "GDK_MEMORY_B8G8R8A8"
               "GDK_MEMORY_A8R8G8B8" "GDK_MEMORY_R8G8B8A8" "GDK_MEMORY_A8B8G8R8"
               "GDK_MEMORY_R8G8B8" "GDK_MEMORY_B8G8R8" "GDK_MEMORY_R16G16B16"
               "GDK_MEMORY_R16G16B16A16_PREMULTIPLIED" "GDK_MEMORY_R16G16B16A16"
               "GDK_MEMORY_R16G16B16_FLOAT"
               "GDK_MEMORY_R16G16B16A16_FLOAT_PREMULTIPLIED"
               "GDK_MEMORY_R16G16B16A16_FLOAT" "GDK_MEMORY_R32G32B32_FLOAT"
               "GDK_MEMORY_R32G32B32A32_FLOAT_PREMULTIPLIED"
               "GDK_MEMORY_R32G32B32A32_FLOAT"
               "GDK_MEMORY_G8A8_PREMULTIPLIED" "GDK_MEMORY_G8A8" "GDK_MEMORY_G8"
               "GDK_MEMORY_G16A16_PREMULTIPLIED" "GDK_MEMORY_G16A16"
               "GDK_MEMORY_G16" "GDK_MEMORY_A8" "GDK_MEMORY_A16"
               "GDK_MEMORY_A16_FLOAT" "GDK_MEMORY_A32_FLOAT"
               "GDK_MEMORY_A8B8G8R8_PREMULTIPLIED" "GDK_MEMORY_B8G8R8X8"
               "GDK_MEMORY_X8R8G8B8" "GDK_MEMORY_R8G8B8X8" "GDK_MEMORY_X8B8G8R8"
               "GDK_MEMORY_G8_B8R8_420" "GDK_MEMORY_G8_R8B8_420"
               "GDK_MEMORY_G8_B8R8_422" "GDK_MEMORY_G8_R8B8_422"
               "GDK_MEMORY_G8_B8R8_444" "GDK_MEMORY_G8_R8B8_444"
               "GDK_MEMORY_G10X6_B10X6R10X6_420"
               "GDK_MEMORY_G12X4_B12X4R12X4_420"
               "GDK_MEMORY_G16_B16R16_420" "GDK_MEMORY_G8_B8_R8_410"
               "GDK_MEMORY_G8_R8_B8_410" "GDK_MEMORY_G8_B8_R8_411"
               "GDK_MEMORY_G8_R8_B8_411" "GDK_MEMORY_G8_B8_R8_420"
               "GDK_MEMORY_G8_R8_B8_420" "GDK_MEMORY_G8_B8_R8_422"
               "GDK_MEMORY_G8_R8_B8_422" "GDK_MEMORY_G8_B8_R8_444"
               "GDK_MEMORY_G8_R8_B8_444" "GDK_MEMORY_G8B8G8R8_422"
               "GDK_MEMORY_G8R8G8B8_422" "GDK_MEMORY_R8G8B8G8_422"
               "GDK_MEMORY_B8G8R8G8_422" "GDK_MEMORY_X6G10_X6B10_X6R10_420"
               "GDK_MEMORY_X6G10_X6B10_X6R10_422"
               "GDK_MEMORY_X6G10_X6B10_X6R10_444"
               "GDK_MEMORY_X4G12_X4B12_X4R12_420"
               "GDK_MEMORY_X4G12_X4B12_X4R12_422"
               "GDK_MEMORY_X4G12_X4B12_X4R12_444" "GDK_MEMORY_G16_B16_R16_420"
               "GDK_MEMORY_G16_B16_R16_422" "GDK_MEMORY_G16_B16_R16_444"
               "GDK_MEMORY_N_FORMATS")
             (glib-test:list-enum-item-names "GdkMemoryFormat")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
               24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44
               45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65)
             (glib-test:list-enum-item-values "GdkMemoryFormat")))
  ;; Check nick names
  (is (equal '("b8g8r8a8-premultiplied" "a8r8g8b8-premultiplied"
               "r8g8b8a8-premultiplied" "b8g8r8a8" "a8r8g8b8" "r8g8b8a8"
               "a8b8g8r8" "r8g8b8" "b8g8r8" "r16g16b16"
               "r16g16b16a16-premultiplied" "r16g16b16a16" "r16g16b16-float"
               "r16g16b16a16-float-premultiplied" "r16g16b16a16-float"
               "r32g32b32-float" "r32g32b32a32-float-premultiplied"
               "r32g32b32a32-float" "g8a8-premultiplied" "g8a8" "g8"
               "g16a16-premultiplied" "g16a16" "g16" "a8" "a16" "a16-float"
               "a32-float" "a8b8g8r8-premultiplied" "b8g8r8x8" "x8r8g8b8"
               "r8g8b8x8" "x8b8g8r8" "g8-b8r8-420" "g8-r8b8-420" "g8-b8r8-422"
               "g8-r8b8-422" "g8-b8r8-444" "g8-r8b8-444" "g10x6-b10x6r10x6-420"
               "g12x4-b12x4r12x4-420" "g16-b16r16-420" "g8-b8-r8-410"
               "g8-r8-b8-410" "g8-b8-r8-411" "g8-r8-b8-411" "g8-b8-r8-420"
               "g8-r8-b8-420" "g8-b8-r8-422" "g8-r8-b8-422" "g8-b8-r8-444"
               "g8-r8-b8-444" "g8b8g8r8-422" "g8r8g8b8-422" "r8g8b8g8-422"
               "b8g8r8g8-422" "x6g10-x6b10-x6r10-420" "x6g10-x6b10-x6r10-422"
               "x6g10-x6b10-x6r10-444" "x4g12-x4b12-x4r12-420"
               "x4g12-x4b12-x4r12-422" "x4g12-x4b12-x4r12-444"
               "g16-b16-r16-420" "g16-b16-r16-422" "g16-b16-r16-444"
               "n-formats")
             (glib-test:list-enum-item-nicks "GdkMemoryFormat")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkMemoryFormat" GDK:MEMORY-FORMAT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_memory_format_get_type")
                                    (:B8G8R8A8-PREMULTIPLIED 0)
                                    (:A8R8G8B8-PREMULTIPLIED 1)
                                    (:R8G8B8A8-PREMULTIPLIED 2)
                                    (:B8G8R8A8 3)
                                    (:A8R8G8B8 4)
                                    (:R8G8B8A8 5)
                                    (:A8B8G8R8 6)
                                    (:R8G8B8 7)
                                    (:B8G8R8 8)
                                    (:R16G16B16 9)
                                    (:R16G16B16A16-PREMULTIPLIED 10)
                                    (:R16G16B16A16 11)
                                    (:R16G16B16-FLOAT 12)
                                    (:R16G16B16A16-FLOAT-PREMULTIPLIED 13)
                                    (:R16G16B16A16-FLOAT 14)
                                    (:R32G32B32-FLOAT 15)
                                    (:R32G32B32A32-FLOAT-PREMULTIPLIED 16)
                                    (:R32G32B32A32-FLOAT 17)
                                    (:G8A8-PREMULTIPLIED 18)
                                    (:G8A8 19)
                                    (:G8 20)
                                    (:G16A16-PREMULTIPLIED 21)
                                    (:G16A16 22)
                                    (:G16 23)
                                    (:A8 24)
                                    (:A16 25)
                                    (:A16-FLOAT 26)
                                    (:A32-FLOAT 27)
                                    (:A8B8G8R8-PREMULTIPLIED 28)
                                    (:B8G8R8X8 29)
                                    (:X8R8G8B8 30)
                                    (:R8G8B8X8 31)
                                    (:X8B8G8R8 32)
                                    (:G8-B8R8-420 33)
                                    (:G8-R8B8-420 34)
                                    (:G8-B8R8-422 35)
                                    (:G8-R8B8-422 36)
                                    (:G8-B8R8-444 37)
                                    (:G8-R8B8-444 38)
                                    (:G10X6-B10X6R10X6-420 39)
                                    (:G12X4-B12X4R12X4-420 40)
                                    (:G16-B16R16-420 41)
                                    (:G8-B8-R8-410 42)
                                    (:G8-R8-B8-410 43)
                                    (:G8-B8-R8-411 44)
                                    (:G8-R8-B8-411 45)
                                    (:G8-B8-R8-420 46)
                                    (:G8-R8-B8-420 47)
                                    (:G8-B8-R8-422 48)
                                    (:G8-R8-B8-422 49)
                                    (:G8-B8-R8-444 50)
                                    (:G8-R8-B8-444 51)
                                    (:G8B8G8R8-422 52)
                                    (:G8R8G8B8-422 53)
                                    (:R8G8B8G8-422 54)
                                    (:B8G8R8G8-422 55)
                                    (:X6G10-X6B10-X6R10-420 56)
                                    (:X6G10-X6B10-X6R10-422 57)
                                    (:X6G10-X6B10-X6R10-444 58)
                                    (:X4G12-X4B12-X4R12-420 59)
                                    (:X4G12-X4B12-X4R12-422 60)
                                    (:X4G12-X4B12-X4R12-444 61)
                                    (:G16-B16-R16-420 62)
                                    (:G16-B16-R16-422 63)
                                    (:G16-B16-R16-444 64)
                                    (:N-FORMATS 65))
             (gobject:get-gtype-definition "GdkMemoryFormat"))))

;;; ----------------------------------------------------------------------------

;;;     GdkColorState

(test gdk-color-state-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkColorState"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkColorState")
          (g:gtype (cffi:foreign-funcall "gdk_color_state_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:color-state
          (glib:symbol-for-gtype "GdkColorState"))))

;;;     gdk_color_state_get_rec2100_linear                  Since 4.16

(test gdk-color-state-rec2100-linear
  (is (typep (gdk:color-state-rec2100-linear) 'gdk:color-state)))

;;;     gdk_color_state_get_rec2100_pq                      Since 4.16

(test gdk-color-state-rec2100-pq
  (is (typep (gdk:color-state-rec2100-pq) 'gdk:color-state)))

;;;     gdk_color_state_get_srgb                            Since 4.16

(test gdk-color-state-srgb
  (is (typep (gdk:color-state-srgb) 'gdk:color-state)))

;;;     gdk_color_state_get_srgb_linear                     Since 4.16

(test gdk-color-state-srgb-linear
  (is (typep (gdk:color-state-srgb-linear) 'gdk:color-state)))

;;;     gdk_color_state_create_cicp_params                  Since 4.16
;;;     gdk_color_state_equal                               Since 4.16

;;; ----------------------------------------------------------------------------

;;;     GdkTexture

(test gdk-texture-class
  ;; Check type
  (is (g:type-is-object "GdkTexture"))
  ;; Check registered name
  (is (eq 'gdk:texture
          (glib:symbol-for-gtype "GdkTexture")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkTexture")
          (g:gtype (cffi:foreign-funcall "gdk_texture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkTexture")))
  ;; Check children
  (is (equal '("GdkDmabufTexture" "GdkGLTexture" "GdkMemoryTexture")
             (glib-test:list-children "GdkTexture")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GdkTexture")))
  ;; Check properties
  (is (equal '("color-state" "height" "width")
             (glib-test:list-properties "GdkTexture")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkTexture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkTexture" GDK:TEXTURE
                      (:SUPERCLASS GOBJECT:OBJECT
                       :EXPORT T
                       :INTERFACES ("GIcon" "GLoadableIcon" "GdkPaintable")
                       :TYPE-INITIALIZER "gdk_texture_get_type")
                      ((COLOR-STATE TEXTURE-COLOR-STATE "color-state"
                        "GdkColorState" T NIL)
                       (HEIGHT TEXTURE-HEIGHT "height" "gint" T NIL)
                       (WIDTH TEXTURE-WIDTH "width" "gint" T NIL)))
             (gobject:get-gtype-definition "GdkTexture"))))

;;;     GdkMemoryTexture

(test gdk-memory-texture-class
  ;; Check type
  (is (g:type-is-object "GdkMemoryTexture"))
  ;; Check registered name
  (is (eq 'gdk:memory-texture
          (glib:symbol-for-gtype "GdkMemoryTexture")))
  ;; Check type initializer
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "g_object_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkTexture")
          (g:type-parent "GdkMemoryTexture")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkMemoryTexture")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GdkMemoryTexture")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkMemoryTexture")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkMemoryTexture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkMemoryTexture" GDK:MEMORY-TEXTURE
                      (:SUPERCLASS GDK:TEXTURE
                       :EXPORT T
                       :INTERFACES ("GIcon" "GLoadableIcon" "GdkPaintable")
                       :TYPE-INITIALIZER "gdk_memory_texture_get_type")
                      NIL)
             (gobject:get-gtype-definition "GdkMemoryTexture"))))

;;;     GdkGLTexture

(test gdk-gl-texture-class
  ;; Check type
  (is (g:type-is-object "GdkGLTexture"))
  ;; Check registered name
  (is (eq 'gdk:gl-texture
          (glib:symbol-for-gtype "GdkGLTexture")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGLTexture")
          (g:gtype (cffi:foreign-funcall "gdk_gl_texture_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkTexture")
          (g:type-parent "GdkGLTexture")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkGLTexture")))
  ;; Check interfaces
  (is (equal '("GdkPaintable" "GIcon" "GLoadableIcon")
             (glib-test:list-interfaces "GdkGLTexture")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkGLTexture")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkGLTexture")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkGLTexture" GDK:GL-TEXTURE
                      (:SUPERCLASS GDK:TEXTURE
                       :EXPORT T
                       :INTERFACES ("GIcon" "GLoadableIcon" "GdkPaintable"))
                      NIL)
             (gobject:get-gtype-definition "GdkGLTexture"))))

;;; --- Properties -------------------------------------------------------------

;;;     color-state                                         Since 4.16
;;;     height
;;;     width

;;; --- Functions --------------------------------------------------------------

;;;     gdk_texture_new_for_pixbuf

;; TODO: Can we release the reference of the pixbuf from the texture

(test gdk-texture-new-for-pixbuf
  (when *first-run-testsuite*
    (glib-test:with-check-memory ((pixbuf 2) texture :strong 1)
      (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
        (setf pixbuf (gdk:pixbuf-new-from-file path))
        (setf texture (gdk:texture-new-for-pixbuf pixbuf))
        (is (typep pixbuf 'gdk-pixbuf:pixbuf))

        (is (=  489 (gdk:pixbuf-width pixbuf)))
        (is (=  537 (gdk:pixbuf-height pixbuf)))
        (is (= 1956 (gdk:pixbuf-rowstride pixbuf)))

        (is (typep texture 'gdk:texture))
        (is (eq :R8G8B8A8 (gdk:texture-format texture)))
        (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
        (is (= 489 (gdk:texture-width texture)))
        (is (= 537 (gdk:texture-height texture)))))))

;;;     gdk_texture_new_from_resource

(test gdk-texture-new-from-resource
  (glib-test:with-check-memory (texture)
    (let ((path (glib-sys:sys-path "test/rtest-resource.gresource")))
      (gio:with-resource (resource path)
        (let* ((path "/com/crategus/test/ducky.png"))
          (setf texture (gdk:texture-new-from-resource path))
          (is (typep texture 'gdk:texture))
          (is (eq :R8G8B8A8 (gdk:texture-format texture)))
          (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
          (is (= 489 (gdk:texture-width texture)))
          (is (= 537 (gdk:texture-height texture))))))))

;;;     gdk_texture_new_from_file

(test gdk-texture-new-from-file
  (glib-test:with-check-memory (file texture)
    (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
      (setf file (g:file-new-for-path path))
      (setf texture (gdk:texture-new-from-file file))
      (is (typep texture 'gdk:texture))
      (is (eq :R8G8B8A8 (gdk:texture-format texture)))
      (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
      (is (= 489 (gdk:texture-width texture)))
      (is (= 537 (gdk:texture-height texture))))))

;;;     gdk_texture_new_from_filename                      Since 4.6

(test gdk-texture-new-from-filename
  (glib-test:with-check-memory (texture)
    (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
      (setf texture (gdk:texture-new-from-filename path))
      (is (typep texture 'gdk:texture))
      (is (eq :R8G8B8A8 (gdk:texture-format texture)))
      (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
      (is (= 489 (gdk:texture-width texture)))
      (is (= 537 (gdk:texture-height texture))))))

;;;     gdk_texture_save_to_png_bytes                      Since 4.6
;;;     gdk_texture_new_from_bytes                         Since 4.6

(test gdk-texture-save-to-png-bytes
  (glib-test:with-check-memory (texture)
    (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
      (setf texture (gdk:texture-new-from-filename path))
      (let ((bytes (gdk:texture-save-to-png-bytes texture)))
        (is (typep (setf texture
                         (gdk:texture-new-from-bytes bytes)) 'gdk:texture))
        (is (eq :R8G8B8A8 (gdk:texture-format texture)))
        (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
        (is (= 489 (gdk:texture-width texture)))
        (is (= 537 (gdk:texture-height texture)))))))

;;;     gdk_texture_save_to_tiff_bytes                     Since 4.6
;;;     gdk_texture_new_from_bytes                         Since 4.6

(test gdk-texture-save-to-tiff-bytes
  (glib-test:with-check-memory (texture)
    (let ((path (glib-sys:sys-path "test/resource/ducky.png")))
      (setf texture (gdk:texture-new-from-filename path))
      (let ((bytes (gdk:texture-save-to-tiff-bytes texture)))

        (is (typep (setf texture
                         (gdk:texture-new-from-bytes bytes)) 'gdk:texture))
        (is (eq :R8G8B8A8 (gdk:texture-format texture)))
        (is (typep (gdk:texture-color-state texture) 'gdk:color-state))
        (is (= 489 (gdk:texture-width texture)))
        (is (= 537 (gdk:texture-height texture)))))))

;;;     gdk_texture_download

(test gdk-texture-download
  (glib-test:with-check-memory (texture)
    (let* ((path (glib-sys:sys-path "test/resource/ducky.png"))
           (size (* 4 489 537))
           (rowstride (* 4 489))
           (data (cffi:foreign-alloc :uchar :count size :initial-element #xff))
           (bytes (g:bytes-new data size)))
      (setf texture (gdk:texture-new-from-filename path))
      ;; Retrieve the texture from the BYTES instance
      (setf texture (gdk:memory-texture-new 489 537
                                            gdk:+memory-default+
                                            bytes
                                            rowstride))
        (is (eq :B8G8R8A8-PREMULTIPLIED (gdk:texture-format texture)))
        (is (= 489 (gdk:texture-width texture)))
        (is (= 537 (gdk:texture-height texture))))))

;;;     gdk_texture_save_to_png
;;;     gdk_texture_save_to_tiff                           Since 4.6

;;;     gdk_memory_texture_new

;;;     gdk_gl_texture_new
;;;     gdk_gl_texture_release

;;; 2026-02-05
