;;; ----------------------------------------------------------------------------
;;; gdk4.texture-downloader.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     TextureDownloader
;;;
;;; Functions
;;;
;;;     gdk_texture_downloader_new
;;;     gdk_texture_downloader_copy
;;;     gdk_texture_downloader_free                         not implemented
;;;
;;;     gdk_texture_downloader_download_bytes
;;;     gdk_texture_downloader_download_into
;;;
;;;     gdk_texture_downloader_get_color_state              Since 4.16
;;;     gdk_texture_downloader_set_color_state              Since 4.16
;;;     gdk_texture_downloader_get_format
;;;     gdk_texture_downloader_set_format
;;;     gdk_texture_downloader_get_texture
;;;     gdk_texture_downloader_set_texture
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkTextureDownloader
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque texture-downloader "GdkTextureDownloader"
  :export t
  :type-initializer "gdk_texture_downloader_get_type"
  :alloc (error "GdkTextureDownloader cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'texture-downloader)
      "GBoxed"
      (documentation 'texture-downloader 'type)
 "@version{2024-11-28}
  @begin{declaration}
(glib:define-gboxed-opaque texture-download \"GdkTextureDownloader\"
  :export t
  :type-initializer \"gdk_texture_downloader_get_type\"
  :alloc (error \"GdkTextureDownloader cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:texture-downloader} structure is used to download the
    contents of a @class{gdk:texture} object.
  @end{short}
  It is intended to be created as a short-term object for a single download,
  but can be used for multipe downloads of different textures or with different
  settings.

  The @class{gdk:texture-downloader} structure can be used to convert data
  between different formats. Create a @class{gdk:texture} object for the
  existing format and then download it in a different format.

  Since 4.10
  @see-constructor{gdk:texture-downloader-new}
  @see-constructor{gdk:texture-downloader-copy}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_downloader_new" texture-downloader-new)
    (g:boxed texture-downloader :return)
 #+liber-documentation
 "@version{2024-12-2}
  @argument[texture]{a @class{gdk:texture} object to download}
  @return{The new @class{gdk:texture-downloader} instance.}
  @begin{short}
    Creates a new texture downloader for @arg{texture}.
  @end{short}
  By default, the downloader will convert the data to the default memory format,
  and to the @code{sRGB} color state.

  Since 4.10
  @see-class{gdk:texture-downloader}
  @see-class{gdk:texture}"
  (texture (g:object texture)))

(export 'texture-downloader-new)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_downloader_copy" texture-downloader-copy)
    (g:boxed texture-downloader :return)
 #+liber-documentation
  "@version{#2024-12-2}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @return{The @class{gdk:texture-downloader} object with a copy.}
  @begin{short}
    Creates a copy of the downloader
  @end{short}

  Since 4.10
  @see-class{gdk:texture-downloader}"
  (downloader (g:boxed texture-downloader)))

(export 'texture-downloader-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_free                             not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_download_bytes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_downloader_download_bytes"
               %texture-downloader-download-bytes) (g:boxed g:bytes :return)
  (downloader (g:boxed texture-downloader))
  (stride (:pointer :size)))

(defun texture-downloader-download-bytes (downloader)
 #+liber-documentation
 "@version{#2024-12-2}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @argument[stride]{an unsigned integer with the stride of the resulting data
    in bytes}
  @return{The @class{g:bytes} instance with the downloaded pixels.}
  @begin{short}
    Downloads the given texture pixels into a @class{g:bytes} instance.
  @end{short}
  The rowstride will be stored in the @arg{stride} value.

  This function will abort if it tries to download a large texture and fails to
  allocate memory. If you think that may happen, you should handle memory
  allocation yourself and use the @fun{gdk:texture-downloader-download-into}
  function once allocation succeeded.

  Since 4.10
  @see-class{gdk:texture-downloader}
  @see-class{g:bytes}
  @see-function{gdk:texture-downloader-download-into}"
  (cffi:with-foreign-object (stride :size)
    (let ((bytes (%texture-downloader-download-bytes downloader stride)))
      (values bytes stride))))

(export 'texture-downloader-download-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_download_into
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_downloader_download_into"
               texture-downloader-download-into) :void
 #+liber-documentation
 "@version{#2024-12-2}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @argument[data]{a pointer to enough memory to be filled with the downloaded
    data of the texture}
  @argument[stride]{an unsigned integer with the rowstrides in bytes}
  @begin{short}
    Downloads the texture into local memory.
  @end{short}

  Since 4.10
  @see-class{gdk:texture-downloader}"
  (downloader (g:boxed texture-downloader))
  (data (:pointer :uchar))
  (stride :size))

(export 'texture-downloader-download-into)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_get_color_state                  Since 4.16
;;; gdk_texture_downloader_set_color_state                  Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(defun (setf texture-downloader-color-state) (value downloader)
  (cffi:foreign-funcall "gdk_texture_downloader_set_color_state"
                        (g:boxed texture-downloader) downloader
                        (g:boxed color-state) value
                        :void)
  value)

#+gtk-4-16
(cffi:defcfun ("gdk_texture_downloader_get_color_state"
               texture-downloader-color-state) (g:boxed color-state :return)
 #+liber-documentation
 "@version{2024-12-2}
  @syntax{(gdk:texture-downloader-color-state downloader) => state}
  @syntax{(setf (gdk:texture-downloader-color-state downloader) state)}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @argument[state]{a @class{gdk:color-state} instance with the color state to
    use}
  @begin{short}
    Sets the color state the downloader will convert the data to.
  @end{short}
  By default, the @code{sRGB} colorstate returned by the
  @fun{gdk:color-state-srgb} function is used.

  Since 4.16
  @see-class{gdk:texture-downloader}
  @see-class{gdk:color-state}"
  (downloader (g:boxed texture-downloader)))

#+gtk-4-16
(export 'texture-downloader-color-state)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_get_format
;;; gdk_texture_downloader_set_format
;;; ----------------------------------------------------------------------------

(defun (setf texture-downloader-format) (value downloader)
  (cffi:foreign-funcall "gdk_texture_downloader_set_format"
                        (g:boxed texture-downloader) downloader
                        memory-format value
                        :void)
  value)

(cffi:defcfun ("gdk_texture_downloader_get_format" texture-downloader-format)
    memory-format
 #+liber-documentation
 "@version{2024-12-2}
  @syntax{(gdk:texture-downloader-format downloader) => format}
  @syntax{(setf (gdk:texture-downloader-format downloader) format)}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @argument[format]{a @class{gdk:memory-format} value with the format to use}
  @begin{short}
    Sets the format the downloader will download.
  @end{short}
  By default, the @code{:default} value is set.

  Since 4.10
  @see-class{gdk:texture-downloader}
  @see-symbol{gdk:memory-format}"
  (downloader (g:boxed texture-downloader)))

(export 'texture-downloader-format)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_downloader_get_texture
;;; gdk_texture_downloader_set_texture
;;; ----------------------------------------------------------------------------

(defun (setf texture-downloader-texture) (value downloader)
  (cffi:foreign-funcall "gdk_texture_downloader_set_texture"
                        (g:boxed texture-downloader) downloader
                        (g:object texture) value
                        :void)
  value)

(cffi:defcfun ("gdk_texture_downloader_get_texture" texture-downloader-texture)
    (g:object texture)
 #+liber-documentation
 "@version{2024-12-2}
  @syntax{(gdk:texture-downloader-texture downloader) => texture}
  @syntax{(setf (gdk:texture-downloader-texture downloader) texture)}
  @argument[downloader]{a @class{gdk:texture-downloader} instance}
  @argument[texture]{a @class{gdk:texture} object to download}
  @begin{short}
    Changes the texture the downloader will download.
  @end{short}

  Since 4.10
  @see-class{gdk:texture-downloader}
  @see-class{gdk:texture}"
  (downloader (g:boxed texture-downloader)))

(export 'texture-downloader-texture)

;;; --- End of file gdk4.texture-downloader.lisp -------------------------------
