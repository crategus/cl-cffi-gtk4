;;; ----------------------------------------------------------------------------
;;; gdk4.texture.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GdkTexture
;;;
;;;     Pixel data
;;;
;;; Types and Values
;;;
;;;     GdkMemoryFormat
;;;
;;;     GdkTexture
;;;     GdkMemoryTexture
;;;     GdkGLTexture
;;;
;;; Accessors
;;;
;;;     gdk_texture_get_color_state                         Since 4.16
;;;     gdk_texture_get_width
;;;     gdk_texture_get_height
;;;
;;; Functions
;;;
;;;     gdk_texture_new_for_pixbuf
;;;     gdk_texture_new_from_resource
;;;     gdk_texture_new_from_file
;;;     gdk_texture_new_from_filename                       Since 4.6
;;;     gdk_texture_new_from_bytes                          Since 4.6
;;;
;;;     gdk_texture_download
;;;     gdk_texture_save_to_png
;;;     gdk_texture_save_to_png_bytes                       Since 4.6
;;;     gdk_texture_save_to_tiff                            Since 4.6
;;;     gdk_texture_save_to_tiff_bytes                      Since 4.6
;;;     gdk_texture_get_format                              Since 4.10
;;;
;;;     gdk_memory_texture_new
;;;
;;;     gdk_gl_texture_new                                  not implemented
;;;     gdk_gl_texture_release                              not implemented
;;;
;;; Properties
;;;
;;;     color-state                                         Since 4.16
;;;     height
;;;     width
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkTexture
;;;         ├── GdkGLTexture
;;;         ╰── GdkMemoryTexture
;;;
;;; Implemented Interfaces
;;;
;;;     GdkTexture implements GdkPaintable.
;;;     GdkMemoryTexture implements GdkPaintable.
;;;     GdkGLTexture implements GdkPaintable.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

(defconstant +memory-default+
  #+little-endian
  :B8G8R8A8-PREMULTIPLIED
  #+big-endian
  :A8R8G8B8-PREMULTIPLIED)

#+liber-documentation
(setf (liber:alias-for-variable '+memory-default+)
      "Constant"
      (documentation '+memory-default+ 'variable)
 "@version{2025-07-30}
  @begin{short}
    The default @sym{gdk:memory-format} value for the memory format used by GTK.
  @end{short}
  This is the format provided by the @fun{gdk:texture-download} function. It is
  equal to the @code{:argb32} value of the @sym{cairo:format-t} enumeration.

  Be aware that this format is different for different endianness.
  @see-symbol{gdk:memory-format}
  @see-symbol{cairo:format-t}
  @see-function{gdk:texture-download}")

(export '+memory-default+)

;;; ----------------------------------------------------------------------------
;;; GdkMemoryFormat
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkMemoryFormat" memory-format
  (:export t
   :type-initializer "gdk_memory_format_get_type")
  :B8G8R8A8-PREMULTIPLIED
  :A8R8G8B8-PREMULTIPLIED
  :R8G8B8A8-PREMULTIPLIED
  :B8G8R8A8
  :A8R8G8B8
  :R8G8B8A8
  :A8B8G8R8
  :R8G8B8
  :B8G8R8
  #+gtk-4-6
  :R16G16B16
  #+gtk-4-6
  :R16G16B16A16-PREMULTIPLIED
  #+gtk-4-6
  :R16G16B16A16
  #+gtk-4-6
  :R16G16B16-FLOAT
  #+gtk-4-6
  :R16G16B16A16-FLOAT-PREMULTIPLIED
  #+gtk-4-6
  :R16G16B16A16-FLOAT
  #+gtk-4-6
  :R32G32B32-FLOAT
  #+gtk-4-6
  :R32G32B32A32-FLOAT-PREMULTIPLIED
  #+gtk-4-6
  :R32G32B32A32-FLOAT
  #+gtk-4-12
  :8A8-PREMULTIPLIED
  #+gtk-4-12
  :G8A8
  #+gtk-4-12
  :G8
  #+gtk-4-12
  :G16A16-PREMULTIPLIED
  #+gtk-4-12
  :G16A16
  #+gtk-4-12
  :G16
  #+gtk-4-12
  :A8
  #+gtk-4-12
  :A16
  #+gtk-4-12
  :A16-FLOAT
  #+gtk-4-12
  :A32-FLOAT
  #+gtk-4-14
  :A8B8G8R8-PREMULTIPLIED
  #+gtk-4-14
  :B8G8R8X8
  #+gtk-4-14
  :X8R8G8B8
  #+gtk-4-14
  :R8G8B8X8
  #+gtk-4-14
  :X8B8G8R8
  :N-FORMATS)

#+liber-documentation
(setf (liber:alias-for-symbol 'memory-format)
      "GEnum"
      (liber:symbol-documentation 'memory-format)
 "@version{2025-07-30}
  @begin{declaration}
(gobject:define-genum \"GdkMemoryFormat\" memory-format
  (:export t
   :type-initializer \"gdk_memory_format_get_type\")
  :B8G8R8A8-PREMULTIPLIED
  :A8R8G8B8-PREMULTIPLIED
  :R8G8B8A8-PREMULTIPLIED
  :B8G8R8A8
  :A8R8G8B8
  :R8G8B8A8
  :A8B8G8R8
  :R8G8B8
  :B8G8R8
  :R16G16B16
  :R16G16B16A16-PREMULTIPLIED
  :R16G16B16A16
  :R16G16B16-FLOAT
  :R16G16B16A16-FLOAT-PREMULTIPLIED
  :R16G16B16A16-FLOAT
  :R32G32B32-FLOAT
  :R32G32B32A32-FLOAT-PREMULTIPLIED
  :R32G32B32A32-FLOAT
  :8A8-PREMULTIPLIED
  :G8A8
  :G8
  :G16A16-PREMULTIPLIED
  :G16A16
  :G16
  :A8
  :A16
  :A16-FLOAT
  :A32-FLOAT
  :A8B8G8R8-PREMULTIPLIED
  :B8G8R8X8
  :X8R8G8B8
  :R8G8B8X8
  :X8B8G8R8
  :N-FORMATS)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:B8G8R8A8-PREMULTIPLIED]{4 bytes for blue, green, red, alpha. The
        color values are premultiplied with the alpha value. This is the default
        memory format for little endianness.}
      @entry[:A8R8G8B8-PREMULTIPLIED]{4 bytes for alpha, red, green, blue. The
        color values are premultiplied with the alpha value. This is the default
        memory format for big endianness.}
      @entry[:R8G8B8A8-PREMULTIPLIED]{4 bytes for red, green, blue, alpha The
        color values are premultiplied with the alpha value.}
      @entry[:B8G8R8A8]{4 bytes for blue, green, red, alpha.}
      @entry[:A8R8G8B8]{4 bytes for alpha, red, green, blue.}
      @entry[:R8G8B8A8]{4 bytes for red, green, blue, alpha.}
      @entry[:A8B8G8R8]{4 bytes for alpha, blue, green, red.}
      @entry[:R8G8B8]{3 bytes for red, green, blue. The data is opaque.}
      @entry[:B8G8R8]{3 bytes for blue, green, red. The data is opaque.}
      @entry[:R16G16B16]{3 guint16 values for red, green, blue. Since 4.6}
      @entry[:R16G16B16A16-PREMULTIPLIED]{4 guint16 values for red, green,
        blue, alpha. The color values are premultiplied with the alpha value.
        Since 4.6}
      @entry[:R16G16B16A16]{4 guint16 values for red, green, blue, alpha.
        Since 4.6}
      @entry[:R16G16B16-FLOAT]{3 half-float values for red, green, blue. The
        data is opaque. Since 4.6}
      @entry[:R16G16B16A16-FLOAT-PREMULTIPLIED]{4 half-float values for red,
        green, blue and alpha. The color values are premultiplied with the
        alpha value. Since 4.6}
      @entry[:R16G16B16A16-FLOAT]{4 half-float values for red, green, blue and
        alpha. Since 4.6}
      @entry[:R32G32B32-FLOAT]{3 float values for red, green, blue.}
      @entry[:R32G32B32A32-FLOAT-PREMULTIPLIED]{4 float values for red, green,
        blue and alpha. The color values are premultiplied with the alpha value.
        Since 4.6}
      @entry[:R32G32B32A32-FLOAT]{4 float values for red, green, blue and
        alpha. Since 4.6}
      @entry[:8A8-PREMULTIPLIED]{2 bytes for grayscale, alpha. The color values
        are premultiplied with the alpha value. Since 4.12}
      @entry[:G8A8]{2 bytes for grayscale, alpha. Since 4.12}
      @entry[:G8]{One byte for grayscale. The data is opaque. Since 4.12}
      @entry[:G16A16-PREMULTIPLIED]{2 guint16 values for grayscale, alpha.
        The color values are premultiplied with the alpha value. Since 4.12}
      @entry[:G16A16]{2 guint16 values for grayscale, alpha. Since 4.12}
      @entry[:G16]{One guint16 value for grayscale. The data is opaque.
        Since 4.12}
      @entry[:A8]{One byte for alpha. Since 4.12}
      @entry[:A16]{One guint16 value for alpha. Since 4.12}
      @entry[:A16-FLOAT]{One half-float value for alpha. Since 4.12}
      @entry[:A32-FLOAT]{One float value for alpha. Since 4.12}
      @entry[:A8B8G8R8-PREMULTIPLIED]{4 bytes for alpha, blue, green, red.
        The color values are premultiplied with the alpha value. Since 4.14}
      @entry[:B8G8R8X8]{4 bytes for blue, green, red, unused. Since 4.14}
      @entry[:X8R8G8B8]{4 bytes for unused, red, green, blue. Since 4.14}
      @entry[:R8G8B8X8]{4 bytes for red, green, blue, unused. Since 4.14}
      @entry[:X8B8G8R8]{4 bytes for unused, blue, green, red. Since 4.14}
      @entry[:N-FORMATS]{The number of formats. This value will change as more
        formats get added, so do not rely on its concrete integer.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{gdk:memory-format} enumeration describes a format that bytes can
    have in memory.
  @end{short}
  It describes formats by listing the contents of the memory passed to it. So
  @val[gdk:memory-format]{:A8R8G8B8} will be 1 byte (8 bits) of alpha, followed
  by a byte each of red, green and blue. It is not endian-dependent, so the
  @val[gdk:memory-format]{:argb32} value of the @sym{cairo:format-t} enumeration
  is represented by different memory format values on architectures with
  different endiannesses.

  Its naming is modelled after
  @url[https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.html#VkFormat]{VkFormat}.
  @see-class{gdk:texture}
  @see-symbol{cairo:format-t}")

;;; ----------------------------------------------------------------------------
;;; GdkTexture
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkTexture" texture
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_texture_get_type")
  (#+gtk-4-16
   (color-state
    texture-color-state
    "color-state" "GdkColorState" t nil)
   (height
    texture-height
    "height" "gint" t nil)
   (width
    texture-width
    "width" "gint" t nil)))

#+liber-documentation
(setf (liber:alias-for-class 'texture)
      "Class"
      (documentation 'texture 'type)
 "@version{2025-03-11}
  @begin{short}
    The @class{gdk:texture} object is the basic element used to refer to pixel
    data.
  @end{short}
  It is primarily meant for pixel data that will not change over multiple
  frames, and will be used for a long time.

  There are various ways to create @class{gdk:texture} objects from a
  @class{gdk-pixbuf:pixbuf} object, or from bytes stored in memory, a file, or
  a @class{g:resource} instance.

  The ownership of the pixel data is transferred to the @class{gdk:texture}
  instance. You can only make a copy of it, with the @fun{gdk:texture-download}
  function.

  The @class{gdk:texture} object is an immutable object: That means you cannot
  change anything about it other than increasing the reference count with the
  @fun{g:object-ref} function.
  @see-constructor{gdk:texture-new-for-pixbuf}
  @see-constructor{gdk:texture-new-from-bytes}
  @see-constructor{gdk:texture-new-from-file}
  @see-constructor{gdk:texture-new-from-filename}
  @see-constructor{gdk:texture-new-from-resource}
  @see-class{gdk-pixbuf:pixbuf}
  @see-class{gdk:color-state}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:texture-color-state ------------------------------------------------

#+(and gtk-4-16 liber-documentation)
(setf (documentation (liber:slot-documentation "color-state" 'texture) t)
 "The @code{color-state} property of type @class{gdk:color-state}
  (Read / Write / Construct only) @br{}
  The color state of the texture. Since 4.16")

#+(and gtk-4-16 liber-documentation)
(setf (liber:alias-for-function 'texture-color-state)
      "Accessor"
      (documentation 'texture-color-state 'function)
 "@version{2025-03-11}
  @syntax{(gdk:texture-color-state object) => state}
  @argument[object]{a @class{gdk:texture} object}
  @argument[state]{a @class{gdk:color-state} instance}
  @begin{short}
    Accessor of the @slot[gdk:texture]{color-state} slot of the
    @class{gdk:texture} class.
  @end{short}
  The @fun{gdk:texture-color-state} function returns the color state associated
  with the texture.

  Since 4.16
  @see-class{gdk:texture}
  @see-class{gdk:color-state}")

;;; --- gdk:texture-height -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'texture) t)
 "The @code{height} property of type @code{:int} (Read / Write / Construct only)
  @br{}
  The height of the texture, in pixels. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'texture-height)
      "Accessor"
      (documentation 'texture-height 'function)
 "@version{2025-08-04}
  @syntax{(gdk:texture-height object) => height}
  @argument[object]{a @class{gdk:texture} object}
  @argument[height]{an integer for the height of the texture}
  @begin{short}
    The accessor for the @slot[gdk:texture]{height} slot of the
    @class{gdk:texture} class returns the height of the texture, in pixels.
  @end{short}
  @see-class{gdk:texture}")

;;; --- gdk:texture-width ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'texture) t)
 "The @code{width} property of type @code{:int} (Read / Write / Construct only)
  @br{}
  The width of the texture, in pixels. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'texture-width)
      "Accessor"
      (documentation 'texture-width 'function)
 "@version{2025-08-04}
  @syntax{(gdk:texture-width object) => width}
  @argument[object]{a @class{gdk:texture} object}
  @argument[width]{an integer for the width of the texture}
  @begin{short}
    The accessor of the @slot[gdk:texture]{width} slot of the
    @class{gdk:texture} class returns the width of the texture, in pixels.
  @end{short}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_for_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_new_for_pixbuf" texture-new-for-pixbuf)
    (g:object texture :return)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture object representing @arg{pixbuf}.
  @end{short}
  @see-class{gdk:texture}
  @see-class{gdk-pixbuf:pixbuf}"
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'texture-new-for-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_new_from_resource" texture-new-from-resource)
    (g:object texture :return)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[path]{a string for the path of the resource file}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture by loading an image from a resource.
  @end{short}
  The file format is detected automatically. The supported formats are PNG and
  JPEG, though more formats might be available.

  It is a fatal error if @arg{path} does not specify a valid image resource
  and the program will abort if that happens. If you are unsure about the
  validity of a resource, use the @fun{gdk:texture-new-from-file} function to
  load it.
  @see-class{gdk:texture}
  @see-function{gdk:texture-new-from-file}"
  (path :string))

(export 'texture-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_new_from_file" %texture-new-from-file)
    (g:object texture :return)
  (file g:object)
  (err :pointer))

(defun texture-new-from-file (file)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[file]{a @class{g:file} object to load}
  @begin{return}
    The newly created @class{gdk:texture} object, or @code{nil} if an error
    occurred.
  @end{return}
  @begin{short}
    Creates a new texture by loading an image from a file.
  @end{short}
  The file format is detected automatically. The supported formats are PNG and
  JPEG, though more formats might be available.
  @see-class{gdk:texture}
  @see-class{g:file}"
  (glib:with-error (err)
    (%texture-new-from-file file err)))

(export 'texture-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_filename
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_new_from_filename" %texture-new-from-filename)
    (g:object texture :return)
  (path :string)
  (err :pointer))

#+gtk-4-6
(defun texture-new-from-filename (path)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[path]{a pathname or namestring for the file to load}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture by loading an image from a file.
  @end{short}
  The file format is detected automatically. The supported formats are PNG and
  JPEG, though more formats might be available.

  This function is threadsafe, so that you can, for example, use the
  @class{g:task} object and the @fun{g:task-run-in-thread} function to avoid
  blocking the main thread while loading a big image.

  Since 4.6
  @see-class{gdk:texture}
  @see-class{g:task}
  @see-function{g:task-run-in-thread}"
  (glib:with-error (err)
    (%texture-new-from-filename (namestring path) err)))

#+gtk-4-6
(export 'texture-new-from-filename)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_bytes                              Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_new_from_bytes" %texture-new-from-bytes)
    (g:object texture :return)
  (bytes (g:boxed g:bytes))
  (err :pointer))

#+gtk-4-6
(defun texture-new-from-bytes (bytes)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[bytes]{a @class{g:bytes} instance containing data to load}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture by loading an image from a memory.
  @end{short}
  The file format is detected automatically. The supported formats are PNG,
  JPEG, and TIFF, though more formats might be available.

  This function is threadsafe, so that you can, for example, use the
  @class{g:task} object and the @fun{g:task-run-in-thread} function to avoid
  blocking the main thread while loading a big image.

  Since 4.6
  @see-class{gdk:texture}
  @see-class{g:bytes}
  @see-class{g:task}
  @see-function{g:task-run-in-thread}"
  (glib:with-error (err)
    (%texture-new-from-bytes bytes err)))

#+gtk-4-6
(export 'texture-new-from-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_download
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_download" texture-download) :void
 #+liber-documentation
 "@version{2025-07-30}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[data]{a pointer to enough memory to be filled with the downloaded
    data of @arg{texture}}
  @argument[stride]{an unsigned integer for the rowstride in bytes}
  @begin{short}
    Downloads the texture into local memory.
  @end{short}
  This may be an expensive operation, as the actual texture data may reside on
  a GPU or on a remote display server.

  The data format of the downloaded data is equivalent to the
  @val[cairo:format-t]{:argb32} value of the @sym{cairo:format-t} enumeration,
  so every downloaded pixel requires 4 bytes of memory.
  @begin[Examples]{dictionary}
  Downloading a texture into a Cairo image surface:
  @begin{pre}
(let ((surface (cairo:image-surface-create :argb32
                                           (gdk:texture-width texture)
                                           (gdk:texture-height texture))))
  (gdk:texture-download texture
                        (cairo:image-surface-data surface)
                        (cairo:image-surface-stride surface))
  (cairo:surface-mark-dirty surface)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{gdk:texture}
  @see-symbol{cairo:format-t}"
  (texture (g:object texture))
  (data :pointer)
  (stride :size))

(export 'texture-download)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_save_to_png
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_save_to_png" texture-save-to-png) :boolean
 #+liber-documentation
 "@version{#2025-03-11}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[filename]{a string for the filename to store to}
  @return{@em{True} if saving succed, @em{false} on failure.}
  @begin{short}
    Store the given texture to the filename as a PNG file.
  @end{short}
  This is a utility function intended for debugging and testing. If you want
  more control over formats, proper error handling or want to store to a
  @class{g:file} object or other location, you might want to look into using
  the GdkPixbuf library.
  @see-class{gdk:texture}"
  (texture (g:object texture))
  (filename :string))

(export 'texture-save-to-png)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_save_to_png_bytes                           Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_png_bytes" texture-save-to-png-bytes)
    (g:boxed g:bytes :return)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[texture]{a @class{gdk:texture} object}
  @return{The newly allocated @class{g:bytes} instance containing PNG data.}
  @begin{short}
    Store the given texture in memory as a PNG file.
  @end{short}
  Use the @fun{gdk:texture-new-from-bytes} function to read it back.

  If you want to serialize a texture, this is a convenient and portable way to
  do that. If you need more control over the generated image, such as attaching
  metadata,  you should look into an image handling library such as the
  GdkPixbuf library. If you are dealing with high dynamic range float data, you
  might also want to consider the @fun{gdk:texture-save-to-tiff-bytes} function
  instead.

  Since 4.6
  @see-class{gdk:texture}
  @see-class{g:bytes}
  @see-function{gdk:texture-save-to-tiff-bytes}"
  (texture (g:object texture)))

#+gtk-4-6
(export 'texture-save-to-png-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_save_to_tiff                                Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_tiff" texture-save-to-tiff) :boolean
 #+liber-documentation
 "@version{#2025-03-11}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[filename]{a string for the filename to store to}
  @return{@em{True} if saving succeeded, @em{false} on failure.}
  @begin{short}
    Store the given texture to the filename as a TIFF file.
  @end{short}
  GTK will attempt to store data without loss.

  Since 4.6
  @see-class{gdk:texture}"
  (texture (g:object texture))
  (filename :string))

#+gtk-4-6
(export 'texture-save-to-tiff)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_save_to_tiff_bytes                          Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_tiff_bytes" texture-save-to-tiff-bytes)
    (g:boxed g:bytes :return)
 #+liber-documentation
 "@version{2025-03-11}
  @argument[texture]{a @class{gdk:texture} object}
  @return{The newly allocated @class{g:bytes} instance containing TIFF data.}
  @begin{short}
    Store the given texture in memory as a TIFF file.
  @end{short}
  Use the @fun{gdk:texture-new-from-bytes} function to read it back.

  This function is intended to store a representation of the texture’s data that
  is as accurate as possible. This is particularly relevant when working with
  high dynamic range images and floating-point texture data.

  If that is not your concern and you are interested in a smaller size and a
  more portable format, you might want to use the
  @fun{gdk:texture-save-to-png-bytes} function.

  Since 4.6
  @see-class{gdk:texture}
  @see-class{g:bytes}
  @see-function{gdk:texture-new-from-bytes}
  @see-function{gdk:texture-save-to-png-bytes}"
  (texture (g:object texture)))

#+gtk-4-6
(export 'texture-save-to-tiff-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_get_format                                  Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gdk_texture_get_format" texture-format) memory-format
 #+liber-documentation
 "@version{2025-07-30}
  @argument[texture]{a @class{gdk:texture} object}
  @begin{return}
    The @sym{gdk:memory-format} value with the preferred format for the data
    of the texture.
  @end{return}
  @begin{short}
    Gets the memory format most closely associated with the data of the texture.
  @end{short}
  Note that it may not be an exact match for texture data stored on the GPU or
  with compression.

  The format can give an indication about the bit depth and opacity of the
  texture and is useful to determine the best format for downloading the
  texture.

  Since 4.10
  @see-class{gdk:texture}
  @see-symbol{gdk:memory-format}"
  (texture (g:object texture)))

#+gtk-4-10
(export 'texture-format)

;;; ----------------------------------------------------------------------------
;;; GdkMemoryTexture
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkMemoryTexture" memory-texture
  (:superclass texture
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_memory_texture_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'memory-texture)
      "Class"
      (documentation 'memory-texture 'type)
 "@version{2025-03-11}
  @begin{short}
    The @class{gdk:memory-texture} class is a @class{gdk:texture} implementation
    representing image data in memory.
  @end{short}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_memory_texture_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_memory_texture_new" memory-texture-new)
    (g:object texture :return)
 #+liber-documentation
 "@version{2025-07-30}
  @argument[width]{an integer for the width of the texture}
  @argument[height]{an integer for the height of the texture}
  @argument[format]{a @sym{gdk:memory-format} value for the format of the data}
  @argument[bytes]{a @class{g:bytes} instance containing the pixel data}
  @argument[stride]{an integer for the rowstride of the data}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture for a blob of image data.
  @end{short}
  The @class{g:bytes} instance must contain @code{stride x height} pixels in
  the given @arg{format}.
  @see-class{gdk:texture}
  @see-class{g:bytes}
  @see-symbol{gdk:memory-format}"
  (width :int)
  (height :int)
  (format memory-format)
  (bytes (g:boxed g:bytes))
  (stride :size))

(export 'memory-texture-new)

;;; ----------------------------------------------------------------------------
;;; GdkGLTexture
;;; ----------------------------------------------------------------------------

#+nil
(gobject:define-gobject "GdkGLTexture" gl-texture
  (:superclass texture
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_gl_texture_get_type")
  nil)

#+nil
(setf (liber:alias-for-class 'gl-texture)
      "Class"
      (documentation 'gl-texture 'type)
 "@version{#2023-08-01}
  @begin{short}
    A @class{gdk:texture} implementation representing a GL texture object.
  @end{short}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_gl_texture_new
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the code

#+nil
(cffi:defcfun ("gdk_gl_texture_new" %gl-texture-new)
    (g:object texture :return)
  (context (g:object gl-context))
  (id :uint)
  (width :int)
  (height :int)
  (destroy :pointer)
  (data :pointer))

#+nil
(defun gl-texture-new (context id width height)
 #+liber-documentation
 "@version{#2025-08-04}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[id]{an unsigned integer for the ID of a texture that was created
    with @arg{context}}
  @argument[width]{an integer for the nominal width of the texture}
  @argument[height]{an integer for the nominal height of the texture}
  @argument[destroy]{a destroy notify that will be called when the GL resources
    are released, not implemented at this time (2023-8-1)}
  @argument[data]{data that gets passed to destroy, not implemented at this
    time (2023-8-1)}
  @return{The newly created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture for an existing GL texture.
  @end{short}
  Note that the GL texture must not be modified until destroy is called, which
  will happen when the @class{gdk:texture} object is finalized, or due to an
  explicit call of the @fun{gdk:gl-texture-release} function.
  @see-class{gdk:texture}
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-texture-release}"
  (%gl-texture-new context
                   id
                   width
                   height
                   (cffi:null-pointer)
                   (cffi:null-pointer)))

#+nil
(export 'gl-texture-new)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_texture_release
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gdk_gl_texture_release" gl-texture-release) :void
 #+liber-documentation
 "@version{#2023-08-01}
  @argument[texture]{a @class{gdk:gl-texture} object}
  @begin{short}
    Releases the GL resources held by a @class{gdk:gl-texture} object that was
    created with the @fun{gdk:gl-texture-new} function.
  @end{short}
  The texture contents are still available via the @fun{gdk:texture-download}
  function, after this function has been called.
  @see-class{gdk:gl-texture}"
  (texture (g:object texture)))

#+nil
(export 'gl-texture-release)

;;; --- End of file gdk4.texture.lisp ------------------------------------------
