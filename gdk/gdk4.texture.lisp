;;; ----------------------------------------------------------------------------
;;; gdk4.texture.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;;     GdkColorState                                       Since 4.16
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
;;;     gdk_color_state_get_rec2100_linear                  Since 4.16
;;;     gdk_color_state_get_rec2100_pq                      Since 4.16
;;;     gdk_color_state_get_srgb                            Since 4.16
;;;     gdk_color_state_get_srgb_linear                     Since 4.16
;;;
;;;     gdk_color_state_create_cicp_params                  Since 4.16
;;;     gdk_color_state_equal                               Since 4.16
;;;     gdk_color_state_ref                                 Since 4.16
;;;     gdk_color_state_unref                               Since 4.16
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
;;;     gdk_gl_texture_new
;;;     gdk_gl_texture_release
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
  :R16G16B16
  :R16G16B16A16-PREMULTIPLIED
  :R16G16B16A16
  :R16G16B16-FLOAT
  :R16G16B16A16-FLOAT-PREMULTIPLIED
  :R16G16B16A16-FLOAT
  :R32G32B32-FLOAT
  :R32G32B32A32-FLOAT-PREMULTIPLIED
  :R32G32B32A32_FLOAT
  :N-FORMATS)

#+liber-documentation
(setf (liber:alias-for-symbol 'memory-format)
      "GEnum"
      (liber:symbol-documentation 'memory-format)
 "@version{#2023-4-12}
  @begin{short}
    The @sym{gdk:memory-format} enumeration describes a format that bytes can
    have in memory.
  @end{short}
  It describes formats by listing the contents of the memory passed to it. So
  @code{:A8R8G8B8} will be 1 byte (8 bits) of alpha, followed by a byte each of
  red, green and blue. It is not endian-dependent, so the @code{:argb32} value
  of the @symbol{cairo:format-t} enumeration is represented by different
  @symol{gdk:memory-format} values on architectures with different endiannesses.

  Its naming is modelled after VkFormat. See
  https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.htmlVkFormat
  for details.
  @begin{pre}
(gobject:define-g-enum \"GdkMemoryFormat\" memory-format
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
  :N-FORMATS)
  @end{pre}
  @begin[code]{table}
    @entry[:B8G8R8A8-PREMULTIPLIED]{4 bytes; for blue, green, red, alpha. The
      color values are premultiplied with the alpha value.}
    @entry[:A8R8G8B8-PREMULTIPLIED]{4 bytes; for alpha, red, green, blue. The
      color values are premultiplied with the alpha value.}
    @entry[:R8G8B8A8-PREMULTIPLIED]{4 bytes; for red, green, blue, alpha The
      color values are premultiplied with the alpha value.}
    @entry[:B8G8R8A8]{4 bytes; for blue, green, red, alpha.}
    @entry[:A8R8G8B8]{4 bytes; for alpha, red, green, blue.}
    @entry[:R8G8B8A8]{4 bytes; for red, green, blue, alpha.}
    @entry[:A8B8G8R8]{4 bytes; for alpha, blue, green, red.}
    @entry[:R8G8B8]{3 bytes; for red, green, blue. The data is opaque.}
    @entry[:B8G8R8]{3 bytes; for blue, green, red. The data is opaque.}
    @entry[:R16G16B16]{3 guint16 values; for red, green, blue. Since 4.6}
    @entry[:R16G16B16A16-PREMULTIPLIED]{4 guint16 values; for red, green, blue,
      alpha. The color values are premultiplied with the alpha value.
      Since 4.6}
    @entry[:R16G16B16A16]{4 guint16 values; for red, green, blue, alpha.
      Since 4.6}
    @entry[:R16G16B16-FLOAT]{3 half-float values; for red, green, blue. The
      data is opaque. Since 4.6}
    @entry[:R16G16B16A16-FLOAT-PREMULTIPLIED]{4 half-float values; for red,
      green, blue and alpha. The color values are premultiplied with the alpha
      value. Since 4.6}
    @entry[:R16G16B16A16-FLOAT]{4 half-float values; for red, green, blue and
      alpha. Since 4.6}
    @entry[:R32G32B32-FLOAT]{No description available.}
    @entry[:R32G32B32A32-FLOAT-PREMULTIPLIED]{4 float values; for red, green,
      blue and alpha. The color values are premultiplied with the alpha value.
      Since 4.6}
    @entry[:R32G32B32A32-FLOAT]{4 float values; for red, green, blue and alpha.
      Since 4.6}
    @entry[:N-FORMATS]{The number of formats. This value will change as more
      formats get added, so do not rely on its concrete integer.}
  @end{table}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; GdkColorState
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(glib:define-gboxed-opaque color-state "GdkColorState"
  :export t
  :type-initializer "gdk_color_state_get_type"
  :alloc (error "GdkColorState cannot be created from the Lisp side"))

#+(and gtk-4-16 liber-documentation)
(setf (liber:alias-for-class 'color-state)
      "GBoxed"
      (documentation 'color-state 'type)
 "@version{2024-10-13}
  @begin{declaration}
(glib:define-gboxed-opaque color-state \"GdkColorState\"
  :export t
  :type-initializer \"gdk_color_state_get_type\"
  :alloc (error \"GdkColorState cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:color-state} instance provides the information to interpret
    colors and pixels in a variety of ways.
  @end{short}
  They are also known as color spaces.

  Crucially, GTK knows how to convert colors from one color state to another.
  The @class{gdk:color-state} instances are immutable and therefore threadsafe.

  Since 4.16
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_rec2100_linear
;;;
;;; Returns the color state object representing the linear rec2100 color space.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gdk_color_state_get_rec2100_linear" color-state-rec2100-linear)
    (g:boxed color-state :return)
)

#+gtk-4-16
(export 'color-state-rec2100-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_rec2100_pq
;;;
;;; Returns the color state object representing the rec2100-pq color space.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gdk_color_state_get_rec2100_pq" color-state-rec2100-pq)
    (g:boxed color-state :return)
)

#+gtk-4-16
(export 'color-state-rec2100-pq)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb
;;;
;;; Returns the color state object representing the sRGB color space.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gdk_color_state_get_srgb" color-state-srgb)
    (g:boxed color-state :return)
)

#+gtk-4-16
(export 'color-state-srgb)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb_linear
;;;
;;; Returns the color state object representing the linearized sRGB color space.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gdk_color_state_get_srgb_linear" color-state-srgb-linear)
    (g:boxed color-state :return)
)

#+gtk-4-16
(export 'color-state-srgb-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_create_cicp_params
;;;
;;; Create a GdkCicpParams representing the colorstate.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_equal
;;;
;;; Compares two GdkColorStates for equality.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_ref
;;;
;;; Increase the reference count of self.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_unref
;;;
;;; Decrease the reference count of self.
;;;
;;; Since 4.16
;;; ----------------------------------------------------------------------------

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
 "@version{2024-10-13}
  @begin{short}
    The @sym{gdk:texture} object is the basic element used to refer to pixel
    data.
  @end{short}
  It is primarily mean for pixel data that will not change over multiple frames,
  and will be used for a long time.

  There are various ways to create @sym{gdk:texture} objects from a
  @class{gdk-pixbuf:pixbuf} object, or a Cairo surface, or other pixel data.

  The ownership of the pixel data is transferred to the @sym{gdk:texture}
  instance; you can only make a copy of it, via the @fun{gdk:texture-download}
  function.

  The @sym{gdk:texture} object is an immutable object: That means you cannot
  change anything about it other than increasing the reference count via the
  @fun{g:object-ref} function.
  @see-constructor{gdk:texture-new-for-pixbuf}
  @see-constructor{gdk:texture-new-from-bytes}
  @see-constructor{gdk:texture-new-from-file}
  @see-constructor{gdk:texture-new-from-filename}
  @see-constructor{gdk:texture-new-from-resource}
  @see-class{gdk-pixbuf:pixbuf}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

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
 "@version{#2023-4-12}
  @syntax{(gdk:texture-height object) => height}
  @argument[object]{a @class{gdk:texture} object}
  @argument[height]{an integer with the height of the texture}
  @begin{short}
    Accessor of the @slot[gdk:texture]{height} slot of the @class{gdk:texture}
    class.
  @end{short}
  The @sym{gdk:texture-height} function returns the height of the texture , in
  pixels.
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
 "@version{#2023-4-12}
  @syntax{(gdk:texture-width object) => width}
  @argument[object]{a @class{gdk:texture} object}
  @argument[width]{an integer with the width of the texture}
  @begin{short}
    Accessor of the @slot[gdk:texture]{width} slot of the @class{gdk:texture}
    class.
  @end{short}
  The @sym{gdk:texture-width} function returns the width of the texture , in
  pixels.
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_for_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_new_for_pixbuf" texture-new-for-pixbuf)
    (g:object texture :already-referenced)
 #+liber-documentation
 "@version{#2023-4-12}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{A newly created @class{gdk:texture} object.}
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
    (g:object texture :already-referenced)
 #+liber-documentation
 "@version{#2023-4-12}
  @argument[path]{a string with the path of the resource file}
  @return{A newly created @class{gdk:texture} object.}
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
    (g:object texture :already-referenced)
  (file g:object)
  (err :pointer))

(defun texture-new-from-file (file)
 #+liber-documentation
 "@version{#2023-4-12}
  @argument[file]{a @class{g:file} object to load}
  @return{A newly created @class{gdk:texture} object, or @code{nil} if an
    error occurred.}
  @begin{short}
    Creates a new texture by loading an image from a file.
  @end{short}
  The file format is detected automatically. The supported formats are PNG and
  JPEG, though more formats might be available.
  @see-class{gdk:texture}
  @see-class{g:file}"
  (glib:with-g-error (err)
    (%texture-new-from-file file err)))

(export 'texture-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_filename
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_new_from_filename" %texture-new-from-filename)
    (g:object texture :already-referenced)
  (path :string)
  (err :pointer))

#+gtk-4-6
(defun texture-new-from-filename (path)
 #+liber-documentation
 "@version{2024-7-12}
  @argument[path]{a pathname or namestring with the file to load, the value is
    a file system path, using the OS encoding}
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
  @see-class{gdk:texture}"
  (glib:with-g-error (err)
    (%texture-new-from-filename (namestring path) err)))

#+gtk-4-6
(export 'texture-new-from-filename)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_bytes                             Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_new_from_bytes" %texture-new-from-bytes)
    (g:object texture :already-referenced)
  (bytes (g:boxed g:bytes))
  (err :pointer))

#+gtk-4-6
(defun texture-new-from-bytes (bytes)
 #+liber-documentation
 "@version{#2024-7-12}
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
  (glib:with-g-error (err)
    (%texture-new-from-bytes bytes err)))

#+gtk-4-6
(export 'texture-new-from-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_download
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_texture_download" texture-download) :void
 #+liber-documentation
 "@version{#2023-4-12}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[data]{a pointer to enough memory to be filled with the downloaded
    data of @arg{texture}}
  @argument[stride]{rowstride in bytes}
  @begin{short}
    Downloads the texture into local memory.
  @end{short}
  This may be an expensive operation, as the actual texture data may reside on
  a GPU or on a remote display server.

  The data format of the downloaded data is equivalent to the @code{:argb32}
  value of the @symbol{cairo:format-t} enumeration, so every downloaded pixel
  requires 4 bytes of memory.
  @begin[Example]{dictionary}
  Downloading a texture into a Cairo image surface:
  @begin{pre}
(let ((surface (cairo:image-surface-reate :argb32
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
 "@version{#2023-5-25}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[filename]{a string with the filename to store to}
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
;;; gdk_texture_save_to_png_bytes                          Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_png_bytes" texture-save-to-png-bytes)
    (g:boxed g:bytes)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[texture]{a @class{gdk:texture} object}
  @return{A newly allocated @class{g:bytes} instance containing PNG data.}
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
;;; gdk_texture_save_to_tiff                               Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_tiff" texture-save-to-tiff) :boolean
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[filename]{a string with the filename to store to}
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
;;; gdk_texture_save_to_tiff_bytes                         Since 4.6
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_texture_save_to_tiff_bytes" texture-save-to-tiff-bytes)
    (g:boxed g:bytes)
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[texture]{a @class{gdk:texture} object}
  @return{A newly allocated @class{g:bytes} instance containing TIFF data.}
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
  @see-function{gdk:texture-new-from-bytes}"
  (texture (g:object texture)))

#+gtk-4-6
(export 'texture-save-to-tiff-bytes)

;;; ----------------------------------------------------------------------------
;;; gdk_texture_get_format                                 Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gdk_texture_get_format" texture-format) memory-format
 #+liber-documentation
 "@version{#2023-5-25}
  @argument[texture]{a @class{gdk:texture} object}
  @return{A @symbol{gdk:memory-format} value with the preferred format for the
    data of the texture.}
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
 "@version{#2023-8-1}
  @begin{short}
    A @class{gdk:texture} implementation representing image data in memory.
  @end{short}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_memory_texture_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_memory_texture_new" memory-texture-new)
    (g:object texture :already-referenced)
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[width]{an integer with the width of the texture}
  @argument[height]{an integer with the height of the texture}
  @argument[format]{a @symbol{gdk:memory-format} value with the format of the
    data}
  @argument[bytes]{a @class{g:bytes} instance containing the pixel data}
  @argument[stride]{an integer with the rowstride of the data}
  @return{A newly created @class{gdk:texture} object.}
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

(gobject:define-gobject "GdkGLTexture" gl-texture
  (:superclass texture
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_gl_texture_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'gl-texture)
      "Class"
      (documentation 'gl-texture 'type)
 "@version{#2023-8-1}
  @begin{short}
    A @class{gdk:texture} implementation representing a GL texture object.
  @end{short}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_gl_texture_new
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is not complete. The arguments destroy and data are
;; not implemented.

(cffi:defcfun ("gdk_gl_texture_new" %gl-texture-new)
    (g:object texture :already-referenced)
  (context (g:object gl-context))
  (id :uint)
  (width :int)
  (height :int)
  (destroy :pointer)
  (data :pointer))

(defun gl-texture-new (context id width height)
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[context]{a @class{gdk:gl-context} object}
  @argument[id]{an unsigned integer with the ID of a texture that was created
    with @arg{context}}
  @argument[width]{an integer with the nominal width of the texture}
  @argument[height]{an integer with the nominal height of the texture}
  @argument[destroy]{a destroy notify that will be called when the GL resources
    are released, not implemented at this time (2023-8-1)}
  @argument[data]{data that gets passed to destroy, not implemented at this
    time (2023-8-1)}
  @return{A newly created @class{gdk:texture} object.}
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

(export 'gl-texture-new)

;;; ----------------------------------------------------------------------------
;;; gdk_gl_texture_release
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_gl_texture_release" gl-texture-release) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[texture]{a @class{gdk:gl-texture} object}
  @begin{short}
    Releases the GL resources held by a @class{gdk:gl-texture} object that was
    created with the @fun{gdk:gl-texture-new} function.
  @end{short}
  The texture contents are still available via the @fun{gdk:texture-download}
  function, after this function has been called.
  @see-class{gdk:gl-texture}"
  (texture (g:object texture)))

(export 'gl-texture-release)

;;; --- End of file gdk4.texture.lisp ------------------------------------------
