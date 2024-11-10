;;; ----------------------------------------------------------------------------
;;; gdk4.dmabuf-texture.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;;     GdkDmabufTexture
;;;     GdkDmabufTextureBuilder
;;;
;;; Accessors
;;;
;;;     gdk_dmabuf_texture_builder_get_display
;;;     gdk_dmabuf_texture_builder_set_display
;;;     gdk_dmabuf_texture_builder_get_fourcc
;;;     gdk_dmabuf_texture_builder_set_fourcc
;;;     gdk_dmabuf_texture_builder_get_height
;;;     gdk_dmabuf_texture_builder_set_height
;;;     gdk_dmabuf_texture_builder_get_modifier
;;;     gdk_dmabuf_texture_builder_set_modifier
;;;     gdk_dmabuf_texture_builder_get_n_planes
;;;     gdk_dmabuf_texture_builder_set_n_planes
;;;     gdk_dmabuf_texture_builder_get_premultiplied
;;;     gdk_dmabuf_texture_builder_set_premultiplied
;;;     gdk_dmabuf_texture_builder_get_update_region
;;;     gdk_dmabuf_texture_builder_set_update_region
;;;     gdk_dmabuf_texture_builder_get_update_texture
;;;     gdk_dmabuf_texture_builder_set_update_texture
;;;     gdk_dmabuf_texture_builder_get_width
;;;     gdk_dmabuf_texture_builder_set_width
;;;
;;; Functions
;;;
;;;     gdk_dmabuf_texture_builder_new
;;;     gdk_dmabuf_texture_builder_build
;;;     gdk_dmabuf_texture_builder_get_fd
;;;     gdk_dmabuf_texture_builder_set_fd
;;;     gdk_dmabuf_texture_builder_get_offset
;;;     gdk_dmabuf_texture_builder_set_offset
;;;     gdk_dmabuf_texture_builder_get_stride
;;;     gdk_dmabuf_texture_builder_set_stride
;;;
;;; Properties
;;;
;;;     display
;;;     fourcc
;;;     height
;;;     modifier
;;;     n-planes
;;;     premultiplied
;;;     update-region
;;;     update-texture
;;;     width
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkTexture
;;;         ╰── GdkDmabufTexture
;;;
;;;     GObject
;;;     ╰── GdkDmabufTextureBuilder
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDmabufTexture
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDmabufTexture" dmabuf-texture
  (:superclass texture
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_dmabuf_texture_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'dmabuf-texture)
      "Class"
      (documentation 'dmabuf-texture 'type)
 "@version{2024-7-11}
  @begin{short}
    The @class{gdk:texture} class representing a DMA buffer.
  @end{short}
  To create a @class{gdk:dmabuf-texture} object, use the auxiliary
  @class{gdk:dmabuf-texture-builder} object.

  Dma-buf textures can only be created on Linux.

  Since 4.14
  @see-class{gdk:texture}
  @see-class{gdk:dmabuf-texture-builder}")

;;; ----------------------------------------------------------------------------
;;; GdkDmabufTextureBuilder
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDmabufTextureBuilder" dmabuf-texture-builder
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gdk_dmabuf_texture_builder_get_type")
  ((display
    dmabuf-texture-builder-display
    "display" "GdkDisplay" t t)
   (fourcc
    dmabuf-texture-builder-fourcc
    "fourcc" "guint" t t)
   (height
    dmabuf-texture-builder-height
    "height" "guint" t t)
   (modifier
    dmabuf-texture-builder-modifier
    "modifier" "guint64" t t)
   (n-planes
    dmabuf-texture-builder-n-planes
    "n-planes" "guint" t t)
   (premultiplied
    dmabuf-texture-builder-premultiplied
    "premultiplied" "gboolean" t t)
   (update-region
    dmabuf-texture-builder-update-region
    "update-region" "CairoRegion" t t)
   (update-texture
    dmabuf-texture-builder-update-texture
    "update-texture" "GdkTexture" t t)
   (width
    dmabuf-texture-builder-width
    "width" "Guint" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'dmabuf-texture-builder)
      "Class"
      (documentation 'dmabuf-texture-builder 'type)
 "@version{2024-7-11}
  @begin{short}
    The @class{gdk:dmabuf-texture-builder} object is a builder used to construct
    @class{gdk:texture} objects from DMA buffers.
  @end{short}
  DMA buffers are commonly called dma-bufs.

  DMA buffers are a feature of the Linux kernel to enable efficient buffer and
  memory sharing between hardware such as codecs, GPUs, displays, cameras and
  the kernel drivers controlling them. For example, a decoder may want its
  output to be directly shared with the display server for rendering without a
  copy.

  Any device driver which participates in DMA buffer sharing, can do so as
  either the exporter or importer of buffers (or both).

  The memory that is shared via DMA buffers is usually stored in non-system
  memory (maybe in device’s local memory or something else not directly
  accessible by the CPU), and accessing this memory from the CPU may have
  higher-than-usual overhead.

  In particular for graphics data, it is not uncommon that data consists of
  multiple separate blocks of memory, for example one block for each of the red,
  green and blue channels. These blocks are called planes. DMA buffers can have
  up to four planes. Even if the memory is a single block, the data can be
  organized in multiple planes, by specifying offsets from the beginning of the
  data.

  DMA buffers are exposed to user-space as file descriptors allowing to pass
  them between processes. If a DMA buffer has multiple planes, there is one file
  descriptor per plane.

  The format of the data (for graphics data, essentially its colorspace) is
  described by a 32-bit integer. These format identifiers are defined in the
  header file @file{drm_fourcc.h} and commonly referred to as @code{fourcc}
  values, since they are identified by 4 ASCII characters. Additionally, each
  DMA buffer has a @code{modifier}, which is a 64-bit integer that describes
  driver-specific details of the memory layout, such as tiling or compression.

  For historical reasons, some producers of dma-bufs do not provide an explicit
  modifier, but instead return @code{DMA_FORMAT_MOD_INVALID} to indicate that
  their modifier is implicit. GTK tries to accommodate this situation by
  accepting @code{DMA_FORMAT_MOD_INVALID} as modifier.

  The operation of the @class{gdk:dmabuf-texture-builder} object is quite
  simple: Create a texture builder, set all the necessary properties, and then
  call the @fun{gdk:dmabuf-texture-builder-build} object to create the new
  texture.

  The required properties for a dma-buf texture are
  @begin{itemize}
    @item{The width and height in pixels.}
    @item{The @code{fourcc} code and @code{modifier} which identify the format
      and memory layout of the dma-buf.}
    @item{The file descriptor, offset and stride for each of the planes.}
  @end{itemize}
  GdkDmabufTextureBuilder can be used for quick one-shot construction of
  textures as well as kept around and reused to construct multiple textures.

  For further information, see the Linux kernel
  @url[https://docs.kernel.org/driver-api/dma-buf.html]{Linux kernel documentation}
  and the @file{drm_fourcc.h} header file.

  Since 4.14
  @see-class{gdk:texture}
  @see-class{gdk:dmabuf-texture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:dmabuf-texture-builder-display -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display"
                                               'dmabuf-texture-builder) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The display that this texture will be used on.")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-display)
      "Accessor"
      (documentation 'dmabuf-texture-builder-display 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-display object) => display}
  @syntax{(setf (gdk:dmabuf-texture-builder-display object) display)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{display} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-display} function returns the display that
  this texture builder is associated with. The
  @setf{gdk:dmabuf-texture-builder-display} function sets the display. The
  display is used to determine the supported dma-buf formats.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}
  @see-class{gdk:display}")

;;; --- gdk:dmabuf-texture-builder-fourcc --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fourcc"
                                               'dmabuf-texture-builder) t)
 "The @code{fourcc} property of type @code{guint} (Read / Write) @br{}
  The format of the texture, as a @code{fourcc} value.")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-fourcc)
      "Accessor"
      (documentation 'dmabuf-texture-builder-fourcc 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-fourcc object) => fourcc}
  @syntax{(setf (gdk:dmabuf-texture-builder-fourcc object) fourcc)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[fourcc]{an integer with the format of the texture or 0 to unset}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{fourcc} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-fourcc} function gets the format
  previously set or 0 if the format was not set. The
  @fun{gdk:dmabuf-texture-builder-fourcc} function sets the format of the
  texture. The format is specified as a fourcc code.

  The format must be set before calling the @fun{gdk:gl-texture-builder-build}
  function.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}
  @see-function{gdk:dmabuf-texture-builder-build}")

;;; --- gdk:dmabuf-texture-builder-height --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height"
                                               'dmabuf-texture-builder) t)
 "The @code{height} property of type @code{guint} (Read / Write) @br{}
  The height of the texture. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-height)
      "Accessor"
      (documentation 'dmabuf-texture-builder-height 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-height object) => height}
  @syntax{(setf (gdk:dmabuf-texture-builder-height object) height)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[height]{an integer with the height of the texture or 0 to unset}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{height} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-height} function gets the height
  previously set or 0 if the height was not set. The
  @setf{gdk:dmabuf-texture-builder-height} function sets the height of the
  texture.

  The height must be set before calling the @fun{gdk:gl-texture-builder-build}
  function.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

;;; --- gdk:dmabuf-texture-builder-modifier ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modifier"
                                               'dmabuf-texture-builder) t)
 "The @code{modifier} property of type @code{guint64} (Read / Write) @br{}
  The modifier. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-modifier)
      "Accessor"
      (documentation 'dmabuf-texture-builder-modifier 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-modifier object) => modifier}
  @syntax{(setf (gdk:dmabuf-texture-builder-modifier object) modifier)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[modifier]{an integer with the modifier value}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{height} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-modifier} function gets the modifier
  value. The @setf{gdk:dmabuf-texture-builder-modifier} function sets the
  modifier.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

;;; --- gdk:dmabuf-texture-builder-n-planes ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-planes"
                                               'dmabuf-texture-builder) t)
 "The @code{n-planes} property of type @code{guint} (Read / Write) @br{}
  The number of planes of the texture. Note that you can set properties for
  other planes, but they will be ignored when constructing the texture.")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-n-planes)
      "Accessor"
      (documentation 'dmabuf-texture-builder-n-planes 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-n-planes object) => n-planes}
  @syntax{(setf (gdk:dmabuf-texture-builder-n-planes object) n-planes)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[n-planes]{an integer with the number of planes}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{n-planes} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-n-planes} function gets the number of
  planes. The @setf{gdk:dmabuf-texture-builder-n-planes} function sets the
  number of planes.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

;;; --- gdk:dmabuf-texture-builder-premultiplied -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "premultiplied"
                                               'dmabuf-texture-builder) t)
 "The @code{premultiplied} property of type @code{:boolean} (Read / Write) @br{}
  Whether the alpha channel is premultiplied into the others. Only relevant if
  the format has alpha. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-premultiplied)
      "Accessor"
      (documentation 'dmabuf-texture-builder-premultiplied 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-premultiplied object) => premultiplied}
  @syntax{(setf (gdk:dmabuf-texture-builder-premultiplied object) premultiplied)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[premultiplied]{a boolean whether the data is premultiplied}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{premultiplied} slot of
    the @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-premultiplied} function gets whether the
  data is premultiplied. The @setf{gdk:dmabuf-texture-builder-premultiplied}
  function sets whether the data is premultiplied.

  Unless otherwise specified, all formats including alpha channels are assumed
  to be premultiplied.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

;;; --- gdk:dmabuf-texture-builder-update-region -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "update-region"
                                               'dmabuf-texture-builder) t)
 "The @code{update-region} property of type @symbol{cairo:region-t}
  (Read / Write) @br{}
  The update region of @slot[gdk:dmabuf-texture-builder]{update-texture}
  value.")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-update-region)
      "Accessor"
      (documentation 'dmabuf-texture-builder-update-region 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-update-region object) => region}
  @syntax{(setf (gdk:dmabuf-texture-builder-update-region object) region)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[region]{a @symbol{cairo:region-t} instance}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{update-region} slot of
    the @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-update-region} function gets the region
  previously set or @code{nil} if none was set. The
  @setf{gdk:dmabuf-texture-builder-update-region} function sets the region to
  be updated by this texture. Together with the
  @slot[gdk:dmabuf-texture-builder]{update-texture} property this describes an
  update of a previous texture.

  When rendering animations of large textures, it is possible that consecutive
  textures are only updating contents in parts of the texture. It is then
  possible to describe this update via these two properties, so that GTK can
  avoid rerendering parts that did not change. An example would be a screen
  recording where only the mouse pointer moves.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}
  @see-symbol{cairo:region-t}
  @see-function{gdk:dmabuf-texture-builder-update-texture}")

;;; --- gdk:dmabuf-texture-builder-update-texture ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "update-texture"
                                               'dmabuf-texture-builder) t)
 "The @code{update-texture} property of type @class{gdk:texture} (Read / Write)
  @br{}
  The texture the @slot[gdk:dmabuf-texture-builder]{update-region} value is
  an update for.")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-update-texture)
      "Accessor"
      (documentation 'dmabuf-texture-builder-update-texture 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-update-texture object) => texture}
  @syntax{(setf (gdk:dmabuf-texture-builder-update-region object) texture)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[texture]{a @class{gdk:texture} object}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{update-texture} slot of
    the @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-update-texture} function gets the texture
  previously set or @code{nil} if none was set. The
  @setf{gdk:dmabuf-texture-builder-update-texture} function sets the texture to
  be updated by this texture. See the
  @fun{gdk:dmabuf-texture-builder-update-region} function for an explanation.
  @see-class{gdk:dmabuf-texture-builder}
  @see-function{gdk:dmabuf-texture-builder-update-region}")

;;; --- gdk:dmabuf-texture-builder-width ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width"
                                               'dmabuf-texture-builder) t)
 "The @code{width} property of type @code{:uint} (Read / Write) @br{}
  The width of the texture. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'dmabuf-texture-builder-width)
      "Accessor"
      (documentation 'dmabuf-texture-builder-width 'function)
 "@version{2024-7-11}
  @syntax{(gdk:dmabuf-texture-builder-width object) => width}
  @syntax{(setf (gdk:dmabuf-texture-builder-width object) width)}
  @argument[object]{a @class{gdk:dmabuf-texture-builder} object}
  @argument[width]{an integer with the width of the texture or 0 to unset}
  @begin{short}
    Accessor of the @slot[gdk:dmabuf-texture-builder]{width} slot of the
    @class{gdk:dmabuf-texture-builder} class.
  @end{short}
  The @fun{gdk:dmabuf-texture-builder-width} function gets the width
  previously set or 0 if the width was not set. The
  @setf{gdk:dmabuf-texture-builder-width} function sets the width of the
  texture.

  The width must be set before calling the @fun{gdk:gl-texture-builder-build}
  function.

  Since 4.14
  @see-class{gdk:dmabuf-texture-builder}")

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_texture_builder_new
;;;
;;; Creates a new texture builder.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

(declaim (inline dmabuf-texture-builder-new))

(defun dmabuf-texture-builder-new ()
  (make-instance 'dmabuf-texture-builder))

(export 'dmabuf-texture-builder-new)

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_texture_builder_build
;;;
;;; Builds a new GdkTexture with the values set up in the builder.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_texture_builder_get_fd
;;; gdk_dmabuf_texture_builder_set_fd
;;;
;;; Gets the file descriptor for a plane.
;;; Sets the file descriptor for a plane.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_texture_builder_get_offset
;;; gdk_dmabuf_texture_builder_set_offset
;;;
;;; Gets the offset value for a plane.
;;; Sets the offset for a plane.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_dmabuf_texture_builder_get_stride
;;; gdk_dmabuf_texture_builder_set_stride
;;;
;;; Gets the stride value for a plane.
;;; Sets the stride for a plane.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.dmabuf-texture.lisp -----------------------------------
