;;; ----------------------------------------------------------------------------
;;; gdk4.texture.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GdkTexture
;;;
;;;     Pixel data
;;;
;;; Types and Values
;;;
;;;     GdkTexture
;;;     GdkMemoryTexture
;;;     GdkGLTexture
;;;     GdkMemoryFormat
;;;
;;;     GDK_MEMORY_DEFAULT
;;;
;;; Functions
;;;
;;;     gdk_texture_new_for_pixbuf
;;;     gdk_texture_new_from_resource
;;;     gdk_texture_new_from_file
;;;     gdk_texture_new_from_filename                      Since 4.6
;;;     gdk_texture_get_width
;;;     gdk_texture_get_height
;;;     gdk_texture_download
;;;     gdk_texture_save_to_png
;;;     gdk_memory_texture_new
;;;     gdk_gl_texture_new
;;;     gdk_gl_texture_release
;;;
;;; Properties
;;;
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
;;; GdkTexture
;;;
;;; GdkTexture is the basic element used to refer to pixel data. It is primarily
;;; mean for pixel data that will not change over multiple frames, and will be
;;; used for a long time.
;;;
;;; There are various ways to create GdkTexture objects from a GdkPixbuf, or a
;;; Cairo surface, or other pixel data.
;;;
;;; The ownership of the pixel data is transferred to the GdkTexture instance;
;;; you can only make a copy of it, via gdk_texture_download().
;;;
;;; GdkTexture is an immutable object: That means you cannot change anything
;;; about it other than increasing the reference count via g_object_ref().
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkTexture" texture
  (:superclass g:object
   :export t
   :interfaces ("GdkPaintable"
                "GIcon"
                "GLoadableIcon")
   :type-initializer "gdk_texture_get_type")
  ((height
    texture-height
    "height" "gint" t t)
   (width
    texture-width
    "width" "gint" t t)))

;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_for_pixbuf ()
;;;
;;; GdkTexture *
;;; gdk_texture_new_for_pixbuf (GdkPixbuf *pixbuf);
;;;
;;; Creates a new texture object representing the GdkPixbuf.
;;;
;;; pixbuf :
;;;     a GdkPixbuf
;;;
;;;  Returns :
;;;     a new GdkTexture
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_texture_new_for_pixbuf" gdk-texture-new-for-pixbuf)
    (g:object texture)
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'gdk-texture-new-for-pixbuf)

;;; ----------------------------------------------------------------------------
;;;gdk_texture_new_from_resource ()
;;;GdkTexture *
;;;gdk_texture_new_from_resource (const char *resource_path);
;;;Creates a new texture by loading an image from a resource. The file format is detected automatically. The supported formats are PNG and JPEG, though more formats might be available.

;;;It is a fatal error if resource_path does not specify a valid image resource and the program will abort if that happens. If you are unsure about the validity of a resource, use gdk_texture_new_from_file() to load it.

;;;Parameters
;;;resource_path

;;;the path of the resource file

;;;
;;;Returns
;;;A newly-created texture


;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_file ()
;;;
;;; GdkTexture *
;;; gdk_texture_new_from_file (GFile *file,
;;;                            GError **error);
;;;
;;; Creates a new texture by loading an image from a file. The file format is
;;; detected automatically. The supported formats are PNG and JPEG, though more
;;; formats might be available.
;;;
;;; If NULL is returned, then error will be set.
;;;
;;; file :
;;;     GFile to load
;;;
;;; error :
;;;     Return location for an error
;;;
;;; Returns :
;;;     A newly-created GdkTexture or NULL if an error occurred.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_texture_new_from_filename ()
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(defcfun ("gdk_texture_new_from_filename" %texture-new-from-filename)
    (g:object texture)
  (path :string)
  (err :pointer))

#+gtk-4-6
(defun texture-new-from-filename (path)
 #+liber-documentation
 "@version{2023-1-29}
  @argument[path]{a pathname or namestring with the file to load, the value is
    a file system path, using the OS encoding}
  @return{A newly-created @class{gdk:texture} object.}
  @begin{short}
    Creates a new texture by loading an image from a file.
  @end{short}
  The file format is detected automatically. The supported formats are PNG and
  JPEG, though more formats might be available.

  This function is threadsafe, so that you can e.g. use @code{GTask} and the
  @code{g_task_run_in_thread()} function to avoid blocking the main thread
  while loading a big image.

  Since 4.6
  @see-class{gdk:texture}"
  (with-g-error (err)
    (%texture-new-from-filename (namestring path) err)))

#+gtk-4-6
(export 'texture-new-from-filename)

;;; ----------------------------------------------------------------------------
;;;gdk_texture_get_width ()
;;;int
;;;gdk_texture_get_width (GdkTexture *texture);
;;;Returns the width of texture , in pixels.

;;;Parameters
;;;texture

;;;a GdkTexture

;;;
;;;Returns
;;;the width of the GdkTexture

;;;gdk_texture_get_height ()
;;;int
;;;gdk_texture_get_height (GdkTexture *texture);
;;;Returns the height of the texture , in pixels.

;;;Parameters
;;;texture

;;;a GdkTexture

;;;
;;;Returns
;;;the height of the GdkTexture

;;;gdk_texture_download ()
;;;void
;;;gdk_texture_download (GdkTexture *texture,
;;;                      guchar *data,
;;;                      gsize stride);
;;;Downloads the texture into local memory. This may be an expensive operation, as the actual texture data may reside on a GPU or on a remote display server.

;;;The data format of the downloaded data is equivalent to CAIRO_FORMAT_ARGB32, so every downloaded pixel requires 4 bytes of memory.

;;;Downloading a texture into a Cairo image surface:

;;;surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
;;;                                      gdk_texture_get_width (texture),
;;;                                      gdk_texture_get_height (texture));
;;;gdk_texture_download (texture,
;;;                      cairo_image_surface_get_data (surface),
;;;                      cairo_image_surface_get_stride (surface));
;;;cairo_surface_mark_dirty (surface);
;;;Parameters
;;;texture

;;;a GdkTexture

;;;
;;;data

;;;pointer to enough memory to be filled with the downloaded data of texture .

;;;[array]
;;;stride

;;;rowstride in bytes

;;;
;;;gdk_texture_save_to_png ()
;;;gboolean
;;;gdk_texture_save_to_png (GdkTexture *texture,
;;;                         const char *filename);
;;;Store the given texture to the filename as a PNG file.

;;;This is a utility function intended for debugging and testing. If you want more control over formats, proper error handling or want to store to a GFile or other location, you might want to look into using the gdk-pixbuf library.

;;;Parameters
;;;texture

;;;a GdkTexture

;;;
;;;filename

;;;the filename to store to

;;;
;;;Returns
;;;TRUE if saving succeeded, FALSE on failure.

;;;gdk_memory_texture_new ()
;;;GdkTexture *
;;;gdk_memory_texture_new (int width,
;;;                        int height,
;;;                        GdkMemoryFormat format,
;;;                        GBytes *bytes,
;;;                        gsize stride);
;;;Creates a new texture for a blob of image data. The GBytes must contain stride x height pixels in the given format.

;;;Parameters
;;;width

;;;the width of the texture

;;;
;;;height

;;;the height of the texture

;;;
;;;format

;;;the format of the data

;;;
;;;bytes

;;;the GBytes containing the pixel data

;;;
;;;stride

;;;rowstride for the data

;;;
;;;Returns
;;;A newly-created GdkTexture

;;;gdk_gl_texture_new ()
;;;GdkTexture *
;;;gdk_gl_texture_new (GdkGLContext *context,
;;;                    guint id,
;;;                    int width,
;;;                    int height,
;;;                    GDestroyNotify destroy,
;;;                    gpointer data);
;;;Creates a new texture for an existing GL texture.

;;;Note that the GL texture must not be modified until destroy is called, which will happen when the GdkTexture object is finalized, or due to an explicit call of gdk_gl_texture_release().

;;;Parameters
;;;context

;;;a GdkGLContext

;;;
;;;id

;;;the ID of a texture that was created with context

;;;
;;;width

;;;the nominal width of the texture

;;;
;;;height

;;;the nominal height of the texture

;;;
;;;destroy

;;;a destroy notify that will be called when the GL resources are released

;;;
;;;data

;;;data that gets passed to destroy

;;;
;;;Returns
;;;A newly-created GdkTexture.

;;;[transfer full]

;;;gdk_gl_texture_release ()
;;;void
;;;gdk_gl_texture_release (GdkGLTexture *self);
;;;Releases the GL resources held by a GdkGLTexture that was created with gdk_gl_texture_new().

;;;The texture contents are still available via the gdk_texture_download() function, after this function has been called.

;;;Parameters
;;;self

;;;a GdkTexture wrapping a GL texture

;;;
;;;Types and Values
;;;GdkTexture
;;;typedef struct _GdkTexture GdkTexture;
;;;The GdkTexture structure contains only private data.

;;;GdkMemoryTexture
;;;typedef struct _GdkMemoryTexture GdkMemoryTexture;
;;;A GdkTexture representing image data in memory.

;;;GdkGLTexture
;;;typedef struct _GdkGLTexture GdkGLTexture;
;;;A GdkTexture representing a GL texture object.

;;;enum GdkMemoryFormat
;;;GdkMemoryFormat describes a format that bytes can have in memory.

;;;It describes formats by listing the contents of the memory passed to it. So GDK_MEMORY_A8R8G8B8 will be 1 byte (8 bits) of alpha, followed by a byte each of red, green and blue. It is not endian-dependent, so CAIRO_FORMAT_ARGB32 is represented by different GdkMemoryFormats on architectures with different endiannesses.

;;;Its naming is modelled after VkFormat (see https://www.khronos.org/registry/vulkan/specs/1.0/html/vkspec.htmlVkFormat for details).

;;;Members
;;;GDK_MEMORY_B8G8R8A8_PREMULTIPLIED

;;;4 bytes; for blue, green, red, alpha. The color values are premultiplied with the alpha value.

;;;
;;;GDK_MEMORY_A8R8G8B8_PREMULTIPLIED

;;;4 bytes; for alpha, red, green, blue. The color values are premultiplied with the alpha value.

;;;
;;;GDK_MEMORY_R8G8B8A8_PREMULTIPLIED

;;;4 bytes; for red, green, blue, alpha The color values are premultiplied with the alpha value.

;;;
;;;GDK_MEMORY_B8G8R8A8

;;;4 bytes; for blue, green, red, alpha.

;;;
;;;GDK_MEMORY_A8R8G8B8

;;;4 bytes; for alpha, red, green, blue.

;;;
;;;GDK_MEMORY_R8G8B8A8

;;;4 bytes; for red, green, blue, alpha.

;;;
;;;GDK_MEMORY_A8B8G8R8

;;;4 bytes; for alpha, blue, green, red.

;;;
;;;GDK_MEMORY_R8G8B8

;;;3 bytes; for red, green, blue. The data is opaque.

;;;
;;;GDK_MEMORY_B8G8R8

;;;3 bytes; for blue, green, red. The data is opaque.

;;;
;;;GDK_MEMORY_N_FORMATS

;;;The number of formats. This value will change as more formats get added, so do not rely on its concrete integer.

;;;
;;;GDK_MEMORY_DEFAULT
;;;#define GDK_MEMORY_DEFAULT GDK_MEMORY_B8G8R8A8_PREMULTIPLIED
;;;This is the default memory format used by GTK and is the format provided by gdk_texture_download(). It is equal to CAIRO_FORMAT_ARGB32.

;;;Be aware that unlike the GdkMemoryFormat values, this format is different for different endianness.

;;;Property Details
;;;The “height” property
;;;  “height”                   int
;;;The height of the texture, in pixels.

;;;Owner: GdkTexture

;;;Flags: Read / Write / Construct Only

;;;Allowed values: >= 1

;;;Default value: 1

;;;The “width” property
;;;  “width”                    int
;;;The width of the texture, in pixels.

;;;Owner: GdkTexture

;;;Flags: Read / Write / Construct Only

;;;Allowed values: >= 1

;;;Default value: 1


;;; --- End of file gdk4.texture.lisp ------------------------------------------
