;;; ----------------------------------------------------------------------------
;;; gdk.pixbuf-interaction.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

;;;GdkPixbuf Interaction
;;;GdkPixbuf Interaction — Functions for obtaining pixbufs

;;;Functions
;;;GdkPixbuf *	gdk_pixbuf_get_from_surface ()
;;;GdkPixbuf *	gdk_pixbuf_get_from_texture ()

(in-package :gdk)

;;;Description
;;;Pixbufs are client-side images. For details on how to create and manipulate pixbufs, see the GdkPixbuf API documentation.

;;;The functions described here allow to obtain pixbufs from GdkSurfaces and cairo surfaces.

;;;Functions
;;;gdk_pixbuf_get_from_surface ()
;;;GdkPixbuf *
;;;gdk_pixbuf_get_from_surface (cairo_surface_t *surface,
;;;                             int src_x,
;;;                             int src_y,
;;;                             int width,
;;;                             int height);
;;;Transfers image data from a cairo_surface_t and converts it to an RGB(A) representation inside a GdkPixbuf. This allows you to efficiently read individual pixels from cairo surfaces.

;;;This function will create an RGB pixbuf with 8 bits per channel. The pixbuf will contain an alpha channel if the surface contains one.

;;;Parameters
;;;surface

;;;surface to copy from

;;;
;;;src_x

;;;Source X coordinate within surface

;;;
;;;src_y

;;;Source Y coordinate within surface

;;;
;;;width

;;;Width in pixels of region to get

;;;
;;;height

;;;Height in pixels of region to get

;;;
;;;Returns
;;;A newly-created pixbuf with a reference count of 1, or NULL on error.

;;;[nullable][transfer full]

;;;gdk_pixbuf_get_from_texture ()
;;;GdkPixbuf *
;;;gdk_pixbuf_get_from_texture (GdkTexture *texture);
;;;Creates a new pixbuf from texture . This should generally not be used in newly written code as later stages will almost certainly convert the pixbuf back into a texture to draw it on screen.

;;;Parameters
;;;texture

;;;a GdkTexture

;;;
;;;Returns
;;;a new GdkPixbuf or NULL in case of an error.

;;;[transfer full][nullable]

;;; --- End of file gdk.pixbuf-interaction.lisp --------------------------------
