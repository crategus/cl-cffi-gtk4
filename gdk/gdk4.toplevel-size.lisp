;;; ----------------------------------------------------------------------------
;;; gdk.toplevel-size.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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
;;;
;;; GdkToplevelSize
;;;
;;;     Information for computing toplevel size
;;;
;;; Types and Values
;;;
;;;     GdkToplevelSize
;;;
;;; Functions
;;;
;;;     gdk_toplevel_size_get_bounds
;;;     gdk_toplevel_size_set_size
;;;     gdk_toplevel_size_set_min_size
;;;     gdk_toplevel_size_set_shadow_width
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;;Description
;;;The GdkToplevelSIze struct contains information that may be useful for users of GdkToplevel to compute a surface size. It also carries information back with the computational result.

;;;Functions
;;;gdk_toplevel_size_get_bounds ()
;;;void
;;;gdk_toplevel_size_get_bounds (GdkToplevelSize *size,
;;;                              int *bounds_width,
;;;                              int *bounds_height);
;;;Retrieves the bounds the toplevel is placed within.

;;;The bounds represent the largest size a toplevel may have while still being able to fit within some type of boundary. Depending on the backend, this may be equivalent to the dimensions of the work area or the monitor on which the window is being presented on, or something else that limits the way a toplevel can be presented.

;;;Parameters
;;;size

;;;a GdkToplevelSize

;;;
;;;bounds_width

;;;return location for width.

;;;[out]
;;;bounds_height

;;;return location for height.

;;;[out]
;;;gdk_toplevel_size_set_size ()
;;;void
;;;gdk_toplevel_size_set_size (GdkToplevelSize *size,
;;;                            int width,
;;;                            int height);
;;;Sets the size the toplevel prefers to be resized to. The size should be within the bounds (see gdk_toplevel_size_get_bounds()). The set size should be considered as a hint, and should not be assumed to be respected by the windowing system, or backend.

;;;Parameters
;;;size

;;;a GdkToplevelSize

;;;
;;;width

;;;the width

;;;
;;;height

;;;the height

;;;
;;;gdk_toplevel_size_set_min_size ()
;;;void
;;;gdk_toplevel_size_set_min_size (GdkToplevelSize *size,
;;;                                int min_width,
;;;                                int min_height);
;;;The minimum size corresponds to the limitations the toplevel can be shrunk to, without resulting in incorrect painting. A user of a GdkToplevel should calculate these given both the existing size, and the bounds retrieved from the GdkToplevelSize object.

;;;The minimum size should be within the bounds (see gdk_toplevel_size_get_bounds()).

;;;Parameters
;;;size

;;;a GdkToplevelSize

;;;
;;;min_width

;;;the minimum width

;;;
;;;min_height

;;;the minimum height

;;;
;;;gdk_toplevel_size_set_shadow_width ()
;;;void
;;;gdk_toplevel_size_set_shadow_width (GdkToplevelSize *size,
;;;                                    int left,
;;;                                    int right,
;;;                                    int top,
;;;                                    int bottom);
;;;The shadow width corresponds to the part of the computed surface size that would consist of the shadow margin surrounding the window, would there be any.

;;;Parameters
;;;size

;;;a GdkToplevelSize

;;;
;;;left

;;;width of the left part of the shadow

;;;
;;;right

;;;width of the right part of the shadow

;;;
;;;top

;;;height of the top part of the shadow

;;;
;;;bottom

;;;height of the bottom part of the shadow

;;;
;;;Types and Values
;;;GdkToplevelSize
;;;typedef struct _GdkToplevelSize GdkToplevelSize;
;;;Struct containing information for computing the size of a GdkToplevel.

;;; --- End of file gdk.toplevel-size.lisp -------------------------------------
