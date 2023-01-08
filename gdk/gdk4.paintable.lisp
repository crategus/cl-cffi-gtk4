;;; ----------------------------------------------------------------------------
;;; gdk.paintable.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
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
;;;
;;; GdkPaintable
;;;
;;;     An interface for a paintable region
;;;
;;; Types and Values
;;;
;;;     GdkPaintable
;;;     GdkPaintableInterface
;;;     GdkPaintableFlags
;;;     GdkSnapshot
;;;
;;; Functions
;;;
;;;     gdk_paintable_get_current_image
;;;     gdk_paintable_snapshot
;;;     gdk_paintable_get_flags
;;;     gdk_paintable_get_intrinsic_width
;;;     gdk_paintable_get_intrinsic_height
;;;     gdk_paintable_get_intrinsic_aspect_ratio
;;;     gdk_paintable_compute_concrete_size
;;;     gdk_paintable_invalidate_contents
;;;     gdk_paintable_invalidate_size
;;;     gdk_paintable_new_empty
;;;
;;; Signals
;;;
;;;     invalidate-contents
;;;     invalidate-size
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkPaintable
;;;
;;;     GObject
;;;     ╰── GdkSnapshot
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkPaintableFlags
;;;
;;; Flags about this object. Implementations use these for optimizations such
;;; as caching.
;;;
;;; GDK_PAINTABLE_STATIC_SIZE :
;;;     The size is immutable. The “invalidate-size” signal will never be
;;;     emitted.
;;;
;;; GDK_PAINTABLE_STATIC_CONTENTS :
;;;     The content is immutable. The “invalidate-contents” signal will never
;;;     be emitted.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GdkPaintable
;;;
;;;Description
;;;GdkPaintable is a simple interface used by GDK and GTK to represent objects that can be painted anywhere at any size without requiring any sort of layout. The interface is inspired by similar concepts elsewhere, such as ClutterContent, HTML/CSS Paint Sources, or SVG Paint Servers.

;;;A GdkPaintable can be snapshot at any time and size using gdk_paintable_snapshot(). How the paintable interprets that size and if it scales or centers itself into the given rectangle is implementation defined, though if you are implementing a GdkPaintable and don't know what to do, it is suggested that you scale your paintable ignoring any potential aspect ratio.

;;;The contents that a GdkPaintable produces may depend on the GdkSnapshot passed to it. For example, paintables may decide to use more detailed images on higher resolution screens or when OpenGL is available. A GdkPaintable will however always produce the same output for the same snapshot.

;;;A GdkPaintable may change its contents, meaning that it will now produce a different output with the same snapshot. Once that happens, it will call gdk_paintable_invalidate_contents() which will emit the “invalidate-contents” signal. If a paintable is known to never change its contents, it will set the GDK_PAINTABLE_STATIC_CONTENTS flag. If a consumer cannot deal with changing contents, it may call gdk_paintable_get_current_image() which will return a static paintable and use that.

;;;A paintable can report an intrinsic (or preferred) size or aspect ratio it wishes to be rendered at, though it doesn't have to. Consumers of the interface can use this information to layout thepaintable appropriately. Just like the contents, the size of a paintable can change. A paintable will indicate this by calling gdk_paintable_invalidate_size() which will emit the “invalidate-size” signal. And just like for contents, if a paintable is known to never change its size, it will set the GDK_PAINTABLE_STATIC_SIZE flag.

;;;Besides API for applications, there are some functions that are only useful for implementing subclasses and should not be used by applications: gdk_paintable_invalidate_contents(), gdk_paintable_invalidate_size(), gdk_paintable_new_empty().


;;;Signal Details
;;;The “invalidate-contents” signal
;;;void
;;;user_function (GdkPaintable *paintable,
;;;               gpointer      user_data)
;;;Emitted when the contents of the paintable change.

;;;Examples for such an event would be videos changing to the next frame or the icon theme for an icon changing.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;user_data

;;;user data set when the signal handler was connected.


;;;Flags: Run Last

;;;The “invalidate-size” signal
;;;void
;;;user_function (GdkPaintable *paintable,
;;;               gpointer      user_data)
;;;Emitted when the intrinsic size of the paintable changes. This means the values reported by at least one of gdk_paintable_get_intrinsic_width(), gdk_paintable_get_intrinsic_height() or gdk_paintable_get_intrinsic_aspect_ratio() has changed.

;;;Examples for such an event would be a paintable displaying the contents of a toplevel surface being resized.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;user_data

;;;user data set when the signal handler was connected.


;;;Flags: Run Last

;;;See Also
;;;ClutterContent, GtkImage, GdkTexture, GtkSnapshot
;;; ----------------------------------------------------------------------------

(define-g-interface "GdkPaintable" paintable
  (:export t
   :type-initializer "gdk_paintable_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkSnapshot
;;;
;;; Base type for snapshot operations.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkSnapshot" snapshot
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_snapshot_get_type")
  nil)

#+liber-documentation
(setf (documentation 'snapshot 'type)
 "@version{#2022-7-10}
  @begin{short}
    Base type for snapshot operations.
  @end{short}
  The subclass of the @sym{gdk:snapshot} class used by GTK is the
  @class{gtk:snapshot} class.
  @see-class{gtk:snapshot}")


;;; ----------------------------------------------------------------------------
;;;gdk_paintable_get_current_image ()
;;;GdkPaintable *
;;;gdk_paintable_get_current_image (GdkPaintable *paintable);
;;;Gets an immutable paintable for the current contents displayed by paintable .

;;;This is useful when you want to retain the current state of an animation, for example to take a screenshot of a running animation.

;;;If the paintable is already immutable, it will return itself.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;Returns
;;;An immutable paintable for the current contents of paintable .

;;;[transfer full]

;;;gdk_paintable_snapshot ()
;;;void
;;;gdk_paintable_snapshot (GdkPaintable *paintable,
;;;                        GdkSnapshot *snapshot,
;;;                        double width,
;;;                        double height);
;;;Snapshots the given paintable with the given width and height at the current (0,0) offset of the snapshot . If width and height are not larger than zero, this function will do nothing.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;snapshot

;;;a GdkSnapshot to snapshot to


;;;width

;;;width to snapshot in


;;;height

;;;height to snapshot in


;;;gdk_paintable_get_flags ()
;;;GdkPaintableFlags
;;;gdk_paintable_get_flags (GdkPaintable *paintable);
;;;Get flags for the paintable. This is oftentimes useful for optimizations.

;;;See GdkPaintableFlags for the flags and what they mean.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;Returns
;;;The GdkPaintableFlags for this paintable.

;;;gdk_paintable_get_intrinsic_width ()
;;;int
;;;gdk_paintable_get_intrinsic_width (GdkPaintable *paintable);
;;;Gets the preferred width the paintable would like to be displayed at. Consumers of this interface can use this to reserve enough space to draw the paintable.

;;;This is a purely informational value and does not in any way limit the values that may be passed to gdk_paintable_snapshot().

;;;If the paintable does not have a preferred width, it returns 0. Negative values are never returned.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;Returns
;;;the intrinsic width of paintable or 0 if none.

;;;gdk_paintable_get_intrinsic_height ()
;;;int
;;;gdk_paintable_get_intrinsic_height (GdkPaintable *paintable);
;;;Gets the preferred height the paintable would like to be displayed at. Consumers of this interface can use this to reserve enough space to draw the paintable.

;;;This is a purely informational value and does not in any way limit the values that may be passed to gdk_paintable_snapshot().

;;;If the paintable does not have a preferred height, it returns 0. Negative values are never returned.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;Returns
;;;the intrinsic height of paintable or 0 if none.

;;;gdk_paintable_get_intrinsic_aspect_ratio ()
;;;double
;;;gdk_paintable_get_intrinsic_aspect_ratio
;;;                               (GdkPaintable *paintable);
;;;Gets the preferred aspect ratio the paintable would like to be displayed at. The aspect ratio is the width divided by the height, so a value of 0.5 means that the paintable prefers to be displayed twice as high as it is wide. Consumers of this interface can use this to preserve aspect ratio when displaying the paintable.

;;;This is a purely informational value and does not in any way limit the values that may be passed to gdk_paintable_snapshot().

;;;Usually when a paintable returns nonzero values from gdk_paintable_get_intrinsic_width() and gdk_paintable_get_intrinsic_height() the aspect ratio should conform to those values, though that is not required.

;;;If the paintable does not have a preferred aspect ratio, it returns 0. Negative values are never returned.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;Returns
;;;the intrinsic aspect ratio of paintable or 0 if none.

;;;gdk_paintable_compute_concrete_size ()
;;;void
;;;gdk_paintable_compute_concrete_size (GdkPaintable *paintable,
;;;                                     double specified_width,
;;;                                     double specified_height,
;;;                                     double default_width,
;;;                                     double default_height,
;;;                                     double *concrete_width,
;;;                                     double *concrete_height);
;;;Applies the sizing algorithm outlined in https://drafts.csswg.org/css-images-3/default-sizing to the given paintable . See that link for more details.

;;;It is not necessary to call this function when both specified_width and specified_height are known, but it is useful to call this function in GtkWidget:measure implementations to compute the other dimension when only one dimension is given.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;specified_width

;;;the width paintable could be drawn into or 0.0 if unknown


;;;specified_height

;;;the height paintable could be drawn into or 0.0 if unknown


;;;default_width

;;;the width paintable would be drawn into if no other constraints were given


;;;default_height

;;;the height paintable would be drawn into if no other constraints were given


;;;concrete_width

;;;will be set to the concrete width computed.

;;;[out]
;;;concrete_height

;;;will be set to the concrete height computed.

;;;[out]
;;;gdk_paintable_invalidate_contents ()
;;;void
;;;gdk_paintable_invalidate_contents (GdkPaintable *paintable);
;;;Called by implementations of GdkPaintable to invalidate their contents. Unless the contents are invalidated, implementations must guarantee that multiple calls of gdk_paintable_snapshot() produce the same output.

;;;This function will emit the “invalidate-contents” signal.

;;;If a paintable reports the GDK_PAINTABLE_STATIC_CONTENTS flag, it must not call this function.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;gdk_paintable_invalidate_size ()
;;;void
;;;gdk_paintable_invalidate_size (GdkPaintable *paintable);
;;;Called by implementations of GdkPaintable to invalidate their size. As long as the size is not invalidated, paintable must return the same values for its intrinsic width, height and aspect ratio.

;;;This function will emit the “invalidate-size” signal.

;;;If a paintable reports the GDK_PAINTABLE_STATIC_SIZE flag, it must not call this function.

;;;Parameters
;;;paintable

;;;a GdkPaintable


;;;gdk_paintable_new_empty ()
;;;GdkPaintable *
;;;gdk_paintable_new_empty (int intrinsic_width,
;;;                         int intrinsic_height);
;;;Returns a paintable that has the given intrinsic size and draws nothing. This is often useful for implementing the GdkPaintableInterface.get_current_image() virtual function when the paintable is in an incomplete state (like a GtkMediaStream before receiving the first frame).

;;;Parameters
;;;intrinsic_width

;;;The intrinsic width to report. Can be 0 for no width.


;;;intrinsic_height

;;;The intrinsic height to report. Can be 0 for no height.


;;;Returns
;;;a GdkPaintable.

;;;[transfer full]

;;; ---- End of file gdk.paintable.lisp ----------------------------------------
