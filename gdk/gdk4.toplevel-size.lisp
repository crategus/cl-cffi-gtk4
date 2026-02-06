;;; ----------------------------------------------------------------------------
;;; gdk4.toplevel-size.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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

;;; ----------------------------------------------------------------------------
;;; GdkToplevelSize
;;; ----------------------------------------------------------------------------

(cffi:defcstruct toplevel-size
  ;; no available fields
  )

#+liber-documentation
(setf (liber:alias-for-symbol 'toplevel-size)
      "CStruct"
      (liber:symbol-documentation 'toplevel-size)
 "@version{2025-09-25}
  @begin{declaration}
(cffi:defcstruct toplevel-size
  ;; no available fields
  )
  @end{declaration}
  @begin{short}
    The @sym{gdk:toplevel-size} structure contains information that may be
    useful for users of @class{gdk:toplevel} objects to compute a surface size.
  @end{short}
  It also carries information back with the computational result.
  @see-class{gdk:toplevel}")

(export 'toplevel-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_get_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_bounds" %toplevel-size-bounds) :void
  (size (:pointer (:struct toplevel-size)))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun toplevel-size-bounds (size)
 #+liber-documentation
 "@version{#2025-09-25}
  @syntax{(gdk:toplevel-size-bounds size) => width, height}
  @argument[size]{a @sym{gdk:toplevel-size} instance}
  @argument[width]{an integer for the width}
  @argument[height]{an integer for the height}
  @begin{short}
    Retrieves the bounds the toplevel is placed within.
  @end{short}
  The bounds represent the largest size a toplevel may have while still being
  able to fit within some type of boundary. Depending on the backend, this may
  be equivalent to the dimensions of the work area or the monitor on which the
  window is being presented on, or something else that limits the way a toplevel
  can be presented.
  @see-symbol{gdk:toplevel-size}
  @see-class{gdk:toplevel}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%toplevel-size-bounds size width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'toplevel-size-bounds)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_set_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_size" toplevel-size-set-size) :void
 #+liber-documentation
 "@version{#2025-09-25}
  @argument[size]{a @sym{gdk:toplevel-size} instance}
  @argument[width]{an integer for the width}
  @argument[height]{an integer for the height}
  @begin{short}
    Sets the size the toplevel prefers to be resized to.
  @end{short}
  The size should be within the bounds, see the @fun{gdk:toplevel-size-bounds}
  function. The set size should be considered as a hint, and should not be
  assumed to be respected by the windowing system, or backend.
  @see-symbol{gdk:toplevel-size}
  @see-function{gdk:toplevel-size-bounds}"
  (size (:pointer (:struct toplevel-size)))
  (width :int)
  (height :int))

(export 'toplevel-size-set-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_set_min_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_min_size" toplevel-size-set-min-size)
    :void
 #+liber-documentation
 "@version{#2025-09-25}
  @argument[size]{a @sym{gdk:toplevel-size} instance}
  @argument[width]{an integer for the minimum width}
  @argument[height]{an integer for the minimum height}
  @begin{short}
    The minimum size corresponds to the limitations the toplevel can be shrunk
    to, without resulting in incorrect painting.
  @end{short}
  A user of a @class{gdk:toplevel} object should calculate these given both the
  existing size, and the bounds retrieved from the @sym{gdk:toplevel-size}
  instance.

  The minimum size should be within the bounds. see the
  @fun{gdk:toplevel-size-bounds} function.
  @see-symbol{gdk:toplevel-size}
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-size-bounds}"
  (size (:pointer (:struct toplevel-size)))
  (width :int)
  (height :int))

(export 'toplevel-size-set-min-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_set_shadow_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_shadow_width"
               toplevel-size-set-shadow-width) :void
 #+liber-documentation
 "@version{#2025-09-25}
  @argument[size]{a @sym{gdk:toplevel-size} instance}
  @argument[left]{an integer for the width of the left part of the shadow}
  @argument[right]{an integer for the width of the right part of the shadow}
  @argument[top]{an integer for the height of the top part of the shadow}
  @argument[bottom]{an integer for the height of the bottom part of the shadow}
  @begin{short}
    The shadow width corresponds to the part of the computed surface size that
    would consist of the shadow margin surrounding the window, would there be
    any.
  @end{short}
  @see-symbol{gdk:toplevel-size}"
  (size (:pointer (:struct toplevel-size)))
  (left :int)
  (right :int)
  (top :int)
  (bottom :int))

(export 'toplevel-size-set-shadow-width)

;;; --- End of file gdk4.toplevel-size.lisp ------------------------------------
