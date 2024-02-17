;;; ----------------------------------------------------------------------------
;;; gdk4.toplevel-size.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GDK library.
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

(cffi:define-foreign-type toplevel-size-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser toplevel-size))

(defmethod cffi:translate-to-foreign (proxy (type toplevel-size-type))
  proxy)

(defmethod cffi:translate-from-foreign (native (type toplevel-size-type))
  native)

#+liber-documentation
(setf (liber:alias-for-symbol 'toplevel-size)
      "CStruct"
      (liber:symbol-documentation 'toplevel-size)
 "@version{#2023-4-10}
  @begin{short}
    The @class{gdk:toplevel-size} structure contains information that may be
    useful for users of @class{gdk:toplevel} objects to compute a surface size.
  @end{short}
  It also carries information back with the computational result.
  @see-class{gdk:toplevel}")

(export 'toplevel-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_get_bounds ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_bounds" %toplevel-size-bounds) :void
  (size toplevel-size)
  (width (:pointer :int))
  (height (:pointer :int)))

(defun toplevel-size-bounds (size)
 #+liber-documentation
 "@version{#2023-4-10}
  @syntax[]{(gdk:toplevel-size size) => width, height}
  @argument[size]{a @symbol{gdk:toplevel-size} instance}
  @argument[width]{an integer with the width}
  @argument[height]{an integer with the height}
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
;;; gdk_toplevel_size_set_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_size" toplevel-size-set-size) :void
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[size]{a @symbol{gdk:toplevel-size} instance}
  @argument[width]{an integer with the width}
  @argument[height]{an integer with the height}
  @begin{short}
    Sets the size the toplevel prefers to be resized to.
  @end{short}
  The size should be within the bounds, see the @fun{gdk:toplevel-size-bounds}
  function. The set size should be considered as a hint, and should not be
  assumed to be respected by the windowing system, or backend.
  @see-symbol{gdk:toplevel-size}
  @see-function{gdk:toplevel-size-bounds}"
  (size toplevel-size)
  (width :int)
  (height :int))

(export 'toplevel-size-set-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_set_min_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_min_size" toplevel-size-set-min-size)
    :void
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[size]{a @symbol{gdk:toplevel-size} instance}
  @argument[width]{an integer with the minimum width}
  @argument[height]{an integer with the minimum height}
  @begin{short}
    The minimum size corresponds to the limitations the toplevel can be shrunk
    to, without resulting in incorrect painting.
  @end{short}
  A user of a @class{gdk:toplevel} object should calculate these given both the
  existing size, and the bounds retrieved from the @symbol{gdk:toplevel-size}
  object.

  The minimum size should be within the bounds. see the
  @fun{gdk:toplevel-size-bounds} function.
  @see-symbol{gdk:toplevel-size}
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-size-bounds}"
  (size toplevel-size)
  (width :int)
  (height :int))

(export 'toplevel-size-set-min-size)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_size_set_shadow_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_size_set_shadow_width"
               toplevel-size-set-shadow-width) :void
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[size]{a @symbol{gdk:toplevel-size} instance}
  @argument[left]{an integer with the width of the left part of the shadow}
  @argument[right]{an integer with the width of the right part of the shadow}
  @argument[top]{an integer with the height of the top part of the shadow}
  @argument[bottom]{an integer with the height of the bottom part of the shadow}
  @begin{short}
    The shadow width corresponds to the part of the computed surface size that
    would consist of the shadow margin surrounding the window, would there be
    any.
  @end{short}
  @see-symbol{gdk:toplevel-size}"
  (size toplevel-size)
  (left :int)
  (right :int)
  (top :int)
  (bottom :int))

(export 'toplevel-size-set-shadow-width)

;;; --- End of file gdk4.toplevel-size.lisp ------------------------------------
