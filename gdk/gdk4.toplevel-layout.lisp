;;; ----------------------------------------------------------------------------
;;; gdk4.toplevel-layout.lisp
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
;;; GdkToplevelLayout
;;;
;;;     Information for presenting toplevels
;;;
;;; Types and Values
;;;
;;;     GdkToplevelLayout
;;;
;;; Functions
;;;
;;;     gdk_toplevel_layout_new
;;;     gdk_toplevel_layout_ref
;;;     gdk_toplevel_layout_unref
;;;     gdk_toplevel_layout_copy
;;;     gdk_toplevel_layout_equal
;;;     gdk_toplevel_layout_set_maximized
;;;     gdk_toplevel_layout_get_maximized
;;;     gdk_toplevel_layout_set_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen_monitor
;;;     gdk_toplevel_layout_set_resizable
;;;     gdk_toplevel_layout_get_resizable
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkToplevelLayout
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkToplevelLayout
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque toplevel-layout "GdkToplevelLayout"
  :type-initializer "gdk_toplevel_layout_get_type"
  :alloc (%toplevel-layout-new))

#+liber-documentation
(setf (liber:alias-for-class 'toplevel-layout)
      "GBoxed"
      (documentation 'toplevel-layout 'type)
 "@version{2024-1-9}
  @begin{short}
    Toplevel surfaces are sovereign windows that can be presented to the user
    in various states (maximized, on all workspaces, etc).
  @end{short}
  The @class{gdk:toplevel-layout} structure contains information that is
  necessary to do so, and is passed to the @fun{gdk:toplevel-present} function.
  @see-constructor{gdk:toplevel-layout-new}
  @see-constructor{gdk:toplevel-layout-copy}
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-present}")

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_layout_new" %toplevel-layout-new) :pointer)

(defun toplevel-layout-new ()
 #+liber-documentation
 "@version{2024-1-9}
  @return{The newly created @class{gdk:toplevel-layout} instance.}
  @begin{short}
    Create a toplevel layout description.
  @end{short}
  Used together with the @fun{gdk:toplevel-present} function to describe how a
  toplevel surface should be placed and behave on-screen.

  The size is in \"application pixels\", not \"device pixels\", see the
  @fun{gdk:surface-scale-factor} function.
  @see-class{gdk:toplevel-layout}
  @see-function{gdk:toplevel-present}
  @see-function{gdk:surface-scale-factor}"
  (make-instance 'toplevel-layout))

(export 'toplevel-layout-new)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_ref ()
;;;
;;; GdkToplevelLayout *
;;; gdk_toplevel_layout_ref (GdkToplevelLayout *layout);
;;;
;;; Increases the reference count of layout .
;;;
;;; layout :
;;;     a GdkToplevelLayout
;;;
;;; Returns :
;;;     the same layout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_unref ()
;;;
;;; void
;;; gdk_toplevel_layout_unref (GdkToplevelLayout *layout);
;;;
;;; Decreases the reference count of layout .
;;;
;;; layout :
;;;     a GdkToplevelLayout
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_layout_copy" toplevel-layout-copy)
     (g:boxed toplevel-layout :return)
 #+liber-documentation
 "@version{2024-1-9}
  @argument[layout]{a @class{gdk:toplevel-layout} instance}
  @return{The @class{gdk:toplevel-layout} instance with the copy of
    @arg{layout}.}
  @begin{short}
    Create a new @class{gdk:toplevel-layout} instance and copy the contents of
    @arg{layout} into it.
  @end{short}
  @see-class{gdk:toplevel-layout}"
  (layout (g:boxed toplevel-layout)))

(export 'toplevel-layout-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_layout_equal" toplevel-layout-equal) :boolean
 #+liber-documentation
 "@version{2024-1-9}
  @argument[layout]{a @class{gdk:toplevel-layout} instance}
  @argument[other]{another @class{gdk:toplevel-layout} instance}
  @return{@em{True} if @arg{layout} and @arg{other} have identical layout
    properties, otherwise @em{false}.}
  @begin{short}
    Check whether @arg{layout} and @arg{other} have identical layout properties.
  @end{short}
  @see-class{gdk:toplevel-layout}"
  (layout (g:boxed toplevel-layout))
  (other (g:boxed toplevel-layout)))

(export 'toplevel-layout-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_set_maximized ()
;;; gdk_toplevel_layout_get_maximized ()
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is not quiet correct. The return value specifies
;; whether the layout specifies the maximized state for the toplevel.

(defun (setf toplevel-layout-maximized) (maximized layout)
  (cffi:foreign-funcall "gdk_toplevel_layout_set_maximized"
                        (g:boxed toplevel-layout) layout
                        :boolean maximized
                        :void)
  maximized)

(cffi:defcfun ("gdk_toplevel_layout_get_maximized" %toplevel-layout-maximized)
    :boolean
  (layout (g:boxed toplevel-layout))
  (maximized (:pointer :boolean)))

(defun toplevel-layout-maximized (layout)
 #+liber-documentation
 "@version{2024-1-9}
  @syntax[]{(gdk:toplevel-layout-maximized layout) => maximized}
  @syntax[]{(setf (gdk:toplevel-layout-maximized layout) maximized)}
  @argument[layout]{a @class{gdk:toplevel-layout} instance}
  @argument[maximized]{a boolean whether the toplevel should be maximized}
  @begin{short}
    If the layout specifies whether the toplevel should go maximized, the
    @arg{maximized} argument is set to @em{true} if it should go fullscreen, or
    @em{false}, if it should go unmaximized.
  @end{short}
  @see-class{gdk:toplevel-layout}"
  (cffi:with-foreign-object (maximized :boolean)
    (when (%toplevel-layout-maximized layout maximized)
      (values (cffi:mem-ref maximized :boolean)))))

(export 'toplevel-layout-maximized)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_get_fullscreen ()
;;; gdk_toplevel_layout_set_fullscreen ()
;;; ----------------------------------------------------------------------------

;; TODO: The implementation is not quiet correct. The return value specifies
;; whether the layout specifies the fullscreen state for the toplevel.

(defun (setf toplevel-layout-fullscreen) (fullscreen layout monitor)
  (cffi:foreign-funcall "gdk_toplevel_layout_set_fullscreen"
                        (g:boxed toplevel-layout) layout
                        :boolean fullscreen
                        (g:object monitor) monitor
                        :void)
  fullscreen)

(cffi:defcfun ("gdk_toplevel_layout_get_fullscreen" %toplevel-layout-fullscreen)
    :boolean
  (layout (g:boxed toplevel-layout))
  (fullscreen (:pointer :boolean)))

(defun toplevel-layout-fullscreen (layout)
 #+liber-documentation
 "@version{2024-1-9}
  @syntax[]{(gdk:toplevel-layout-fullscreen layout) => fullscreen}
  @syntax[]{(setf (gdk:toplevel-layout-fullscreen layout) fullscreen)}
  @argument[layout]{a @class{gdk:toplevel-layout} instance}
  @argument[fullscreen]{a boolean whether the layout specifies the fullscreen
    state for the toplevel}
  @begin{short}
    If the layout specifies whether the toplevel should go fullscreen, the
    @arg{fullscreen} value is set to @em{true} if it should go fullscreen, or
    @em{false}, if it should go unfullscreen.
  @end{short}
  @see-class{gdk:toplevel-layout}"
  (cffi:with-foreign-object (fullscreen :boolean)
    (when (%toplevel-layout-fullscreen layout fullscreen)
      (values (cffi:mem-ref fullscreen :boolean)))))

(export 'toplevel-layout-fullscreen)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_get_fullscreen_monitor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_layout_get_fullscreen_monitor"
               toplevel-layout-fullscreen-monitor) (g:object monitor)
 #+liber-documentation
 "@version{2024-1-9}
  @argument[layout]{a @class{gdk:toplevel-layout} object}
  @return{The @class{gdk:monitor} object on which @arg{layout} fullscreens.}
  @begin{short}
    Returns the monitor that the layout is fullscreening the surface on.
  @end{short}
  @see-class{gdk:toplevel-layout}
  @see-class{gdk:monitor}"
  (layout (g:boxed toplevel-layout)))

(export 'toplevel-layout-fullscreen-monitor)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_layout_get_resizable ()
;;; gdk_toplevel_layout_set_resizable ()
;;; ----------------------------------------------------------------------------

(defun (setf toplevel-layout-resizable) (resizable layout)
  (cffi:foreign-funcall "gdk_toplevel_layout_set_resizable"
                        (g:boxed toplevel-layout) layout
                        :boolean resizable
                        :void)
  resizable)

(cffi:defcfun ("gdk_toplevel_layout_get_resizable" toplevel-layout-resizable)
    :boolean
 #+liber-documentation
 "@version{2024-1-9}
  @syntax[]{(gdk:toplevel-layout-resizable layout) => resizable}
  @syntax[]{(setf (gdk:toplevel-layout-resizable layout) resizable)}
  @argument[layout]{a @class{gdk:toplevel-layout} instance}
  @argument[resizable]{@em{true} if the layout is resizable}
  @begin{short}
    The @fun{gdk:toplevel-layout-resizable} function returns whether the layout
    should allow the user to resize the surface.
  @end{short}
  The @setf{gdk:toplevel-layout-resizable} function sets whether the layout
  should allow the user to resize the surface after it has been presented.
  @see-class{gdk:toplevel-layout}"
  (layout (g:boxed toplevel-layout)))

(export 'toplevel-layout-resizable)

;;; --- End of file gdk4.toplevel-layout.lisp ----------------------------------
