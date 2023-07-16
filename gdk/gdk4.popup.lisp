;;; ----------------------------------------------------------------------------
;;; gdk4.popup.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; Popups
;;;
;;;     Interface for popup surfaces
;;;
;;; Types and Values
;;;
;;;     GdkPopup
;;;
;;; Accessor
;;;
;;;     gdk_popup_get_autohide
;;;     gdk_popup_get_parent
;;;
;;; Functions
;;;
;;;     gdk_popup_present
;;;     gdk_popup_get_surface_anchor
;;;     gdk_popup_get_rect_anchor
;;;     gdk_popup_get_position_x
;;;     gdk_popup_get_position_y
;;;
;;; Properties
;;;
;;;     autohide
;;;     parent
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkPopup
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkPopup
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GdkPopup" popup
  (:export t
   :type-initializer "gdk_popup_get_type")
  ((autohide
    popup-autohide
    "autohide" "gboolean" t t)
   (parent
    popup-parent
    "parent" "GdkSurface" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'popup)
      "Interface"
      (documentation 'popup 'type)
 "@version{#2023-4-9}
  @begin{short}
    A @sym{gdk:popup} object is a surface that is attached to another surface,
    called its \"parent\", and is positioned relative to it.
  @end{short}
  @sym{gdk:popup} objects are typically used to implement menus and similar
  popups. They can be modal, which is indicated by the @code{autohide} property.
  @see-class{gdk:toplevel}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- popup-autohide ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autohide" 'popup) t)
 "The @code{autohide} property of type @code{:boolean}
  (Read / Write / Construct only) @br{}
  Whether to hide on outside clicks. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'popup-autohide)
      "Accessor"
      (documentation 'popup-autohide 'function)
 "@version{#2023-4-9}
  @syntax[]{(gdk:popup-autohide object) => autohide}
  @syntax[]{(setf (gdk:popup-autohide object) autohide)}
  @argument[object]{a @class{gdk:popup} object}
  @argument[autohide]{@em{True} if @arg{popup} will autohide.}
  @begin{short}
    Accessor of the @slot[gdk:popup]{autohide} slot of the @class{gdk:popup}
    class.
  @end{short}
  The @sym{gdk:popup-autohide} function returns whether the popup is set to
  hide on outside clicks.
  @see-class{gdk:popup}
  @see-class{gdk:surface}")

;;; --- popup-parent -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "parent" 'popup) t)
 "The @code{parent} property of type @class{gdk:surface}
  (Read / Write / Construct only) @br{}
  The parent surface.")

#+liber-documentation
(setf (liber:alias-for-function 'popup-parent)
      "Accessor"
      (documentation 'popup-parent 'function)
 "@version{#2023-4-9}
  @syntax[]{(gdk:popup-parent object) => parent}
  @syntax[]{(setf (gdk:popup-parent object) parent)}
  @argument[object]{a @class{gdk:popup} object}
  @argument[parent]{a @class{gdk:surface} parent surface}
  @begin{short}
    Accessor of the @slot[gdk:popup]{parent} slot of the @class{gdk:popup}
    class.
  @end{short}
  The @sym{gdk:popup-parent} function returns the parent surface of a popup.
  @see-class{gdk:popup}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; gdk_popup_present ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_present" popup-present) :boolean
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[popup]{a @class{gdk:popup} object to show}
  @argument[width]{an integer with the unconstrained popup width to layout}
  @argument[height]{an integer with the unconstrained popup height to layout}
  @argument[layout]{a @class{gdk:popup-layout} object used to layout}
  @return{@em{False} if it failed to be presented, otherwise @em{true}.}
  @begin{short}
    Present @arg{popup} after having processed the @class{gdk:popup-layout}
    rules.
  @end{short}
  If the popup was previously now showing, it will be showed, otherwise it will
  change position according to @arg{layout}.

  After calling this function, the result should be handled in response to the
  \"layout\" signal being emitted. The resulting popup position can be queried
  using the @fun{gdk:popup-position-x}, @fun{gdk:popup-position-y} functions,
  and the resulting size will be sent as parameters in the layout signal. Use
  the @fun{gdk:popup-rect-anchor} and @fun{gdk:popup-surface-anchor} function
  to get the resulting anchors.

  Presenting may fail, for example if the popup is set to autohide and is
  immediately hidden upon being presented. If presenting failed, the \"layout\"
  signal will not me emitted.
  @see-class{gdk:popup}
  @see-class{gdk:popup-layout}
  @see-function{gdk:popup-position-x}
  @see-function{gdk:popup-position-y}
  @see-function{gdk:popup-rect-anchor}
  @see-function{gdk:popup-surface-anchor}"
  (popup (g:object popup))
  (width :int)
  (height :int)
  (layout (g:object popup-layout)))

(export 'popup-present)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_get_surface_anchor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_get_surface_anchor" popup-surface-anchor) gravity
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[popup]{a @class{gdk:popup} object}
  @return{The @symbol{gdk:gravity} value with the current anchor value of
    @arg{popup}.}
  @begin{short}
    Gets the current popup surface anchor.
  @end{short}
  The value returned may change after calling the @fun{gdk:popup-present}
  function, or after the \"layout\" signal is emitted.
  @see-class{gdk:popup}
  @see-symbol{gdk:gravity}"
  (popup (g:object popup)))

(export 'popup-surface-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_get_rect_anchor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_get_rect_anchor" popup-rect-anchor) gravity
 "@version{#2023-4-10}
  @argument[popup]{a @class{gdk:popup} object}
  @return{The @symbol{gdk:gravity} value with the current rectangle anchor
    value of @arg{popup}.}
  @begin{short}
    Gets the current popup surface anchor.
  @end{short}
  The value returned may change after calling the @fun{gdk:popup-present}
  function, or after the \"layout\" signal is emitted.
  @see-class{gdk:popup}
  @see-symbol{gdk:gravity}"
  (popup (g:object popup)))

(export 'popup-rect-anchor)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_get_position_x ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_get_position_x" popup-position-x) :int
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[popup]{a @class{gdk:popup} object}
  @return{An integer with the x coordinate of @arg{popup} position.}
  @short{Obtains the position of the popup relative to its parent.}
  @see-class{gdk:popup}"
  (popup (g:object popup)))

(export 'popup-position-x)

;;; ----------------------------------------------------------------------------
;;; gdk_popup_get_position_y ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_popup_get_position_y" popup-position-y) :int
 #+liber-documentation
 "@version{#2023-4-10}
  @argument[popup]{a @class{gdk:popup} object}
  @return{An integer with the y coordinate of @arg{popup} position.}
  @short{Obtains the position of the popup relative to its parent.}
  @see-class{gdk:popup}"
  (popup (g:object popup)))

(export 'popup-position-y)

;;; --- End of file gdk4.popup.lisp --------------------------------------------
