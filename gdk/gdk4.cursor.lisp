;;; ----------------------------------------------------------------------------
;;; gdk4.cursor.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; Cursors
;;;
;;;     Named and texture cursors
;;;
;;; Types and Values
;;;
;;;     GdkCursor
;;;
;;; Accessors
;;;
;;;     gdk_cursor_get_fallback
;;;     gdk_cursor_get_hotspot_x
;;;     gdk_cursor_get_hotspot_y
;;;     gdk_cursor_get_name
;;;     gdk_cursor_get_texture
;;;
;;; Functions
;;;
;;;     gdk_cursor_new_from_texture
;;;     gdk_cursor_new_from_name
;;;     gdk_cursor_new_from_callback                        Since 4.16
;;;
;;;     GdkCursorGetTextureCallback                         Since 4.16
;;;
;;; Properties
;;;
;;;     fallback
;;;     hotspot-x
;;;     hotspot-y
;;;     name
;;;     texture
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkCursor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCursor
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkCursor" cursor
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_cursor_get_type")
  ((fallback
    cursor-fallback
    "fallback" "GdkCursor" t nil)
   (hotspot-x
    cursor-hotspot-x
    "hotspot-x" "gint" t nil)
   (hotspot-y
    cursor-hotspot-y
    "hotspot-y" "gint" t nil)
   (name
    cursor-name
    "name" "gchararray" t nil)
   (texture
    cursor-texture
    "texture" "GdkTexture" t nil)))

#+liber-documentation
(setf (documentation 'cursor 'type)
 "@version{2024-06-30}
  @begin{short}
    The @class{gdk:cursor} object represents a cursor.
  @end{short}
  Cursors are immutable objects, so once you created them, there is no way to
  modify them later. Create a new cursor when you want to change something
  about it.

  Cursors by themselves are not very interesting, they must be bound to a
  window for users to see them. This is done with the @fun{gdk:surface-cursor}
  or the @fun{gdk:surface-device-cursor} functions. Applications will typically
  use higher-level GTK functions such as the @fun{gtk:widget-cursor} function
  instead.

  Cursors are not bound to a given @class{gdk:display} object, so they can be
  shared. However, the appearance of cursors may vary when used on different
  platforms.

  There are multiple ways to create cursors. The platform's own cursors can be
  created with the @fun{gdk:cursor-new-from-name} function. That function lists
  the commonly available names that are shared with the CSS specification. Other
  names may be available, depending on the platform in use. On some platforms,
  what images are used for named cursors may be influenced by the cursor theme.

  Another option to create a cursor is to use the
  @fun{gdk:cursor-new-from-texture} function and provide an image to use for
  the cursor.

  To ease work with unsupported cursors, a fallback cursor can be provided. If
  a @class{gdk:surface} object cannot use a cursor because of the reasons
  mentioned above, it will try the fallback cursor. Fallback cursors can
  themselves have fallback cursors again, so it is possible to provide a chain
  of progressively easier to support cursors. If none of the provided cursors
  can be supported, the default cursor will be the ultimate fallback.
  @see-constructor{gdk:cursor-new-from-name}
  @see-constructor{gdk:cursor-new-from-texture}
  @see-slot{gdk:cursor-fallback}
  @see-slot{gdk:cursor-hotspot-x}
  @see-slot{gdk:cursor-hotspot-y}
  @see-slot{gdk:cursor-name}
  @see-slot{gdk:cursor-texture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:cursor-fallback ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fallback" 'cursor) t)
 "The @code{fallback} property of type @class{gdk:cursor}
  (Read / Write / Construct only) @br{}
  Cursor image to fall back to if the cursor cannot be displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-fallback)
      "Accessor"
      (documentation 'cursor-fallback 'function)
 "@version{2025-08-04}
  @syntax{(gdk:cursor-fallback object) => fallback}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[fallback]{a @class{gdk:cursor} object for the fallback of the
    cursor or @code{nil} to use the default cursor as fallback}
  @begin{short}
    The accessor for the @slot[gdk:cursor]{fallback} slot of the
    @class{gdk:cursor} class returns the fallback for @arg{cursor}.
  @end{short}
  The fallback will be used if the cursor is not available on a given
  @class{gdk:display} object.

  For named cursors, this can happen when using nonstandard names or when using
  an incomplete cursor theme. For textured cursors, this can happen when the
  texture is too large or when the @class{gdk:display} object it is used on
  does not support textured cursors.
  @see-class{gdk:cursor}
  @see-class{gdk:display}")

;;; --- gdk:cursor-hotspot-x ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hotspot-x" 'cursor) t)
 "The @code{hotspot-x} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  Horizontal offset of the cursor hotspot. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-hotspot-x)
      "Accessor"
      (documentation 'cursor-hotspot-x 'function)
 "@version{2025-08-04}
  @syntax{(gdk:cursor-hotspot-x object) => hotspot}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[hotspot]{an integer for the horizontal offset of the hotspot or 0
    for named cursors}
  @begin{short}
    The accessor for the @slot[gdk:cursor]{hotspot-x} slot of the
    @class{gdk:cursor} class returns the horizontal offset of the hotspot.
  @end{short}
  The hotspot indicates the pixel that will be directly above the cursor.

  Note that named cursors may have a nonzero hotspot, but this function will
  only return the hotspot position for cursors created with the
  @fun{gdk:cursor-new-from-texture} function.
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-from-texture}
  @see-function{gdk:cursor-hotspot-y}")

;;; --- gdk:cursor-hotspot-y ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hotspot-y" 'cursor) t)
 "The @code{hotspot-y} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  Vertical offset of the cursor hotspot. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-hotspot-y)
      "Accessor"
      (documentation 'cursor-hotspot-y 'function)
 "@version{2025-08-04}
  @syntax{(gdk:cursor-hotspot-y object) => hotspot}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[hotspot]{an integer for the vertical offset of the hotspot or 0
    or named cursors}
  @begin{short}
    The accessor for the @slot[gdk:cursor]{hotspot-y} slot of the
    @class{gdk:cursor} class returns the vertical offset of the hotspot.
  @end{short}
  The hotspot indicates the pixel that will be directly above the cursor.

  Note that named cursors may have a nonzero hotspot, but this function will
  only return the hotspot position for cursors created with the
  @fun{gdk:cursor-new-from-texture} function.
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-from-texture}
  @see-function{gdk:cursor-hotspot-x}")

;;; --- gdk:cursor-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'cursor) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name of the cursor. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-name)
      "Accessor"
      (documentation 'cursor-name 'function)
 "@version{2025-08-04}
  @syntax{(gdk:cursor-name object) => name}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[name]{a string for the name of the cursor or @code{nil} if it is
    not a named cursor}
  @begin{short}
    The accessor for the @slot[gdk:cursor]{name} slot of the @class{gdk:cursor}
    class gets or sets the name of the cursor.
  @end{short}
  If the cursor is not a named cursor, @code{nil} will be returned.
  @see-class{gdk:cursor}")

;;; --- gdk:cursor-texture -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "texture" 'cursor) t)
 "The @code{texture} property of type @class{gdk:texture}
  (Read / Write / Construct only) @br{}
  The texture displayed by this cursor.")

#+liber-documentation
(setf (liber:alias-for-function 'cursor-texture)
      "Accessor"
      (documentation 'cursor-texture 'function)
 "@version{2024-08-04}
  @syntax{(gdk:cursor-texture object) => texture}
  @argument[object]{a @class{gdk:cursor} object}
  @argument[texture]{a @class{gdk:texture} object for the texture for
    @arg{cursor} or @code{nil} if it is a named cursor}
  @begin{short}
    The accessor for the @slot[gdk:cursor]{texture} slot of the
    @class{gdk:cursor} class returns the texture for the cursor.
  @end{short}
  If the cursor is a named cursor, @code{nil} will be returned.
  @see-class{gdk:cursor}
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_from_texture" %cursor-new-from-texture)
    (g:object cursor)
  (texture (g:object texture))
  (xhotspot :int)
  (yhotspot :int)
  (fallback (g:object cursor)))

(defun cursor-new-from-texture (texture xhotspot yhotspot &optional fallback)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[texture]{a @class{gdk:texture} object for the texture providing
    the pixel data}
  @argument[xhotspot]{an integer for the horizontal offset of the hotspot of
    the cursor}
  @argument[yhotspot]{an integer for the vertical offset of the hotspot of
    the cursor}
  @argument[fallback]{an optional @class{gdk:cursor} object to fall back to
    when this one cannot be supported, the default is @code{nil}}
  @return{The new @class{gdk:cursor} object.}
  @begin{short}
    Creates a new cursor from a @class{gdk:texture} object.
  @end{short}
  @see-class{gdk:cursor}
  @see-class{gdk:texture}"
  (%cursor-new-from-texture texture xhotspot yhotspot fallback))

(export 'cursor-new-from-texture)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cursor_new_from_name" %cursor-new-from-name)
    (g:object cursor)
  (name :string)
  (fallback (g:object cursor)))

(defun cursor-new-from-name (name &optional fallback)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[name]{a string for the name of the cursor}
  @argument[fallback]{an optional @class{gdk:cursor} object to fall back to
    when this one cannot be supported, the default is @code{nil}}
  @begin{return}
    The new @class{gdk:cursor} object, or @code{nil} if there is no cursor with
    the given @arg{name}.
  @end{return}
  @begin{short}
    Creates a new cursor by looking up @arg{name} in the current cursor theme.
  @end{short}
  A recommended set of cursor names that will work across different platforms
  can be found in the CSS specification:
  @begin{table}
    @entry[\"none\"]{}
    @entry[\"default\"]{@image[cursor-default]{}}
    @entry[\"help\"]{@image[cursor-help]{}}
    @entry[\"pointer\"]{@image[cursor-pointer]{}}
    @entry[\"context-menu\"]{@image[cursor-context-menu]{}}
    @entry[\"progress\"]{@image[cursor-progress]{}}
    @entry[\"wait\"]{@image[cursor-wait]{}}
    @entry[\"cell\"]{@image[cursor-cell]{}}
    @entry[\"crosshair\"]{@image[cursor-crosshair]{}}
    @entry[\"text\"]{@image[cursor-text]{}}
    @entry[\"vertical-text\"]{@image[cursor-vertical-text]{}}
    @entry[\"alias\"]{@image[cursor-alias]{}}
    @entry[\"copy\"]{@image[cursor-copy]{}}
    @entry[\"no-drop\"]{@image[cursor-no-drop]{}}
    @entry[\"move\"]{@image[cursor-move]{}}
    @entry[\"not-allowed\"]{@image[cursor-not-allowed]{}}
    @entry[\"grab\"]{@image[cursor-grab]{}}
    @entry[\"grabbing\"]{@image[cursor-grabbing]{}}
    @entry[\"all-scroll\"]{@image[cursor-all-scroll]{}}
    @entry[\"col-resize\"]{@image[cursor-col-resize]{}}
    @entry[\"row-resize\"]{@image[cursor-row-resize]{}}
    @entry[\"n-resize\"]{@image[cursor-n-resize]{}}
    @entry[\"e-resize\"]{@image[cursor-e-resize]{}}
    @entry[\"s-resize\"]{@image[cursor-s-resize]{}}
    @entry[\"w-resize\"]{@image[cursor-w-resize]{}}
    @entry[\"ne-resize\"]{@image[cursor-ne-resize]{}}
    @entry[\"nw-resize\"]{@image[cursor-nw-resize]{}}
    @entry[\"sw-resize\"]{@image[cursor-sw-resize]{}}
    @entry[\"se-resize\"]{@image[cursor-se-resize]{}}
    @entry[\"ew-resize\"]{@image[cursor-ew-resize]{}}
    @entry[\"ns-resize\"]{@image[cursor-ns-resize]{}}
    @entry[\"nesw-resize\"]{@image[cursor-nesw-resize]{}}
    @entry[\"nwse-resize\"]{@image[cursor-nwse-resize]{}}
    @entry[\"zoom-in\"]{@image[cursor-zoom-in]{}}
    @entry[\"zoom-out\"]{@image[cursor-zoom-out]{}}
  @end{table}
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:cursor-new-from-name \"wait\")
=> #<GDK:CURSOR {1013383A73@}>
    @end{pre}
  @end{dictionary}
  @see-class{gdk:cursor}"
  (%cursor-new-from-name name fallback))

(export 'cursor-new-from-name)

;;; ----------------------------------------------------------------------------
;;;     gdk_cursor_new_from_callback                        Since 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;     GdkCursorGetTextureCallback                         Since 4.16
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.cursor.lisp -------------------------------------------
