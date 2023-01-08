;;; ----------------------------------------------------------------------------
;;; gdk.cursor.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;;
;;; A GdkCursor represents a cursor. Its contents are private.
;;;
;;; Cursors are immutable objects, so they can not change after they have been
;;; constructed.
;;;
;;; These functions are used to create and destroy cursors. Cursors are
;;; immutable objects, so once you created them, there is no way to modify them
;;; later. Create a new cursor when you want to change something about it.
;;;
;;; Cursors by themselves are not very interesting, they must be bound to a
;;; window for users to see them. This is done with gdk_surface_set_cursor() or
;;; gdk_surface_set_device_cursor(). Applications will typically use
;;; higher-level GTK functions such as gtk_widget_set_cursor() instead.
;;;
;;; Cursors are not bound to a given GdkDisplay, so they can be shared. However,
;;; the appearance of cursors may vary when used on different platforms.
;;;
;;; There are multiple ways to create cursors. The platform's own cursors can be
;;; created with gdk_cursor_new_from_name(). That function lists the commonly
;;; available names that are shared with the CSS specification. Other names may
;;; be available, depending on the platform in use. On some platforms, what
;;; images are used for named cursors may be influenced by the cursor theme.
;;;
;;; Another option to create a cursor is to use gdk_cursor_new_from_texture()
;;; and provide an image to use for the cursor.
;;;
;;; To ease work with unsupported cursors, a fallback cursor can be provided. If
;;; a GdkSurface cannot use a cursor because of the reasons mentioned above, it
;;; will try the fallback cursor. Fallback cursors can themselves have fallback
;;; cursors again, so it is possible to provide a chain of progressively easier
;;; to support cursors. If none of the provided cursors can be supported, the
;;; default cursor will be the ultimate fallback.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkCursor" cursor
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

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “fallback” property
;;;
;;;  “fallback”                 GdkCursor *
;;;
;;; Cursor image to fall back to if this cursor cannot be displayed.
;;;
;;; Owner: GdkCursor
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “hotspot-x” property
;;;
;;;  “hotspot-x”                int
;;;
;;; Horizontal offset of the cursor hotspot.
;;;
;;; Owner: GdkCursor
;;; Flags: Read / Write / Construct Only
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “hotspot-y” property
;;;
;;;  “hotspot-y”                int
;;;
;;; Vertical offset of the cursor hotspot.
;;;
;;; Owner: GdkCursor
;;; Flags: Read / Write / Construct Only
;;; Allowed values: >= 0
;;; Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “name” property
;;;
;;;  “name”                     char *
;;;
;;; Name of this cursor.
;;;
;;; Owner: GdkCursor
;;; Flags: Read / Write / Construct Only
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “texture” property
;;;
;;;  “texture”                  GdkTexture *
;;;
;;; The texture displayed by this cursor.
;;;
;;; Owner: GdkCursor
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_texture ()
;;;
;;; GdkCursor *
;;; gdk_cursor_new_from_texture (GdkTexture *texture,
;;;                              int hotspot_x,
;;;                              int hotspot_y,
;;;                              GdkCursor *fallback);
;;;
;;; Creates a new cursor from a GdkTexture.
;;;
;;; texture :
;;;     the texture providing the pixel data
;;;
;;; hotspot_x :
;;;     the horizontal offset of the “hotspot” of the cursor
;;;
;;; hotspot_y :
;;;     the vertical offset of the “hotspot” of the cursor
;;;
;;; fallback :
;;;     NULL or the GdkCursor to fall back to when this one cannot be supported.
;;;
;;; Returns :
;;;     a new GdkCursor.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_texture" cursor-new-from-texture)
    (g:object cursor)
  (texture (g:object texture))
  (hotspot-x :int)
  (hotspot-y :int)
  (fallback (g:object cursor)))

(export 'cursor-new-from-texture)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_new_from_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_cursor_new_from_name" cursor-new-from-name) (g:object cursor)
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[name]{a string with the name of the cursor}
  @argument[fallback]{@code{nil} or a @class{gdk:cursor} object to fall back to
    when this one cannot be supported}
  @return{A new @class{gdk-cursor} object, or @code{nil} if there is no cursor
    with the given @arg{name}.}
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
(gdk-cursor-new-from-name (gdk-display-default) \"wait\")
=> #<GDK-X11-CURSOR {1001AFE123@}>
(gdk-cursor-new-from-name (gdk-display-default) \"unknown\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{gdk:cursor}"
  (name :string)
  (fallback (g:object cursor)))

(export 'cursor-new-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_fallback ()
;;;
;;; GdkCursor *
;;; gdk_cursor_get_fallback (GdkCursor *cursor);
;;;
;;; Returns the fallback for this cursor . The fallback will be used if this
;;; cursor is not available on a given GdkDisplay.
;;;
;;; For named cursors, this can happen when using nonstandard names or when
;;; using an incomplete cursor theme. For textured cursors, this can happen when
;;; the texture is too large or when the GdkDisplay it is used on does not
;;; support textured cursors.
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the fallback of the cursor or NULL to use the default cursor as
;;;     fallback.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_name ()
;;;
;;; const char *
;;; gdk_cursor_get_name (GdkCursor *cursor);
;;;
;;; Returns the name of the cursor. If the cursor is not a named cursor, NULL
;;; will be returned.
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the name of the cursor or NULL if it is not a named cursor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_texture ()
;;;
;;; GdkTexture *
;;; gdk_cursor_get_texture (GdkCursor *cursor);
;;;
;;; Returns the texture for the cursor. If the cursor is a named cursor, NULL
;;; will be returned.
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the texture for cursor or NULL if it is a named cursor.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_hotspot_x ()
;;;
;;; int
;;; gdk_cursor_get_hotspot_x (GdkCursor *cursor);
;;;
;;; Returns the horizontal offset of the hotspot. The hotspot indicates the
;;; pixel that will be directly above the cursor.
;;;
;;; Note that named cursors may have a nonzero hotspot, but this function will
;;; only return the hotspot position for cursors created with
;;; gdk_cursor_new_from_texture().
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the horizontal offset of the hotspot or 0 for named cursors
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_cursor_get_hotspot_y ()
;;;
;;; int
;;; gdk_cursor_get_hotspot_y (GdkCursor *cursor);
;;;
;;; Returns the vertical offset of the hotspot. The hotspot indicates the pixel
;;; that will be directly above the cursor.
;;;
;;; Note that named cursors may have a nonzero hotspot, but this function will
;;; only return the hotspot position for cursors created with
;;; gdk_cursor_new_from_texture().
;;;
;;; cursor :
;;;     a GdkCursor.
;;;
;;; Returns :
;;;     the vertical offset of the hotspot or 0 for named cursors
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.cursor.lisp --------------------------------------------
