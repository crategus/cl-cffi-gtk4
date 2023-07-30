;;; ----------------------------------------------------------------------------
;;; gdk4.drop.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; Types and Values
;;;
;;;     GdkDrop
;;;
;;; Accessors
;;;
;;;     gdk_drop_get_actions
;;;     gdk_drop_get_device
;;;     gdk_drop_get_display
;;;     gdk_drop_get_drag
;;;     gdk_drop_get_formats
;;;     gdk_drop_get_surface
;;;
;;; Functions
;;;
;;;     gdk_drop_status
;;;     gdk_drop_finish
;;;     gdk_drop_read_async
;;;     gdk_drop_read_finish
;;;     gdk_drop_read_value_async
;;;     gdk_drop_read_value_finish
;;;
;;; Properties
;;;
;;;     actions
;;;     device
;;;     display
;;;     drag
;;;     formats
;;;     surface
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrop
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDrop
;;;
;;; The GdkDrop struct contains only private fields and should not be accessed
;;; directly.
;;;
;;;
;;; These functions provide a low-level interface for drag and drop.
;;;
;;; The GdkDrag object represents the source side of an ongoing DND operation.
;;; It is created when a drag is started, and stays alive for duration of the
;;; DND operation. After a drag has been started with gdk_drag_begin(), the
;;; caller gets informed about the status of the ongoing drag operation with
;;; signals on the GdkDrag object.
;;;
;;; The GdkDrop object represents the target side of an ongoing DND operation.
;;; Possible drop sites get informed about the status of the ongoing drag
;;; operation with events of type GDK_DRAG_ENTER, GDK_DRAG_LEAVE,
;;; GDK_DRAG_MOTION and GDK_DROP_START. The GdkDrop object can be obtained from
;;; these GdkEvents using gdk_dnd_event_get_drop().
;;;
;;; The actual data transfer is initiated from the target side via an async
;;; read, using one of the GdkDrop functions for this purpose:
;;; gdk_drop_read_async() or gdk_drop_read_value_async().
;;;
;;; GTK provides a higher level abstraction based on top of these functions,
;;; and so they are not normally needed in GTK applications. See the Drag and
;;; Drop section of the GTK documentation for more information.
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDrop" drop
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_drop_get_type")
  ((actions
    drop-actions
    "actions" "GdkDragAction" t nil)
   (device
    drop-device
    "device" "GdkDevice" t nil)
   (display
    drop-display
    "display" "GdkDisplay" t nil)
   (drag
    drop-drag
    "drag" "GdkDrag" t nil)
   (formats
    drop-formats
    "formats" "GdkContentFormats" t nil)
   (surface
    drop-surface
    "surface" "GdkSurface" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- drop-actions -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “actions” property
;;;
;;;  “actions”                  GdkDragAction
;;;
;;; The possible actions for this drop
;;;
;;; Owner: GdkDrop
;;; Flags: Read / Write / Construct Only
;;; Default value: GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_actions ()
;;;
;;; GdkDragAction
;;; gdk_drop_get_actions (GdkDrop *self);
;;;
;;; Returns the possible actions for this GdkDrop. If this value contains
;;; multiple actions - ie gdk_drag_action_is_unique() returns FALSE for the
;;; result - gdk_drop_finish() must choose the action to use when accepting the
;;; drop. This will only happen if you passed GDK_ACTION_ASK as one of the
;;; possible actions in gdk_drop_status(). GDK_ACTION_ASK itself will not be
;;; included in the actions returned by this function.
;;;
;;; This value may change over the lifetime of the GdkDrop both as a response
;;; to source side actions as well as to calls to gdk_drop_status() or
;;; gdk_drop_finish(). The source side will not change this value anymore once
;;; a drop has started.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     The possible GdkDragActions
;;; ----------------------------------------------------------------------------

;;; --- drop-device ------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “device” property
;;;
;;;  “device”                   GdkDevice *
;;;
;;; The GdkDevice performing the drop
;;;
;;; Owner: GdkDrop
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_device ()
;;;
;;; GdkDevice *
;;; gdk_drop_get_device (GdkDrop *self);
;;;
;;; Returns the GdkDevice performing the drop.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     The GdkDevice performing the drop.
;;; ----------------------------------------------------------------------------

;;; --- drop-display -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “display” property
;;;
;;;  “display”                  GdkDisplay *
;;;
;;; The GdkDisplay that the drop belongs to.
;;;
;;; Owner: GdkDrop
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_display ()
;;;
;;; GdkDisplay *
;;; gdk_drop_get_display (GdkDrop *self);
;;;
;;; Gets the GdkDisplay that self was created for.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     a GdkDisplay.
;;; ----------------------------------------------------------------------------

;;; --- drop-drag --------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “drag” property
;;;
;;;  “drag”                     GdkDrag *
;;;
;;; The GdkDrag that initiated this drop
;;;
;;; Owner: GdkDrop
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_drag ()
;;;
;;; GdkDrag *
;;; gdk_drop_get_drag (GdkDrop *self);
;;;
;;; If this is an in-app drag-and-drop operation, returns the GdkDrag that
;;; corresponds to this drop.
;;;
;;; If it is not, NULL is returned.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     the corresponding GdkDrag.
;;; ----------------------------------------------------------------------------

;;; --- drop-formats -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The possible formats that the drop can provide its data in.
;;;
;;; Owner: GdkDrop
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_drop_get_formats (GdkDrop *self);
;;;
;;; Returns the GdkContentFormats that the drop offers the data to be read in.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     The possible GdkContentFormats.
;;; ----------------------------------------------------------------------------

;;; --- drop-surface -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “surface” property
;;;
;;;  “surface”                  GdkSurface *
;;;
;;; The GdkSurface the drop happens on
;;;
;;; Owner: GdkDrop
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_get_surface ()
;;;
;;; GdkSurface *
;;; gdk_drop_get_surface (GdkDrop *self);
;;;
;;; Returns the GdkSurface performing the drop.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; Returns :
;;;     The GdkSurface performing the drop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_status ()
;;;
;;; void
;;; gdk_drop_status (GdkDrop *self,
;;;                  GdkDragAction actions,
;;;                  GdkDragAction preferred);
;;;
;;; Selects all actions that are potentially supported by the destination.
;;;
;;; When calling this function, do not restrict the passed in actions to the
;;; ones provided by gdk_drop_get_actions(). Those actions may change in the
;;; future, even depending on the actions you provide here.
;;;
;;; The preferred action is a hint to the drag'n'drop mechanism about which
;;; action to use when multiple actions are possible.
;;;
;;; This function should be called by drag destinations in response to
;;; GDK_DRAG_ENTER or GDK_DRAG_MOTION events. If the destination does not yet
;;; know the exact actions it supports, it should set any possible actions first
;;; and then later call this function again.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; actions :
;;;     Supported actions of the destination, or 0 to indicate that a drop will
;;;     not be accepted
;;;
;;; preferred :
;;;     A unique action that's a member of actions indicating the preferred
;;;     action.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_finish ()
;;;
;;; void
;;; gdk_drop_finish (GdkDrop *self,
;;;                  GdkDragAction action);
;;;
;;; Ends the drag operation after a drop.
;;;
;;; The action must be a single action selected from the actions available via
;;; gdk_drop_get_actions().
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; action :
;;;     the action performed by the destination or 0 if the drop failed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_async ()
;;;
;;; void
;;; gdk_drop_read_async (GdkDrop *self,
;;;                      const char **mime_types,
;;;                      int io_priority,
;;;                      GCancellable *cancellable,
;;;                      GAsyncReadyCallback callback,
;;;                      gpointer user_data);
;;;
;;; Asynchronously read the dropped data from a GdkDrop in a format that
;;; complies with one of the mime types.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; mime_types :
;;;     pointer to an array of mime types.
;;;
;;; io_priority :
;;;     the io priority for the read operation
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     a GAsyncReadyCallback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_finish ()
;;;
;;; GInputStream *
;;; gdk_drop_read_finish (GdkDrop *self,
;;;                       GAsyncResult *result,
;;;                       const char **out_mime_type,
;;;                       GError **error);
;;;
;;; Finishes an async drop read operation, see gdk_drop_read_async().
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; out_mime_type :
;;;     return location for the used mime type.
;;;
;;; error :
;;;     location to store error information on failure, or NULL.
;;;
;;; Returns :
;;;     the GInputStream, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_value_async ()
;;;
;;; void
;;; gdk_drop_read_value_async (GdkDrop *self,
;;;                            GType type,
;;;                            int io_priority,
;;;                            GCancellable *cancellable,
;;;                            GAsyncReadyCallback callback,
;;;                            gpointer user_data);
;;;
;;; Asynchronously request the drag operation's contents converted to the given
;;; type . When the operation is finished callback will be called. You can then
;;; call gdk_drop_read_value_finish() to get the resulting GValue.
;;;
;;; For local drag'n'drop operations that are available in the given GType, the
;;; value will be copied directly. Otherwise, GDK will try to use
;;; gdk_content_deserialize_async() to convert the data.
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; type :
;;;     a GType to read
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     callback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_value_finish ()
;;;
;;; const GValue *
;;; gdk_drop_read_value_finish (GdkDrop *self,
;;;                             GAsyncResult *result,
;;;                             GError **error);
;;;
;;; Finishes an async drop read started with gdk_drop_read_value_async().
;;;
;;; self :
;;;     a GdkDrop
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     a GValue containing the result.
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.drop.lisp ---------------------------------------------
