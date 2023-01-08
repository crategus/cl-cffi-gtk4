;;; ----------------------------------------------------------------------------
;;; gdk.drag-and-drop.lisp
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
;;; Drag And Drop
;;;
;;;     Functions for controlling drag and drop handling
;;;
;;; Types and Values
;;;
;;;     GdkDrag
;;;     GdkDrop
;;;
;;;     GdkDragCancelReason
;;;     GdkDragAction
;;;
;;;     GDK_ACTION_ALL
;;;
;;;     GdkDragSurface
;;;     GdkDragSurfaceInterface
;;;
;;; Accessors
;;;
;;;     gdk_drag_get_actions
;;;     gdk_drag_get_content
;;;     gdk_drag_get_device
;;;     gdk_drag_get_display
;;;     gdk_drag_get_formats
;;;     gdk_drag_get_selected_action
;;;     gdk_drag_get_surface
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
;;;     gdk_drag_drop_done
;;;     gdk_drag_begin
;;;     gdk_drag_get_drag_surface
;;;     gdk_drag_set_hotspot
;;;     gdk_drag_action_is_unique
;;;     gdk_drag_surface_present
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
;;;     content
;;;     device
;;;     display
;;;     formats
;;;     selected-action
;;;     surface
;;;
;;;     actions
;;;     device
;;;     display
;;;     drag
;;;     formats
;;;     surface
;;;
;;; Signals
;;;
;;;     cancel
;;;     dnd-finished
;;;     drop-performed
;;;
;;;Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkDragSurface
;;;
;;;     GObject
;;;     ├── GdkDrag
;;;     ╰── GdkDrop
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkDragCancelReason
;;;
;;; Used in GdkDrag to the reason of a cancelled DND operation.
;;;
;;; GDK_DRAG_CANCEL_NO_TARGET
;;;     There is no suitable drop target.
;;;
;;; GDK_DRAG_CANCEL_USER_CANCELLED
;;;     Drag cancelled by the user
;;;
;;; GDK_DRAG_CANCEL_ERROR
;;;     Unspecified error.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkDragCanelReason" drag-cancel-reason
  (:export t
   :type-initializer "gdk_drag_cancel_reason_get_type")
  :no-target
  :user-cancelled
  :error)

;;; ----------------------------------------------------------------------------
;;; enum GdkDragAction
;;;
;;; Used in GdkDrop and GdkDrag to indicate the actions that the destination
;;; can and should do with the dropped data.
;;;
;;; GDK_ACTION_COPY
;;;     Copy the data.
;;;
;;; GDK_ACTION_MOVE
;;;     Move the data, i.e. first copy it, then delete it from the source using
;;;     the DELETE target of the X selection protocol.
;;;
;;; GDK_ACTION_LINK
;;;     Add a link to the data. Note that this is only useful if source and
;;;     destination agree on what it means, and is not supported on all
;;;     platforms.
;;;
;;; GDK_ACTION_ASK
;;;     Ask the user what to do with the data.
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkDragAction" drag-action
  (:export t
   :type-initializer "gdk_drag_action_get_type")
  :copy
  :move
  :link
  :ask)

;;; ----------------------------------------------------------------------------
;;; GDK_ACTION_ALL
;;;
;;; #define GDK_ACTION_ALL (GDK_ACTION_COPY | GDK_ACTION_MOVE | GDK_ACTION_LINK)
;;;
;;; Defines all possible DND actions. This can be used in gdk_drop_status()
;;; messages when any drop can be accepted or a more specific drop method is
;;; not yet known.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkDragSurface
;;;
;;; typedef struct _GdkDragSurface GdkDragSurface;
;;;
;;; A GdkDragSurface is an interface implemented by GdkSurfaces used during a
;;; DND operation.
;;; ----------------------------------------------------------------------------

(define-g-interface "GdkDragSurface" drag-surface
  (:export t
   :type-initializer "gdk_drag_surface_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; GdkDragSurfaceInterface
;;;
;;; typedef struct _GdkDragSurfaceInterface GdkDragSurfaceInterface;
;;;
;;; The GdkDragSurfaceInterface implementation is private to GDK.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkDrag
;;;
;;; The GdkDrag struct contains only private fields and should not be accessed
;;; directly.
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
;;;
;;; Signal Details
;;;
;;; The “cancel” signal
;;;
;;; void
;;; user_function (GdkDrag            *drag,
;;;                GdkDragCancelReason reason,
;;;                gpointer            user_data)
;;;
;;; The drag operation was cancelled.
;;;
;;; drag :
;;;     The object on which the signal is emitted
;;;
;;; reason :
;;;     The reason the drag was cancelled
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;;
;;; The “dnd-finished” signal
;;;
;;; void
;;; user_function (GdkDrag *drag,
;;;                gpointer user_data)
;;;
;;; The drag operation was finished, the destination finished reading all data.
;;; The drag object can now free all miscellaneous data.
;;;
;;; drag :
;;;     The object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;;
;;; The “drop-performed” signal
;;;
;;; void
;;; user_function (GdkDrag *drag,
;;;                gpointer user_data)
;;;
;;; The drag operation was performed on an accepting client.
;;;
;;; drag :
;;;     The object on which the signal is emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkDrag" drag
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_drag_get_type")
  ((actions
    drag-actions
    "actions" "GdkDragAction" t t)
   (content
    drag-content
    "content" "GdkContentProvider" t nil)
   (device
    drag-device
    "device" "GdkDevice" t nil)
   (display
    drag-display
    "display" "GdkDisplay" t nil)
   (formats
    drag-formats
    "formats" "GdkContentFormats" t nil)
   (selected-action
    drag-selected-action
    "selected-action" "GdkDragAction" t t)
   (surface
    drag-surface
    "surface" "GdkSurface" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- drag-actions -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “actions” property
;;;
;;;  “actions”                  GdkDragAction
;;;
;;; The possible actions.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_actions ()
;;;
;;; GdkDragAction
;;; gdk_drag_get_actions (GdkDrag *drag);
;;;
;;; Determines the bitmask of possible actions proposed by the source.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     the GdkDragAction flags
;;; ----------------------------------------------------------------------------

;;; --- drag-content -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “content” property
;;;
;;;  “content”                  GdkContentProvider *
;;;
;;; The GdkContentProvider.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_content ()
;;;
;;; GdkContentProvider *
;;; gdk_drag_get_content (GdkDrag *drag);
;;;
;;; Returns the GdkContentProvider associated to the GdkDrag object.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     The GdkContentProvider associated to drag .
;;; ----------------------------------------------------------------------------

;;; --- drag-device ------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “device” property
;;;
;;;  “device”                   GdkDevice *
;;;
;;; The GdkDevice that is performing the drag.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_device ()
;;;
;;; GdkDevice *
;;; gdk_drag_get_device (GdkDrag *drag);
;;;
;;; Returns the GdkDevice associated to the GdkDrag object.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     The GdkDevice associated to drag .
;;; ----------------------------------------------------------------------------

;;; --- drag-display -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “display” property
;;;
;;;  “display”                  GdkDisplay *
;;;
;;; The GdkDisplay that the drag belongs to.
;;;
;;; Owner: GdkDrag
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_display ()
;;;
;;; GdkDisplay *
;;; gdk_drag_get_display (GdkDrag *drag);
;;;
;;; Gets the GdkDisplay that the drag object was created for.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     a GdkDisplay.
;;; ----------------------------------------------------------------------------

;;; --- drag-formats -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The possible formats that the drag can provide its data in.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_drag_get_formats (GdkDrag *drag);
;;;
;;; Retrieves the formats supported by this GdkDrag object.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     a GdkContentFormats.
;;; ----------------------------------------------------------------------------

;;; --- drag-selected-action ---------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “selected-action” property
;;;
;;;  “selected-action”          GdkDragAction
;;;
;;; The currently selected action.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_selected_action ()
;;;
;;; GdkDragAction
;;; gdk_drag_get_selected_action (GdkDrag *drag);
;;;
;;; Determines the action chosen by the drag destination.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     a GdkDragAction value
;;; ----------------------------------------------------------------------------

;;; --- drag-surface -----------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “surface” property
;;;
;;;  “surface”                  GdkSurface *
;;;
;;; The surface where the drag originates.
;;;
;;; Owner: GdkDrag
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_surface ()
;;;
;;; GdkSurface *
;;; gdk_drag_get_surface (GdkDrag *drag);
;;;
;;; Returns the GdkSurface where the drag originates.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     The GdkSurface where the drag originates.
;;; ----------------------------------------------------------------------------


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

(define-g-object-class "GdkDrop" drop
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
;;; gdk_drag_drop_done ()
;;;
;;; void
;;; gdk_drag_drop_done (GdkDrag *drag,
;;;                     gboolean success);
;;;
;;; Inform GDK if the drop ended successfully. Passing FALSE for success may
;;; trigger a drag cancellation animation.
;;;
;;; This function is called by the drag source, and should be the last call
;;; before dropping the reference to the drag .
;;;
;;; The GdkDrag will only take the first gdk_drag_drop_done() call as effective,
;;; if this function is called multiple times, all subsequent calls will be
;;; ignored.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; success :
;;;     whether the drag was ultimatively successful
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin ()
;;;
;;; GdkDrag *
;;; gdk_drag_begin (GdkSurface *surface,
;;;                 GdkDevice *device,
;;;                 GdkContentProvider *content,
;;;                 GdkDragAction actions,
;;;                 double dx,
;;;                 double dy);
;;;
;;; Starts a drag and creates a new drag context for it.
;;;
;;; This function is called by the drag source. After this call, you probably
;;; want to set up the drag icon using the surface returned by
;;; gdk_drag_get_drag_surface().
;;;
;;; This function returns a reference to the GdkDrag object, but GTK keeps its
;;; own reference as well, as long as the DND operation is going on.
;;;
;;; Note: if actions include GDK_ACTION_MOVE, you need to listen for the “dnd-
;;; finished” signal and delete the data at the source if
;;; gdk_drag_get_selected_action() returns GDK_ACTION_MOVE.
;;;
;;; surface :
;;;     the source surface for this drag
;;;
;;; device :
;;;     the device that controls this drag
;;;
;;; content :
;;;     the offered content.
;;;
;;; actions :
;;;     the actions supported by this drag
;;;
;;; dx :
;;;     the x offset to device 's position where the drag nominally started
;;;
;;; dy :
;;;     the y offset to device 's position where the drag nominally started
;;;
;;; Returns :
;;;     a newly created GdkDrag or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_drag_surface ()
;;;
;;; GdkSurface *
;;; gdk_drag_get_drag_surface (GdkDrag *drag);
;;;
;;; Returns the surface on which the drag icon should be rendered during the
;;; drag operation. Note that the surface may not be available until the drag
;;; operation has begun. GDK will move the surface in accordance with the
;;; ongoing drag operation. The surface is owned by drag and will be destroyed
;;; when the drag operation is over.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     the drag surface, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_set_hotspot ()
;;;
;;; void
;;; gdk_drag_set_hotspot (GdkDrag *drag,
;;;                       int hot_x,
;;;                       int hot_y);
;;;
;;; Sets the position of the drag surface that will be kept under the cursor
;;; hotspot. Initially, the hotspot is at the top left corner of the drag
;;; surface.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; hot_x :
;;;     x coordinate of the drag surface hotspot
;;;
;;; hot_y :
;;;     y coordinate of the drag surface hotspot
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_action_is_unique ()
;;;
;;; gboolean
;;; gdk_drag_action_is_unique (GdkDragAction action);
;;;
;;; Checks if action represents a single action or if it includes multiple flags
;;; that can be selected from.
;;;
;;; When action is 0 - ie no action was given, TRUE is returned.
;;;
;;; action :
;;;     a GdkDragAction
;;;
;;; Returns :
;;;     TRUE if exactly one action was given
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_drag_surface_present ()
;;;
;;; gboolean
;;; gdk_drag_surface_present (GdkDragSurface *drag_surface,
;;;                           int width,
;;;                           int height);
;;;
;;; Present drag_surface .
;;;
;;; drag_surface :
;;;     the GdkDragSurface to show
;;;
;;; width :
;;;     the unconstrained drag_surface width to layout
;;;
;;; height :
;;;     the unconstrained drag_surface height to layout
;;;
;;; Returns :
;;;     FALSE if it failed to be presented, otherwise TRUE.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_drag_surface_present" drag-surface-present) :boolean
  (surface (g:object drag-surface))
  (width :int)
  (height :int))

(export 'drag-surface-present)

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

;;; --- End of file gdk.drag-and-drop.lisp -------------------------------------
