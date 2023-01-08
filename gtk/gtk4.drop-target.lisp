;;; ----------------------------------------------------------------------------
;;; gtk.drop-target.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkDropTarget
;;;
;;;     Event controller to receive DND drops
;;;
;;; Types and Values
;;;
;;;     GtkDropTarget
;;;
;;; Accessors
;;;
;;;     gtk_drop_target_set_actions
;;;     gtk_drop_target_get_actions
;;;     gtk_drop_target_get_drop
;;;     gtk_drop_target_get_formats
;;;     gtk_drop_target_set_preload
;;;     gtk_drop_target_get_preload
;;;     gtk_drop_target_get_value
;;;
;;; Functions
;;;
;;;     gtk_drop_target_new
;;;     gtk_drop_target_set_gtypes
;;;     gtk_drop_target_get_gtypes
;;;     gtk_drop_target_reject
;;;
;;; Properties
;;;
;;;     actions
;;;     drop
;;;     formats
;;;     preload
;;;     value
;;;
;;; Signals
;;;
;;;     accept
;;;     drop
;;;     enter
;;;     leave
;;;     motion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkDropTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropTarget
;;;
;;; GtkDropTarget is an event controller implementing a simple way to receive
;;; Drag-and-Drop operations.
;;;
;;; The most basic way to use a GtkDropTarget to receive drops on a widget is
;;; to create it via gtk_drop_target_new() passing in the GType of the data you
;;; want to receive and connect to the GtkDropTarget::drop signal to receive
;;; the data.
;;;
;;; GtkDropTarget supports more options, such as:
;;;
;;; rejecting potential drops via the “accept” signal and the
;;; gtk_drop_target_reject() function to let other drop targets handle the drop
;;;
;;; tracking an ongoing drag operation before the drop via the “enter”, “motion”
;;; and “leave” signals
;;;
;;; configuring how to receive data by setting the “preload” property and
;;; listening for its availability via the “value” property
;;;
;;; However, GtkDropTarget is ultimately modeled in a synchronous way and only
;;; supports data transferred via GType. If you want full control over an
;;; ongoing drop, the GtkDropTargetAsync object gives you this ability.
;;;
;;; While a pointer is dragged over the drop target's widget and the drop has
;;; not been rejected, that widget will receive the GTK_STATE_FLAG_DROP_ACTIVE
;;; state, which can be used to style the widget.
;;;
;;; Signal Details
;;;
;;; The “accept” signal
;;;
;;; gboolean
;;; user_function (GtkDropTarget *self,
;;;                GdkDrop       *drop,
;;;                gpointer       user_data)
;;;
;;; The ::accept signal is emitted on the drop site when a drop operation is
;;; about to begin. If the drop is not accepted, FALSE will be returned and the
;;; drop target will ignore the drop. If TRUE is returned, the drop is accepted
;;; for now but may be rejected later via a call to gtk_drop_target_reject() or
;;; ultimately by returning FALSE from GtkDropTarget::drop
;;;
;;; The default handler for this signal decides whether to accept the drop based
;;; on the formats provided by the drop .
;;;
;;; If the decision whether the drop will be accepted or rejected needs
;;; inspecting the data, this function should return TRUE, the
;;; GtkDropTarget:preload property should be set and the value should be
;;; inspected via the GObject::notify:value signal and then call
;;; gtk_drop_target_reject().
;;;
;;; self :
;;;     the GtkDropTarget
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; Returns :
;;;     TRUE if drop is accepted
;;;
;;; Flags: Run Last
;;;
;;; The “drop” signal
;;;
;;; gboolean
;;; user_function (GtkDropTarget *self,
;;;                GValue        *value,
;;;                double         x,
;;;                double         y,
;;;                gpointer       user_data)
;;;
;;; The ::drop signal is emitted on the drop site when the user drops the data
;;; onto the widget. The signal handler must determine whether the pointer
;;; position is in a drop zone or not. If it is not in a drop zone, it returns
;;; FALSE and no further processing is necessary.
;;;
;;; Otherwise, the handler returns TRUE. In this case, this handler will accept
;;; the drop. The handler is responsible for rading the given value and
;;; performing the drop operation.
;;;
;;; self :
;;;     the GtkDropTarget
;;;
;;; value :
;;;     the GValue being dropped
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     whether the drop was accepted at the given pointer position
;;;
;;; Flags: Run Last
;;;
;;; The “enter” signal
;;;
;;; GdkDragAction
;;; user_function (GtkDropTarget *self,
;;;                double         x,
;;;                double         y,
;;;                gpointer       user_data)
;;;
;;; The ::enter signal is emitted on the drop site when the pointer enters the
;;; widget. It can be used to set up custom highlighting.
;;;
;;; self :
;;;     the GtkDropTarget
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     Preferred action for this drag operation or 0 if dropping is not
;;;     supported at the current x ,y location.
;;;
;;; Flags: Run Last
;;;
;;; The “leave” signal
;;;
;;; void
;;; user_function (GtkDropTarget *self,
;;;                gpointer       user_data)
;;;
;;; The ::leave signal is emitted on the drop site when the pointer leaves the
;;; widget. Its main purpose it to undo things done in “enter”.
;;;
;;; self :
;;;     the GtkDropTarget
;;;
;;; Flags: Run Last
;;;
;;; The “motion” signal
;;;
;;; GdkDragAction
;;; user_function (GtkDropTarget *self,
;;;                double         x,
;;;                double         y,
;;;                gpointer       user_data)
;;;
;;; The ::motion signal is emitted while the pointer is moving over the drop
;;; target.
;;;
;;; self :
;;;     the GtkDropTarget
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     Preferred action for this drag operation or 0 if dropping is not
;;;     supported at the current x ,y location.
;;;
;;; Flags: Run Last
;;;
;;; See Also :
;;;
;;;      GdkDrop, GtkDropTargetAsync
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “actions” property
;;;
;;;  “actions”                  GdkDragAction
;;;
;;; The GdkDragActions that this drop target supports
;;;
;;; Owner: GtkDropTarget
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_set_actions ()
;;;
;;; void
;;; gtk_drop_target_set_actions (GtkDropTarget *self,
;;;                              GdkDragAction actions);
;;;
;;; Sets the actions that this drop target supports.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; actions :
;;;     the supported actions
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_actions ()
;;;
;;; GdkDragAction
;;; gtk_drop_target_get_actions (GtkDropTarget *self);
;;;
;;; Gets the actions that this drop target supports.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; Returns :
;;;     the actions that this drop target supports
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “drop” property
;;;
;;;   “drop”                     GdkDrop *
;;;
;;; The GdkDrop that is currently being performed
;;;
;;; Owner: GtkDropTarget
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_drop ()
;;;
;;; GdkDrop *
;;; gtk_drop_target_get_drop (GtkDropTarget *self);
;;;
;;; Gets the currently handled drop operation.
;;;
;;; If no drop operation is going on, NULL is returned.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; Returns :
;;;     The current drop.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The GdkContentFormats that determine the supported data formats
;;;
;;; Owner: GtkDropTarget
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_formats ()
;;;
;;; GdkContentFormats *
;;; gtk_drop_target_get_formats (GtkDropTarget *self);
;;;
;;; Gets the data formats that this drop target accepts.
;;;
;;; If the result is NULL, all formats are expected to be supported.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; Returns :
;;;     the supported data formats.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “preload” property
;;;
;;;  “preload”                  gboolean
;;;
;;; Whether the drop data should be preloaded when the pointer is only hovering
;;; over the widget but has not been released.
;;;
;;; Setting this property allows finer grained reaction to an ongoing drop at
;;; the cost of loading more data.
;;;
;;; The default value for this property is FALSE to avoid downloading huge
;;; amounts of data by accident. For example, if somebody drags a full document
;;; of gigabytes of text from a text editor across a widget with a preloading
;;; drop target, this data will be downloaded, even if the data is ultimately
;;; dropped elsewhere.
;;;
;;; For a lot of data formats, the amount of data is very small (like
;;; GDK_TYPE_RGBA), so enabling this property does not hurt at all. And for
;;; local-only drag'n'drop operations, no data transfer is done, so enabling it
;;; there is free.
;;;
;;; Owner: GtkDropTarget
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_set_preload ()
;;;
;;; void
;;; gtk_drop_target_set_preload (GtkDropTarget *self,
;;;                              gboolean preload);
;;;
;;; Sets the GtkDropTarget:preload property.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; preload :
;;;     TRUE to preload drop data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_preload ()
;;;
;;; gboolean
;;; gtk_drop_target_get_preload (GtkDropTarget *self);
;;;
;;; Gets the value of the GtkDropTarget:preload property.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; Returns :
;;;     TRUE if drop data should be preloaded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “value” property
;;;
;;;  “value”                    GValue *
;;;
;;; The value for this drop operation or NULL if the data has not been loaded
;;; yet or no drop operation is going on.
;;;
;;; Data may be available before the GtkDropTarget::drop signal gets emitted -
;;; for example when the GtkDropTarget:preload property is set. You can use the
;;; GObject::notify signal to be notified of available data.
;;;
;;; Owner: GtkDropTarget
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_value ()
;;;
;;; const GValue *
;;; gtk_drop_target_get_value (GtkDropTarget *self);
;;;
;;; Gets the value of the GtkDropTarget:value property.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; Returns :
;;;     The current drop data.
;;; ----------------------------------------------------------------------------



;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_new ()
;;;
;;; GtkDropTarget *
;;; gtk_drop_target_new (GType type,
;;;                      GdkDragAction actions);
;;;
;;; Creates a new GtkDropTarget object.
;;;
;;; If the drop target should support more than 1 type, pass G_TYPE_INVALID for
;;; type and then call gtk_drop_target_set_gtypes().
;;;
;;; type :
;;;     The supported type or G_TYPE_INVALID
;;;
;;; actions :
;;;     the supported actions
;;;
;;; Returns :
;;;     the new GtkDropTarget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_set_gtypes ()
;;;
;;; void
;;; gtk_drop_target_set_gtypes (GtkDropTarget *self,
;;;                             GType *types,
;;;                             gsize n_types);
;;;
;;; Sets the supported GTypes for this drop target.
;;;
;;; The GtkDropTarget::drop signal will
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; types :
;;;     all supported GTypes that can be dropped.
;;;
;;; n_types :
;;;     number of types
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_get_gtypes ()
;;;
;;; const GType *
;;; gtk_drop_target_get_gtypes (GtkDropTarget *self,
;;;                             gsize *n_types);
;;;
;;; Gets the list of supported GTypes for self . If no type have been set, NULL
;;; will be returned.
;;;
;;; self :
;;;     a GtkDropTarget
;;;
;;; n_types :
;;;     optional pointer to take the number of GTypes contained in the return
;;;     value.
;;;
;;; Returns :
;;;     G_TYPE_INVALID-terminated array of types included in formats or NULL if
;;;     none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_reject ()
;;;
;;; void
;;; gtk_drop_target_reject (GtkDropTarget *self);
;;;
;;; Rejects the ongoing drop operation.
;;;
;;; If no drop operation is ongoing - when GdkDropTarget:drop returns NULL -
;;; this function does nothing.
;;;
;;; This function should be used when delaying the decision on whether to accept
;;; a drag or not until after reading the data.
;;;
;;; self :
;;;     a GtkDropTarget
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.drop-target.lisp ---------------------------------------
