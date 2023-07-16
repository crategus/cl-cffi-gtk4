;;; ----------------------------------------------------------------------------
;;; gtk4.drop-target-async.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 -2023 Dieter Kaiser
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
;;; GtkDropTargetAsync
;;;
;;;     Event controller to receive DND drops
;;;
;;; Types and Values
;;;
;;;     GtkDropTargetAsync
;;;
;;; Accessors
;;;
;;;     gtk_drop_target_async_set_actions
;;;     gtk_drop_target_async_get_actions
;;;     gtk_drop_target_async_set_formats
;;;     gtk_drop_target_async_get_formats
;;;
;;;
;;; Functions
;;;
;;;     gtk_drop_target_async_new
;;;     gtk_drop_target_async_reject_drop
;;;
;;; Properties
;;;
;;;     actions
;;;     formats
;;;
;;; Signals
;;;
;;;     accept
;;;     drag-enter
;;;     drag-leave
;;;     drag-motion
;;;     drop
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkDropTargetAsync
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropTargetAsync
;;;
;;; GtkDropTargetAsync is an auxiliary object that can be used to receive
;;; Drag-and-Drop operations. It is the more complete but also more complex
;;; method of handling drop operations compared to GtkDropTarget and you should
;;; only use it if GtkDropTarget doesn't provide all the features you need.
;;;
;;; To use a GtkDropTargetAsync to receive drops on a widget, you create a
;;; GtkDropTargetAsync object, configure which data formats and actions you
;;; support, connect to its signals, and then attach it to the widget with
;;; gtk_widget_add_controller().
;;;
;;; During a drag operation, the first signal that a GtkDropTargetAsync emits is
;;; “accept”, which is meant to determine whether the target is a possible drop
;;; site for the ongoing drop. The default handler for the ::accept signal
;;; accepts the drop if it finds a compatible data format and an action that is
;;; supported on both sides.
;;;
;;; If it is, and the widget becomes a target, you will receive a “drag-enter”
;;; signal, followed by “drag-motion” signals as the pointer moves, optionally
;;; a “drop” signal when a drop happens, and finally a “drag-leave” signal when
;;; the pointer moves off the widget.
;;;
;;; The ::drag-enter and ::drag-motion handler return a GdkDragAction to update
;;; the status of the ongoing operation. The ::drop handler should decide if it
;;; ultimately accepts the drop and if it does, it should initiate the data
;;; transfer and finish the operation by calling gdk_drop_finish().
;;;
;;; Between the ::drag-enter and ::drag-leave signals the widget is a current
;;; drop target, and will receive the GTK_STATE_FLAG_DROP_ACTIVE state, which
;;; can be used by themes to style the widget as a drop target.
;;;
;;; Signal Details
;;;
;;; The “accept” signal
;;;
;;; gboolean
;;; user_function (GtkDropTargetAsync *self,
;;;                GdkDrop            *drop,
;;;                gpointer            user_data)
;;;
;;; The ::accept signal is emitted on the drop site when a drop operation is
;;; about to begin.
;;;
;;; If the drop is not accepted, FALSE will be returned and the drop target will
;;; ignore the drop. If TRUE is returned, the drop is accepted for now but may
;;; be rejected later via a call to gtk_drop_target_async_reject() or ultimately
;;; by returning FALSE from “drop”.
;;;
;;; The default handler for this signal decides whether to accept the drop based
;;; on the formats provided by the drop .
;;;
;;; If the decision whether the drop will be accepted or rejected needs further
;;; processing, such as inspecting the data, this function should return TRUE
;;; and proceed as is drop was accepted and if it decides to reject the drop
;;; later, it should call gtk_drop_target_async_reject_drop().
;;;
;;; self :
;;;     the GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; Returns :
;;;     TRUE if drop is accepted
;;;
;;; Flags: Run Last
;;;
;;; The “drag-enter” signal
;;;
;;; GdkDragAction
;;; user_function (GtkDropTargetAsync *self,
;;;                GdkDrop            *drop,
;;;                double              x,
;;;                double              y,
;;;                gpointer            user_data)
;;;
;;; The ::drag-enter signal is emitted on the drop site when the pointer enters
;;; the widget. It can be used to set up custom highlighting.
;;;
;;; self :
;;;     the GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     Preferred action for this drag operation.
;;;
;;; Flags: Run Last
;;;
;;; The “drag-leave” signal
;;;
;;; void
;;; user_function (GtkDropTargetAsync *self,
;;;                GdkDrop            *drop,
;;;                gpointer            user_data)
;;;
;;; The ::drag-leave signal is emitted on the drop site when the pointer leaves
;;; the widget. Its main purpose it to undo things done in “drag-enter”.
;;;
;;; self :
;;;     the GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; Flags: Run Last
;;;
;;; The “drag-motion” signal
;;;
;;; GdkDragAction
;;; user_function (GtkDropTargetAsync *self,
;;;                GdkDrop            *drop,
;;;                double              x,
;;;                double              y,
;;;                gpointer            user_data)
;;;
;;; The ::drag-motion signal is emitted while the pointer is moving over the
;;; drop target.
;;;
;;; self :
;;;     the GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     Preferred action for this drag operation.
;;;
;;; Flags: Run Last
;;;
;;; The “drop” signal
;;;
;;; gboolean
;;; user_function (GtkDropTargetAsync *self,
;;;                GdkDrop            *drop,
;;;                double              x,
;;;                double              y,
;;;                gpointer            user_data)
;;;
;;; The ::drop signal is emitted on the drop site when the user drops the data
;;; onto the widget. The signal handler must determine whether the pointer
;;; position is in a drop zone or not. If it is not in a drop zone, it returns
;;; FALSE and no further processing is necessary.
;;;
;;; Otherwise, the handler returns TRUE. In this case, this handler will accept
;;; the drop. The handler must ensure that gdk_drop_finish() is called to let
;;; the source know that the drop is done. The call to gdk_drop_finish() must
;;; only be done when all data has been received.
;;;
;;; To receive the data, use one of the read functions provides by GdkDrop such
;;; as gdk_drop_read_async() or gdk_drop_read_value_async().
;;;
;;; self :
;;;     the GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop
;;;
;;; x :
;;;     the x coordinate of the current pointer position
;;;
;;; y :
;;;     the y coordinate of the current pointer position
;;;
;;; Returns :
;;;     whether the drop is accepted at the given pointer position
;;;
;;; Flags: Run Last
;;;
;;; See Also
;;;
;;;     GtkDropTarget
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
;;; Owner: GtkDropTargetAsync
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_set_actions ()
;;;
;;; void
;;; gtk_drop_target_async_set_actions (GtkDropTargetAsync *self,
;;;                                    GdkDragAction actions);
;;;
;;; Sets the actions that this drop target supports.
;;;
;;; self :
;;;     a GtkDropTargetAsync
;;;
;;; actions :
;;;     the supported actions
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_get_actions ()
;;;
;;; GdkDragAction
;;; gtk_drop_target_async_get_actions (GtkDropTargetAsync *self);
;;;
;;; Gets the actions that this drop target supports.
;;;
;;; self :
;;;     a GtkDropTargetAsync
;;;
;;; Returns :
;;;     the actions that this drop target supports
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The GdkContentFormats that determines the supported data formats
;;;
;;; Owner: GtkDropTargetAsync
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_set_formats ()
;;;
;;; void
;;; gtk_drop_target_async_set_formats (GtkDropTargetAsync *self,
;;;                                    GdkContentFormats *formats);
;;;
;;; Sets the data formats that this drop target will accept.
;;;
;;; self :
;;;     a GtkDropTargetAsync
;;;
;;; formats :
;;;     the supported data formats or NULL for any format.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_get_formats ()
;;;
;;; GdkContentFormats *
;;; gtk_drop_target_async_get_formats (GtkDropTargetAsync *self);
;;;
;;; Gets the data formats that this drop target accepts.
;;;
;;; If the result is NULL, all formats are expected to be supported.
;;;
;;; self :
;;;     a GtkDropTargetAsync
;;;
;;; Returns :
;;;     the supported data formats.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_new ()
;;;
;;; GtkDropTargetAsync *
;;; gtk_drop_target_async_new (GdkContentFormats *formats,
;;;                            GdkDragAction actions);
;;;
;;; Creates a new GtkDropTargetAsync object.
;;;
;;; formats :
;;;     the supported data formats.
;;;
;;; actions :
;;;     the supported actions
;;;
;;; Returns :
;;;     the new GtkDropTargetAsync
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_reject_drop ()
;;;
;;; void
;;; gtk_drop_target_async_reject_drop (GtkDropTargetAsync *self,
;;;                                    GdkDrop *drop);
;;;
;;; Sets the drop as not accepted on this drag site.
;;;
;;; This function should be used when delaying the decision on whether to accept
;;; a drag or not until after reading the data.
;;;
;;; self :
;;;     a GtkDropTargetAsync
;;;
;;; drop :
;;;     the GdkDrop of an ongoing drag operation
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.drop-target-async.lisp --------------------------------
