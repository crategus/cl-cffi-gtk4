;;; ----------------------------------------------------------------------------
;;; gtk4.drag-source.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkDragSource
;;;
;;;     Event controller to initiate DND operations
;;;
;;; Types and Values
;;;
;;;     GtkDragSource
;;;
;;; Accessors
;;;
;;;     gtk_drag_source_set_actions
;;;     gtk_drag_source_get_actions
;;;     gtk_drag_source_set_content
;;;     gtk_drag_source_get_content
;;;
;;; Functions
;;;
;;;     gtk_drag_source_new
;;;     gtk_drag_source_set_icon
;;;     gtk_drag_source_drag_cancel
;;;     gtk_drag_source_get_drag
;;;
;;;     gtk_drag_check_threshold
;;;
;;; Properties
;;;
;;;     actions
;;;     content
;;;
;;; Signals
;;;
;;;     drag-begin
;;;     drag-cancel
;;;     drag-end
;;;     prepare
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkDragSource
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDragSource
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkDragSource" drag-source
  (:superclass gesture-single
   :export t
   :interfaces ()
   :type-initializer "gtk_drag_source_get_type")
  ((actions
    drag-source-actions
    "actions" "GdkDragAction" t t)
   (content
    drag-source-content
    "content" "GdkContentProvider" t t)))

#+liber-documentation
(setf (documentation 'drag-source 'type)
 "@version{#2022-7-31}
  @begin{short}
    The @sym{gtk:drag-source} object is an auxiliary object that is used to
    initiate Drag and Drop operations.
  @end{short}
  It can be set up with the necessary ingredients for a DND operation ahead of
  time. This includes the source for the data that is being transferred, in the
  form of a @class{gdk-content-provider} object, the desired action, and the
  icon to use during the drag operation. After setting it up, the drag source
  must be added to a widget as an event controller, using the
  @fun{gtk:widget-add-controller} function.

  Setting up the content provider and icon ahead of time only makes sense when
  the data does not change. More commonly, you will want to set them up just in
  time. To do so, the @sym{gtk:drag-source} object has \"prepare\" and
  \"drag-begin\" signals. The \"prepare\" signal is emitted before a drag is
  started, and can be used to set the content provider and actions that the drag
  should be started with. The \"drag-begin\" signal is emitted after the
  @class{gdk-drag} object has been created, and can be used to set up the drag
  icon.

  During the DND operation, the @sym{gtk:drag-source} object emits signals that
  can be used to obtain updates about the status of the operation, but it is not
  normally necessary to connect to any signals, except for one case: when the
  supported actions include the @code{:move} value of the
  @symbol{gdk-drag-action} flags, you need to listen for the \"drag-end\" signal
  and delete the data after it has been transferred.
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-begin\" signal}
      @begin{pre}
lambda (source drag)    :run-last
      @end{pre}
      The signal is emitted on the drag source when a drag is started. It can
      be used to e.g. set a custom drag icon with the
      @fun{gtk:drag-source-set-icon} function.
      @begin[code]{table}
        @entry[source]{A @sym{gtk:drag-source} object.}
        @entry[drag]{A @class{gdk-drag} object.}
      @end{table}
    @subheading{The \"drag-cancel\" signal}
      @begin{pre}
lambda (source drag reason)    :      run-last
      @end{pre}
      The signal is emitted on the drag source when a drag has failed. The
      signal handler may handle a failed drag operation based on the type of
      error. It should return @em{true} if the failure has been handled and the
      default \"drag operation failed\" animation should not be shown.
      @begin[code]{table}
        @entry[source]{A @sym{gtk:drag-source} object.}
        @entry[drag]{A @class{gdk-drag} object.}
        @entry[reason]{A @symbol{gdk-drag-cancel-reason} value with the
          information on why the drag failed.}
        @entry[Returns]{@em{True} if the failed drag operation has been already
          handled.}
      @end{table}
    @subheading{The \"drag-end\" signal}
      @begin{pre}
lambda (source drag delete)    :run-last
      @end{pre}
      The signal is emitted on the drag source when a drag is finished. A
      typical reason to connect to this signal is to undo things done in the
      \"prepare\" or \"drag-begin\" handler.
      @begin[code]{table}
        @entry[source]{A @sym{gtk:drag-source} object.}
        @entry[drag]{A @class{gdk-drag} object.}
        @entry[delete]{@em{True} if the drag was performing @code{:move}, and
        the data should be deleted.}
        @entry[Returns]{@em{True} if the failed drag operation has been already
          handled.}
      @end{table}
    @subheading{The \"prepare\" signal}
      @begin{pre}
lambda (source x y)    :run-last
      @end{pre}
      The signal is emitted when a drag is about to be initiated. It returns the
      @class{gdk-content-provider} object to use for the drag that is about to
      start. The default handler for this signal returns the value of the
      \"content\" property, so if you set up that property ahead of time, you
      do not need to connect to this signal.
      @begin[code]{table}
        @entry[source]{A @sym{gtk:drag-source} object.}
        @entry[x]{A double float with the x coordinate of the drag starting
          point.}
        @entry[y]{A double float with the y coordinate of the drag starting
          point.}
        @entry[Returns]{A @class{gdk-content-provider} object, or @code{nil}.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:drag-source-actions}
  @see-slot{gtk:drag-source-content}
  @see-constructor{gtk:drag-source-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “actions” property
;;;
;;;  “actions”                  GdkDragAction
;;;
;;; The actions that are supported by drag operations from the source.
;;;
;;; Note that you must handle the “drag-end” signal if the actions include
;;; GDK_ACTION_MOVE.
;;;
;;; Owner: GtkDragSource
;;;
;;; Flags: Read / Write
;;;
;;; Default value: GDK_ACTION_COPY
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “content” property
;;;
;;;  “content”                  GdkContentProvider *
;;;
;;; The data that is offered by drag operations from this source, in the form
;;; of a GdkContentProvider.
;;;
;;; Owner: GtkDragSource
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_new ()
;;;
;;; GtkDragSource *
;;; gtk_drag_source_new (void);
;;;
;;; Creates a new GtkDragSource object.
;;;
;;; Returns :
;;;     the new GtkDragSource
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_content ()
;;;
;;; void
;;; gtk_drag_source_set_content (GtkDragSource *source,
;;;                              GdkContentProvider *content);
;;;
;;; Sets a content provider on a GtkDragSource.
;;;
;;; When the data is requested in the cause of a DND operation, it will be
;;; obtained from the content provider.
;;;
;;; This function can be called before a drag is started, or in a handler for
;;; the “prepare” signal.
;;;
;;; You may consider setting the content provider back to NULL in a “drag-end”
;;; signal handler.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; content :
;;;     a GdkContentProvider, or NULL.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_get_content ()
;;;
;;; GdkContentProvider *
;;; gtk_drag_source_get_content (GtkDragSource *source);
;;;
;;; Gets the current content provider of a GtkDragSource.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; Returns :
;;;     the GdkContentProvider of source .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_actions ()
;;;
;;; void
;;; gtk_drag_source_set_actions (GtkDragSource *source,
;;;                              GdkDragAction actions);
;;;
;;; Sets the actions on the GtkDragSource.
;;;
;;; During a DND operation, the actions are offered to potential drop targets.
;;; If actions include GDK_ACTION_MOVE, you need to listen to the “drag-end”
;;; signal and handle delete_data being TRUE.
;;;
;;; This function can be called before a drag is started, or in a handler for
;;; the “prepare” signal.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; actions :
;;;     the actions to offer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_get_actions ()
;;;
;;; GdkDragAction
;;; gtk_drag_source_get_actions (GtkDragSource *source);
;;;
;;; Gets the actions that are currently set on the GtkDragSource.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; Returns :
;;;     the actions set on source
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_set_icon ()
;;;
;;; void
;;; gtk_drag_source_set_icon (GtkDragSource *source,
;;;                           GdkPaintable *paintable,
;;;                           int hot_x,
;;;                           int hot_y);
;;;
;;; Sets a paintable to use as icon during DND operations.
;;;
;;; The hotspot coordinates determine the point on the icon that gets aligned
;;; with the hotspot of the cursor.
;;;
;;; If paintable is NULL, a default icon is used.
;;;
;;; This function can be called before a drag is started, or in a “prepare” or
;;; “drag-begin” signal handler.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; paintable :
;;;     the GdkPaintable to use as icon, or NULL.
;;;
;;; hot_x :
;;;     the hotspot X coordinate on the icon
;;;
;;; hot_y :
;;;     the hotspot Y coordinate on the icon
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gtk_drag_source_drag_cancel ()
;;;
;;; void
;;; gtk_drag_source_drag_cancel (GtkDragSource *source);
;;;
;;; Cancels a currently ongoing drag operation.
;;;
;;; source :
;;;     a GtkDragSource
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_source_get_drag ()
;;;
;;; GdkDrag *
;;; gtk_drag_source_get_drag (GtkDragSource *source);
;;;
;;; Returns the underlying GdkDrag object for an ongoing drag.
;;;
;;; source :
;;;     a GtkDragSource
;;;
;;; Returns :
;;;     the GdkDrag of the current drag operation, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_check_threshold ()
;;;
;;; gboolean
;;; gtk_drag_check_threshold (GtkWidget *widget,
;;;                           int start_x,
;;;                           int start_y,
;;;                           int current_x,
;;;                           int current_y);
;;;
;;; Checks to see if a mouse drag starting at (start_x , start_y ) and ending at
;;; (current_x , current_y ) has passed the GTK drag threshold, and thus should
;;; trigger the beginning of a drag-and-drop operation.
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; start_x :
;;;     X coordinate of start of drag
;;;
;;; start_y :
;;;     Y coordinate of start of drag
;;;
;;; current_x :
;;;     current X coordinate
;;;
;;; current_y :
;;;     current Y coordinate
;;;
;;; Returns :
;;;     TRUE if the drag threshold has been passed.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.drag-source.lisp --------------------------------------
