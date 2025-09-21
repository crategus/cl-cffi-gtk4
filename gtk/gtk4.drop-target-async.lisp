;;; ----------------------------------------------------------------------------
;;; gtk4.drop-target-async.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 -2025 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDropTargetAsync" drop-target-async
  (:superclass event-controller
   :export t
   :interfaces ()
   :type-initializer "gtk_drop_target_async_get_type")
  ((actions
    drop-target-async-actions
    "actions" "GdkDragAction" t t)
   (formats
    drop-target-async-formats
    "formats" "GdkContentFormats" t t)))

#+liber-documentation
(setf (documentation 'drop-target-async 'type)
 "@version{2025-07-19}
  @begin{short}
    The @class{gtk:drop-target-async} object is an auxiliary object that can be
    used to receive Drag-and-Drop operations.
  @end{short}
  It is the more complete but also more complex method of handling drop
  operations compared to GtkDropTarget and you should only use it if
  @class{gtk:drop-target} object does not provide all the features you need.

  To use a @class{gtk:drop-target-async} object to receive drops on a widget,
  you create a @class{gtk:drop-target-async} object, configure which data
  formats and actions you support, connect to its signals, and then attach it to
  the widget with the @fun{gtk:widget-add-controller} function.

  During a drag operation, the first signal that a @class{gtk:drop-target-async}
  object emits is the @sig[gtk:drop-target-async]{accept} signal, which is meant
  to determine whether the target is a possible drop site for the ongoing drop.
  The default handler for the @sig[gtk:drop-target-async]{accept} signal accepts
  the drop if it finds a compatible data format and an action that is supported
  on both sides.

  If it is, and the widget becomes a target, you will receive a
  @sig[gtk:drop-target-async]{drag-enter} signal, followed by
  @sig[gtk:drop-target-async]{drag-motion} signals as the pointer moves,
  optionally a @sig[gtk:drop-target-async]{drop} signal when a drop happens,
  and finally a @sig[gtk:drop-target-async]{drag-leave} signal when the pointer
  moves off the widget.

  The @sig[gtk:drop-target-async]{drag-enter} and
  @sig[gtk:drop-target-async]{drag-motion} handler return a
  @sym{gdk:drag-action} value to update the status of the ongoing operation.
  The @sig[gtk:drop-target-async]{drop} signal handler should decide if it
  ultimately accepts the drop and if it does, it should initiate the data
  transfer and finish the operation by calling the @fun{gdk:drop-finish}
  function.

  Between the @sig[gtk:drop-target-async]{drag-enter} and
  @sig[gtk:drop-target-async]{drag-leave} signals the widget is a current drop
  target, and will receive the @val[gtk:state-flags]{:drop-active} state of
  the @sym{gtk:state-flags} flags, which can be used by themes to style the
  widget as a drop target.
  @begin[Signal Details]{dictionary}
    @begin[drop-target-async::accept]{signal}
      @begin{pre}
lambda (target drop)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[target]{The @class{gtk:drop-target-async} object.}
        @entry[drop]{The @class{gdk:drop} object.}
        @entry[Returns]{@em{True} if the drop is accepted.}
      @end{simple-table}
      The signal is emitted on the drop site when a drop operation is about to
      begin.  If the drop is not accepted, @em{false} will be returned and the
      drop target will ignore the drop. If @em{true} is returned, the drop is
      accepted for now but may be rejected later via a call to the
      @fun{gtk:drop-target-async-reject-drop} function or ultimately by
      returning @em{false} from the @sig[gtk:drop-target-async]{drop} signal
      handler. The default handler for this signal decides whether to accept the
      drop based on the formats provided by the @arg{drop} object. If the
      decision whether the drop will be accepted or rejected needs further
      processing, such as inspecting the data, this function should return
      @em{true} and proceed as this drop was accepted and if it decides to
      reject the drop later, it should call the
      @fun{gtk:drop-target-async-reject-drop} function.
    @end{signal}
    @begin[drop-target-async::drag-enter]{signal}
      @begin{pre}
lambda (target drop x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[target]{The @class{gtk:drop-target-async} object.}
        @entry[drop]{The @class{gdk:drop} object.}
        @entry[x]{The double float for the x coordinate of the current pointer
          position.}
        @entry[y]{The double float for the y coordinate of the current pointer
          position.}
        @entry[Returns]{The boolean for the preferred action for this drag
          operation.}
      @end{simple-table}
      The signal is emitted on the drop site when the pointer enters the widget.
      It can be used to set up custom highlighting.
    @end{signal}
    @begin[drop-target-async::drag-leave]{signal}
      @begin{pre}
lambda (target drop)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[target]{The @class{gtk:drop-target-async} object.}
        @entry[drop]{The @class{gdk:drop} object.}
      @end{simple-table}
      The signal is emitted on the drop site when the pointer leaves the widget.
      Its main purpose it to undo things done in the
      @sig[gtk:drop-target-async]{drag-enter} signal handler.
    @end{signal}
    @begin[drop-target-async::drag-motion]{signal}
      @begin{pre}
lambda (target drop x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[target]{The @class{gtk:drop-target-async} object.}
        @entry[drop]{The @class{gdk:drop} object.}
        @entry[x]{The double float for the x coordinate of the current pointer
          position.}
        @entry[y]{The double float for the y coordinate of the current pointer
          position.}
        @entry[Returns]{The boolean for the preferred action for this drag
          operation.}
      @end{simple-table}
      The signal is emitted while the pointer is moving over the drop target.
    @end{signal}
    @begin[drop-target-async::drop]{signal}
      @begin{pre}
lambda (target drop x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[target]{The @class{gtk:drop-target-async} object.}
        @entry[drop]{The @class{gdk:drop} object.}
        @entry[x]{The double float for the x coordinate of the current pointer
          position.}
        @entry[y]{The double float for the y coordinate of the current pointer
          position.}
        @entry[Returns]{Whether the drop is accepted at the given pointer
          position.}
      @end{simple-table}
      The signal is emitted on the drop site when the user drops the data onto
      the widget. The signal handler must determine whether the pointer position
      is in a drop zone or not. If it is not in a drop zone, it returns
      @em{false} and no further processing is necessary. Otherwise, the handler
      returns @em{true}. In this case, this handler will accept the drop. The
      handler must ensure that the @fun{gdk:drop-finish} function is called to
      let the source know that the drop is done. The call to the
      @fun{gdk:drop-finish} function must only be done when all data has been
      received. To receive the data, use one of the read functions provides by
      the @class{gdk:drop} object such as the @fun{gdk:drop-read-async} or
      @fun{gdk:drop-read-value-async} functions.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:drop-target-async-new}
  @see-slot{gtk:drop-target-async-actions}
  @see-slot{gtk:drop-target-async-formats}
  @see-class{gtk:drop-target}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:drop-target-async-actions ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "actions" 'drop-target-async) t)
 "The @code{actions} property of type @sym{gdk:drag-action} (Read / Write) @br{}
  The actions that this drop target supports.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-async-actions)
      "Accessor"
      (documentation 'drop-target-async-actions 'function)
 "@version{#2025-09-21}
  @syntax{(gtk:drop-target-async-actions object) => actions}
  @syntax{(setf (gtk:drop-target-async-actions object) actions)}
  @argument[object]{a @class{gtk:drop-target-async} object}
  @argument[actions]{a @sym{gdk:drag-action} value}
  @begin{short}
    The accessor for the @slot[gtk:drop-target-async]{actions} slot of the
    @class{gtk:drop-target-async} class gets or sets the actions that this drop
    target supports.
  @end{short}
  @see-class{gtk:drop-target-async}
  @see-symbol{gdk:drag-action}")

;;; --- gtk:drop-target-async-formats ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'drop-target-async) t)
 "The @code{formats} property of type @class{gdk:content-formats} (Read / Write)
  @br{}
  The content formats that determines the supported data formats.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-async-formats)
      "Accessor"
      (documentation 'drop-target-async-formats 'function)
 "@version{#2025-09-21}
  @syntax{(gtk:drop-target-async-formats object) => formats}
  @syntax{(setf (gtk:drop-target-async-formats object) formats)}
  @argument[object]{a @class{gtk:drop-target-async} object}
  @argument[formats]{a @class{gdk:content-formats} instance for the supported
    data formats}
  @begin{short}
    The accessor for the @slot[gtk:drop-target-async]{formats} slot of the
    @class{gtk:drop-target-async} class gets or sets the data formats that this
    drop target accepts.
  @end{short}
  If the result is @code{nil}, all formats are expected to be supported.
  @see-class{gtk:drop-target-async}
  @see-class{gdk:content-formats}")

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_target_async_new" drop-target-async-new)
    (g:object drop-target-async :return)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[formats]{a @class{gdk:content-formats} instance for the supported
    data formats}
  @argument[actions]{a @sym{gdk:drag-action} value for the supported actions}
  @return{The new @class{gtk:drop-target-async} object.}
  @short{Creates a new @class{gtk:drop-target-async} object.}
  @see-class{gtk:drop-target-async}
  @see-class{gdk:content-formats}
  @see-symbol{gdk:drag-action}"
  (formats (g:boxed gdk:content-formats))
  (actions gdk:drag-action))

(export 'drop-target-async-new)

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_async_reject_drop
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_target_async_reject_drop"
               drop-target-async-reject-drop) :void
 #+liber-documentation
 "@version{#2023-09-29}
  @argument[target]{a @class{gtk:drop-target-async} object}
  @argument[drop]{a @class{gdk:drop} object of an ongoing drag operation}
  @begin{short}
    Sets the drop as not accepted on this drag site.
  @end{short}
  This function should be used when delaying the decision on whether to accept
  a drag or not until after reading the data.
  @see-class{gtk:drop-target-async}
  @see-class{gdk:drop}"
  (target (g:object drop-target-async))
  (drop (g:object gdk:drop)))

(export 'drop-target-async-reject-drop)

;;; --- End of file gtk4.drop-target-async.lisp --------------------------------
