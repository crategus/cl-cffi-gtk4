;;; ----------------------------------------------------------------------------
;;; gdk4.drag.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; Drag And Drop
;;;
;;;     Functions for controlling drag and drop handling
;;;
;;; Types and Values
;;;
;;;     GdkDrag
;;;
;;;     GdkDragCancelReason
;;;     GdkDragAction
;;;
;;;     GDK_ACTION_ALL
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
;;; Functions
;;;
;;;     gdk_drag_drop_done
;;;     gdk_drag_begin
;;;     gdk_drag_get_drag_surface
;;;     gdk_drag_set_hotspot
;;;     gdk_drag_action_is_unique
;;;
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
;;; Signals
;;;
;;;     cancel
;;;     dnd-finished
;;;     drop-performed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDrag
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDragCancelReason
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkDragCancelReason" drag-cancel-reason
  (:export t
   :type-initializer "gdk_drag_cancel_reason_get_type")
  :no-target
  :user-cancelled
  :error)

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-cancel-reason)
      "GEnum"
      (liber:symbol-documentation 'drag-cancel-reason)
 "@version{2025-07-26}
  @begin{declaration}
(gobject:define-genum \"GdkDragCancelReason\" drag-cancel-reason
  (:export t
   :type-initializer \"gdk_drag_cancel_reason_get_type\")
  :no-target
  :user-cancelled
  :error)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:no-target]{There is no suitable drop target.}
      @entry[:user-cancelled]{Drag cancelled by the user.}
      @entry[:error]{Unspecified error.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used in the @class{gdk:drag} object to the reason of a cancelled DND
    operation.
  @end{short}
  @see-class{gdk:drag}")

;;; ----------------------------------------------------------------------------
;;; GdkDragAction
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkDragAction" drag-action
  (:export t
   :type-initializer "gdk_drag_action_get_type")
  (:none 0)
  (:copy #.(ash 1 0))
  (:move #.(ash 1 1))
  (:link #.(ash 1 2))
  (:ask #.(ash 1 3)))

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-action)
      "GFlags"
      (liber:symbol-documentation 'drag-action)
 "@version{2025-07-26}
  @begin{declaration}
(gobject:define-gflags \"GdkDragAction\" drag-action
  (:export t
   :type-initializer \"gdk_drag_action_get_type\")
  (:none 0)
  (:copy #.(ash 1 0))
  (:move #.(ash 1 1))
  (:link #.(ash 1 2))
  (:ask #.(ash 1 3)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:copy]{Copy the data.}
      @entry[:move]{Move the data, that is, first copy it, then delete it from
        the source using the @code{DELETE} target of the X selection protocol.}
      @entry[:link]{Add a link to the data. Note that this is only useful if
        source and destination agree on what it means, and is not supported on
        all platforms.}
      @entry[:ask]{Ask the user what to do with the data.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used in @class{gdk:drop} and @class{gdk:drag} objects to indicate the
    actions that the destination can and should do with the dropped data.
  @end{short}
  @see-function{gdk:drag}
  @see-function{gdk:drop}")

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
;;; GdkDrag
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDrag" drag
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

#+liber-documentation
(setf (documentation 'drag 'type)
 "@version{2025-07-31}
  @begin{short}
    The @class{gdk:drag} object represents the source of an ongoing DND
    operation.
  @end{short}
  A @class{gdk:drag} object is created when a drag is started, and stays alive
  for duration of the DND operation. After a drag has been started with the
  @fun{gdk:drag-begin} function, the caller gets informed about the status of
  the ongoing drag operation with signals on the @class{gdk:drag} object.

  GTK provides a higher level abstraction based on top of these functions, and
  so they are not normally needed in GTK applications. See the \"Drag and Drop\"
  section of the GTK documentation for more information.
  @begin[Signal Details]{dictionary}
    @begin[drag::cancel]{signal}
      @begin{pre}
lambda (drag reason)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is
          emitted.}
        @entry[reason]{The @sym{gdk:drag-cancel-reason} value with the reason
          the drag was cancelled.}
      @end{simple-table}
      The drag operation was cancelled.
    @end{signal}
    @begin[drag::dnd-finished]{signal}
      @begin{pre}
lambda (drag)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is
          emitted.}
      @end{simple-table}
      The drag operation was finished, the destination finished reading all
      data. The drag object can now free all miscellaneous data.
    @end{signal}
    @begin[drag::drop-performed]{signal}
      @begin{pre}
lambda (drag)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is
          emitted.}
      @end{simple-table}
      The drag operation was performed on an accepting client.
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:drag-actions}
  @see-slot{gdk:drag-content}
  @see-slot{gdk:drag-device}
  @see-slot{gdk:drag-display}
  @see-slot{gdk:drag-formats}
  @see-slot{gdk:drag-selected-action}
  @see-slot{gdk:drag-surface}
  @see-class{gdk:drop}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:drag-actions -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "actions" 'drag) t)
 "The @code{actions} property of type @sym{gdk:drag-action} (Read / Write) @br{}
  The possible actions of the drag. @br{}
  Default value: @val[gdk:drag-action]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'drag-actions)
      "Accessor"
      (documentation 'drag-actions 'function)
 "@version{#2025-07-31}
  @syntax{(gdk:drag-actions object) => actions}
  @argument[object]{a @class{gdk:drag} object}
  @argument[actions]{a @sym{gdk:drag-action} value}
  @begin{short}
    The accessor for the @slot[gdk:drag]{actions} slot of the @class{gdk:drag}
    class determines the possible actions proposed by the drag source.
  @end{short}
  @see-class{gdk:drag}
  @see-symbol{gdk:drag-action}")

;;; --- gdk:drag-content -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content" 'drag) t)
 "The @code{content} property of type @class{gdk:content-provider}
  (Read / Write / Construct Only) @br{}
  The content provider.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-content)
      "Accessor"
      (documentation 'drag-content 'function)
 "@version{#2025-09-29}
  @syntax{(gdk:drag-content object) => content}
  @argument[object]{a @class{gdk:drag} object}
  @argument[content]{a @class{gdk:content-provider} object associated to
    @arg{object}}
  @begin{short}
    The accessor for the @slot[gdk:drag]{content} slot of the @class{gdk:drag}
    class returns the content provider associated to the drag object.
  @end{short}
  @see-class{gdk:drag}
  @see-class{gdk:content-provider}")

;;; --- gdk:drag-device --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "device" 'drag) t)
 "The @code{device} property of type @class{gdk:device}
  (Read / Write / Construct Only) @br{}
  The device that is performing the drag.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-device)
      "Accessor"
      (documentation 'drag-device 'function)
 "@version{#2025-09-29}
  @syntax{(gdk:drag-device object) => device}
  @argument[object]{a @class{gdk:drag} object}
  @argument[device]{a @class{gdk:device} object associated to @arg{object}}
  @begin{short}
    The accessor for the @slot[gdk:drag]{device} slot of the @class{gdk:drag}
    class returns the device associated to the drag object.
  @end{short}
  @see-class{gdk:drag}
  @see-class{gdk:device}")

;;; --- gdk:drag-display -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'drag) t)
 "The @code{display} property of type @class{gdk:display} (Read) @br{}
  The display that the drag belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-display)
      "Accessor"
      (documentation 'drag-display 'function)
 "@version{#2025-09-29}
  @syntax{(gdk:drag-display object) => display}
  @argument[object]{a @class{gdk:drag} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    The accessor for the @slot[gdk:drag]{display} slot of the @class{gdk:drag}
    class returns the display that the drag object was created for.
  @end{short}
  @see-class{gdk:drag}
  @see-class{gdk:display}")

;;; --- gdk:drag-formats -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'drag) t)
 "The @code{formats} property of type @class{gdk:content-formats}
  (Read / Write / Construct Only) @br{}
  The possible content formats that the drag can provide its data in.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-formats)
      "Accessor"
      (documentation 'drag-formats 'function)
 "@version{#2025-09-29}
  @syntax{(gdk:drag-formats object) => formats}
  @argument[object]{a @class{gdk:drag} object}
  @argument[formats]{a @class{gdk:content-formats} object}
  @begin{short}
    The accessor for the @slot[gdk:drag]{formats} slot of the @class{gdk:drag}
    class returns the formats supported by the drag object.
  @end{short}
  @see-class{gdk:drag}
  @see-class{gdk:content-formats}")

;;; --- gdk:drag-selected-action -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected-action" 'drag) t)
 "The @code{selected-action} property of type @class{gdk:drag-action}
  (Read / Write) @br{}
  The currently selected action of the drag. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'drag-selected-action)
      "Accessor"
      (documentation 'drag-selected-action 'function)
 "@version{#2025-07-31}
  @syntax{(gdk:drag-selected-action object) => action}
  @argument[object]{a @class{gdk:drag} object}
  @argument[action]{a @sym{gdk:drag-action} value}
  @begin{short}
    The accessor for the @slot[gdk:drag]{selected-action} slot of the
    @class{gdk:drag} class determines the action chosen by the drag destination.
  @end{short}
  @see-class{gdk:drag}
  @see-symbol{gdk:drag-action}")

;;; --- gdk:drag-surface -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'drag) t)
 "The @code{surface} property of type @class{gdk:surface}
  (Read / Write / Construct Only) @br{}
  The surface where the drag originates.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-surface)
      "Accessor"
      (documentation 'drag-surface 'function)
 "@version{#2025-09-29}
  @syntax{(gdk:drag-surface object) => surface}
  @argument[object]{a @class{gdk:drag} object}
  @argument[surface]{a @class{gdk:surface} object where the drag originates}
  @begin{short}
    The accessor for the @slot[gdk:drag]{surface} slot of the @class{gdk:drag}
    class returns the surface where the drag originates.
  @end{short}
  @see-class{gdk:drag}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; gdk_drag_drop_done
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_drop_done" drag-drop-done) :void
 #+liber-documentation
 "@version{#2024-01-07}
  @argument[drag]{a @class{gdk:drag} object}
  @argument[success]{a boolean whether the drag was ultimatively successful}
  @begin{short}
    Inform GDK if the drop ended successfully.
  @end{short}
  Passing @em{false} for @arg{success} may trigger a drag cancellation
  animation. This function is called by the drag source, and should be the last
  call before dropping the reference to the drag object.

  The @class{gdk:drag} object will only take the first @fun{gdk:drag-drop-done}
  function call as effective, if this function is called multiple times, all
  subsequent calls will be ignored.
  @see-class{gdk:drag}"
  (drag (g:object drag))
  (success :boolean))

(export 'drag-drop-done)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_begin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_begin" drag-begin) (g:object drag)
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[surface]{a @class{gdk:surface} object for this drag object}
  @argument[device]{a @class{gdk:device} object that controls the drag object}
  @argument[content]{a @class{gdk:content-provider} object for the offered
    content}
  @argument[actions]{a @sym{gdk:drag-action} value for the actions supported
    by this drag object}
  @argument[dx]{a double float for the x offset to the position of the device
    where the drag nominally started}
  @argument[dy]{a double float for the y offset to the position of the device
    where the drag nominally started}
  @return{The newly created @class{gdk:drag} object of @code{nil} on error.}
  @begin{short}
    Starts a drag and creates a new drag context for it.
  @end{short}
  This function is called by the drag source. After this call, you probably
  want to set up the drag icon using the surface returned by the
  @fun{gdk:drag-drag-surface} function.

  This function returns a reference to the @class{gdk:drag} object, but GTK
  keeps its own reference as well, as long as the DND operation is going on.

  Note: If @arg{actions} include @val[gdk:drag-action]{:move}, you need to
  listen for the @sig[gdk:drag]{dnd-finished} signal and delete the data at the
  source if the @fun{gdk:drag-selected-action} function returns
  @val[gdk:drag-action]{:move}.
  @see-class{gdk:drag}
  @see-class{gdk:surface}
  @see-class{gdk:device}
  @see-class{gdk:content-provider}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drag-drag-surface}
  @see-function{gdk:drag-selected-action}"
  (surface (g:object surface))
  (device (g:object device))
  (content (g:object content-provider))
  (actions drag-action)
  (dx :double)
  (dy :double))

(export 'drag-begin)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_get_drag_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_drag_surface" drag-drag-surface) (g:object surface)
 #+liber-documentation
 "@version{#2024-01-07}
  @argument[drag]{a @class{gdk:drag} object}
  @return{The @class{gdk:surface} object or @code{nil}.}
  @begin{short}
    Returns the surface on which the drag icon should be rendered during the
    drag operation.
  @end{short}
  Note that the surface may not be available until the drag operation has begun.
  GDK will move the surface in accordance with the ongoing drag operation. The
  surface is owned by @arg{drag} and will be destroyed when the drag operation
  is over.
  @see-class{gdk:drag}
  @see-class{gdk:surface}"
  (drag (g:object drag)))

(export 'drag-drag-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_set_hotspot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_set_hotspot" drag-set-hotspot) :void
 #+liber-documentation
 "@version{#2025-08-01}
  @argument[drag]{a @class{gdk:drag} object}
  @argument[xhotspot]{an integer for the x coordinate of the drag surface
    hotspot}
  @argument[yhotspot]{an integer for the y coordinate of the drag surface
    hotspot}
  @begin{short}
    Sets the position of the drag surface that will be kept under the cursor
    hotspot.
  @end{short}
  Initially, the hotspot is at the top left corner of the drag surface.
  @see-class{gdk:drag}"
  (drag (g:object drag))
  (xhotspot :int)
  (yhotspot :int))

(export 'drag-set-hotspot)

;;; ----------------------------------------------------------------------------
;;; gdk_drag_action_is_unique
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drag_action_is_unique" drag-action-is-unique) :boolean
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[action]{a @sym{gdk:drag-action} value}
  @return{@em{True} if exactly one action was given.}
  @begin{short}
    Checks if @arg{action} represents a single action or if it includes
    multiple flags that can be selected from.
  @end{short}
  When @arg{action} is @code{:none}, that is no action was given, @em{true} is
  returned.
  @see-class{gdk:drag}
  @see-symbol{gdk:drag-action}"
  (action drag-action))

(export 'drag-action-is-unique)

;;; --- End of file gdk4.drag.lisp ---------------------------------------------
