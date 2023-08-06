;;; ----------------------------------------------------------------------------
;;; gdk4.drag.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; enum GdkDragCancelReason
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkDragCanelReason" drag-cancel-reason
  (:export t
   :type-initializer "gdk_drag_cancel_reason_get_type")
  :no-target
  :user-cancelled
  :error)

#+liber-documentation
(setf (liber:alias-for-symbol 'drag-cancel-reason)
      "GEnum"
      (liber:symbol-documentation 'drag-cancel-reason)
 "@version{#2023-8-4}
  @begin{short}
    Used in the @class{gdk:drag} object to the reason of a cancelled DND 
    operation.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkDragCanelReason\" drag-cancel-reason
  (:export t
   :type-initializer \"gdk_drag_cancel_reason_get_type\")
  :no-target
  :user-cancelled
  :error)  
  @end{pre}
  @begin[code]{table}
    @entry[:no-target]{There is no suitable drop target.}
    @entry[:user-cancelled]{Drag cancelled by the user.}
    @entry[:error]{Unspecified error.}
  @end{table}
  @see-class{gdk:drag}")

;;; ----------------------------------------------------------------------------
;;; enum GdkDragAction
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkDragAction" drag-action
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
 "@version{#2023-7-23}
  @begin{short}
    Used in @class{gdk:drop} and @class{gdk:drag} objects to indicate the
    actions that the destination can and should do with the dropped data.
  @end{short}
  @begin{pre}
(gobject:define-g-flags \"GdkDragAction\" drag-action
  (:export t
   :type-initializer \"gdk_drag_action_get_type\")
  (:none 0)
  (:copy #.(ash 1 0))
  (:move #.(ash 1 1))
  (:link #.(ash 1 2))
  (:ask #.(ash 1 3)))
  @end{pre}
  @begin[code]{table}
    @entry[:copy]{Copy the data.}
    @entry[:move]{Move the data, i.e. first copy it, then delete it from the
      source using the @code{DELETE} target of the X selection protocol.}
    @entry[:link]{Add a link to the data. Note that this is only useful if
      source and destination agree on what it means, and is not supported on
      all platforms.}
    @entry[:ask]{Ask the user what to do with the data.}
  @end{table}
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

(gobject:define-g-object-class "GdkDrag" drag
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
 "@version{#2023-8-4}
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
    @subheading{The \"cancel\" signal}
      @begin{pre}
lambda (drag reason)    :run-last
      @end{pre}
      The drag operation was cancelled.
      @begin[code]{table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is 
          emitted.}
        @entry[reason]{A @symbol{gdk:drag-cancel-reason} value with the reason 
        the drag was cancelled.}
      @end{table}
    @subheading{The \"dnd-finished\" signal}
      @begin{pre}
lambda (drag)    :run-last
      @end{pre}
      The drag operation was finished, the destination finished reading all 
      data. The drag object can now free all miscellaneous data.
      @begin[code]{table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is 
          emitted.}
      @end{table}
    @subheading{The \"drop-performed\" signal}
      @begin{pre}
lambda (drag)    :run-last
      @end{pre}
      The drag operation was performed on an accepting client.
      @begin[code]{table}
        @entry[drag]{The @class{gdk:drag} object on which the signal is 
          emitted.}
      @end{table}
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

;;; --- drag-actions -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "actions" 'drag) t)
 "The @code{actions} property of type @symbol{gdk:drag-action} (Read / Write) 
  @br{}
  The possible actions of the drag. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'drag-actions)
      "Accessor"
      (documentation 'drag-actions 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-actions object) => actions}
  @argument[object]{a @class{gdk:drag} object}
  @argument[actions]{a @symbol{gdk:drag-action} value}
  @begin{short}
    Accessor of the @slot[gdk:drag]{actions} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-actions} function determines the possible actions proposed 
  by the drag source.
  @see-class{gdk:drag}
  @see-symbol{gdk:drag-action}")

;;; --- drag-content -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content" 'drag) t)
 "The @code{content} property of type @class{gdk:content-provider} 
  (Read / Write / Construct Only) @br{}
  The content provider.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-content)
      "Accessor"
      (documentation 'drag-content 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-content object) => content}
  @argument[object]{a @class{gdk:drag} object}
  @argument[content]{a @class{gdk:content-provider} object associated to
    @arg{object}}
  @begin{short}
    Accessor of the @slot[gdk:drag]{content} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-content} function returns the content provider associated 
  to the drag object.  
  @see-class{gdk:drag}
  @see-class{gdk:content-provider}")

;;; --- drag-device ------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "device" 'drag) t)
 "The @code{device} property of type @class{gdk:device} 
  (Read / Write / Construct Only) @br{}
  The device that is performing the drag.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-device)
      "Accessor"
      (documentation 'drag-device 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-device object) => device}
  @argument[object]{a @class{gdk:drag} object}
  @argument[device]{a @class{gdk:device} object associated to @arg{object}}
  @begin{short}
    Accessor of the @slot[gdk:drag]{device} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-device} function returns the device associated to the drag 
  object.  
  @see-class{gdk:drag}
  @see-class{gdk:device}")

;;; --- drag-display -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'drag) t)
 "The @code{display} property of type @class{gdk:display} (Read) @br{}
  The display that the drag belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-display)
      "Accessor"
      (documentation 'drag-display 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-display object) => display}
  @argument[object]{a @class{gdk:drag} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:drag]{display} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-display} function returns the display that the drag object
  was created for. 
  @see-class{gdk:drag}
  @see-class{gdk:display}")

;;; --- drag-formats -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'drag) t)
 "The @code{formats} property of type @class{gdk:content-formats} 
  (Read / Write / Construct Only) @br{}
  The possible content formats that the drag can provide its data in.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-formats)
      "Accessor"
      (documentation 'drag-formats 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-formats object) => formats}
  @argument[object]{a @class{gdk:drag} object}
  @argument[formats]{a @class{gdk:content-formats} object}
  @begin{short}
    Accessor of the @slot[gdk:drag]{formats} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-formats} function retrieves the formats supported by the 
  drag object.
  @see-class{gdk:drag}
  @see-class{gdk:content-formats}")

;;; --- drag-selected-action ---------------------------------------------------

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
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-selected-action object) => action}
  @argument[object]{a @class{gdk:drag} object}
  @argument[action]{a @symbol{gdk:drag-action} value}
  @begin{short}
    Accessor of the @slot[gdk:drag]{selected-action} slot of the 
    @class{gdk:drag} class.
  @end{short}
  The @fun{gdk:drag-selected-action} function determines the action chosen by 
  the drag destination.  
  @see-class{gdk:drag}
  @see-symbol{gdk:drag-action}")

;;; --- drag-surface -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'drag) t)
 "The @code{surface} property of type @class{gdk:surface} 
  (Read / Write / Construct Only) @br{}
  The surface where the drag originates.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-surface)
      "Accessor"
      (documentation 'drag-surface 'function)
 "@version{#2023-8-4}
  @syntax[]{(gdk:drag-surface object) => surface}
  @argument[object]{a @class{gdk:drag} object}
  @argument[surface]{a @class{gdk:surface} object where the drag originates}
  @begin{short}
    Accessor of the @slot[gdk:drag]{surface} slot of the @class{gdk:drag}
    class.
  @end{short}
  The @fun{gdk:drag-surface} function returns the surface where the drag
  originates. 
  @see-class{gdk:drag}
  @see-class{gdk:surface}")

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

(cffi:defcfun ("gdk_drag_drop_done" drag-drop-done) :void
  (drag (g:object drag))
  (success :boolean))

(export 'drag-drop-done)

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

(cffi:defcfun ("gdk_drag_begin" drag-begin) (g:object drag)
  (surface (g:object surface))
  (device (g:object device))
  (content (g:object content-provider))
  (actions drag-action)
  (dx :double)
  (dy :double))

(export 'drag-begin)

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

(cffi:defcfun ("gdk_drag_drag_surface" drag-drag-surface) (g:object surface)
  (drag (g:object drag)))

(export 'drag-drag-surface)

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

(cffi:defcfun ("gdk_drag_set_hotspot" drag-set-hotspot) :void
  (drag (g:object drag))
  (xhot :int)
  (yhot :int))

(export 'drag-set-hot-spot)

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

(cffi:defcfun ("gdk_drag_action_is_unique" drag-action-is-unique) :boolean
  (action drag-action))

(export 'drag-action-is-unique)

;;; --- End of file gdk4.drag.lisp ---------------------------------------------
