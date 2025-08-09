;;; ----------------------------------------------------------------------------
;;; gdk4.drop.lisp
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDrop" drop
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

#+liber-documentation
(setf (documentation 'drop 'type)
 "@version{#2025-07-31}
  @begin{short}
    The @class{gdk:drop} object represents the target side of an ongoing DND
    operation.
  @end{short}
  Possible drop sites get informed about the status of the ongoing drag
  operation with events of @code{:enter}, @code{:leave}, @code{:motion} and
  @code{:start} type. The @class{gdk:drop} object can be obtained from these
  @class{gdk:events} events using the @fun{gdk:dnd-event-drop} function.

  The actual data transfer is initiated from the target side via an async read,
  using one of the @class{gdk:drop} functions for this purpose:
  the @fun{gdk:drop-read-async} or @fun{gdk:drop-read-value-async} functions.

  GTK provides a higher level abstraction based on top of these functions, and
  so they are not normally needed in GTK applications. See the Drag and Drop
  section of the GTK documentation for more information.
  @see-class{gdk:drag}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:drop-actions -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "actions" 'drop) t)
 "The @code{actions} property of type @sym{gdk:drag-action}
  (Read / Write / Construct Only) @br{}
  The possible actions for the drop. @br{}
  Default value: '(:copy :move :link)")

#+liber-documentation
(setf (liber:alias-for-function 'drop-actions)
      "Accessor"
      (documentation 'drop-actions 'function)
 "@version{#2025-07-31}
  @syntax{(gdk:drop-actions object) => actions}
  @argument[object]{a @class{gdk:drop} object}
  @argument[actions]{a @sym{gdk:drag-action} value}
  @begin{short}
    The accessor for the @slot[gdk:drop]{actions} slot of the @class{gdk:drop}
    class returns the possible actions for the drop object.
  @end{short}
  If this value contains multiple actions, that is, the
  @fun{gdk:drag-action-is-unique} function returns @em{false} for the result,
  the @fun{gdk:drop-finish} function must choose the action to use when
  accepting the drop. This will only happen if you passed
  @val[gdk:drag-action]{:ask} as one of the possible actions in the
  @fun{gdk:drop-status} function. The @val[gdk:drag-action]{:ask} value itself
  will not be included in the actions returned by this function.

  This value may change over the lifetime of the drop object both as a response
  to source side actions as well as to calls to the @fun{gdk:drop-status}
  function or the @fun{gdk:drop-finish} function. The source side will not
  change this value anymore once a drop has started.
  @see-class{gdk:drop}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drop-status}
  @see-function{gdk:drop-finish}
  @see-function{gdk:drag-action-is-unique}")

;;; --- gdk:drop-device --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "device" 'drop) t)
 "The @code{device} property of type @class{gdk:device}
  (Read / Write / Construct Only) @br{}
  The device performing the drop.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-device)
      "Accessor"
      (documentation 'drop-device 'function)
 "@version{#2023-08-07}
  @syntax{(gdk:drop-device object) => device}
  @argument[object]{a @class{gdk:drop} object}
  @argument[device]{a @class{gdk:device} object performing the drop}
  @begin{short}
    Accessor of the @slot[gdk:drop]{device} slot of the @class{gdk:drop}
    class.
  @end{short}
  The @fun{gdk:drop-device} function returns the device performing the drop.
  @see-class{gdk:drop}
  @see-class{gdk:device}")

;;; --- gdk:drop-display -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'drop) t)
 "The @code{display} property of type @class{gdk:display} (Read) @br{}
  The display that the drop object belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-display)
      "Accessor"
      (documentation 'drop-display 'function)
 "@version{#2023-08-07}
  @syntax{(gdk:drop-display object) => display}
  @argument[object]{a @class{gdk:drop} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:drop]{display} slot of the @class{gdk:drop}
    class.
  @end{short}
  The @fun{gdk:drop-display} function gets the display that @arg{drop} was
  created for.
  @see-class{gdk:drop}
  @see-class{gdk:display}")

;;; --- gdk:drop-drag ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "drag" 'drop) t)
 "The @code{drag} property of type @class{gdk:drag}
  (Read / Write / Construct Only) @br{}
  The drag object that initiated the drop.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-drag)
      "Accessor"
      (documentation 'drop-drag 'function)
 "@version{#2023-08-07}
  @syntax{(gdk:drop-drag object) => drag}
  @argument[object]{a @class{gdk:drop} object}
  @argument[drag]{a corresponding @class{gdk:drag} object}
  @begin{short}
    Accessor of the @slot[gdk:drop]{drag} slot of the @class{gdk:drop}
    class.
  @end{short}
  If this is an in-app drag-and-drop operation, returns the drag object that
  corresponds to this drop. If it is not, @code{nil} is returned.
  @see-class{gdk:drop}
  @see-class{gdk:drag}")

;;; --- gdk:drop-formats -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'drop) t)
 "The @code{formats} property of type @class{gdk:content-formats}
  (Read / Write / Construct Only) @br{}
  The possible formats that the drop object can provide its data in.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-formats)
      "Accessor"
      (documentation 'drop-formats 'function)
 "@version{#2023-08-07}
  @syntax{(gdk:drop-formats object) => formats}
  @argument[object]{a @class{gdk:drop} object}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @begin{short}
    Accessor of the @slot[gdk:drop]{formats} slot of the @class{gdk:drop}
    class.
  @end{short}
  The @fun{gdk:drop-formats} function returns the content formats that the drop
  offers the data to be read in.
  @see-class{gdk:drop}
  @see-class{gdk:content-formats}")

;;; --- gdk:drop-surface -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "surface" 'drop) t)
 "The @code{surface} property of type @class{gdk:surface}
  (Read / Write / Construct Only) @br{}
  The  surface the drop happens on.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-surface)
      "Accessor"
      (documentation 'drop-surface 'function)
 "@version{#2023-08-07}
  @syntax{(gdk:drop-surface object) => surface}
  @argument[object]{a @class{gdk:drop} object}
  @argument[surface]{a @class{gdk:surface} object performing the drop}
  @begin{short}
    Accessor of the @slot[gdk:drop]{surface} slot of the @class{gdk:drop}
    class.
  @end{short}
  The @fun{gdk:drop-surface} function returns the surface performing the drop.
  @see-class{gdk:drop}
  @see-class{gdk:surface}")

;;; ----------------------------------------------------------------------------
;;; gdk_drop_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_status" drop-status) :void
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[drop]{a @class{gdk:drop} object}
  @argument[actions]{a @sym{gdk:drag-action} value for the supported actions
    of the destination, or @val[gdk:drag-action]{:none} to indicate that a drop
    will not be accepted}
  @argument[preferred]{a @sym{gdk:drag-action} value for the unique action that
    is a member of @arg{actions} indicating the preferred action}
  @begin{short}
    Selects all actions that are potentially supported by the destination.
  @end{short}
  When calling this function, do not restrict the passed in @arg{actions} to
  the ones provided by the @fun{gdk:drop-actions} function. Those actions may
  change in the future, even depending on the actions you provide here.

  The preferred action is a hint to the drag'n'drop mechanism about which
  action to use when multiple actions are possible.

  This function should be called by drag destinations in response to
  @val[gdk:event-type]{:drag-enter} or @val[gdk:event-type]{:drag-motion} events.
  If the destination does not yet know the exact actions it supports, it should
  set any possible actions first and then later call this function again.
  @see-class{gdk:drop}
  @see-symbol{gdk:drag-action}
  @see-function{gdk:drop-actions}"
  (drop (g:object drop))
  (actions drag-action)
  (preferred drag-action))

(export 'drop-status)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_finish" drop-finish) :void
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[drop]{a @class{gdk:drop} object}
  @argument[action]{a @sym{gdk:drag-action} value for the action performed by
    the destination or @val[gdk:drag-action]{:none} if the drop failed}
  @begin{short}
    Ends the drag operation after a drop.
  @end{short}
  The action must be a single action selected from the actions available via
  the @fun{gdk:drop-actions} function.
  @see-class{gdk:drop}
  @see-function{gdk:drop-actions}"
  (drop (g:object drop))
  (action drag-action))

(export 'drop-finish)

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

;; We need GInputStream for the implementation.

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

;; We need GInputStream for the implementation.

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_value_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_read_async" %drop-read-value-async) :void
  (drop (g:object drop))
  (gtype g:type-t)
  (priority :int)
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun drop-read-value-async (drop gtype priority cancellable func)
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[drop]{a @class{gdk:drop} object}
  @argument[gtype]{a @class{g:type-t} type ID to read}
  @argument[priority]{an integer for the I/O priority of the request}
  @argument[cancellable]{an optional @class{g:cancellable} object, @code{nil}
    to ignore}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the request is satisfied}
  @begin{short}
    Asynchronously request the drag operation's contents converted to the given
    @arg{gtype}.
  @end{short}
  When the operation is finished @arg{func} will be called. You can then call
  the @fun{gdk:drop-read-value-finish} function to get the resulting
  @sym{g:value} value.

  For local drag'n'drop operations that are available in the given @arg{gtype},
  the value will be copied directly. Otherwise, GDK will try to use the
  @fun{gdk:content-deserialize-async} to convert the data.
  @see-class{gdk:drop}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%drop-read-value-async drop
                            gtype
                            priority
                            cancellable
                            (cffi:callback g:async-ready-callback)
                            ptr)))

(export 'drop-read-value-async)

;;; ----------------------------------------------------------------------------
;;; gdk_drop_read_value_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_drop_read_value_finish" %drop-read-value-finish)
    (:pointer (:struct g:value))
  (drop (g:object drop))
  (result (g:object g:async-result))
  (err :pointer))

(defun drop-read-value-finish (drop result)
 #+liber-documentation
 "@version{#2025-07-31}
  @argument[drop]{a @class{gdk:drop} object}
  @argument[result]{a @class{g:async-result} object}
  @return{The @sym{g:value} value containing the result.}
  @begin{short}
    Finishes an async drop read started with the @fun{gdk:drop-read-value-async}
    function.
  @end{short}
  @see-class{gdk:drop}
  @see-class{g:async-result}
  @see-symbol{g:value}
  @see-function{gdk:drop-read-value-async}"
  (glib:with-ignore-error (err)
    (%drop-read-value-finish drop result err)))

(export 'drop-read-value-finish)

;;; --- End of file gdk4.drop.lisp ---------------------------------------------
