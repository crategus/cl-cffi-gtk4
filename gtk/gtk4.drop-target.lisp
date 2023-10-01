;;; ----------------------------------------------------------------------------
;;; gtk4.drop-target.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
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
;;;     current-drop                                       Since 4.4
;;;     drop                                               Deprecated 4.4
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
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkDropTarget" drop-target
  (:superclass event-controller
   :export t
   :interfaces ()
   :type-initializer "gtk_drop_target_get_type")
  ((actions
    drop-target-actions
    "actions" "GdkDragAction" t t)
   #+gtk-4-4
   (current-drop
    drop-target-current-drop
    "current-drop" "GdkDrop" t nil)
   (drop
    drop-target-drop
    "drop" "GdkDrop" t nil)
   (formats
    drop-target-formats
    "formats" "GdkContentFormats" t t)
   (preload
    drop-target-preload
    "preload" "gboolean" t t)
   (value
    drop-target-value
    "value" "GValue" t nil)))

#+liber-documentation
(setf (documentation 'drop-target 'type)
 "@version{2023-9-18}
  @begin{short}
    The @class{gtk:drop-target} object is an event controller implementing a
    simple way to receive Drag-and-Drop operations.
  @end{short}
  The most basic way to use a @class{gtk:drop-target} object to receive drops on
  a widget is to create it via the @fun{gtk:drop-target-new} function passing
  in the @code{GType} of the data you want to receive and connect to the
  \"current-drop\" signal to receive the data.

  The @class{gtk:drop-target} object supports more options, such as:
  @begin{itemize}
    @item{rejecting potential drops via the \"accept\" signal and the
      @fun{gtk:drop-target-reject} function to let other drop targets handle
      the drop}
    @item{tracking an ongoing drag operation before the drop via the \"enter\",
      \"motion\" and \"leave\" signals}
    @item{configuring how to receive data by setting the “preload” property and
      listening for its availability via the @slot[gdk:drop-target]{value}
      property}
  @end{itemize}
  However, the @class{gtk:drop-target} object is ultimately modeled in a
  synchronous way and only supports data transferred via @code{GType}. If you
  want full control over an ongoing drop, the @class{gtk:drop-target-async}
  object gives you this ability.

  While a pointer is dragged over the drop target's widget and the drop has
  not been rejected, that widget will receive the @code{:drop-active} state,
  which can be used to style the widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"accept\" signal}
      @begin{pre}
lambda (target drop)    :run-last
      @end{pre}
      The signal is emitted on the drop site when a drop operation is about to
      begin. If the drop is not accepted, @em{false} will be returned and the
      drop target will ignore the drop. If @em{true} is returned, the drop is
      accepted for now but may be rejected later via a call to the
      @fun{gtk:drop-target-reject} function or ultimately by returning
      @em{false} from the \"drop\" signal.

      The default handler for this signal decides whether to accept the drop
      based on the formats provided by @arg{drop}.

      If the decision whether the drop will be accepted or rejected needs
      inspecting the data, this function should return @em{true}, the
      @slot[gtk:drop-target]{preload} property should be set and the value
      should be inspected via the \"notify::value\" signal and then call the
      @fun{gtk:drop-target-reject} function.
      @begin[code]{table}
        @entry[target]{The @class{gtk:drag-target} object.}
        @entry[drop]{The @class{gdk:drop} object.}
        @entry[Returns]{@em{True} if @arg{drop} is accepted.}
      @end{table}
    @subheading{The \"drop\" signal}
      @begin{pre}
lambda (target value x y)    :run-last
      @end{pre}
      The signal is emitted on the drop site when the user drops the data onto
      the widget. The signal handler must determine whether the pointer position
      is in a drop zone or not. If it is not in a drop zone, it returns
      @em{false} and no further processing is necessary. Otherwise, the handler
      returns @em{true}. In this case, this handler will accept the drop. The
      handler is responsible for reading the given @arg{value} and performing
      the drop operation.
      @begin[code]{table}
        @entry[target]{The @class{gtk:drag-target} object.}
        @entry[value]{The @symbol{g:value} instance being dropped.}
        @entry[x]{A double float with the x coordinate of the current pointer
          position.}
        @entry[y]{A double float with the x coordinate of the current pointer
          position.}
        @entry[Returns]{Whether the drop was accepted at the given pointer
          position.}
      @end{table}
    @subheading{The \"enter\" signal}
      @begin{pre}
lambda (target x y)    :run-last
      @end{pre}
      The signal is emitted on the drop site when the pointer enters the widget.
      It can be used to set up custom highlighting.
      @begin[code]{table}
        @entry[target]{The @class{gtk:drag-target} object.}
        @entry[x]{A double float with the x coordinate of the current pointer
          position.}
        @entry[y]{A double float with the x coordinate of the current pointer
          position.}
        @entry[Returns]{A @symbol{gdk:drag-action} value with the preferred
          action for this drag operation or 0 if dropping is not supported at
          the current @code{x,y} location.}
      @end{table}
    @subheading{The \"leave\" signal}
      @begin{pre}
lambda (target)    :run-last
      @end{pre}
      The signal is emitted on the drop site when the pointer leaves the widget.
      Its main purpose is to undo things done in \"enter\" signal handler.
      @begin[code]{table}
        @entry[target]{The @class{gtk:drag-target} object.}
      @end{table}
      @subheading{The \"motion\" signal}
      @begin{pre}
lambda (target x y)    :run-last
      @end{pre}
      The signal is emitted while the pointer is moving over the drop target.
      @begin[code]{table}
        @entry[target]{The @class{gtk:drag-target} object.}
        @entry[x]{A double float with the x coordinate of the current pointer
          position.}
        @entry[y]{A double float with the x coordinate of the current pointer
          position.}
        @entry[Returns]{A @symbol{gdk:drag-action} value with the preferred
          action for this drag operation or 0 if dropping is not supported at
          the current @code{x,y} location.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:drop-target-new}
  @see-slot{gtk:drop-target-actions}
  @see-slot{gtk:drop-target-current-drop}
  @see-slot{gtk:drop-target-drop}
  @see-slot{gtk:drop-target-formats}
  @see-slot{gtk:drop-target-preload}
  @see-slot{gtk:drop-target-value}
  @see-class{gdk:drop}
  @see-class{gtk:drop-target-async}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- drop-target-actions ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "actions" 'drop-target) t)
 "The @code{actions} property of type @symbol{gdk:drag-action} (Read / Write)
  @br{}
  The actions that this drop target supports. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-actions)
      "Accessor"
      (documentation 'drop-target-actions 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-actions object) => actions}
  @syntax[]{(setf (gtk:drop-target-actions object) actions)}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[actions]{a @symbol{gdk:drag-action} value}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{actions} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-actions} function gets the actions that this drop
  target supports. The @setf{gtk:drop-target-actions} function sets the actions.
  @see-class{gtk:drop-target}
  @see-symbol{gdk:drag-action}")

;;; --- drop-target-current-drop -----------------------------------------------

#+(and gtk-4-4 liber-documentation)
(setf (documentation (liber:slot-documentation "current-drop" 'drop-target) t)
 "The @code{current-drop} property of type @class{gdk:drop} (Read) @br{}
  The drop that is currently being performed. Since 4.4")

#+(and gtk-4-4 liber-documentation)
(setf (liber:alias-for-function 'drop-target-current-drop)
      "Accessor"
      (documentation 'drop-target-current-drop 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-current-drop object) => drop}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[drop]{a @class{gdk:drop} object with the current drop}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{current-drop} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-current-drop} function gets the currently handled
  drop operation. If no drop operation is going on, @code{nil} is returned.

  Since 4.4
  @see-class{gtk:drop-target}
  @see-symbol{gdk:drop}")

;;; --- drop-target-drop -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "drop" 'drop-target) t)
 "The @code{drop} property of type @class{gdk:drop} (Read) @br{}
  The drop that is currently being performed. Deprecated 4.4")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-drop)
      "Accessor"
      (documentation 'drop-target-drop 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-drop object) => drop}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[drop]{a @class{gdk:drop} object with the current drop}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{drop} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-drop} function gets the currently handled drop
  operation.

  Deprecated 4.4
  @see-class{gtk:drop-target}
  @see-symbol{gdk:drop}")

;;; --- drop-target-formats ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'drop-target) t)
 "The @code{formats} property of type @class{gdk:content-formats} (Read) @br{}
  The content formats that determine the supported data formats.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-formats)
      "Accessor"
      (documentation 'drop-target-formats 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-formats object) => formats}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[formats]{a @class{gdk:content-formats} instance with the
    supported data formats}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{formats} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-formats} function gets the data formats that this
  drop target accepts. If the result is @code{nil}, all formats are expected to
  be supported.
  @see-class{gtk:drop-target}
  @see-class{gdk:content-formats}")

;;; --- drop-target-preload ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "preload" 'drop-target) t)
 "The @code{preload} property of type @code{:boolean} (Read / Write) @br{}
  Whether the drop data should be preloaded when the pointer is only hovering
  over the widget but has not been released. Setting this property allows finer
  grained reaction to an ongoing drop at the cost of loading more data. The
  default value for this property is @em{false} to avoid downloading huge
  amounts of data by accident. For example, if somebody drags a full document
  of gigabytes of text from a text editor across a widget with a preloading
  drop target, this data will be downloaded, even if the data is ultimately
  dropped elsewhere. For a lot of data formats, the amount of data is very
  small, like the @class{gdk:rgba} color, so enabling this property does not
  hurt at all. And for local-only drag'n'drop operations, no data transfer is
  done, so enabling it there is free. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-preload)
      "Accessor"
      (documentation 'drop-target-preload 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-preload object) => preload}
  @syntax[]{(setf (gtk:drop-target-preload object) preload)}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[preload]{a boolean whether the drop data should be preloaded}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{preload} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-preload} function gets the value of the
  @slot[gtk:drop-target]{preload} property. The
  @setf{gtk:drop-target-preload} function sets the property.
  @see-class{gtk:drop-target}")

;;; --- drop-target-value ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'drop-target) t)
 "The @code{value} property of type @symbol{g:value} (Read) @br{}
  The value for this drop operation or @code{nil} if the data has not been
  loaded yet or no drop operation is going on. Data may be available before the
  \"current-drop\" signal gets emitted - for example when the
  @slot[gtk:drop-target]{preload} property is set. You can use the \"notify\"
  signal to be notified of available data.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-target-value)
      "Accessor"
      (documentation 'drop-target-value 'function)
 "@version{#2023-9-18}
  @syntax[]{(gtk:drop-target-value object) => value}
  @argument[object]{a @class{gtk:drop-target} object}
  @argument[value]{a @symbol{g:value} instance with the current drop data}
  @begin{short}
    Accessor of the @slot[gtk:drop-target]{value} slot of the
    @class{gtk:drop-target} class.
  @end{short}
  The @fun{gtk:drop-target-value} function gets the value of the
  @slot[gtk:drop-target]{value} property.
  @see-class{gtk:drop-target}
  @see-symbol{g:value}")

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_target_new" drop-target-new) (g:object drop-target)
 #+liber-documentation
 "@version{#2023-9-16}
  @argument[gtype]{a @class{g:type-t} type with the supported type}
  @argument[actions]{a @symbol{gdk:drag-action} value with the supported
    actions}
  @return{A new @class{gtk:drop-target} object.}
  @begin{short}
    Creates a new drop target.
  @end{short}
  If the drop target should support more than one type, pass @code{nil} for
  @arg{gtype} and then call the @fun{gtk:drop-target-gtypes} function.
  @see-class{gtk:drop-target}
  @see-class{g:type-t}
  @see-symbol{gdk:drag-action}
  @see-function{gtk:drop-target-gtypes}"
  (gtype g:type-t)
  (actions gdk:drag-action))

(export 'drop-target-new)

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_set_gtypes ()
;;; gtk_drop_target_get_gtypes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_target_set_gtypes" %drop-target-set-gtypes) :void
  (target (g:object drop-target))
  (gtypes (:pointer :size))
  (n-gtypes :size))

(defun (setf drop-target-gtypes) (gtypes target)
  (let ((n-gtypes (length gtypes))
        (gtypes (mapcar #'g:gtype gtypes)))
    (cffi:with-foreign-object (gtypes-ptr 'g:type-t n-gtypes)
      (iter (for gtype in gtypes)
            (for count from 0)
            (setf (cffi:mem-aref gtypes-ptr 'g:type-t count) gtype))
      (%drop-target-set-gtypes target gtypes-ptr n-gtypes))
    gtypes))

(cffi:defcfun ("gtk_drop_target_get_gtypes" %drop-target-get-gtypes)
    (:pointer :size)
  (target (g:object drop-target))
  (n-gtypes (:pointer :size)))

(defun drop-target-gtypes (target)
 #+liber-documentation
 "@version{2023-9-18}
  @syntax[]{(gtk:drop-target-gtypes target) => gtypes}
  @syntax[]{(setf (gtk:drop-target-gtypes target) gtypes)}
  @argument[target]{a @class{gtk:drop-target} object}
  @argument[gtypes]{a list of @class{g:type-t} types}
  @begin{short}
    The @fun{gtk:drop-target-gtypes} function gets the list of supported GTypes
    for @arg{target}.
  @end{short}
  If no type have been set, @code{nil} will be returned. The
  @setf{gtk:drop-target} function sets the supported GTypes for the drop target.
  @see-class{gtk:drop-target}
  @see-class{g:type-t}"
  (cffi:with-foreign-object (n-gtypes :size)
    (let ((ptr (%drop-target-get-gtypes target n-gtypes)))
      (prog1
        (iter (for count from 0 below (cffi:mem-ref n-gtypes :size))
              (collect (cffi:mem-aref ptr 'g:type-t count)))
        (glib:free ptr)))))

(export 'drop-target-gtypes)

;;; ----------------------------------------------------------------------------
;;; gtk_drop_target_reject ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_target_reject" drop-target-reject) :void
 #+liber-documentation
 "@version{#2023-7-31}
  @argument[target]{a @class{gtk:drop-target} object}
  @begin{short}
    Rejects the ongoing drop operation.
  @end{short}
  If no drop operation is ongoing - when the \"drop-current\" signal returns
  @code{nil} - this function does nothing.

  This function should be used when delaying the decision on whether to accept
  a drag or not until after reading the data.
  @see-class{gtk:drop-target}"
  (target (g:object drop-target)))

(export 'drop-target-reject)

;;; --- End of file gtk4.drop-target.lisp --------------------------------------
