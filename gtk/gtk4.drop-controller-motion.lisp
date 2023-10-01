;;; ----------------------------------------------------------------------------
;;; gtk4.drop-controller-motion.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkDropControllerMotion
;;;
;;;     Event controller for motion events during a drop
;;;
;;; Types and Values
;;;
;;;     GtkDropControllerMotion
;;;
;;; Functions
;;;
;;;     gtk_drop_controller_motion_new
;;;     gtk_drop_controller_motion_contains_pointer
;;;     gtk_drop_controller_motion_is_pointer
;;;     gtk_drop_controller_motion_get_drop
;;;
;;; Properties
;;;
;;;     contains-pointer
;;;     drop
;;;     is-pointer
;;;
;;; Signals
;;;
;;;     enter
;;;     leave
;;;     move
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkDropControllerMotion
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropControllerMotion
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkDropControllerMotion" drop-controller-motion
  (:superclass event-controller
   :export t
   :interfaces ()
   :type-initializer "gtk_drop_controller_motion_get_type")
  ((contains-pointer
    drop-controller-motion-contains-pointer
    "contains-pointer" "gboolean" t nil)
   (drop
    drop-controller-motion-drop
    "drop" "GdkDrop" t nil)
   (is-pointer
    drop-controller-motion-is-pointer
    "is-pointer" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'drop-controller-motion 'type)
 "@version{#2023-9-29}
  @begin{short}
    The @class{gtk:drop-controller-motion} object is an event controller meant
    for tracking the pointer hovering over a widget during a drag and drop
    operation.
  @end{short}
  It is modeled after the @class{gtk:event-controller-motion} object so if you
  have used that, this should feel really familiar. The drop controller is not
  able to accept drops, use the @class{gtk:drop-target} object for that purpose.
  @begin[Signal Details]{dictionary}
    @subheading{The \"enter\" signal}
      @begin{pre}
lambda (controller x y)    :run-last
      @end{pre}
      Signals that the pointer has entered the widget.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:drop-controller-motion} object.}
        @entry[x]{A double float with the x coordinate of the pointer location.}
        @entry[y]{A double float with the y coordinate of the pointer location.}
      @end{table}
    @subheading{The \"leave\" signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      Signals that the pointer has left the widget.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:drop-controller-motion} object.}
      @end{table}
    @subheading{The \"motion\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Emitted when the pointer moves inside the widget.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:drop-controller-motion} object.}
        @entry[x]{A double float with the x coordinate of the pointer location.}
        @entry[y]{A double float with the y coordinate of the pointer location.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:drop-controller-motion-new}
  @see-slot{gtk:drop-controller-motion-contains-pointer}
  @see-slot{gtk:drop-controller-motion-drop}
  @see-slot{gtk:drop-controller-motion-is-pointer}
  @see-class{gdk:drop}
  @see-class{gtk:drop-target}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- drop-controller-motion-contains-pointer --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "contains-pointer"
                                               'drop-controller-motion) t)
 "The @code{contains-pointer} property of type @code{:boolean} (Read) @br{}
  Whether the pointer of a Drag-and-Drop operation is in the controller’s widget
  or a descendant. See also the @slot[gtk:drop-controller-motion]{is-pointer}
  property. When handling crossing events, this property is updated before the
  \"enter\" signal, but after the \"leave\" signal is emitted.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-controller-motion-contains-pointer)
      "Accessor"
      (documentation 'drop-controller-motion-contains-pointer 'function)
 "@version{#2023-9-29}
  @syntax[]{(gtk:drop-controller-motion-contains-pointer object) => contains}
  @argument[object]{a @class{gtk:drop-controller-motion} object}
  @argument[contains]{@em{true} if a dragging pointer is within @arg{object}
    or one of its children}
  @begin{short}
    Accessor of the @slot[gtk:drop-controller-motion]{contains-pointer} slot of
    the @class{gtk:drop-controller-motion} class.
  @end{short}
  The @fun{gtk:drop-controller-motion-contains-pointer} function returns the
  value of the property.
  @see-class{gtk:drop-controller-motion}")

;;; --- drop-controller-motion-drop --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "drop"
                                               'drop-controller-motion) t)
 "The @code{drop} property of type @class{gdk:drop} (Read) @br{}
  The ongoing drop operation over the widget of the controller or its
  descendant. If no drop operation is going on, this property returns
  @code{nil}. The event controller should not modify the drop, but it might want
  to query its properties. When handling crossing events, this property is
  updated before the \"enter\" signal, but after the \"leave\" signal is
  emitted.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-controller-motion-drop)
      "Accessor"
      (documentation 'drop-controller-motion-drop 'function)
 "@version{#2023-9-29}
  @syntax[]{(gtk:drop-controller-motion-drop object) => drop}
  @argument[object]{a @class{gtk:drop-controller-motion} object}
  @argument[drop]{a @class{gdk:drop} object currently happening within
    @arg{object} or @code{nil} if none}
  @begin{short}
    Accessor of the @slot[gtk:drop-controller-motion]{drop} slot of the
    @class{gtk:drop-controller-motion} class.
  @end{short}
  The @fun{gtk:drop-controller-motion-drop} function returns the value of the
  property.
  @see-class{gtk:drop-controller-motion}
  @see-class{gdk:drop}")

;;; --- drop-controller-motion-is-pointer --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-pointer"
                                               'drop-controller-motion) t)
 "The @code{is-pointer} property of type @code{:boolean} (Read) @br{}
  Whether the pointer is in the controllers widget itself, as opposed to in a
  descendent widget. See also the
  @slot[gtk:drop-controller-motion]{contains-pointer} property. When handling
  crossing events, this property is updated before the\"enter\" signal, but
  after the \"leave\" signal is emitted.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-controller-motion-is-pointer)
      "Accessor"
      (documentation 'drop-controller-motion-is-pointer 'function)
 "@version{#2023-9-29}
  @syntax[]{(gtk:drop-controller-motion-is-pointer object) => is-pointer}
  @argument[object]{a @class{gtk:drop-controller-motion} object}
  @argument[is-pointer]{@em{true} if a dragging pointer is within @arg{object}
    but not one of its children}
  @begin{short}
    Accessor of the @slot[gtk:drop-controller-motion]{is-pointer} slot of the
    @class{gtk:drop-controller-motion} class.
  @end{short}
  The @fun{gtk:drop-controller-motion-is-pointer} function returns @em{true} if
  a Drag-and-Drop operation is within the widget @arg{object}, not one of its
  children.
  @see-class{gtk:drop-controller-motion}")

;;; ----------------------------------------------------------------------------
;;; gtk_drop_controller_motion_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline drop-controller-motion-new))

(defun drop-controller-motion-new ()
 #+liber-documentation
 "@version{#2023-9-29}
  @return{The new @class{gtk:drop-controller-motion} object.}
  @begin{short}
    Creates a new event controller that will handle pointer motion events during
    drag and drop.
  @end{short}
  @see-class{gtk:drop-controller-motion}"
  (make-instance 'drop-controller-motion))

(export 'drop-controller-motion-new)

;;; --- End of file gtk4.drop-controller-motion.lisp ---------------------------
