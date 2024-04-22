;;; ----------------------------------------------------------------------------
;;; gtk4.revealer.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkRevealer
;;;
;;;     Hide and show with animation
;;;
;;; Types and Values
;;;
;;;     GtkRevealer
;;;     GtkRevealerTransitionType
;;;
;;; Accessors
;;;
;;;     gtk_revealer_get_child
;;;     gtk_revealer_set_child
;;;     gtk_revealer_get_reveal_child
;;;     gtk_revealer_set_reveal_child
;;;     gtk_revealer_get_child_revealed
;;;     gtk_revealer_get_transition_duration
;;;     gtk_revealer_set_transition_duration
;;;     gtk_revealer_get_transition_type
;;;     gtk_revealer_set_transition_type
;;;
;;; Functions
;;;
;;;     gtk_revealer_new
;;;
;;; Properties
;;;
;;;     child
;;;     child-revealed
;;;     reveal-child
;;;     transition-duration
;;;     transition-type
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRevealer
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRevealerTransitionType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkRevealerTransitionType" revealer-transition-type
  (:export t
   :type-initializer "gtk_revealer_transition_type_get_type")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5)
  (:swing-right 6)
  (:swing-left 7)
  (:swing-up 8)
  (:swing-down 9))

#+liber-documentation
(setf (liber:alias-for-symbol 'revealer-transition-type)
      "GEnum"
      (liber:symbol-documentation 'revealer-transition-type)
 "@version{2024-4-12}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkRevealerTransitionType\" revealer-transition-type
  (:export t
   :type-initializer \"gtk_revealer_transition_type_get_type\")
  (:none 0)
  (:crossfade 1)
  (:slide-right 2)
  (:slide-left 3)
  (:slide-up 4)
  (:slide-down 5)
  (:swing-right 6)
  (:swing-left 7)
  (:swing-up 8)
  (:swing-down 9))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No transition}
      @entry[:crossfade]{Fade in.}
      @entry[:slide-right]{Slide in from the left.}
      @entry[:slide-left]{Slide in from the right.}
      @entry[:slide-up]{Slide in from the bottom.}
      @entry[:slide-down]{Slide in from the top.}
      @entry[:swing-right]{Floop in from the left.}
      @entry[:swing-left]{Floop in from the right.}
      @entry[:swing-up]{Floop in from the bottom.}
      @entry[:swing-down]{Floop in from the top.}
    @end{table}
  @end{values}
  @begin{short}
    These enumeration values describe the possible transitions when the child
    widget of a @class{gtk:revealer} widget is shown or hidden.
  @end{short}
  @see-class{gtk:revealer}")

;;; ----------------------------------------------------------------------------
;;; GtkRevealer
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkRevealer" revealer
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_revealer_get_type")
  ((child
    revealer-child
    "child" "GtkWidget" t t)
   (child-revealed
    revealer-child-revealed
    "child-revealed" "gboolean" t nil)
   (reveal-child
    revealer-reveal-child
    "reveal-child" "gboolean" t t)
   (transition-duration
    revealer-transition-duration
    "transition-duration" "guint" t t)
   (transition-type
    revealer-transition-type
    "transition-type" "GtkRevealerTransitionType" t t)))

#+liber-documentation
(setf (documentation 'revealer 'type)
 "@version{2024-4-15}
  @begin{short}
    The @class{gtk:revealer} widget is a container which animates the transition
    of its child widget from invisible to visible.
  @end{short}
  The style of transition can be controlled with a value of the
  @fun{gtk:revealer-transition-type} enumeration. These animations respect
  the @slot[gtk:settings]{gtk-enable-animations} setting.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:revealer} implementation has a single CSS node with name
    @code{revealer}. When styling the @class{gtk:revealer} widget using CSS,
    remember that it only hides its contents, not itself. That means applied
    margin, padding and borders will be visible even when the
    @slot[gtk:revealer]{reveal-child} property is set to @em{false}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:revealer} implementation uses the @code{:group} role of the
    @symbol{gtk:accessible-role} enumeration. The child widget of the
    @class{gtk:revealer} widget, if set, is always available in the
    accessibility tree, regardless of the state of the revealer widget.
  @end{dictionary}
  @see-constructor{gtk:revealer-new}
  @see-slot{gtk:revealer-child}
  @see-slot{gtk:revealer-child-revealed}
  @see-slot{gtk:revealer-reveal-child}
  @see-slot{gtk:revealer-transition-duration}
  @see-slot{gtk:revealer-transition-type}
  @see-class{gtk:expander}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:revealer-child -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'revealer) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-child)
      "Accessor"
      (documentation 'revealer-child 'function)
 "@version{2023-8-8}
  @syntax{(gtk:revealer-child object) => child}
  @syntax{(setf (gtk:revealer-child object) child)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{child} slot of the @class{gtk:revealer}
    class.
  @end{short}
  The @fun{gtk:revealer-child} function gets the child widget of the revealer.
  The @setf{gtk:revealer-child} function sets the child widget.
  @see-class{gtk:revealer}
  @see-class{gtk:widget}")

;;; --- gtk:revealer-child-revealed --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child-revealed" 'revealer) t)
 "The @code{child-revealed} property of type @code{:boolean} (Read) @br{}
  Whether the child widget is revealed and the animation target reached. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-child-revealed)
      "Accessor"
      (documentation 'revealer-child-revealed 'function)
 "@version{2023-8-8}
  @syntax{(gtk:revealer-child-revealed object) => revealed}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[revealed]{a boolean whether the child widget is revealed}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{child-revealed} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @fun{gtk:revealer-child-revealed} function returns whether the child
  widget is fully revealed, in other words whether the transition to the
  revealed state is completed.
  @see-class{gtk:revealer}")

;;; --- gtk:revealer-reveal-child ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reveal-child" 'revealer) t)
 "The @code{reveal-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether the container should reveal the child widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-reveal-child)
      "Accessor"
      (documentation 'revealer-reveal-child 'function)
 "@version{2023-8-8}
  @syntax{(gtk:revealer-reveal-child object) => reveal}
  @syntax{(setf (gtk:revealer-reveal-child object) reveal)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[reveal]{@em{true} to reveal the child widget}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{reveal-child} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @fun{gtk:revealer-reveal-child} function returns whether the child widget
  is currently revealed. The @setf{gtk:revealer-reveal-child} function tells
  the revealer to reveal or conceal its child widget.

  This function returns @em{true} as soon as the transition to the revealed
  state is started. To learn whether the child widget is fully revealed, i.e.
  the transition is completed, use the @fun{gtk:revealer-child-revealed}
  function. The transition will be animated with the current transition type of
  the revealer.
  @see-class{gtk:revealer}
  @see-function{gtk:revealer-child-revealed}")

;;; --- gtk:revealer-transition-duration ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transition-duration"
                                               'revealer) t)
 "The @code{transition-duration} property of type @code{:uint} (Read / Write)
  @br{}
  The animation duration, in milliseconds. @br{}
  Default value: 250")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-transition-duration)
      "Accessor"
      (documentation 'revealer-transition-duration 'function)
 "@version{2023-8-8}
  @syntax{(gtk:revealer-transition-duration object) => duration}
  @syntax{(setf (gtk:revealer-transition-duration object) duration)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[duration]{an unsigned integer with the duration, in milliseconds}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{transition-duration} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @fun{gtk:revealer-transition-duration} function returns the amount of
  time in milliseconds that transitions will take. The
  @setf{gtk:revealer-transition-duration} function sets the duration.
  @see-class{gtk:revealer}")

;;; --- gtk:revealer-transition-type -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transition-type" 'revealer) t)
 "The @code{transition-type} property of type
  @symbol{gtk:revealer-transition-type} (Read / Write) @br{}
  The type of animation used to transition. @br{}
  Default value: @code{:slide-down}")

#+liber-documentation
(setf (liber:alias-for-function 'revealer-transition-type)
      "Accessor"
      (documentation 'revealer-transition-type 'function)
 "@version{2023-8-8}
  @syntax{(gtk:revealer-transition-type object) => setting}
  @syntax{(setf (gtk:revealer-transition-type object) setting)}
  @argument[object]{a @class{gtk:revealer} widget}
  @argument[setting]{a value of the @symbol{gtk:revealer-transition-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:revealer]{transition-type} slot of the
    @class{gtk:revealer} class.
  @end{short}
  The @fun{gtk:revealer-transition-type} function gets the type of animation
  that will be used for transitions in the revealer. The
  @setf{gtk:revealer-transition-duration} function sets the type of animation.
  Available types include various kinds of fades and slides.
  @see-class{gtk:revealer}
  @see-symbol{gtk:revealer-transition-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_revealer_new
;;; ----------------------------------------------------------------------------

(declaim (inline revealer-new))

(defun revealer-new ()
 #+liber-documentation
 "@version{2023-8-8}
  @return{The new @class{gtk:revealer} widget.}
  @short{Creates a new revealer.}
  @see-class{gtk:revealer}"
  (make-instance 'revealer))

(export 'revealer-new)

;;; --- End of file gtk4.revealer.lisp -----------------------------------------
