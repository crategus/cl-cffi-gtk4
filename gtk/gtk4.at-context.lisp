;;; ----------------------------------------------------------------------------
;;; gtk4.at-context.lisp
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
;;; GtkATContext
;;;
;;;     An object communicating to Assistive Technologies
;;;
;;; Types and Values
;;;
;;;     GtkATContext
;;;
;;; Accessors
;;;
;;;     gtk_at_context_get_accessible
;;;     gtk_at_context_get_accessible_role
;;;
;;; Functions
;;;
;;;     gtk_at_context_create
;;;
;;; Properties
;;;
;;;     accessible
;;;     accessible-role
;;;     display
;;;
;;; Signals
;;;
;;;     state-change
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkATContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkATContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkATContext" at-context
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_at_context_get_type")
  ((accessible
    at-context-accessible
    "accessible" "GtkAccessible" t nil)
   (accessible-role
    at-context-accessible-role
    "accessible-role" "GtkAccessibleRole" t t)
   (display
    at-context-display
    "display" "GdkDidplay" t t)))

#+liber-documentation
(setf (documentation 'at-context 'type)
 "@version{#2023-4-21}
  @begin{short}
    The @sym{gtk-at-context} class is an abstract class provided by GTK to
    communicate to platform-specific assistive technologies API.
  @end{short}
  Each platform supported by GTK implements a @sym{gtk:at-context} subclass,
  and is responsible for updating the accessible state in response to state
  changes in the @class{gtk:accessible} object.
  @begin[Signal Details]{dictionary}
    @subheading{The \"state-change\" signal}
      @begin{pre}
lambda (context)    :run-first
      @end{pre}
      Emitted when the attributes of the accessible for the @sym{gtk:at-context}
      instance change.
      @begin[code]{table}
        @entry[context]{A @sym{gtk:at-context} object.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:at-context-create}
  @see-class{gtk:accessible}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- at-context-accessible --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible" 'at-context) t)
 "The @code{accessible} property of type @class{gtk:accessible}
  (Read / Write / Construct only) @br{}
  The accessible that created the AT context.")

#+liber-documentation
(setf (liber:alias-for-function 'at-context-accessible)
      "Accessor"
      (documentation 'at-context-accessible 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:at-context-accessible object) => accessible}
  @argument[object]{a @class{gtk:at-context} object}
  @argument[accessible]{a @class{gtk:accessible} object}
  @begin{short}
    Accessor of the @slot[gtk:at-context]{accessible} slot of the
    @class{gtk:at-context} class.
  @end{short}
  Retrieves the @class{gtk:accessible} object using this context.
  @see-class{gtk:at-context}
  @see-class{gtk:accessible}")

;;; --- at-context-accessible-role ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-role" 'at-context) t)
 "The @code{accessible-role} property of type @symbol{gtk:accessible-role}
  (Read / Write / Construct) @br{}
  The accessible role used by the AT context. Depending on the given role,
  different states and properties can be set or retrieved. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'at-context-accessible-role)
      "Accessor"
      (documentation 'at-context-accessible-role 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:at-context-accessible-role object) => role}
  @argument[object]{a @class{gtk:at-context} object}
  @argument[role]{a @symbol{gtk:accessible-role} value}
  @begin{short}
    Accessor of the @slot[gtk:at-context]{accessible-role} slot of the
    @class{gtk:at-context} class.
  @end{short}
  Retrieves the accessible role of the context.
  @see-class{gtk:at-context}
  @see-symbol{gtk:accessible-role}")

;;; --- at-context-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'at-context) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The display for the AT context.")

#+liber-documentation
(setf (liber:alias-for-function 'at-context-display)
      "Accessor"
      (documentation 'at-context-display 'function)
 "@version{#2023-4-21}
  @syntax[]{(gtk:at-context-display object) => display}
  @argument[object]{a @class{gtk:at-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gtk:at-context]{display} slot of the
    @class{gtk:at-context} class.
  @end{short}
  @see-class{gtk:at-context}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; gtk_at_context_create ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_at_context_create" at-context-create) (g:object at-context)
 #+liber-documentation
 "@version{#2023-4-21}
  @argument[role]{a @symbol{gtk:accessible-role} value used by the context}
  @argument[accessible]{a @class{gtk:accessible} object using the context}
  @argument[display]{a @class{gdk:display} object used by the context}
  @begin{short}
    Creates a new @class{gtk:at-context} instance for the given accessible role,
    accessible instance, and display connection.
  @end{short}
  The @class{gtk:at-context} implementation being instantiated will depend on
  the platform.
  @see-class{gtk:at-context}
  @see-class{gtk:accessible}
  @see-class{gdk:display}
  @see-symbol{gtk:accessible-role}"
  (role accessible-role)
  (accessible (g:object accessible))
  (display (g:object gdk:display)))

(export 'at-context-create)

;;; --- End of file gtk4.at-context.lisp ---------------------------------------
