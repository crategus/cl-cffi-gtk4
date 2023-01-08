;;; ----------------------------------------------------------------------------
;;; gtk.overlay.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkOverlay
;;;
;;;     A container which overlays widgets on top of each other
;;;
;;; Types and Values
;;;
;;;     GtkOverlay
;;;
;;; Accessors
;;;
;;;     gtk_overlay_get_child
;;;     gtk_overlay_set_child
;;;
;;; Functions
;;;
;;;     gtk_overlay_new
;;;     gtk_overlay_add_overlay
;;;     gtk_overlay_remove_overlay
;;;     gtk_overlay_get_measure_overlay
;;;     gtk_overlay_set_measure_overlay
;;;     gtk_overlay_get_clip_overlay
;;;     gtk_overlay_set_clip_overlay
;;;
;;; Properties
;;;
;;;     child
;;;
;;; Signals
;;;
;;;     get-child-position
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkOverlay
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOverlay
;;; ----------------------------------------------------------------------------

;; TODO: The "get-child-position" signal handler has an return location
;; of type GdkRectangle. Check the Lisp implementation of a handler.

(define-g-object-class "GtkOverlay" overlay
  (:superclass widget
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget")
    :type-initializer "gtk_overlay_get_type")
  ((child
    overlay-child
    "child" "GtkWidget" t t)))

#+liber-documentation
(setf (documentation 'overlay 'type)
 "@version{#2022-7-31}
  @begin{short}
    The @sym{gtk:overlay} widget is a container which contains a single main
    widget, on top of which it can place overlay widgets.
  @end{short}

  @image[overlay]{Figure: GtkOverlay}

  The position of each overlay widget is determined by its
  @slot[widget]{halign} and @slot[widget]{valign} properties. E.g. a
  overlay widget with both alignments set to @code{:start} will be placed at
  the top left corner of the main widget, whereas an overlay with the
  @slot[widget]{halign} property set to @code{:center} and the
  @slot[widget]{valign} property set to @code{:end} will be placed a the
  bottom edge of the main widget, horizontally centered. The position can be
  adjusted by setting the margin properties of the overlay widget to non-zero
  values.

  More complicated placement of overlays is possible by connecting to the
  \"get-child-position\" signal.
  @begin[GtkOverlay as GtkBuildable]{dictionary}
    The @sym{gtk:overlay} implementation of the @class{gtk:buildable} interface
    supports placing a child widget as an overlay by specifying
    @code{\"overlay\"} as the @code{\"type\"} attribute of a @code{<child>}
    element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:overlay} implementation has a single CSS node with the name
    @code{overlay}. Overlay children whose alignments cause them to be
    positioned at an edge get the @code{.left}, @code{.right}, @code{.top},
    and/or @code{.bottom} style classes according to their position.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"get-child-position\" signal}
      @begin{pre}
lambda (overlay widget allocation)    :run-last
      @end{pre}
      The signal is emitted to determine the position and size of any overlay
      child widgets. A handler for this signal should fill allocation with the
      desired position and size for @arg{widget}, relative to the 'main' child
      of the overlay. The default handler for this signal uses the
      @slot[widget]{halign} and @slot[widget]{valign} properties of the
      widget to determine the position and gives the widget its natural size,
      except that an alignment of @code{:fill} will cause the overlay to be
      full-width/height. If the main child is a @class{gtk:scrolled-window}
      widget, the overlays are placed relative to its contents.
      @begin[code]{table}
        @entry[overlay]{The @sym{gtk:overlay} widget which emitted the signal.}
        @entry[widget]{The @class{gtk:widget} child widget to position.}
        @entry[allocation]{Return location of type @class{gdk:rectangle} for
          the allocation.}
        @entry[Returns]{@em{True} if the allocation has been filled.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:overlay-child}
  @see-constructor{gtk:overlay-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- overlay-child ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'overlay) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'overlay-child)
      "Accessor"
      (documentation 'overlay-child 'function)
 "@version{#2022-7-31}
  @syntax[]{(gtk:overlay-child object) => child}
  @syntax[]{(setf (gtk:overlay-child object) child)}
  @argument[object]{a @class{gtk:overlay} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:overlay]{child} slot of the @class{gtk:overlay}
    class.
  @end{short}
  The @sym{gtk:overlay-child} function gets the child widget of the overlay.
  The @sym{(setf gtk:overlay-child)} function sets the child widget.
  @see-class{gtk:paned}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline overlay-new))

(defun overlay-new ()
 #+liber-documentation
 "@version{#2022-7-31}
  @return{A new @class{gtk:overlay} widget.}
  @begin{short}
    Creates a new overlay widget.
  @end{short}
  @see-class{gtk:overlay}"
  (make-instance 'overlay))

(export 'overlay-new)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_add_overlay
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_overlay_add_overlay" overlay-add-overlay) :void
 #+liber-documentation
 "@version{#2022-7-31}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be added}
  @begin{short}
    Adds a child widget to the overlay widget.
  @end{short}
  The child widget will be stacked on top of the main widget added with the
  @fun{gtk:overlay-child} function.

  The position at which the child widget is placed is determined from its
  @slot[widget]{halign} and @slot[widget]{valign} properties.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}
  @see-function{gtk:overlay-child}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-add-overlay)

;;; --- overlay-remove-overlay ---------------------------------------------

(defcfun ("gtk_overlay_remove_overlay" overlay-remove-overlay) :void
 #+liber-documentation
 "@version{#2022-7-31}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be removed}
  @begin{short}
    Removes an overlay that was added with the @fun{gtk:overlay-add-overlay}
    function.
  @end{short}
  @see-class{gtk:overlay}
  @see-class{gtk:widget}
  @see-function{gtk:overlay-add-overlay}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-remove-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_measure_overlay
;;; gtk_overlay_set_measure_overlay -> overlay-measure-overlay
;;; ----------------------------------------------------------------------------

(defun (setf overlay-measure-overlay) (value overlay widget)
  (foreign-funcall "gtk_overlay_set_measure_overlay"
                   (g:object overlay) overlay
                   (g:object widget) widget
                   :boolean value
                   :void)
  value)

(defcfun ("gtk_overlay_get_measure_overlay" overlay-measure-overlay)
    :boolean
 #+liber-documentation
 "@version{#2022-7-31}
  @syntax[]{(gtk:overlay-measure-overlay overlay widget) => measure}
  @syntax[]{(setf (gtk:overlay-measure-overlay overlay widget) measure)}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} child widget}
  @argument[measure]{a boolean whether the child widget should be measured}
  @begin{short}
    The @sym{gtk:overlay-measure-overlay} function gets whether the size of the
    child widget is included in the measurement of the overlay.
  @end{short}
  The @sym{(setf gtk:overlay-measure-overlay)} function sets whether the child
  widget is included in the measured size of the overlay.

  The overlay will request the size of the largest child that has this property
  set to @em{true}. Children who are not included may be drawn outside of
  the allocation of the overlay if they are too large.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-measure-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_clip_overlay
;;; gtk_overlay_set_clip_overlay -> overlay-clip-overlay
;;; ----------------------------------------------------------------------------

(defun (setf overlay-clip-overlay) (value overlay widget)
  (foreign-funcall "gtk_overlay_set_clip_overlay"
                   (g:object overlay) overlay
                   (g:object widget) widget
                   :boolean value
                   :void)
  value)

(defcfun ("gtk_overlay_get_clip_overlay" overlay-clip-overlay) :boolean
 #+liber-documentation
 "@version{#2022-7-31}
  @syntax[]{(gtk:overlay-clip-overlay overlay widget) => clip}
  @syntax[]{(setf (gtk:overlay-clip-overlay overlay widget) clip)}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} child widget}
  @argument[clip]{a boolean whether the child widget should be clipped}
  @begin{short}
    The @sym{gtk:overlay-clip-overlay} function gets whether the child widget
    should be clipped within the parent.
  @end{short}
  The @sym{(setf gtk:overlay-clip-overlay)} sets whether the child widget
  should be clipped within the parent.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-clip-overlay)

;;; --- End of file gtk.overlay.lisp -------------------------------------------
