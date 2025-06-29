;;; ----------------------------------------------------------------------------
;;; gtk4.overlay.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkOverlay" overlay
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
 "@version{2025-06-29}
  @begin{short}
    The @class{gtk:overlay} widget contains a single main widget, on top of
    which it can place overlay widgets.
  @end{short}

  @image[overlay]{Figure: GtkOverlay}

  The position of each overlay widget is determined by its
  @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties. For
  example, an overlay widget with both alignments set to @val[gtk:align]{:start}
  will be placed at the top left corner of the main widget, whereas an overlay
  widget with the @slot[gtk:widget]{halign} property set to
  @val[gtk:align]{:center} and the @slot[gtk:widget]{valign} property set to
  @val[gtk:align]{:end} will be placed a the bottom edge of the main widget,
  horizontally centered. The position can be adjusted by setting the margin
  properties of the overlay widget to non-zero values.

  More complicated placement of overlay widgets is possible by connecting to
  the @sig[gtk:overlay]{get-child-position} signal.

  The minimum and natural sizes of an overlay widget are those of its main
  child. The sizes of overlay children are not considered when measuring these
  preferred sizes.
  @begin[GtkOverlay as GtkBuildable]{dictionary}
    The @class{gtk:overlay} implementation of the @class{gtk:buildable}
    interface supports placing a child widget as an overlay by specifying
    @code{\"overlay\"} as the @code{\"type\"} attribute of a @code{<child>}
    element.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:overlay} implementation has a single CSS node with the name
    @code{overlay}. Overlay children whose alignments cause them to be
    positioned at an edge get the @code{.left}, @code{.right}, @code{.top},
    and/or @code{.bottom} style classes according to their position.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[overlay::get-child-position]{signal}
      @begin{pre}
lambda (overlay widget allocation)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[overlay]{The @class{gtk:overlay} widget which emitted the
          signal.}
        @entry[widget]{The @class{gtk:widget} overlay widget to position.}
        @entry[allocation]{Return location of type @class{gdk:rectangle} for
          the allocation.}
        @entry[Returns]{@em{True} if @arg{allocation} has been filled.}
      @end{simple-table}
      The signal is emitted to determine the position and size of any overlay
      widgets. A handler for this signal should fill @arg{allocation} with the
      desired position and size for @arg{widget}, relative to the main child
      of the overlay. The default handler for this signal uses the
      @slot[gtk:widget]{halign} and @slot[gtk:widget]{valign} properties of the
      widget to determine the position and gives the widget its natural size,
      except that an alignment of @val[gtk:align]{:fill} will cause the overlay
      to be full-width/height. If the main child is a
      @class{gtk:scrolled-window} widget, the overlays are placed relative to
      its contents.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:overlay-child}
  @see-constructor{gtk:overlay-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:overlay-child ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'overlay) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The main child widget of the overlay. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'overlay-child)
      "Accessor"
      (documentation 'overlay-child 'function)
 "@version{2024-10-02}
  @syntax{(gtk:overlay-child object) => child}
  @syntax{(setf (gtk:overlay-child object) child)}
  @argument[object]{a @class{gtk:overlay} widget}
  @argument[child]{a @class{gtk:widget} main widget}
  @begin{short}
    Accessor of the @slot[gtk:overlay]{child} slot of the @class{gtk:overlay}
    class.
  @end{short}
  The @fun{gtk:overlay-child} function gets the main widget of the overlay.
  The @setf{gtk:overlay-child} function sets the main widget.
  @see-class{gtk:paned}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_new
;;; ----------------------------------------------------------------------------

(declaim (inline overlay-new))

(defun overlay-new ()
 #+liber-documentation
 "@version{2024-10-02}
  @return{The new @class{gtk:overlay} widget.}
  @short{Creates a new overlay.}
  @see-class{gtk:overlay}"
  (make-instance 'overlay))

(export 'overlay-new)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_add_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_overlay_add_overlay" overlay-add-overlay) :void
 #+liber-documentation
 "@version{2024-10-02}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} overlay widget to be added}
  @begin{short}
    Adds an overlay widget to the overlay.
  @end{short}
  The overlay widget will be stacked on top of the main widget added with the
  @fun{gtk:overlay-child} function. The position at which the overlay widget
  is placed is determined from its @slot[gtk:widget]{halign} and
  @slot[gtk:widget]{valign} properties.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}
  @see-function{gtk:overlay-child}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-add-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_remove_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_overlay_remove_overlay" overlay-remove-overlay) :void
 #+liber-documentation
 "@version{2024-10-02}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} overlay widget to be removed}
  @begin{short}
    Removes an overlay widget that was added with the
    @fun{gtk:overlay-add-overlay} function.
  @end{short}
  @see-class{gtk:overlay}
  @see-class{gtk:widget}
  @see-function{gtk:overlay-add-overlay}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-remove-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_measure_overlay
;;; gtk_overlay_set_measure_overlay
;;; ----------------------------------------------------------------------------

(defun (setf overlay-measure-overlay) (value overlay widget)
  (cffi:foreign-funcall "gtk_overlay_set_measure_overlay"
                        (g:object overlay) overlay
                        (g:object widget) widget
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_overlay_get_measure_overlay" overlay-measure-overlay)
    :boolean
 #+liber-documentation
 "@version{2024-10-02}
  @syntax{(gtk:overlay-measure-overlay overlay widget) => measure}
  @syntax{(setf (gtk:overlay-measure-overlay overlay widget) measure)}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} overlay widget}
  @argument[measure]{a boolean whether the overlay widget should be measured}
  @begin{short}
    The @fun{gtk:overlay-measure-overlay} function gets whether the size of the
    overlay widget is included in the measurement of the overlay.
  @end{short}
  The @setf{gtk:overlay-measure-overlay} function sets the property.

  The overlay will request the size of the largest overlay widget that has this
  property set to @em{true}. Children who are not included may be drawn outside
  of the allocation of the overlay if they are too large.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-measure-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_get_clip_overlay
;;; gtk_overlay_set_clip_overlay
;;; ----------------------------------------------------------------------------

(defun (setf overlay-clip-overlay) (value overlay widget)
  (cffi:foreign-funcall "gtk_overlay_set_clip_overlay"
                        (g:object overlay) overlay
                        (g:object widget) widget
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_overlay_get_clip_overlay" overlay-clip-overlay) :boolean
 #+liber-documentation
 "@version{2024-10-02}
  @syntax{(gtk:overlay-clip-overlay overlay widget) => clip}
  @syntax{(setf (gtk:overlay-clip-overlay overlay widget) clip)}
  @argument[overlay]{a @class{gtk:overlay} widget}
  @argument[widget]{a @class{gtk:widget} overlay widget}
  @argument[clip]{a boolean whether the overlay widget should be clipped}
  @begin{short}
    The @fun{gtk:overlay-clip-overlay} function gets whether the overlay widget
    should be clipped within the overlay.
  @end{short}
  The @setf{gtk:overlay-clip-overlay} function sets the property.
  @see-class{gtk:overlay}
  @see-class{gtk:widget}"
  (overlay (g:object overlay))
  (widget (g:object widget)))

(export 'overlay-clip-overlay)

;;; --- End of file gtk4.overlay.lisp ------------------------------------------
