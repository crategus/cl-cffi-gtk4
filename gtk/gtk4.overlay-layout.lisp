;;; ----------------------------------------------------------------------------
;;; gtk4.overlay-layout.lisp
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
;;; GtkOverlayLayout
;;;
;;;     Layout manager that places widgets as overlays
;;;
;;; Types and Values
;;;
;;;     GtkOverlayLayout
;;;     GtkOverlayLayoutChild
;;;
;;; Accessors
;;;
;;;     gtk_overlay_layout_child_set_measure
;;;     gtk_overlay_layout_child_get_measure
;;;     gtk_overlay_layout_child_set_clip_overlay
;;;     gtk_overlay_layout_child_get_clip_overlay
;;;
;;; Functions
;;;
;;;     gtk_overlay_layout_new
;;;
;;; Properties
;;;
;;;     clip-overlay
;;;     measure
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ├── GtkLayoutChild
;;;     │   ╰── GtkOverlayLayoutChild
;;;     ╰── GtkLayoutManager
;;;         ╰── GtkOverlayLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkOverlayLayoutChild
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkOverlayLayoutChild" overlay-layout-child
  (:superclass layout-child
   :export t
   :interfaces ()
   :type-initializer "gtk_overlay_layout_child_get_type")
  ((clip-overlay
    overlay-layout-child-clip-overlay
    "clip-overlay" "gboolean" t t)
   (measure
    overlay-layout-child-measure
    "measure" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'overlay-layout-child 'type)
 "@version{#2023-4-19}
  @begin{short}
    The @sym{gtk:overlay-layout-child} subclass for children in a
    @class{gtk:overlay-layout} object.
  @end{short}
  @see-class{gtk:overlay-layout}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- overlay-layout-child-clip-overlay --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "clip-overlay"
                                               'overlay-layout-child) t)
 "The @code{clip-overlay} property of type @code{:boolean} (Read / Write) @br{}
  Whether the child widget should be clipped to fit the size of the parent.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'overlay-layout-child-clip-overlay)
      "Accessor"
      (documentation 'overlay-layout-child-clip-overlay 'function)
 "@version{#2023-4-19}
  @syntax[]{(gtk:overlay-layout-child-clip-overlay object) => clip}
  @syntax[]{(setf (gtk:overlay-layout-child-clip-overlay object) clip)}
  @argument[object]{a @class{gtk:overlay-layout-child} object}
  @argument[clip]{a boolean whether the child widget is clipped}
  @begin{short}
    Accessor of the @slot[gtk:overlay-layout-child]{clip-overlay} slot of the
    @class{gtk:overlay-layout-child} class.
  @end{short}
  The @sym{gtk:overlay-layout-child-clip-region} function retrieves whether the
  child widget is clipped. The
  @sym{(setf gtk:overlay-layout-child-clip-overlay)} function sets whether to
  clip the child widget.
  @see-class{gtk:overlay-layout-child}")

;;; --- overlay-layout-child-measure -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "measure"
                                               'overlay-layout-child) t)
 "The @code{measure} property of type @code{:boolean} (Read / Write) @br{}
  Whether the child widget size should contribute to the overlay layout
  measurement. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'overlay-layout-child-measure)
      "Accessor"
      (documentation 'overlay-layout-child-measure 'function)
 "@version{#2023-4-19}
  @syntax[]{(gtk:overlay-layout-child-measure object) => measure}
  @syntax[]{(setf (gtk:overlay-layout-child-measure object) measure)}
  @argument[object]{a @class{gtk:overlay-layout-child} object}
  @argument[measure]{a boolean whether to measure the child widget}
  @begin{short}
    Accessor of the @slot[gtk:overlay-layout-child]{measure} slot of the
    @class{gtk:overlay-layout-child} class.
  @end{short}
  The @sym{gtk:overlay-layout-child-measure} function retrieves whether the
  child widget is measured. The @sym{(setf gtk:overlay-layout-child-measure)}
  function sets whether to measure the child widget.
  @see-class{gtk:overlay-layout-child}")

;;; ----------------------------------------------------------------------------
;;; GtkOverlayLayout
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkOverlayLayout" overlay-layout
  (:superclass layout-manager
   :export t
   :interfaces ()
   :type-initializer "gtk_overlay_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'overlay-layout 'type)
 "@version{#2023-4-19}
  @begin{short}
    The @sym{gtk:overlay-layout} object is the layout manager used by
    the @class{gtk:overlay} widget.
  @end{short}
  It places widgets as overlays on top of the main child.

  This is not a reusable layout manager, since it expects its widget to be a
  @class{gtk:overlay} widget. It only listed here so that its layout properties
  get documented.
  @see-constructor{gtk:overlay-layout-new}
  @see-class{gtk:overlay}")

;;; ----------------------------------------------------------------------------
;;; gtk_overlay_layout_new ()
;;; ----------------------------------------------------------------------------

(defun overlay-layout-new ()
 #+liber-documentation
 "@version{#2023-4-19}
  @return{The newly created @class{gtk:overlay-layout} object}
  @short{Creates a new overlay layout manager.}
  @see-class{gtk:overlay-layout}"
  (make-instance 'overlay-layout))

(export 'overlay-layout-new)

;;; --- End of file gtk4.overlay-layout.lisp -----------------------------------
