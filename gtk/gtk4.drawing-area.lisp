;;; ----------------------------------------------------------------------------
;;; gtk4.drawing-area.lisp
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
;;; GtkDrawingArea
;;;
;;;     A widget for custom user interface elements
;;;
;;; Types and Values
;;;
;;;     GtkDrawingArea
;;;
;;; Accessors
;;;
;;;     gtk_drawing_area_get_content_height
;;;     gtk_drawing_area_set_content_height
;;;     gtk_drawing_area_get_content_width
;;;     gtk_drawing_area_set_content_width
;;;
;;; Callbacks
;;;
;;;     GtkDrawingAreaDrawFunc
;;;
;;; Functions
;;;
;;;     gtk_drawing_area_new
;;;     gtk_drawing_area_set_draw_func
;;;
;;; Properties
;;;
;;;     content-height
;;;     content-width
;;;
;;; Signals
;;;
;;;     resize
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDrawingArea
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDrawingArea
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDrawingArea" drawing-area
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_drawing_area_get_type")
  ((content-height
    drawing-area-content-height
    "content-height" "gint" t t)
   (content-width
    drawing-area-content-width
    "content-width" "gint" t t)))

#+liber-documentation
(setf (documentation 'drawing-area 'type)
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:drawing-area} widget is a widget that allows drawing with
    Cairo.
  @end{short}
  It is essentially a blank widget. You can draw on it.

  @image[drawing-area]{Figure: GtkDrawingArea}

  After creating a drawing area, the application may want to connect to:
  @begin{itemize}
    @begin{item}
      The @sig[gtk:widget]{realize} signal to take any necessary actions when
      the widget is instantiated on a particular display. Create GDK resources
      in response to this signal.
    @end{item}
    @begin{item}
      The @sig[gtk:drawing-area]{resize} signal to take any necessary actions
      when the widget changes size.
    @end{item}
    @begin{item}
      Call the @fun{gtk:drawing-area-set-draw-func} function to handle redrawing
      the contents of the widget.
    @end{item}
  @end{itemize}
  The draw function is normally called when a drawing area first comes onscreen,
  or when it is covered by another window and then uncovered. You can also force
  a redraw by adding to the \"damage region\" of the drawing area’s window using
  the @fun{gtk:widget-queue-draw} function. This will cause the drawing area to
  call the draw function again.

  The available routines for drawing are documented in the Cairo documentation.
  GDK offers additional API to integrate with Cairo, like the
  @fun{gdk:cairo-set-source-rgba} or @fun{gdk:cairo-set-source-pixbuf}
  functions.

  To receive mouse events on a drawing area, you will need to use event
  controllers. To receive keyboard events, you will need to set the
  @slot[gtk:widget]{can-focus} property on the drawing area, and you should
  probably draw some user-visible indication that the drawing area is focused.

  If you need more complex control over your widget, you should consider
  creating your own GtkWidget subclass.
  @begin[Examples]{dictionary}
    The following example demonstrates using a drawing area to display a
    circle in the normal widget foreground color.
    @begin{pre}
(defun do-drawing-area (&optional application)
  (let* ((area (make-instance 'gtk:drawing-area))
         (window (make-instance 'gtk:window
                                :application application
                                :child area
                                :title \"Drawing Area\"
                                :default-width 400
                                :default-height 300)))
    ;; Set a drawing function
    (gtk:drawing-area-set-draw-func area
        (lambda (widget cr width height)
          (let ((color (gtk:widget-color widget)))
            (gdk:cairo-set-source-rgba cr color)
            ;; Draw and fill a circle on the drawing area
            (cairo:arc cr
                       (/ width 2.0)
                       (/ height 2.0)
                       (- (/ (min width height) 2.0) 12)
                       0.0
                       (* 2.0 pi))
            (cairo:fill cr))))
    ;; Show the window
    (setf (gtk:widget-visible window) t)))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[drawing-area::resize]{signal}
      @begin{pre}
lambda (area width height)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[area]{The @class{gtk:drawing-area} widget that emitted the
          signal.}
        @entry[width]{The integer for the width of the viewport.}
        @entry[height]{The integer for the height of the viewport.}
      @end{simple-table}
      The signal is emitted once when the widget is realized, and then each
      time the widget is changed while realized. This is useful in order to
      keep state up to date with the widget size, like for instance a backing
      surface.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:drawing-area-content-height}
  @see-slot{gtk:drawing-area-content-width}
  @see-constructor{gtk:drawing-area-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:drawing-area-content-height ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content-height"
                                               'drawing-area) t)
 "The @code{content-height} property of type @code{:int} (Read / Write) @br{}
  The content height. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'drawing-area-content-height)
      "Accessor"
      (documentation 'drawing-area-content-height 'function)
 "@version{2025-09-28}
  @syntax{(gtk:drawing-area-content-height object) => height}
  @syntax{(setf (gtk:drawing-area-content-height object) height)}
  @argument[object]{a @class{gtk:drawing-area} widget}
  @argument[height]{an integer for the content height}
  @begin{short}
    The accessor for the @slot[gtk:drawing-area]{content-height} slot of the
    @class{gtk:drawing-area} class gets or sets the content height of the
    drawing area.
  @end{short}

  Note that because widgets may be allocated larger sizes than they requested,
  it is possible that the actual height passed to your draw function is larger
  than the height set here. You can use the @fun{gtk:widget-valign} function to
  avoid that.

  If the height is set to 0, the default, the drawing area may disappear.
  @see-class{gtk:drawing-area}
  @see-function{gtk:drawing-area-content-width}
  @see-function{gtk:widget-valign}")

;;; --- gtk:drawing-area-content-width -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content-width"
                                               'drawing-area) t)
 "The @code{content-width} property of type @code{:int} (Read / Write) @br{}
  The content width. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'drawing-area-content-width)
      "Accessor"
      (documentation 'drawing-area-content-width 'function)
 "@version{2025-09-28}
  @syntax{(gtk:drawing-area-content-width object) => width}
  @syntax{(setf (gtk:drawing-area-content-width object) width)}
  @argument[object]{a @class{gtk:drawing-area} widget}
  @argument[width]{an integer for the content height}
  @begin{short}
    The accessor for the @slot[gtk:drawing-area]{content-width} slot of the
    @class{gtk:drawing-area} class gets or sets the content width of the drawing
    area.
  @end{short}

  Note that because widgets may be allocated larger sizes than they requested,
  it is possible that the actual height passed to your draw function is larger
  than the height set here. You can use the @fun{gtk:widget-halign} function to
  avoid that.

  If the height is set to 0, the default, the drawing area may disappear.
  @see-class{gtk:drawing-area}
  @see-function{gtk:drawing-area-content-width}
  @see-function{gtk:widget-halign}")

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_new
;;; ----------------------------------------------------------------------------

(declaim (inline drawing-area-new))

(defun drawing-area-new ()
 #+liber-documentation
 "@version{2024-10-26}
  @return{The new @class{gtk:drawing-area} widget.}
  @short{Creates a new drawing area.}
  @see-class{gtk:drawing-area}"
  (make-instance 'drawing-area))

(export 'drawing-area-new)

;;; ----------------------------------------------------------------------------
;;; GtkDrawingAreaDrawFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback drawing-area-draw-func :void
    ((area (g:object drawing-area))
     (cr (:pointer (:struct cairo:context-t)))
     (width :int)
     (height :int)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func area cr width height)))

#+liber-documentation
(setf (liber:alias-for-symbol 'drawing-area-draw-func)
      "Callback"
      (liber:symbol-documentation 'drawing-area-draw-func)
 "@version{2025-07-25}
  @syntax{lambda (area cr width height)}
  @argument[area]{a @class{gtk:drawing-area} widget}
  @argument[cr]{a @sym{cairo:context-t} instance to draw to}
  @argument[height]{an integer for the actual width of the contents}
  @argument[width]{an integer for the actual height of the contents}
  @begin{short}
    Whenever the drawing area needs to redraw, this callback function will be
    called.
  @end{short}
  This function should exclusively redraw the contents of the drawing area and
  must not call any widget functions that cause changes.
  @see-class{gtk:drawing-area}
  @see-symbol{cairo:context-t}
  @see-function{gtk:drawing-area-content-height}
  @see-function{gtk:drawing-area-content-width}")

(export 'drawing-area-draw-func)

;;; ----------------------------------------------------------------------------
;;; gtk_drawing_area_set_draw_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drawing_area_set_draw_func" %drawing-area-set-draw-func)
    :void
  (area (g:object drawing-area))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun drawing-area-set-draw-func (area func)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[area]{a @class{gtk:drawing-area} widget}
  @argument[func]{a @sym{gtk:drawing-area-draw-func} callback function}
  @begin{short}
    Setting a draw function is the main thing you want to do when using a
    drawing area.
  @end{short}
  The draw function is called whenever GTK needs to draw the contents of the
  drawing area to the screen.

  The draw function will be called during the drawing stage of GTK. In the
  drawing stage it is not allowed to change properties of any GTK widgets or
  call any functions that would cause any properties to be changed. You should
  restrict yourself exclusively to drawing your contents in the draw function.

  If what you are drawing does change, call the @fun{gtk:widget-queue-draw}
  function on the drawing area. This will cause a redraw and will call the draw
  function again.
  @see-class{gtk:drawing-area}
  @see-symbol{gtk:drawing-area-draw-func}
  @see-function{gtk:widget-queue-draw}"
  (%drawing-area-set-draw-func area
          (cffi:callback drawing-area-draw-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'drawing-area-set-draw-func)

;;; --- End of file gtk4.drawing-area.lisp -------------------------------------
