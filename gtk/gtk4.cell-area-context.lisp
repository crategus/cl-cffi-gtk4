;;; ----------------------------------------------------------------------------
;;; gtk4.cell-area-context.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; GtkCellAreaContext
;;;
;;;     Stores geometrical information for a series of rows in a GtkCellArea
;;;
;;; Types and Values
;;;
;;;     GtkCellAreaContext
;;;
;;; Accessors
;;;
;;;     gtk_cell_area_context_get_area
;;;
;;; Functions
;;;
;;;     gtk_cell_area_context_allocate
;;;     gtk_cell_area_context_reset
;;;     gtk_cell_area_context_get_preferred_width
;;;     gtk_cell_area_context_get_preferred_height
;;;     gtk_cell_area_context_get_preferred_height_for_width
;;;     gtk_cell_area_context_get_preferred_width_for_height
;;;     gtk_cell_area_context_get_allocation
;;;     gtk_cell_area_context_push_preferred_width
;;;     gtk_cell_area_context_push_preferred_height
;;;
;;; Properties
;;;
;;;     area
;;;     minimum-height
;;;     minimum-width
;;;     natural-height
;;;     natural-width
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkCellAreaContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellAreaContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellAreaContext" cell-area-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_area_context_get_type")
  ((area
    cell-area-context-area
    "area" "GtkCellArea" t t)
   (minimum-height
    cell-area-context-minimum-height
    "minimum-height" "gint" t nil)
   (minimum-width
    cell-area-context-minimum-width
    "minimum-width" "gint" t nil)
   (natural-height
    cell-area-context-natural-height
    "natural-height" "gint" t nil)
   (natural-width
    cell-area-context-natural-width
    "natural-width" "gint" t nil)))

#+liber-documentation
(setf (documentation 'cell-area-context 'type)
 "@version{#2020-6-28}
  @begin{short}
    The @sym{gtk:cell-area-context} object is created by a given
    @class{gtk:cell-area} implementation via its @code{create_context()} virtual
    method and is used to store cell sizes and alignments for a series of
    @class{gtk:tree-model} rows that are requested and rendered in the same
    context.
  @end{short}

  @class{gtk:cell-layout} widgets can create any number of contexts in which to
  request and render groups of data rows. However its important that the same
  context which was used to request sizes for a given @class{gtk:tree-model} row
  also be used for the same row when calling other @class{gtk:cell-area} APIs
  such as the @fun{gtk:cell-area-render} and @fun{gtk:cell-area-event}
  functions.
  @see-slot{gtk:cell-area-context-area}
  @see-slot{gtk:cell-area-context-minimum-height}
  @see-slot{gtk:cell-area-context-minimum-width}
  @see-slot{gtk:cell-area-context-natural-height}
  @see-slot{gtk:cell-area-context-natural-width}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-class{gtk:cell-layout}
  @see-function{gtk:cell-area-render}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- cell-area-context-area ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "area"
                                               'cell-area-context) t)
 "The @code{area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area this context was created by.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-context-area)
      "Accessor"
      (documentation 'cell-area-context-area 'function)
 "@version{#2020-6-28}
  @syntax[]{(gtk:cell-area-context-area object) => area}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[area]{a @class{gtk:cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-context]{area} slot of the
    @class{gtk:cell-area-context} class.
  @end{short}

  The @sym{gtk:cell-area-context-area} function fetches the
  @class{gtk:cell-area} object the context was created by.

  This is generally unneeded by layouting widgets. However it is important for
  the context implementation itself to fetch information about the area it is
  being used for. For instance at @code{GtkCellAreaContextClass.allocate()}
  time its important to know details about any cell spacing that the
  @class{gtk:cell-area} is configured with in order to compute a proper
  allocation.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}")

;;; --- cell-area-context-minimum-height -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "minimum-height"
                                               'cell-area-context) t)
 "The @code{minimum-height} property of type @code{:int} (Read) @br{}
  The minimum height for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-height} function. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-context-minimum-height)
      "Accessor"
      (documentation 'cell-area-context-minimum-height 'function)
 "@version{#2020-6-28}
  @syntax[]{(gtk:cell-area-context-minimum-height object) => height}
  @syntax[]{(setf (gtk:cell-area-context-minimum-height object) height}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[height]{an integer with the minimum height}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-context]{minimum-height} slot of the
    @class{gtk:cell-area-context} class.
  @end{short}

  The minimum height for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-height} function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-height}")

;;; --- cell-area-context-minimum-width ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "minimum-width"
                                               'cell-area-context) t)
 "The @code{minimum-width} property of type @code{:int} (Read) @br{}
  The minimum width for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-width} function. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-context-minimum-width)
      "Accessor"
      (documentation 'cell-area-context-minimum-width 'function)
 "@version{#2020-6-28}
  @syntax[]{(gtk:cell-area-context-minimum-width object) => width}
  @syntax[]{(setf (gtk:cell-area-context-minimum-width object) width}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[width]{an integer with the minimum width}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-context]{minimum-width} slot of the
    @class{gtk:cell-area-context} class.
  @end{short}

  The minimum width for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-width} function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-width}")

;;; --- cell-area-context-natural-height -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "natural-height"
                                               'cell-area-context) t)
 "The @code{natural-height} property of type @code{:int} (Read) @br{}
  The natural height for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-height} function. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-context-natural-height)
      "Accessor"
      (documentation 'cell-area-context-natural-height 'function)
 "@version{#2020-6-28}
  @syntax[]{(gtk:cell-area-context-natural-height object) => height}
  @syntax[]{(setf (gtk:cell-area-context-natural-height object) height}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[height]{an integer with the natural height}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-context]{natural-height} slot of the
    @class{gtk:cell-area-context} class.
  @end{short}

  The natural height for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-height} function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-height}")

;;; --- cell-area-context-natural-width ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "natural-width"
                                               'cell-area-context) t)
 "The @code{natural-width} property of type @code{:int} (Read) @br{}
  The natural width for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-width} function. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-context-natural-width)
      "Accessor"
      (documentation 'cell-area-context-natural-width 'function)
 "@version{#2020-6-28}
  @syntax[]{(gtk:cell-area-context-natural-width object) => width}
  @syntax[]{(setf (gtk:cell-area-context-natural-width object) width}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[width]{an integer with the natural width}
  @begin{short}
    Accessor of the @slot[gtk:cell-area-context]{natural-width} slot of the
    @class{gtk:cell-area-context} class.
  @end{short}

  The natural width for the @class{gtk:cell-area} in this context for all
  @class{gtk:tree-model} rows that this context was requested for using the
  @fun{gtk:cell-area-preferred-width} function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-width}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_allocate ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_allocate" cell-area-context-allocate) :void
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[width]{an integer with the allocated width for all
    @class{gtk:tree-model} rows rendered with context, or -1}
  @argument[height]{an integer with the allocated height for all
    @class{gtk:tree-model} rows rendered with context, or -1}
  @begin{short}
    Allocates a width and/or a height for all rows which are to be rendered
    with @arg{context}.
  @end{short}

  Usually allocation is performed only horizontally or sometimes vertically
  since a group of rows are usually rendered side by side vertically or
  horizontally and share either the same width or the same height. Sometimes
  they are allocated in both horizontal and vertical orientations producing a
  homogeneous effect of the rows. This is generally the case for
  @class{gtk:tree-view} when the @slot[gtk:tree-view]{fixed-height-mode}
  property is enabled.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-view}"
  (context (g:object cell-area-context))
  (width :int)
  (height :int))

(export 'cell-area-context-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_reset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_reset" cell-area-context-reset) :void
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @begin{short}
    Resets any previously cached request and allocation data.
  @end{short}

  When underlying @class{gtk:tree-model} data changes its important to reset the
  context if the content size is allowed to shrink. If the content size is only
  allowed to grow, this is usually an option for views rendering large data
  stores as a measure of optimization, then only the row that changed or was
  inserted needs to be (re)requested with the
  @fun{gtk:cell-area-preferred-width} function.

  When the new overall size of the context requires that the allocated size
  changes, or whenever this allocation changes at all, the variable row sizes
  need to be re-requested for every row.

  For instance, if the rows are displayed all with the same width from top to
  bottom then a change in the allocated width necessitates a recalculation of
  all the displayed row heights using the
  @fun{gtk:cell-area-preferred-height-for-width} function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-width}
  @see-function{gtk:cell-area-preferred-height-for-width}"
  (context (g:object cell-area-context)))

(export 'cell-area-context-reset)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_get_preferred_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_get_preferred_width"
          %cell-area-context-preferred-width) :void
  (context (g:object cell-area-context))
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun cell-area-context-preferred-width (context)
 #+liber-documentation
 "@version{#2020-6-27}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}
      @br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Gets the accumulative preferred width for all rows which have been requested
    with this context.
  @end{short}

  After the @fun{gtk:cell-area-context-reset} function is called and/or before
  ever requesting the size of a @class{gtk:cell-area}, the returned values
  are 0.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-context-reset}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%cell-area-context-preferred-width context
                                        minimum-width
                                        natural-width)
    (values (cffi:mem-ref minimum-width :int)
            (cffi:mem-ref natural-width :int))))

(export 'cell-area-context-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_get_preferred_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_get_preferred_height"
          %cell-area-context-preferred-height) :void
  (context (g:object cell-area-context))
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun cell-area-context-preferred-height (context)
 #+liber-documentation
 "@version{#2020-6-27}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @begin{return}
    @arg{minimum-height} -- an integer with the minimum height, or @code{nil}
      @br{}
    @arg{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Gets the accumulative preferred height for all rows which have been
    requested with this context.
  @end{short}

  After the @fun{gtk:cell-area-context-reset} function is called and/or before
  ever requesting the size of a @class{gtk:cell-area}, the returned values
  are 0.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-context-reset}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%cell-area-context-preferred-height context
                                         minimum-height
                                         natural-height)
    (values (cffi:mem-ref minimum-height :int)
            (cffi:mem-ref natural-height :int))))

(export 'cell-area-context-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_get_preferred_height_for_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_get_preferred_height_for_width"
          %cell-area-context-preferred-height-for-width) :void
  (context (g:object cell-area-context))
  (width :int)
  (minium-height (:pointer :int))
  (natural-height (:pointer :int)))

(defun cell-area-context-preferred-height-for-width (context width)
 #+liber-documentation
 "@version{#2020-6-27}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[width]{an integer with a proposed width for allocation}
  @begin{return}
    @arg{minimum-height} -- an integer with the minimum height, or @code{nil}
      @br{}
    @arg{natural-height} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Gets the accumulative preferred height for @arg{width} for all rows which
    have been requested for the same said width with this context.
  @end{short}

  After the @fun{gtk:cell-area-context-reset} function is called and/or before
  ever requesting the size of a @class{gtk:cell-area}, the returned values
  are -1.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-context-reset}"
  (with-foreign-objects ((minimum-height :int) (natural-height :int))
    (%cell-area-context-preferred-height-for-width context
                                                   width
                                                   minimum-height
                                                   natural-height)
    (values (cffi:mem-ref minimum-height :int)
            (cffi:mem-ref natural-height :int))))

(export 'cell-area-context-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_get_preferred_width_for_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_get_preferred_width_for_height"
          %cell-area-context-preferred-width-for-height) :void
  (context (g:object cell-area-context))
  (height :int)
  (minium-width (:pointer :int))
  (natural-width (:pointer :int)))

(defun cell-area-context-preferred-width-for-height (context height)
 #+liber-documentation
 "@version{#2020-6-27}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[height]{an integer with a proposed height for allocation}
  @begin{return}
    @arg{minimum-width} -- an integer with the minimum width, or @code{nil}
      @br{}
    @arg{natural-width} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Gets the accumulative preferred width for @arg{height} for all rows which
    have been requested for the same said height with this context.
  @end{short}

  After the @fun{gtk:cell-area-context-reset} function is called and/or before
  ever requesting the size of a @class{gtk:cell-area}, the returned values
  are -1.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-context-reset}"
  (with-foreign-objects ((minimum-width :int) (natural-width :int))
    (%cell-area-context-preferred-height-for-width context
                                                   height
                                                   minimum-width
                                                   natural-width)
    (values (cffi:mem-ref minimum-width :int)
            (cffi:mem-ref natural-width :int))))

(export 'cell-area-context-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_get_allocation () -> cell-area-context-allocation
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_get_allocation"
          %cell-area-context-allocation) :void
  (context (g:object cell-area-context))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun cell-area-context-allocation (context)
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @begin{return}
    @arg{width} -- an integer with the allocated width, or @code{nil} @br{}
    @arg{height} -- an integer with the allocated height, or @code{nil}
  @end{return}
  @begin{short}
    Fetches the current allocation size for @arg{context}.
  @end{short}

  If the context was not allocated in @arg{width} or @arg{height}, or if the
  context was recently reset with the @fun{gtk:cell-area-context-reset}
  function, the returned value will be -1.
  @see-class{gtk:cell-area-context}
  @see-function{gtk:cell-area-context-reset}"
  (with-foreign-objects ((width :int) (height :int))
    (%cell-area-context-allocation context width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'cell-area-context-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_push_preferred_width ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_push_preferred_width"
           cell-area-context-push-preferred-width) :void
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[minimum-width]{an integer with the proposed new minimum width for
    @arg{context}}
  @argument[natural-width]{an integer with the proposed new natural width for
    @arg{context}}
  @begin{short}
    Causes the minimum and/or natural width to grow if the new proposed sizes
    exceed the current minimum and natural width.
  @end{short}

  This is used by @class{gtk:cell-area-context} implementations during the
  request process over a series of @class{gtk:tree-model} rows to progressively
  push the requested width over a series of the
  @fun{gtk:cell-area-preferred-width} requests function.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-width}"
  (context (g:object cell-area-context))
  (minimum-width :int)
  (natural-width :int))

(export 'cell-area-context-push-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_context_push_preferred_height ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_cell_area_context_push_preferred_height"
           cell-area-context-push-preferred-height) :void
 #+liber-documentation
 "@version{#2020-6-28}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[minimum-height]{an integer with the proposed new minimum height for
    @arg{context}}
  @argument[natural-height]{an integer with the proposed new natural height for
    @arg{context}}
  @begin{short}
    Causes the minimum and/or natural height to grow if the new proposed sizes
    exceed the current minimum and natural height.
  @end{short}

  This is used by @class{gtk:cell-area-context} implementations during the
  request process over a series of @class{gtk:tree-model} rows to progressively
  push the requested height over a series of the
  @fun{gtk:cell-area-preferred-height} function requests.
  @see-class{gtk:cell-area-context}
  @see-class{gtk:tree-model}
  @see-function{gtk:cell-area-preferred-height}"
  (context (g:object cell-area-context))
  (minimum-height :int)
  (natural-height :int))

(export 'cell-area-context-push-preferred-height)

;;; --- End of file gtk4.cell-area-context.lisp --------------------------------
