;;; ----------------------------------------------------------------------------
;;; gtk4.cell-area.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkCellArea
;;;
;;;     An abstract class for laying out GtkCellRenderers
;;;
;;; Types and Values
;;;
;;;     GtkCellArea
;;;
;;; Accessors
;;;
;;;     gtk_cell_area_get_edit_widget
;;;     gtk_cell_area_get_edited_cell
;;;     gtk_cell_area_set_focus_cell
;;;     gtk_cell_area_get_focus_cell
;;;
;;; Functions
;;;
;;;     GtkCellCallback
;;;     GtkCellAllocCallback
;;;
;;;     gtk_cell_area_add
;;;     gtk_cell_area_remove
;;;     gtk_cell_area_has_renderer
;;;     gtk_cell_area_foreach
;;;     gtk_cell_area_foreach_alloc
;;;     gtk_cell_area_event
;;;     gtk_cell_area_snapshot
;;;     gtk_cell_area_get_cell_allocation
;;;     gtk_cell_area_get_cell_at_position
;;;     gtk_cell_area_create_context
;;;     gtk_cell_area_copy_context
;;;     gtk_cell_area_get_request_mode
;;;     gtk_cell_area_get_preferred_width
;;;     gtk_cell_area_get_preferred_height_for_width
;;;     gtk_cell_area_get_preferred_height
;;;     gtk_cell_area_get_preferred_width_for_height
;;;     gtk_cell_area_get_current_path_string
;;;     gtk_cell_area_apply_attributes
;;;     gtk_cell_area_attribute_connect
;;;     gtk_cell_area_attribute_disconnect
;;;     gtk_cell_area_attribute_get_column
;;;     gtk_cell_area_class_install_cell_property           not implemented
;;;     gtk_cell_area_class_find_cell_property
;;;     gtk_cell_area_class_list_cell_properties
;;;     gtk_cell_area_add_with_properties
;;;     gtk_cell_area_cell_set
;;;     gtk_cell_area_cell_get
;;;     gtk_cell_area_cell_set_valist                       not needed
;;;     gtk_cell_area_cell_get_valist                       not needed
;;;     gtk_cell_area_cell_set_property
;;;     gtk_cell_area_cell_get_property
;;;     gtk_cell_area_is_activatable
;;;     gtk_cell_area_activate
;;;     gtk_cell_area_focus
;;;     gtk_cell_area_add_focus_sibling
;;;     gtk_cell_area_remove_focus_sibling
;;;     gtk_cell_area_is_focus_sibling
;;;     gtk_cell_area_get_focus_siblings
;;;     gtk_cell_area_get_focus_from_sibling
;;;     gtk_cell_area_activate_cell
;;;     gtk_cell_area_stop_editing
;;;     gtk_cell_area_inner_cell_area
;;;     gtk_cell_area_request_renderer
;;;
;;; Properties
;;;
;;;     edit-widget
;;;     edited-cell
;;;     focus-cell
;;;
;;; Signals
;;;
;;;     add-editable
;;;     apply-attributes
;;;     focus-changed
;;;     remove-editable
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellArea
;;;             ╰── GtkCellAreaBox
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellLayout
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellArea
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCellArea" cell-area
  (:superclass g:initially-unowned
   :export t
   :interfaces ("GtkCellLayout"
                "GtkBuildable")
   :type-initializer "gtk_cell_area_get_type")
  ((edit-widget
    cell-area-edit-widget
    "edit-widget" "GtkCellEditable" t nil)
   (edited-cell
    cell-area-edited-cell
    "edited-cell" "GtkCellRenderer" t nil)
   (focus-cell
    cell-area-focus-cell
    "focus-cell" "GtkCellRenderer" t nil)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj cell-area) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:CELL-AREA is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'cell-area 'type)
 "@version{2024-7-7}
  @begin{short}
    The @class{gtk:cell-area} class is an abstract class for
    @class{gtk:cell-layout} widgets, also referred to as \"layouting widgets\",
    to interface with an arbitrary number of @class{gtk:cell-renderer} objects
    and interact with the user for a given @class{gtk:tree-model} row.
  @end{short}

  The cell area handles events, focus navigation, drawing and size requests
  and allocations for a given row of data.

  Usually users do not have to interact with the @class{gtk:cell-area} object
  directly unless they are implementing a cell-layouting widget themselves.

  @subheading{Requesting area sizes}
  As outlined in the @class{gtk:widget} geometry management section, GTK uses a
  height-for-width geometry management system to compute the sizes of widgets
  and user interfaces. The @class{gtk:cell-area} object uses the same semantics
  to calculate the size of an area for an arbitrary number of
  @class{gtk:tree-model} rows.

  When requesting the size of a cell area one needs to calculate the size for
  a handful of rows, and this will be done differently by different layouting
  widgets. For instance a @class{gtk:tree-view-column} object always lines up
  the areas from top to bottom while a @class{gtk:icon-view} widget on the other
  hand might enforce that all areas received the same width and wrap the areas
  around, requesting height for more cell areas when allocated less width.

  It is also important for areas to maintain some cell alignments with areas
  rendered for adjacent rows, cells can appear \"columnized\" inside an area
  even when the size of cells are different in each row. For this reason the
  @class{gtk:cell-area} object uses a @class{gtk:cell-area-context} object to
  store the alignments and sizes along the way, as well as the overall largest
  minimum and natural size for all the rows which have been calculated with the
  said context.

  The @class{gtk:cell-area-context} object is an opaque object specific to the
  @class{gtk:cell-area} object which created it, see the
  @fun{gtk:cell-area-create-context} function. The owning cell-layouting widget
  can create as many contexts as it wishes to calculate sizes of rows which
  should receive the same size in at least one orientation, horizontally or
  vertically. However, it is important that the same
  @class{gtk:cell-area-context} object which was used to request the sizes for
  a given @class{gtk:tree-model} row be used when rendering or processing events
  for that row.

  In order to request the width of all the rows at the root level of a
  @class{gtk:tree-model} object one would do the following:

  @b{Example:}
  Requesting the width of a handful of @class{gtk:tree-model} rows
  @begin{pre}
GtkTreeIter iter;
gint        minimum_width;
gint        natural_width;

valid = gtk_tree_model_get_iter_first (model, &iter);
while (valid)
  {
    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
    gtk_cell_area_get_preferred_width (area, context, widget, NULL, NULL);

    valid = gtk_tree_model_iter_next (model, &iter);
  @}
gtk_cell_area_context_get_preferred_width (context, &minimum_width,
                                                    &natural_width);
  @end{pre}
  Note that in this example it is not important to observe the returned
  minimum and natural width of the area for each row unless the cell-layouting
  object is actually interested in the widths of individual rows. The overall
  width is however stored in the accompanying @class{gtk:cell-area-context}
  object and can be consulted at any time.

  This can be useful since @class{gtk:cell-layout} widgets usually have to
  support requesting and rendering rows in treemodels with an exceedingly
  large amount of rows. The @class{gtk:cell-layout} widget in that case would
  calculate the required width of the rows in an idle or timeout source, see
  the @fun{g:timeout-add} function, and when the widget is requested its
  actual width in @code{get_preferred_width()} it can simply consult the width
  accumulated so far in the @class{gtk:cell-area-context} object.

  A simple example where rows are rendered from top to bottom and take up the
  full width of the layouting widget would look like:

  @b{Example:} A typical @code{get_preferred_width()} implementation
  @begin{pre}
static void
foo_get_preferred_width (GtkWidget       *widget,
                         gint            *minimum_size,
                         gint            *natural_size)
{
  Foo        *foo  = FOO (widget);
  FooPrivate *priv = foo->priv;

  foo_ensure_at_least_one_handfull_of_rows_have_been_requested (foo);

  gtk_cell_area_context_get_preferred_width (priv->context, minimum_size,
                                                            natural_size);
@}
  @end{pre}
  In the above example the @code{Foo} widget has to make sure that some row
  sizes have been calculated, the amount of rows that @code{Foo} judged was
  appropriate to request space for in a single timeout iteration, before
  simply returning the amount of space required by the area via the
  @class{gtk:cell-area-context} object.

  Requesting the height for width, or width for height, of an area is a similar
  task except in this case the @class{gtk:cell-area-context} object does not
  store the data, actually, it does not know how much space the layouting widget
  plans to allocate it for every row. It is up to the layouting widget to render
  each row of data with the appropriate height and width which was requested by
  the @class{gtk:cell-area} object.

  In order to request the height for width of all the rows at the root level
  of a @class{gtk:tree-model} object one would do the following:

  @b{Example:}
  Requesting the height for width of a handful of @class{gtk:tree-model} rows
  @begin{pre}
GtkTreeIter iter;
gint        minimum_height;
gint        natural_height;
gint        full_minimum_height = 0;
gint        full_natural_height = 0;

valid = gtk_tree_model_get_iter_first (model, &iter);
while (valid)
  {
    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
    gtk_cell_area_get_preferred_height_for_width (area, context, widget,
                                                  width, &minimum_height,
                                                  &natural_height);

    if (width_is_for_allocation)
       cache_row_height (&iter, minimum_height, natural_height);

    full_minimum_height += minimum_height;
    full_natural_height += natural_height;

    valid = gtk_tree_model_iter_next (model, &iter);
  @}
  @end{pre}
  Note that in the above example we would need to cache the heights returned
  for each row so that we would know what sizes to render the areas for each
  row. However we would only want to really cache the heights if the request
  is intended for the layouting widgets real allocation.

  In some cases the layouting widget is requested the height for an arbitrary
  @code{for_width}, this is a special case for layouting widgets who need to
  request size for tens of thousands of rows. For this case it is only
  important that the layouting widget calculate one reasonably sized chunk of
  rows and return that height synchronously. The reasoning here is that any
  layouting widget is at least capable of synchronously calculating enough
  height to fill the screen height, or scrolled window height, in response to
  a single call to the @code{get_preferred_height_for_width()} function.
  Returning a perfect height for width that is larger than the screen area is
  inconsequential since after the layouting receives an allocation from a
  scrolled window it simply continues to drive the the scrollbar values while
  more and more height is required for the row heights that are calculated in
  the background.

  @subheadint{Rendering Areas}
  Once area sizes have been aquired at least for the rows in the visible area
  of the layouting widget they can be rendered at @code{draw()} time.

  A crude example of how to render all the rows at the root level runs as
  follows:

  @b{Example:}
  Requesting the width of a handful of @class{gtk:tree-model} rows
    @begin{pre}
GtkAllocation allocation;
GdkRectangle  cell_area = { 0, @};
GtkTreeIter   iter;
gint          minimum_width;
gint          natural_width;

gtk_widget_get_allocation (widget, &allocation);
cell_area.width = allocation.width;

valid = gtk_tree_model_get_iter_first (model, &iter);
while (valid)
  {
    cell_area.height = get_cached_height_for_row (&iter);

    gtk_cell_area_apply_attributes (area, model, &iter, FALSE, FALSE);
    gtk_cell_area_render (area, context, widget, cr,
                          &cell_area, &cell_area, state_flags, FALSE);

    cell_area.y += cell_area.height;

    valid = gtk_tree_model_iter_next (model, &iter);
  @}
  @end{pre}
  Note that the cached height in this example really depends on how the
  layouting widget works. The layouting widget might decide to give every row
  its minimum or natural height or, if the model content is expected to fit
  inside the layouting widget without scrolling, it would make sense to
  calculate the allocation for each row at \"size-allocate\" time using the
  @code{gtk_distribute_natural_allocation()} function.

  @subheading{Handling Events and Driving Keyboard Focus}
  Passing events to the area is as simple as handling events on any normal
  widget and then passing them to the the @fun{gtk:cell-area-event} function
  API as they come in. Usually the @class{gtk:cell-area} object is only
  interested in button events, however some customized derived areas can be
  implemented who are interested in handling other events. Handling an event
  can trigger the @code{\"focus-changed\"} signal to fire. As well as the
  @code{\"add-editable\"} signal in the case that an editable cell was clicked
  and needs to start editing. You can call the @fun{gtk:cell-area-stop-editing}
  function at any time to cancel any cell editing that is currently in progress.

  The @class{gtk:cell-area} object drives keyboard focus from cell to cell in a
  way similar to @class{gtk:widget} object. For layouting widgets that support
  giving focus to cells it is important to remember to pass the
  @code{GTK_CELL_RENDERER_FOCUSED} value to the area functions for the row that
  has focus and to tell the area to paint the focus at render time.

  Layouting widgets that accept focus on cells should implement the
  @code{focus()} virtual method. The layouting widget is always responsible
  for knowing where @class{gtk:tree-model} rows are rendered inside the
  widget, so at @code{focus()} time the layouting widget should use the
  @class{gtk:cell-area} methods to navigate focus inside the area and then
  observe the @symbol{gtk:direction-type} value to pass the focus to adjacent
  rows and areas.

  A basic example of how the @code{focus()} virtual method should be
  implemented:

  @b{Example:} Implementing keyboard focus navigation
  @begin{pre}
static gboolean
foo_focus (GtkWidget       *widget,
           GtkDirectionType direction)
{
  Foo        *foo  = FOO (widget);
  FooPrivate *priv = foo->priv;
  gint        focus_row;
  gboolean    have_focus = FALSE;

  focus_row = priv->focus_row;

  if (!gtk_widget_has_focus (widget))
    gtk_widget_grab_focus (widget);

  valid = gtk_tree_model_iter_nth_child (priv->model, &iter, NULL,
                                         priv->focus_row);
  while (valid)
    {
      gtk_cell_area_apply_attributes (priv->area, priv->model, &iter,
                                      FALSE, FALSE);

      if (gtk_cell_area_focus (priv->area, direction))
        {
           priv->focus_row = focus_row;
           have_focus = TRUE;
           break;
        @}
      else
        {
          if (direction == GTK_DIR_RIGHT ||
              direction == GTK_DIR_LEFT)
            break;
          else if (direction == GTK_DIR_UP ||
                   direction == GTK_DIR_TAB_BACKWARD)
           {
              if (focus_row == 0)
                break;
              else
               {
                  focus_row--;
                  valid = gtk_tree_model_iter_nth_child (priv->model,
                                                         &iter, NULL,
                                                         focus_row);
               @}
            @}
          else
            {
              if (focus_row == last_row)
                break;
              else
                {
                  focus_row++;
                  valid = gtk_tree_model_iter_next (priv->model, &iter);
                @}
            @}
        @}
    @}
    return have_focus;
@}
  @end{pre}
  Note that the layouting widget is responsible for matching the
  @symbol{gtk:direction-type} values to the way it lays out its cells.

  @subheading{Cell Properties}
  The @class{gtk:cell-area} class introduces cell properties for
  @class{gtk:cell-renderer} objects. This provides some general interfaces for
  defining the relationship cell areas have with their cells. For instance in a
  @class{gtk:cell-area-box} object a cell might \"expand\" and receive extra
  space when the area is allocated more than its full natural request, or a cell
  might be configured to \"align\" with adjacent rows which were requested and
  rendered with the same @class{gtk:cell-area-context} object.

  Use the @code{gtk_cell_area_class_install_cell_property()} function to
  install cell properties for a cell area class and the
  @fun{gtk:cell-area-class-find-cell-property} or
  @fun{gtk:cell-area-class-list-cell-properties} functions to get
  information about existing cell properties.

  To set or get the value of a cell property, use the
  @fun{gtk:cell-area-cell-property}, @fun{gtk:cell-area-cell-get}, and
  @fun{gtk:cell-area-cell-set} functions.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"add-editable\" signal}
      @begin{pre}
lambda (area renderer editable cell-area path)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[area]{The @class{gtk:cell-area} object where editing started.}
        @entry[renderer]{The @class{gtk:cell-renderer} object that started the
          edited.}
        @entry[editable]{The @class{gtk:cell-editable} widget to add.}
        @entry[cell-area]{The @class{gtk:widget} object relative
          @class{gdk:rectangle} coordinates where @arg{editable} should be
          added.}
        @entry[path]{The @class{gtk:tree-path} string this edit was initiated
          for.}
      @end{table}
      Indicates that editing has started on @arg{renderer} and that
      @arg{editable} should be added to the owning cell-layouting widget at
      @arg{cell-area}.
    @subheading{The \"apply-attributes\" signal}
      @begin{pre}
lambda (area model iter is-expander is-expanded)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[area]{The @class{gtk:cell-area} object to apply the attributes
          to.}
        @entry[model]{The @class{gtk:tree-model} object to apply the attributes
          from.}
        @entry[iter]{The @class{gtk:tree-iter} instance indicating which row to
          apply the attributes of.}
        @entry[is-expander]{Whether the view shows children for this row.}
        @entry[is-expanded]{Whether the view is currently showing the children
          of this row.}
      @end{table}
      The signal is emitted whenever applying attributes to the cell area from
      the model.
    @subheading{The \"focus-changed\" signal}
      @begin{pre}
lambda (area renderer path)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[area]{The @class{gtk:cell-area} object where focus changed.}
        @entry[renderer]{The @class{gtk:cell-renderer} object that has focus.}
        @entry[path]{The current @class{gtk:tree-path} string set for area.}
      @end{table}
      Indicates that focus changed on the cell area. The signal is emitted
      either as a result of focus handling or event handling. It is possible
      that the signal is emitted even if the currently focused renderer did not
      change, this is because focus may change to the same renderer in the same
      cell area for a different row of data.
    @subheading{The \"remove-editable\" signal}
      @begin{pre}
lambda (area renderer editable)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[area]{The @class{gtk:cell-area} object where editing finished.}
        @entry[renderer]{The @class{gtk:cell-renderer} object that finished
          editeding.}
        @entry[editable]{The @class{gtk:cell-editable} widget to remove.}
      @end{table}
      Indicates that editing finished on @arg{renderer} and that @arg{editable}
      should be removed from the owning cell-layouting widget.
  @end{dictionary}
  @see-slot{gtk:cell-area-edit-widget}
  @see-slot{gtk:cell-area-edited-cell}
  @see-slot{gtk:cell-area-focus-cell}
  @see-class{gtk:cell-area-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-area-edit-widget ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "edit-widget" 'cell-area) t)
 "The @code{edit-widget} property of type @class{gtk:cell-editable} (Read) @br{}
  The widget currently editing the edited cell. This property is read-only and
  only changes as a result of calling the @fun{gtk:cell-area-activate-cell}
  function.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-edit-widget)
      "Accessor"
      (documentation 'cell-area-edit-widget 'function)
 "@version{2024-7-7}
  @syntax{(gtk:cell-area-edit-widget object) => widget}
  @argument[object]{a @class{gtk:cell-area} object}
  @argument[widget]{a @class{gtk:cell-editable} widget}
  @begin{short}
    Accessor of the @slot[gtk:cell-area]{edit-widget} slot of the
    @class{gtk:cell-area} class.
  @end{short}
  The @fun{gtk:cell-area-edit-widget} function gets the widget currently used
  to edit the currently edited cell.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-editable}")

;;; --- gtk:cell-area-edited-cell ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "edited-cell" 'cell-area) t)
 "The @code{edited-cell} property of type @class{gtk:cell-renderer} (Read) @br{}
  The cell in the area that is currently edited. This property is read-only and
  only changes as a result of calling the @fun{gtk:cell-area-activate-cell}
  function.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-edited-cell)
      "Accessor"
      (documentation 'cell-area-edited-cell 'function)
 "@version{2024-7-7}
  @syntax{(gtk:cell-area-edited-cell object) => renderer}
  @argument[object]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-area]{edited-cell} slot of the
    @class{gtk:cell-area} class.
  @end{short}
  The @fun{gtk:cell-area-edited-cell} function gets the
  @class{gtk:cell-renderer} object in the area that is currently being edited.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-area-focus-cell -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-cell" 'cell-area) t)
 "The @code{focus-cell} property of type @class{gtk:cell-renderer}
  (Read / Write) @br{}
  The cell in the area that currently has focus.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-area-focus-cell)
      "Accessor"
      (documentation 'cell-area-focus-cell 'function)
 "@version{2024-7-7}
  @syntax{(gtk:cell-area-edited-cell object) => renderer}
  @syntax{(setf (gtk:cell-area-edited-cell object) renderer}
  @argument[object]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to give focus to}
  @begin{short}
    Accessor of the @slot[gtk:cell-area]{focus-cell} slot of the
    @class{gtk:cell-area} class.
  @end{short}
  The @fun{gtk:cell-area-focus-cell} function retrieves the currently focused
  cell for the area. The @setf{gtk:cell-area-focus-cell} function explicitly
  sets the currently focused cell to @arg{renderer}.

  This is generally called by implementations of the
  @code{GtkCellAreaClass.focus()} or @code{GtkCellAreaClass.event()} functions,
  however it can also be used to implement functions such as the
  @fun{gtk:tree-view-set-cursor-on-cell} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:tree-view-set-cursor-on-cell}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_add" cell-area-add) :void
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to add to @arg{area}}
  @begin{short}
    Adds a cell renderer to the cell area with the default child cell
    properties.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer)))

(export 'cell-area-add)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_remove" cell-area-remove) :void
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to remove from area}
  @short{Removes a cell renderer from the cell area.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer)))

(export 'cell-area-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_has_renderer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_has_renderer" cell-area-has-renderer) :boolean
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to check}
  @return{@em{True} if @arg{renderer} is in the @arg{area}.}
  @short{Checks if the cell area contains @arg{renderer}.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer)))

(export 'cell-area-has-renderer)

;;; ----------------------------------------------------------------------------
;;; GtkCellCallback
;;; ----------------------------------------------------------------------------

(cffi:defcallback cell-callback :boolean
    ((renderer (g:object cell-renderer))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func renderer)
      (return-from-cell-callback () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-callback)
      "Callback"
      (liber:symbol-documentation 'cell-callback)
 "@version{2024-7-7}
  @syntax{lambda (renderer) => result}
  @argument[renderer]{a @class{gtk:cell-renderer} object to operate on}
  @argument[result]{@em{true} to stop iterating over cells}
  @begin{short}
    The type of the callback function used for iterating over the cell renderers
    of a @class{gtk:cell-area} object, see the @fun{gtk:cell-area-foreach}
    function.
  @end{short}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-foreach}")

(export 'cell-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_foreach" %cell-area-foreach) :void
  (area (g:object cell-area))
  (func :pointer)
  (data :pointer))

(defun cell-area-foreach (area func)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[func]{a @symbol{gtk:cell-callback} callback function to call}
  @short{Calls a callback function for every cell renderer in the cell area.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-symbol{gtk:cell-callback}"
  (glib:with-stable-pointer (ptr func)
    (%cell-area-foreach area
                        (cffi:callback cell-callback)
                        ptr)))

(export 'cell-area-foreach)

;;; ----------------------------------------------------------------------------
;;; GtkCellAllocCallback
;;; ----------------------------------------------------------------------------

(cffi:defcallback cell-alloc-callback :boolean
    ((renderer (g:object cell-renderer))
     (cell (g:boxed gdk:rectangle))
     (background (g:boxed gdk:rectangle))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func renderer cell background)
      (return-from-cell-alloc-callback () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-alloc-callback)
      "Callback"
      (liber:symbol-documentation 'cell-alloc-callback)
 "@version{2024-7-7}
  @syntax{lambda (renderer cell background) => result}
  @argument[renderer]{a @class{gtk:cell-renderer} object to operate on}
  @argument[cell]{a @class{gdk:rectangle} area allocated to @arg{renderer}
    inside the rectangle provided to the @fun{gtk:cell-area-foreach-alloc}
    function}
  @argument[background]{a @class{gdk:rectangle} background area for
    @arg{renderer} inside the background area provided to the
    @fun{gtk:cell-area-foreach-alloc} function}
  @argument[result]{@em{true} to stop iterating over cells}
  @begin{short}
    The type of the callback function used for iterating over the cell
    renderers of a @class{gtk:cell-area} object, see the
    @fun{gtk:cell-area-foreach-alloc} function.
  @end{short}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:rectangle}
  @see-function{gtk:cell-area-foreach-alloc}")

(export 'cell-alloc-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_foreach_alloc
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_foreach_alloc" %cell-area-foreach-alloc) :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (cell (g:boxed gdk:rectangle))
  (backgound (g:boxed gdk:rectangle))
  (func :pointer)
  (data :pointer))

(defun cell-area-foreach-alloc (area context widget cell background func)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering to}
  @argument[cell]{a @class{gdk:rectangle} instance with the relative coordinates
    and size of @arg{widget} for rendering @arg{area}}
  @argument[background]{a @class{gdk:rectangle} instance with the relative
    coordinates of the background for rendering @arg{area}}
  @argument[func]{a @symbol{gtk:cell-alloc-callback} callback function}
  @begin{short}
    Calls the callback function for every @class{gtk:cell-renderer} object in
    the cell area with the allocated rectangle inside @arg{cell}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:cell-alloc-callback}"
  (glib:with-stable-pointer (ptr func)
    (%cell-area-foreach-alloc area
                              context
                              widget
                              cell
                              background
                              (cffi:callback cell-alloc-callback)
                              ptr)))

(export 'cell-area-foreach-alloc)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_event
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_event" cell-area-event) :int
 #+liber-documentation
 "@version{#2024-7-26}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object for this row of
    data}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering to}
  @argument[event]{a @class{gdk:event} instance to handle}
  @argument[cell]{a @class{gdk:rectangle} instance with the widget relative
    coordinates for @arg{area}}
  @argument[flags]{a @symbol{gtk:cell-renderer-state} value for @arg{area} in
    this row}
  @return{@em{True} if the event was handled by the cell area.}
  @begin{short}
    Delegates event handling to a cell area.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-class{gdk:event}
  @see-symbol{gtk:cell-renderer-state}"
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (event gdk:event)
  (cell (g:boxed gdk:rectangle))
  (flags cell-renderer-state))

(export 'cell-area-event)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_snapshot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_snapshot" cell-area-snapshot) :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object for this row of
    data}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering to}
  @argument[snapshot]{a @class{gtk:snapshot} object to draw to}
  @argument[cell]{a @class{gdk:rectangle} instance with the widget relative
    coordinates for @arg{area}}
  @argument[flags]{a @symbol{gtk:cell-renderer-state} value for @arg{area} in
    this row}
  @argument[paint-focus]{a @symbol{gtk:cell-renderer-state} value for
    @arg{area} in this row}
  @begin{short}
    Snapshots @arg{area}'s cells according to @arg{area}'s layout onto at the
    given coordinates.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-class{gtk:snapshot}
  @see-symbol{gtk:cell-renderer-state}"
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (snapshot (g:object snapshot))
  (background (g:boxed gdk:rectangle))
  (cell (g:boxed gdk:rectangle))
  (flags cell-renderer-state)
  (paint-focus :boolean))

(export 'cell-area-snapshot)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_allocation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_cell_allocation" %cell-area-cell-allocation)
    :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (renderer (g:object cell-renderer))
  (cell (g:boxed gdk:rectangle))
  (allocation (g:boxed gdk:rectangle)))

(defun cell-area-cell-allocation (area context widget renderer cell)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object used to hold sizes
    for area}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering on}
  @argument[renderer]{a @class{gtk:cell-renderer} object to get the allocation
    for}
  @argument[cell]{a @class{gdk:rectangle} instance with the whole allocated
    area for @arg{area} in @arg{widget} for this row}
  @return{The @class{gdk:rectangle} instance with the allocation for
    @arg{renderer}.}
  @begin{short}
    Derives the allocation of the cell renderer inside the cell area if
    @arg{cell} were to be renderered in @arg{area}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}"
  (let ((allocation (gdk:rectangle-new)))
    (%cell-area-cell-allocation area
                                context
                                widget
                                renderer
                                cell
                                allocation)
    allocation))

(export 'cell-area-cell-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_cell_at_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_cell_at_position" %cell-area-cell-at-position)
    (g:object cell-renderer)
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (cell (g:boxed gdk:rectangle))
  (x :int)
  (y :int)
  (alloc (g:boxed gdk:rectangle)))

(defun cell-area-cell-at-position (area context widget cell x y)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object used to hold sizes
    for @arg{area}}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering on}
  @argument[cell]{a @class{gdk:rectangle} instance with the whole allocated area
    for @arg{area} in @arg{widget} for this row}
  @argument[x]{an integer with the x position}
  @argument[y]{an integer with the y position}
  @begin{return}
   The @class{gtk:cell-renderer} object at @arg{x} and @arg{y} for the first
   value and the @class{gdk:rectangle} allocation for the inner allocated area
   of the returned cell renderer.
  @end{return}
  @begin{short}
    Gets the @class{gtk:cell-renderer} object at x and y coordinates inside
    @arg{area} and the full cell allocation for it inside @arg{cell}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}"
  (let* ((alloc (gdk:rectangle-new))
         (renderer (%cell-area-cell-at-position area
                                                context
                                                widget
                                                cell
                                                x y
                                                alloc)))
    (values renderer alloc)))

(export 'cell-area-cell-at-position)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_create_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_create_context" cell-area-create-context)
    (g:object cell-area-context :return)
 #+liber-documentation
 "@version{2024-10-9}
  @argument[area]{a @class{gtk:cell-area} object}
  @return{The newly created @class{gtk:cell-area-context} object which can be
    used with @arg{area}.}
  @begin{short}
    Creates a cell area context to be used with @arg{area} for all purposes.
  @end{short}
  The @class{gtk:cell-area-context} object stores geometry information for rows
  for which it was operated on, it is important to use the same context for the
  same row of data at all times, that is, one should render and handle events
  with the same @class{gtk:cell-area-context} object which was used to request
  the size of those rows of data.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}"
  (area (g:object cell-area)))

(export 'cell-area-create-context)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_copy_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_copy_context" cell-area-copy-context)
    (g:object cell-area-context :return)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object to copy}
  @return{The newly created @class{gtk:cell-area-context} object copy of
    @arg{context}.}
  @begin{short}
    This is sometimes needed for cases where rows need to share alignments in
    one orientation but may be separately grouped in the opposing orientation.
  @end{short}

  For instance, the @class{gtk:icon-view} widget creates all icons (rows) to
  have the same width and the cells theirin to have the same horizontal
  alignments. However each row of icons may have a separate collective height.
  The @class{gtk:icon-view} widget uses this to request the heights of each row
  based on a context which was already used to request all the row widths that
  are to be displayed.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}"
  (area (g:object cell-area))
  (context (g:object cell-area-context)))

(export 'cell-area-copy-context)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_request_mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_request_mode" cell-area-request-mode)
    size-request-mode
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @return{The @symbol{gtk:size-request-mode} value preferred by @arg{area}.}
  @begin{short}
    Gets whether the area prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-symbol{gtk:size-request-mode}"
  (area (g:object cell-area)))

(export 'cell-area-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_preferred_width" %cell-area-preferred-width)
    :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (minium (:pointer :int))
  (natural (:pointer :int)))

(defun cell-area-preferred-width (area context widget)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object to perform this
    request with}
  @argument[widget]{a @class{gtk:widget} object where @arg{area} will be
    rendering}
  @begin{return}
    @arg{minimum} -- an integer with the minimum width, or @code{nil} @br{}
    @arg{natural} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural width of the cell area.
  @end{short}
  The @arg{area} argument will store some geometrical information in
  @arg{context} along the way, when requesting sizes over an arbitrary number
  of rows, its not important to check the @arg{minimum} and @arg{natural} of
  this call but rather to consult the
  @fun{gtk:cell-area-context-preferred-width} function after a series of
  requests.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-function{gtk:cell-area-context-preferred-width}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-area-preferred-width area
                                context
                                widget
                                minimum
                                natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-area-preferred-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height_for_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_preferred_height_for_width"
               %cell-area-preferred-height-for-width) :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (width :int)
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-area-preferred-height-for-width (area context widget width)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object which has already
    been requested for widths}
  @argument[widget]{a @class{gtk:widget} object where @arg{area} will be
    rendering}
  @argument[width]{an integer with the width for which to check the height of
    this area}
  @begin{return}
    @arg{minimum} -- an integer with the minimum height, or @code{nil} @br{}
    @arg{natural} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural height of the cell area if it would be given
    the specified width.
  @end{short}
  The @arg{area} argument stores some geometrical information in @arg{context}
  along  the way while calling the @fun{gtk:cell-area-preferred-width} function.
  It is important to perform a series of @fun{gtk:cell-area-preferred-width}
  requests with @arg{context} first and then call the
  @fun{gtk:cell-area-preferred-height-for-width} function on each cell area
  individually to get the height for width of each fully requested row.

  If at some point, the width of a single row changes, it should be requested
  with the @fun{gtk:cell-area-preferred-width} function again and then the full
  width of the requested rows checked again with the
  @fun{gtk:cell-area-context-preferred-width} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtkcell-area-context}
  @see-class{gtk:widget}
  @see-function{gtk:cell-area-preferred-width}
  @see-function{gtk:cell-area-context-preferred-width}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-area-preferred-height-for-width area
                                           context
                                           widget
                                           width
                                           minimum
                                           natural)
       (values (cffi:mem-ref minimum :int)
               (cffi:mem-ref natural :int))))

(export 'cell-area-preferred-height-for-width)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_preferred_height" %cell-area-preferred-height)
    :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (minium (:pointer :int))
  (natural (:pointer :int)))

(defun cell-area-preferred-height (area context widget)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object to perform this
    request with}
  @argument[widget]{a @class{gtk:widget} where area will be rendering}
  @begin{return}
    @arg{minimum} -- an integer with the minimum height, or @code{nil} @br{}
    @arg{natural} -- an integer with the natural height, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves an initial minimum and natural height of the cell area.
  @end{short}
  The @arg{area} argument will store some geometrical information in
  @arg{context} along the way, when requesting sizes over an arbitrary number of
  rows, its not important to check the @arg{minimum} and @arg{natural} of this
  call but rather to consult the @fun{gtk:cell-area-context-preferred-height}
  function after a series of requests.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-function{gtk:cell-area-context-preferred-height}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-area-preferred-height area
                                 context
                                 widget
                                 minimum
                                 natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-area-preferred-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_preferred_width_for_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_preferred_width_for_height"
          %cell-area-preferred-width-for-height) :void
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (height :int)
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-area-preferred-width-for-height (area context widget height)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object which has already
    been requested for widths}
  @argument[widget]{a @class{gtk:widget} object where @arg{area} will be
    rendering}
  @argument[height]{an integer with the height for which to check the width of
    this area}
  @begin{return}
    @arg{minimum} -- an integer with the minimum width, or @code{nil} @br{}
    @arg{natural} -- an integer with the natural width, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves a minimum and natural width of the cell area if it would be given
    the specified height.
  @end{short}
  The @arg{area} argument stores some geometrical information in @arg{context}
  along the way while calling the @fun{gtk:cell-area-preferred-height} function.
  It is important to perform a series of the
  @fun{gtk:cell-area-preferred-height} function requests with @arg{context}
  first and then call the @fun{gtk:cell-area-preferred-width-for-height}
  function on each cell area individually to get the height for width of each
  fully requested row.

  If at some point, the height of a single row changes, it should be requested
  with the @fun{gtk:cell-area-preferred-height} function again and then the full
  height of the requested rows checked again with the
  @fun{gtk:cell-area-context-preferred-height} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-arrea}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-function{gtk:cell-area-preferred-height}
  @see-function{gtk:cell-area-context-preferred-height}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-area-preferred-width-for-height area
                                           context
                                           widget
                                           height
                                           minimum
                                           natural)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int))))

(export 'cell-area-preferred-width-for-height)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_current_path_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_current_path_string"
               cell-area-current-path-string) :string
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @return{The current @class{gtk:tree-path} string for the current attributes
    applied to @arg{area}.}
  @begin{short}
    Gets the current @class{gtk:tree-path} string for the currently applied
    @class{gtk:tree-iter} iterator.
  @end{short}
  This is implicitly updated when the @fun{gtk:cell-area-apply-attributes}
  function is called and can be used to interact with renderers from
  @class{gtk:cell-area} subclasses.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-path}
  @see-function{gtk:cell-area-apply-attributes}"
  (area (g:object cell-area)))

(export 'cell-area-current-path-string)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_apply_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_apply_attributes" cell-area-apply-attributes)
    :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[model]{a @class{gtk:tree-model} object to pull values from}
  @argument[iter]{a @class{gtk:tree-iter} iterator in @arg{model} to apply
  values for}
  @argument[is-expander]{a boolean whether @arg{iter} has children}
  @argument[is-expanded]{a boolean whether @arg{iter} is expanded in the view
  and children are visible}
  @begin{short}
    Applies any connected attributes to the renderers in the cell area by
    pulling the values from the tree model.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}"
  (area (g:object cell-area))
  (model (g:object tree-model))
  (iter (g:boxed tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'cell-area-apply-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_connect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_attribute_connect" cell-area-attribute-connect)
    :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to connect an attribute
    for}
  @argument[attribute]{a string with the attribute name}
  @argument[column]{an integer for the @class{gtk:tree-model} object column to
    fetch attribute values from}
  @begin{short}
    Connects an attribute to apply values from @arg{column} for the tree model
    in use.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:tree-model}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (attribute :string)
  (column :int))

(export 'cell-area-attribute-connect)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_disconnect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_attribute_disconnect"
               cell-area-attribute-disconnect) :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to disconnect an
    attribute for}
  @argument[attribute]{a string with the attribute name}
  @begin{short}
    Disconnects @arg{attribute} for the renderer in the cell area so that
    @arg{attribute} will no longer be updated with values from the model.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (attribute :string))

(export 'cell-area-attribute-disconnect)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_attribute_get_column
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_attribute_get_column"
               cell-area-attribute-column) :int
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @argument[attribute]{a string with an attribute on the renderer}
  @return{The integer with the model column, or -1.}
  @begin{short}
    Returns the model column that an attribute has been mapped to, or -1 if the
    attribute is not mapped.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (attribute :string))

(export 'cell-area-attribute-column)

;;; ----------------------------------------------------------------------------
;; gtk_cell_area_class_install_cell_property ()
;;;
;;; void
;;; gtk_cell_area_class_install_cell_property
;;;                                (GtkCellAreaClass *aclass,
;;;                                 guint property_id,
;;;                                 GParamSpec *pspec);
;;;
;;; Installs a cell property on a cell area class.
;;;
;;; aclass :
;;;     a GtkCellAreaClass
;;;
;;; property_id :
;;;     the id for the property
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_find_cell_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_class_find_cell_property"
               %cell-area-class-find-cell-property)
    (:pointer (:struct gobject:param-spec))
  (class (:pointer (:struct gobject:type-class)))
  (property :string))

(defun cell-area-class-find-cell-property (gtype property)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[property]{a string with the name of the cell property to find}
  @begin{return}
    The @symbol{g:param-spec} instance of the cell property or a
    @code{null-pointer} if the @arg{gtype} type has no child property with that
    name.
  @end{return}
  @begin{short}
    Finds a cell property of a cell area type by name.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-symbol{g:type-t}
  @see-symbol{g:param-spec}"
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (let ((pspec (%cell-area-class-find-cell-property class property)))
        (unless (cffi:null-pointer-p pspec) pspec))
      (gobject:type-class-unref class))))

(export 'cell-area-class-find-cell-property)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_class_list_cell_properties
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_class_list_cell_properties"
               %cell-area-class-list-cell-properties)
    (:pointer (:pointer (:struct gobject:param-spec)))
  (class (:pointer (:struct gobject::object-class)))
  (n-props (:pointer :uint)))

(defun cell-area-class-list-cell-properties (gtype)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The list of @symbol{g:param-spec} instances.}
  @short{Returns the cell properties of a cell area class.}
  @begin[Notes]{dictionary}
    In the Lisp binding we pass the type of a cell area class and not
    a pointer to the cell area class as argument to the function.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{g:type-t}
  @see-class{g:param-spec}"
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (cffi:with-foreign-object (n-props :uint)
        (let ((pspecs (%cell-area-class-list-cell-properties class n-props)))
          (unwind-protect
            (iter (for count from 0 below (cffi:mem-ref n-props :uint))
                  (for pspec = (cffi:mem-aref pspecs :pointer count))
                  (collect pspec))
            (glib:free pspecs))))
      (g:type-class-unref class))))

(export 'cell-area-class-list-cell-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_with_properties
;;; ----------------------------------------------------------------------------

(defun cell-area-add-with-properties (area renderer &rest args)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to be placed inside
    @arg{area}}
  @argument[args]{property names and values}
  @begin{short}
    Adds renderer to @arg{area}, setting cell properties at the same time.
  @end{short}
  See the @fun{gtk:cell-area-add} and @fun{gtk:cell-area-cell-set} functions
  for more details.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-add}
  @see-function{gtk:cell-area-cell-set}"
  (cell-area-add area renderer)
  (apply #'cell-area-cell-set area renderer args))

(export 'cell-area-add-with-properties)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set
;;; ----------------------------------------------------------------------------

(defun cell-area-cell-set (area renderer &rest args)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object which is cell inside
    @arg{area}}
  @argument[args]{cell property names and values}
  @begin{short}
    Sets one or more cell properties for the cell in the cell area.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-cell-get}
  @see-function{gtk:cell-area-cell-property}"
  (iter (for (name value) on args by #'cddr)
        (setf (cell-area-cell-property area renderer name) value)))

(export 'cell-area-cell-set)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get
;;; ----------------------------------------------------------------------------

(defun cell-area-cell-get (area renderer &rest args)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object which is inside
    @arg{area}}
  @argument[args]{strings with the cell property names to get the values for}
  @return{The list with the values of the cell properties.}
  @begin{short}
    Gets the values of one or more cell properties for the cell renderer in
    the cell area.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-cell-set}
  @see-function{gtk:cell-area-cell-property}"
  (iter (for arg in args)
        (collect (cell-area-cell-property area renderer arg))))

(export 'cell-area-cell-get)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set_valist ()                        not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_get_valist ()                        not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_cell_set_property
;;; gtk_cell_area_cell_get_property
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_cell_set_property" %cell-area-cell-set-property)
    :void
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (property :string)
  (value (:pointer (:struct g:value))))

(defun (setf cell-area-cell-property) (value area renderer property)
  (let ((gtype (gobject:param-spec-value-type
                   (cell-area-class-find-cell-property
                       (gobject:type-from-instance area) property))))
    (cffi:with-foreign-object (new-value '(:struct g:value))
      (gobject:set-g-value new-value value gtype :zero-gvalue t)
      (%cell-area-cell-set-property area renderer property new-value)
      (gobject:value-unset new-value)
      (values value))))

(cffi:defcfun ("gtk_cell_area_cell_get_property" %cell-area-cell-get-property)
    :void
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (property :string)
  (value (:pointer (:struct g:value))))

(defun cell-area-cell-property (area renderer property)
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:cell-area-property area renderer property) => value}
  @syntax{(setf (gtk:cell-area-property area renderer property) value)}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object which is inside
    @arg{area}}
  @argument[property]{a string with the name of the cell property}
  @argument[value]{a value for the property}
  @begin{short}
    Gets or sets the value of a cell property for the cell renderer inside
    the cell area.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (let ((gtype (gobject:param-spec-value-type
                   (cell-area-class-find-cell-property
                       (gobject:type-from-instance area)
                       property))))
    (cffi:with-foreign-object (value '(:struct g:value))
      (g:value-init value gtype)
      (%cell-area-cell-get-property area renderer property value)
      (prog1
        (gobject:parse-g-value value)
        (g:value-unset value)))))

(export 'cell-area-cell-property)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_activatable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_is_activatable" cell-area-is-activatable) :boolean
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @return{The boolean whether @arg{area} can do anything when activated.}
  @begin{short}
    Returns whether the cell area can do anything when activated, after applying
     new attributes to @arg{area}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}"
  (area (g:object cell-area)))

(export 'cell-area-is-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_activate" cell-area-activate) :boolean
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[context]{a @class{gtk:cell-area-context} object in @arg{context}
    with the current row data}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering on}
  @argument[cell]{a @class{gdk:rectangle} instance with the size and location of
    @arg{area} relative to allocation of the widget}
  @argument[flags]{a @symbol{gtk:cell-renderer-state} value for @arg{area} for
    this row of data}
  @argument[edit-only]{if @em{true} then only cell renderers that are
    @code{:editable} will be activated}
  @return{The boolean whether @arg{area} was successfully activated.}
  @begin{short}
    Activates @arg{area}, usually by activating the currently focused cell,
    however some subclasses which embed widgets in the area can also activate
    a widget if it currently has the focus.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-context}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:cell-renderer-state}"
  (area (g:object cell-area))
  (context (g:object cell-area-context))
  (widget (g:object widget))
  (cell (g:boxed gdk:rectangle))
  (flags cell-renderer-state)
  (edit-only :boolean))

(export 'cell-area-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_focus" cell-area-focus) :boolean
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[direction]{a value of the @symbol{gtk:direction-type} enumeration}
  @return{@em{True} if focus remains inside @arg{area} as a result of this
    call.}
  @begin{short}
    This should be called by the owning layout widget of the cell area when
    focus is to be passed to @arg{area}, or moved within @arg{area} for a given
    direction and row data.
  @end{short}

  Implementing @class{gtk:cell-area} classes should implement this method to
  receive and navigate focus in its own way particular to how it lays out cells.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-symbol{gtk:direction-type}"
  (area (g:object cell-area))
  (direction direction-type))

(export 'cell-area-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_add_focus_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_add_focus_sibling" cell-area-add-focus-sibling)
    :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk:cell-renderer} object to add to the focus area
    of the cell renderer}
  @begin{short}
    Adds @arg{sibling} to focusable area of the cell renderer, focus will be
    drawn around @arg{renderer} and all of its siblings if @arg{renderer} can
    focus for a given row.
  @end{short}

  Events handled by focus siblings can also activate the given focusable
  @arg{renderer}.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (sibling (g:object cell-renderer)))

(export 'cell-area-add-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_remove_focus_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_remove_focus_sibling"
               cell-area-remove-focus-sibling) :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk:cell-renderer} object to remove from the focus
  area of the cell renderer}
  @begin{short}
    Removes @arg{sibling} from the focus sibling list of the cell renderer.
  @end{short}
  See the @fun{gtk:cell-area-add-focus-sibling} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-add-focus-sibling}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (sibling (g:object cell-renderer)))

(export 'cell-area-remove-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_is_focus_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_is_focus_sibling" cell-area-is-focus-sibling)
    :boolean
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object expected to have focus}
  @argument[sibling]{a @class{gtk:cell-renderer} object to check against
    the sibling list of the cell renderer}
  @return{@em{True} if @arg{sibling} is a focus sibling of @arg{renderer}.}
  @begin{short}
    Returns whether @arg{sibling} is one of the focus siblings of the cell
    renderer.
  @end{short}
  See the @fun{gtk:cell-area-add-focus-sibling} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-area-add-focus-sibling}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (sibling (g:object cell-renderer)))

(export 'cell-area-is-focus-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_siblings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_focus_siblings" cell-area-focus-siblings)
    (g:list-t (g:object cell-renderer) :free-from-foreign nil)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object expected to have focus}
  @return{The list of @class{gtk:cell-renderer} objects.}
  @begin{short}
    Gets the focus sibling cell renderers for @arg{renderer}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer)))

(export 'cell-area-focus-siblings)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_get_focus_from_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_get_focus_from_sibling"
               cell-area-focus-from-sibling) (g:object cell-renderer)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object}
  @return{The @class{gtk:cell-renderer} object for which @arg{renderer} is a
    sibling, or @code{nil}.}
  @begin{short}
    Gets the cell renderer which is expected to be focusable for which
    @arg{renderer} is, or may be a sibling.
  @end{short}

  This is handy for @class{gtk:cell-area} subclasses when handling events, after
  determining the cell renderer at the event location it can then chose to
  activate the focus cell for which the event cell may have been a sibling.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}"
  (area (g:object cell-area))
  (renderer (g:object cell-renderer)))

(export 'cell-area-focus-from-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_activate_cell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_activate_cell" cell-area-activate-cell) :boolean
 #+liber-documentation
 "@version{#2024-7-26}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering
    onto}
  @argument[renderer]{a @class{gtk:cell-renderer} object in @arg{area} to
    activate}
  @argument[event]{a @class{gdk:event} instance for which cell activation
    should occur}
  @argument[cell]{a @class{gdk:rectangle} instance in @arg{widget} relative
    coordinates of @arg{renderer} for the current row}
  @argument[flags]{a value of the @symbol{gtk:cell-renderer-state} flags for
    @arg{renderer}}
  @return{The boolean whether cell activation was successful.}
  @begin{short}
    This is used by @class{gtk:cell-area} subclasses when handling events to
    activate cells, the base @class{gtk:cell-area} class activates cells for
    keyboard events for free in its own @code{GtkCellArea->activate()}
    implementation.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:widget}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:event}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:cell-renderer-state}"
  (area (g:object cell-area))
  (widget (g:object widget))
  (renderer (g:object cell-renderer))
  (event gdk:event)
  (cell (g:boxed gdk:rectangle))
  (flags cell-renderer-state))

(export 'cell-area-activate-cell)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_stop_editing
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_stop_editing" cell-area-stop-editing) :void
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[canceled]{a boolean whether editing was canceled}
  @begin{short}
    Explicitly stops the editing of the currently edited cell.
  @end{short}
  If the @arg{canceled} argument is @em{true}, the currently edited cell
  renderer will emit the @code{\"editing-canceled\"} signal, otherwise the
  @code{\"editing-done\"} signal will be emitted on the current edit widget.

  See the @fun{gtk:cell-area-edited-cell} and @fun{gtk:cell-area-edit-widget}
  functions.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-function{gtk:cell-area-edited-cell}
  @see-function{gtk:cell-area-edit-widget}"
  (area (g:object cell-area))
  (canceled :boolean))

(export 'cell-area-stop-editing)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_inner_cell_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_inner_cell_area" %cell-area-inner-cell-area) :void
  (area (g:object cell-area))
  (widget (g:object widget))
  (cell (g:boxed gdk:rectangle))
  (inner (g:boxed gdk:rectangle)))

(defun cell-area-inner-cell-area (area widget cell)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering
    onto}
  @argument[cell]{a @class{gdk:rectangle} instance with the widget relative
    coordinates where one of the cells of the cell area is to be placed}
  @return{The @class{gdk:rectangle} instance with the inner cell area.}
  @begin{short}
    This is a convenience function for @class{gtk:cell-area} implementations to
    get the inner area where a given @class{gtk:cell-renderer} object will be
    rendered.
  @end{short}
  It removes any padding previously added by the
  @fun{gtk:cell-area-request-renderer} function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:widget}
  @see-class{gtk:cell-renderer}
  @see-class{gdk:rectangle}
  @see-function{gtk:cell-area-request-renderer}"
  (let ((inner (gdk:rectangle-new)))
    (%cell-area-inner-cell-area area widget cell inner)
    inner))

(export 'cell-area-inner-cell-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_area_request_renderer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_area_request_renderer" %cell-area-request-renderer)
    :void
  (area (g:object cell-area))
  (renderer (g:object cell-renderer))
  (orientation orientation)
  (widget (g:object widget))
  (for-size :int)
  (minimum (:pointer :int))
  (natural (:pointer :int)))

(defun cell-area-request-renderer (area
                                   renderer
                                   orientation
                                   widget
                                   for-size)
 #+liber-documentation
 "@version{#2024-7-7}
  @argument[area]{a @class{gtk:cell-area} object}
  @argument[renderer]{a @class{gtk:cell-renderer} object to request size for}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration in
    which to request size}
  @argument[widget]{a @class{gtk:widget} object that @arg{area} is rendering
    onto}
  @argument[for-size]{an integer with the allocation contextual size to request
    for, or -1 if the base request for the orientation is to be returned}
  @begin{return}
    @arg{minimum} -- an integer with the minimum size, or @code{nil} @br{}
    @arg{natural} -- an integer with the natural size, or @code{nil}
  @end{return}
  @begin{short}
    This is a convenience function for @class{gtk:cell-area} implementations to
    request size for cell renderers.
  @end{short}
  It is important to use this function to request size and then use the
  @fun{gtk:cell-area-inner-cell-area} function at render and event time since
  this function will add padding around the cell for focus painting.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-area} implementation is deprecated since 4.10. List
    views use widgets for displaying their contents.
  @end{dictionary}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:widget}
  @see-symbol{gtk:orientation}
  @see-function{gtk:cell-area-inner-cell-area}"
  (cffi:with-foreign-objects ((minimum :int) (natural :int))
    (%cell-area-request-renderer area
                                 renderer
                                 orientation
                                 widget
                                 for-size
                                 minimum
                                 natural)
      (values (cffi:mem-ref minimum :int)
              (cffi:mem-ref natural :int))))

(export 'cell-area-request-renderer)

;;; --- End of file gtk4.cell-area.lisp ----------------------------------------
