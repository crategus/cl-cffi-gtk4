;;; ----------------------------------------------------------------------------
;;; gtk4.cell-layout.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkCellLayout
;;;
;;;     An interface for packing cells
;;;
;;; Types and Values
;;;
;;;     GtkCellLayout
;;;
;;; Functions
;;;
;;;     GtkCellLayoutDataFunc
;;;
;;;     gtk_cell_layout_pack_start
;;;     gtk_cell_layout_pack_end
;;;     gtk_cell_layout_get_area
;;;     gtk_cell_layout_get_cells
;;;     gtk_cell_layout_reorder
;;;     gtk_cell_layout_clear
;;;     gtk_cell_layout_set_attributes
;;;     gtk_cell_layout_add_attribute
;;;     gtk_cell_layout_set_cell_data_func
;;;     gtk_cell_layout_clear_attributes
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkCellLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellLayout
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkCellLayout" cell-layout
  (:export t
   :type-initializer "gtk_cell_layout_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj cell-layout) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:CELL-LAYOUT is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'cell-layout)
      "Interface"
      (documentation 'cell-layout 'type)
 "@version{2024-7-8}
  @begin{short}
    The @class{gtk:cell-layout} interface is an interface to be implemented by
    all objects which want to provide a @class{gtk:tree-view-column} object
    like API for packing cells, setting attributes and data functions.
  @end{short}

  One of the notable features provided by implementations of the
  @class{gtk:cell-layout} interface are attributes. Attributes let you set the
  properties in flexible ways. They can just be set to constant values like
  regular properties. But they can also be mapped to a column of the underlying
  tree model with the @fun{gtk:cell-layout-add-attribute} function, which means
  that the value of the attribute can change from cell to cell as they are
  rendered by the cell renderer. Finally, it is possible to specify a function
  with the @fun{gtk:cell-layout-set-cell-data-func} function that is called to
  determine the value of the attribute for each cell that is rendered.
  @begin[GtkCellLayouts as GtkBuildable]{dictionary}
    Implementations of the @class{gtk:cell-layout} interface which also
    implement the @class{gtk:buildable} interface accept
    @class{gtk:cell-renderer} objects as @code{<child>} elements in UI
    definitions. They support a custom @code{<attributes>} element for their
    children, which can contain multiple @code{<attribute>} elements. Each
    @code{<attribute>} element has a name attribute which specifies a property
    of the cell renderer. The content of the element is the attribute value.

    @b{Example:} A UI definition fragment specifying attributes
    @begin{pre}
<object class=\"GtkCellView\">
  <child>
    <object class=\"GtkCellRendererText\"/>
    <attributes>
      <attribute name=\"text\">0</attribute>
    </attributes>
  </child>
</object>
    @end{pre}
    Furthermore for implementations of the @class{gtk:cell-layout} interface
    that use a @class{gtk:cell-area} object to lay out cells, all
    @class{gtk:cell-layout} objects in GTK use a @class{gtk:cell-area} object,
    cell properties can also be defined in the format by specifying the custom
    @code{<cell-packing>} attribute which can contain multiple @code{<property>}
    elements defined in the normal way.

    @b{Example:} A UI definition fragment specifying cell properties
    @begin{pre}
<object class=\"GtkTreeViewColumn\">
  <child>
    <object class=\"GtkCellRendererText\"/>
    <cell-packing>
      <property name=\"align\">True</property>
      <property name=\"expand\">False</property>
    </cell-packing>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. List
    views use widgets to display their contents. See the
    @class{gtk:layout-manager} implementation for layout manager delegate
    objects.
  @end{dictionary}
  @see-class{gtk:cell-area}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_pack_start" %cell-layout-pack-start) :void
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer))
  (expand :boolean))

(defun cell-layout-pack-start (layout cell &key (expand t))
 #+liber-documentation
 "@version{#2024-5-1}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space
    allocated to @arg{layout}}
  @begin{short}
    Packs @arg{cell} into the beginning of @arg{layout}.
  @end{short}
  If the @arg{expand} keyword argument is @em{false}, then @arg{cell} is
  allocated no more space than it needs. Any unused space is divided evenly
  between cells for which @arg{expand} is @em{true}. The default value of the
  @arg{expand} keyword argument is @em{true}. Note that reusing the same cell
  renderer is not supported.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}"
  (%cell-layout-pack-start layout cell expand))

(export 'cell-layout-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_pack_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_pack_end" %cell-layout-pack-end) :void
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer))
  (expand :boolean))

(defun cell-layout-pack-end (layout cell &key (expand t))
 #+liber-documentation
 "@version{#2023-9-16}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object}
  @argument[expand]{@em{true} if @arg{cell} is to be given extra space
    allocated to @arg{layout}}
  @begin{short}
    Adds @arg{cell} to the end of @arg{layout}.
  @end{short}
  If the @arg{expand} keyword argument is @em{false}, then @arg{cell} is
  allocated no more space than it needs. Any unused space is divided evenly
  between cells for which @arg{expand} is @em{true}. The default value of the
  @arg{expand} keyword argument is @em{true}. Note that reusing the same cell
  renderer is not supported.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}"
  (%cell-layout-pack-end layout cell expand))

(export 'cell-layout-pack-end)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_get_area" cell-layout-area)
    (g:object cell-area)
 "@version{#2023-9-16}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @return{The @class{gtk:cell-area} object used by @arg{layout}.}
  @begin{short}
    Returns the underlying cell area which might be @arg{layout} if called
    on a @class{gtk:cell-area} object or might be @code{nil} if no cell area is
    used by @arg{layout}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-area}"
  (layout (g:object cell-layout)))

(export 'cell-layout-area)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_get_cells
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_get_cells" cell-layout-cells)
    (g:list-t g:object)
 #+liber-documentation
 "@version{#2024-5-1}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @return{The list of @class{gtk:cell-renderer} objects.}
  @begin{short}
    Returns the cell renderers which have been added to the cell layout.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}"
  (layout (g:object cell-layout)))

(export 'cell-layout-cells)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_reorder
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_reorder" cell-layout-reorder) :void
 #+liber-documentation
 "@version{#2023-9-16}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object to reorder}
  @argument[position]{an integer with the new position to insert @arg{cell} at}
  @begin{short}
    Reinserts @arg{cell} at the given position.
  @end{short}
  Note that @arg{cell} has already to be packed into @arg{layout} for this
  to function properly.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}"
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer))
  (position :int))

(export 'cell-layout-reorder)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_clear" cell-layout-clear) :void
 #+liber-documentation
 "@version{#2024-5-1}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @begin{short}
    Unsets all the mappings on all renderers on @arg{layout} and removes
    all renderers from @arg{layout}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}"
  (layout (g:object cell-layout)))

(export 'cell-layout-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_attributes
;;; ----------------------------------------------------------------------------

(defun cell-layout-set-attributes (layout cell &rest attributes)
 #+liber-documentation
 "@version{#2024-5-1}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object}
  @argument[attributes]{pairs of attributes}
  @begin{short}
    Sets the attributes of @arg{layout}.
  @end{short}
  The attributes should be in attribute/column order, as in the
  @fun{gtk:cell-layout-add-attribute} function. All existing attributes are
  removed, and replaced with the new attributes.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:layout}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-layout-add-attribute}"
  (iter (for (attribute column) on attributes by #'cddr)
        (cell-layout-add-attribute layout cell attribute column)))

(export 'cell-layout-set-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_add_attribute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_add_attribute" cell-layout-add-attribute) :void
 #+liber-documentation
 "@version{#2024-7-8}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object}
  @argument[attribute]{a string with an attribute on the renderer}
  @argument[column]{an integer with the column position on the model to get the
    attribute from}
  @begin{short}
    Adds an attribute mapping to the list in @arg{layout}.
  @end{short}
  The @arg{column} argument is the column of the model to get a value from, and
  @arg{attribute} is the parameter on @arg{cell} to be set from the value. So
  for example if column 2 of the model contains strings, you could have the
  @slot[gtk:cell-renderer-text]{text} attribute of a
  @class{gtk:cell-renderer-text} object get its values from column 2.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:cell-renderer-text}"
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer))
  (attribute :string)
  (column :int))

(export 'cell-layout-add-attribute)

;;; ----------------------------------------------------------------------------
;;; GtkCellLayoutDataFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback cell-layout-data-func :void
  ((layout (g:object cell-layout))
   (cell (g:object cell-renderer))
   (model (g:object tree-model))
   (iter (g:boxed tree-iter))
   (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func layout cell model iter)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-layout-data-func)
      "Callback"
      (liber:symbol-documentation 'cell-layout-data-func)
 "@version{#2024-5-3}
  @syntax{lambda (layout cell model iter)}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object whose value is to be set}
  @argument[model]{a @class{gtk:tree-model} object}
  @argument[iter]{a @class{gtk:tree-iter} iterator indicating the row to set
    the value for}
  @begin{short}
    A callback function which should set the value of @arg{layout}'s cell
   renderer(s) as appropriate.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:tree-model}
  @see-class{gtk:tree-iter}
  @see-function{gtk:cell-layout-set-cell-data-func}")

(export 'cell-layout-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_set_cell_data_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_set_cell_data_func"
               %cell-layout-set-cell-data-func) :void
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun cell-layout-set-cell-data-func (layout cell func)
 #+liber-documentation
 "@version{#2024-5-1}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object}
  @argument[func]{a @symbol{gtk:cell-layout-data-func} callback function to use,
    or @code{nil}}
  @begin{short}
    Sets the callback function to use for @arg{layout}.
  @end{short}
  This function is used instead of the standard attributes mapping for setting
  the column value, and should set the value of @arg{layout}'s cell renderer(s)
  as appropriate. The @arg{func} callback function may be @code{nil} to remove
  a previously set function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}
  @see-symbol{gtk:cell-layout-data-func}"
  (if func
      (%cell-layout-set-cell-data-func
              layout
              cell
              (cffi:callback cell-layout-data-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%cell-layout-set-cell-data-func
              layout
              cell
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'cell-layout-set-cell-data-func)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_layout_clear_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_layout_clear_attributes" cell-layout-clear-attributes)
     :void
 #+liber-documentation
 "@version{#2023-9-16}
  @argument[layout]{a @class{gtk:cell-layout} object}
  @argument[cell]{a @class{gtk:cell-renderer} object to clear the attribute
    mapping on}
  @begin{short}
    Clears all existing attributes previously set with the
    @fun{gtk:cell-layout-add-attribute} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-layout} implementation is deprecated since 4.10. Please
    do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-layout}
  @see-class{gtk:cell-renderer}
  @see-function{gtk:cell-layout-add-attribute}"
  (layout (g:object cell-layout))
  (cell (g:object cell-renderer)))

(export 'cell-layout-clear-attributes)

;;; --- End of file gtk4.cell-layout.lisp --------------------------------------
