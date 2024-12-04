;;; ----------------------------------------------------------------------------
;;; gtk4.grid-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; GtkGridView
;;;
;;;     A widget for displaying grids
;;;
;;; Types and Values
;;;
;;;     GtkGridView
;;;
;;; Accessors
;;;
;;;     gtk_grid_view_get_enable_rubberband
;;;     gtk_grid_view_set_enable_rubberband
;;;     gtk_grid_view_get_factory
;;;     gtk_grid_view_set_factory
;;;     gtk_grid_view_get_max_columns
;;;     gtk_grid_view_set_max_columns
;;;     gtk_grid_view_get_min_columns
;;;     gtk_grid_view_set_min_columns
;;;     gtk_grid_view_get_model
;;;     gtk_grid_view_set_model
;;;     gtk_grid_view_get_single_click_activate
;;;     gtk_grid_view_set_single_click_activate
;;;     gtk_grid_view_get_tab_behavior                     Since 4.12
;;;     gtk_grid_view_set_tab_behavior                     Since 4.12
;;;
;;; Functions
;;;
;;;     gtk_grid_view_new
;;;     gtk_grid_view_scroll_to                            Since 4.12
;;;
;;; Properties
;;;
;;;     enable-rubberband
;;;     factory
;;;     max-columns
;;;     min-columns
;;;     model
;;;     single-click-activate
;;;     tab-behavior                                       Since 4.12
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Actions
;;;
;;;     list.activate-item
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkListBase
;;;                 ╰── GtkGridView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGridView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGridView" grid-view
  (:superclass list-base
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_grid_view_get_type")
  ((enable-rubberband
    grid-view-enable-rubberband
    "enable-rubberband" "gboolean" t t)
   (factory
    grid-view-factory
    "factory" "GtkListItemFactory" t t)
   (max-columns
    grid-view-max-columns
    "max-columns" "guint" t t)
   (min-columns
    grid-view-min-columns
    "min-columns" "guint" t t)
   (model
    grid-view-model
    "model" "GtkSelectionModel"  t t)
   (single-click-activate
    grid-view-single-click-activate
    "single-click-activate" "gboolean" t t)
   #+gtk-4-12
   (tab-behavior
    grid-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))

#+liber-documentation
(setf (documentation 'grid-view 'type)
 "@version{2024-11-28}
  @begin{short}
    The @class{gtk:grid-view} widget is a widget to present a view into a large
    dynamic grid of items.
  @end{short}
  The @class{gtk:grid-view} widget uses its factory to generate one child
  widget for each visible item and shows them in a grid. The orientation of the
  grid view determines if the grid reflows vertically or horizontally.

  The @class{gtk:grid-view} widget allows the user to select items according to
  the selection characteristics of the model. For models that allow multiple
  selected items, it is possible to turn on rubberband selection, using the
  @slot[gtk:grid-view]{enable-rubberband} property.

  To learn more about the list widget framework, see the list widget overview.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
gridview
├── child
│
├── child
│
┊
╰── [rubberband]
    @end{pre}
    The @class{gtk:grid-view} implementation uses a single CSS node with name
    @code{gridview}. Each child uses a single CSS node with name @code{child}.
    If the @slot[gtk:list-item]{activatable} property is set, the corresponding
    row will have the @code{.activatable} style class. For rubberband selection,
    a subnode with name @code{rubberband} is used.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:grid-view} implementation uses the @code{:grid} role, and
    the items use the @code{:grid-cell} role from the
    @class{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (gridview position)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gridview]{The @class{gtk:grid-view} widget.}
        @entry[position]{An unsigned integer with the position of the item to
          activate.}
      @end{table}
      The signal is emitted when a cell has been activated by the user. This
      allows for a convenient way to handle activation in a grid view. See the
      @slot[gtk:list-item]{activatable} property for details on how to use this
      signal.
  @end{dictionary}
  @see-constructor{gtk:grid-view-new}
  @see-slot{gtk:grid-view-enable-rubberband}
  @see-slot{gtk:grid-view-factory}
  @see-slot{gtk:grid-view-max-columns}
  @see-slot{gtk:grid-view-min-columns}
  @see-slot{gtk:grid-view-model}
  @see-slot{gtk:grid-view-single-click-activate}
  @see-slot{gtk:grid-view-tab-behavior}
  @see-class{gtk:selection-model}
  @see-class{gtk:list-view}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:grid-view-enable-rubberband ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-rubberband"
                                               'grid-view) t)
 "The @code{enable-rubberband} property of type @code{:boolean} (Read / Write)
  @br{}
  Allow rubberband selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-enable-rubberband)
      "Accessor"
      (documentation 'grid-view-enable-rubberband 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-enable-rubberband object) => setting}
  @syntax{(setf (gtk:grid-view-enable-rubberband object) setting)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[setting]{@em{true} if rubberband selection is enabled}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{enable-rubberband} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-enable-rubberband} function returns whether selections
  can be selected by dragging with the mouse. The
  @setf{gtk:grid-view-enable-rubberband} function sets whether selections can be
  changed by dragging with the mouse.
  @see-class{gtk:grid-view}")

;;; --- gtk:grid-view-factory --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'grid-view) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  Factory for populating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-factory)
      "Accessor"
      (documentation 'grid-view-factory 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-factory object) => factory}
  @syntax{(setf (gtk:grid-view-factory object) factory)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use, or
    @code{nil} for none}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{factory} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-factory} function gets the factory that is currently
  used to populate list items. The @setf{gtk:grid-view-factory} function sets
  the factory.
  @see-class{gtk:grid-view}
  @see-class{gtk:selection-model}")

;;; --- gtk:grid-view-max-columns ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-columns" 'grid-view) t)
 "The @code{max-columns} property of type @code{:uint} (Read / Write) @br{}
  If this number is smaller than the @slot[gtk:grid-view]{min-columns} property,
  that value is used instead. @br{}
  Allowed values: >= 1 @br{}
  Default value: 7")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-max-columns)
      "Accessor"
      (documentation 'grid-view-max-columns 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-max-colums object) => max}
  @syntax{(setf (gtk:grid-view-max-columns object) max)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[max]{an unsigned integer with the maximum of columns}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{max-columns} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-max-columns} function gets the maximum number of
  columns that the grid will use. The @setf{gtk:grid-view-max-columns} function
  sets the maximum number of columns to use. This number must be at least 1. If
  the @slot[gtk:grid-view]{max-columns} property is smaller than the minimum set
  via the @fun{gtk:grid-view-min-columns} function, that value is used instead.
  @see-class{gtk:grid-view}
  @see-function{gtk:grid-view-min-columns}")

;;; --- gtk:grid-view-min-columns ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-columns" 'grid-view) t)
 "The @code{min-columns} property of type @code{:uint} (Read / Write) @br{}
  Minimum number of columns per row. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-min-columns)
      "Accessor"
      (documentation 'grid-view-min-columns 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-max-colums object) => min}
  @syntax{(setf (gtk:grid-view-min-columns object) min)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[min]{an unsigned integer with the minimum of columns}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{min-columns} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-min-columns} function gets the minimum number of
  columns that the grid will use. The @setf{gtk:grid-view-min-columns} function
  sets the minimum number of columns to use. This number must be at least 1. If
  the @slot[gtk:grid-view]{min-columns} property is smaller than the minimum set
  via the @fun{gtk:grid-view-max-columns} function, that value is ignored.
  @see-class{gtk:grid-view}
  @see-function{gtk:grid-view-max-columns}")

;;; --- gtk:grid-view-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'grid-view) t)
 "The @code{model} property of type @class{gtk:selection-model} (Read / Write)
  @br{}
  Model for the items displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-model)
      "Accessor"
      (documentation 'grid-view-model 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-model object) => model}
  @syntax{(setf (gtk:grid-view-model object) model)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[model]{a @class{gtk:selection-model} object to use}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{model} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-model} function gets the model that is currently used
  to read the items displayed. The @setf{gtk:grid-view-model} function sets the
  model to use. This must be a @class{gtk:selection-model} object.
  @see-class{gtk:grid-view}
  @see-class{gtk:selection-model}")

;;; --- gtk:grid-view-single-click-activate ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-click-activate"
                                               'grid-view) t)
 "The @code{single-click-activate} property of type @code{:boolean}
  (Read / Write) @br{}
  Activate rows on single click and select them on hover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'grid-view-single-click-activate)
      "Accessor"
      (documentation 'grid-view-single-click-activate 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-single-click-activate object) => setting}
  @syntax{(setf (gtk:grid-view-single-click-activate object) setting)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[setting]{@em{true} if rows are activated on single click}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{single-click-activate} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-single-click-activate} function returns whether rows
  will be activated on single click and selected on hover. The
  @setf{gtk:grid-view-single-click-activate} function sets whether rows should
  be activated on single click and selected on hover.
  @see-class{gtk:grid-view}")

;;; --- gtk:grid-view-tab-behavior ---------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "tab-behavior" 'grid-view) t)
 "The @code{tab-beavior} property of type @symbol{gtk:list-tab-behavior}
  (Read / Write) @br{}
  Behavior of the @kbd{Tab} key. @br{}
  Default value: @code{:all}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'grid-view-tab-behavior)
      "Accessor"
      (documentation 'grid-view-tab-behavior 'function)
 "@version{2024-11-28}
  @syntax{(gtk:grid-view-tab-behavior object) => setting}
  @syntax{(setf (gtk:grid-view-tab-behavior object) setting)}
  @argument[object]{a @class{gtk:grid-view} object}
  @argument[setting]{a @symbol{gtk:list-tab-behavior} value}
  @begin{short}
    Accessor of the @slot[gtk:grid-view]{tab-behavior} slot of the
    @class{gtk:grid-view} class.
  @end{short}
  The @fun{gtk:grid-view-tab-behavior} function gets the behavior of the
  @key{Tab} and @code{Shift+Tab} keys. The @setf{gtk:grid-view-tab-behavior}
  function sets the tab behavior.
  @see-class{gtk:grid-view}
  @see-symbol{gtk:list-tab-behavior}")

;;; ----------------------------------------------------------------------------
;;; gtk_grid_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline grid-view-new))

(defun grid-view-new (&optional model factory)
 #+liber-documentation
 "@version{2024-11-28}
  @argument[model]{an optional @class{gtk:selection-model} object to use, or
    the @code{nil} default value}
  @argument[factory]{an optional @class{gtk:list-item-factory} object to
    populate items with or the default @code{nil} value}
  @return{The new @class{gtk:grid-view} widget using the given @arg{model} and
    @arg{factory}.}
  @begin{short}
    Creates a new grid view that uses the given @arg{factory} for mapping items
    to widgets.
  @end{short}
  @see-class{gtk:grid-view}
  @see-class{gtk:selection-model}
  @see-class{gtk:list-item-factory}"
  (make-instance 'grid-view
                 :model model
                 :factory factory))

(export 'grid-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_grid_view_scroll_to
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_list_view_scroll_to" grid-view-scroll-to) :void
 #+liber-documentation
 "@version{#2024-11-28}
  @argument[gridview]{a @class{gtk:grid-view} widget}
  @argument[pos]{an unsigned integer with the position of the item}
  @argument[flags]{a @symbol{gtk:list-scroll-flags} value with the actions to
    perform}
  @argument[scroll]{a @class{gtk:scroll-info} instance with the details of how
    to perform the scroll operation or @code{nil} to scroll into the grid view}
  @begin{short}
    Scrolls to the item at the given position and performs the actions specified
    in @arg{flags}.
  @end{short}
  This function works no matter if the grid view is shown or focused. If it is
  not, then the changes will take effect once that happens.

  Since 4.12
  @see-class{gtk:grid-view}
  @see-class{gtk:scroll-info}
  @see-symbol{gtk:list-scroll-flags}"
  (gridview (g:object grid-view))
  (pos :uint)
  (flags list-scroll-flags)
  (scroll (g:boxed scroll-info)))

#+gtk-4-12
(export 'grid-view-scroll-to)

;;; --- End of file gtk4.grid-view.lisp ----------------------------------------
