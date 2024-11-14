;;; ----------------------------------------------------------------------------
;;; gtk4.drop-down.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; GtkDropDown
;;;
;;;     Choose an item from a list
;;;
;;; Types and Values
;;;
;;;     GtkDropDown
;;;
;;; Accessors
;;;
;;;     gtk_drop_down_get_enable_search
;;;     gtk_drop_down_set_enable_search
;;;     gtk_drop_down_get_expression
;;;     gtk_drop_down_set_expression
;;;     gtk_drop_down_get_factory
;;;     gtk_drop_down_set_factory
;;;     gtk_drop_down_get_header_factory                   Since 4.12
;;;     gtk_drop_down_set_header_factory                   Since 4.12
;;;     gtk_drop_down_get_list_factory
;;;     gtk_drop_down_set_list_factory
;;;     gtk_drop_down_get_model
;;;     gtk_drop_down_set_model
;;;     gtk_drop_down_get_search_match_mode                Since 4.12
;;;     gtk_drop_down_set_search_match_mode                Since 4.12
;;;     gtk_drop_down_get_selected
;;;     gtk_drop_down_set_selected
;;;     gtk_drop_down_get_selected_item
;;;     gtk_drop_down_get_show_arrow                       Since 4.6
;;;     gtk_drop_down_set_show_arrow                       Since 4.6
;;;
;;; Functions
;;;
;;;     gtk_drop_down_new
;;;     gtk_drop_down_new_from_strings
;;;
;;; Properties
;;;
;;;     enable-search
;;;     expression
;;;     factory
;;;     header-factory                                     Since 4.12
;;;     list-factory
;;;     model
;;;     search-match-mode                                  Since 4.12
;;;     selected
;;;     selected-item
;;;     show-arrow                                         Since 4.6
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.6
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDropDown
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropDown
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDropDown" drop-down
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_drop_down_get_type")
  ((enable-search
    drop-down-enable-search
    "enable-search" "gboolean" t t)
   (expression
    drop-down-expression
    "expression" "GtkExpression" t t)
   (factory
    drop-down-factory
    "factory" "GtkListItemFactory" t t)
   #+gtk-4-12
   (header-factory
    drop-down-header-factory
    "header-factory" "GtkListItemFactory" t t)
   (list-factory
    drop-down-list-factory
    "list-factory" "GtkListItemFactory" t t)
   (model
    drop-down-model
    "model" "GListModel" t t)
   #+gtk-4-12
   (search-match-mode
    drop-down-search-match-mode
    "search-match-mode" "GtkStringFilterMatchMode" t t)
   (selected
    drop-down-selected
    "selected" "guint" t t)
   (selected-item
    drop-down-selected-item
    "selected-item" "GObject" t nil)
   #+gtk-4-6
   (show-arrow
    drop-down-show-arrow
    "show-arrow" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'drop-down 'type)
 "@version{2024-1-10}
  @begin{short}
    The @class{gtk:drop-down} widget is a widget that allows the user to choose
    an item from a list of options.
  @end{short}
  The @class{gtk:drop-down} widget displays the selected choice.

  The options are given to the @class{gtk:drop-down} widget in the form of a
  @class{g:list-model} object, and how the individual options are represented
  is determined by a @class{gtk:list-item-factory} object. The default factory
  displays simple strings.

  The @class{gtk:drop-down} widget knows how to obtain strings from the items
  in a @class{gtk:string-list} object. For other models, you have to provide an
  expression to find the strings via the @fun{gtk:drop-down-expression}
  function.

  The @class{gtk:drop-down} widget can optionally allow search in the popup,
  which is useful if the list of options is long. To enable the search entry,
  use the @fun{gtk:drop-down-enable-search} function.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:drop-down} implementation has a single CSS node with name
    @code{dropdown}, with the button and popover nodes as children.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:drop-down} implementation uses the @code{:combo-box} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signals]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (dropdown)    :action
      @end{pre}
      The signal is an action signal and emitting it causes the drop down to
      pop up its dropdown. The default handler is called before the handlers.
      Since 4.6
      @begin[code]{table}
        @entry[dropdown]{The @class{gtk:drop-down} widget.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:drop-down-new}
  @see-constructor{gtk:drop-down-new-from-strings}
  @see-slot{gtk:drop-down-enable-search}
  @see-slot{gtk:drop-down-expression}
  @see-slot{gtk:drop-down-factory}
  @see-slot{gtk:drop-down-header-factory}
  @see-slot{gtk:drop-down-list-factory}
  @see-slot{gtk:drop-down-model}
  @see-slot{gtk:drop-down-search-match-mode}
  @see-slot{gtk:drop-down-selected}
  @see-slot{gtk:drop-down-selected-item}
  @see-slot{gtk:drop-down-show-arrow}
  @see-class{gtk:combo-box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:drop-down-enable-search --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-search" 'drop-down) t)
 "The @code{enable-search} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show a search entry in the popup. Note that search requires
  the @slot[gtk:drop-down]{expression} property to be set. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-enable-search)
      "Accessor"
      (documentation 'drop-down-enable-search 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-enable-search object) => setting}
  @syntax{(setf (gtk:drop-down-enable-search object) setting)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[setting]{a boolean whether to enable search}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{enable-search} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-enable-search} function returns whether search is
  enabled. The @setf{drop-down-enable-search} function sets whether a search
  entry will be shown in the popup that allows to search for items in the list.
  Note that the @slot[gtk:drop-down]{expression} property must be set for search
  to work.
  @see-class{gtk:drop-down}
  @see-function{gtk:drop-down-expression}")

;;; --- gtk:drop-down-expression -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'drop-down) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  An expression to evaluate to obtain strings to match against the search term
  See the @slot[gtk:drop-down]{enable-search} property. If the
  @slot[gtk:drop-down]{factory} property is not set, the expression is also
  used to bind strings to labels produced by a default factory.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-expression)
      "Accessor"
      (documentation 'drop-down-expression 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-expression object) => expression}
  @syntax{(setf (gtk:drop-down-expression object) expression)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[expression]{a @class{gtk:expression} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{expression} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-expression} function gets the expression. The
  @setf{drop-down-expression} function sets the expression that gets evaluated
  to obtain strings from items when searching in the popup. The expression must
  have a value type of \"gchararray\".
  @see-class{gtk:drop-down}
  @see-class{gtk:expression}")

;;; --- gtk:drop-down-factory --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'drop-down) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  Factory for populating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-factory)
      "Accessor"
      (documentation 'drop-down-factory 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-factory object) => factory}
  @syntax{(setf (gtk:drop-down-factory object) factory)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[factory]{a @class{gtk:list-item-factory} object, or @code{nil}
    for none}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{factory} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-factory} function gets the factory that is currently
  used to populate list items. The @setf{drop-down-factory} function sets the
  factory to use for populating list items. The factory is always used for the
  item in the button. It is also used for items in the popup if
  the @slot[gtk:drop-down]{list-factory} property is not set.
  @see-class{gtk:drop-down}
  @see-class{gtk:list-item-factory}")

;;; --- gtk:drop-down-header-factory -------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "header-factory" 'drop-down) t)
 "The @code{header-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for creating header widgets for the popup. Since 4.12")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-header-factory)
      "Accessor"
      (documentation 'drop-down-header-factory 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-header-factory object) => factory}
  @syntax{(setf (gtk:drop-down-header-factory object) factory)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[factory]{a @class{gtk:list-item-factory} object, or @code{nil}
    for none}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{header-factory} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-header-factory} function sets the factory that is
  currently used to create header widgets for the popup. The
  @setf{drop-down-header-factory} function sets the factory.

  Since 4.12
  @see-class{gtk:drop-down}
  @see-class{gtk:list-item-factory}")

;;; --- gtk:drop-down-list-factory ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "list-factory" 'drop-down) t)
 "The @code{list-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for populating list items in the popup. If this is not set, the
  @slot[gtk:drop-down]{factory} property is used.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-list-factory)
      "Accessor"
      (documentation 'drop-down-list-factory 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-list-factory object) => factory}
  @syntax{(setf (gtk:drop-down-list-factory object) factory)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[factory]{a @class{gtk:list-item-factory} object, or @code{nil}
    for none}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{list-factory} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-list-factory} function gets the factory that is
  currently used to populate list items in the popup. The
  @setf{drop-down-list-factory} function sets the factory to use for populating
  list items in the popup.
  @see-class{gtk:drop-down}
  @see-class{gtk:list-item-factory}")

;;; --- gtk:drop-down-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'drop-down) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  Model for the displayed items.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-model)
      "Accessor"
      (documentation 'drop-down-model 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-model object) => model}
  @syntax{(setf (gtk:drop-down-model object) model)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[model]{a @class{g:list-model} object to use, or @code{nil} for none}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{model} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-model} function gets the model that provides the
  displayed items. The @setf{drop-down-model} function sets the model to use.
  @see-class{gtk:drop-down}
  @see-class{g:list-model}")

;;; --- gtk:drop-down-search-match-mode ----------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "search-match-mode"
                                               'drop-down) t)
 "The @code{search-match-mode} property of type
  @symbol{gtk:string-filter-match-mode} (Read / Write) @br{}
  The match mode for the search filter. Since 4.12")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-search-match-mode)
      "Accessor"
      (documentation 'drop-down-search-match-mode 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-search-match-mode object) => mode}
  @syntax{(setf (gtk:drop-down-search-match-mode object) mode)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[mode]{a @symbol{gtk:string-filter-match-mode} value with the
    match mode}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{search-match-mode} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-search-match-mode} function returns the match mode
  that the search filter is using. The @setf{drop-down-search-match-mode}
  function sets the match mode for the search filter.

  Since 4.12
  @see-class{gtk:drop-down}
  @see-symbol{gtk:string-filter-match-mode}")

;;; --- gtk:drop-down-selected -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'drop-down) t)
 "The @code{selected} property of type @code{:uint} (Read / Write) @br{}
  The position of the selected item in @slot[gtk:drop-down]{model}, or
  @var{gtk:+invalid-list-position+} if no item is selected. @br{}
  Default value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-selected)
      "Accessor"
      (documentation 'drop-down-selected 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-selected object) => selected}
  @syntax{(setf (gtk:drop-down-selected object) selected)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[selected]{an unsigned integer with the position of the item to
    select, or the @var{gtk:+invalid-list-position+} value if no position is
    selected}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{selected} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-selected} function gets the position of the selected
  item. The @setf{drop-down-selected} function selects the item at the given
  position.
  @see-class{gtk:drop-down}")

;;; --- gtk:drop-down-selected-item --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected-item" 'drop-down) t)
 "The @code{selected-item} property of type @class{g:object} (Read) @br{}
  The selecte item.")

#+liber-documentation
(setf (liber:alias-for-function 'drop-down-selected-item)
      "Accessor"
      (documentation 'drop-down-selected-item 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-selected-item object) => item}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[item]{A pointer to the selected item.}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{selected-item} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-selected-item} function gets the selected item. If no
  item is selected, @code{cffi:null-pointer} is returned.
  @see-class{gtk:drop-down}")

;;; --- gtk:drop-down-show-arrow -----------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "show-arrow" 'drop-down) t)
 "The @code{show-arrow} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show an arrow within the GtkDropDown widget. Since 4.6 @br{}
  Default value: @em{true}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'drop-down-show-arrow)
      "Accessor"
      (documentation 'drop-down-show-arrow 'function)
 "@version{2024-1-10}
  @syntax{(gtk:drop-down-show-arrow object) => setting}
  @syntax{(setf (gtk:drop-down-show-arrow object) setting)}
  @argument[object]{a @class{gtk:drop-down} widget}
  @argument[setting]{a boolean whether to show an arrow within the widget}
  @begin{short}
    Accessor of the @slot[gtk:drop-down]{show-arrow} slot of the
    @class{gtk:drop-down} class.
  @end{short}
  The @fun{gtk:drop-down-show-arrow} function returns whether to show an arrow
  within the widget. The @setf{drop-down-show-arrow} function sets whether an
  arrow will be displayed.

  Since 4.6
  @see-class{gtk:drop-down}")

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_down_new" %drop-down-new) (g:object widget)
  (model (g:object g:list-model))
  (expression expression))

(defun drop-down-new (model expression)
 #+liber-documentation
 "@version{2024-8-16}
  @argument[model]{a @class{g:list-model} object to use, or @code{nil} for none}
  @argument[expression]{a @class{gtk:expression} instance to use, or @code{nil}
    for none}
  @return{The new @class{gtk:drop-down} widget.}
  @begin{short}
    Creates a new @class{gtk:drop-down} widget.
  @end{short}
  You may want to call the @setf{gtk:drop-down-factory} function to set up a way
  to map its items to widgets.
  @see-class{gtk:drop-down}
  @see-function{gtk:drop-down-factory}"
  (let ((expression (or expression (cffi:null-pointer))))
    (%drop-down-new model expression)))

(export 'drop-down-new)

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new_from_strings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drop_down_new_from_strings" drop-down-new-from-strings)
    (g:object widget)
 #+liber-documentation
 "@version{2024-1-10}
  @argument[strings]{a list of strings to put in the dropdown}
  @return{The new @class{gtk:drop-down} widget.}
  @begin{short}
    Creates a new dropdown widget that is populated with the strings in
    @arg{strings}.
  @end{short}
  @see-class{gtk:drop-down}"
  (strings g:strv-t))

(export 'drop-down-new-from-strings)

;;; --- End of file gtk4.drop-down.lisp ----------------------------------------
