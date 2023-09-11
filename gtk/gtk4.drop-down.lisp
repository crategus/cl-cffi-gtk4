;;; ----------------------------------------------------------------------------
;;; gtk4.drop-down.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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

(gobject:define-g-object-class "GtkDropDown" drop-down
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
 "@version{#2023-9-10}
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

;;; --- drop-down-enable-search ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-search" 'drop-down) t)
 "The @code{enable-search} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show a search entry in the popup. Note that search requires
  the @slot[gtk:drop-down]{expression} property to be set. @br{}
  Default value: @em{false}")


;;; --- drop-down-expression ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expression" 'drop-down) t)
 "The @code{expression} property of type @class{gtk:expression} (Read / Write)
  @br{}
  An expression to evaluate to obtain strings to match against the search term
  See the @slot[gkt:drop-down]{enable-search} property. If the
  @slot[gtk:drop-down]{factory} property is not set, the expression is also
  used to bind strings to labels produced by a default factory.")


;;; --- drop-down-factory ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'drop-down) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  Factory for populating list items.")


;;; --- drop-down-header-factory -----------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "header-factory" 'drop-down) t)
 "The @code{header-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for creating header widgets for the popup. Since 4.12")


;;; --- drop-down-list-factory -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "list-factory" 'drop-down) t)
 "The @code{list-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for populating list items in the popup. If this is not set, the
  @slot[gtk:drop-down]{factory} property is used.")


;;; --- drop-down-model --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'drop-down) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  Model for the displayed items.")


;;; --- drop-down-search-match-mode --------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "search-match-mode"
                                               'drop-down) t)
 "The @code{search-match-mode} property of type
  @symbol{gtk:string-filter-match-mode} (Read / Write) @br{}
  The match mode for the search filter. Since 4.12")


;;; --- drop-down-selected -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'drop-down) t)
 "The @code{selected} property of type @code{:uint} (Read / Write) @br{}
  The position of the selected item in @slot[gtk:drop-down]{model}, or
  @variable{gtk:+gtk-invalid-list-position+} if no item is selected. @br{}
  Default value: 4294967295")


;;; --- drop-down-selected-item ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected-item" 'drop-down) t)
 "The @code{selected-item} property of type @class{g:object} (Read) @br{}
  The selecte item.")


;;; --- drop-down-show-arrow ---------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "show-arrow" 'drop-down) t)
 "The @code{show-arrow} property of type @code{:boolean} (Read / Write) @br{}
  Whether to show an arrow within the GtkDropDown widget. Since 4.6 @br{}
  Default value: @em{true}")


;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new ()
;;;
;;; GtkWidget *
;;; gtk_drop_down_new (GListModel *model,
;;;                    GtkExpression *expression);
;;;
;;; Creates a new GtkDropDown.
;;;
;;; You may want to call gtk_drop_down_set_factory() to set up a way to map its
;;; items to widgets.
;;;
;;; model :
;;;     the model to use or NULL for none.
;;;
;;; expression :
;;;     the expression to use or NULL for none.
;;;
;;; Returns :
;;;     a new GtkDropDown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new_from_strings ()
;;;
;;; GtkWidget *
;;; gtk_drop_down_new_from_strings (const char * const *strings);
;;;
;;; Creates a new GtkDropDown that is populated with the strings in strings .
;;;
;;; strings :
;;;     The strings to put in the dropdown.
;;;
;;; Returns :
;;;     a new GtkDropDown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_model ()
;;;
;;; void
;;; gtk_drop_down_set_model (GtkDropDown *self,
;;;                          GListModel *model);
;;;
;;; Sets the GListModel to use.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; model :
;;;     the model to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_model ()
;;;
;;; GListModel *
;;; gtk_drop_down_get_model (GtkDropDown *self);
;;;
;;; Gets the model that provides the displayed items.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The model in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_selected ()
;;;
;;; void
;;; gtk_drop_down_set_selected (GtkDropDown *self,
;;;                             guint position);
;;;
;;; Selects the item at the given position.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; position :
;;;     the position of the item to select, or GTK_INVALID_LIST_POSITION
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_selected ()
;;;
;;; guint
;;; gtk_drop_down_get_selected (GtkDropDown *self);
;;;
;;; Gets the position of the selected item.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     the position of the selected item, or GTK_INVALID_LIST_POSITION if not
;;;     item is selected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_selected_item ()
;;;
;;; gpointer
;;; gtk_drop_down_get_selected_item (GtkDropDown *self);
;;;
;;; Gets the selected item. If no item is selected, NULL is returned.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The selected item.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_factory ()
;;;
;;; void
;;; gtk_drop_down_set_factory (GtkDropDown *self,
;;;                            GtkListItemFactory *factory);
;;;
;;; Sets the GtkListItemFactory to use for populating list items.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; factory :
;;;     the factory to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_factory ()
;;;
;;; GtkListItemFactory *
;;; gtk_drop_down_get_factory (GtkDropDown *self);
;;;
;;; Gets the factory that's currently used to populate list items.
;;;
;;; The factory returned by this function is always used for the item in the
;;; button. It is also used for items in the popup if “list-factory” is not set.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The factory in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_list_factory ()
;;;
;;; void
;;; gtk_drop_down_set_list_factory (GtkDropDown *self,
;;;                                 GtkListItemFactory *factory);
;;;
;;; Sets the GtkListItemFactory to use for populating list items in the popup.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; factory :
;;;     the factory to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_list_factory ()
;;;
;;; GtkListItemFactory *
;;; gtk_drop_down_get_list_factory (GtkDropDown *self);
;;;
;;; Gets the factory that's currently used to populate list items in the popup.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The factory in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_expression ()
;;;
;;; void
;;; gtk_drop_down_set_expression (GtkDropDown *self,
;;;                               GtkExpression *expression);
;;;
;;; Sets the expression that gets evaluated to obtain strings from items when
;;; searching in the popup. The expression must have a value type of
;;; G_TYPE_STRING.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; expression :
;;;     a GtkExpression, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_expression ()
;;;
;;; GtkExpression *
;;; gtk_drop_down_get_expression (GtkDropDown *self);
;;;
;;; Gets the expression set with gtk_drop_down_set_expression().
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     a GtkExpression or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_enable_search ()
;;;
;;; void
;;; gtk_drop_down_set_enable_search (GtkDropDown *self,
;;;                                  gboolean enable_search);
;;;
;;; Sets whether a search entry will be shown in the popup that allows to search
;;; for items in the list.
;;;
;;; Note that “expression” must be set for search to work.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; enable_search :
;;;     whether to enable search
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_enable_search ()
;;;
;;; gboolean
;;; gtk_drop_down_get_enable_search (GtkDropDown *self);
;;;
;;; Returns whether search is enabled.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     TRUE if the popup includes a search entry
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.drop-down.lisp ----------------------------------------
