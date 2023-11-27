;;; ----------------------------------------------------------------------------
;;; gtk4.combo-box.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkComboBox
;;;
;;;     A widget used to choose from a list of items
;;;
;;; Types and Values
;;;
;;;     GtkComboBox
;;;     GtkSensitivityType
;;;
;;; Accessors
;;;
;;;     gtk_combo_box_get_active
;;;     gtk_combo_box_set_active
;;;     gtk_combo_box_get_active_id
;;;     gtk_combo_box_set_active_id
;;;     gtk_combo_box_set_button_sensitivity
;;;     gtk_combo_box_get_button_sensitivity
;;;     gtk_combo_box_set_child
;;;     gtk_combo_box_get_child
;;;     gtk_combo_box_set_entry_text_column
;;;     gtk_combo_box_get_entry_text_column
;;;     gtk_combo_box_get_has_entry
;;;     gtk_combo_box_get_id_column
;;;     gtk_combo_box_set_id_column
;;;     gtk_combo_box_get_model
;;;     gtk_combo_box_set_model
;;;     gtk_combo_box_set_popup_fixed_width
;;;     gtk_combo_box_get_popup_fixed_width
;;;
;;; Functions
;;;
;;;     gtk_combo_box_new
;;;     gtk_combo_box_new_with_entry
;;;     gtk_combo_box_new_with_model
;;;     gtk_combo_box_new_with_model_and_entry
;;;     gtk_combo_box_get_active_iter
;;;     gtk_combo_box_set_active_iter
;;;     gtk_combo_box_popup
;;;     gtk_combo_box_popup_for_device
;;;     gtk_combo_box_popdown
;;;     gtk_combo_box_get_row_separator_func
;;;     gtk_combo_box_set_row_separator_func
;;;
;;; Properties
;;;
;;;     active
;;;     active-id
;;;     button-sensitivity
;;;     child
;;;     entry-text-column
;;;     has-entry
;;;     has-frame
;;;     id-column
;;;     model
;;;     popup-fixed-width
;;;     popup-shown
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.6
;;;     changed
;;;     format-entry-text
;;;     move-active
;;;     popdown
;;;     popup
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkComboBox
;;;                 ╰── GtkComboBoxText
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkCellLayout
;;;     GtkCellEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBox
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkComboBox" combo-box
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkCellLayout"
                "GtkCellEditable")
   :type-initializer "gtk_combo_box_get_type")
  ((active
    combo-box-active
    "active" "gint" t t)
   (active-id
    combo-box-active-id
    "active-id" "gchararray" t t)
   (button-sensitivity
    combo-box-button-sensitivity
    "button-sensitivity" "GtkSensitivityType" t t)
   (child
    combo-box-child
    "child" "GtkWidget" t t)
   (entry-text-column
    combo-box-entry-text-column
    "entry-text-column" "gint" t t)
   (has-entry
    combo-box-has-entry
    "has-entry" "gboolean" t nil)
   (has-frame
    combo-box-has-frame
    "has-frame" "gboolean" t t)
   (id-column
    combo-box-id-column
    "id-column" "gint" t t)
   (model
    combo-box-model
    "model" "GtkTreeModel" t t)
   (popup-fixed-width
    combo-box-popup-fixed-width
    "popup-fixed-width" "gboolean" t t)
   (popup-shown
    combo-box-popup-shown
    "popup-shown" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'combo-box 'type)
 "@version{2023-9-1}
  @begin{short}
    A @class{gtk:combo-box} widget allows the user to choose from a list of
    valid choices.
  @end{short}
  The @class{gtk:combo-box} widget displays the selected choice. When activated,
  the @class{gtk:combo-box} widget displays a popup which allows the user to
  make a new choice. The style in which the selected value is displayed, and
  the style of the popup is determined by the current theme. It may be similar
  to a Windows style combo box.

  The @class{gtk:combo-box} widget uses the model-view pattern. The list of
  valid choices is specified in the form of a tree model, and the display of
  the choices can be adapted to the data in the model by using cell renderers,
  as you would in a tree view. This is possible since the @class{gtk:combo-box}
  class implements the @class{gtk:cell-layout} interface. The tree model
  holding the valid choices is not restricted to a flat list, it can be a real
  tree, and the popup will reflect the tree structure.

  To allow the user to enter values not in the model, the @code{has-entry}
  property allows the @class{gtk:combo-box} widget to contain a
  @class{gtk:entry} widget. This entry can be accessed by calling the
  @fun{gtk:combo-box-child} function on the combo box.

  For a simple list of textual choices, the model-view API of the
  @class{gtk:combo-box} widget can be a bit overwhelming. In this case, the
  @class{gtk:combo-box-text} widget offers a simple alternative. Both the
  @class{gtk:combo-box} widget and the @class{gtk:combo-box-text} widget can
  contain an entry.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 CSS nodes
 combobox
 ├── box.linked
 │   ╰── button.combo
 │       ╰── box
 │           ├── cellview
 │           ╰── arrow
 ╰── window.popup
    @end{pre}
    A normal @code{combobox} contains a @code{box} with the @code{.linked}
    class, a @code{button} with the @code{.combo} class and inside those
    buttons, there are a @code{cellview} and an @code{arrow}.
    @begin{pre}
 combobox
 ├── box.linked
 │   ├── entry.combo
 │   ╰── button.combo
 │       ╰── box
 │           ╰── arrow
 ╰── window.popup
    @end{pre}
    A @class{gtk:combo-box} widget with an entry has a single CSS node with
    name @code{combobox}. It contains a @code{box} with the @code{.linked}
    class. That box contains an @code{entry} and a @code{button}, both with the
    @code{.combo} class added. The button also contains another node with name
    @code{arrow}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:combo-box} implementation uses the @code{:combo-box} role of
    the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (combo)    :action
      @end{pre}
      Emitted to when the combo box is activated. The signal is an action signal
      and emitting it causes the combo box to pop up its dropdown. Since 4.6
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (combo)    :run-last
      @end{pre}
      The signal is emitted when the active item is changed. The can be due to
      the user selecting a different item from the list, or due to a call to
      the function @fun{gtk:combo-box-active-iter}. It will also be emitted
      while typing into the entry of a combo box with an entry.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
    @subheading{The \"format-entry-text\" signal}
      @begin{pre}
lambda (combo pathstr)    :run-last
      @end{pre}
      A signal which allows you to change how the text displayed in a combo
      box's entry is displayed. Connect a signal handler which returns an
      allocated string representing @arg{path}. That string will then be used
      to set the text in the combo box's entry. The default signal handler uses
      the text from the @code{entry-text-column} property model column. Here is
      an example signal handler which fetches data from the model and displays
      it in the entry. For combo boxes that are created with an entry. See the
      @code{has-entry} property.
      @begin{pre}
(defun format-entry-text-callback (combo pathstr)
  (let* ((model (gtk:combo-box-model combo))
         (iter (gtk:tree-model-iter-from-string model pathstr))
         (value (gtk:tree-model-value model iter col-value)))
    (format nil \"~a\" value)))
      @end{pre}
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
        @entry[pathstr]{A string representing the @class{gtk:tree-path}
          instance from the combo box's current model to format text for.}
        @entry[Returns]{A string representing the value at @argp{pathstr} for
          the current @class{gtk:combo-box} model.}
      @end{table}
    @subheading{The \"move-active\" signal}
      @begin{pre}
lambda (combo scroll)    :action
      @end{pre}
      A keybinding signal which gets emitted to move the active selection.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
        @entry[scroll]{A value of the @symbol{gtk:scroll-type} enumeration.}
      @end{table}
    @subheading{The \"popdown\" signal}
      @begin{pre}
lambda (combo)    :action
      @end{pre}
      A keybinding signal which gets emitted to popdown the combo box list. The
      default bindings for this signal are the @kbd{Alt+Up} and @kbd{Escape}
      keys.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
    @subheading{The \"popup\" signal}
      @begin{pre}
lambda (combo)    :action
      @end{pre}
      A keybinding signal which gets emitted to popup the combo box list. The
      default binding for this signal is the @kbd{Alt+Down} key.
      @begin[code]{table}
        @entry[combo]{The @class{gtk:combo-box} widget that received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:combo-box-active}
  @see-slot{gtk:combo-box-active-id}
  @see-slot{gtk:combo-box-button-sensitivity}
  @see-slot{gtk:combo-box-child}
  @see-slot{gtk:combo-box-entry-text-column}
  @see-slot{gtk:combo-box-has-entry}
  @see-slot{gtk:combo-box-has-frame}
  @see-slot{gtk:combo-box-id-column}
  @see-slot{gtk:combo-box-model}
  @see-slot{gtk:combo-box-popup-fixed-width}
  @see-slot{gtk:combo-box-popup-shown}
  @see-constructor{gtk:combo-box-new}
  @see-constructor{gtk:combo-box-new-with-entry}
  @see-constructor{gtk:combo-box-new-with-model}
  @see-constructor{gtk:combo-box-new-with-model-and-entry}
  @see-class{gtk:tree-model}
  @see-class{gtk:combo-box-text}
  @see-class{gtk:cell-renderer}
  @see-class{gtk:drop-down}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- combo-box-active -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'combo-box) t)
 "The @code{active} property of type @code{:int} (Read / Write) @br{}
  The item which is currently active. If the model is a non-flat treemodel,
  and the active item is not an immediate child of the root of the tree, this
  property has the value @code{(first (gtk:tree-path-indices path))}, where
  @arg{path} is the @class{gtk:tree-path} instance of the active item. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-active)
      "Accessor"
      (documentation 'combo-box-active 'function)
 "@version{2023-9-1}
  @syntax[]{(gtk:combo-box-active object) => index}
  @syntax[]{(setf (gtk:combo-box-active object) index)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[index]{an integer with the index in the model passed during
    construction, or -1 to have no active item}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{active} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-active} function returns the index of the currently
  active item, or -1 if there is no active item. The
  @setf{gtk:combo-box-active} function sets the active item.

  If the model is a non-flat tree model, and the active item is not an
  immediate child of the root of the tree, this function returns
  @code{(first (gtk:tree-path-indices path))}, where @code{path} is the
  @class{gtk:tree-path} instance of the active item.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-path}
  @see-class{gtk:drop-down}")

;;; --- combo-box-active-id ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active-id" 'combo-box) t)
 "The @code{active-id} property of type @code{:string} (Read / Write) @br{}
  The value of the ID column of the active row. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-active-id)
      "Accessor"
      (documentation 'combo-box-active-id 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-active-id object) => active-id}
  @syntax[]{(setf (gtk:combo-box-active-id object) active-id)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[active-id]{a string with the ID of the row to select, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{active-id} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-active-id} function returns the ID of the active row
  of the combo box. This value is taken from the active row and the column
  specified by the @slot[gtk:combo-box]{id-column} property of the combo box.
  The @setf{gtk:combo-box-active-id} function changes the active row of the
  combo box to the one that has an ID equal to @arg{active-id}, or unsets the
  active row if @arg{active-id} is @code{nil}. Rows having a @code{nil} ID
  string cannot be made active by this function.

  If the @code{id-column} property of @arg{combo-box} is unset or if no row
  has the given ID then the function does nothing and returns @code{nil}.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}
  @see-function{gtk:combo-box-id-column}")

;;; --- combo-box-button-sensitivity -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "button-sensitivity"
                                               'combo-box) t)
 "The @code{button-sensitivity} property of type @symbol{gtk:sensitivity-type}
  (Read / Write) @br{}
  Whether the dropdown button is sensitive when the model is empty. @br{}
  Default value: @code{:auto}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-button-sensitivity)
      "Accessor"
      (documentation 'combo-box-button-sensitivity 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-button-sensitivity object) => sensitivity}
  @syntax[]{(setf (gtk:combo-box-button-sensitivity object) sensitivity)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[sensitivity]{a value of the @symbol{gtk:sensitivity-type}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{button-sensitivity} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-button-sensitivity} function returns whether the combo
  box sets the dropdown button sensitive or not when there are no items in the
  model. The @setf{gtk:combo-box-button-sensitivity} function sets the
  sensitivity.

  @code{:on} if the dropdown button is sensitive when the model is empty,
  @code{:off} if the button is always insensitive or @code{:auto} if it
  is only sensitive as long as the model has one item to be selected.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}
  @see-symbol{gtk:sensitivity-type}")

;;; --- combo-box-child --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'combo-box) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-child)
      "Accessor"
      (documentation 'combo-box-child 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-child object) => child}
  @syntax[]{(setf (gtk:combo-box-child object) child)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{child} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-child} function gets the child of the combo box. The
  @setf{gtk:combo-box-child} function sets the child widget.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}
  @see-class{gtk:widget}")

;;; --- combo-box-entry-text-column --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "entry-text-column"
                                               'combo-box) t)
 "The @code{entry-text-column} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model to associate with strings from the entry
  if the combo was created with the value @em{true} for the @code{has-entry}
  property. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-entry-text-column)
      "Accessor"
      (documentation 'combo-box-entry-text-column 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-entry-text-column object) => text-column}
  @syntax[]{(setf (gtk:combo-box-entry-text-column object) text-column)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[text-column]{an integer with a column in model to get the strings
    from for the internal entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{entry-text-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-entry-text-column} function returns the column which
  the combo box is using to get the strings from to display in the internal
  entry. The @setf{gtk:combo-box-entry-text-column} function sets the model
  column which the combo box should use to get strings.

  The column @arg{text-column} in the model of the combo box must be of type
  @code{gchararray}. This is only relevant if the combo box has been created
  with the @code{has-entry} property as @em{true}.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; --- combo-box-has-entry ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-entry" 'combo-box) t)
 "The @code{has-entry} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the combo box has an entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-has-entry)
      "Accessor"
      (documentation 'combo-box-has-entry 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-has-entry object) => has-entry}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[has-entry]{a boolean whether the combo box has an entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{has-entry} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-has-entry} function returns whether the combo box has
  an entry.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; --- combo-box-has-frame ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'combo-box) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether a frame is drawn around the entry. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-has-frame)
      "Accessor"
      (documentation 'combo-box-has-frame 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-has-frame object) => has-frame}
  @syntax[]{(setf (gtk:combo-box-has-frame object) has-frame)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[has-frame]{a boolean whether a frame is drawn around the entry}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{has-frame} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  Controls whether a frame is drawn around the entry.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; --- combo-box-id-column ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "id-column" 'combo-box) t)
 "The @code{id-column} property of type @code{:int} (Read / Write) @br{}
  The column in the combo box's model that provides string IDs for the values
  in the model, if not equal to -1. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-id-column)
      "Accessor"
      (documentation 'combo-box-id-column 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-id-column object) => id-column}
  @syntax[]{(setf (gtk:combo-box-id-column object) id-column)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[id-column]{an integer with a column in model to get string IDs for
    values from}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{id-column} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-id-column} function returns the model column which the
  combo box is using to get string IDs for values from. The
  @setf{gtk:combo-box-id-column} function sets the model column.

  The column @arg{id-column} in the model of the combo box must be of type
  @code{gchararray}.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; --- combo-box-model --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'combo-box) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model from which the combo box takes the values shown in the list.")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-model)
      "Accessor"
      (documentation 'combo-box-model 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-model object) => model}
  @syntax[]{(setf (gtk:combo-box-model object) model)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{model} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-model} function returns the model which is acting as
  data source for the combo box. The @setf{gtk:combo-box-model} function sets
  the model. Will unset a previously set model, if applicable. If @arg{model}
  is @code{nil}, then it will unset the model.

  Note that this function does not clear the cell renderers, you have to call
  the @fun{gtk:cell-layout-clear} function yourself if you need to set up
  different cell renderers for the new model.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-class{gtk:drop-down}
  @see-function{gtk:cell-layout-clear}")

;;; --- combo-box-popup-fixed-width --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-fixed-width"
                                               'combo-box) t)
 "The @code{popup-fixed-width} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the popup's width should be a fixed width matching the allocated
  width of the combo box. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-popup-fixed-width)
      "Accessor"
      (documentation 'combo-box-popup-fixed-width 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-popup-fixed-width object) => fixed}
  @syntax[]{(setf (gtk:combo-box-popup-fixed-width object) fixed)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[fixed]{a boolean whether to use a fixed popup width}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{popup-fixed-width} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  The @fun{gtk:combo-box-popup-fixed-width} function gets whether the popup
  uses a fixed width matching the allocated width of the combo box. The
  @setf{gtk:combo-box-popup-fixed-width} function specifies whether the popup's
  width should be a fixed.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; --- combo-box-popup-shown --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-shown" 'combo-box) t)
 "The @code{popup-shown} property of type @code{:boolean} (Read) @br{}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the
  @code{\"notify::popup-shown\"} signal. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'combo-box-popup-shown)
      "Accessor"
      (documentation 'combo-box-popup-shown 'function)
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-popup-shown object) => popup-shown}
  @syntax[]{(setf (gtk:combo-box-popup-shown object) popup-shown)}
  @argument[object]{a @class{gtk:combo-box} widget}
  @argument[popup-shown]{a boolean whether the combo boxes dropdown is popped
    up}
  @begin{short}
    Accessor of the @slot[gtk:combo-box]{popup-shown} slot of the
    @class{gtk:combo-box} class.
  @end{short}
  Whether the combo boxes dropdown is popped up. Note that this property is
  mainly useful, because it allows you to connect to the
  @code{\"notify::popup-shown\"} signal.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new))

(defun combo-box-new ()
 #+liber-documentation
 "@version{#2023-9-1}
  @return{A new @class{gtk:combo-box} widget.}
  @short{Creates a new empty combo box.}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-NEW is deprecated since 4.10.~%"))
  (make-instance 'combo-box))

(export 'combo-box-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-entry))

(defun combo-box-new-with-entry ()
 #+liber-documentation
 "@version{#2023-9-1}
  @return{A new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box with an entry.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-NEW-WITH-ENTRY is deprecated since 4.10.~%"))
  (make-instance 'combo-box
                 :has-entry t))

(export 'combo-box-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-model))

(defun combo-box-new-with-model (model)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{A new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new combo box with the model initialized to @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-NEW-WITH-MODEL is deprecated since 4.10.~%"))
  (make-instance 'combo-box
                 :model model))

(export 'combo-box-new-with-model)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_new_with_model_and_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-new-with-model-and-entry))

(defun combo-box-new-with-model-and-entry (model)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[model]{a @class{gtk:tree-model} object}
  @return{A new @class{gtk:combo-box} widget.}
  @begin{short}
    Creates a new empty combo box with an entry and with the model initialized
    to @arg{model}.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-model}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-NEW-WITH-MODEL-AND-ENTRY is deprecated ~
             since 4.10.~%"))
  (make-instance 'combo-box
                 :model model
                 :has-entry t))

(export 'combo-box-new-with-model-and-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_active_iter ()
;;; gtk_combo_box_set_active_iter () -> combo-box-active-iter
;;; ----------------------------------------------------------------------------

(defun (setf combo-box-active-iter) (iter combo)
  (cffi:foreign-funcall "gtk_combo_box_set_active_iter"
                        (g:object combo-box) combo
                        (g:boxed tree-iter) iter
                        :void)
  iter)

(cffi:defcfun ("gtk_combo_box_get_active_iter" %combo-box-active-iter)
    :boolean
  (combo (g:object combo-box))
  (iter (g:boxed tree-iter)))

(defun combo-box-active-iter (combo)
 #+liber-documentation
 "@version{#2023-9-1}
  @syntax[]{(gtk:combo-box-active-iter combo-box) => iter}
  @syntax[]{(setf (gtk:combo-box-active-iter combo-box) iter)}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[iter]{the @class{gtk:tree-iter}, or @code{nil}}
  @begin{short}
    Accessor of the active iterator of the combo box.
  @end{short}
  The @fun{gtk:combo-box-active-iter} function returns @arg{iter} to point to
  the current active item, if it exists. The @setf{gtk:combo-box-active-iter}
  function sets the current active item to be the one referenced by @arg{iter},
  or unsets the active item if @arg{iter} is @code{nil}.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:tree-iter}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-ACTIVE-ITER is deprecated since 4.10.~%"))
  (let ((iter (make-instance 'tree-iter)))
    (when (%combo-box-active-iter combo iter)
      iter)))

(export 'combo-box-active-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-popup))

(cffi:defcfun ("gtk_combo_box_popup" %combo-box-popup) :void
  (combo (g:object combo-box)))

(defun combo-box-popup (combo)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @begin{short}
    Pops up the menu or dropdown list of the combo box.
  @end{short}
  This function is mostly intended for use by accessibility technologies.
  Applications should have little use for it.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-POPUP is deprecated since 4.10.~%"))
  (%combo-box-popup combo))

(export 'combo-box-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popup_for_device ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-popup-for-device))

(cffi:defcfun ("gtk_combo_box_popup_for_device" %combo-box-popup-for-device)
    :void
  (combo (g:object combo-box))
  (device (g:object gdk:device)))

(defun combo-box-popup-for-device (combo device)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[device]{a @class{gdk:device} object}
  @begin{short}
    Pops up the menu or dropdown list of the combo box.
  @end{short}
  The popup window will be grabbed so only @arg{device} and its associated
  pointer/keyboard are the only @class{gdk:device} objects able to send events
  to it.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gdk:device}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-POPUP-FOR-DEVICE is deprecated since ~
             4.10.~%"))
  (%combo-box-popup-for-device combo device))

(export 'combo-box-popup-for-device)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_popdown ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-popdown))

(cffi:defcfun ("gtk_combo_box_popdown" %combo-box-popdown) :void
  (combo (g:object combo-box)))

(defun combo-box-popdown (combo)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @begin{short}
    Hides the menu or dropdown list of the combo box.
  @end{short}
  This function is mostly intended for use by accessibility technologies.
  Applications should have little use for it.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-POPDOWN is deprecated since 4.10.~%"))
  (%combo-box-popdown combo))

(export 'combo-box-popdown)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_get_row_separator_func ()                not exported
;;; ----------------------------------------------------------------------------

;; TODO: Is this function useful in the Lisp implementation?

(cffi:defcfun ("gtk_combo_box_get_row_separator_func"
               combo-box-get-row-separator-func) :pointer
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[combo-box]{a @class{gtk:combo-box} widget}
  @return{The current row separator function.}
  @short{Returns the current row separator function.}
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}"
  (combo-box (g:object combo-box)))

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_set_row_separator_func ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_combo_box_set_row_separator_func"
               %combo-box-set-row-separator-func) :void
  (combo (g:object combo-box))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun combo-box-set-row-separator-func (combo func)
 #+liber-documentation
 "@version{#2023-9-1}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[func]{a @symbol{gtk:tree-view-row-separator-func} callback function}
  @begin{short}
    Sets the row separator function, which is used to determine whether a row
    should be drawn as a separator.
  @end{short}
  If the row separator function is @code{nil}, no separators are drawn. This is
  the default value.
  @begin[Warning]{dictionary}
    The @class{gtk:combo-box} implementation is deprecated since 4.10. Use the
    @class{gtk:drop-down} widget instead.
  @end{dictionary}
  @see-class{gtk:combo-box}
  @see-class{gtk:drop-down}
  @see-symbol{gtk:tree-view-row-separator-func}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (format *debug-io*
            "Warning: GTK:COMBO-BOX-SET-ROW-SEPARATOR-FUNC is deprecated since ~
             4.10.~%"))
  (%combo-box-set-row-separator-func
          combo
          (cffi:callback tree-view-row-separator-func)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'combo-box-set-row-separator-func)

;;; --- End of file gtk4.combo-box.lisp ----------------------------------------
