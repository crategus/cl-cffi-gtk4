;;; ----------------------------------------------------------------------------
;;; gtk4.widget.lisp
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
;;; GtkWidget
;;;
;;;     Base class for all widgets
;;;
;;; Types and Values
;;;
;;;     GtkWidget
;;;
;;;     GtkRequisition
 ;;     GtkAllocation                                     not implemented
; ;     GtkTextDirection                                  gtk4.enumerations.lisp
;;;     GtkPickFlags                                      gtk4.enumerations.lisp
;;;     GtkOverflow                                       gtk4.enumerations.lisp
;;;     GtkSizeRequestMode                                gtk4.emumerations.lisp
;;;     GtkRequestedSize                                  not implemented
;;;     GtkAlign                                          gtk4.enumerations.lisp
;;;
;;; Accessors
;;;
;;;     gtk_widget_get_can_focus
;;;     gtk_widget_set_can_focus
;;;     gtk_widget_get_can_target
;;;     gtk_widget_set_can_target
;;;     gtk_widget_get_css_classes
;;;     gtk_widget_set_css_classes
;;;     gtk_widget_get_css_name
;;;     gtk_widget_get_cursor
;;;     gtk_widget_set_cursor
;;;     gtk_widget_get_focus_on_click
;;;     gtk_widget_set_focus_on_click
;;;     gtk_widget_get_focusable
;;;     gtk_widget_set_focusable
;;;     gtk_widget_get_halign
;;;     gtk_widget_set_halign
;;;     gtk_widget_has_default
;;;     gtk_widget_has_focus
;;;     gtk_widget_get_has_tooltip
;;;     gtk_widget_set_has_tooltip
;;;     gtk_widget_get_hexpand
;;;     gtk_widget_set_hexpand
;;;     gtk_widget_get_hexpand_set
;;;     gtk_widget_set_hexpand_set
;;;     gtk_widget_set_layout_manager
;;;     gtk_widget_get_layout_manager
;;;     gtk_widget_set_limit_events                         Since 4.18
;;;     gtk_widget_get_limit_events                         Since 4.18
;;;     gtk_widget_get_margin_bottom
;;;     gtk_widget_set_margin_bottom
;;;     gtk_widget_get_margin_end
;;;     gtk_widget_set_margin_end
;;;     gtk_widget_get_margin_start
;;;     gtk_widget_set_margin_start
;;;     gtk_widget_get_margin_top
;;;     gtk_widget_set_margin_top
;;;     gtk_widget_set_name
;;;     gtk_widget_get_name
;;;     gtk_widget_get_opacity
;;;     gtk_widget_set_opacity
;;;     gtk_widget_get_overflow
;;;     gtk_widget_set_overflow
;;;     gtk_widget_get_parent
;;;     gtk_widget_set_receives_default
;;;     gtk_widget_get_receives_default
;;;     gtk_widget_get_root
;;;     gtk_widget_get_scale_factor
;;;     gtk_widget_get_sensitive
;;;     gtk_widget_set_sensitive
;;;     gtk_widget_get_tooltip_markup
;;;     gtk_widget_set_tooltip_markup
;;;     gtk_widget_get_tooltip_text
;;;     gtk_widget_set_tooltip_text
;;;     gtk_widget_get_valign
;;;     gtk_widget_set_valign
;;;     gtk_widget_get_vexpand
;;;     gtk_widget_set_vexpand
;;;     gtk_widget_get_vexpand_set
;;;     gtk_widget_set_vexpand_set
;;;     gtk_widget_get_visible
;;;     gtk_widget_set_visible
;;;
;;; Functions
;;;
;;;     gtk_widget_in_destruction
;;;     gtk_widget_show                                     Deprecated 4.10
;;;     gtk_widget_hide                                     Deprecated 4.10

;;;     gtk_widget_map
;;;     gtk_widget_unmap
;;;     gtk_widget_get_mapped

;;;     gtk_widget_realize
;;;     gtk_widget_unrealize
;;;     gtk_widget_get_realized

;;;     gtk_widget_queue_draw
;;;     gtk_widget_queue_resize
;;;     gtk_widget_queue_allocate
;;;     gtk_widget_get_frame_clock
;;;
;;;     GtkTickCallback
;;;
;;;     gtk_widget_add_tick_callback
;;;     gtk_widget_remove_tick_callback
;;;     gtk_widget_size_allocate
;;;     gtk_widget_allocate
;;;
;;;     gtk_widget_class_add_shortcut
;;;     gtk_widget_class_add_binding                        not implemented
;;;     gtk_widget_class_add_binding_signal                 not implemented
;;;     gtk_widget_class_add_binding_action                 not implemented
;;;     gtk_widget_class_get_layout_manager_type
;;;     gtk_widget_class_set_layout_manager_type
;;;
;;;     gtk_widget_class_set_activate_signal                not implemented
;;;     gtk_widget_class_set_activate_signal_from_name      not implemented
;;;     gtk_widget_class_get_activate_signal                not implemented
;;;
;;;     gtk_widget_activate
;;;     gtk_widget_is_focus
;;;     gtk_widget_grab_focus
;;;     gtk_widget_get_parent
;;;     gtk_widget_set_parent
;;;     gtk_widget_unparent
;;;     gtk_widget_get_native
;;;     gtk_widget_get_ancestor
;;;     gtk_widget_is_ancestor
;;;     gtk_widget_translate_coordinates                    Deprecated 4.12
;;;     gtk_widget_add_controller
;;;     gtk_widget_remove_controller
;;;     gtk_widget_get_direction
;;;     gtk_widget_set_direction
;;;     gtk_widget_get_default_direction
;;;     gtk_widget_set_default_direction
;;;     gtk_widget_create_pango_context
;;;     gtk_widget_get_pango_context
;;;     gtk_widget_get_font_options                         Deprecated 4.16
;;;     gtk_widget_set_font_options                         Deprecated 4.16
;;;     gtk_widget_get_font_map
;;;     gtk_widget_set_font_map
;;;     gtk_widget_create_pango_layout
;;;     gtk_widget_set_cursor_from_name
;;;
;;;     gtk_widget_class_get_accessible_role
;;;     gtk_widget_class_set_accessible_role
;;;     gtk_widget_child_focus
;;;     gtk_widget_get_child_visible
;;;     gtk_widget_set_child_visible
;;;     gtk_widget_get_settings
;;;     gtk_widget_get_clipboard
;;;     gtk_widget_get_primary_clipboard
;;;     gtk_widget_get_display
;;;     gtk_widget_get_size_request
;;;     gtk_widget_set_size_request
;;;
;;;     gtk_widget_list_mnemonic_labels
;;;     gtk_widget_add_mnemonic_label
;;;     gtk_widget_remove_mnemonic_label
;;;     gtk_widget_mnemonic_activate
;;;
;;;     gtk_widget_error_bell
;;;     gtk_widget_keynav_failed
;;;     gtk_widget_trigger_tooltip_query
;;;
;;;     gtk_widget_get_allocated_width                      Deprecated 4.12
;;;     gtk_widget_get_allocated_height                     Deprecated 4.12
;;;     gtk_widget_get_allocation                           Deprecated 4.12
;;;     gtk_widget_get_allocated_baseline                   Deprecated 4.12
;;;
;;;     gtk_widget_get_width
;;;     gtk_widget_get_height
;;;     gtk_widget_get_size
;;;     gtk_widget_get_baseline                             Since 4.12
;;;     gtk_widget_compute_bounds
;;;     gtk_widget_compute_transform
;;;     gtk_widget_compute_point
;;;     gtk_widget_contains
;;;     gtk_widget_pick
;;;     gtk_widget_get_focus_child
;;;     gtk_widget_set_focus_child
;;;     gtk_widget_is_sensitive
;;;     gtk_widget_is_visible
;;;     gtk_widget_get_state_flags
;;;     gtk_widget_set_state_flags
;;;     gtk_widget_unset_state_flags
;;;     gtk_widget_has_visible_focus
;;;     gtk_widget_is_drawable
;;;
;;;     gtk_widget_measure
;;;     gtk_widget_snapshot
;;;     gtk_widget_snapshot_child
;;;     gtk_widget_get_next_sibling
;;;     gtk_widget_get_prev_sibling
;;;     gtk_widget_get_first_child
;;;     gtk_widget_get_last_child
;;;     gtk_widget_insert_before
;;;     gtk_widget_insert_after
;;;     gtk_widget_should_layout

;;;     gtk_widget_get_color                                Since 4.10

;;;     gtk_widget_add_css_class
;;;     gtk_widget_remove_css_class
;;;     gtk_widget_has_css_class
;;;     gtk_widget_class_get_css_name
;;;     gtk_widget_class_set_css_name
;;;     gtk_widget_get_style_context                        Deprecated 4.10
;;;
;;;     gtk:widget-add-provider                             Lisp extension
;;;     gtk:widget-remove-provider                          Lisp extension
;;;
;;;     gtk_requisition_new
;;;     gtk_requisition_copy
;;;     gtk_requisition_free                                not needed
;;;     gtk_widget_get_request_mode
;;;     gtk_widget_get_preferred_size
;;;     gtk_distribute_natural_allocation                   not needed
;;;     gtk_widget_compute_expand
;;;
;;;     gtk_widget_init_template
;;;     gtk_widget_dispose_template                         Since 4.8
;;;     gtk_widget_get_template_child
;;;     gtk_widget_class_set_template
;;;     gtk_widget_class_set_template_from_resource
;;;     gtk_widget_class_set_template_scope
;;;     gtk_widget_class_bind_template_child
;;;     gtk_widget_class_bind_template_child_internal
;;;     gtk_widget_class_bind_template_child_private
;;;     gtk_widget_class_bind_template_child_internal_private
;;;     gtk_widget_class_bind_template_child_full
;;;     gtk_widget_class_bind_template_callback
;;;     gtk_widget_class_bind_template_callback_full
;;;
;;;     gtk_widget_observe_children
;;;     gtk_widget_observe_controllers
;;;     gtk_widget_insert_action_group
;;;     gtk_widget_activate_action
;;;     gtk_widget_activate_action_variant
;;;     gtk_widget_activate_default
;;;
;;;     GtkWidgetActionActivateFunc
;;;
;;;     gtk_widget_class_install_action
;;;     gtk_widget_class_install_property_action
;;;     gtk_widget_class_query_action
;;;     gtk_widget_action_set_enabled
;;;
;;; Properties
;;;
;;;     can-focus
;;;     can-target
;;;     css-classes
;;;     css-name
;;;     cursor
;;;     focus-on-click
;;;     focusable
;;;     halign
;;;     has-default
;;;     has-focus
;;;     has-tooltip
;;;     height-request
;;;     hexpand
;;;     hexpand-set
;;;     layout-manager
;;;     limit-events                                        Since 4.18
;;;     margin-bottom
;;;     margin-end
;;;     margin-start
;;;     margin-top
;;;     name
;;;     opacity
;;;     overflow
;;;     parent
;;;     receives-default
;;;     root
;;;     scale-factor
;;;     sensitive
;;;     tooltip-markup
;;;     tooltip-text
;;;     valign
;;;     vexpand
;;;     vexpand-set
;;;     visible
;;;     width-request
;;;
;;; Signals
;;;
;;;     destroy
;;;     direction-changed
;;;     hide
;;;     keynav-failed
;;;     map
;;;     mnemonic-activate
;;;     move-focus
;;;     query-tooltip
;;;     realize
;;;     show
;;;     state-flags-changed
;;;     unmap
;;;     unrealize
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRequestedSize
;;; ----------------------------------------------------------------------------

;; Only used for the gtk_distribute_natural_allocation() function and not
;; implemented.

;;; ----------------------------------------------------------------------------
;;; GtkRequisition
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct requisition "GtkRequisition"
  (:export t
   :type-initializer "gtk_requisition_get_type")
  (width :int :initform 0)
  (height :int :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'requisition)
      "GBoxed"
      (documentation 'requisition 'type)
 "@version{2025-02-13}
  @begin{declaration}
(glib:define-gboxed-cstruct requisition \"GtkRequisition\"
  (:export t
   :type-initializer \"gtk_requistion_get_type\")
  (width :int :initform 0)
  (height :int :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[width]{The integer with the desired width of the widget.}
      @entry[height]{The integer with the desired height of the widget.}
    @end{table}
  @end{values}
  @begin{short}
    The @class{gtk:requisition} structure represents the desired size of a
    widget.
  @end{short}
  See the section called \"Height-for-width Geometry Management\" in the
  @class{gtk:widget} documentation for more information.
  @see-constructor{gtk:requisition-new}
  @see-constructor{gtk:requisition-copy}
  @see-slot{gtk:requisition-height}
  @see-slot{gtk:requisition-width}
  @see-class{gtk:widget}
  @see-function{gtk:widget-preferred-size}")

;;; ----------------------------------------------------------------------------
;;; Accessors of GtkRequistion
;;; ----------------------------------------------------------------------------

;;; --- gtk:requisition-height -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'requisition-height)
      "Accessor"
      (documentation 'requisition-height 'function)
 "@version{2025-02-13}
  @syntax{(gtk:requisition-height instance) => height}
  @syntax{(setf (gtk:requisition-height instance) height)}
  @argument[instance]{a @class{gtk:requisition} instance}
  @argument[height]{an integer for the height}
  @begin{short}
    Accessor of the @arg{height} slot of the @class{gtk:requisition} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar requisition (gtk:requisition-new)) => REQUISITION
(setf (gtk:requisition-height requisition) 100) => 100
(gtk:requisition-height requisition) => 100
    @end{pre}
  @end{dictionary}
  @see-class{gtk:requisition}
  @see-function{gtk:requisition-width}")

(export 'requisition-height)

;;; --- gtk:requisition-width --------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'requisition-width)
      "Accessor"
      (documentation 'requisition-width 'function)
 "@version{2025-02-13}
  @syntax{(gtk:requisition-width instance) => width}
  @syntax{(setf (gtk:requisition-width instance) width)}
  @argument[instance]{a @class{gtk:requisition} instance}
  @argument[width]{an integer for the width}
  @begin{short}
    Accessor of the @arg{width} slot of the @class{gtk:requisition} structure.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar requisition (gtk:requisition-new)) => REQUISITION
(setf (gtk:requisition-width requisition) 100) => 100
(gtk:requisition-width requisition) => 100
    @end{pre}
  @end{dictionary}
  @see-class{gtk:requisition}
  @see-function{gtk:requisition-height}")

(export 'requisition-width)

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_new
;;; ----------------------------------------------------------------------------

(declaim (inline requisition-new))

(defun requisition-new (&key (width 0) (height 0))
 #+liber-documentation
 "@version{2025-02-13}
  @argument[width]{an integer for the width, default 0}
  @argument[height]{an integer for the height, default 0}
  @return{The new @class{gtk:requisition} instance.}
  @short{Allocates a new @class{gtk:requisition} instance.}
  @see-class{gtk:requisition}"
  (make-requisition :width width :height height))

(export 'requisition-new)

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_copy
;;; ----------------------------------------------------------------------------

(declaim (inline requisition-copy))

(defun requisition-copy (requisition)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[requisition]{a @class{gtk:requisition} instance}
  @begin{return}
    The @class{gtk:requisition} instance with the copy of @arg{requisition}.
  @end{return}
  @short{Copies a @class{gtk:requisition} instance.}
  @see-class{gtk:requisition}"
  (copy-requisition requisition))

(export 'requisition-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_requisition_free                                    not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkWidget" widget
  (:superclass g:initially-unowned
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_widget_get_type")
  ((can-focus
    widget-can-focus
    "can-focus" "gboolean" t t)
   (can-target
    widget-can-target
    "can-target" "gboolean" t t)
   (css-classes
    widget-css-classes
    "css-classes" "GStrv" t t)
   (css-name
    widget-css-name
    "css-name" "gchararray" t t)
   (cursor
    widget-cursor
    "cursor" "GdkCursor" t t)
   (focus-on-click
    widget-focus-on-click
    "focus-on-click" "gboolean" t t)
   (focusable
    widget-focusable
    "focusable" "gboolean" t t)
   (halign
    widget-halign
    "halign" "GtkAlign" t t)
   (has-default
    widget-has-default
    "has-default" "gboolean" t nil)
   (has-focus
    widget-has-focus
    "has-focus" "gboolean" t nil)
   (has-tooltip
    widget-has-tooltip
    "has-tooltip" "gboolean" t t)
   (height-request
    widget-height-request
    "height-request" "gint" t t)
   (hexpand
    widget-hexpand
    "hexpand" "gboolean" t t)
   (hexpand-set
    widget-hexpand-set
    "hexpand-set" "gboolean" t t)
   (layout-manager
    widget-layout-manager
    "layout-manager" "GtkLayoutManager" t t)
   #+gtk-4-18
   (limit-events
    widget-limit-events
    "limit-events" "gboolean" t t)
   (margin-bottom
    widget-margin-bottom
    "margin-bottom" "gint" t t)
   (margin-end
    widget-margin-end
    "margin-end" "gint" t t)
   (margin-start
    widget-margin-start
    "margin-start" "gint" t t)
   (margin-top
    widget-margin-top
    "margin-top" "gint" t t)
   (name
    widget-name
    "name" "gchararray" t t)
   (opacity
    widget-opacity
    "opacity" "gdouble" t t)
   (overflow
    widget-overflow
    "overflow" "GtkOverflow" t t)
   (parent
    widget-parent
    "parent" "GtkWidget" t nil)
   (receives-default
    widget-receives-default
    "receives-default" "gboolean" t t)
   (root
    widget-root
    "root" "GtkRoot" t nil)
   (scale-factor
    widget-scale-factor
    "scale-factor" "gint" t nil)
   (sensitive
    widget-sensitive
    "sensitive" "gboolean" t t)
   (tooltip-markup
    widget-tooltip-markup
    "tooltip-markup" "gchararray" t t)
   (tooltip-text
    widget-tooltip-text
    "tooltip-text" "gchararray" t t)
   (valign
    widget-valign
    "valign" "GtkAlign" t t)
   (vexpand
    widget-vexpand
    "vexpand" "gboolean" t t)
   (vexpand-set
    widget-vexpand-set
    "vexpand-set" "gboolean" t t)
   (visible
    widget-visible
    "visible" "gboolean" t t)
   (width-request
    widget-width-request
    "width-request" "gint" t t)))

;; TODO: Extend the documentation for widget templates.

#+liber-documentation
(setf (documentation 'widget 'type)
 "@version{2025-02-13}
  @begin{short}
    The @class{gtk:widget} class is the base class all widgets in GTK derive
    from. It manages the widget life cycle, layout, states and style.
  @end{short}

  @subheading{Height-for-width Geometry Management}
  GTK uses a height-for-width and width-for-height geometry management system.
  Height-for-width means that a widget can change how much vertical space it
  needs, depending on the amount of horizontal space that it is given and
  similar for width-for-height. The most common example is a label that reflows
  to fill up the available width, wraps to fewer lines, and therefore needs
  less height.

  GTK also supports baseline vertical alignment of widgets. This means that
  widgets are positioned such that the typographical baseline of widgets in the
  same row are aligned. This happens if a widget supports baselines, has a
  vertical alignment of @code{:baseline}, and is inside a container that
  supports baselines and has a natural \"row\" that it aligns to the baseline,
  or a baseline assigned to it by the grandparent.

  If a widget ends up baseline aligned it will be allocated all the space in
  the parent as if it was @code{:fill}, but the selected baseline can be found
  via the @fun{gtk:widget-allocated-baseline} function. If this has a value
  other than -1 you need to align the widget such that the baseline appears at
  the position.

  @subheading{GtkWidget as GtkBuildable}
  The @class{gtk:widget} implementation of the @class{gtk:buildable} interface
  supports various custom elements to specify additional aspects of widgets
  that are not directly expressed as properties.

  If the widget uses a @class{gtk:layout-manager} object, the @class{gtk:widget}
  implementation supports a custom @code{<layout>} element, used to define
  layout properties:
  @begin{pre}
<object class=\"GtkGrid\" id=\"my_grid\">
  <child>
    <object class=\"GtkLabel\" id=\"label1\">
      <property name=\"label\">Description</property>
      <layout>
        <property name=\"column\">0</property>
        <property name=\"row\">0</property>
        <property name=\"row-span\">1</property>
        <property name=\"column-span\">1</property>
      </layout>
    </object>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"description_entry\">
      <layout>
        <property name=\"column\">1</property>
        <property name=\"row\">0</property>
        <property name=\"row-span\">1</property>
        <property name=\"column-span\">1</property>
      </layout>
    </object>
  </child>
</object>
  @end{pre}
  The @class{gtk:widget} implementation allows style information such as style
  classes to be associated with widgets, using the custom @code{<style>}
  element:
  @begin{pre}
<object class=\"GtkButton\" id=\"button1\">
  <style>
    <class name=\"my-special-button-class\"/>
    <class name=\"dark-button\"/>
  </style>
</object>
  @end{pre}
  The @class{gtk:widget} implementation allows defining accessibility
  information, such as properties, relations, and states, using the custom
  @code{<accessibility>} element:
  @begin{pre}
<object class=\"GtkButton\" id=\"button1\">
  <accessibility>
    <property name=\"label\">Download</property>
    <relation name=\"labelled-by\">label1</relation>
  </accessibility>
</object>
  @end{pre}
  @subheading{Building composite widgets from template XML}
  The @class{gtk:widget} implementation exposes some facilities to automate the
  proceedure of creating composite widgets using templates.

  To create composite widgets with @class{gtk:builder} XML, one must associate
  the interface description with the widget class at class initialization time
  using the @fun{gtk:widget-class-set-template} function.

  The interface description semantics expected in composite template
  descriptions is slightly different from regulare @class{gtk:builder} XML.
  Unlike regular interface descriptions, the @fun{gtk:widget-class-set-template}
  function will expect a @code{<template>} tag as a direct child of the toplevel
  @code{<interface>} tag. The @code{<template>} tag must specify the
  @code{\"class\"} attribute which must be the type name of the widget.
  Optionally, the @code{\"parent\"} attribute may be specified to specify the
  direct parent type of the widget type, this is ignored by the
  @class{gtk:builder} object but can be used by UI design tools to introspect
  what kind of properties and internal children exist for a given type when the
  actual type does not exist.

  The XML which is contained inside the @code{<template>} tag behaves as if it
  were added to the @code{<object>} tag defining the widget itself. You may set
  properties on the widget by inserting @code{<property>} tags into the
  @code{<template>} tag, and also add @code{<child>} tags to add children and
  extend the widget in the normal way you would with @code{<object>} tags.

  Additionally, @code{<object>} tags can also be added before and after the
  initial @code{<template>} tag in the normal way, allowing one to define
  auxilary objects which might be referenced by other widgets declared as
  children of the @code{<template>} tag.

  @b{Example:} A @class{gtk:builder} template definition
    @begin{pre}
<interface>
  <template class=\"FooWidget\" parent=\"GtkBox\">
    <property name=\"orientation\">GTK_ORIENTATION_HORIZONTAL</property>
    <property name=\"spacing\">4</property>
    <child>
      <object class=\"GtkButton\" id=\"hello_button\">
        <property name=\"label\">Hello World</property>
      </object>
    </child>
    <child>
      <object class=\"GtkButton\" id=\"goodbye_button\">
        <property name=\"label\">Goodbye World</property>
      </object>
    </child>
  </template>
</interface>
    @end{pre}
  @begin[Signal Details]{dictionary}
    @subheading{The \"destroy\" signal}
      @begin{pre}
lambda (widget)    :no-hooks
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Signals that all holders of a reference to the widget should release the
      reference that they hold. May result in finalization of the widget if all
      references are released.
    @subheading{The \"direction-changed\" signal}
      @begin{pre}
lambda (widget direction)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object on which the signal is
          emitted.}
        @entry[direction]{The previous @symbol{gtk:text-direction} value with
          the text direction of the widget.}
      @end{table}
      Emitted when the text direction of a widget changes.
    @subheading{The \"hide\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the widget is hidden, for example with the
      @fun{gtk:widget-hide} function.
    @subheading{The \"keynav-failed\" signal}
      @begin{pre}
lambda (widget direction)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[direction]{The @symbol{gtk:direction-type} value with the
          direction of movement.}
        @entry[Returns]{@em{True} if stopping keyboard navigation is fine,
          @em{false} if the emitting widget should try to handle the keyboard
          navigation attempt in its parent container(s).}
      @end{table}
      Gets emitted if keyboard navigation fails.
    @subheading{The \"map\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the widget is going to be mapped, that is when the widget
      is visible, which is controlled with the @fun{gtk:widget-visible}
      function, and all its parents up to the toplevel widget are also visible.
      Once the map has occurred, the @code{\"map-event\"} signal will be
      emitted. The @code{\"map\"} signal can be used to determine whether a
      widget will be drawn, for instance it can resume an animation that was
      stopped during the emission of the @code{\"unmap\"} signal.
    @subheading{The \"mnemonic-activate\" signal}
      @begin{pre}
lambda (widget cycling)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[cycling]{@em{True} if there are other widgets with the same
          mnemonic.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked
          for the event, @em{false} to propagate the event further.}
      @end{table}
      The default handler for this signal activates the widget if the
      @arg{cycling} argument is @em{false}, or just makes the widget grab focus
      if the @arg{cycling} argument is @em{true}.
    @subheading{The \"move-focus\" signal}
      @begin{pre}
lambda (widget direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[direction]{The @symbol{gtk:direction-type} value with the
          direction.}
      @end{table}
    @subheading{The \"query-tooltip\" signal}
      @begin{pre}
lambda (widget x y mode tooltip)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[x]{The integer with the x coordinate of the cursor position where
          the request has been emitted, relative to the left side of the widget.}
        @entry[y]{The integer with the y coordinate of the cursor position where
          the request has been emitted, relative to the top of the widget.}
        @entry[mode]{@em{True} if the tooltip was trigged using the keyboard.}
        @entry[tooltip]{The @class{gtk:tooltip} object.}
        @entry[Returns]{@em{True} if the tooltip should be shown right now,
          @em{false} otherwise.}
      @end{table}
      Emitted when the tooltip of the widget is about to be shown. This happens
      when the @slot[gtk:widget]{has-tooltip} property is @em{true} and the
      hover timeout has expired with the cursor hovering \"above\" the widget or
      emitted when the widget got focus in keyboard mode. Using the given
      coordinates, the signal handler should determine whether a tooltip should
      be shown for the widget. If this is the case @em{true} should be returned,
      @em{false} otherwise. Note that if the @arg{mode} argument is @em{true},
      the @arg{x} and @arg{y} values are undefined and should not be used. The
      signal handler is free to manipulate the @arg{tooltip} argument with the
      therefore destined function calls.
    @subheading{The \"realize\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the widget is associated with a @class{gdk:surface} object,
      which means that the @fun{gtk:widget-realize} function has been called or
      the widget has been mapped, that is, it is going to be drawn.
    @subheading{The \"show\" signal}
      @begin{pre}
lambda (widget)
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the widget is shown, for example with the
      @fun{gtk:widget-show} function.
    @subheading{The \"state-flags-changed\" signal}
      @begin{pre}
lambda (widget flags)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
        @entry[flags]{The previous @symbol{gtk:state-flags} value with the state
          flags.}
      @end{table}
      Emitted when the widget state changes.
    @subheading{The \"unmap\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the widget is going to be unmapped, which means that either
      it or any of its parents up to the toplevel widget have been set as
      hidden. As the  @code{\"unmap\"} signal indicates that a widget will not
      be shown any longer, it can be used to, for example, stop an animation on
      the widget.
    @subheading{The \"unrealize\" signal}
      @begin{pre}
lambda (widget)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[widget]{The @class{gtk:widget} object which received the signal.}
      @end{table}
      Emitted when the @class{gdk:surface} object associated with the widget
      is destroyed, which means that the @fun{gtk:widget-unrealize} function has
      been called or the widget has been unmapped, that is, it is going to be
      hidden.
  @end{dictionary}
  @see-slot{gtk:widget-can-focus}
  @see-slot{gtk:widget-can-target}
  @see-slot{gtk:widget-css-classes}
  @see-slot{gtk:widget-css-name}
  @see-slot{gtk:widget-cursor}
  @see-slot{gtk:widget-focus-on-click}
  @see-slot{gtk:widget-focusable}
  @see-slot{gtk:widget-halign}
  @see-slot{gtk:widget-has-default}
  @see-slot{gtk:widget-has-focus}
  @see-slot{gtk:widget-has-tooltip}
  @see-slot{gtk:widget-height-request}
  @see-slot{gtk:widget-hexpand}
  @see-slot{gtk:widget-hexpand-set}
  @see-slot{gtk:widget-layout-manager}
  @see-slot{gtk:widget-limit-events}
  @see-slot{gtk:widget-margin-bottom}
  @see-slot{gtk:widget-margin-end}
  @see-slot{gtk:widget-margin-start}
  @see-slot{gtk:widget-margin-top}
  @see-slot{gtk:widget-name}
  @see-slot{gtk:widget-opacity}
  @see-slot{gtk:widget-overflow}
  @see-slot{gtk:widget-parent}
  @see-slot{gtk:widget-receives-default}
  @see-slot{gtk:widget-root}
  @see-slot{gtk:widget-scale-factor}
  @see-slot{gtk:widget-sensitive}
  @see-slot{gtk:widget-tooltip-markup}
  @see-slot{gtk:widget-tooltip-text}
  @see-slot{gtk:widget-valign}
  @see-slot{gtk:widget-vexpand}
  @see-slot{gtk:widget-vexpand-set}
  @see-slot{gtk:widget-visible}
  @see-slot{gtk:widget-width-request}
  @see-class{gtk:buildable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:widget-can-focus ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-focus" 'widget) t)
 "The @code{can-focus} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget can accept the input focus. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-can-focus)
      "Accessor"
      (documentation 'widget-can-focus 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-can-focus object) => setting}
  @syntax{(setf (gtk:widget-can-focus object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether or not @arg{widget} can own the input
    focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{can-focus} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-can-focus} function returns @em{true} if the widget can
  own the input focus, @em{false} otherwise. The @setf{gtk:widget-can-focus}
  function sets whether the widget can own the input focus.

  See the @fun{gtk:widget-grab-focus} function for actually setting the input
  focus on a widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}")

;;; --- gtk:widget-can-target --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-target" 'widget) t)
 "The @code{can-target} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget can receive pointer events. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-can-target)
      "Accessor"
      (documentation 'widget-can-target 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-can-target object) => setting}
  @syntax{(setf (gtk:widget-can-target object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether or not @arg{widget} can receive pointer
    events}
  @begin{short}
    Accessor of the @slot[gtk:widget]{can-target} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-can-target} function returns whether the widget can be
  the target of pointer events. The @setf{gtk:widget-can-target} function sets
  the property.
  @see-class{gtk:widget}")

;;; --- gtk:widget-css-classes -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "css-classes" 'widget) t)
 "The @code{css-classes} property of type @code{g:strv-t} (Read / Write) @br{}
  The list of CSS classes applied to the widget.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-css-classes)
      "Accessor"
      (documentation 'widget-css-classes 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-css-classes object) => classes}
  @syntax{(setf (gtk:widget-css-classes object) classes)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[classes]{a list of strings for the CSS classes applied to the
    widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{css-classes} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-css-classes} function returns the list of style classes
  applied to the widget. The @setf{gtk:widget-css-classes} function clears all
  style classes applied to the widget and replace them with @arg{classes}.
  @see-class{gtk:widget}")

;;; --- gtk:widget-css-name ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "css-name" 'widget) t)
 "The @code{css-name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name of the widget in the CSS tree. This property is meant to be set by
  widget implementations, typically in their instance function.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-css-name)
      "Accessor"
      (documentation 'widget-css-name 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-css-name object) => name}
  @syntax{(setf (gtk:widget-css-name object) name)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[name]{a string for the name of the widget in the CSS tree}
  @begin{short}
    Accessor of the @slot[gtk:widget]{css-name} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-css-name} function returns the CSS name that is used for
  the widget. This property is meant to be set by widget implementations,
  typically in their instance init function.
  @see-class{gtk:widget}")

;;; --- gtk:widget-cursor ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor" 'widget) t)
 "The @code{cursor} property of type @class{gdk:cursor} (Read / Write) @br{}
  The cursor used by the widget.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-cursor)
      "Accessor"
      (documentation 'widget-cursor 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-cursor object) => cursor}
  @syntax{(setf (gtk:widget-cursor object) cursor)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[cursor]{a @class{gdk:cursor} object}
  @begin{short}
    Accessor of the @slot[gtk:widget]{cursor} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-cursor} function queries the cursor set on the widget.
  The @setf{gtk:widget-cursor} function sets the cursor to be shown when pointer
  devices point towards the widget. If the @slot[gtk:widget]{cursor} property
  is @code{nil}, the widget will use the cursor inherited from the parent
  widget.
  @see-class{gtk:widget}
  @see-class{gdk:cursor}")

;;; --- gtk:widget-focus-on-click ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focus-on-click" 'widget) t)
 "The @code{focus-on-click} property of type @code{:boolean} (Read / Write)@br{}
  Whether the widget should grab focus when it is clicked with the mouse. This
  property is only relevant for widgets that can take focus. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-focus-on-click)
      "Accessor"
      (documentation 'widget-focus-on-click 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-focus-on-click object) => setting}
  @syntax{(setf (gtk:widget-focus-on-click object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget should grab focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{focus-on-click} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-focus-on-click} function returns @em{true} if the widget
  should grab focus when it is clicked with the mouse. The
  @setf{gtk:widget-focus-on-click} function sets whether the widget should grab
  focus.

  Making mouse clicks not grab focus is useful in places like toolbars where you
  do not want the keyboard focus removed from the main area of the application.
  @see-class{gtk:widget}")

;;; --- gtk:widget-focusable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focusable" 'widget) t)
 "The @code{focusable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget itself will accept the input focus.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-focusable)
      "Accessor"
      (documentation 'widget-focusable 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-focusable object) => setting}
  @syntax{(setf (gtk:widget-focusable object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether @arg{object} itself will accept the
    input focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{focusable} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-focusable} function determines whether the widget can own
  the input focus. The @setf{gtk:widget-focusable} function specifies whether
  the widget can own the input focus.

  Widget implementations should set the @slot[gtk:widget]{focusable} property to
  @em{true} in their init function if they want to receive keyboard input. Note
  that having the @slot[gtk:widget]{focusable} property be @em{true} is only one
  of the necessary conditions for being focusable. A widget must also have the
  @slot[gtk:widget]{sensitive} and @slot[gtk:widget]{can-focus} properties be
  @em{true} and not have an ancestor that has the @slot[gtk:widget]{can-focus}
  property set to @em{false} in order to receive input focus. See the
  @fun{gtk:widget-grab-focus} function for actually setting the input focus on
  a widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}
  @see-function{gtk:widget-can-focus}
  @see-function{gtk:widget-sensitive}")

;;; --- gtk:widget-halign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "halign" 'widget) t)
 "The @code{halign} property of type @symbol{gtk:align} (Read / Write) @br{}
  How to distribute horizontal space if the widget gets extra space. @br{}
  Default value: @code{:fill}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-halign)
      "Accessor"
      (documentation 'widget-halign 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-halign object) => align}
  @syntax{(setf (gtk:widget-halign object) align)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[align]{a @symbol{gtk:align} value}
  @begin{short}
    Accessor of the @slot[gtk:widget]{halign} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-halign} function returns the horizontal alignment of the
  widget. The @setf{gtk:widget-halign} function sets the horizontal alignment.
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-valign}")

;;; --- gtk:widget-has-default -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-default" 'widget) t)
 "The @code{has-default} property of type @code{:boolean} (Read) @br{}
  Whether the widget is the default widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-default)
      "Accessor"
      (documentation 'widget-has-default 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-has-default object) => setting}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget is the default widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-default} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-has-default} function returns @em{true} if the widget is
  the current default widget within its toplevel, @em{false} otherwise.
  @see-class{gtk:widget}")

;;; --- gtk:widget-has-focus ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-focus" 'widget) t)
 "The @code{has-focus} property of type @code{:boolean} (Read) @br{}
  Whether the widget has the input focus. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-focus)
      "Accessor"
      (documentation 'widget-has-focus 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-has-focus object) => setting}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget has the input focus}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-focus} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-has-focus} function returns @em{true} if the widget has
  the global input focus.

  See the @fun{gtk:widget-is-focus} function for the difference between having
  the global input focus, and only having the focus within a toplevel.
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-focus}")

;;; --- gtk:widget-has-tooltip -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-tooltip" 'widget) t)
 "The @code{has-tooltip} property of type @code{:boolean} (Read / Write) @br{}
  Enables or disables the emission of the @code{\"query-tooltip\"} signal on the
  widget. A @em{true} value indicates that the widget can have a tooltip, in
  this case the widget will be queried using the @code{\"query-tooltip\"} signal
  to determine whether it will provide a tooltip or not. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-has-tooltip)
      "Accessor"
      (documentation 'widget-has-tooltip 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-has-tooltip object) => setting}
  @syntax{(setf (gtk:widget-has-tooltip object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the emission of the
    @code{\"query-toolip\"} signal is enabled or disabled}
  @begin{short}
    Accessor of the @slot[gtk:widget]{has-tooltip} slot of the
    @class{gtk:widget} class.
  @end{short}
  Enables or disables the emission of the @code{\"query-tooltip\"} signal on the
  widget. A @em{true} value indicates that the widget can have a tooltip, in
  this case the widget will be queried using the @code{\"query-tooltip\"} signal
  to determine whether it will provide a tooltip or not.
  @see-class{gtk:widget}")

;;; --- gtk:widget-height-request ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height-request" 'widget) t)
 "The @code{height-request} property of type @code{:int} (Read / Write) @br{}
  Override for height request of the widget, or -1 if natural request should be
  used. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-height-request)
      "Accessor"
      (documentation 'widget-height-request 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-height-request object) => height}
  @syntax{(setf (gtk:widget-height-request object) height)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[height]{an integer for the height request}
  @begin{short}
    Accessor of the @slot[gtk:widget]{height-request} slot of the
    @class{gtk:widget} class.
  @end{short}
  See the @fun{gtk:widget-size-request} function for details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-width-request}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-hexpand -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hexpand" 'widget) t)
 "The @code{hexpand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand horizontally. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-hexpand)
      "Accessor"
      (documentation 'widget-hexpand 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-hexpand object) => setting}
  @syntax{(setf (gtk:widget-hexpand object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to expand horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{hexpand} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-hexpand} function gets whether the widget would like any
  available extra horizontal space. This function only looks at the own
  @slot[gtk:widget]{hexpand} flag of the widget, rather than computing whether
  the entire widget tree rooted at this widget wants to expand.

  The @setf{gtk:widget-hexpand} function sets whether the widget would like any
  available extra horizontal space. Call this function to set the expand flag
  if you would like your widget to become larger horizontally when the window
  has extra room.

  By default, widgets automatically expand if any of their children want to
  expand. To see if a widget will automatically expand given its current
  children and state, call the @fun{gtk:widget-compute-expand} function. A
  container can decide how the expandability of children affects the expansion
  of the container by overriding the @code{compute_expand} virtual method on
  the @class{gtk:widget} class.

  Setting the @slot[gtk:widget]{hexpand} property explicitly with this function
  will override the automatic expand behavior. This function forces the widget
  to expand or not to expand, regardless of children. The override occurs
  because the @fun{gtk:widget-hexpand} function sets the
  @slot[gtk:widget]{hexpand-set} property, which causes the
  @slot[gtk:widget]{hexpand} property of the widget to be used, rather than
  looking at children and widget state.
  @see-class{gtk:widget}
  @see-function{gtk:widget-vexpand}
  @see-function{gtk:widget-hexpand-set}
  @see-function{gtk:widget-compute-expand}")

;;; --- gtk:widget-hexpand-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hexpand-set" 'widget) t)
 "The @code{hexpand-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use the @code{hexpand} property. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-hexpand-set)
      "Accessor"
      (documentation 'widget-hexpand-set 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-hexpand-set object) => setting}
  @syntax{(setf (gtk:widget-hexpand-set object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to use the @slot[gtk:widget]{hexpand}
    property}
  @begin{short}
    Accessor of the @slot[gtk:widget]{hexpand-set} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-hexpand-set} function gets whether the
  @fun{gtk:widget-hexpand} function has been used to explicitly set the expand
  flag on this widget.

  The @slot[gtk:widget]{hexpand-set} property will be set automatically when
  you call the @fun{gtk:widget-hexpand} function to set the
  @slot[gtk:widget]{hexpand} property, so the most likely reason to use the
  @setf{gtk:widget-hexpand-set} function would be to unset an explicit expand
  flag.

  If the @slot[gtk:widget]{hexpand} property is set, then it overrides any
  computed expand value based on child widgets. If the
  @slot[gtk:widget]{hexpand} property is not set, then the expand value depends
  on whether any children of the widget would like to expand. There are few
  reasons to use this function, but it is here for completeness and consistency.
  @see-class{gtk:widget}
  @see-function{gtk:widget-hexpand}")

;;; --- gtk:widget-layout-manager ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "layout-manager" 'widget) t)
 "The @code{layout-manager} property of type @class{gtk:layout-manager}
  (Read / Write) @br{}
  The layout manager to use to compute the preferred size of the widget, and
  allocate its children. This property is meant to be set by widget
  implementations, typically in their instance init function.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-layout-manager)
      "Accessor"
      (documentation 'widget-layout-manager 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-layout-manager object) => manager}
  @syntax{(setf (gtk:widget-layout-manager object) manager)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @begin{short}
    Accessor of the @slot[gtk:widget]{layout-manager} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-layout-manager} function retrieves the layout manager used
  by the widget. The @setf{gtk:widget-layout-manager} function sets the layout
  manager delegate instance that provides an implementation for measuring and
  allocating the children of the widget.
  @begin[Examples]{dictionary}
    Get the layout manager for a button and a grid:
    @begin{pre}
(gtk:widget-layout-manager (make-instance 'gtk:button))
=> #<GTK:BIN-LAYOUT {1005DDB073@}>
(gtk:widget-layout-manager (make-instance 'gtk:grid))
=> #<GTK:GRID-LAYOUT {1005DD9863@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:layout-manager}")

;;; --- gtk:widget-limit-events ------------------------------------------------

#+(and gtk-4-18 liber-documentation)
(setf (documentation (liber:slot-documentation "limit-events" 'widget) t)
 "The @code{limit-events} property of type @code{:boolean} (Read / Write) @br{}
  Makes this widget act like a modal dialog, with respect to event delivery.
  Global event controllers will not handle events with targets inside the
  widget, unless they are set up to ignore propagation limits. See
  the @fun{gtk:event-controller-propagation-limit} function. Since 4.18 @br{}
  Default value: @em{false}")

#+(and gtk-4-18 liber-documentation)
(setf (liber:alias-for-function 'widget-limit-events)
      "Accessor"
      (documentation 'widget-limit-events 'function)
 "@version{2025-05-13}
  @syntax{(gtk:widget-limit-events object) => setting}
  @syntax{(setf (gtk:widget-limit-events object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[manager]{a boolean whether to limit events}
  @begin{short}
    Accessor of the @slot[gtk:widget]{limit-events} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-limit-events} function gets whether the widget acts like
  a modal dialog, with respect to event delivery. The
  @setf{gtk:widget-limit-events} function sets the property.

  Makes this widget act like a modal dialog, with respect to event delivery.
  Global event controllers will not handle events with targets inside the
  widget, unless they are set up to ignore propagation limits. See
  the @fun{gtk:event-controller-propagation-limit} function.

  Since 4.18
  @see-class{gtk:widget}
  @see-function{gtk:event-contoller-propagation-limit}")

;;; --- gtk:widget-margin-bottom -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-bottom" 'widget) t)
 "The @code{margin-bottom} property of type @code{:int} (Read / Write) @br{}
  Margin on bottom side of the widget. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-bottom)
      "Accessor"
      (documentation 'widget-margin-bottom 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-margin-bottom object) => margin}
  @syntax{(setf (gtk:widget-margin-bottom object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer for the margin on the bottom side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-bottom} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-bottom} function gets the bottom marging of the
  widget. The @setf{gtk:widget-margin-bottom} function sets the bottom margin.

  This property adds margin outside of the normal size request of the widget.
  The margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-end --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-end" 'widget) t)
 "The @code{margin-end} property of type @code{:int} (Read / Write) @br{}
  Margin on end of the widget, horizontally. This property supports
  left-to-right text directions. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-end)
      "Accessor"
      (documentation 'widget-margin-end 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-margin-end object) => margin}
  @syntax{(setf (gtk:widget-margin-end object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer for the margin on the end side of the widget,
    horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-end} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-end} function gets the value of the end margin of
  the widget. The @setf{gtk:widget-margin-end} function sets the end margin.

  This property supports left-to-right text directions. This property adds
  margin outside of the normal size request of the widget. The margin will be
  added in addition to the size from the @fun{gtk:widget-size-request} function
  for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-start ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-start" 'widget) t)
 "The @code{margin-start} property of type @code{:int} (Read / Write) @br{}
  Margin on start of the widget, horizontally. This property supports
  left-to-right and right-to-left text directions. This property adds margin
  outside of the normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-start)
      "Accessor"
      (documentation 'widget-margin-start 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-margin-start object) => margin}
  @syntax{(setf (gtk:widget-margin-start object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer for the margin on the start side of the widget,
    horizontally}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-start} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-start} function returns the start margin of the
  widget. The @setf{gtk:widget-margin-start} function sets the start margin.

  This property supports left-to-right and right-to-left text directions. This
  property adds margin outside of the normal size request of the widget. The
  margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-margin-top --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "margin-top" 'widget) t)
 "The @code{margin-top} property of type @code{:int} (Read / Write) @br{}
  Margin on top side of the widget. This property adds margin outside of the
  normal size request of the widget. @br{}
  Allowed values: [0,32767] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-margin-top)
      "Accessor"
      (documentation 'widget-margin-top 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-margin-top object) => margin}
  @syntax{(setf (gtk:widget-margin-top object) margin)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[margin]{an integer for the margin on the top side of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{margin-top} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-margin-top} function returns the top margin of widget.
  The @setf{gtk:widget-margin-top} function sets the top margin.

  This property adds margin outside of the normal size request of the widget.
  The margin will be added in addition to the size from the
  @fun{gtk:widget-size-request} function for example.
  @see-class{gtk:widget}
  @see-function{gtk:widget-size-request}")

;;; --- gtk:widget-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'widget) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  The name of the widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-name)
      "Accessor"
      (documentation 'widget-name 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-name object) => name}
  @syntax{(setf (gtk:widget-name object) name)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[name]{a string for the name of the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{name} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-name} function retrieves the name of the widget. The
  @setf{gtk:widget-name} function sets the name.

  Widgets can be named, which allows you to refer to them from a CSS file. You
  can apply a style to widgets with a particular name in the CSS file. Note
  that the CSS syntax has certain special characters to delimit and represent
  elements in a selector (period, #, >, *...), so using these will make your
  widget impossible to match by name. Any combination of alphanumeric symbols,
  dashes and underscores will suffice.
  @see-class{gtk:widget}")

;;; --- gtk:widget-opacity -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "opacity" 'widget) t)
 "The @code{opacity} property of type @code{:double} (Read / Write) @br{}
  The requested opacity of the widget. @br{}
  Allowed values: [0.0,1.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'widget-opacity)
      "Accessor"
      (documentation 'widget-opacity 'function)
 "@version{2025-06-15}
  @syntax{(gtk:widget-opacity object) => opacity}
  @syntax{(setf (gtk:widget-opacity object) opacity)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[opacity]{a number coerced to a double float for the opacity of
    the widget}
  @begin{short}
    Accessor of the @slot[gtk:widget]{opacity} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-opacity} function fetches the requested opacity for the
  widget. The @setf{gtk:widget-opacity} function request the widget to be
  rendered partially transparent, with opacity 0.0 being fully transparent and
  1.0 fully opaque.

  Opacity works on both toplevel widgets and child widgets, although there are
  some limitations. For toplevel widgets, applying opacity depends on the
  capabilities of the windowing system. On X11, this has any effect only on X
  displays with a compositing manager, see the @fun{gdk:display-is-composited}
  function. On Windows and Wayland it should always work, although setting an
  opacity after the window has been shown may cause some flicker.

  Note that the opacity is inherited through inclusion. If you set a toplevel
  widget to be partially translucent, all of its content will appear
  translucent, since it is ultimatively rendered on that toplevel widget. The
  opacity value itself is not inherited by child widgets, since that would make
  widgets deeper in the hierarchy progressively more translucent. As a
  consequence, @class{gtk:popover} and other @class{gtk:native} widgets with
  their own surface will use their own opacity value, and thus by default
  appear non-translucent, even if they are attached to a toplevel widget that
  is translucent.
  @see-class{gtk:widget}
  @see-class{gtk:native}
  @see-class{gtk:popover}
  @see-function{gdk:display-is-composited}")

;;; --- gtk:widget-overflow  ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overflow" 'widget) t)
 "The @code{overflow} property of type @class{gtk:overflow} (Read / Write) @br{}
  How content outside the content area of the widget is treated. This property
  is meant to be set by widget implementations, typically in their instance init
  function.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-overflow)
      "Accessor"
      (documentation 'widget-overflow 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-overflow object) => overflow}
  @syntax{(setf (gtk:widget-overflow object) overflow)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[overflow]{a value of the @symbol{gtk:overflow} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:widget]{overflow} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-overflow} function returns the overflow value of the
  widget. The @setf{gtk:widget-overflow} function sets how the widget treats
  content that is drawn outside the content area of the widget. See the
  definition of the @symbol{gtk:overflow} enumeration for details.

  This setting is provided for widget implementations and should not be used by
  application code. The default value is @code{:visible}.
  @see-class{gtk:widget}
  @see-symbol{gtk:overflow}")

;;; --- gtk:widget-parent ------------------------------------------------------

;; TODO: Rework the implementation of gtk_widget_get_parent and
;; gtk_widget_set_parent functions.

#+liber-documentation
(setf (documentation (liber:slot-documentation "parent" 'widget) t)
 "The @code{parent} property of type @class{gtk:widget} (Read) @br{}
  The parent widget of this widget.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-parent)
      "Accessor"
      (documentation 'widget-parent 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-parent object) => parent}
  @argument[object]{a @class{gtk:widget} object}
  @argument[parent]{a parent @class{gtk:widget} object}
  @begin{short}
    Accessor of the @slot[gtk:widget]{parent} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-parent} function returns the parent widget of the widget.
  @begin[Notes]{dictionary}
    The @slot[gtk:widget]{parent} property is not writable. Use the
    @fun{gtk:widget-set-parent} function if you need to set a parent widget
    in a widget implementation.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-set-parent}")

;;; --- gtk:widget-receives-default --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "receives-default" 'widget) t)
 "The @code{receives-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the widget will receive the default action when it is focused. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-receives-default)
      "Accessor"
      (documentation 'widget-receives-default 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-receives-default object) => setting}
  @syntax{(setf (gtk:widget-receives-default object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget will receive the default
    action}
  @begin{short}
    Accessor of the @slot[gtk:widget]{receives-default} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-receives-default} function determines whether the widget
  is alyways treated as default widget within its toplevel when it has the
  focus, even if another widget is the default. The
  @setf{gtk:widget-receives-default} function specifies whether the widget will
  be treated as the default widget.
  @see-class{gtk:widget}")

;;; --- gtk:widget-root --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "root" 'widget) t)
 "The @code{root} property of type @class{gtk:root} (Read) @br{}
  The root widget of the widget tree containing this widget. This will be
  @code{nil} if the widget is not contained in a root widget.")

#+liber-documentation
(setf (liber:alias-for-function 'widget-root)
      "Accessor"
      (documentation 'widget-root 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-root object) => root}
  @argument[object]{a @class{gtk:widget} object}
  @argument[root]{a @class{gtk:root} root widget}
  @begin{short}
    The @fun{gtk:widget-root} function returns the @class{gtk:root} widget of
    the widget.
  @end{short}
  This function will return @code{nil} if the widget is not contained inside a
  widget tree with a root widget. The @class{gtk:root} widget will return
  itself here. See the @fun{gtk:window-destroy} function for an example
  of how this function can be used in a signal handler to get the toplevel
  window.
  @see-class{gtk:widget}
  @see-class{gtk:root}
  @see-function{gtk:window-destroy}")

;;; --- gtk:widget-scale-factor ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-factor" 'widget) t)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor of the widget. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-scale-factor)
      "Accessor"
      (documentation 'widget-scale-factor 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-scale-factor object) => scale}
  @argument[object]{a @class{gtk:widget} object}
  @argument[scale]{an integer for the scale factor}
  @begin{short}
    The @fun{gtk:widget-scale-factor} function retrieves the internal scale
    factor that maps from window coordinates to the actual device pixels.
  @end{short}
  On traditional systems this is 1, on high density outputs, it can be a higher
  value (typically 2).
  @see-class{gtk:widget}")

;;; --- gtk:widget-sensitive ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sensitive" 'widget) t)
 "The @code{sensitive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget responds to input. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-sensitive)
      "Accessor"
      (documentation 'widget-sensitive 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-sensitive object) => setting}
  @syntax{(setf (gtk:widget-sensitive object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether the widget responds to input}
  @begin{short}
    Accessor of the @slot[gtk:widget]{sensitive} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-sensitive} function returns the sensitivity of the widget.
  The @setf{gtk:widget-sensitive} function sets the sensitivity.

  A widget is sensitive if the user can interact with it. Insensitive widgets
  are \"grayed out\" and the user cannot interact with them. Insensitive widgets
  are known as \"inactive\", \"disabled\", or \"ghosted\" in some other
  toolkits.

  The effective sensitivity of a widget is however determined by both its own
  and its parent sensitivity of the widget. See the
  @fun{gtk:widget-is-sensitive} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-sensitive}")

;;; --- gtk:widget-tooltip-markup ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-markup" 'widget) t)
 "The @code{tooltip-markup} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string, which is marked up with
  the Pango text markup language. This is a convenience property which will take
  care of getting the tooltip shown if the given string is not @code{nil}. The
  @slot[gtk:widget]{has-tooltip} property will automatically be set to @em{true}
  and there will be taken care of the @code{\"query-tooltip\"} signal in the
  default signal handler. Note that if both the @slot[gtk:widget]{tooltip-text}
  and @slot[gtk:widget]{tooltip-markup} properties are set, the last one wins.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-tooltip-markup)
      "Accessor"
      (documentation 'widget-tooltip-markup 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-tooltip-markup object) => markup}
  @syntax{(setf (gtk:widget-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[markup]{a string for the text of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:widget]{tooltip-markup} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-tooltip-markup} function gets the contents of the tooltip.
  The @setf{gtk:widget-tooltip-markup} function sets @arg{markup} as the
  contents of the tooltip, which is marked up with the Pango text markup
  language.

  This function will take care of setting the @slot[gtk:widget]{has-tooltip}
  property to @em{true} and of the default handler for the
  @code{\"query-tooltip\"} signal.

  See also the @slot[gtk:widget]{tooltip-markup} property and the
  @fun{gtk:tooltip-set-markup} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-tooltip}
  @see-function{gtk:tooltip-set-markup}")

;;; --- gtk:widget-tooltip-text ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tooltip-text" 'widget) t)
 "The @code{tooltip-text} property of type @code{:string} (Read / Write) @br{}
  Sets the text of the tooltip to be the given string. This is a convenience
  property which will take care of getting the tooltip shown if the given string
  is not @code{nil}. The @slot[gtk:widget]{has-tooltip} property will
  automatically be set to @em{true} and there will be taken care of the
  @code{\"query-tooltip\"} signal in the default signal handler. Note that if
  both the @slot[gtk:widget]{tooltip-text} and @slot[gtk:widget]{tooltip-markup}
  properties are set, the last one wins. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-tooltip-text)
      "Accessor"
      (documentation 'widget-tooltip-text 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-tooltip-text object) => text}
  @syntax{(setf (gtk:widget-tooltip-text object) text)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[text]{a string for the text of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:widget]{tooltip-text} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-tooltip-text} function gets the contents of the tooltip.
  The @setf{gtk:widget-tooltip-text} function sets @arg{text} as the contents of
  the tooltip.

  This function will take care of setting the @slot[gtk:widget]{has-tooltip}
  property to @em{true} and of the default handler for the
  @code{\"query-tooltip\"} signal.

  See also the @slot[gtk:widget]{tooltip-text} property and the
  @fun{gtk:tooltip-set-text} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-tooltip}
  @see-function{gtk:tooltip-set-text}")

;;; --- gtk:widget-valign ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "valign" 'widget) t)
 "The @code{valign} property of type @symbol{gtk:align} (Read / Write) @br{}
  How to distribute vertical space if the widget gets extra space. @br{}
  Default value: @code{:fill}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-valign)
      "Accessor"
      (documentation 'widget-valign 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-valign object) => align}
  @syntax{(setf (gtk:widget-valign object) align)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[align]{a value of the @symbol{gtk:align} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:widget]{valign} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-valign} function gets the vertical alignment of the
  widget. The @setf{gtk:widget-valign} function sets the vertical alignment.
  @see-class{gtk:widget}
  @see-symbol{gtk:align}
  @see-function{gtk:widget-halign}")

;;; --- gtk:widget-vexpand -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vexpand" 'widget) t)
 "The @code{vexpand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand vertically. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-vexpand)
      "Accessor"
      (documentation 'widget-vexpand 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-vexpand object) => setting}
  @syntax{(setf (gtk:widget-vexpand object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to expand vertically}
  @begin{short}
    Accessor of the @slot[gtk:widget]{vexpand} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-vexpand} function gets whether the widget would like any
  available extra vertical space. The @setf{gtk:widget-vexpand} function sets
  whether the widget would like any available extra vertical space.

  See the @fun{gtk:widget-hexpand} function for more details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand-set}")

;;; --- gtk:widget-vexpand-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vexpand-set" 'widget) t)
 "The @code{vexpand-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use the @code{vexpand} property. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-vexpand-set)
      "Accessor"
      (documentation 'widget-vexpand-set 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-vexpand-set object) => setting}
  @syntax{(setf (gtk:widget-vexpand-set object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether to use the @slot[gtk:widget]{vexpand}
    property}
  @begin{short}
    Accessor of the @slot[gtk:widget]{vexpand-set} slot of the
    @class{gtk:widget} class.
  @end{short}
  The @fun{gtk:widget-vexpand-set} function gets whether the
  @setf{gtk:widget-vexpand} function has been used to explicitly set the expand
  flag on this widget. The @setf{gtk:widget-vexpand-set} function sets whether
  the @slot[gtk:widget]{vexpand} property will be used.

  See the @fun{gtk:widget-hexpand-set} function for more details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-vexpand}
  @see-function{gtk:widget-hexpand-set}")

;;; --- gtk:widget-visible -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'widget) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether the widget is visible. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'widget-visible)
      "Accessor"
      (documentation 'widget-visible 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-visible object) => setting}
  @syntax{(setf (gtk:widget-visible object) setting)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[setting]{a boolean whether @arg{widget} is visible}
  @begin{short}
    Accessor of the @slot[gtk:widget]{visible} slot of the @class{gtk:widget}
    class.
  @end{short}
  The @fun{gtk:widget-visible} function determines whether the widget is
  visible. The @setf{gtk:widget-visible} function sets the visibility state.
  Note that this does not take into account whether the parent of the widget is
  also visible or the widget is obscured in any way.

  If you want to take into account whether the parent of the widget is also
  marked as visible, use the @fun{gtk:widget-is-visible} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-is-visible}")

;;; --- gtk:widget-width-request -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-request" 'widget) t)
 "The @code{width-request} property of type @code{:int} (Read / Write) @br{}
  Override for width request of the widget, or -1 if natural request should be
  used. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'widget-width-request)
      "Accessor"
      (documentation 'widget-width-request 'function)
 "@version{2025-02-13}
  @syntax{(gtk:widget-width-request object) => width}
  @syntax{(setf (gtk:widget-width-request object) width)}
  @argument[object]{a @class{gtk:widget} object}
  @argument[width]{an integer for the width request}
  @begin{short}
    Accessor of the @slot[gtk:widget]{width-request} slot of the
    @class{gtk:widget} class.
  @end{short}
  See the @fun{gtk:widget-size-request} function for details.
  @see-class{gtk:widget}
  @see-function{gtk:widget-height-request}
  @see-function{gtk:widget-size-request}")

;;; ----------------------------------------------------------------------------
;;; gtk_widget_in_destruction
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_in_destruction" widget-in-destruction) :boolean
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is being destroyed.}
  @begin{short}
    Returns whether the widget is currently being destroyed.
  @end{short}
  This information can sometimes be used to avoid doing unnecessary work.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-in-destruction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_show
;;; ----------------------------------------------------------------------------

(declaim (inline widget-show))

(cffi:defcfun ("gtk_widget_show" %widget-show) :void
  (widget (g:object widget)))

(defun widget-show (widget)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags a widget to be displayed.
  @end{short}
  Any widget that is not shown will not appear on the screen. Remember that you
  have to show the containers containing a widget, in addition to the widget
  itself, before it will appear onscreen. When a toplevel container is shown,
  it is immediately realized and mapped. Other shown widgets are realized and
  mapped when their toplevel container is realized and mapped.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the @fun{gtk:widget-visible}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-visible}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-SHOW is deprecated since 4.10."))
  (%widget-show widget))

(export 'widget-show)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_hide
;;; ----------------------------------------------------------------------------

(declaim (inline widget-hide))

(cffi:defcfun ("gtk_widget_hide" %widget-hide) :void
  (widget (g:object widget)))

(defun widget-hide (widget)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Reverses the effects of the @fun{gtk:widget-show} function.
  @end{short}
  This is causing the widget to be hidden, so it is invisible to the user.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the @fun{gtk:widget-visible}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-visible}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-HIDE is deprecated since 4.10."))
  (%widget-hide widget))

(export 'widget-hide)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_map
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_map" widget-map) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be mapped if it is not already.
  @end{short}
  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-unmap}"
  (widget (g:object widget)))

(export 'widget-map)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unmap
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unmap" widget-unmap ) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be unmapped if it is currently mapped.
  @end{short}
  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-map}"
  (widget (g:object widget)))

(export 'widget-unmap)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_mapped
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_mapped" widget-mapped) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget is mapped, @em{false} otherwise.}
  @begin{short}
    This function determines whether the widget is mapped.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-map}"
  (widget (g:object widget)))

(export 'widget-mapped)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_realize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_realize" widget-realize) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Creates the GDK resources associated with a widget.
  @end{short}
  Normally realization happens implicitly. If you show a widget and all its
  parent containers, then the widget will be realized and mapped automatically.

  Realizing a widget requires all the parent widgets of the widget to be
  realized. Calling the @fun{gtk:widget-realize} function realizes the parents
  of the widget in addition to the widget itself. If a widget is not yet
  inside a toplevel window when you realize it, bad things will happen.

  This function is primarily used in widget implementations, and is not very
  useful otherwise. Many times when you think you might need it, a better
  approach is to connect to a signal that will be called after the widget is
  realized automatically, such as the @code{\"realize\"} signal.
  @see-class{gtk:widget}
  @see-function{gtk:widget-unrealize}"
  (widget (g:object widget)))

(export 'widget-realize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unrealize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unrealize" widget-unrealize) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Causes a widget to be unrealized.
  @end{short}
  Frees all GDK resources associated with the widget. This function is only
  useful in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-realize}"
  (widget (g:object widget)))

(export 'widget-unrealize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_realized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_realized" widget-realized) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget is realized, @em{false} otherwise.}
  @begin{short}
    This function determines whether the widget is realized.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-realize}"
  (widget (g:object widget)))

(export 'widget-realized)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_draw
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_draw" widget-queue-draw) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Schedules the widget to be redrawn in the paint phase of the current or the
    next frame.
  @end{short}
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-queue-draw)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_resize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_resize" widget-queue-resize) :void
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags a widget to have its size renegotiated.
  @end{short}
  Should be called when a widget for some reason has a new size request. For
  example, when you change the text in a label, the @class{gtk:label} widget
  queues a resize to ensure there is enough space for the new text.

  This function is only for use in widget implementations.
  @begin[Notes]{dictionary}
    You cannot call the @fun{gtk:widget-queue-resize} function on a widget from
    inside its implementation of the @code{size_allocate} virtual method. Calls
    to this function from inside the virtual method will be silently ignored.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:label}"
  (widget (g:object widget)))

(export 'widget-queue-resize)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_queue_allocate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_queue_allocate" widget-queue-allocate) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Flags the widget for a rerun of the @code{size_allocate ()} virtual
    function.
  @end{short}
  Use this function instead of the @fun{gtk:widget-queue-resize} function when
  the  size request of the widget did not change but it wants to reposition its
  contents. An example user of this function is the @fun{gtk:widget-halign}
  function.

  This function is only for use in widget implementations.
  @see-class{gtk:widget}
  @see-function{gtk:widget-queue-resize}
  @see-function{gtk:widget-halign}"
  (widget (g:object widget)))

(export 'widget-queue-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_frame_clock
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_frame_clock" widget-frame-clock)
    (g:object gdk-frame-clock)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    The @class{gdk:frame-clock} object, or @code{nil} if @arg{widget} is
    unrealized.
  @end{return}
  @begin{short}
    Obtains the frame clock for a widget.
  @end{short}
  The frame clock is a global \"ticker\" that can be used to drive animations
  and repaints. The most common reason to get the frame clock is to call the
  @fun{gdk:frame-clock-frame-time} function, in order to get a time to use for
  animating. For example you might record the start of the animation with an
  initial value from the @fun{gdk:frame-clock-frame-time} function, and then
  update the animation by calling the @fun{gdk:frame-clock-frame-time} function
  again during each repaint.

  The @fun{gdk:frame-clock-request-phase} function will result in a new frame
  on the clock, but will not necessarily repaint any widgets. To repaint a
  widget, you have to use the @fun{gtk:widget-queue-draw} function which
  invalidates the widget, thus scheduling it to receive a draw on the next
  frame. The @fun{gtk:widget-queue-draw} function will also end up requesting a
  frame on the appropriate frame clock.

  A frame clock of the widget will not change while the widget is mapped.
  Reparenting a widget, which implies a temporary unmap, can change the frame
  clock of the widget.

  Unrealized widgets do not have a frame clock.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gdk:frame-clock-frame-time}
  @see-function{gdk:frame-clock-request-phase}
  @see-function{gtk:widget-queue-draw}"
  (widget (g:object widget)))

(export 'widget-frame-clock)

;;; ----------------------------------------------------------------------------
;;; GtkTickCallback
;;; ----------------------------------------------------------------------------

(cffi:defcallback tick-callback :boolean
    ((widget (g:object widget))
     (clock (g:object gdk:frame-clock))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func widget clock)
      (return () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'tick-callback)
      "Callback"
      (liber:symbol-documentation 'tick-callback)
 "@version{2025-03-01}
  @syntax{lambda (widget clock) => result}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[clock]{a @class{gdk:frame-clock} object for the widget, same as
    calling the @fun{gtk:widget-frame-clock} function}
  @argument[result]{@em{true}, if the tick callback function should continue to
    be called, @em{false}, if the tick callback function should be removed}
  @begin{short}
    Callback type for adding a function to update animations.
  @end{short}
  See the @fun{gtk:widget-add-tick-callback} function.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gtk:widget-add-tick-callback}
  @see-function{gtk:widget-frame-clock}")

(export 'tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_tick_callback
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_tick_callback" %widget-add-tick-callback) :uint
  (widget (g:object widget))
  (func :pointer)
  (data :pointer)
  (notify :pointer))

(defun widget-add-tick-callback (widget func)
 #+liber-documentation
 "@version{2025-03-01}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[func]{a @symbol{gtk:tick-callback} callback function to call for
    updating animations}
  @begin{return}
    The unsigned integer ID for the connection of this callback function,
    remove the callback function by passing it to the
    @fun{gtk:widget-remove-tick-callback} function.
  @end{return}
  @begin{short}
    Queues an animation frame update and adds a callback function to be called
    before each frame.
  @end{short}
  Until the tick callback function is removed, it will be called frequently,
  usually at the frame rate of the output device or as quickly as the
  application can be repainted, whichever is slower. For this reason, it is most
  suitable for handling graphics that change every frame or every few frames.
  The tick callback function does not automatically imply a relayout or repaint.
  If you want a repaint or relayout, and are not changing widget properties that
  would trigger that, for example, changing the text of a @class{gtk:label}
  widget, then you will have to call the @fun{gtk:widget-queue-resize} function
  or the @fun{gtk:widget-queue-draw} function yourself.

  The @fun{gdk:frame-clock-frame-time} function should generally be used for
  timing continuous animations and the
  @fun{gdk:frame-timings-predicted-presentation-time} function if you are
  trying to display isolated frames at particular times.

  This is a more convenient alternative to connecting directly to the
  @code{\"update\"} signal of the @class{gdk:frame-clock} object, since you do
  not have to worry about when the @class{gdk:frame-clock} object is assigned
  to a widget.
  @see-class{gtk:widget}
  @see-class{gdk:frame-clock}
  @see-function{gtk:widget-remove-tick-callback}
  @see-function{gtk:widget-queue-resize}
  @see-function{gtk:widget-queue-draw}
  @see-function{gdk:frame-clock-frame-time}
  @see-function{gdk:frame-timings-predicted-presentation-time}"
  (%widget-add-tick-callback widget
          (cffi:callback tick-callback)
          (glib:allocate-stable-pointer func)
          (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'widget-add-tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_tick_callback
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_tick_callback" widget-remove-tick-callback)
    :void
 #+liber-documentation
 "@version{2025-03-01}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[id]{an unsigned integer for the ID returned by the
    @fun{gtk:widget-add-tick-callback} function}
  @begin{short}
    Removes a tick callback function previously registered with the
    @fun{gtk:widget-add-tick-callback} function.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-tick-callback}"
  (widget (g:object widget))
  (id :uint))

(export 'widget-remove-tick-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_size_allocate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_size_allocate" widget-size-allocate) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[allocation]{a @class{gdk:rectangle} instance for the position and
    size to be allocated to @arg{widget}}
  @argument[baseline]{an integer for the baseline of the child widget, or -1}
  @begin{short}
    Allocates the widget with a transformation that translates the origin to
    the position in @arg{allocation}.
  @end{short}
  This is a simple form of the @fun{gtk:widget-allocate} function.
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-allocate}"
  (widget (g:object widget))
  (allocation (g:boxed gdk:rectangle))
  (baseline :int))

(export 'widget-size-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_allocate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_allocate" widget-allocate) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[width]{an integer for the new width of the widget}
  @argument[height]{an integer for the new height of the widget}
  @argument[baseline]{an integer for the baseline of the widget, or -1}
  @argument[transform]{a @class{gsk:transform} object to be applied to the
    widget}
  @begin{short}
    This function is only used by @class{gtk:widget} subclasses, to assign a
    size, position and (optionally) baseline to their child widgets.
  @end{short}
  In this function, the allocation and baseline may be adjusted. The given
  allocation will be forced to be bigger than the minimum size of the widget,
  as well as at least 0×0 in size. For a version that does not take a transform,
  see the @fun{gtk:widget-size-allocate} function.
  @see-class{gtk:widget}
  @see-class{gsk:transform}
  @see-function{gtk:widget-size-allocate}"
  (widget (g:object widget))
  (width :int)
  (height :int)
  (baseline :int)
  (transform (g:boxed gsk:transform)))

(export 'widget-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_add_shortcut
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_add_shortcut" %widget-class-add-shortcut) :void
  (class :pointer)
  (shortcut (g:object shortcut)))

(defun widget-class-add-shortcut (gtype shortcut)
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[gtype]{a @class{g:type-t} type ID for the widget class to add the
    shortcut to}
  @argument[shortcut]{a @class{gtk:shortcut} object to add}
  @begin{short}
    Installs a shortcut for the widget class of @class{g:type-t} type.
  @end{short}
  Every instance created for the widget class or its subclasses will inherit
  this shortcut and trigger it.

  Shortcuts added this way will be triggered in the @code{:bubble} propagation
  phase, which means they may also trigger if child widgets have focus.

  This function must only be used in class initialization functions otherwise
  it is not guaranteed that the shortcut will be installed.
  @see-class{gtk:widget}
  @see-class{gtk:shortcut}
  @see-class{g:type-t}
  @see-symbol{gtk:propagation-phase}"
  (let ((wclass (g:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-add-shortcut wclass shortcut)
      (g:type-class-unref wclass))))

(export 'widget-class-add-shortcut)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_add_binding ()
;;;
;;; void
;;; gtk_widget_class_add_binding (GtkWidgetClass *widget_class,
;;;                               guint keyval,
;;;                               GdkModifierType mods,
;;;                               GtkShortcutFunc callback,
;;;                               const char *format_string,
;;;                               ...);
;;;
;;; Creates a new shortcut for widget_class that calls the given callback with
;;; arguments read according to format_string . The arguments and format string
;;; must be provided in the same way as with g_variant_new().
;;;
;;; This function is a convenience wrapper around
;;; gtk_widget_class_add_shortcut() and must be called during class
;;; initialization. It does not provide for user_data, if you need that, you
;;; will have to use gtk_widget_class_add_shortcut() with a custom shortcut.
;;;
;;; widget_class :
;;;     the class to add the binding to
;;;
;;; keyval :
;;;     key value of binding to install
;;;
;;; mods:
;;;     key modifier of binding to install
;;;
;;; callback :
;;;     the callback to call upon activation
;;;
;;; format_string :
;;;     GVariant format string for arguments or NULL for no arguments.
;;;
;;; ... :
;;;     arguments, as given by format string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_add_binding_signal ()
;;;
;;; void
;;; gtk_widget_class_add_binding_signal (GtkWidgetClass *widget_class,
;;;                                      guint keyval,
;;;                                      GdkModifierType mods,
;;;                                      const char *signal,
;;;                                      const char *format_string,
;;;                                      ...);
;;;
;;; Creates a new shortcut for widget_class that emits the given action signal
;;; with arguments read according to format_string . The arguments and format
;;; string must be provided in the same way as with g_variant_new().
;;;
;;; This function is a convenience wrapper around
;;; gtk_widget_class_add_shortcut() and must be called during class
;;; initialization.
;;;
;;; widget_class :
;;;     the class to add the binding to
;;;
;;; keyval :
;;;     key value of binding to install
;;;
;;; mods :
;;;     key modifier of binding to install
;;;
;;; signal :
;;;     the signal to execute
;;;
;;; format_string :
;;;     GVariant format string for arguments or NULL for no arguments.
;;;
;;; ... :
;;;     arguments, as given by format string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_add_binding_action ()
;;;
;;; void
;;; gtk_widget_class_add_binding_action (GtkWidgetClass *widget_class,
;;;                                      guint keyval,
;;;                                      GdkModifierType mods,
;;;                                      const char *action_name,
;;;                                      const char *format_string,
;;;                                      ...);
;;;
;;; Creates a new shortcut for widget_class that activates the given action_name
;;; with arguments read according to format_string . The arguments and format
;;; string must be provided in the same way as with g_variant_new().
;;;
;;; This function is a convenience wrapper around
;;; gtk_widget_class_add_shortcut() and must be called during class
;;; initialization.
;;;
;;; widget_class :
;;;     the class to add the binding to
;;;
;;; keyval :
;;;     key value of binding to install
;;;
;;; mods :
;;;     key modifier of binding to install
;;;
;;; action_name :
;;;     the action to activate
;;;
;;; format_string :
;;;     GVariant format string for arguments or NULL for no arguments.
;;;
;;; ... :
;;;     arguments, as given by format string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_get_layout_manager_type
;;; gtk_widget_class_set_layout_manager_type
;;; ----------------------------------------------------------------------------

(defun (setf widget-class-layout-manager-type) (value gtype)
  (let ((wclass (g:type-class-ref gtype)))
    (unwind-protect
      (progn
        (cffi:foreign-funcall "gtk_widget_class_set_layout_manager_type"
                              :pointer wclass
                              g:type-t value
                              :void)
        value)
      (g:type-class-unref wclass))))

(defun widget-class-layout-manager-type (gtype)
 #+liber-documentation
 "@version{2025-02-13}
  @syntax{(gtk:widget-class-layout-manager-type gtype) => value}
  @syntax{(setf (gtk:widget-class-layout-manager-type gtype) value)}
  @argument[gtype]{a @class{g:type-t} type ID for the widget class}
  @argument[value]{a @class{g:type-t} type ID for the object type that
    implements the @class{gtk:layout-manager} object for the widget class of
    @arg{gtype} type}
  @begin{short}
    The @fun{gtk:widget-class-layout-manager-type} function retrieves the type
    of the @class{gtk:layout-manager} object used by the widget of the given
    @arg{gtype} type.
  @end{short}
  The @fun{gtk:widget-class-layout-manager-type} function sets the type to be
  used for creating layout managers for widgets of @arg{gtype} type.

  This function should only be called from class init functions of widgets.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:widget-class-layout-manager-type \"GtkBox\")
=> #<GTYPE :name \"GtkBoxLayout\" :id 99175136080768>
(gtk:widget-class-layout-manager-type \"GtkButton\")
=> #<GTYPE :name \"GtkBinLayout\" :id 99175136080464>
(gtk:widget-class-layout-manager-type \"GtkWindow\")
=> NIL
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:layout-manager}
  @see-class{g:type-t}"
  (let ((wclass (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:foreign-funcall "gtk_widget_class_get_layout_manager_type"
                            :pointer wclass
                            g:type-t)
      (g:type-class-unref wclass))))

(export 'widget-class-layout-manager-type)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_activate_signal ()
;;;
;;; void
;;; gtk_widget_class_set_activate_signal (GtkWidgetClass *widget_class,
;;;                                       guint signal_id);
;;;
;;; Sets the GtkWidgetClass.activate_signal field with the given signal_id ; the
;;; signal will be emitted when calling gtk_widget_activate().
;;;
;;; The signal_id must have been registered with g_signal_new() or
;;; g_signal_newv() before calling this function.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; signal_id :
;;;     the id for the activate signal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_activate_signal_from_name ()
;;;
;;; void
;;; gtk_widget_class_set_activate_signal_from_name
;;;                                (GtkWidgetClass *widget_class,
;;;                                 const char *signal_name);
;;;
;;; Sets the GtkWidgetClass.activate_signal field with the signal id for the
;;; given signal_name ; the signal will be emitted when calling
;;; gtk_widget_activate().
;;;
;;; The signal_name of widget_type must have been registered with g_signal_new()
;;; or g_signal_newv() before calling this function.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; signal_name :
;;;     the name of the activate signal of widget_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_get_activate_signal ()
;;;
;;; guint
;;; gtk_widget_class_get_activate_signal (GtkWidgetClass *widget_class);
;;;
;;; Retrieves the signal id for the activation signal set using
;;; gtk_widget_class_set_activate_signal().
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; Returns :
;;;     a signal id, or 0 if the widget class does not specify an activation
;;;     signal
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_activate" widget-activate) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object that is activatable}
  @return{@em{True} if the widget was activatable.}
  @begin{short}
    For widgets that can be \"activated\", buttons, menu items, and so on, this
    function activates them.
  @end{short}
  Activation is what happens when you press the @kbd{Enter} key on a widget
  during key navigation. If the widget is not activatable, the function returns
  @em{false}.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_focus" widget-is-focus) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the widget is the focus widget.}
  @begin{short}
    Determines if the widget is the focus widget within its toplevel.
  @end{short}
  This does not mean that the @slot[gtk:widget]{has-focus} property is
  necessarily set. The @slot[gtk:widget]{has-focus} property will only be set
  if the toplevel widget additionally has the global input focus.
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-focus}"
  (widget (g:object widget)))

(export 'widget-is-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_grab_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_grab_focus" widget-grab-focus) :boolean
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if the focus is now inside @arg{widget}.}
  @begin{short}
    Causes the widget to have the keyboard focus for the @class{gtk:window}
    widget it is inside.
  @end{short}

  If the widget is not focusable, or its @code{grab_focus()} implementation
  cannot transfer the focus to a descendant of the widget that is focusable, it
  will not take focus and @em{false} will be returned.

  Calling the @fun{gtk:widget-grab-focus} function on an already focused widget
  is allowed, should not have an effect, and returns @em{true}.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-function{gtk:widget-can-focus}"
  (widget (g:object widget)))

(export 'widget-grab-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_parent" widget-get-parent) (g:object widget)
 #+liber-documentation
 "@version{2025-05-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The parent @class{gtk:widget} object for @arg{widget}.}
  @short{Returns the parent widget of the widget.}
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-get-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_parent
;;; ----------------------------------------------------------------------------

;; TODO: Rework the implementation of the gtk_widget_get_parent and
;; gtk_widget_set_parent functions. Do we need the gtk:widget-get-parent
;; function?!

(cffi:defcfun ("gtk_widget_set_parent" widget-set-parent) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[parent]{a parent @class{gtk:widget} object}
  @begin{short}
    Sets @arg{parent} as the parent widget of @arg{widget}.
  @end{short}
  This takes care of details such as updating the state and style of the child
  to reflect its new location and resizing the parent. The opposite function
  is the @fun{gtk:widget-unparent} function.

  This function is useful only when implementing subclasses of the
  @class{gtk:widget} object.
  @begin[Notes]{dictionary}
    The @slot[gtk:widget]{parent} property is not writable. You have to use
    this function to set the parent widget.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-unparent}"
  (widget (g:object widget))
  (parent (g:object widget)))

(export 'widget-set-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unparent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unparent" widget-unparent) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Dissociate the widget from its parent.
  @end{short}
  This function is only for use in widget implementations, typically in dispose.
  @see-class{gtk:widget}
  @see-function{gtk:widget-parent}"
  (widget (g:object widget)))

(export 'widget-unparent)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_native
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_native" widget-native) (g:object native)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:native} widget of @arg{widget}, or @code{nil}.}
  @begin{short}
    Returns the @class{gtk:native} widget that contains @arg{widget}, or
    @code{nil} if the widget is not contained inside a widget tree with a
    native ancestor.
  @end{short}
  The @class{gtk:native} widget will return itself here.
  @see-class{gtk:widget}
  @see-class{gtk:native}"
  (widget (g:object widget)))

(export 'widget-native)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_ancestor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_ancestor" widget-ancestor) (g:object widget)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[gtype]{an ancestor @class{g:type-t} type ID}
  @return{The @class{gtk:widget} ancestor widget, or @arg{nil} if not found.}
  @begin{short}
    Gets the first ancestor of the widget with type @arg{gtype}.
  @end{short}
  For example, the call @code{(gtk:widget-ancestor widget \"GtkBox\")} gets
  the first @class{gtk:box} widget that is an ancestor of the widget.

  Note that unlike the @fun{gtk:widget-is-ancestor} function, the
  @fun{gtk:widget-ancestor} function considers the widget to be an ancestor of
  itself.
  @see-class{gtk:widget}
  @see-class{g:type-t}
  @see-function{gtk:widget-is-ancestor}"
  (widget (g:object widget))
  (gtype g:type-t))

(export 'widget-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_ancestor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_ancestor" widget-is-ancestor) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[ancestor]{another @class{gtk:widget} object}
  @begin{return}
    @em{True} if @arg{ancestor} contains the widget as a child, grandchild,
    great grandchild, and so on.
  @end{return}
  @begin{short}
    Determines whether the widget is somewhere inside @arg{ancestor}, possibly
    with intermediate containers.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-ancestor}"
  (widget (g:object widget))
  (ancestor (g:object widget)))

(export 'widget-is-ancestor)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_translate_coordinates                        Deprecated 4.12
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_translate_coordinates" %widget-translate-coordinates)
    :boolean
  (src (g:object widget))
  (dst (g:object widget))
  (xsrc :int)
  (ysrc :int)
  (xdst (:pointer :int))
  (ydst (:pointer :int)))

(defun widget-translate-coordinates (src dst xsrc ysrc)
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-translate-coordinates src dest xsrc ysrc) => xdest, ydest}
  @argument[src]{a @class{gtk:widget} object}
  @argument[dest]{a @class{gtk:widget} object}
  @argument[xsrc]{an integer for the x position relative to @arg{src}}
  @argument[ysrc]{an integer for the y position relative to @arg{src}}
  @begin{return}
    @em{False} if either the widget was not realized, or there was no
    common ancestor. Otherwise the x position and the y position relative to
    @arg{dest}.
  @end{return}
  @begin{short}
    Translate coordinates relative to the allocation of @arg{src} to
    coordinates relative to the allocations of  @arg{dest}.
  @end{short}
  In order to perform this operation, both widgets must be realized, and must
  share a common toplevel.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:widget-compute-point} function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-compute-point}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TRANSLATE-COORDINATES is deprecated since 4.12."))
  (cffi:with-foreign-objects ((xdst :int) (ydst :int))
    (when (%widget-translate-coordinates src dst xsrc ysrc xdst ydst)
      (values (cffi:mem-ref xdst :int)
              (cffi:mem-ref ydst :int)))))

(export 'widget-translate-coordinates)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_controller
;;; ----------------------------------------------------------------------------

;; Note: The widget takes ownership of the data, and is responsible for
;; freeing it. Therefore we have to add a reference when passing the
;; controller instance to the function.

(cffi:defcfun ("gtk_widget_add_controller" %widget-add-controller) :void
  (widget (g:object widget))
  (controller (g:object event-controller)))

(defun widget-add-controller (widget controller)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[controller]{a @class{gtk:event-controller} object that has not
    added to a widget yet}
  @begin{short}
    Adds the event controller to the widget so that it will receive events.
  @end{short}
  You will usually want to call this function right after creating any kind of
  event controller.
  @see-class{gtk:widget}
  @see-class{gtk:event-controller}
  @see-function{gtk:widget-remove-controller}"
  (%widget-add-controller widget (g:object-ref controller)))

(export 'widget-add-controller)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_controller
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_controller" widget-remove-controller) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[controller]{a @class{gtk:event-controller} object}
  @begin{short}
    Removes the event controller from the widget, so that it does not process
    events anymore.
  @end{short}
  The event controller should not be used again.

  Widgets will remove all event controllers automatically when they are
  destroyed, there is normally no need to call this function.
  @see-class{gtk:widget}
  @see-class{gtk:event-controller}
  @see-function{gtk:widget-add-controller}"
  (widget (g:object widget))
  (controller (g:object event-controller)))

(export 'widget-remove-controller)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_direction
;;; gtk_widget_set_direction
;;; ----------------------------------------------------------------------------

(defun (setf widget-direction) (direction widget)
  (cffi:foreign-funcall "gtk_widget_set_direction"
                        (g:object widget) widget
                        text-direction direction
                        :void)
  direction)

(cffi:defcfun ("gtk_widget_get_direction" widget-direction) text-direction
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-direction widget) => direction}
  @syntax{(setf (gtk:widget-direction widget) direction)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration}
  @begin{short}
    The @fun{gtk:widget-direction} function gets the reading direction for the
    widget.
  @end{short}
  The @setf{gtk:widget-direction} function sets the reading direction.

  This direction controls the primary direction for widgets containing text,
  and also the direction in which the children of a container are packed. The
  ability to set the direction is present in order so that correct localization
  into languages with right-to-left reading directions can be done. Generally,
  applications will let the default reading direction present, except for
  containers where the containers are arranged in an order that is explicitely
  visual rather than logical, such as buttons for text justification.

  If the direction is set to the @code{:none} value, then the value set by the
  @fun{gtk:widget-default-direction} function will be used.
  @see-class{gtk:widget}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-default-direction}"
  (widget (g:object widget)))

(export 'widget-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_default_direction
;;; gtk_widget_set_default_direction
;;; ----------------------------------------------------------------------------

(defun (setf widget-default-direction) (direction)
  (cffi:foreign-funcall "gtk_widget_set_default_direction"
                        text-direction direction
                        :void)
  direction)

(cffi:defcfun ("gtk_widget_get_default_direction" widget-default-direction)
    text-direction
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-default-direction) => direction}
  @syntax{(setf (gtk:widget-default-direction) direction)}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration
    for the default direction, this cannot be @code{:none}.}
  @begin{short}
    The @fun{gtk:widget-default-direction} function obtains the current default
    reading direction.
  @end{short}
  The @setf{gtk:widget-default-direction} function sets the default reading
  direction for widgets where the direction has not been explicitly set by the
  @fun{gtk:widget-direction} function.
  @see-class{gtk:widget}
  @see-symbol{gtk:text-direction}
  @see-function{gtk:widget-direction}")

(export 'widget-default-direction)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_create_pango_context" widget-create-pango-context)
    (g:object pango:context :return)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The new @class{pango:context} object.}
  @begin{short}
    Creates a new Pango context with the appropriate font map, font description,
    and base direction for drawing text for this widget.
  @end{short}
  See also the @fun{gtk:widget-pango-context} function.
  @see-class{gtk:widget}
  @see-class{pango:context}
  @see-function{gtk:widget-pango-context}"
  (widget (g:object widget)))

(export 'widget-create-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_pango_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_pango_context" widget-pango-context)
    (g:object pango:context)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{pango:context} object for the widget.}
  @begin{short}
    Gets a Pango context with the appropriate font map, font description, and
    base direction for this widget.
  @end{short}
  Unlike the Pango context returned by the @fun{gtk:widget-create-pango-context}
  function, this Pango context is owned by the widget, it can be used until the
  screen for the widget changes or the widget is removed from its toplevel, and
  will be updated to match any changes to the  attributes of the widget.

  If you create and keep a @class{pango:layout} object using this Pango context,
  you must deal with changes to the Pango context by calling the
  @fun{pango:layout-context-changed} function on the Pango layout in response to
  the @code{\"style-updated\"} and @code{\"direction-changed\"} signals for the
  widget.
  @see-class{gtk:widget}
  @see-class{pango:layout}
  @see-function{gtk:widget-create-pango-context}
  @see-function{pango:layout-context-changed}"
  (widget (g:object widget)))

(export 'widget-pango-context)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_font_options
;;; gtk_widget_set_font_options
;;; ----------------------------------------------------------------------------

(defun (setf widget-font-options) (options widget)
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-FONT-OPTIONS is deprecated since 4.16."))
  (let ((options1 (or options (cffi:null-pointer))))
    (cffi:foreign-funcall "gtk_widget_set_font_options"
                          (g:object widget) widget
                          (:pointer (:struct cairo:font-options-t)) options1
                          :void)
    options))

(cffi:defcfun ("gtk_widget_get_font_options" %widget-font-options)
    (:pointer (:struct cairo:font-options-t))
  (widget (g:object widget)))

(defun widget-font-options (widget)
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-font-options widget) => options}
  @syntax{(setf (gtk:widget-font-options widget) options)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[options]{a @symbol{cairo:font-options-t} instance, or @code{nil} to
    unset any previously set default font options}
  @begin{short}
    The @fun{gtk:widget-font-options} function returns the font options used
    for Pango rendering.
  @end{short}
  The @setf{gtk:widget-font-options} function sets the font options. When not
  set, the default font options for the GDK screen will be used.
  @begin[Warning]{dictionary}
    Deprecated since 4.16. Do not use this function in newly written code.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:screen}
  @see-symbol{cairo:font-options-t}"
  #+(and gtk-4-16 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-FONT-OPTIONS is deprecated since 4.16."))
  (let ((options (%widget-font-options widget)))
    (unless (cffi:null-pointer-p options)
      options)))

(export 'widget-font-options)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_font_map
;;; gtk_widget_set_font_map
;;; ----------------------------------------------------------------------------

(defun (setf widget-font-map) (fontmap widget)
  (cffi:foreign-funcall "gtk_widget_set_font_map"
                        (g:object widget) widget
                        (g:object pango:font-map) fontmap
                        :void)
  fontmap)

(cffi:defcfun ("gtk_widget_get_font_map" widget-font-map)
    (g:object pango:font-map)
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-font-map widget) => fontmap}
  @syntax{(setf (gtk:widget-font-map widget) fontmap)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[fontmap]{a @class{pango:font-map} object, or @code{nil} to unset
    any previously set font map}
  @begin{short}
    The @fun{gtk:widget-font-map} function gets the font map that has been set.
  @end{short}
  The @setf{gtk:widget-font-map} function sets the font map to use for Pango
  rendering. When not set, the widget will inherit the font map from its parent.
  @see-class{gtk:widget}
  @see-class{pango:font-map}"
  (widget (g:object widget)))

(export 'widget-font-map)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_create_pango_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_create_pango_layout" %widget-create-pango-layout)
    (g:object pango:layout :return)
  (widget (g:object widget))
  (text :string))

(defun widget-create-pango-layout (widget text)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[text]{a string for the text to set on the Pango layout, can be
    @code{nil}}
  @return{The new @class{pango:layout} object.}
  @begin{short}
    Creates a new @class{pango:layout} object with the appropriate font map,
    font description, and base direction for drawing text for this widget.
  @end{short}

  If you keep a @class{pango:layout} object created in this way around, in order
  to notify the layout of changes to the base direction or font of this widget,
  you must call the @fun{pango:layout-context-changed} function in response to
  the @code{\"style-updated\"} and @code{\"direction-changed\"} signals for the
  widget.
  @see-class{gtk:widget}
  @see-class{pango:layout}
  @see-function{pango:layout-context-changed}"
  (%widget-create-pango-layout widget
                               (or text (cffi:null-pointer))))

(export 'widget-create-pango-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_set_cursor_from_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_set_cursor_from_name" %widget-set-cursor-from-name)
    :void
  (widget (g:object widget))
  (name :string))

(defun widget-set-cursor-from-name (widget name)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[name]{a string for the name of the cursor or @code{nil} to use
    the default cursor}
  @begin{short}
    Sets a named cursor to be shown when pointer devices point towards
    @arg{widget}.
  @end{short}
  This is a utility function that creates a cursor via the
  @fun{gdk:cursor-new-from-name} function and then sets it on @arg{widget} with
  the @fun{gtk:widget-cursor} function. See those two functions for details.

  On top of that, this function allows @arg{name} to be @code{nil}, which will
  do the same as calling the @fun{gtk:widget-cursor} function with a @code{nil}
  cursor.
  @see-class{gtk:widget}
  @see-function{gdk:cursor-new-from-name}
  @see-function{gtk:widget-cursor}"
  (%widget-set-cursor-from-name widget
                                (or name (cffi:null-pointer))))

(export 'widget-set-cursor-from-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_get_accessible_role
;;; gtk_widget_class_set_accessible_role
;;; ----------------------------------------------------------------------------

(defun (setf widget-class-accessible-role) (role gtype)
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:foreign-funcall "gtk_widget_class_set_accessible_role"
                            :pointer class
                            accessible-role role
                            :void)
      (g:type-class-unref class))
    role))

(cffi:defcfun ("gtk_widget_class_get_accessible_role"
               %widget-class-accessible-role) accessible-role
  (class :pointer))

(defun widget-class-accessible-role (gtype)
  #+liber-documentation
 "@version{2025-02-13}
  @syntax{(gtk:widget-class-accessible-role gtype) => role}
  @syntax{(setf (gtk:widget-class-accessible-role gtype) role)}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[role]{a @symbol{gtk:accessible-role} value}
  @begin{short}
    The @fun{gtk:widget-class-accessible-role} function gets the accessible role
    used by the given widget class.
  @end{short}
  The @setf{gtk:widget-class-accessible-role} function sets the accessible role.
  Different accessible roles have different states, and are rendered differently
  by assistive technologies.
  @see-class{gtk:widget}
  @see-symbol{gtk:accessible-role}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-accessible-role class)
      (g:type-class-unref class))))

(export 'widget-class-accessible-role)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_child_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_child_focus" widget-child-focus) :boolean
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a @symbol{gtk:direction-type} value for the direction
    of focus movement}
  @return{@em{True} if the focus ended up inside @arg{widget}.}
  @begin{short}
    Called by widgets as the user moves around the window using keyboard
    shortcuts.
  @end{short}
  The @arg{direction} argument indicates what kind of motion is taking place
  (up, down, left, right, tab forward, tab backward).

  This function calls the @code{Gtk.WidgetClass.focus()} virtual function.
  Widgets can override the virtual function in order to implement appropriate
  focus behavior.

  The default @code{Gtk.WidgetClass.focus()} virtual function for a widget
  should return @em{true} if moving in @arg{direction} left the focus on a
  focusable location inside that widget, and @em{false} if moving in
  @arg{direction} moved the focus outside the widget. When returning @em{true},
  widgets normally call the @fun{gtk:widget-grab-focus} function to place the
  focus accordingly. When returning @em{false}, they do not modify the current
  focus location.

  This function is used by custom widget implementations. If you are writing an
  application, you would use the @fun{gtk:widget-grab-focus} function to move
  the focus to a particular widget.
  @see-class{gtk:widget}
  @see-symbol{gtk:direction-type}
  @see-function{gtk:widget-grab-focus}"
  (widget (g:object widget))
  (direction direction-type))

(export 'widget-child-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_child_visible
;;; gtk_widget_set_child_visible
;;; ----------------------------------------------------------------------------

(defun (setf widget-child-visible) (visible widget)
  (cffi:foreign-funcall "gtk_widget_set_child_visible"
                        (g:object widget) widget
                        :boolean visible
                        :void)
  visible)

(cffi:defcfun ("gtk_widget_get_child_visible" widget-child-visible) :boolean
 #+liber-documentation
 "@version{#2025-02-13}
  @syntax{(gtk:widget-child-visible widget) => visible}
  @syntax{(setf (gtk:widget-child-visible widget) visible)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[visible]{if @em{true}, @arg{widget} should be mapped along with its
    parent}
  @begin{short}
    The @fun{gtk:widget-child-visible} function returns @em{true} if the widget
    is mapped with the parent.
  @end{short}
  The @setf{gtk:widget-child-visible} function sets whether the widget should be
  mapped along with its parent when its parent is mapped and the widget has been
  shown with the @fun{gtk:widget-show} function.

  The child visibility can be set for the widget before it is added to a
  container with the @fun{gtk:widget-parent} function, to avoid mapping children
  unnecessary before immediately unmapping them. However it will be reset to its
  default state of @em{true} when the widget is removed from a container.

  Note that changing the child visibility of a widget does not queue a resize
  on the widget. Most of the time, the size of a widget is computed from all
  visible children, whether or not they are mapped. If this is not the case,
  the container can queue a resize itself.

  This function is only useful for container implementations and never should
  be called by an application.
  @see-class{gtk:widget}
  @see-function{gtk:widget-show}
  @see-function{gtk:widget-parent}"
  (widget (g:object widget)))

(export 'widget-child-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_settings
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_settings" widget-settings) (g:object settings)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The relevant @class{gtk:settings} object.}
  @begin{short}
    Gets the settings object holding the settings used for this widget.
  @end{short}
  Note that this function can only be called when the widget is attached to a
  toplevel, since the settings object is specific to a particular
  @class{gdk:display} object. If you want to monitor the widget for changes in
  its settings, connect to the @code{notify::display} signal.
  @see-class{gtk:widget}
  @see-class{gtk:settings}
  @see-class{gdk:display}"
  (widget (g:object widget)))

(export 'widget-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_clipboard" widget-clipboard)
    (g:object gdk-clipboard)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The appropriate @class{gdk:clipboard} object.}
  @begin{short}
    Gets the clipboard object for the widget.
  @end{short}
  This is an utility function to get the clipboard object for the
  @class{gdk:display} object that the widget is using. Note that this function
  always works, even when the widget is not realized yet.
  @see-class{gtk:widget}
  @see-class{gdk:clipboard}
  @see-class{gdk:display}
  @see-function{gdk:display-clipboard}"
  (widget (g:object widget)))

(export 'widget-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_primary_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_primary_clipboard" widget-primary-clipboard)
    (g:object gdk:clipboard)
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    The @class{gdk:clipboard} object with the appropriate clipboard object.
  @end{return}
  @begin{short}
    This is a utility function to get the primary clipboard object for the
    @class{gdk:display} object that @arg{widget} is using.
  @end{short}
  Note that this function always works, even when the widget is not realized
  yet.
  @see-class{gtk:widget}
  @see-class{gdk:clipboard}
  @see-class{gdk:display}"
  (widget (g:object widget)))

(export 'widget-primary-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_display" widget-display) (g:object gdk:display)
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:display} object for the toplevel of this widget.}
  @begin{short}
    Gets the display for the toplevel window associated with the widget.
  @end{short}
  This function can only be called after the widget has been added to a widget
  hierarchy with a @class{gtk:window} widget at the top. In general, you should
  only create display specific resources when a widget has been realized.
  @see-class{gtk:widget}
  @see-class{gtk:window}
  @see-class{gdk:display}"
  (widget (g:object widget)))

(export 'widget-display)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_size_request
;;; gtk_widget_set_size_request
;;; ----------------------------------------------------------------------------

(defun (setf widget-size-request) (size widget)
  (destructuring-bind (width height) size
    (cffi:foreign-funcall "gtk_widget_set_size_request"
                          (g:object widget) widget
                          :int width
                          :int height
                          :void)
    (values width height)))

(cffi:defcfun ("gtk_widget_get_size_request" %widget-size-request) :void
  (widget (g:object widget))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun widget-size-request (widget)
 #+liber-documentation
 "@version{2025-02-13}
  @syntax{(gtk:widget-size-request object) => width, height}
  @syntax{(setf (gtk:widget-size-request object) (list width height))}
  @argument[object]{a @class{gtk:widget} object}
  @argument[width]{an integer for the width}
  @argument[height]{an integer for the height}
  @begin{short}
    The @fun{gtk:widget-size-request} function gets the size request of the
    widget.
  @end{short}
  A value of -1 returned in @arg{width} or @arg{height} indicates that that
  dimension has not been set explicitly and the natural requisition of the
  widget will be used instead. To get the size a widget will actually request,
  call the @fun{gtk:widget-measure} function instead of this function.

  The @setf{gtk:widget-size-request} function sets the minimum size of a widget.
  That is, the size request of the widget will be @arg{width} by @arg{height}.
  You can use this function to force a widget to be either larger or smaller
  than it normally would be.

  In most cases, the @fun{gtk:window-default-size} function is a better choice
  for toplevel windows than this function. Setting the default size will still
  allow users to shrink the window. Setting the size request will force them to
  leave the window at least as large as the size request.

  Note the inherent danger of setting any fixed size - themes, translations
  into other languages, different fonts, and user action can all change the
  appropriate size for a given widget. So, it is basically impossible to
  hardcode a size that will always be correct.

  The size request of a widget is the smallest size a widget can accept while
  still functioning well and drawing itself correctly. However in some strange
  cases a widget may be allocated less than its requested size, and in many
  cases a widget may be allocated more space than it requested.

  If the size request in a given direction is -1 (unset), then the \"natural\"
  size request of the widget will be used instead.

  The size request set here does not include any margin from the
  @slot[gtk:widget]{margin-start}, @slot[gtk:widget]{margin-end},
  @slot[gtk:widget]{margin-top}, and @slot[gtk:widget]{margin-bottom}
  properties, but it does include pretty much all other padding or border
  properties set by any subclass of the @class{gtk:widget} class.
  @see-class{gtk:widget}
  @see-function{gtk:widget-measure}
  @see-function{gtk:window-default-size}
  @see-function{gtk:widget-margin-start}
  @see-function{gtk:widget-margin-end}
  @see-function{gtk:widget-margin-top}
  @see-function{gtk:widget-margin-bottom}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%widget-size-request widget width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'widget-size-request)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_list_mnemonic_labels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_list_mnemonic_labels" widget-list-mnemonic-labels)
    (g:list-t (g:object widget))
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The list of @class{gtk:widget} objects with the mnemonic labels.}
  @begin{short}
    Returns a list of the widgets, normally labels, for which this widget is
    the target of a mnemonic.
  @end{short}
  See for example the @fun{gtk:label-mnemonic-widget} function for more
  information about mnemonic labels.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq button (gtk:button-new-with-mnemonic \"_Hello\"))
=> #<GTK-BUTTON {C2794C9@}>
(gtk:widget-list-mnemonic-labels button)
=> (#<GTK-LABEL {C292FE1@}>)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-mnemonic-label}
  @see-function{gtk:label-mnemonic-widget}"
  (widget (g:object widget)))

(export 'widget-list-mnemonic-labels)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_mnemonic_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_mnemonic_label" widget-add-mnemonic-label) :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[label]{a @class{gtk:widget} object that acts as a mnemonic label
    for @arg{widget}}
  @begin{short}
    Adds a widget to the list of mnemonic labels for this widget.
  @end{short}
  See the @fun{gtk:widget-list-mnemonic-labels} function for a list of mnemonic
  labels for this widget.

  Note the list of mnemonic labels for the widget is cleared when the widget is
  destroyed, so the caller must make sure to update its internal state at this
  point as well, by using a connection to the @code{\"destroy\"} signal or a
  weak notifier.
  @see-class{gtk:widget}
  @see-function{gtk:widget-list-mnemonic-labels}
  @see-function{gtk:widget-remove-mnemonic-label}"
  (widget (g:object widget))
  (label (g:object widget)))

(export 'widget-add-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_mnemonic_label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_mnemonic_label" widget-remove-mnemonic-label)
    :void
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[label]{a @class{gtk:widget} object that was previously set as a
    mnemonic label for @arg{widget}}
  @begin{short}
    Removes a widget from the list of mnemonic labels for this widget.
  @end{short}
  See the @fun{gtk:widget-list-mnemonic-labels} function for a list of mnemonic
  labels for the widget. The widget must have previously been added to the list
  with the @fun{gtk:widget-add-mnemonic-label} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-mnemonic-label}
  @see-function{gtk:widget-list-mnemonic-labels}"
  (widget (g:object widget))
  (label (g:object widget)))

(export 'widget-remove-mnemonic-label)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_mnemonic_activate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_mnemonic_activate" widget-mnemonic-activate) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[cycling]{@em{true} if there are other widgets with the same
    mnemonic}
  @return{@em{True} if the signal has been handled.}
  @begin{short}
    Emits the @code{\"mnemonic-activate\"} signal.
  @end{short}
  The default handler for this signal activates the widget if @arg{cycling} is
  @em{false}, and just grabs the focus if @arg{cycling} is @em{true}.
  @see-class{gtk:widget}"
  (widget (g:object widget))
  (cycling :boolean))

(export 'widget-mnemonic-activate)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_error_bell
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_error_bell" widget-error-bell) :void
 #+liber-documentation
 "@version{2025-02-03}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Notifies the user about an input-related error on this widget.
  @end{short}
  If the @slot[gtk:settings]{gtk-error-bell} setting is @em{true}, it calls the
  @fun{gdk:surface-beep} function, otherwise it does nothing.

  Note that the effect of the @fun{gdk:surface-beep} function can be configured
  in many ways, depending on the windowing backend and the desktop environment
  or window manager that is used.
  @see-class{gtk:widget}
  @see-function{gdk:surface-beep}
  @see-function{gtk:settings-gtk-error-bell}"
  (widget (g:object widget)))

(export 'widget-error-bell)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_keynav_failed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_keynav_failed" widget-keynav-failed) :boolean
 #+liber-documentation
 "@version{2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[direction]{a @symbol{gtk:direction-type} value for the direction
    of focus movement}
  @begin{return}
    @em{True} if stopping keyboard navigation is fine, @em{false} if the
    emitting widget should try to handle the keyboard navigation attempt in its
    parent container(s).
  @end{return}
  @begin{short}
    Emits the @code{\"notify::keynav-failed\"} signal on the widget.
  @end{short}
  This function should be called whenever keyboard navigation within a single
  widget hits a boundary.

  The return value of this function should be interpreted in a way similar to
  the return value of @fun{gtk:widget-child-focus} function. When @em{true} is
  returned, stay in the widget, the failed keyboard navigation is OK and/or
  there is nowhere we can/should move the focus to. When @em{false} is returned,
  the caller should continue with keyboard navigation outside the widget, for
  example, by calling the @fun{gtk:widget-child-focus} function on the toplevel
  of the widget.

  The default @code{\"keynav-failed\"} signal handler returns @em{false} for
  the @code{:tab-forward} and @code{:tab-backward} values of the
  @symbol{gtk:direction-type} enumeration. For the other values of the
  @symbol{gtk:direction-type} enumeration it returns @em{true}.

  Whenever the default handler returns @em{true}, it also calls
  the @fun{gtk:widget-error-bell} function to notify the user of the failed
  keyboard navigation.

  A use case for providing an own implementation of the @code{\"keynav-failed\"}
  signal handler (either by connecting to it or by overriding it) would be a row
  of @class{gtk:entry} widgets where the user should be able to navigate the
  entire row with the cursor keys, as for example, known from user interfaces
  that require entering license keys.
  @see-class{gtk:widget}
  @see-class{gtk:entry}
  @see-symbol{gtk:direction-type}
  @see-function{gtk:widget-child-focus}
  @see-function{gtk:widget-error-bell}"
  (widget (g:object widget))
  (direction direction-type))

(export 'widget-keynav-failed)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_trigger_tooltip_query
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_trigger_tooltip_query" widget-trigger-tooltip-query)
    :void
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Triggers a tooltip query on the display where the toplevel of @arg{widget}
    is located.
  @end{short}
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-trigger-tooltip-query)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_width" %widget-allocated-width) :int
  (widget (g:object widget)))

(defun widget-allocated-width (widget)
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the width of the widget.}
  @begin{short}
    Returns the width that has currently been allocated to @arg{widget}.
  @end{short}
  @begin[Notes]{dictionary}
    The @fun{gtk:widget-allocated-width} function is equivalent to the call
    @begin{pre}
(gdk:rectangle-width (gtk:widget-allocation widget))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @fun{gtk:widget-width}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-allocated-height}
  @see-function{gtk:widget-allocation}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-ALLOCATED-WIDTH is deprecated since 4.12."))
  (%widget-allocated-width widget))

(export 'widget-allocated-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_height" %widget-allocated-height) :int
  (widget (g:object widget)))

(defun widget-allocated-height (widget)
 #+liber-documentation
 "@version{#2025-02-13}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the height of the widget.}
  @begin{short}
    Returns the height that has currently been allocated to @arg{widget}.
  @end{short}
  @begin[Notes]{dictionary}
    The @fun{gtk:widget-allocated-height} function is equivalent to the call
    @begin{pre}
(gdk:rectangle-height (gtk:widget-allocation widget))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @fun{gtk:widget-height}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-allocated-width}
  @see-function{gtk:widget-allocation}
  @see-function{gtk:widget-height}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-ALLOCATED-HEIGHT is deprecated since 4.12."))
  (%widget-allocated-height widget))

(export 'widget-allocated-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocation" %widget-allocation) :void
  (widget (g:object widget))
  (allocation (g:boxed gdk:rectangle)))

(defun widget-allocation (widget)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:rectangle} instance with the allocation.}
  @begin{short}
    Retrieves the allocation of the widget.
  @end{short}
  Note, when implementing a layout container: an allocation of the widget will
  be its \"adjusted\" allocation, that is, the parent of the widget typically
  calls the @fun{gtk:widget-size-allocate} function with an allocation, and that
  allocation is then adjusted (to handle margin and alignment for example)
  before assignment to the widget. The @fun{gtk:widget-allocation} function
  returns the adjusted allocation that was actually assigned to the widget. The
  adjusted allocation is guaranteed to be completely contained within the
  @fun{gtk:widget-size-allocate} allocation, however.

  So a layout container is guaranteed that its children stay inside the assigned
  bounds, but not that they have exactly the bounds the container assigned.
  @begin[Notes]{dictionary}
    In the Lisp binding to GTK this function does not return an allocation
    of type @code{GtkAllocation}, but a @class{gdk:rectangle} instance. In the
    C implementation the @code{GtkAllocation} type is a synonym for the
    @class{gdk:rectangle} type.
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:widget-compute-bounds}, @fun{gtk:widget-width}, or
    @fun{gtk:widget-height} functions instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gdk:rectangle}
  @see-function{gtk:widget-size-allocate}
  @see-function{gtk:widget-compute-bounds}
  @see-function{gtk:widget-width}
  @see-function{gtk:widget-height}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-ALLOCATION is deprecated since 4.12."))
  (let ((allocation (gdk:rectangle-new)))
    (%widget-allocation widget allocation)
    allocation))

(export 'widget-allocation)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_allocated_baseline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_allocated_baseline" %widget-allocated-baseline)
    :int
  (widget (g:object widget)))

(defun widget-allocated-baseline (widget)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object to query}
  @return{The integer with the baseline of the widget, or -1 if none.}
  @begin{short}
    Returns the baseline that has currently been allocated to the widget.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the @fun{gtk:widget-baseline}
    function instead.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-baseline}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-ALLOCATED-BASELINE is deprecated since 4.12."))
  (%widget-allocated-baseline widget))

(export 'widget-allocated-baseline)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_width" widget-width) :int
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The integer with the width of @arg{widget}.}
  @begin{short}
    Returns the content width of the widget, as passed to its size allocate
    implementation.
  @end{short}
  This is the size you should be using in the @code{GtkWidgetClass.snapshot()}
  virtual function. For pointer events, see the @fun{gtk:widget-contains}
  function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-contains}
  @see-function{gtk:widget-height}"
  (widget (g:object widget)))

(export 'widget-width)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_height" widget-height) :int
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The integer with the height of @arg{widget}.}
  @begin{short}
    Returns the content width of the widget, as passed to its size allocate
    implementation.
  @end{short}
  This is the size you should be using in the @code{GtkWidgetClass.snapshot()}
  virtual function. For pointer events, see the @fun{gtk:widget-contains}
  function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-contains}
  @see-function{gtk:widget-width}"
  (widget (g:object widget)))

(export 'widget-height)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_size" widget-size) :int
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[orientation]{a @symbol{gtk:orientation} value for the orientation
    to query}
  @begin{return}
    The integer with the size of the widget in the given @arg{orientation}.
  @end{return}
  @begin{short}
    Returns the content width or height of the widget, depending on the given
    orientation.
  @end{short}
  This is equivalent to calling the @fun{gtk:widget-width} function for the
  @code{:horizontal} value or the @fun{gtk:widget-height} function for the
  @code{:vertical} value, but can be used when writing orientation independent
  code, such as when implementing @class{gtk:orientable} widgets.
  @see-class{gtk:widget}
  @see-class{gtk:orientable}
  @see-symbol{gtk:orientation}
  @see-function{gtk:widget-width}
  @see-function{gtk:widget-height}"
  (widget (g:object widget))
  (orientation orientation))

(export 'widget-size)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_baseline                                 Since 4.12
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_widget_get_baseline" widget-baseline) :int
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The integer with the baseline of @arg{widget}, or -1 if none.}
  @begin{short}
    Returns the baseline that has currently been allocated to the widget.
  @end{short}
  This function is intended to be used when implementing handlers for the
  @code{GtkWidgetClass.snapshot()} virtual function, and when allocating child
  widgets in the @code{GtkWidgetClass.size_allocate()} virtual function.

  Since 4.12
  @see-class{gtk:widget}"
  (widget (g:object widget)))

#+gtk-4-12
(export 'widget-baseline)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_compute_bounds" %widget-compute-bounds) :boolean
  (widget (g:object widget))
  (target (g:object widget))
  (bounds (:pointer (:struct graphene:rect-t))))

(defun widget-compute-bounds (widget target bounds)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[target]{a @class{gtk:widget} target widget}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the result}
  @return{The @symbol{graphene:rect-t} instance with the bounds, or @code{nil}.}
  @begin{short}
    Computes the bounds for @arg{widget} in the coordinate space of
    @arg{target}.
  @end{short}
  The bounds of the widget are the bounding box of the region that it is
  expected to draw in. See the coordinate system overview to learn more.

  If the operation is successful, @arg{bounds} is returned. If @arg{widget} has
  no bounds or the bounds cannot be expressed in the coordinate space of
  @arg{target}, for example, if both widgets are in different windows,
  @code{nil} is returned and @arg{bounds} is set to the zero rectangle.

  It is valid for @arg{widget} and @arg{target} to be the same widget.
  @see-class{gtk:widget}
  @see-symbol{graphene:rect-t}"
  (when (%widget-compute-bounds widget target bounds)
    bounds))

(export 'widget-compute-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_compute_transform" %widget-compute-transform)
    :boolean
  (widget (g:object widget))
  (target (g:object widget))
  (transform (:pointer (:struct graphene:matrix-t))))

(defun widget-compute-transform (widget target transform)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[target]{a @class{gtk:widget} object for the target widget that the
    matrix will transform to}
  @argument[transform]{a @symbol{graphene:matrix-t} instance for the final
    transformation}
  @begin{return}
    The @symbol{graphene:matrix-t} instance with the transformation, or
    @code{nil} if transform could not be computed, for example, when
    @arg{widget} and @arg{target} do not share a common ancestor. In that case
    @arg{transform} gets set to the identity matrix.
  @end{return}
  @begin{short}
    Computes a matrix suitable to describe a transformation from the coordinate
    system of @arg{widget} into the coordinate system of @arg{target}.
  @end{short}
  @see-class{gtk:widget}
  @see-symbol{graphene:matrix-t}"
  (when (%widget-compute-transform widget target transform)
    transform))

(export 'widget-compute-transform)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_compute_point" %widget-compute-point) :boolean
  (widget (g:object widget))
  (target (g:object widget))
  (point (:pointer (:struct graphene:point-t)))
  (out (:pointer (:struct graphene:point-t))))

(defun widget-compute-point (widget target point out)
  #+liber-documentation
  "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object to query}
  @argument[target]{a @class{gtk:widget} object to transform into}
  @argument[point]{a @symbol{graphene:point-t} instance for a point in the
    coordinate sytem of @arg{widget}}
  @argument[out]{a @symbol{graphene:point-t} instance for the corresponding
    coordinates in the coordinate system of @arg{target}}
  @begin{return}
    The @symbol{graphene:point-t} instance if the point could be determined,
    @code{nil} on failure, in this case, @arg{out} is set to @code{(0,0)}.
  @end{return}
  @begin{short}
    Translates the given @arg{point} in the coordinates of @arg{widget} to
    coordinates relative to the coordinate system of @arg{target}.
  @end{short}
  In order to perform this operation, both widgets must share a common root.
  @see-class{gtk:widget}
  @see-symbol{graphene:point-t}"
  (when (%widget-compute-point widget target point out)
    out))

(export 'widget-compute-point)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_contains
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_contains" %widget-contains) :boolean
  (widget (g:object widget))
  (x :double)
  (y :double))

(defun widget-contains (widget x y)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object to query}
  @argument[x]{a number coerced to a double float for the x coordinate to test,
    relative to the origin of the widget}
  @argument[y]{a number coerced to a double float for the y coordinate to test,
    relative to the origin of the widget}
  @return{@em{True} if @arg{widget} contains @code{(x,y)}.}
  @begin{short}
    Tests if the point at @code{(x,y)} is contained in the widget.
  @end{short}
  The coordinates for @code{(x,y)} must be in widget coordinates, so
  @code{(0,0)} is assumed to be the top left of the content area of the widget.
  @see-class{gtk:widget}"
  (%widget-contains widget
                    (coerce x 'double-float)
                    (coerce y 'double-float)))

(export 'widget-contains)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_pick
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_pick" %widget-pick) (g:object widget)
  (widget (g:object widget))
  (x :double)
  (y :double)
  (flags pick-flags))

(defun widget-pick (widget x y flags)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[x]{a number coerced to a double float for the x coordinate to test,
    relative to the origin of the widget}
  @argument[y]{a number coerced to a double float for the y coordinate to test,
    relative to the origin of the widget}
  @argument[flags]{a @symbol{gtk:pick-flags} value to influence what is picked}
  @begin{return}
    The descendant @class{gtk:widget} object at the given coordinate or
    @code{nil} if none.
  @end{return}
  @begin{short}
    Finds the descendant of @arg{widget}, including @arg{widget} itself,
    closest to the screen at the point @code{(x,y)}.
  @end{short}
  The point must be given in widget coordinates, so @code{(0,0)} is assumed to
  be the top left of the content area of the widget.

  Usually widgets will return @code{nil} if the given coordinate is not
  contained in widget checked via the @fun{gtk:widget-contains} function.
  Otherwise they will recursively try to find a child that does not return
  @code{nil}. Widgets are however free to customize their picking algorithm.

  This function is used on the toplevel to determine the widget below the
  mouse cursor for purposes of hover highlighting and delivering events.
  @see-class{gtk:widget}
  @see-symbol{gtk:pick-flags}
  @see-function{gtk:widget-contains}"
  (%widget-pick widget
                (coerce x 'double-float)
                (coerce y 'double-float)
                flags))

(export 'widget-pick)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_focus_child
;;; gtk_widget_set_focus_child
;;; ----------------------------------------------------------------------------

(defun (setf widget-focus-child) (child widget)
  (cffi:foreign-funcall "gtk_widget_set_focus_child"
                        (g:object widget) widget
                        (g:object widget) child
                        :void)
  child)

(cffi:defcfun ("gtk_widget_get_focus_child" widget-focus-child)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[child]{a @class{gtk:widget} object for a direct child widget, or
    @code{nil} to unset the focus child of @arg{widget}}
  @begin{short}
    The @fun{gtk:widget-focus-child} function returns the current focus child
    of @arg{widget}.
  @end{short}
  The @setf{gtk:widget-focus-child} function sets @arg{child} as the current
  focus child of @arg{widget}. The previous focus child will be unset.

  This function is only suitable for widget implementations. If you want a
  certain widget to get the input focus, call the @fun{gtk:widget-grab-focus}
  function on it.
  @see-class{gtk:widget}
  @see-function{gtk:widget-grab-focus}"
  (widget (g:object widget)))

(export 'widget-focus-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_sensitive" widget-is-sensitive) :boolean
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is effectively sensitive.}
  @begin{short}
    Returns the widgets effective sensitivity, which means it is sensitive
    itself and also its parent widget is sensitive.
  @end{short}
  @see-class{gtk:widget}
  @see-function{gtk:widget-sensitive}"
  (widget (g:object widget)))

(export 'widget-is-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_visible
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_visible" widget-is-visible) :boolean
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} and all its parents are visible.}
  @begin{short}
    Determines whether the widget and all its parents are marked as visible.
  @end{short}
  This function does not check if the widget is obscured in any way. See also
  the @fun{gtk:widget-visible} function.
  @see-class{gtk:widget}
  @see-function{gtk:widget-visible}"
  (widget (g:object widget)))

(export 'widget-is-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_state_flags
;;; gtk_widget_set_state_flags
;;; ----------------------------------------------------------------------------

(defun (setf widget-state-flags) (flags widget &optional (clear nil))
  (cffi:foreign-funcall "gtk_widget_set_state_flags"
                        (g:object widget) widget
                        state-flags flags
                        :boolean clear
                        :void)
  flags)

(cffi:defcfun ("gtk_widget_get_state_flags" widget-state-flags) state-flags
 #+liber-documentation
 "@version{2025-02-15}
  @syntax{(gtk:widget-state-flags widget) => flags}
  @syntax{(setf (gtk:widget-state-flags widget clear) flags)}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[flags]{a @symbol{gtk:state-flags} value}
  @argument[clear]{an optional boolean whether to clear the state flags before
    turning on flags}
  @begin{short}
    The @fun{gtk:widget-state-flags} function returns the widget state as a
    flag set.
  @end{short}
  The @setf{gtk:widget-state-flags} function sets the widget state flags.

  It is worth mentioning that the effective @code{:insensitive} state will be
  returned, that is, also based on parent insensitivity, even if @arg{widget}
  itself is sensitive.

  Also note that if you are looking for a way to obtain the
  @symbol{gtk:state-flags} values to pass to a @class{gtk:style-context}
  function, you should look at the @fun{gtk:style-context-state} function.

  This function is for use in widget implementations. Turns on flag values in
  the current widget state, insensitive, prelighted, and so on.
  @see-class{gtk:widget}
  @see-class{gtk:style-context}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:style-context-state}"
  (widget (g:object widget)))

(export 'widget-state-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_unset_state_flags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_unset_state_flags" widget-unset-state-flags) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[flags]{a @symbol{gtk:state-flags} value to turn off}
  @begin{short}
    Turns off flag values for the current widget state, insensitive, prelighted,
    and so on.
  @end{short}
  See the @fun{gtk:widget-state-flags} function.

  This function is for use in widget implementations.
  @see-class{gtk:widget}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-state-flags}"
  (widget (g:object widget))
  (flags state-flags))

(export 'widget-unset-state-flags)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_visible_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_has_visible_focus" widget-has-visible-focus) :boolean
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} should display a focus rectangle.}
  @begin{short}
    Determines if the widget should show a visible indication that it has the
    global input focus.
  @end{short}
  This is a convenience function that takes into account whether focus
  indication should currently be shown in the toplevel window of @arg{widget}.
  See the @fun{gtk:window-focus-visible} function for more information about
  focus indication.

  To find out if the widget has the global input focus, use the
  @fun{gtk:widget-has-focus} function.
  @see-class{gtk:widget}
  @see-function{gtk:window-focus-visible}
  @see-function{gtk:widget-has-focus}"
  (widget (g:object widget)))

(export 'widget-has-visible-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_is_drawable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_is_drawable" widget-is-drawable) :boolean
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{@em{True} if @arg{widget} is drawable, @em{false} otherwise.}
  @begin{short}
    Determines whether the widget can be drawn to.
  @end{short}
  A widget can be drawn to if it is mapped and visible.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-is-drawable)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_measure
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_measure" %widget-measure) :void
  (widget (g:object widget))
  (orientation orientation)
  (for-size :int)
  (minimum (:pointer :int))
  (natural (:pointer :int))
  (minimum-baseline (:pointer :int))
  (natural-baseline (:pointer :int)))

(defun widget-measure (widget orientation for-size)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[orientation]{a @symbol{gtk:orientation} value for the orientation
    to measure}
  @argument[for-size]{an integer for the size for the opposite of
    @arg{orientation}, that is, if @arg{orientation} is @code{:horizontal}, this
    is the height the widget should be measured with. The @code{:vertical} case
    is analogous. This way, both height-for-width and width-for-height requests
    can be implemented. If no size is known, -1 can be passed.}
  @begin{return}
    @arg{minimum} -- an integer with the minimum size @br{}
    @arg{natural} -- an integer with the natural size @br{}
    @arg{minimum-baseline} -- an integer with the baseline position for the
      minimum size @br{}
    @arg{natural-baseline} -- an integer with the baseline position for the
      natural size
  @end{return}
  @begin{short}
    Measures @arg{widget} in the orientation @arg{orientation} and for the
    given @arg{for-size}.
  @end{short}
  As an example, if @arg{orientation} is @code{:horizontal} and @arg{for-size}
  is 300, this functions will compute the minimum and natural width of
  @arg{widget} if it is allocated at a height of 300 pixels.
  @see-class{gtk:widget}
  @see-symbol{gtk:orientation}"
  (cffi:with-foreign-objects ((minimum :int)
                              (natural :int)
                              (minimum-baseline :int)
                              (natural-baseline :int))
    (%widget-measure widget
                     orientation
                     for-size
                     minimum
                     natural
                     minimum-baseline
                     natural-baseline)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int)
            (cffi:mem-ref minimum-baseline :int)
            (cffi:mem-ref natural-baseline :int))))

(export 'widget-measure)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_snapshot
;;; ----------------------------------------------------------------------------

(defun widget-snapshot (widget snapshot)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[snapshot]{a @class{gtk:snapshot} object as passed to the widget,
    in particular, no calls to the @fun{gtk:snapshot-translate} function or
    other transform calls should have been made}
  @begin{short}
    Calls the @fun{gtk:widget-snapshot} virtual function for @arg{widget} and
    its children when a new snapshot of @arg{widget} has to be taken.
  @end{short}
  @begin[Notes]{dictionary}
    This function is not exported from the C library and implemented for the
    Lisp API with the @fun{gtk:widget-snapshot-child} function.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:snapshot}
  @see-function{gtk:widget-snapshot-child}
  @see-function{gtk:snapshot-translate}"
  (iter  (for child first (widget-first-child widget)
                    then (widget-next-sibling child))
         (while child)
         (widget-snapshot-child widget child snapshot)))

(export 'widget-snapshot)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_snapshot_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_snapshot_child" widget-snapshot-child) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[child]{a @class{gtk:widget} child widget of @arg{widget}}
  @argument[snapshot]{a @class{gtk:snapshot} object as passed to the widget,
    in particular, no calls to the @fun{gtk:snapshot-translate} function or
    other transform calls should have been made}
  @begin{short}
    When a widget receives a call to the snapshot function, it must send
    synthetic @fun{gtk:widget-snapshot} calls to all children.
  @end{short}
  This function provides a convenient way of doing this. A widget, when it
  receives a call to its @fun{gtk:widget-snapshot} virtual function, calls the
  @fun{gtk:widget-snapshot-child} function once for each child, passing in the
  snapshot the widget received.

  The @fun{gtk:widget-snapshot-child} function takes care of translating the
  origin of @arg{snapshot}, and deciding whether the child needs to be snapshot.

  This function does nothing for children that implement the @class{gtk:native}
  interface.
  @see-class{gtk:widget}
  @see-class{gtk:snapshot}
  @see-class{gtk:native}
  @see-function{gtk:widget-snapshot}
  @see-function{gtk:snapshot-translate}"
  (widget (g:object widget))
  (child (g:object widget))
  (snapshot (g:object snapshot)))

(export 'widget-snapshot-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_next_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_next_sibling" widget-next-sibling)
    (g:object widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:widget} sibling widget.}
  @begin{short}
    Returns the next sibling of the widget.
  @end{short}
  This API is primarily meant for widget implementations.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-next-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_prev_sibling
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_prev_sibling" widget-prev-sibling)
    (g:object widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:widget} sibling widget.}
  @begin{short}
    Returns the previous sibling of the widget.
  @end{short}
  This API is primarily meant for widget implementations.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-prev-sibling)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_first_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_first_child" widget-first-child)
    (g:object widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:widget} object.}
  @begin{short}
    Returns the first child of the widget.
  @end{short}
  This API is primarily meant for widget implementations.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-first-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_last_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_last_child" widget-last-child) (g:object widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:widget} object.}
  @begin{short}
    Returns the last child of the widget.
  @end{short}
  This API is primarily meant for widget implementations.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-last-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_insert_before
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_insert_before" widget-insert-before) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[parent]{a @class{gtk:widget} object for the parent widget to insert
    @arg{widget} into}
  @argument[next]{a @class{gtk:widget} object for the next sibling of
    @arg{widget} or @code{nil}}
  @begin{short}
    Inserts @arg{widget} into the child widget list of @arg{parent}.
  @end{short}
  It will be placed before @arg{next}, or at the end if @arg{next} is
  @code{nil}.

  After calling this function, the @fun{gtk:widget-next-sibling} function will
  return @arg{next}. If @arg{parent} is already set as the parent widget of
  @arg{widget}, this function can also be used to reorder @arg{widget} in the
  child widget list of @arg{parent}.

  This API is primarily meant for widget implementations. If you are just
  using a widget, you must use its own API for adding children.
  @see-class{gtk:widget}
  @see-function{gtk:widget-next-sibling}"
  (widget (g:object widget))
  (parent (g:object widget))
  (next (g:object widget)))

(export 'widget-insert-before)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_insert_after
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_insert_after" widget-insert-after) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[parent]{a @class{gtk:widget} object for the parent widget to insert
    @arg{widget} into}
  @argument[prev]{a @class{gtk:widget} object for the previous sibling of
    @arg{widget} or @code{nil}}
  @begin{short}
    Inserts @arg{widget} into the child widget list of @arg{parent}.
  @end{short}
  It will be placed after @arg{prev}, or at the beginning if @arg{prev} is
  @code{nil}.

  After calling this function, the @fun{gtk:widget-prev-sibling} function will
  return @arg{prev}. If @arg{parent} is already set as the parent widget of
  @arg{widget}, this function can also be used to reorder @arg{widget} in the
  child widget list of @arg{parent}.

  This API is primarily meant for widget implementations. If you are just
  using a widget, you must use its own API for adding children.
  @see-class{gtk:widget}
  @see-function{gtk:widget-prev-sibling}"
  (widget (g:object widget))
  (parent (g:object widget))
  (prev (g:object widget)))

(export 'widget-insert-after)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_should_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_should_layout" widget-should-layout) :boolean
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @em{True} if @arg{widget} should be included in measureing and allocating.
  @end{return}
  @begin{short}
    Returns whether @arg{widget} should contribute to the measuring and
    allocation of its parent.
  @end{short}
  This is @em{false} for invisible children, but also for children that have
  their own surface.
  @see-class{gtk:widget}"
  (widget (g:object widget)))

(export 'widget-should-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_color
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_widget_get_color" %widget-color) :void
  (widget (g:object widget))
  (color (g:boxed gdk:rgba)))

#+gtk-4-10
(defun widget-color (widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gdk:rgba} instance with the color.}
  @begin{short}
    Gets the current foreground color for the CSS style of the widget.
  @end{short}
  This function should only be used in snapshot implementations that need to do
  custom drawing with the foreground color.

  Since 4.10
  @see-class{gtk:widget}
  @see-class{gdk:rgba}"
  (let ((color (gdk:rgba-new)))
    (%widget-color widget color)
    color))

#+gtk-4-10
(export 'widget-color)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_add_css_class
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_add_css_class" widget-add-css-class) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[class]{a string for the style class to add, without the leading
    \".\" used for notation of style classes}
  @begin{short}
    Adds @arg{class} to the widget.
  @end{short}
  After calling this function, the style of the widget will match for
  @arg{class}, after the CSS matching rules.
  @begin[Examples]{dictionary}
    Get the CSS style classes, add and remove a CSS style class:
    @begin{pre}
(defvar dialog (make-instance 'gtk:about-dialog)) => DIALOG
(gtk:widget-css-classes dialog)
=> (\"background\" \"csd\" \"aboutdialog\")
(gtk:widget-add-css-class dialog \"mystyle\")
(gtk:widget-css-classes dialog)
=> (\"background\" \"csd\" \"aboutdialog\" \"mystyle\")
(gtk:widget-remove-css-class dialog \"mystyle\")
(gtk:widget-css-classes dialog)
=> (\"background\" \"csd\" \"aboutdialog\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-remove-css-class}
  @see-function{gtk:widget-css-classes}"
  (widget (g:object widget))
  (class :string))

(export 'widget-add-css-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_remove_css_class
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_remove_css_class" widget-remove-css-class) :void
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[class]{a string for the style class to remove from @arg{widget},
    without the leading \".\" used for notation of style classes}
  @begin{short}
    Removes @arg{class} from @arg{widget}.
  @end{short}
  After this, the style of the widget will stop matching for @arg{class}.
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-css-class}"
  (widget (g:object widget))
  (class :string))

(export 'widget-remove-css-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_has_css_class
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_has_css_class" widget-has-css-class) :boolean
 #+liber-documentation
 "@version{2025-06-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[class]{a string for the style class, without the leading \".\"
    used for notation of style classes}
  @begin{return}
    @em{True} if @arg{class} is currently applied to @arg{widget}, @em{false}
    otherwise.
  @end{return}
  @short{Returns whether @arg{class} is currently applied to @arg{widget}.}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-css-class}"
  (widget (g:object widget))
  (class :string))

(export 'widget-has-css-class)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_get_css_name
;;; gtk_widget_class_set_css_name
;;; ----------------------------------------------------------------------------

(defun (setf widget-class-css-name) (name gtype)
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (cffi:foreign-funcall "gtk_widget_class_set_css_name"
                            :pointer class
                            :string name
                            :void)
      (g:type-class-unref class))
    name))

(cffi:defcfun ("gtk_widget_class_get_css_name" %widget-class-css-name) :string
  (class :pointer))

(defun widget-class-css-name (gtype)
  #+liber-documentation
 "@version{2025-02-15}
  @syntax{(gtk:widget-class-css-name gtype) => name}
  @syntax{(setf (gtk:widget-class-css-name gtype) name)}
  @argument[gtype]{a @class{g:type-t} type ID}
  @argument[name]{a string for the CSS name}
  @begin{short}
    The @fun{gtk:widget-class-css-name} function gets the name used by this
   widget class for matching in CSS code.
  @end{short}
  The @setf{gtk:widget-class-css-name} function sets the name. If this function
  is not called for a given class, the name of the parent class is used.
  @see-class{gtk:widget}
  @see-class{g:type-t}"
  (let ((class (g:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-css-name class)
      (g:type-class-unref class))))

(export 'widget-class-css-name)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_style_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_style_context" %widget-style-context)
    (g:object style-context)
  (widget (g:object widget)))

(defun widget-style-context (widget)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{gtk:style-context} object.}
  @begin{short}
    Returns the style context associated to the widget.
  @end{short}
  The returned object is guaranteed to be the same for the lifetime of
  the widget.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Style contexts will be removed in
    GTK 5.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:style-context}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:WIDGET-STYLE-CONTEXT is deprecated since 4.10."))
  (%widget-style-context widget))

(export 'widget-style-context)

;;; ----------------------------------------------------------------------------
;;; gtk:widget-add-provider                                 Lisp extension
;;; ----------------------------------------------------------------------------

(defun widget-add-provider (widget provider
                            &optional (priority +priority-application+))
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an optional unsigned integer for the priority of the
    style provider}
  @return{The string with an unique identifier for the added @arg{provider}.}
  @begin{short}
    Adds a style provider to the display of @arg{widget} to be used in style
    construction.
  @end{short}
  The style provider is removed from the display when the widget is destroyed.
  Call the @fun{gtk:widget-remove-provider} function with the returned unique
  identifier to remove the provider from the display of the widget.

  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{gtk:+priority-fallback+} and @var{gtk:+priority-user+} priorities. The
  default value is @var{gtk:+priority-application+}. See the
  @fun{gtk:style-context-add-provider} documentation for more information.
  @begin[Notes]{dictionary}
    This function is a Lisp extension that provides a convenient way to apply a
    provider to the display of the widget. The style provider is added with the
    @fun{gtk:style-context-add-provider-for-display} function. A destroy
    notify callback function is installed with the @fun{g:object-set-data-full}
    function. It removes the style provider with the
    @fun{gtk:style-context-remove-provider-for-display} function for the display
    when the widget is destroyed. Alternativly, you can remove the provider
    from the display of the widget with the @fun{gtk:widget-remove-provider}
    function.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-remove-provider}"
  (let ((display (widget-display widget))
        (key (symbol-name (gensym))))
    (style-context-add-provider-for-display display provider priority)
    (gobject:object-set-data-full widget key
        (lambda ()
          (style-context-remove-provider-for-display display provider)))
    key))

(export 'widget-add-provider)

;;; ----------------------------------------------------------------------------
;;; gtk:widget-remove-provider                              Lisp extension
;;; ----------------------------------------------------------------------------

(defun widget-remove-provider (widget key)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[key]{a string for the unique identifier for the style provider,
    this is the return value of the @fun{gtk:widget-add-provider} function}
  @begin{short}
    Removes a style provider from the display of the widget.
  @end{short}
  The style provider has been added with the @fun{gtk:widget-add-provider}
  function.
  @begin[Notes]{dictionary}
    This function calls the destroy notify callback function that has been
    installed with the @fun{gtk:widget-add-provider} function. The destroy
    notify callback function calls the
    @fun{gtk:style-context-remove-provider-for-display} function.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-function{gtk:widget-add-provider}
  @see-function{gtk:style-context-remove-provider-for-display}"
  (setf (gobject:object-data widget key) nil))

(export 'widget-remove-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_request_mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_request_mode" widget-request-mode)
    size-request-mode
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @symbol{gtk:size-request-mode} value preferred by @arg{widget}.}
  @begin{short}
    Gets whether the widget prefers a height-for-width layout or a
    width-for-height layout.
  @end{short}
  Single-child widgets generally propagate the preference of their child, more
  complex widgets need to request something either in context of their children
  or in context of their allocation capabilities.
  @see-class{gtk:widget}
  @see-symbol{gtk:size-request-mode}"
  (widget (g:object widget)))

(export 'widget-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_preferred_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_preferred_size" %widget-preferred-size) :void
  (widget (g:object widget))
  (minium-size (g:boxed requisition))
  (natural-size (g:boxed requisition)))

(defun widget-preferred-size (widget)
 #+liber-documentation
 "@version{#2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    @arg{minimum-size} -- a @class{gtk:requisition} instance with the minimum
      size, or @code{nil} @br{}
    @arg{natural-size} -- a @class{gtk:requisition} instance with the the
      natural size, or @code{nil}
  @end{return}
  @begin{short}
    Retrieves the minimum and natural size of a widget, taking into account the
    preference for height-for-width management of the widget.
  @end{short}

  This is used to retrieve a suitable size by container widgets which do not
  impose any restrictions on the child placement. It can be used to deduce
  toplevel window and menu sizes as well as child widgets in free-form
  containers such as the @class{gtk:fixed} widget.

  Use the @fun{gtk:widget-measure} function if you want to support baseline
  alignment.
  @begin[Notes]{dictionary}
    Handle with care. Note that the natural height of a height-for-width widget
    will generally be a smaller size than the minimum height, since the required
    height for the natural width is generally smaller than the required height
    for the minimum width.
  @end{dictionary}
  @see-class{gtk:widget}
  @see-class{gtk:requisition}
  @see-function{gtk:widget-measure}"
 (let ((minimum-size (requisition-new))
       (natural-size (requisition-new)))
    (%widget-preferred-size widget minimum-size natural-size)
    (values minimum-size
            natural-size)))

(export 'widget-preferred-size)

;;; ----------------------------------------------------------------------------
;;; gtk_distribute_natural_allocation
;;;
;;; Distributes extra_space to child sizes by bringing smaller children up to
;;; natural size first.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_compute_expand
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_compute_expand" widget-compute-expand) :boolean
 #+liber-documentation
 "@version{2025-02-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[orientation]{a @symbol{gtk:orientation} value with the expand
    direction}
  @return{The boolean whether the widget tree rooted here should be expanded.}
  @begin{short}
    Computes whether a container should give this widget extra space when
    possible.
  @end{short}
  Containers should check this, rather than looking at the
  @slot[gtk:widget]{hexpand} or @slot[gtk:widget]{vexpand} properties.

  This function already checks whether the widget is visible, so visibility
  does not need to be checked separately. Non-visible widgets are not
  expanded.

  The computed expand value uses either the expand setting explicitly set on
  the widget itself, or, if none has been explicitly set, the widget may
  expand if some of its children do.
  @see-class{gtk:widget}
  @see-symbol{gtk:orientation}
  @see-function{gtk:widget-hexpand}
  @see-function{gtk:widget-vexpand}"
  (widget (g:object widget))
  (orientation orientation))

(export 'widget-compute-expand)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_init_template
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_init_template" widget-init-template) :void
 #+liber-documentation
 "@version{2024-09-14}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    Creates and initializes child widgets defined in templates.
  @end{short}
  This function must be called in the instance initializer for any class which
  assigned itself a template using the @fun{gtk:widget-class-set-template}
  function.

  It is important to call this function in the instance initializer of a
  GtkWidget subclass. One reason is that generally derived widgets will assume
  that parent class composite widgets have been created in their instance
  initializers. Another reason is that when calling the @fun{g:object-new}
  function on a widget with composite templates, it is important to build the
  composite widgets before the construct properties are set. Properties passed
  to the @fun{g:object-new} function should take precedence over properties set
  in the private template XML.
  @see-class{gtk:widget}
  @see-function{gtk:widget-class-set-template}
  @see-function{g:object-new}"
  (widget (g:object widget)))

(export 'widget-init-template)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_dispose_template
;;; ----------------------------------------------------------------------------

#+gtk-4-8
(cffi:defcfun ("gtk_widget_dispose_template" widget-dispose-template) :void
 #+liber-documentation
 "@version{2024-09-14}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[gtype]{a @class{g:type-t} type ID of the widget to finalize the
    template for}
  @begin{short}
    Clears the template children for the given @arg{widget}.
  @end{short}
  This function is the opposite of the @fun{gtk:widget-init-template} function,
  and it is used to clear all the template children from a widget instance. If
  you bound a template child to a field in the instance structure, or in the
  instance private data structure, the field will be set to @code{NULL} after
  this function returns.

  You should call this function inside the @code{GObjectClass.dispose()}
  implementation of any widget that called the @fun{gtk:widget-init-template}
  function. Typically, you will want to call this function last, right before
  chaining up to the parent type’s dispose implementation.

  Since 4.8
  @see-class{gtk:widget}
  @see-class{g:type-t}
  @see-function{gtk:widget-init-template}"
  (widget (g:object widget))
  (gtype g:type-t))

#+gtk-4-8
(export 'widget-dispose-template)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_get_template_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_get_template_child" widget-template-child) g:object
 #+liber-documentation
 "@version{2025-06-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[gtype]{a @class{g:type-t} type ID to get a template child for}
  @argument[name]{a string for the ID of the child defined in the template XML}
  @begin{return}
    The @class{g:object} instance built in the template XML with the ID name.
  @end{return}
  @begin{short}
    Fetch an object build from the template XML for @arg{gtype} in this widget
    instance.
  @end{short}
  This will only report children which were previously declared with the
  @fun{gtk:widget-class-bind-template-child} function or one of its variants.

  This function is only meant to be called for code which is private to the
  @arg{gtype} which declared the child and is meant for language bindings
  which cannot easily make use of the @class{g:object} structure offsets.
  @see-class{gtk:widget}
  @see-class{g:object}
  @see-class{g:type-t}
  @see-function{gtk:widget-class-bind-template-child}"
  (widget (g:object widget))
  (gtype g:type-t)
  (name :string))

(export 'widget-template-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_template
;;; ----------------------------------------------------------------------------

;; FIXME: This implementation does not work. What is wrong?

(cffi:defcfun ("gtk_widget_class_set_template" %widget-class-set-template) :void
  (class (:pointer (:struct gobject:type-class)))
  (template (g:boxed glib:bytes)))

(defun widget-class-set-template (gtype template)
 #+liber-documentation
 "@version{2024-09-14}
  @argument[gtype]{a @class{g:type-t} type ID of the widget class}
  @argument[template]{a string holding the @class{gtk:builder} XML}
  @begin{short}
    This should be called at class initialization time to specify the
    @class{gtk:builder} XML to be used to extend a widget.
  @end{short}
  For convenience, the @fun{gtk:widget-class-set-template-from-resource}
  function is also provided.

  Note that any class that installs templates must call the
  @fun{gtk:widget-init-template} function in the instance initializer of the
  widget.
  @see-class{gtk:widget}
  @see-function{gtk:widget-init-template}
  @see-function{gtk:widget-class-set-template-from-resource}"
  (let ((class (gobject:type-class-ref gtype)))
    (multiple-value-bind (data len)
        (cffi:foreign-string-alloc template)
        (let ((bytes (glib:bytes-new data len)))
          (format t "  data : ~a~%" (glib:bytes-data bytes))
          (format t "  size : ~a~%" (glib:bytes-size bytes))
          (format t "   str : ~a~%"
                    (cffi:foreign-string-to-lisp (glib:bytes-data bytes)))
          (unwind-protect
            (%widget-class-set-template class bytes)
            (progn
              (gobject:type-class-unref class)
;             (cffi:foreign-string-free data)
))))))

(export 'widget-class-set-template)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_template_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_set_template_from_resource"
               %widget-class-set-template-from-resource) :void
  (class (:pointer (:struct gobject:type-class)))
  (name :string))

(defun widget-class-set-template-from-resource (gtype name)
 #+liber-documentation
 "@version{2025-06-15}
  @argument[gtype]{a @class{g:type-t} type ID of the widget class}
  @argument[name]{a string for the name of the resource to load the template
    from}
  @begin{short}
    A convenience function to call the @fun{gtk:widget-class-set-template}
    function.
  @end{short}

  Note that any class that installs templates must call the
  @fun{gtk:widget-init-template} function in the instance initializer of the
  widget.
  @see-class{gtk:widget}
  @see-class{g:type-t}
  @see-function{gtk:widget-class-set-template}
  @see-function{gtk:widget-init-template}"
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-set-template-from-resource class name)
      (gobject:type-class-unref class))))

(export 'widget-class-set-template-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_set_template_scope ()
;;;
;;; void
;;; gtk_widget_class_set_template_scope (GtkWidgetClass *widget_class,
;;;                                      GtkBuilderScope *scope);
;;;
;;; For use in language bindings, this will override the default GtkBuilderScope
;;; to be used when parsing GtkBuilder XML from this class’s template data.
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; widget_class :
;;;     A GtkWidgetClass
;;;
;;; scope :
;;;     The GtkBuilderScope to use when loading the class template.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_set_template_scope"
               %widget-class-set-template-scope) :void
  (class :pointer)
  (scope (g:object builder-scope)))

(defun widget-class-set-template-scope (gtype scope)
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-set-template-scope class scope)
      (gobject:type-class-unref class))))

(export 'widget-class-set-template-scope)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child(widget_class, TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; instance structure.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; TypeName :
;;;     the type name of this widget
;;;
;;; member_name :
;;;     name of the instance member in the instance struct for data_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_bind_template_child_full"
               %widget-class-bind-template-child-full) :void
  (class :pointer)
  (name :string)
  (internal :boolean)
  (offset :size))

(defun widget-class-bind-template-child (gtype name)
 #+liber-documentation
 "@version{2025-06-15}
  @argument[gtype]{a @class{g:type-t} type ID of the widget class}
  @argument[name]{a string for the ID of the child defined in the template XML}
  @begin{short}
    Binds a child widget defined in a template to the widget class of type
    @arg{gtype}.
  @end{short}
  @see-class{gtk:widget}"
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-bind-template-child-full class name nil 0)
      (gobject:type-class-unref class))))

(export 'widget-class-bind-template-child)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_internal()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_internal(widget_class,
;;;                                               TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class , and also
;;; makes it available as an internal child in GtkBuilder, under the name
;;; member_name .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; instance structure.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; TypeName :
;;;     the type name, in CamelCase
;;;
;;; member_name :
;;;     name of the instance member in the instance struct for data_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_private()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_private(widget_class,
;;;                                              TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; private data structure (it uses G_PRIVATE_OFFSET(), so the private struct
;;; must be added with G_ADD_PRIVATE()).
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; TypeName :
;;;     the type name of this widget
;;;
;;; member_name :
;;;     name of the instance private member in the private struct for data_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_internal_private()
;;;
;;; #define
;;; gtk_widget_class_bind_template_child_internal_private(widget_class,
;;;                                                       TypeName, member_name)
;;;
;;; Binds a child widget defined in a template to the widget_class , and also
;;; makes it available as an internal child in GtkBuilder, under the name
;;; member_name .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_child_full() function.
;;;
;;; This macro will use the offset of the member_name inside the TypeName
;;; private data structure.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; TypeName :
;;;     the type name, in CamelCase
;;;
;;; member_name :
;;;     name of the instance private member on the private struct for data_type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_child_full ()
;;;
;;; void
;;; gtk_widget_class_bind_template_child_full
;;;                                (GtkWidgetClass *widget_class,
;;;                                 const char *name,
;;;                                 gboolean internal_child,
;;;                                 gssize struct_offset);
;;;
;;; Automatically assign an object declared in the class template XML to be set
;;; to a location on a freshly built instance’s private data, or alternatively
;;; accessible via gtk_widget_get_template_child().
;;;
;;; The struct can point either into the public instance, then you should use
;;; G_STRUCT_OFFSET(WidgetType, member) for struct_offset , or in the private
;;; struct, then you should use G_PRIVATE_OFFSET(WidgetType, member).
;;;
;;; An explicit strong reference will be held automatically for the duration of
;;; your instance’s life cycle, it will be released automatically when
;;; GObjectClass.dispose() runs on your instance and if a struct_offset that is
;;; != 0 is specified, then the automatic location in your instance public or
;;; private data will be set to NULL. You can however access an automated child
;;; pointer the first time your classes GObjectClass.dispose() runs, or
;;; alternatively in GtkWidgetClass.destroy().
;;;
;;; If internal_child is specified, GtkBuildableIface.get_internal_child() will
;;; be automatically implemented by the GtkWidget class so there is no need to
;;; implement it manually.
;;;
;;; The wrapper macros gtk_widget_class_bind_template_child(),
;;; gtk_widget_class_bind_template_child_internal(),
;;; gtk_widget_class_bind_template_child_private() and
;;; gtk_widget_class_bind_template_child_internal_private() might be more
;;; convenient to use.
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; widget_class :
;;;     A GtkWidgetClass
;;;
;;; name :
;;;     The “id” of the child defined in the template XML
;;;
;;; internal_child :
;;;     Whether the child should be accessible as an “internal-child” when this
;;;     class is used in GtkBuilder XML
;;;
;;; struct_offset :
;;;     The structure offset into the composite widget’s instance public or
;;;     private structure where the automated child pointer should be set, or 0
;;;     to not assign the pointer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_callback()
;;;
;;; #define
;;; gtk_widget_class_bind_template_callback(widget_class, callback)
;;;
;;; Binds a callback function defined in a template to the widget_class .
;;;
;;; This macro is a convenience wrapper around the
;;; gtk_widget_class_bind_template_callback_full() function. It is not supported
;;; after gtk_widget_class_set_template_scope() has been used on widget_class .
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; callback :
;;;     the callback symbol
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_bind_template_callback_full ()
;;;
;;; void
;;; gtk_widget_class_bind_template_callback_full
;;;                                (GtkWidgetClass *widget_class,
;;;                                 const char *callback_name,
;;;                                 GCallback callback_symbol);
;;;
;;; Declares a callback_symbol to handle callback_name from the template XML
;;; defined for widget_type . This function is not supported after
;;; gtk_widget_class_set_template_scope() has been used on widget_class . See
;;; gtk_builder_cscope_add_callback_symbol().
;;;
;;; Note that this must be called from a composite widget classes class
;;; initializer after calling gtk_widget_class_set_template().
;;;
;;; widget_class :
;;;     A GtkWidgetClass
;;;
;;; callback_name :
;;;     The name of the callback as expected in the template XML
;;;
;;; callback_symbol :
;;;     The callback symbol.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_class_bind_template_callback_full"
               %widget-class-bind-template-callback-full) :void
  (class :pointer)
  (name :string)
  (symbol :string))

(defun widget-class-bind-template-callback (gtype name symbol)
  (let ((class (gobject:type-class-ref gtype)))
    (unwind-protect
      (%widget-class-bind-template-callback-full class name symbol)
      (gobject:type-class-unref class))))

(export 'widget-class-bind-template-callback)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_observe_children
;;; ----------------------------------------------------------------------------

;; We need the implementation of this undocumented GType to get the
;; correct return value from this function and the
;; gtk:widget-observe-controllers function
(gobject:define-gobject "GtkListListModel" list-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"))
  ((item-type
    %list-list-model-item-type
    "item-type" "GType" t nil)
   (n-items
    list-list-model-n-items
    "n-items" "guint" t nil)))

(cffi:defcfun ("gtk_widget_observe_children" widget-observe-children)
    (g:object gio:list-model :return)
 "@version{#2024-12-09}
  @argument[widget]{a @class{gtk:widget} object}
  @return{The @class{g:list-model} object tracking the children of @arg{widet}.}
  @begin{short}
    Returns a @class{g:list-model} object to track the children of @arg{widget}.
  @end{short}

  Calling this function will enable extra internal bookkeeping to track children
  and emit signals on the returned list model. It may slow down operations a
  lot. Applications should try hard to avoid calling this function because of
  the slowdowns.
  @see-class{gtk:widget}
  @see-class{g:list-model}"
  (widget (g:object widget)))

(export 'widget-observe-children)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_observe_controllers
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_observe_controllers" widget-observe-controllers)
    (g:object gio:list-model :return)
 "@version{#2025-06-15}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{return}
    The @class{g:list-model} object tracking the controllers of @arg{widet}.
  @end{return}
  @begin{short}
    Returns a @class{g:list-model} object to track the
    @class{gtk:event-controller} objects of @arg{widget}.
  @end{short}

  Calling this function will enable extra internal bookkeeping to track
  controllers and emit signals on the returned list model. It may slow down
  operations a lot. Applications should try hard to avoid calling this function
  because of the slowdowns.
  @see-class{gtk:widget}
  @see-class{g:list-model}
  @see-class{gtk:event-controller}"
  (widget (g:object widget)))

(export 'widget-observe-controllers)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_insert_action_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_widget_insert_action_group" widget-insert-action-group)
    :void
 #+liber-documentation
 "@version{2025-06-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[prefix]{a string for the prefix for actions in @arg{group}}
  @argument[group]{a @class{g:action-group} instance}
  @begin{short}
    Inserts an action group into @arg{widget}.
  @end{short}
  Children of @arg{widget} that implement the @class{gtk:actionable} interface
  can then be associated with actions in @arg{group} by setting their
  @slot[gtk:actionable]{action-name} property to the
  @code{\"prefix.action-name\"} value.
  @see-class{gtk:widget}
  @see-class{g:action-group}"
  (widget (g:object widget))
  (prefix :string)
  (group (g:object g:action-group)))

(export 'widget-insert-action-group)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate_action_variant ()
;;;
;;; gboolean
;;; gtk_widget_activate_action_variant (GtkWidget *widget,
;;;                                     const char *name,
;;;                                     GVariant *args);
;;;
;;; Looks up the action in the action groups associated with widget and its
;;; ancestors, and activates it.
;;;
;;; If the action is in an action group added with
;;; gtk_widget_insert_action_group(), the name is expected to be prefixed with
;;; the prefix that was used when the group was inserted.
;;;
;;; The arguments must match the actions expected parameter type, as returned by
;;; g_action_get_parameter_type().
;;;
;;; widget :
;;;     a GtkWidget
;;;
;;; name :
;;;     the name of the action to activate
;;;
;;; args :
;;;     parameters to use, or NULL.
;;;
;;; Returns :
;;;     TRUE if the action was activated, FALSE if the action does not exist.
;;; ----------------------------------------------------------------------------

;; TODO: We have to pass an array of parameters. This function is called
;; internally from the GTK:WIDGET-ACTIVATE-ACTION function.

(cffi:defcfun ("gtk_widget_activate_action_variant"
               %widget-activate-action-variant) :boolean
  (widget (g:object widget))
  (name :string)
  (args :pointer))

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate_action
;;; ----------------------------------------------------------------------------

;; TODO: The implementation of the parameters is missing

(defun widget-activate-action (widget name)
 #+liber-documentation
 "@version{2025-06-15}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[name]{a string for the name of the action to activate}
  @begin{return}
    @em{True} if the action was activated, @em{false} if the action does not
    exist.
  @end{return}
  @begin{short}
    Looks up the action in the action groups associated with widget and its
    ancestors, and activates it.
  @end{short}
  @begin[Notes]{dictionary}
    In the Lisp binding the implementation of the parameters for an action is
    missing.
  @end{dictionary}
  @see-class{gtk:widget}"
  (%widget-activate-action-variant widget name (cffi:null-pointer)))

(export 'widget-activate-action)

;;; ----------------------------------------------------------------------------
;;; gtk_widget_activate_default ()
;;;
;;; void
;;; gtk_widget_activate_default (GtkWidget *widget);
;;;
;;; Activate the default.activate action from widget .
;;;
;;; widget :
;;;     a GtkWidget
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkWidgetActionActivateFunc ()
;;;
;;; void
;;; (*GtkWidgetActionActivateFunc) (GtkWidget *widget,
;;;                                 const char *action_name,
;;;                                 GVariant *parameter);
;;;
;;; The type of the callback functions used for activating actions installed
;;; with gtk_widget_class_install_action().
;;;
;;; The parameter must match the parameter_type of the action.
;;;
;;; widget :
;;;     the widget to which the action belongs
;;;
;;; action_name :
;;;     the action name
;;;
;;; parameter :
;;;     parameter for activation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_action ()
;;;
;;; void
;;; gtk_widget_class_install_action (GtkWidgetClass *widget_class,
;;;                                  const char *action_name,
;;;                                  const char *parameter_type,
;;;                                  GtkWidgetActionActivateFunc activate);
;;;
;;; This should be called at class initialization time to specify actions to be
;;; added for all instances of this class.
;;;
;;; Actions installed by this function are stateless. The only state they have
;;; is whether they are enabled or not.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; action_name :
;;;     a prefixed action name, such as "clipboard.paste"
;;;
;;; parameter_type :
;;;     the parameter type, or NULL.
;;;
;;; activate :
;;;     callback to use when the action is activated.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_install_property_action ()
;;;
;;; void
;;; gtk_widget_class_install_property_action
;;;                                (GtkWidgetClass *widget_class,
;;;                                 const char *action_name,
;;;                                 const char *property_name);
;;;
;;; Installs an action called action_name on widget_class and binds its state to
;;; the value of the property_name property.
;;;
;;; This function will perform a few santity checks on the property selected via
;;; property_name . Namely, the property must exist, must be readable, writable
;;; and must not be construct-only. There are also restrictions on the type of
;;; the given property, it must be boolean, int, unsigned int, double or string.
;;; If any of these conditions are not met, a critical warning will be printed
;;; and no action will be added.
;;;
;;; The state type of the action matches the property type.
;;;
;;; If the property is boolean, the action will have no parameter and toggle the
;;; property value. Otherwise, the action will have a parameter of the same type
;;; as the property.
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; action_name :
;;;     name of the action
;;;
;;; property_name :
;;;     name of the property in instances of widget_class or any parent class.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_class_query_action ()
;;;
;;; gboolean
;;; gtk_widget_class_query_action (GtkWidgetClass *widget_class,
;;;                                guint index_,
;;;                                GType *owner,
;;;                                const char **action_name,
;;;                                const GVariantType **parameter_type,
;;;                                const char **property_name);
;;;
;;; Queries the actions that have been installed for a widget class using
;;; gtk_widget_class_install_action() during class initialization.
;;;
;;; Note that this function will also return actions defined by parent classes.
;;; You can identify those by looking at owner .
;;;
;;; widget_class :
;;;     a GtkWidgetClass
;;;
;;; index_ :
;;;     position of the action to query
;;;
;;; owner :
;;;     return location for the type where the action was defined.
;;;
;;; action_name :
;;;     return location for the action name.
;;;
;;; parameter_type :
;;;     return location for the parameter type.
;;;
;;; property_name :
;;;     return location for the property name.
;;;
;;; Returns :
;;;     TRUE if the action was found, FALSE if index_ is out of range
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_widget_action_set_enabled                           not implemented
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.widget.lisp -------------------------------------------
