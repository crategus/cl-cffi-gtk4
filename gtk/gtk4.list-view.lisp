;;; ----------------------------------------------------------------------------
;;; gtk4.list-view.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; GtkListView
;;;
;;;     A widget for displaying lists
;;;
;;; Types and Values
;;;
;;;     GtkListBase
;;;     GtkListView
;;;     GtkListTabBehavior
;;;
;;; Accessors
;;;
;;;     gtk_list_view_get_factory
;;;     gtk_list_view_set_factory
;;;     gtk_list_view_get_header_factory                   Since 4.12
;;;     gtk_list-view_set_header_factory                   Since 4.12
;;;     gtk_list_view_get_model
;;;     gtk_list_view_set_model
;;;     gtk_list_view_get_show_separators
;;;     gtk_list_view_set_show_separators
;;;     gtk_list_view_get_single_click_activate
;;;     gtk_list_view_set_single_click_activate
;;;     gtk_list_view_get_enable_rubberband
;;;     gtk_list_view_set_enable_rubberband
;;;     gtk_list_view_get_tab_behavior                     Since 4.12
;;;     gtk_list_view_set_tab_behavior                     Since 4.12
;;;
;;; Functions
;;;
;;;     gtk_list_view_new
;;;     gtk_list_view_scroll_to                            Since 4.12
;;;
;;; Properties (GtkListBase)
;;;
;;;     orientation
;;;
;;; Properties (GtkListView)
;;;
;;;     enable-rubberband
;;;     factory
;;;     header-factory                                     Since 4.12
;;;     model
;;;     show-separators
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
;;;                 ╰── GtkListView
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
;;; GtkListTabBehavior
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(gobject:define-g-enum "GtkListTabBehavior" list-tab-behavior
  (:export t
   :type-initializer "gtk_list_tab_behavior_get_type")
  (:all 0)
  (:item 1)
  (:cell 2))

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-symbol 'list-tab-behavior)
      "GEnum"
      (liber:symbol-documentation 'list-tab-behavior)
 "@version{#2023-9-8}
  @begin{short}
    Used to configure the focus behavior in the @code{:forward} and
    @code{:backward} direction, like the @kbd{Tab} key in a
    @class{gtk:list-view} widget.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkListTabBehavior\" list-tab-behavior
  (:export t
   :type-initializer \"gtk_list_tab_behavior_get_type\")
  (:all 0)
  (:item 1)
  (:cell 2))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{Cycle through all focusable items of the list.}
    @entry[:item]{Cycle through a single list element, then move focus out of
      the list. Moving focus between items needs to be done with the arrow
      keys.}
    @entry[:cell]{Cycle only through a single cell, then move focus out of the
      list. Moving focus between cells needs to be done with the arrow keys.
      This is only relevant for cell-based widgets like the
      @class{gtk:column-view} widget, otherwise it behaves like @code{:item}.}
  @end{table}
  Since 4.12
  @see-class{gtk:list-view}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; GtkListBase
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListBase" list-base
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_list_base_get_type")
  ((orientation
    list-base-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (documentation 'list-base 'type)
 "@version{#2023-9-8}
  @begin{short}
    The @class{gtk:list-base} class is the abstract base class for GTK’s list
    widgets.
  @end{short}
  @see-class{gtk:list-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- list-base-orientation --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'list-base) t)
 "The @code{orientation} property of type @symbol{gtk:orientation}
  (Read / Write) @br{}
  The orientation of the list. @br{}
  Default value: @code{:vertical}")

#+liber-documentation
(setf (liber:alias-for-function 'list-base-orientation)
      "Accessor"
      (documentation 'list-base-orientation 'function)
 "@version{#2023-9-8}
  @syntax[]{(gtk:list-base-orientation object) => orientation}
  @syntax[]{(setf (gtk:list-base-orientation object) orientation)}
  @argument[object]{a @class{gtk:list-base} widget}
  @argument[orientation]{a @symbol{gtk:orientation} value}
  @begin{short}
    Accessor of the @slot[gtk:list-base]{orientation} slot of the
    @class{gtk:list-base} class.
  @end{short}
  @see-slot{gtk:list-base-orientation}
  @see-class{gtk:list-view}")

;;; ----------------------------------------------------------------------------
;;; GtkListView
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListView" list-view
  (:superclass list-base
   :export t
   :interfaces nil
   :type-initializer "gtk_list_view_get_type")
  ((enable-rubberband
    list-view-enable-rubberband
    "enable-rubberband" "gboolean" t t)
   (factory
    list-view-factory
    "factory" "GtkListItemFactory" t t)
   #+gtk-4-12
   (header-factory
    list-view-header-factory
    "header-factory" "GtkListItemFactory" t t)
   (model
    list-view-model
    "model" "GtkSelectionModel" t t)
   (show-separators
    list-view-show-separators
    "show-separators" "gboolean" t t)
   (single-click-activate
    list-view-single-click-activate
    "single-click-activate" "gboolean" t t)
   #+gtk-4-12
   (tab-behavior
    list-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))

#+liber-documentation
(setf (documentation 'list-view 'type)
 "@version{#2023-9-8}
  @begin{short}
    The @class{gtk:list-view} widget is a widget to present a view into a large
    dynamic list of items.
  @end{short}
  The @class{gtk:list-view} widget uses its factory to generate one row widget
  for each visible item and shows them in a linear display, either vertically
  or horizontally. The @slot[gtk:list-view]{show-separators} property offers a
  simple way to display separators between the rows.

  The @class{gtk:list-view} widget allows the user to select items according to
  the selection characteristics of the model. For models that allow multiple
  selected items, it is possible to turn on rubberband selection, using the
  @slot[gtk:list-view]{enable-rubberband} property.

  If you need multiple columns with headers, see the @class{gtk:column-view}
  widget.

  To learn more about the list widget framework, see the overview.
  @begin[Example]{dictionary}
    An example of using the @class{gtk:list-view} widget:
    @begin{pre}
static void
setup_listitem_cb (GtkListItemFactory *factory,
                   GtkListItem        *list_item)
{
  GtkWidget *image;

  image = gtk_image_new ();
  gtk_image_set_icon_size (GTK_IMAGE (image), GTK_ICON_SIZE_LARGE);
  gtk_list_item_set_child (list_item, image);
@}

static void
bind_listitem_cb (GtkListItemFactory *factory,
                  GtkListItem        *list_item)
{
  GtkWidget *image;
  GAppInfo *app_info;

  image = gtk_list_item_get_child (list_item);
  app_info = gtk_list_item_get_item (list_item);
  gtk_image_set_from_gicon (GTK_IMAGE (image), g_app_info_get_icon (app_info));
@}

static void
activate_cb (GtkListView  *list,
             guint         position,
             gpointer      unused)
{
  GAppInfo *app_info;

  app_info = g_list_model_get_item (G_LIST_MODEL (gtk_list_view_get_model (list)), position);
  g_app_info_launch (app_info, NULL, NULL, NULL);
  g_object_unref (app_info);
@}

...

  model = create_application_list ();

  factory = gtk_signal_list_item_factory_new ();
  g_signal_connect (factory, \"setup\", G_CALLBACK (setup_listitem_cb), NULL);
  g_signal_connect (factory, \"bind\", G_CALLBACK (bind_listitem_cb), NULL);

  list = gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new (model)), factory);

  g_signal_connect (list, \"activate\", G_CALLBACK (activate_cb), NULL);

  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (sw), list);
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
listview[.separators][.rich-list][.navigation-sidebar][.data-table]
├── row[.activatable]
│
├── row[.activatable]
│
┊
╰── [rubberband]
    @end{pre}
    The @class{gtk:list-view} implementation uses a single CSS node named
    @code{listview}. It may carry the @code{.separators} style class, when the
    @slot[gtk:list-view]{show-separators} property is set. Each child widget
    uses a single CSS node named row. For rubberband selection, a node with
    name rubberband is used.

    The main @code{listview} node may also carry style classes to select the
    style of list presentation: @code{.rich-list}, @code{.navigation-sidebar}
    or @code{.data-table}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:list-view} implementation uses the @code{:list} role, and
    the list items use the @code{:list-item} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (listview position)    :run-last
      @end{pre}
      The signal is emitted when a row has been activated by the user, usually
      via activating the @code{GtkListView|list.activate-item action}. This
      allows for a convenient way to handle activation in a listview. See the
      @fun{gtk:list-item-activatable} function for details on how to use this
      signal.
      @begin[code]{table}
        @entry[listview]{The @class{gtk:list-view} widget.}
        @entry[position]{An unsigned integer with the position of the item to
        activate.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:list-view-new}
  @see-slot{gtk:list-view-enable-rubberband}
  @see-slot{gtk:list-view-factory}
  @see-slot{gtk:list-view-header-factory}
  @see-slot{gtk:list-view-model}
  @see-slot{gtk:list-view-show-separators}
  @see-slot{gtk:list-view-single-click-activate}
  @see-slot{gtk:list-view-tab-behavior}
  @see-class{gtk:selection-model}
  @see-class{gtk:column-view}
  @see-class{gtk:grid-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- list-view-enable-rubberband --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-rubberband"
                                               'list-view) t)
 "The @code{enable-rubberband} property of type @code{:boolean} (Read / Write)
  @br{}
  Allow rubberband selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-enable-rubberband)
      "Accessor"
      (documentation 'list-view-enable-rubberband 'function)
 "@version{#2023-9-9}
  @syntax[]{(gtk:list-view-enable-rubberband object) => setting}
  @syntax[]{(setf (gtk:list-view-enable-rubberband object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if rubberband selection is enabled}
  @begin{short}
    Accessor of the @slot[gtk:list-view]{enable-rubberband} slot of the
    @class{gtk:list-view} class.
  @end{short}
  The @fun{gtk:list-view-enable-rubberband} function returns whether rows can
  be selected by dragging with the mouse. The
  @sym{(setf gtk:list-view-enable-rubberband)} function sets whether selections
  can be changed by dragging with the mouse.
  @see-class{gtk:list-view}")

;;; --- list-view-factory ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'list-view) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  Factory for populating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-factory)
      "Accessor"
      (documentation 'list-view-factory 'function)
 "@version{#2023-9-6}
  @syntax[]{(gtk:list-view-factory object) => factory}
  @syntax[]{(setf (gtk:list-view-factory object) factory)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use, or
    @code{nil} for none}
  @begin{short}
    Accessor of the @slot[gtk:list-view]{factory} slot of the
    @class{gtk:list-view} class.
  @end{short}
  The @fun{gtk:list-view-factory} function gets the factory that is currently
  used to populate list items. The @sym{(setf gtk:list-view-factory)} function
  sets the factory.
  @see-class{gtk:list-view}
  @see-class{gtk:list-item-factory}")

;;; --- list-view-header-factory -----------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "header-factory" 'list-view) t)
 "The @code{header-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  Factory for creating header widgets. Since 4.12")

;;; --- list-view-model --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'list-view) t)
 "The @code{model} property of type @class{gtk:selection-model} (Read / Write)
  @br{}
  Model for the items displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-model)
      "Accessor"
      (documentation 'list-view-model 'function)
 "@version{#2023-9-6}
  @syntax[]{(gtk:list-view-model object) => model}
  @syntax[]{(setf (gtk:list-view-model object) model)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[model]{a @class{gtk:selection-model} object to use}
  @begin{short}
    Accessor of the @slot[gtk:list-view]{model} slot of the
    @class{gtk:list-view} class.
  @end{short}
  The @fun{gtk:list-view-model} function gets the model that is currently used
  to read the items displayed. The @sym{(setf gtk:list-view-model)} function
  sets the model to use. This must be a @class{gtk:selection-model} object.
  @see-class{gtk:list-view}
  @see-class{gtk:selection-model}")

;;; --- list-view-show-separators ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-separators" 'list-view) t)
 "The @code{show-separators} property of type @code{:boolean} (Read / Write)
  @br{}
  Show separators between rows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-show-separators)
      "Accessor"
      (documentation 'list-view-show-separators 'function)
 "@version{#2023-9-6}
  @syntax[]{(gtk:list-view-show-separators object) => setting}
  @syntax[]{(setf (gtk:list-view-show-separators object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if the list box shows separators}
  @begin{short}
    Accessor of the @slot[gtk:list-view]{show-separators} slot of the
    @class{gtk:list-view} class.
  @end{short}
  The @fun{gtk:list-view-show-separators} function returns whether the list box
  should show separators between rows. The @sym{(setf gtk:list-view-separators)}
  function sets whether the list box should show separators between rows.
  @see-class{gtk:list-view}")

;;; --- list-view-single-click-activate ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-click-activate"
                                               'list-view) t)
 "The @code{single-click-activate} property of type @code{:boolean}
  (Read / Write) @br{}
  Activate rows on single click and select them on hover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-single-click-activate)
      "Accessor"
      (documentation 'list-view-single-click-activate 'function)
 "@version{#2023-9-9}
  @syntax[]{(gtk:list-view-single-click-activate object) => setting}
  @syntax[]{(setf (gtk:list-view-single-click-activate object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if rows are activated on single click}
  @begin{short}
    Accessor of the @slot[gtk:list-view]{single-click-activate} slot of the
    @class{gtk:list-view} class.
  @end{short}
  The @fun{gtk:list-view-single-click-activate} function returns whether rows
  will be activated on single click and selected on hover. The
  @sym{(setf gtk:list-view-single-click-activate)} function sets whether rows
  should be activated on single click and selected on hover.
  @see-class{gtk:list-view}")

;;; --- list-view-tab-behavior -------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "tab-behavior" 'list-view) t)
 "The @code{tab-behavior} property of type @symbol{gtk:list-tab-behavior}
  (Read / Write) @br{}
  Behavior of the @kbd{Tab} key. @br{}
  Default value: @code{:all}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_view_new ()
;;;
;;; GtkWidget *
;;; gtk_list_view_new (GtkSelectionModel *model,
;;;                    GtkListItemFactory *factory);
;;;
;;; Creates a new GtkListView that uses the given factory for mapping items to
;;; widgets.
;;;
;;; The function takes ownership of the arguments, so you can write code like
;;;
;;;  list_view = gtk_list_view_new (create_model(),
;;;    gtk_builder_list_item_factory_new_from_resource ("/resource.ui"));
;;;
;;; model :
;;;     the model to use, or NULL.
;;;
;;; factory :
;;;     The factory to populate items with, or NULL.
;;;
;;; Returns :
;;;     a new GtkListView using the given model and factory
;;; ----------------------------------------------------------------------------

(declaim (inline list-view-new))

(defun list-view-new (model factory)
  (make-instance 'list-view
                 :model model
                 :factory factory))

(export 'list-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_view_scroll_to
;;;
;;; Scrolls to the item at the given position and performs the actions
;;; specified in flags.
;;;
;;; Since 4.12
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_list_view_scroll_to" list-view-scroll-to) :void
  (listview (g:object list-view))
  (pos :uint)
  (flags list-scroll-flags)
  (scroll (g:boxed scroll-info)))

#+gtk-4-12
(export 'list-view-scroll-to)

;;; --- End of file gtk4.list-view.lisp ----------------------------------------
