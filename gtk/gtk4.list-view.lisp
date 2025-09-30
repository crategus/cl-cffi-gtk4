;;; ----------------------------------------------------------------------------
;;; gtk4.list-view.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;;     GtkListScrollFlags
;;;
;;; Accessors
;;;
;;;     gtk_list_view_get_factory
;;;     gtk_list_view_set_factory
;;;     gtk_list_view_get_header_factory                    Since 4.12
;;;     gtk_list-view_set_header_factory                    Since 4.12
;;;     gtk_list_view_get_model
;;;     gtk_list_view_set_model
;;;     gtk_list_view_get_show_separators
;;;     gtk_list_view_set_show_separators
;;;     gtk_list_view_get_single_click_activate
;;;     gtk_list_view_set_single_click_activate
;;;     gtk_list_view_get_enable_rubberband
;;;     gtk_list_view_set_enable_rubberband
;;;     gtk_list_view_get_tab_behavior                      Since 4.12
;;;     gtk_list_view_set_tab_behavior                      Since 4.12
;;;
;;; Functions
;;;
;;;     gtk_list_view_new
;;;     gtk_list_view_scroll_to                             Since 4.12
;;;
;;; Properties (GtkListBase)
;;;
;;;     orientation
;;;
;;; Properties (GtkListView)
;;;
;;;     enable-rubberband
;;;     factory
;;;     header-factory                                      Since 4.12
;;;     model
;;;     show-separators
;;;     single-click-activate
;;;     tab-behavior                                        Since 4.12
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
(gobject:define-genum "GtkListTabBehavior" list-tab-behavior
  (:export t
   :type-initializer "gtk_list_tab_behavior_get_type")
  (:all 0)
  (:item 1)
  (:cell 2))

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-symbol 'list-tab-behavior)
      "GEnum"
      (liber:symbol-documentation 'list-tab-behavior)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkListTabBehavior\" list-tab-behavior
  (:export t
   :type-initializer \"gtk_list_tab_behavior_get_type\")
  (:all 0)
  (:item 1)
  (:cell 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:all]{Cycle through all focusable items of the list.}
      @entry[:item]{Cycle through a single list element, then move focus out of
        the list. Moving focus between items needs to be done with the arrow
        keys.}
      @entry[:cell]{Cycle only through a single cell, then move focus out of the
        list. Moving focus between cells needs to be done with the arrow keys.
        This is only relevant for cell-based widgets like the
        @class{gtk:column-view} widget, otherwise it behaves like @code{:item}.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to configure the focus behavior in the @code{:forward} and
    @code{:backward} direction, like the @kbd{Tab} key in a
    @class{gtk:list-view} widget.
  @end{short}

  Since 4.12
  @see-class{gtk:list-view}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; GtkListScrollFlags
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(gobject:define-gflags "GtkListScrollFlags" list-scroll-flags
  (:export t
   :type-initializer "gtk_list_scroll_flags_get_type")
  (:none 0)
  (:focus #.(ash 1 0))
  (:select #.(ash 1 1)))

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-symbol 'list-scroll-flags)
      "GFlags"
      (liber:symbol-documentation 'list-scroll-flags)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-gflags \"GtkListScrollFlags\" list-scroll-flags
  (:export t
   :type-initializer \"gtk_list_scroll_flags_get_type\")
  (:none 0)
  (:focus #.(ash 1 0))
  (:select #.(ash 1 1)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Do not do anything extra.}
      @entry[:focus]{Focus the target item.}
      @entry[:select]{Select the target item and unselect all other items.}
    @end{simple-table}
  @end{values}
  @begin{short}
    List of actions to perform when scrolling to items in a list widget.
  @end{short}

  Since 4.12
  @see-class{gtk:list-view}")

;;; ----------------------------------------------------------------------------
;;; GtkListBase
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListBase" list-base
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
 "@version{2025-07-27}
  @begin{short}
    The @class{gtk:list-base} class is the abstract base class for GTK list
    widgets.
  @end{short}
  @see-slot{gtk:list-base-orientation}
  @see-class{gtk:list-view}
  @see-class{gtk:grid-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:list-base-orientation ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'list-base) t)
 "The @code{orientation} property of type @sym{gtk:orientation} (Read / Write)
  @br{}
  The orientation of the list widget. @br{}
  Default value: @val[gtk:orientation]{:vertical}")

#+liber-documentation
(setf (liber:alias-for-function 'list-base-orientation)
      "Accessor"
      (documentation 'list-base-orientation 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-base-orientation object) => orientation}
  @syntax{(setf (gtk:list-base-orientation object) orientation)}
  @argument[object]{a @class{gtk:list-base} widget}
  @argument[orientation]{a @sym{gtk:orientation} value}
  @begin{short}
    The accessor for the @slot[gtk:list-base]{orientation} slot of the
    @class{gtk:list-base} class gets or sets the orientation of the list widget.
  @end{short}
  @see-class{gtk:list-base}
  @see-symbol{gtk:orientation}")

;;; ----------------------------------------------------------------------------
;;; GtkListView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListView" list-view
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
 "@version{2025-07-27}
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

  To learn more about the list widget framework, see the List Widget Overview
  section.
  @begin[Examples]{dictionary}
    This is a complete example of how to use the @class{gtk:list-view} widget.
    The example is included in the GTK 4 demo, which comes with the
    @code{cl-cffi-gtk4} library.
    @begin{pre}
(defun create-application-list ()
  (let ((store (g:list-store-new \"GAppInfo\"))
        (apps (g:app-info-all)))
    (dolist (app apps)
      (g:list-store-append store app))
  store))

(defun do-list-view-applauncher (&optional application)
  (let* ((factory (gtk:signal-list-item-factory-new))
         (model (create-application-list))
         (listview (gtk:list-view-new (gtk:single-selection-new model) factory))
         (scrolled (gtk:scrolled-window-new))
         (window (make-instance 'gtk:window
                                :title \"Application launcher\"
                                :application application
                                :child scrolled
                                :default-width 600
                                :default-height 380)))
    (g:signal-connect factory \"setup\"
        (lambda (factory item)
          (declare (ignore factory))
          (let ((box (gtk:box-new :horizontal 12))
                (image (make-instance 'gtk:image
                                      :icon-size :large)))
            (gtk:box-append box image)
            (gtk:box-append box (gtk:label-new \"\"))
            (setf (gtk:list-item-child item) box))))
    (g:signal-connect factory \"bind\"
        (lambda (factory item)
          (declare (ignore factory))
          (let* ((image (gtk:widget-first-child (gtk:list-item-child item)))
                 (label (gtk:widget-next-sibling image))
                 (appinfo (gtk:list-item-item item)))
            (gtk:image-set-from-gicon image (g:app-info-icon appinfo))
            (setf (gtk:label-label label)
                  (g:app-info-display-name appinfo)))))
    (g:signal-connect listview \"activate\"
        (lambda (listview pos)
          (let* ((model (gtk:list-view-model listview))
                 (appinfo (g:list-model-item model pos))
                 (display (gtk:widget-display listview))
                 (context (gdk:display-app-launch-context display)))
            (unless (g:app-info-launch appinfo nil context)
              (let* ((message (format nil \"Could not launch ~a\"
                                          (g:app-info-display-name appinfo)))
                     (dialog (make-instance 'gtk:alert-dialog
                                            :message message)))
                (gtk:alert-dialog-show dialog (gtk:widget-root listview)))))))
    (setf (gtk:scrolled-window-child scrolled) listview)
    (gtk:window-present window)))
    @end{pre}
  @end{dictionary}
  @begin[Actions]{dictionary}
    The @class{gtk:list-view} implementation defines a set of built-in actions:
    @begin[code]{table}
      @entry[list.activate-item]{Activates the item at given position by
        emitting the @code{GtkListView::activate} signal.}
    @end{table}
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
    name rubberband is used. The main @code{listview} node may also carry
    @code{.rich-list}, @code{.navigation-sidebar} or @code{.data-table} style
    classes to select the style of list presentation.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:list-view} implementation uses the
    @val[gtk:accessible-role]{:list} role, and the list items use the
    @val[gtk:accessible-role]{:list-item} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[list-view::activate]{signal}
      @begin{pre}
lambda (listview position)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[listview]{The @class{gtk:list-view} widget.}
        @entry[position]{The unsigned integer for the position of the item to
        activate.}
      @end{simple-table}
      The signal is emitted when a row has been activated by the user, usually
      via activating the @code{GtkListView::list.activate-item} action. This
      allows for a convenient way to handle activation in a list view. See the
      @fun{gtk:list-item-activatable} function for details on how to use this
      signal.
    @end{signal}
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

;;; --- gtk:list-view-enable-rubberband ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-rubberband"
                                               'list-view) t)
 "The @code{enable-rubberband} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to allow rubberband selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-enable-rubberband)
      "Accessor"
      (documentation 'list-view-enable-rubberband 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-enable-rubberband object) => setting}
  @syntax{(setf (gtk:list-view-enable-rubberband object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if rubberband selection is enabled}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{enable-rubberband} slot of the
    @class{gtk:list-view} class gets or sets whether rows can be selected by
    dragging with the mouse.
  @end{short}
  @see-class{gtk:list-view}")

;;; --- gtk:list-view-factory --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'list-view) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for populating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-factory)
      "Accessor"
      (documentation 'list-view-factory 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-factory object) => factory}
  @syntax{(setf (gtk:list-view-factory object) factory)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use, or
    @code{nil} for none}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{factory} slot of the
    @class{gtk:list-view} class gets or sets the factory that is currently used
    to populate list items.
  @end{short}
  @see-class{gtk:list-view}
  @see-class{gtk:list-item-factory}")

;;; --- gtk:list-view-header-factory -------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "header-factory" 'list-view) t)
 "The @code{header-factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for creating header widgets. Since 4.12")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'list-view-header-factory)
      "Accessor"
      (documentation 'list-view-header-factory 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-header-factory object) => factory}
  @syntax{(setf (gtk:list-view-header-factory object) factory)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{header-factory} slot of the
    @class{gtk:list-view} class gets or sets the factory to use for populating
    the @class{gtk:list-header} objects used in section headers.
  @end{short}
  If this factory is set to @code{nil}, the list will not show section headers.

  Since 4.12
  @see-class{gtk:list-view}
  @see-class{gtk:list-item-factory}
  @see-class{gtk:list-header}")

;;; --- gtk:list-view-model ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'list-view) t)
 "The @code{model} property of type @class{gtk:selection-model} (Read / Write)
  @br{}
  The model for the items displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-model)
      "Accessor"
      (documentation 'list-view-model 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-model object) => model}
  @syntax{(setf (gtk:list-view-model object) model)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[model]{a @class{gtk:selection-model} object to use}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{model} slot of the
    @class{gtk:list-view} class gets or sets the model that is currently used to
    read the items displayed.
  @end{short}
  This must be a @class{gtk:selection-model} object.
  @see-class{gtk:list-view}
  @see-class{gtk:selection-model}")

;;; --- gtk:list-view-show-separators ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-separators" 'list-view) t)
 "The @code{show-separators} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show separators between rows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-show-separators)
      "Accessor"
      (documentation 'list-view-show-separators 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-show-separators object) => setting}
  @syntax{(setf (gtk:list-view-show-separators object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if the list box shows separators}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{show-separators} slot of the
    @class{gtk:list-view} class gets or sets whether the list box should show
    separators between rows.
  @end{short}
  @see-class{gtk:list-view}")

;;; --- gtk:list-view-single-click-activate ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-click-activate"
                                               'list-view) t)
 "The @code{single-click-activate} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to activate rows on single click and select them on hover. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-view-single-click-activate)
      "Accessor"
      (documentation 'list-view-single-click-activate 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-single-click-activate object) => setting}
  @syntax{(setf (gtk:list-view-single-click-activate object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{@em{true} if rows are activated on single click}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{single-click-activate} slot of the
    @class{gtk:list-view} class gets or sets whether rows will be activated on
    single click and selected on hover.
  @end{short}
  @see-class{gtk:list-view}")

;;; --- gtk:list-view-tab-behavior ---------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "tab-behavior" 'list-view) t)
 "The @code{tab-behavior} property of type @sym{gtk:list-tab-behavior}
  (Read / Write) @br{}
  The behavior of the @kbd{Tab} and @kbd{Shift+Tab} keys. Since 4.12 @br{}
  Default value: @val[gtk:list-tab-behavior]{:all}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'list-view-tab-behavior)
      "Accessor"
      (documentation 'list-view-tab-behavior 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-view-tab-behavior object) => setting}
  @syntax{(setf (gtk:list-view-tab-behavior object) setting)}
  @argument[object]{a @class{gtk:list-view} object}
  @argument[setting]{a @sym{gtk:list-tab-behavior} value}
  @begin{short}
    The accessor for the @slot[gtk:list-view]{tab-behavior} slot of the
    @class{gtk:list-view} class gets or sets the behavior set for the @kbd{Tab}
    and @kbd{Shift+Tab} keys.
  @end{short}

  Since 4.12
  @see-class{gtk:list-view}
  @see-symbol{gtk:list-tab-behavior}")

;;; ----------------------------------------------------------------------------
;;; gtk_list_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline list-view-new))

(defun list-view-new (&optional model factory)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[model]{an optional @class{gtk:selection-model} object to use, or
    the default @code{nil} value}
  @argument[factory]{an optional @class{gtk:list-item-factory} object to
    populate items with or the default @code{nil} value}
  @begin{return}
    The new @class{gtk:list-view} widget using the given @arg{model} and
    @arg{factory}.
  @end{return}
  @begin{short}
    Creates a new list view that uses the given @arg{factory} for mapping items
    to widgets.
  @end{short}
  @see-class{gtk:list-view}
  @see-class{gtk:selection-model}
  @see-class{gtk:list-item-factory}"
  (make-instance 'list-view
                 :model model
                 :factory factory))

(export 'list-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_list_view_scroll_to
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_list_view_scroll_to" %list-view-scroll-to) :void
  (listview (g:object list-view))
  (pos :uint)
  (flags list-scroll-flags)
  (scroll (g:boxed scroll-info)))

#+gtk-4-12
(defun list-view-scroll-to (listview pos flags &optional scroll)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[listview]{a @class{gtk:list-view} widget}
  @argument[pos]{an unsigned integer for the position of the item}
  @argument[flags]{a @sym{gtk:list-scroll-flags} value for the actions to
    perform}
  @argument[scroll]{an optional @class{gtk:scroll-info} instance for the details
    of how to perform the scroll operation or the default @code{nil} value to
    scroll into view}
  @begin{short}
    Scrolls to the item at the given position and performs the actions
    specified in @arg{flags}.
  @end{short}
  This function works no matter if the list view is shown or focused. If it
  is not, then the changes will take effect once that happens.

  Since 4.12
  @see-class{gtk:list-view}
  @see-class{gtk:scroll-info}
  @see-symbol{gtk:list-scroll-flags}"
  (%list-view-scroll-to listview pos flags scroll))

#+gtk-4-12
(export 'list-view-scroll-to)

;;; --- End of file gtk4.list-view.lisp ----------------------------------------
