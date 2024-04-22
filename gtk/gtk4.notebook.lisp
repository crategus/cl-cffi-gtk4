;;; ----------------------------------------------------------------------------
;;; gtk4.notebook.lisp
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
;;; GtkNotebook
;;;
;;;     A tabbed notebook container
;;;
;;; Types and Values
;;;
;;;     GtkNotebookTab
;;;     GtkNotebookPage
;;;
;;; Accessors
;;;
;;;     gtk_notebook_page_get_child
;;;
;;; Properties
;;;
;;;     child
;;;     detachable
;;;     menu
;;;     menu-label
;;;     position
;;;     reorderable
;;;     tab
;;;     tab-expand
;;;     tab-fill
;;;     tab-label
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkNotebook
;;;
;;; Accessors
;;;
;;;     gtk_notebook_set_group_name
;;;     gtk_notebook_get_group_name
;;;     gtk_notebook_get_page
;;;     gtk_notebook_get_pages
;;;     gtk_notebook_get_scrollable
;;;     gtk_notebook_set_scrollable
;;;     gtk_notebook_get_show_border
;;;     gtk_notebook_set_show_border
;;;     gtk_notebook_get_show_tabs
;;;     gtk_notebook_set_show_tabs
;;;     gtk_notebook_get_tab_pos
;;;     gtk_notebook_set_tab_pos
;;;
;;; Functions
;;;
;;;     gtk_notebook_new
;;;     gtk_notebook_append_page
;;;     gtk_notebook_append_page_menu
;;;     gtk_notebook_prepend_page
;;;     gtk_notebook_prepend_page_menu
;;;     gtk_notebook_insert_page
;;;     gtk_notebook_insert_page_menu
;;;     gtk_notebook_remove_page
;;;     gtk_notebook_detach_tab
;;;     gtk_notebook_page_num
;;;     gtk_notebook_next_page
;;;     gtk_notebook_prev_page
;;;     gtk_notebook_reorder_child
;;;     gtk_notebook_popup_enable
;;;     gtk_notebook_popup_disable
;;;     gtk_notebook_get_current_page
;;;     gtk_notebook_get_menu_label
;;;     gtk_notebook_get_nth_page
;;;     gtk_notebook_get_n_pages
;;;     gtk_notebook_get_tab_label
;;;     gtk_notebook_set_menu_label
;;;     gtk_notebook_set_menu_label_text
;;;     gtk_notebook_set_tab_label
;;;     gtk_notebook_set_tab_label_text
;;;     gtk_notebook_set_tab_reorderable
;;;     gtk_notebook_set_tab_detachable
;;;     gtk_notebook_get_menu_label_text
;;;     gtk_notebook_get_tab_label_text
;;;     gtk_notebook_get_tab_reorderable
;;;     gtk_notebook_get_tab_detachable
;;;     gtk_notebook_set_current_page
;;;     gtk_notebook_set_action_widget
;;;     gtk_notebook_get_action_widget
;;;
;;; Properties
;;;
;;;     enable-popup
;;;     group-name
;;;     page
;;;     pages
;;;     scrollable
;;;     show-border
;;;     show-tabs
;;;     tab-pos
;;;
;;; Signals
;;;
;;;     change-current-page
;;;     create-window
;;;     focus-tab
;;;     move-focus-out
;;;     page-added
;;;     page-removed
;;;     page-reordered
;;;     reorder-tab
;;;     select-page
;;;     switch-page
;;;
;;; Actions
;;;
;;;     menu.popup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ├── GInitiallyUnowned
;;;     │   ╰── GtkWidget
;;;     │       ╰── GtkNotebook
;;;     ╰── GtkNotebookPage
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNotebookTab
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkNotebookTab" notebook-tab
  (:export t
   :type-initializer "gtk_notebook_tab_get_type")
  (:tab-first 0)
  (:tab-last 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'notebook-tab)
      "GEnum"
      (liber:symbol-documentation 'notebook-tab)
 "@version{2024-4-16}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkNotebookTab\" notebook-tab
  (:export t
   :type-initializer \"gtk_notebook_tab_get_type\")
  (:tab-first 0)
  (:tab-last 1))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:tab-first]{}
      @entry[:tab-last]{}
    @end{table}
  @end{values}
  @begin{short}
    The values of this enumeration are used as arguments of the
    @code{\"focus-tab\"} signal.
  @end{short}
  @see-class{gtk:notebook}")

;;; ----------------------------------------------------------------------------
;;; GtkNotebookPage
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNotebookPage" notebook-page
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_notebook_page_get_type")
  ((child
    notebook-page-child
    "child" "GtkWidget" t t)
   (detachable
    notebook-page-detachable
    "detachable" "gboolean" t t)
   (menu
    notebook-page-menu
    "menu" "GtkWidget" t t)
   (menu-label
    notebook-page-menu-label
    "menu-label" "gchararray" t t)
   (position
    notebook-page-position
    "position" "gint" t t)
   (reorderable
    notebook-page-reorderable
    "reorderable" "gboolean" t t)
   (tab
    notebook-page-tab
    "tab" "GtkWidget" t t)
   (tab-expand
    notebook-page-tab-expand
    "tab-expand" "gboolean" t t)
   (tab-fill
    notebook-page-tab-fill
    "tab-fill" "gboolean" t t)
   (tab-label
    notebook-page-tab-label
    "tab-label" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'notebook-page 'type)
 "@version{2024-4-16}
  @begin{short}
    A page in the @class{gtk:notebook} widget.
  @end{short}
  @see-slot{gtk:notebook-page-child}
  @see-slot{gtk:notebook-page-detachable}
  @see-slot{gtk:notebook-page-menu}
  @see-slot{gtk:notebook-page-menu-label}
  @see-slot{gtk:notebook-page-position}
  @see-slot{gtk:notebook-page-reorderable}
  @see-slot{gtk:notebook-page-tab}
  @see-slot{gtk:notebook-page-tab-expand}
  @see-slot{gtk:notebook-page-tab-fill}
  @see-slot{gtk:notebook-page-tab-label}
  @see-class{gtk:notebook}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:notebook-page-child ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'notebook-page) t)
 "The @code{child} property of type @class{gtk:widget}
  (Read / Write / Construct only) @br{}
  The child widget for the notebook page.")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-child)
      "Accessor"
      (documentation 'notebook-page-child 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-child object) => child}
  @syntax{(setf (gtk:notebook-page-child object) child)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{child} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The child widget for the notebook page.
  @see-class{gtk:notebook-page}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-page-detachable -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "detachable" 'notebook-page) t)
 "The @code{detachable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tab is detachable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-detachable)
      "Accessor"
      (documentation 'notebook-page-detachable 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-detachable object) => detachable}
  @syntax{(setf (gtk:notebook-page-detachable object) detachable)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[detachable]{a boolean whether the tab is detachable}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{detachable} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  Whether the tab is detachable.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-menu -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu" 'notebook-page) t)
 "The @code{menu} property of type @class{gtk:widget}
  (Read / Write / Construct only) @br{}
  The label widget displayed in the menu entry of the child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-menu)
      "Accessor"
      (documentation 'notebook-page-menu 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-menu object) => menu}
  @syntax{(setf (gtk:notebook-page-menu object) menu)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[menu]{a @class{gtk:widget} label widget displayed in the menu entry
    of the child widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{menu} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The label widget displayed in the menu entry of the child widget.
  @see-class{gtk:notebook-page}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-page-menu-label -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-label" 'notebook-page) t)
 "The @code{menu-label} property of type @code{:string} (Read / Write) @br{}
  The text of the menu widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-menu-label)
      "Accessor"
      (documentation 'notebook-page-menu-label 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-menu-label object) => label}
  @syntax{(setf (gtk:notebook-page-menu-label object) label)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[label]{a string with the text of the menu widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{menu-label} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The text of the menu widget.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-position ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'notebook-page) t)
 "The @code{position} property of type @code{:int} (Read / Write) @br{}
  The index of the child widget in the parent. @br{}
  Allowed values: >= -1 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-position)
      "Accessor"
      (documentation 'notebook-page-position 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-position object) => position}
  @syntax{(setf (gtk:notebook-page-position object) position)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[position]{an integer with the index of the child widget in the
    parent}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{position} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The index of the child widget in the parent.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-reorderable ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "reorderable" 'notebook-page) t)
 "The @code{reorderable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tab is reorderable by user action. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-reorderable)
      "Accessor"
      (documentation 'notebook-page-reorderable 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-reorderable object) => reorderable}
  @syntax{(setf (gtk:notebook-page-reorderable object) reorderable)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[reorderable]{a boolean whether the tab is reorderable}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{reorderable} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  Whether the tab is reorderable by user action.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-tab --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab" 'notebook-page) t)
 "The @code{tab} property of type @class{gtk:widget}
  (Read / Write / Construct only) @br{}
  The tab widget for the notebook page.")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-tab)
      "Accessor"
      (documentation 'notebook-page-tab 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-tab object) => tab}
  @syntax{(setf (gtk:notebook-page-tab object) tab)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[tab]{a @class{gtk:widget} tab widget for the notebook page}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{tab} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The tab widget for the notebook page.
  @see-class{gtk:notebook-page}
  @see-class{gtk:widget}")

;;; --- gtk:notebook-page-tab-expand -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab-expand" 'notebook-page) t)
 "The @code{tab-expand} property of type @code{:boolean} (Read / Write) @br{}
  Whether to expand the tab of the child widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-tab-expand)
      "Accessor"
      (documentation 'notebook-page-tab-expand 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-tab-expand object) => expand}
  @syntax{(setf (gtk:notebook-page-tab-expand object) expand)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[expand]{a boolean whether to expand the tab of the child widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{tab-expand} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  Whether to expand the tab of the child widget.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-tab-fill ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab-fill" 'notebook-page) t)
 "The @code{tab-fill} property of type @code{:boolean} (Read / Write) @br{}
  Whether the tab of the child widget should fill the allocated area. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-tab-fill)
      "Accessor"
      (documentation 'notebook-page-tab-fill 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-tab-fill object) => fill}
  @syntax{(setf (gtk:notebook-page-tab-fill object) fill)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[fill]{a boolean whether to expand the tab of the child widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{tab-fill} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  Whether to expand the tab of the child widget.
  @see-class{gtk:notebook-page}")

;;; --- gtk:notebook-page-tab-label --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab-label" 'notebook-page) t)
 "The @code{tab-label} property of type @code{:string} (Read / Write) @br{}
  The text of the tab widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page-tab-label)
      "Accessor"
      (documentation 'notebook-page-tab-label 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page-tab-label object) => label}
  @syntax{(setf (gtk:notebook-page-tab-label object) label)}
  @argument[object]{a @class{gtk:notebook-page} object}
  @argument[label]{a string with the text of the tab widget}
  @begin{short}
    Accessor of the @slot[gtk:notebook-page]{tab-label} slot of the
    @class{gtk:notebook-page} class.
  @end{short}
  The text of the tab widget.
  @see-class{gtk:notebook-page}")

;;; ----------------------------------------------------------------------------
;;; GtkNotebook
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkNotebook" notebook
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_notebook_get_type")
  ((enable-popup
    notebook-enable-popup
    "enable-popup" "gboolean" t t)
   (group-name
    notebook-group-name
    "group-name" "gchararray" t t)
   (page
    notebook-page
    "page" "gint" t t)
   (pages
    notebook-pages
    "pages" "GListModel" t nil)
   (scrollable
    notebook-scrollable
    "scrollable" "gboolean" t t)
   (show-border
    notebook-show-border
    "show-border" "gboolean" t t)
   (show-tabs
    notebook-show-tabs
    "show-tabs" "gboolean" t t)
   (tab-pos
    notebook-tab-pos
    "tab-pos" "GtkPositionType" t t)))

#+liber-documentation
(setf (documentation 'notebook 'type)
 "@version{2024-4-17}
  @begin{short}
    The @class{gtk:notebook} widget is a layout container whose children are
    pages that can be switched between using tab labels along one edge.
  @end{short}

  @image[notebook]{Figure: GtkNotebook}

  There are many configuration options for @class{gtk:notebook} widgets. Among
  other things, you can choose on which edge the tabs appear, see the
  @fun{gtk:notebook-tab-pos} function, whether, if there are too many tabs to
  fit the notebook should be made bigger or scrolling arrows added, see the
  @fun{gtk:notebook-scrollable} function, and whether there will be a popup menu
  allowing the users to switch pages, see the @fun{gtk:notebook-popup-enable}
  and @fun{gtk:notebook-popup-disable} functions.
  @begin[GtkNotebook as GtkBuildable]{dictionary}
    The @class{gtk:notebook} implementation of the @class{gtk:buildable}
    interface supports placing children into tabs by specifying @code{\"tab\"}
    as the @code{\"type\"} attribute of a @code{<child>} element. Note that the
    content of the tab must be created before the tab can be filled. A tab child
    can be specified without specifying a @code{<child>} type attribute. To add
    a child widget in the notebooks action area, specify @code{\"action-start\"}
    or @code{\"action-end\"} as the @code{\"type\"} attribute of the
    @code{<child>} element.

    @b{Example:} A UI definition fragment with the @class{gtk:notebook} widget
    @begin{pre}
<object class=\"GtkNotebook\">
  <child>
    <object class=\"GtkLabel\" id=\"notebook-content\">
      <property name=\"label\">Content</property>
    </object>
  </child>
  <child type=\"tab\">
    <object class=\"GtkLabel\" id=\"notebook-tab\">
      <property name=\"label\">Tab</property>
    </object>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
notebook
├── header.top
│   ├── [<action widget>]
│   ├── tabs
│   │   ├── [arrow]
│   │   ├── tab
│   │   │   ╰── <tab label>
│   │   │
│   │   ├── tab[.reorderable-page]
│   │   │   ╰── <tab label>
│   │   ╰── [arrow]
│   ╰── [<action widget>]
│
╰── stack
    ├── <child>
    │
    ╰── <child>
    @end{pre}
    The @class{gtk:notebook} implementation has a main CSS node with name
    @code{notebook}, a subnode with name @code{header} and below that a subnode
    with name @code{tabs} which contains one subnode per tab with name
    @code{tab}. If action widgets are present, their CSS nodes are placed next
    to the tabs node. If the notebook is scrollable, CSS nodes with name
    @code{arrow} are placed as first and last child of the tabs node. The main
    node gets the @code{.frame} style class when the notebook has a border, see
    the @fun{gtk:notebook-show-border} function. The header node gets one of
    the @code{.top}, @code{.bottom}, @code{.left} or @code{.right} style
    classes, depending on where the tabs are placed. For reorderable pages, the
    tab node gets the @code{.reorderable-page} style class. A tab node gets the
    @code{.dnd} style class while it is moved with drag and drop. The nodes are
    always arranged from left-to-right, regardless of text direction.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:notebook} implementation uses the following roles:
    @begin{itemize}
      @item{The @code{:group} role for the notebook widget.}
      @item{The @code{:tab-list} role for the list of tabs.}
      @item{The @code{:tab} role for each tab.}
      @item{The @code{:tab-panel} role for each page.}
    @end{itemize}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"change-current-page\" signal}
      @begin{pre}
lambda (notebook offset)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[offset]{The integer with the offset to step forward or backward
          for a negative integer.}
      @end{table}
    @subheading{The \"create-window\" signal}
      @begin{pre}
lambda (notebook page)    :run-last
      @end{pre}
      The signal is emitted when a detachable tab is dropped on the root window.
      A handler for this signal can create a window containing a notebook where
      the tab will be attached. It is also responsible for moving/resizing the
      window and adding the necessary properties to the notebook, e.g. the
      @code{group-name} property.
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk:widget} tab of @arg{notebook} that is being
          detached.}
        @entry[Returns]{The @class{gtk:notebook} widget that @arg{page} should
          be added to, or @code{nil}.}
      @end{table}
    @subheading{The \"focus-tab\" signal}
      @begin{pre}
lambda (notebook tab)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[tab]{The value of the @symbol{gtk:notebook-tab} enumeration.}
      @end{table}
    @subheading{The \"move-focus-out\" signal}
      @begin{pre}
lambda (notebook direction)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[direction]{The value of the @symbol{gtk:direction-type}
          enumeration.}
      @end{table}
    @subheading{The \"page-added\" signal}
      @begin{pre}
lambda (notebook child num)    :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page is added to the
      notebook.
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"page-removed\" signal}
      @begin{pre}
lambda (notebook child num)   :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page is removed from
      the notebook.
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"page-reordered\" signal}
      @begin{pre}
lambda (notebook child num)    :run-last
      @end{pre}
      The signal is emitted in the notebook right after a page has been
      reordered.
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[child]{The @class{gtk:widget} child page affected.}
        @entry[num]{The unsigned integer with the child page number.}
      @end{table}
    @subheading{The \"reorder-tab\" signal}
      @begin{pre}
lambda (notebook direction move-to-last)   :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[direction]{The value of the @symbol{gtk:direction-type}
          enumeration.}
        @entry[move-to-last]{The boolean.}
      @end{table}
    @subheading{The \"select-page\" signal}
      @begin{pre}
lambda (notebook move-focus)    :action
      @end{pre}
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[move-focus]{The boolean.}
      @end{table}
    @subheading{The \"switch-page\" signal}
      @begin{pre}
lambda (notebook page num)    :run-last
      @end{pre}
      Emitted when the user or a function changes the current page.
      @begin[code]{table}
        @entry[notebook]{The @class{gtk:notebook} widget emitting the signal.}
        @entry[page]{The @class{gtk:widget} current page.}
        @entry[num]{The unsigned integer with the index of the page.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:notebook-new}
  @see-slot{gtk:notebook-enable-popup}
  @see-slot{gtk:notebook-group-name}
  @see-slot{gtk:notebook-page}
  @see-slot{gtk:notebook-scrollable}
  @see-slot{gtk:notebook-show-border}
  @see-slot{gtk:notebook-show-tabs}
  @see-slot{gtk:notebook-tab-pos}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:notebook-enable-popup ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-popup" 'notebook) t)
 "The @code{enable-popup} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-enable-popup)
      "Accessor"
      (documentation 'notebook-enable-popup 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-enable-popup object) => enable}
  @syntax{(setf (gtk:notebook-enable-popup object) enable)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[enable]{if @em{true}, pops up a menu}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{enable-popup} slot of the
    @class{gtk:notebook} class.
  @end{short}
  If @em{true}, pressing the right mouse button on the notebook pops up a menu
  that you can use to go to a page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-enable}
  @see-function{gtk:notebook-popup-disable}")

;;; --- gtk:notebook-group-name ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group-name" 'notebook) t)
 "The @code{group-name} property of type @code{:string} (Read / Write) @br{}
  Group name for tab drag and drop. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-group-name)
      "Accessor"
      (documentation 'notebook-group-name 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-group-name object) => name}
  @syntax{(setf (gtk:notebook-group-name object) name)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[name]{a string with the name of the notebook group, or @code{nil}
    to unset it}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{group-name} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-group-name} function gets the current group name for the
  notebook. The @setf{gtk:notebook-group-name} function sets a group name.

  Notebooks with the same name will be able to exchange tabs via drag and
  drop. A notebook with a @code{nil} group name will not be able to exchange
  tabs with any other notebook.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-page ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page" 'notebook) t)
 "The @code{page} property of type @code{:int} (Read / Write) @br{}
  The index of the current page. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-page)
      "Accessor"
      (documentation 'notebook-page 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-page object) => page}
  @syntax{(setf (gtk:notebook-page object) page)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[page]{an integer with the index of the current page}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{page} slot of the @class{gtk:notebook}
    class.
  @end{short}
  The index of the current page.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-pages -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pages" 'notebook) t)
 "The @code{pages} property of type @class{g:list-model} (Read) @br{}
  The pages of the notebook.")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-pages)
      "Accessor"
      (documentation 'notebook-pages 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-pages object) => pages}
  @syntax{(setf (gtk:notebook-pages object) pages)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[pages]{a @class{g:list-model} object with the pages of the notebook}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{pages} slot of the @class{gtk:notebook}
    class.
  @end{short}
  The @fun{gtk:notebook-pages} function returns a @class{g:list-model} object
  that contains the pages of the notebook, and can be used to keep an up-to-date
  view.
  @see-class{gtk:notebook}
  @see-class{g:list-model}")

;;; --- gtk:notebook-scrollable ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scrollable" 'notebook) t)
 "The @code{scrollable} property of type @code{:boolean} (Read / Write) @br{}
  If @em{true}, scroll arrows are added if there are too many tabs to fit. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-scrollable)
      "Accessor"
      (documentation 'notebook-scrollable 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-scrollable object) => scrollable}
  @syntax{(setf (gtk:notebook-scrollable object) scrollable)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[scrollable]{@em{true} if scroll arrows should be added}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{scrollable} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-scrollable} function returns whether the tab label area
  has arrows for scrolling if there are too many tabs to fit in the area. The
  @setf{gtk:notebook-scrollable} function sets whether the tab label area will
  have arrows for scrolling.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-show-border -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-border" 'notebook) t)
 "The @code{show-border} property of type @code{:boolean} (Read / Write) @br{}
  Whether the border should be shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-show-border)
      "Accessor"
      (documentation 'notebook-show-border 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-show-border object) => show-border}
  @syntax{(setf (gtk:notebook-show-border object) show-border)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[show-border]{@em{true} if a bevel should be drawn around the
    notebook}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{show-border} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-show-border} function returns whether a bevel will be
  drawn around the notebook pages. The @setf{gtk:notebook-show-border} function
  sets whether a bevel will be drawn.

  This only has a visual effect when the tabs are not shown. See the
  @fun{gtk:notebook-show-tabs} function.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-show-tabs}")

;;; --- gtk:notebook-show-tabs -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-tabs" 'notebook) t)
 "The @code{show-tabs} property of type @code{:boolean} (Read / Write) @br{}
  Whether tabs should be shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-show-tabs)
      "Accessor"
      (documentation 'notebook-show-tabs 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-show-tabs object) => show-tabs}
  @syntax{(setf (gtk:notebook-show-tabs object) show-tabs)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[show-tabs]{@em{true} if the tabs should be shown}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{show-tabs} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-show-tabs} function returns whether the tabs of the
  notebook are shown. The @setf{gtk:notebook-show-tabs} function sets whether
  to show the tabs.
  @see-class{gtk:notebook}")

;;; --- gtk:notebook-tab-pos ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tab-pos" 'notebook) t)
 "The @code{tab-pos} property of type @symbol{gtk:position-type} (Read / Write)
  @br{}
  Which side of the notebook holds the tabs. @br{}
  Default value: @code{:top}")

#+liber-documentation
(setf (liber:alias-for-function 'notebook-tab-pos)
      "Accessor"
      (documentation 'notebook-tab-pos 'function)
 "@version{2024-4-22}
  @syntax{(gtk:notebook-tab-pos object) => pos}
  @syntax{(setf (gtk:notebook-tab-pos object) pos)}
  @argument[object]{a @class{gtk:notebook} widget}
  @argument[pos]{a value of the @symbol{gtk:position-type} enumeration with the
    edge to draw the tabs at}
  @begin{short}
    Accessor of the @slot[gtk:notebook]{tab-pos} slot of the
    @class{gtk:notebook} class.
  @end{short}
  The @fun{gtk:notebook-tab-pos} function gets the edge at which the tabs for
  switching pages in the notebook are drawn. The @setf{gtk:notebook-tab-pos}
  function sets the edge.
  @see-class{gtk:notebook}
  @see-symbol{gtk:position-type}")

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_new
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-new))

(defun notebook-new ()
 #+liber-documentation
 "@version{2024-4-22}
  @return{The newly created @class{gtk:notebook} widget.}
  @short{Creates a new notebook with no pages.}
  @see-class{gtk:notebook}"
  (make-instance 'notebook))

(export 'notebook-new)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_append_page" notebook-append-page) :int
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_append_page_menu                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_append_page_menu" notebook-append-page-menu) :int
  (notebook (g:object widget))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page                              not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prepend_page" notebook-prepend-page) :int
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prepend_page_menu                         not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prepend_page_menu" notebook-prepend-page-menu) :int
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget)))

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_insert_page" notebook-insert-page) :int
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (pos :int))

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_insert_page_menu                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_insert_page_menu" notebook-insert-page-menu) :int
  (notebook (g:object notebook))
  (child (g:object widget))
  (tab (g:object widget))
  (menu (g:object widget))
  (pos :int))

;;; ----------------------------------------------------------------------------
;;; gtk:notebook-add-page                                   Lisp extension
;;; ----------------------------------------------------------------------------

(defun notebook-add-page (notebook child tab &key (pos :end) menu)
 #+liber-documentation
 "@version{2024-4-22}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child widget to use as the content of
    the page}
  @argument[tab]{a @class{gtk:widget} object to use as the label for the page,
    or @code{nil} to use the default label, 'page N'}
  @argument[pos]{an integer with the index starting at 0 at which to insert the
    page, or -1 to append the page after all other pages, or @code{:end} to
    append the page, @code{:start} to prepend the page, the default value is
    @code{:end}}
  @argument[menu]{a @class{gtk:widget} object to use as a label for the
    page-switch menu, if that is enabled}
  @begin{return}
    The integer with the index starting from 0 of the added page in the
    notebook, or -1 if the function fails.
  @end{return}
  @begin{short}
    Inserts a page into the notebook depending on the value of the @arg{pos}
    keyword argument with the @code{:end} default value:
  @end{short}
  @begin[code]{table}
    @begin[:start]{entry}
      Prepends a page to the notebook. This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_notebook_prepend_page()}}
        @item{@code{gtk_notebook_prepend_page_menu()}}
      @end{itemize}
    @end{entry}
    @begin[:end]{entry}
      Appends a page to the notebook. This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_notebook_append_page()}}
        @item{@code{gtk_notebook_append_page_menu()}}
      @end{itemize}
    @end{entry}
    @begin[otherwise]{entry}
      Insert a page into the notebook at the given @arg{pos}, which is an
      integer with the index starting from 0. This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_notebook_insert_page()}}
        @item{@code{gtk_notebook_insert_page_menu()}}
      @end{itemize}
    @end{entry}
  @end{table}
  If the @arg{menu} optinal argument is @code{nil}, that is the default value,
  and the @arg{tab} argument is a @class{gtk:label} widget or @code{nil}, then
  the menu label will be a newly created label with the same text as @arg{tab}.
  If the @arg{tab} argument is not a @class{gtk:label} widget, the @arg{menu}
  argument must be specified if the page-switch menu is to be used.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:label}"
  (assert (typep pos '(or integer (member :start :end))))
  (assert (typep menu '(or null g:object (member :default))))
  (case pos
    (:end
      (if menu
          (notebook-append-page-menu notebook
                                         child
                                         tab
                                         (if (eq menu :default)
                                             (cffi:null-pointer)
                                             menu))
          (notebook-append-page notebook child tab)))
    (:start
     (if menu
         (notebook-prepend-page-menu notebook
                                         child
                                         tab
                                         (if (eq menu :default)
                                             (cffi:null-pointer)
                                             menu))
         (notebook-prepend-page notebook child tab)))
    (otherwise
     (if menu
         (notebook-insert-page-menu notebook
                                        child
                                        tab
                                        (if (eq menu :default)
                                            (cffi:null-pointer)
                                            menu)
                                        pos)
         (notebook-insert-page notebook
                                   child
                                   tab
                                   pos)))))

(export 'notebook-add-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_remove_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_remove_page" %notebook-remove-page) :void
  (notebook (g:object notebook))
  (num :int))

(defun notebook-remove-page (notebook page-or-number)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[page-or-number]{an integer with the index of a notebook page,
    starting from 0, if -1, the last page will be removed, or the
    @class{gtk:widget} child page}
  @begin{short}
    Removes a page from the notebook given the page widget or its index in the
    notebook.
  @end{short}
  @begin[Note]{dictionary}
    In the Lisp implementation the argument can be an integer for the index or
    the page widget. The index of the page widget is got with the function
    @fun{gtk:notebook-page-num} and passed to the C function.
  @end{dictionary}
  @begin[Example]{dictionary}
    @begin{pre}
(defvar notebook (make-instance 'gtk:notebook))
=> NOTEBOOK
(defvar page (make-instance 'gtk:frame))
=> PAGE
(gtk:notebook-append-page notebook page nil)
=> 0
(gtk:notebook-remove-page notebook page)
(gtk:notebook-append-page notebook page nil)
=> 0
(gtk:notebook-remove-page notebook 0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-page-num}"
  (%notebook-remove-page notebook
                         (etypecase page-or-number
                           (integer page-or-number)
                           (widget (notebook-page-num notebook
                                                      page-or-number)))))

(export 'notebook-remove-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_detach_tab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_detach_tab" notebook-detach-tab) :void
 #+liber-documentation
 "@version{#2022-9-10}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @begin{short}
    Removes the child page from the notebook.
  @end{short}
  This function is very similar to the @fun{gtk:notebook-remove-page} function,
  but additionally informs the notebook that the removal is happening as part
  of a tab drag and drop operation, which should not be cancelled.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-remove-page}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-detach-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_page_num
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_page_num" notebook-page-num) :int
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child}
  @begin{return}
    The index of the page containing child, or -1 if child is not in the
    notebook.
  @end{return}
  @begin{short}
    Finds the index of the page which contains the given child widget.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-page-num)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_next_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_next_page" notebook-next-page) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Switches to the next page.
  @end{short}
  Nothing happens if the current page is the last page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-prev-page}"
  (notebook (g:object notebook)))

(export 'notebook-next-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_prev_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_prev_page" notebook-prev-page) :void
 #+liber-documentation
 "@version{#2021-17-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Switches to the previous page.
  @end{short}
  Nothing happens if the current page is the first page.
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-next-page}"
  (notebook (g:object notebook)))

(export 'notebook-prev-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_reorder_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_reorder_child" notebook-reorder-child) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page to move}
  @argument[pos]{an integer with the position, or -1 to move to the end}
  @begin{short}
    Reorders the page containing the child, so that it appears in the given
    position.
  @end{short}
  If the position is greater than or equal to the number of children in the
  list or negative, the child will be moved to the end of the list.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (child (g:object widget))
  (pos :int))

(export 'notebook-reorder-child)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_enable
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-popup-enable))

(defun notebook-popup-enable (notebook)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Enables the popup menu.
  @end{short}
  If the user clicks with the right mouse button on the tab labels, a menu with
  all the pages will be popped up.
  @begin[Note]{dictionary}
    This function calls the @fun{gtk:notebook-enable-popup} function with the
    @em{true} value.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-disable}
  @see-function{gtk:notebook-enable-popup}"
  (setf (notebook-enable-popup notebook) t))

(export 'notebook-popup-enable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_popup_disable
;;; ----------------------------------------------------------------------------

(declaim (inline notebook-popup-disable))

(defun notebook-popup-disable (notebook)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @begin{short}
    Disables the popup menu.
  @end{short}
  See the @fun{gtk:notebook-popup-enable} function.
  @begin[Note]{dictionary}
    This function calls the @fun{gtk:notebook-enable-popup} function with the
    @em{false} value.
  @end{dictionary}
  @see-class{gtk:notebook}
  @see-function{gtk:notebook-popup-enable}
  @see-function{gtk:notebook-enable-popup}"
  (setf (notebook-enable-popup notebook) nil))

(export 'notebook-popup-disable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_current_page
;;; gtk_notebook_set_current_page
;;; ----------------------------------------------------------------------------

(defun (setf notebook-current-page) (num notebook)
  (cffi:foreign-funcall "gtk_notebook_set_current_page"
                        (g:object notebook) notebook
                        :int num
                        :void)
  num)

(cffi:defcfun ("gtk_notebook_get_current_page" notebook-current-page) :int
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-current-page notebook) => num}
  @syntax{(setf (gtk:notebook-current-page notebook) num)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[num]{an integer with the index of the page to switch to, starting
    from 0, if negative, the last page will be used, if greater than the number
    of pages in the notebook, nothing will be done}
  @begin{short}
    The @fun{gtk:notebook-current-page} function returns an integer with the
    index starting from 0 of the page number of the current page.
  @end{short}
  The @setf{gtk:notebook-current-page} function switches to the given page
  number.

  Note that due to historical reasons, the @class{gtk:notebook} widget refuses
  to switch to a page unless the child widget is visible. Therefore, it is
  recommended to show child widgets before adding them to a notebook.
  @see-class{gtk:notebook}"
  (notebook (g:object notebook)))

(export 'notebook-current-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label
;;; gtk_notebook_set_menu_label
;;; ----------------------------------------------------------------------------

(defun (setf notebook-menu-label) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_menu_label"
                        (g:object notebook) notebook
                        (g:object widget) child
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_menu_label" notebook-menu-label)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-menu-label notebook child) => menu}
  @syntax{(setf (gtk:notebook-menu-label notebook child) menu)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child contained in a page of the
    notebook}
  @argument[menu]{a @class{gtk:widget} menu label, or @code{nil} for default}
  @begin{short}
    The @fun{gtk:notebook-menu-label} function returns the menu label, or
    @code{nil} if the notebook page does not have a menu label other than the
    default tab label.
  @end{short}
  The @setf{gtk:notebook-menu-label} function changes the menu label for the
  page containing the child.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-menu-label-text}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-menu-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_nth_page
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook" notebook-nth-page) (g:object widget)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[num]{an integer with the index of a page in the notebook, or -1 to
    get the last page}
  @return{The @class{gtk:widget} child page, or @code{nil} if @arg{num} is out
    of bounds.}
  @begin{short}
    Returns the child widget contained in page number @arg{num}.
  @end{short}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (num :int))

(export 'notebook-nth-page)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_n_pages -> notebook-n-pages
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_notebook_get_n_pages" notebook-n-pages) :int
 #+liber-documentation
 "@version{#2024-4-17}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @return{The integer with the number of pages in the notebook.}
  @short{Gets the number of pages in a notebook.}
  @see-class{gtk:notebook}"
  (notebook (g:object notebook)))

(export 'notebook-n-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label
;;; gtk_notebook_set_tab_label
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-label) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_tab_label"
                        (g:object notebook) notebook
                        (g:object widget) child
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_tab_label" notebook-tab-label)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-tab-label notebook child) => tab}
  @syntax{(setf (gtk:notebook-tab-label notebook child) tab)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[tab]{a @class{gtk:widget} tab label to use, or @code{nil} for
    default tab label}
  @begin{short}
    The @fun{gtk:notebook-tab-label} function returns the tab label widget for
    the page child.
  @end{short}
  The @code{nil} value is returned if the child is not in the notebook or if no
  tab label has been set for the child. The @setf{gtk:notebook-tab-label}
  function changes the tab label for the child page. If the @code{nil} value is
  specified for @arg{tab}, then the page will have the label 'page N'.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-tab-label-text}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-tab-label)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_menu_label_text
;;; gtk_notebook_set_menu_label_text
;;; ----------------------------------------------------------------------------

(defun (setf notebook-menu-label-text) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_menu_label_text"
                        (g:object notebook) notebook
                        (g:object widget) child
                        :string value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_menu_label_text" notebook-menu-label-text)
    :string
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-menu-label-text notebook child) => text}
  @syntax{(setf (gtk:notebook-menu-label-text notebook child) text)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child of a page of the notebook}
  @argument[text]{a string with the label text}
  @begin{short}
    The @fun{gtk:notebook-menu-label-text} function retrieves the text of the
    menu label for the page containing child.
  @end{short}
  The @setf{gtk:notebook-menu-label-text} function creates a new label and sets
  it as the menu label of the child page.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-menu-label}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-menu-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_label_text
;;; gtk_notebook_set_tab_label_text
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-label-text) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_tab_label_text"
                        (g:object notebook) notebook
                        (g:object widget) child
                        :string value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_tab_label_text" notebook-tab-label-text)
    :string
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-tab-label-text notebook child) => text}
  @syntax{(setf (gtk:notebook-tab-label-text notebook child) text)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child contained in a page of the
    notebook}
  @argument[text]{a string with the label text}
  @begin{short}
    The @fun{gtk:notebook-tab-label-text} function retrieves the text of the
    tab label for the page containing child.
  @end{short}
  The @setf{gtk:notebook-tab-label-text} function creates a new label and sets
  it as the tab label for the page containing child.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-tab-label}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-tab-label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_set_tab_reorderable
;;; gtk_notebook_get_tab_reorderable
;;; ----------------------------------------------------------------------------

(defun (setf notebook-tab-reorderable) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_tab_reorderable"
                        (g:object notebook) notebook
                        (g:object widget) child
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_tab_reorderable" notebook-tab-reorderable)
    :boolean
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-tab-reorderable notebook child) => reorderable}
  @syntax{(setf (gtk:notebook-tab-reorderable notebook child) reorderable)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[reorderable]{a boolean whether the tab is reorderable or not}
  @begin{short}
    The @fun{gtk:notebook-tab-reorderable} function gets whether the tab can be
    reordered via drag and drop or not.
  @end{short}
  The @setf{gtk:notebook-tab-reorderable} function sets whether the notebook
  tab can be reordered.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-tab-reorderable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_tab_detachable
;;; gtk_notebook_set_tab_detachable
;;; ----------------------------------------------------------------------------

;; TODO: Make a Drag and Drop Lisp example

(defun (setf notebook-tab-detachable) (value notebook child)
  (cffi:foreign-funcall "gtk_notebook_set_tab_detachable"
                        (g:object notebook) notebook
                        (g:object widget) child
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_tab_detachable" notebook-tab-detachable)
    :boolean
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-tab-detachable notebook child) => detachable}
  @syntax{(setf (gtk:notebook-tab-detachable notebook child) detachable)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[child]{a @class{gtk:widget} child page}
  @argument[detachable]{a boolean whether the tab is detachable or not}
  @begin{short}
    The @fun{gtk:notebook-tab-detachable} function returns whether the tab
    content can be detached from the notebook to another notebook or widget.
  @end{short}
  The @setf{gtk:notebook-tab-detachable} function sets whether the tab can be
  detached.

  Note that two notebooks must share a common group identificator, see the
  @fun{gtk:notebook-group-name} function, to allow automatic tabs interchange
  between them.
  @begin{examples}
    If you want a widget to interact with a notebook through DnD, i.e. accept
    dragged tabs from it, it must be set as a drop destination and accept the
    \"GTK_NOTEBOOK_TAB\" target. The notebook will fill the selection with a
    GtkWidget** pointing to the child widget that corresponds to the dropped
    tab.
    @begin{pre}
static void
on_drop_zone_drag_data_received (GtkWidget        *widget,
                                 GdkDragContext   *context,
                                 gint              x,
                                 gint              y,
                                 GtkSelectionData *selection_data,
                                 guint             info,
                                 guint             time,
                                 gpointer          user_data)
{
  GtkWidget *notebook;
  GtkWidget **child;

  notebook = gtk_drag_get_source_widget (context);
  child = (void*) gtk_selection_data_get_data (selection_data);

  process_widget (*child);
  gtk_container_remove (GTK_CONTAINER (notebook), *child);
@}
    @end{pre}
    If you want a notebook to accept drags from other widgets, you will have to
    set your own DnD code to do it.
  @end{examples}
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-function{gtk:notebook-group-name}"
  (notebook (g:object notebook))
  (child (g:object widget)))

(export 'notebook-tab-detachable)

;;; ----------------------------------------------------------------------------
;;; gtk_notebook_get_action_widget
;;; gtk_notebook_set_action_widget
;;; ----------------------------------------------------------------------------

(defun (setf notebook-action-widget) (value notebook packtype)
  (cffi:foreign-funcall "gtk_notebook_set_action_widget"
                        (g:object notebook) notebook
                        pack-type packtype
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_notebook_get_action_widget" notebook-action-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-4-17}
  @syntax{(gtk:notebook-action-widget notebook packtype) => widget}
  @syntax{(setf (gtk:notebook-action-widget notebook packtype) widget)}
  @argument[notebook]{a @class{gtk:notebook} widget}
  @argument[packtype]{a @symbol{gtk:pack-type} value for the action}
  @argument[widget]{a @class{gtk:widget} object}
  @begin{short}
    The @fun{gtk:notebook-action-widget} function gets one of the action
    widgets.
  @end{short}
  The @setf{gtk:notebook-action-widget} function sets the widget as one of the
  action widgets. Depending on the pack type the widget will be placed before or
  after the tabs. You can use a @class{gtk:box} widget if you need to pack more
  than one widget on the same side.
  @see-class{gtk:notebook}
  @see-class{gtk:widget}
  @see-class{gtk:box}
  @see-symbol{gtk:pack-type}"
  (notebook (g:object notebook))
  (packtype pack-type))

(export 'notebook-action-widget)

;;; --- End of file gtk4.notebook.lisp -----------------------------------------
