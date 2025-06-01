;;; ----------------------------------------------------------------------------
;;; gtk4.package.lisp
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

(in-package :cl-user)

(defpackage :gtk
  (:use :iterate :common-lisp)
  (:import-from :cffi)
  (:import-from :glib)
  (:import-from :gobject)
  (:import-from :gdk-pixbuf)
  (:import-from :pango))

(in-package :gtk)

#+sbcl
(when (and (find-package "SB-INT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT"))
           :traps nil))

(defvar *cl-cffi-gtk-build-time* (multiple-value-list (get-decoded-time)))
(defvar *cl-cffi-gtk-version*
        (asdf:component-version (asdf:find-system :cl-cffi-gtk4)))

#+liber-documentation
(setf (documentation (find-package :gtk) t)
 "This is the API documentation of a Lisp binding to GTK.
  GTK is a library for creating graphical user interfaces. It works on many
  UNIX-like platforms, Windows, and OS X. GTK is released under the GNU Library
  General Public License (GNU LGPL), which allows for flexible licensing of
  client applications. GTK has a C-based object-oriented architecture that
  allows for maximum flexibility. Bindings for many other languages have been
  written, including C++, Objective-C, Guile/Scheme, Perl, Python, TOM, Ada95,
  Free Pascal, and Eiffel.
  @begin[GListModel support]{section}
    @begin[GtkBitset]{subsection}
      @about-class{bitset}
      @about-function{bitset-ref}
      @about-function{bitset-unref}
      @about-function{bitset-new-empty}
      @about-function{bitset-new-range}
      @about-function{bitset-copy}
      @about-function{bitset-contains}
      @about-function{bitset-is-empty}
      @about-function{bitset-equals}
      @about-function{bitset-minimum}
      @about-function{bitset-maximum}
      @about-function{bitset-size}
      @about-function{bitset-size-in-range}
      @about-function{bitset-nth}
      @about-function{bitset-remove-all}
      @about-function{bitset-add}
      @about-function{bitset-remove}
      @about-function{bitset-add-range}
      @about-function{bitset-remove-range}
      @about-function{bitset-add-range-closed}
      @about-function{bitset-remove-range-closed}
      @about-function{bitset-add-rectangle}
      @about-function{bitset-remove-rectangle}
      @about-function{bitset-union}
      @about-function{bitset-intersect}
      @about-function{bitset-subtract}
      @about-function{bitset-difference}
      @about-function{bitset-shift-left}
      @about-function{bitset-shift-right}
      @about-function{bitset-splice}
      @about-symbol{bitset-iter}
      @about-function{bitset-iter-init-first}
      @about-function{bitset-iter-init-last}
      @about-function{bitset-iter-init-at}
      @about-function{bitset-iter-next}
      @about-function{bitset-iter-previous}
      @about-function{bitset-iter-value}
      @about-function{bitset-iter-is-valid}
    @end{subsection}
    @begin[GtkExpression]{subsection}
      @about-class{expression}
      @about-function{expression-ref}
      @about-function{expression-unref}
      @about-function{expression-value-type}
      @about-function{expression-is-static}
      @about-function{expression-evaluate}
      @about-function{expression-evaluate-value}
      @about-function{expression-bind}
      @about-symbol{expression-notify}
      @about-function{expression-watch}
      @about-class{expression-watch}
      @about-function{expression-watch-evaluate}
      @about-function{expression-watch-evaluate-value}
      @about-function{expression-watch-unwatch}
      @about-function{property-expression-new}
      @about-function{property-expression-new-for-pspec}
      @about-function{property-expression-expression}
      @about-function{property-expression-pspec}
      @about-function{constant-expression-new}
      @about-function{constant-expression-new-for-value}
      @about-function{constant-expression-value}
      @about-function{object-expression-new}
      @about-function{object-expression-object}
      @about-function{closure-expression-new}
      @about-function{cclosure-expression-new}
      @about-symbol{VALUE_HOLDS_EXPRESSION}
      @about-function{value-expression}
      @about-function{value-take-expression}
      @about-function{value-dup-expression}
      @about-symbol{param-spec-expression}
      @about-function{param-spec-expression}
    @end{subsection}
    @begin[GtkFilter]{subsection}
      @about-symbol{filter-match}
      @about-symbol{filter-change}
      @about-class{filter}
      @about-function{filter-match}
      @about-function{filter-strictness}
      @about-function{filter-changed}
    @end{subsection}
    @begin[GtkCustomFilter]{subsection}
      @about-class{custom-filter}
      @about-function{custom-filter-new}
      @about-symbol{custom-filter-func}
      @about-function{custom-filter-set-filter-func }
    @end{subsection}
    @begin[GtkMultiFilter]{subsection}
      @about-class{multi-filter}
      @about-generic{multi-filter-item-type}
      @about-generic{multi-filter-n-items}
      @about-function{multi-filter-append}
      @about-function{multi-filter-remove}
      @about-class{any-filter}
      @about-function{any-filter-new}
      @about-class{every-filter}
      @about-function{every-filter-new}
    @end{subsection}
    @begin[GtkBoolFilter]{subsection}
      @about-class{bool-filter}
      @about-generic{bool-filter-expression}
      @about-generic{bool-filter-invert}
      @about-function{bool-filter-new}
    @end{subsection}
    @begin[GtkStringFilter]{subsection}
      @about-symbol{string-filter-match-mode}
      @about-class{string-filter}
      @about-generic{string-filter-expression}
      @about-generic{string-filter-ignore-case}
      @about-generic{string-filter-match-mode}
      @about-generic{string-filter-search}
      @about-function{string-filter-new}
    @end{subsection}
    @begin[GtkFileFilter]{subsection}
      @about-class{file-filter}
      @about-generic{file-filter-mime-types}
      @about-generic{file-filter-name}
      @about-generic{file-filter-patterns}
      @about-generic{file-filter-suffixes}
      @about-function{file-filter-new}
      @about-function{file-filter-new-from-gvariant}
      @about-function{file-filter-add-mime-type}
      @about-function{file-filter-add-pattern}
      @about-function{file-filter-add-pixbuf-formats}
      @about-function{file-filter-add-suffix}
      @about-function{file-filter-attributes}
      @about-function{file-filter-to-gvariant}
    @end{subsection}
    @begin[GtkSorter]{subsection}
      @about-symbol{sorter-order}
      @about-symbol{sorter-change}
      @about-class{sorter}
      @about-function{sorter-compare}
      @about-function{sorter-order}
      @about-function{sorter-changed}
      @about-function{ordering-from-cmpfunc}
    @end{subsection}
    @begin[GtkCustomSorter]{subsection}
      @about-class{custom-sorter}
      @about-function{custom-sorter-new}
      @about-function{custom-sorter-set-sort-func}
    @end{subsection}
    @begin[GtkMultiSorter]{subsection}
      @about-class{multi-sorter}
      @about-generic{multi-sorter-item-type}
      @about-generic{multi-sorter-n-items}
      @about-function{multi-sorter-new}
      @about-function{multi-sorter-append}
      @about-function{multi-sorter-remove}
    @end{subsection}
    @begin[GtkStringSorter]{subsection}
      @about-symbol{collation}
      @about-class{string-sorter}
      @about-generic{string-sorter-collation}
      @about-generic{string-sorter-expression}
      @about-generic{string-sorter-ignore-case}
      @about-function{string-sorter-new}
    @end{subsection}
    @begin[GtkNumericSorter]{subsection}
      @about-class{numeric-sorter}
      @about-generic{numeric-sorter-expression}
      @about-generic{numeric-sorter-sort-order}
      @about-function{numeric-sorter-new}
    @end{subsection}
    @begin[GtkSelectionModel]{subsection}
      @about-class{selection-model}
      @about-function{selection-model-is-selected}
      @about-function{selection-model-selection}
      @about-function{selection-model-selection-in-range}
      @about-function{selection-model-select-item}
      @about-function{selection-model-unselect-item}
      @about-function{selection-model-select-range}
      @about-function{selection-model-unselect-range}
      @about-function{selection-model-select-all}
      @about-function{selection-model-unselect-all}
      @about-function{selection-model-set-selection}
      @about-function{selection-model-selection-changed}
    @end{subsection}
    @begin[GtkNoSelection]{subsection}
      @about-class{no-selection}
      @about-generic{no-selection-item-type}
      @about-generic{no-selection-model}
      @about-generic{no-selection-n-items}
      @about-function{no-selection-new}
    @end{subsection}
    @begin[GtkSingleSelection]{subsection}
      @about-variable{gtk:+invalid-list-position+}
      @about-class{single-selection}
      @about-generic{single-selection-autoselect}
      @about-generic{single-selection-can-unselect}
      @about-generic{single-selection-item-type}
      @about-generic{single-selection-model}
      @about-generic{single-selection-n-items}
      @about-generic{single-selection-selected}
      @about-generic{single-selection-selected-item}
      @about-function{single-selection-new}
    @end{subsection}
    @begin[GtkMultiSelection]{subsection}
      @about-class{multi-selection}
      @about-generic{multi-selection-item-type}
      @about-generic{multi-selection-model}
      @about-generic{multi-selection-n-items}
      @about-function{multi-selection-new}
    @end{subsection}
    @begin[GtkSectionModel]{subsection}
      @about-class{section-model}
      @about-function{section-model-section}
      @about-function{section-model-sections-changed}
    @end{subsection}
    @begin[GtkFilterListModel]{subsection}
      @about-class{filter-list-model}
      @about-generic{filter-list-model-filter}
      @about-generic{filter-list-model-incremental}
      @about-generic{filter-list-model-item-type}
      @about-generic{filter-list-model-model}
      @about-generic{filter-list-model-n-items}
      @about-generic{filter-list-model-pending}
      @about-function{filter-list-model-new}
    @end{subsection}
    @begin[GtkFlattenListModel]{subsection}
      @about-class{flatten-list-model}
      @about-generic{flatten-list-model-item-type}
      @about-generic{flatten-list-model-model}
      @about-generic{flatten-list-model-n-items}
      @about-function{flatten-list-model-new}
      @about-function{flatten-list-model-model-for-item}
    @end{subsection}
    @begin[GtkMapListModel]{subsection}
      @about-class{map-list-model}
      @about-generic{map-list-model-has-map}
      @about-generic{map-list-model-item-type}
      @about-generic{map-list-model-model}
      @about-generic{map-list-model-n-items}
      @about-function{map-list-model-new}
      @about-symbol{map-list-model-map-func}
      @about-function{map-list-model-set-map-func}
    @end{subsection}
    @begin[GtkSliceListModel]{subsection}
      @about-class{slice-list-model}
      @about-generic{slice-list-model-item-type}
      @about-generic{slice-list-model-model}
      @about-generic{slice-list-model-n-items}
      @about-generic{slice-list-model-offset}
      @about-generic{slice-list-model-size}
      @about-function{slice-list-model-new}
    @end{subsection}
    @begin[GtkSortListModel]{subsection}
      @about-class{sort-list-model}
      @about-generic{sort-list-model-incremental}
      @about-generic{sort-list-model-item-type}
      @about-generic{sort-list-model-model}
      @about-generic{sort-list-model-n-items}
      @about-generic{sort-list-model-pending}
      @about-generic{sort-list-model-section-sorter}
      @about-generic{sort-list-model-sorter}
      @about-function{sort-list-model-new}
    @end{subsection}
    @begin[GtkSelectionFilterModel]{subsection}
      @about-class{selection-filter-model}
      @about-generic{selection-filter-model-item-type}
      @about-generic{selection-filter-model-model}
      @about-generic{selection-filter-model-n-items}
      @about-function{selection-filter-model-new}
    @end{subsection}
    @begin[GtkBookmarkList]{subsection}
      @about-class{bookmark-list}
      @about-generic{bookmark-list-attributes}
      @about-generic{bookmark-list-filename}
      @about-generic{bookmark-list-io-priority}
      @about-generic{bookmark-list-item-type}
      @about-generic{bookmark-list-loading}
      @about-generic{bookmark-list-n-items}
      @about-function{bookmark-list-new}
      @about-function{bookmark-list-is-loading}
    @end{subsection}
    @begin[GtkDirectoryList]{subsection}
      @about-class{directory-list}
      @about-generic{directory-list-attributes}
      @about-generic{directory-list-error}
      @about-generic{directory-list-file}
      @about-generic{directory-list-io-priority}
      @about-generic{directory-list-item-type}
      @about-generic{directory-list-loading}
      @about-generic{directory-list-monitored}
      @about-generic{directory-list-n-items}
      @about-function{directory-list-new}
      @about-function{directory-list-is-loading}
    @end{subsection}
    @begin[GtkStringList]{subsection}
      @about-class{string-object}
      @about-generic{string-object-string}
      @about-function{string-object-new}
      @about-class{string-list}
      @about-generic{string-list-item-type}
      @about-generic{string-list-n-items}
      @about-function{string-list-new}
      @about-function{string-list-append}
      @about-function{string-list-take}
      @about-function{string-list-remove}
      @about-function{string-list-splice}
      @about-function{string-list-string}
      @about-function{string-list-find}
    @end{subsection}
  @end{section}
  @begin[List-based Widgets]{section}
    @begin[List Widget Overview]{subsection}
      GTK provides powerful widgets to display and edit lists of data. This
      document gives an overview over the concepts and how they work together
      to allow developers to implement lists. Lists are intended to be used
      whenever developers want to display many objects in roughly the same way.

      Lists are perfectly fine to be used for very short list of only 2 or 3
      elements, but generally scale to millions of items. Of course, the larger
      the list grows, the more care needs to be taken to choose the right data
      structures to keep things running well. Lists are meant to be used with
      changing data, both with the items itself changing as well as the list
      adding and removing items. Of course, they work just as well with static
      data.

      @subheading{Terminology}
      These terms are used throughout the documentation when talking about lists
      and you should be aware of what they refer to. These are often generic
      terms that have a specific meaning in this context.

      Views or list widgets are the widgets that hold and manage the lists.
      Examples of these widgets would be the @class{gtk:list-view} or
      @class{gtk:grid-view} widgets.

      Views display data from a model. Models implement the @class{g:list-model}
      interface and can be provided in a variety of ways:
      @begin{itemize}
        @item{List model implementations for many specific types of data already
          exist, for example the @class{gtk:directory-list} or
          @class{gtk:string-list} objects.}
        @item{There are generic list model implementations like the
          @class{g:list-store} object that allow building lists of arbitrary
          objects.}
        @item{Wrapping list models like @class{gtk:filter-list-model} or
          @class{gtk:sort-list-model} objects modify, adapt or combine other
          models.}
        @item{Last but not least, developers are encouraged to create their own
          @class{g:list-model} implementations. The interface is kept
          deliberately small to make this easy.}
      @end{itemize}
      The same model can be used in multiple different views and wrapped with
      multiple different models at once.

      The elements in a model are called items. All items are @class{g:object}
      instances. Every item in a model has a position which is the unsigned
      integer that describes where in the model the item is located. The first
      item in a model is at position 0. The position of an item can change as
      other items are added or removed from the model.

      It is important to be aware of the difference between items and positions
      because the mapping from position to item is not permanent, so developers
      should think about whether they want to track items or positions when
      working with models. Oftentimes some things are really hard to do one way
      but very easy the other way.

      The other important part of a view is a factory. Each factory is a
      @class{gtk:list-item-factory} implementation that takes care of mapping
      the items of the model to widgets that can be shown in the view.

      The way factories do this is by creating a list item for each item that is
      currently in use. List items are always @class{gtk:list-item} instances.
      They are only ever created by GTK and provide information about what item
      they are meant to display.

      Different factory implementations use various different methods to allow
      developers to add the right widgets to list items and to link those
      widgets with the item managed by the list item. Finding a suitable factory
      implementation for the data displayed, the programming language and
      development environment is an important task that can simplify setting up
      the view tremendously.

      Views support selections via a selection model. A selection model is an
      implementation of the @class{gtk:selection-model} interface on top of the
      @class{g:list-model} interface that allows marking each item in a model
      as either selected or not selected. Just like regular models, this can be
      implemented either by implementing the @class{gtk:selection-model}
      interface directly or by wrapping a model with one of the GTK models
      provided for this purposes, such as the @class{gtk:no-selection} or
      @class{gtk:single-selection} models.

      The behavior of selection models - that is, which items they allow
      selecting and what effect this has on other items - is completely up to
      the selection model. As such, single-selections, multi-selections or
      sharing selection state between different selection models and/or views
      is possible. The selection state of an item is exposed in the list item
      via the @slot[gtk:list-item]{selected} property.

      Views and list items also support activation. Activation means that double
      clicking or pressing enter while inside a focused row will cause the view
      to emit a signal such as the @code{GtkListView::activate} signal. This
      provides an easy way to set up lists, but can also be turned off on
      list items if undesired.

      Both selections and activation are supported among other things via
      widget actions. This allows developers to add widgets to their lists that
      cause selections to change or to trigger activation via the
      @class{gtk:actionable} interface. For a list of all supported actions see
      the relevant documentation.

      @subheading{Behind the scenes}
      While it is not a problem for short lists to instantiate widgets for every
      item in the model, once lists grow to thousands or millions of elements,
      this gets less feasible. Because of this, the views only create a limited
      amount of list items and recycle them by binding them to new items. In
      general, views try to keep list items available only for the items that
      can actually be seen on screen.

      While this behavior allows views to scale effortlessly to huge lists, it
      has a few implications for what can be done with views. For example, it is
      not possible to query a view for a list item used for a certain position -
      there might not be one and even if there is, that list item might soon be
      recycled for a new position.

      It is also important that developers save state they care about in the
      item and do not rely on the widgets they created as those widgets can be
      recycled for a new position at any time causing any state to be lost.

      Another important requirement for views is that they need to know which
      items are not visible so they can be recycled. Views achieve that by
      implementing the @class{gtk:scrollable} interface and expecting to be
      placed directly into a @class{gtk:scrolled-window} widget.

      Of course, if you are only using models with few items, this is not
      important and you can treat views like any other widget. But if you use
      large lists and your performance suffers, you should be aware of this.
      Views also allow tuning the number of list items they create such as with
      the @fun{gtk:grid-view-max-columns} function, and developers running into
      performance problems should definitely study the tradeoffs of those and
      experiment with them.

      @subheading{Choosing the right model}
      GTK offers a wide variety of wrapping models which change or supplement an
      existing model (or models) in some way. But when it comes to storing your
      actual data, there are only a few ready-made choices available:
      the @class{g:list-store}, @class{gtk:string-list}, and
      @class{gtk:directory-list} objects.

      The @class{g:list-store} object is backed by a balanced tree and has
      performance characteristics that are expected for that data structure. It
      works reasonably well for dataset sizes in the 1,000,000 range, and can
      handle insertions and deletions. It uses a cached iterator to make linear
      access to the items fast.

      The @class{gtk:string-list} object is not a general store - it can only
      handle strings. It is backed by an dynamically allocated array and has
      performance characteristics that are expected for that data structure.
      The @class{gtk:string-list} object is a good fit for any place where you
      would otherwise use @code{char*[@]} and works best if the dataset is not
      very dynamic.

      The @class{gtk:directory-list} object is a list model that wraps the
      @code{g_file_enumerate_children_async()} function. It presents a
      @class{g:list-model} object and fills it asynchronously with the
      @code{GFiles} returned from that function.

      If these models do not fit your use case or scalability requirements, you
      should make a custom @class{g:list-model} implementation. It is a small
      interface and not very hard to implement.

      @subheading{Displaying trees}
      While the @class{gtk:tree-view} widget provided built-in support for
      trees, the list widgets, and in particular the @class{g:list-model}
      implementation do not. This was a design choice because the common use
      case is displaying lists and not trees and it greatly simplifies the API
      interface provided.

      However, GTK provides functionality to make lists look and behave like
      trees for use cases that require trees. This is achieved by using the
      @class{gtk:tree-list-model} model to flatten a tree into a list. The
      @class{gtk:tree-expander} widget can then be used inside a list item to
      allow users to expand and collapse rows and provide a similar experience
      to the @class{gtk:tree-view} widget. Developers should refer to those
      objects’ API reference for more discussion on the topic.

      @subheading{List styles}
      One of the advantages of the new list widgets over GtkTreeView and cell
      renderers is that they are styleable using GTK CSS. This provides a lot of
      flexibility. The themes that ship with GTK provide a few predefined list
      styles that can be used in many situations.

      @image[rich-list]{Figure: Rich list}

      This rich list style is low density, spacious and uses an outline focus
      ring. It is suitable for lists of controls, for example, in preference
      dialogs or settings panels. Use the @code{.rich-list} style class.

      @image[navigation-sidebar]{Figure: Navigation sidebar}

      The sidebar style of list is medium density, using a full background to
      indicate focus and selection. Use the @code{.navigation-sidebar} style
      class.

      @image[data-table]{Figure: Data table}

      The data table style of list is a high density table, similar in style to
      a traditional tree view. Individual cells can be selectable and editable.
      Use the @code{.data-table} style class.

      @subheading{Sections}
      List models can optionally group their items into sections, by
      implementing the @class{gtk:section-model} interface. The
      @class{gtk:list-view} widget can display headers for sections, by
      installing a separate header factory.

      Many GTK list models support section inherently, or they pass through the
      section of a model they are wrapping.

      @subheading{Comparison to GtkTreeView}
      Developers familiar with GtkTreeView may wonder how this way of doing
      lists compares to the way they know. This section will try to outline the
      similarities and differences between the two.

      This new approach tries to provide roughly the same functionality as the
      old approach but often uses a very different way to achieve these goals.
      The main difference and one of the primary reasons for this new
      development is that items can be displayed using regular widgets and the
      separate cell renderer machinery is no longer necessary. This allows all
      benefits that widgets provide, such as complex layout, animations and CSS
      styling.

      The other big difference is the massive change to the data model.
      The @class{gtk:tree-model} interface was a rather complex interface for a
      tree data structure. The @class{g:list-model} interface is deliberately
      designed to be a very simple data structure for lists only. See above for
      how to still do trees with this new model. Another big change is that the
      new model allows for bulk changes via the @code{GListModel::items-changed}
      signal while the @class{gtk:tree-model} interface only allows a single
      item to change at once. The goal here is of course to encourage
      implementation of custom list models.

      Another consequence of the new model is that it is now easily possible to
      refer to the contents of a row in the model directly by keeping the item,
      while the @class{gtk:tree-row-reference} implementation was a very slow
      mechanism to achieve the same. And because the items are real objects,
      developers can make them emit change signals causing list items and their
      children to update, which was not possible with the @class{gtk:tree-model}
      implementation.

      The selection handling is also different. While selections used to be
      managed via custom code in each widget, selection state is now meant to
      be managed by the selection models. In particular this allows for complex
      use cases with specialized requirements.

      Finally here is a quick comparison chart of equivalent functionality to
      look for when transitioning code:
      @begin{pre}
Old                     New
-------------------------------------------------------
GtkTreeModel            GListModel
GtkTreePath             guint position, GtkTreeListRow
GtkTreeIter             guint position
GtkTreeRowReference     GObject item
GtkListStore            GListStore
GtkTreeStore            GtkTreeListModel, GtkTreeExpander
GtkTreeSelection        GtkSelectionModel
GtkTreeViewColumn       GtkColumnView
GtkTreeView             GtkListView, GtkColumnView
GtkCellView             GtkListItem
GtkComboBox             GtkDropDown
GtkIconView             GtkGridView
GtkTreeSortable         GtkColumnView
GtkTreeModelSort        GtkSortListModel
GtkTreeModelFilter      GtkFilterListModel
GtkCellLayout           GtkListItemFactory
GtkCellArea             GtkWidget
GtkCellRenderer         GtkWidget
      @end{pre}
    @end{subsection}
    @begin[GtkListItem]{subsection}
      @about-class{list-item}
      @about-generic{list-item-accessible-description}
      @about-generic{list-item-accessible-label}
      @about-generic{list-item-activatable}
      @about-generic{list-item-child}
      @about-generic{list-item-focusable}
      @about-generic{list-item-item}
      @about-generic{list-item-position}
      @about-generic{list-item-selectable}
      @about-generic{list-item-selected}
    @end{subsection}
    @begin[GtkListItemFactory]{subsection}
      @about-class{list-item-factory}
    @end{subsection}
    @begin[GtkSignalListItemFactory]{subsection}
      @about-class{signal-list-item-factory}
      @about-function{signal-list-item-factory-new}
    @end{subsection}
    @begin[GtkBuilderListItemFactory]{subsection}
      @about-class{builder-list-item-factory}
      @about-generic{builder-list-item-factory-bytes}
      @about-generic{builder-list-item-factory-resource}
      @about-generic{builder-list-item-factory-scope}
      @about-function{builder-list-item-factory-new-from-bytes}
      @about-function{builder-list-item-factory-new-from-resource}
    @end{subsection}
    @begin[GtkScrollInfo]{subsection}
      @about-class{scroll-info}
      @about-function{scroll-info-new}
      @about-function{scroll-info-enable-horizontal}
      @about-function{scroll-info-enable-vertical}
    @end{subsection}
    @begin[GtkListHeader]{subsection}
      @about-class{list-header}
      @about-generic{list-header-child}
      @about-generic{list-header-end}
      @about-generic{list-header-item}
      @about-generic{list-header-n-items}
      @about-generic{list-header-start}
    @end{subsection}
    @begin[GtkListView]{subsection}
      @about-symbol{list-tab-behavior}
      @about-symbol{list-scroll-flags}
      @about-class{list-base}
      @about-generic{list-base-orientation}
      @about-class{list-view}
      @about-generic{list-view-enable-rubberband}
      @about-generic{list-view-factory}
      @about-generic{list-view-header-factory}
      @about-generic{list-view-model}
      @about-generic{list-view-show-separators}
      @about-generic{list-view-single-click-activate}
      @about-generic{list-view-tab-behavior}
      @about-function{list-view-new}
      @about-function{list-view-scroll-to}
    @end{subsection}
    @begin[GtkGridView]{subsection}
      @about-class{grid-view}
      @about-generic{grid-view-enable-rubberband}
      @about-generic{grid-view-factory}
      @about-generic{grid-view-max-columns}
      @about-generic{grid-view-min-columns}
      @about-generic{grid-view-model}
      @about-generic{grid-view-single-click-activate}
      @about-generic{grid-view-tab-behavior}
      @about-function{grid-view-new}
      @about-function{grid-view-scroll-to}
    @end{subsection}
    @begin[GtkColumnView]{subsection}
      @about-class{column-view}
      @about-generic{column-view-columns}
      @about-generic{column-view-enable-rubberband}
      @about-generic{column-view-header-factory}
      @about-generic{column-view-model}
      @about-generic{column-view-reorderable}
      @about-generic{column-view-row-factory}
      @about-generic{column-view-show-column-separators}
      @about-generic{column-view-show-row-separators}
      @about-generic{column-view-single-click-activate}
      @about-generic{column-view-sorter}
      @about-generic{column-view-tab-behavior}
      @about-function{column-view-new}
      @about-function{column-view-append-column}
      @about-function{column-view-insert-column}
      @about-function{column-view-remove-column}
      @about-function{column-view-sort-by-column}
    @end{subsection}
    @begin[GtkColumnViewColumn]{subsection}
      @about-class{column-view-column}
      @about-generic{column-view-column-column-view}
      @about-generic{column-view-column-expand}
      @about-generic{column-view-column-factory}
      @about-generic{column-view-column-fixed-width}
      @about-generic{column-view-column-header-menu}
      @about-generic{column-view-column-id}
      @about-generic{column-view-column-resizable}
      @about-generic{column-view-column-sorter}
      @about-generic{column-view-column-title}
      @about-generic{column-view-column-visible}
      @about-function{column-view-column-new}
    @end{subsection}
    @begin[GtkColumnViewCell]{subsection}
      @about-class{column-view-cell}
      @about-generic{column-view-cell-child}
      @about-generic{column-view-cell-focusable}
      @about-generic{column-view-cell-item}
      @about-generic{column-view-cell-position}
      @about-generic{column-view-cell-selected}
    @end{subsection}
    @begin[GtkColumnViewRow]{subsection}
      @about-class{column-view-row}
      @about-generic{column-view-row-accessible-description}
      @about-generic{column-view-row-accessible-label}
      @about-generic{column-view-row-activatable}
      @about-generic{column-view-row-focusable}
      @about-generic{column-view-row-item}
      @about-generic{column-view-row-position}
      @about-generic{column-view-row-selectable}
      @about-generic{column-view-row-selected}
    @end{subsection}
    @begin[GtkColumnViewSorter]{subsection}
      @about-class{column-view-sorter}
      @about-generic{column-view-sorter-primary-sort-column}
      @about-generic{column-view-sorter-primary-sort-order}
      @about-function{column-view-sorter-n-sort-columns}
      @about-function{column-view-sorter-nth-sort-column}
    @end{subsection}
    @begin[GtkDropDown]{subsection}
      @about-class{drop-down}
      @about-generic{drop-down-enable-search}
      @about-generic{drop-down-expression}
      @about-generic{drop-down-factory}
      @about-generic{drop-down-header-factory}
      @about-generic{drop-down-list-factory}
      @about-generic{drop-down-model}
      @about-generic{drop-down-search-match-mode}
      @about-generic{drop-down-selected}
      @about-generic{drop-down-selected-item}
      @about-generic{drop-down-show-arrow}
      @about-function{drop-down-new}
      @about-function{drop-down-new-from-strings}
    @end{subsection}
  @end{section}
  @begin[Tree support]{section}
    @begin[GtkTreeListModel]{subsection}
      @about-class{tree-list-row}
      @about-generic{tree-list-row-children}
      @about-generic{tree-list-row-depth}
      @about-generic{tree-list-row-expandable}
      @about-generic{tree-list-row-expanded}
      @about-generic{tree-list-row-item}
      @about-function{tree-list-row-child-row}
      @about-function{tree-list-row-parent}
      @about-function{tree-list-row-position}
      @about-function{tree-list-row-is-expandable}
      @about-class{tree-list-model}
      @about-generic{tree-list-model-autoexpand}
      @about-generic{tree-list-model-item-type}
      @about-generic{tree-list-model-model}
      @about-generic{tree-list-model-n-items}
      @about-generic{tree-list-model-passthrough}
      @about-symbol{tree-list-model-create-model-func}
      @about-function{tree-list-model-new}
      @about-function{tree-list-model-row}
      @about-function{tree-list-model-child-row}
    @end{subsection}
    @begin[GtkTreeListRowSorter]{subsection}
      @about-class{tree-list-row-sorter}
      @about-generic{tree-list-row-sorter-sorter}
      @about-function{tree-list-row-sorter-new}
    @end{subsection}
    @begin[GtkTreeExpander]{subsection}
      @about-class{tree-expander}
      @about-generic{tree-expander-child}
      @about-generic{tree-expander-hide-expander}
      @about-generic{tree-expander-indent-for-depth}
      @about-generic{tree-expander-indent-for-icon}
      @about-generic{tree-expander-item}
      @about-generic{tree-expander-list-row}
      @about-function{tree-expander-new}
    @end{subsection}
  @end{section}
  @begin[Application support]{section}
    @begin[GtkApplication]{subsection}
      @about-symbol{application-inhibit-flags}
      @about-class{application}
      @about-generic{application-active-window}
      @about-generic{application-menubar}
      @about-generic{application-register-session}
      @about-generic{application-screensaver-active}
      @about-function{application-new}
      @about-function{application-add-window}
      @about-function{application-remove-window}
      @about-function{application-windows}
      @about-function{application-window-by-id}
      @about-function{application-inhibit}
      @about-function{application-uninhibit}
      @about-function{application-menu-by-id}
      @about-function{application-list-action-descriptions}
      @about-function{application-accels-for-action}
      @about-function{application-actions-for-accel}
    @end{subsection}
    @begin[GtkApplicationWindow]{subsection}
      @about-class{application-window}
      @about-generic{application-window-show-menubar}
      @about-function{application-window-new}
      @about-function{application-window-id}
      @about-function{application-window-help-overlay}
    @end{subsection}
  @end{section}
  @begin[Interface builder]{section}
    @begin[GtkBuildable]{subsection}
      @about-class{buildable}
      @about-function{buildable-buildable-id}
    @end{subsection}
    @begin[GtkBuilder]{subsection}
      @about-symbol{builder-closure-flags}
      @about-class{builder-scope}
      @about-class{builder-cl-scope}
      @about-class{builder}
      @about-generic{builder-current-object}
      @about-generic{builder-scope}
      @about-generic{builder-translation-domain}
      @about-function{builder-new}
      @about-function{builder-new-from-file}
      @about-function{builder-new-from-resource}
      @about-function{builder-new-from-string}
      @about-function{builder-add-from-file}
      @about-function{builder-add-from-resource}
      @about-function{builder-add-from-string}
      @about-function{builder-add-objects-from-file}
      @about-function{builder-add-objects-from-resource}
      @about-function{builder-add-objects-from-string}
      @about-function{builder-extend-with-template}
      @about-function{builder-object}
      @about-function{builder-objects}
      @about-function{builder-expose-object}
      @about-function{builder-create-closure}
      @about-function{builder-type-from-name}
      @about-function{builder-value-from-string}
      @about-function{builder-value-from-string-type}
    @end{subsection}
  @end{section}
  @begin[Windows]{section}
    @begin[GtkRoot]{subsection}
      @about-class{root}
      @about-function{root-display}
      @about-function{root-focus}
    @end{subsection}
    @begin[GtkNative]{subsection}
      @about-class{native}
      @about-function{native-for-surface}
      @about-function{native-surface}
      @about-function{native-renderer}
      @about-function{native-surface-transform}
      @about-function{native-realize}
      @about-function{native-unrealize}
    @end{subsection}
    @begin[GtkWindow]{subsection}
      @about-class{window}
      @about-generic{window-application}
      @about-generic{window-child}
      @about-generic{window-decorated}
      @about-generic{window-default-height}
      @about-generic{window-default-widget}
      @about-generic{window-default-width}
      @about-generic{window-deletable}
      @about-generic{window-destroy-with-parent}
      @about-generic{window-display}
      @about-generic{window-focus-visible}
      @about-generic{window-focus-widget}
      @about-generic{window-fullscreened}
      @about-generic{window-handle-menubar-accel}
      @about-generic{window-hide-on-close}
      @about-generic{window-icon-name}
      @about-generic{window-is-active}
      @about-generic{window-maximized}
      @about-generic{window-mnemonics-visible}
      @about-generic{window-modal}
      @about-generic{window-resizable}
      @about-generic{window-startup-id}
      @about-generic{window-suspended}
      @about-generic{window-title}
      @about-generic{window-titlebar}
      @about-generic{window-transient-for}
      @about-function{window-new}
      @about-function{window-destroy}
      @about-function{window-default-size}
      @about-function{window-is-active}
      @about-function{window-is-maximized}
      @about-function{window-is-fullscreen}
      @about-function{window-toplevels}
      @about-function{window-list-toplevels}
      @about-function{window-focus}
      @about-function{window-present}
      @about-function{window-present-with-time}
      @about-function{window-close}
      @about-function{window-minimize}
      @about-function{window-unminimize}
      @about-function{window-maximize}
      @about-function{window-unmaximize}
      @about-function{window-fullscreen}
      @about-function{window-fullscreen-on-monitor}
      @about-function{window-unfullscreen}
      @about-function{window-default-icon-name}
      @about-function{window-group}
      @about-function{window-has-group}
      @about-function{window-set-auto-startup-notification}
      @about-function{window-set-interactive-debugging}
      @about-function{window-is-suspended}
    @end{subsection}
    @begin[GtkAboutDialog]{subsection}
      @about-symbol{license}
      @about-class{about-dialog}
      @about-generic{about-dialog-artists}
      @about-generic{about-dialog-authors}
      @about-generic{about-dialog-comments}
      @about-generic{about-dialog-copyright}
      @about-generic{about-dialog-documenters}
      @about-generic{about-dialog-license}
      @about-generic{about-dialog-license-type}
      @about-generic{about-dialog-logo}
      @about-generic{about-dialog-logo-icon-name}
      @about-generic{about-dialog-program-name}
      @about-generic{about-dialog-translator-credits}
      @about-generic{about-dialog-version}
      @about-generic{about-dialog-website}
      @about-generic{about-dialog-website-label}
      @about-generic{about-dialog-wrap-license}
      @about-function{about-dialog-new}
      @about-function{about-dialog-add-credit-section}
      @about-function{show-about-dialog}
    @end{subsection}
    @begin[GtkAlertDialog]{subsection}
      @about-class{alert-dialog}
      @about-generic{alert-dialog-buttons}
      @about-generic{alert-dialog-cancel-button}
      @about-generic{alert-dialog-default-button}
      @about-generic{alert-dialog-detail}
      @about-generic{alert-dialog-message}
      @about-generic{alert-dialog-modal}
      @about-function{alert-dialog-new}
      @about-function{alert-dialog-choose}
      @about-function{alert-dialog-choose-finish}
      @about-function{alert-dialog-show}
    @end{subsection}
    @begin[GtkWindowGroup]{subsection}
      @about-class{window-group}
      @about-function{window-group-new}
      @about-function{window-group-add-window}
      @about-function{window-group-remove-window}
      @about-function{window-group-list-windows}
    @end{subsection}
    @begin[GtkNativeDialog]{subsection}
      @about-class{native-dialog}
      @about-generic{native-dialog-modal}
      @about-generic{native-dialog-title}
      @about-generic{native-dialog-transient-for}
      @about-generic{native-dialog-visible}
      @about-function{native-dialog-show}
      @about-function{native-dialog-hide}
      @about-function{native-dialog-destroy}
    @end{subsection}
  @end{section}
  @begin[Layout Widgets]{section}
    @begin[GtkBox]{subsection}
      @about-class{box}
      @about-generic{box-baseline-child}
      @about-generic{box-baseline-position}
      @about-generic{box-homogeneous}
      @about-generic{box-spacing}
      @about-function{box-new}
      @about-function{box-append}
      @about-function{box-prepend}
      @about-function{box-remove}
      @about-function{box-insert-child-after}
      @about-function{box-reorder-child-after}
    @end{subsection}
    @begin[GtkCenterBox]{subsection}
      @about-class{center-box}
      @about-generic{center-box-baseline-position}
      @about-generic{center-box-center-widget}
      @about-generic{center-box-end-widget}
      @about-generic{center-box-shrink-center-last}
      @about-generic{center-box-start-widget}
      @about-function{center-box-new}
    @end{subsection}
    @begin[GtkGrid]{subsection}
      @about-class{grid}
      @about-generic{grid-baseline-row}
      @about-generic{grid-column-homogeneous}
      @about-generic{grid-column-spacing}
      @about-generic{grid-row-homogeneous}
      @about-generic{grid-row-spacing}
      @about-function{grid-new}
      @about-function{grid-attach}
      @about-function{grid-attach-next-to}
      @about-function{grid-remove}
      @about-function{grid-child-at}
      @about-function{grid-query-child}
      @about-function{grid-insert-row}
      @about-function{grid-insert-column}
      @about-function{grid-remove-row}
      @about-function{grid-remove-column}
      @about-function{grid-insert-next-to}
      @about-function{grid-row-baseline-position}
    @end{subsection}
    @begin[GtkRevealer]{subsection}
      @about-symbol{revealer-transition-type}
      @about-class{revealer}
      @about-generic{revealer-child}
      @about-generic{revealer-child-revealed}
      @about-generic{revealer-reveal-child}
      @about-generic{revealer-transition-duration}
      @about-generic{revealer-transition-type}
      @about-function{revealer-new}
    @end{subsection}
    @begin[GtkListBox]{subsection}
      @about-class{list-box-row}
      @about-generic{list-box-row-activatable}
      @about-generic{list-box-row-child}
      @about-generic{list-box-row-selectable}
      @about-function{list-box-row-new}
      @about-function{list-box-row-changed}
      @about-function{list-box-row-is-selected}
      @about-function{list-box-row-header}
      @about-function{list-box-row-index}
      @about-class{list-box}
      @about-generic{list-box-accept-unpaired-release}
      @about-generic{list-box-activate-on-single-click}
      @about-generic{list-box-selection-mode}
      @about-generic{list-box-show-separators}
      @about-generic{list-box-tab-behavior}
      @about-function{list-box-new}
      @about-function{list-box-prepend}
      @about-function{list-box-append}
      @about-function{list-box-insert}
      @about-function{list-box-remove}
      @about-function{list-box-remove-all}
      @about-function{list-box-select-row}
      @about-function{list-box-unselect-row}
      @about-function{list-box-select-all}
      @about-function{list-box-unselect-all}
      @about-function{list-box-selected-row}
      @about-function{list-box-selected-rows}
      @about-function{list-box-adjustment}
      @about-function{list-box-set-placeholder}
      @about-function{list-box-row-at-index}
      @about-function{list-box-row-at-y}
      @about-symbol{list-box-foreach-func}
      @about-function{list-box-selected-foreach}
      @about-symbol{list-box-filter-func}
      @about-function{list-box-set-filter-func}
      @about-function{list-box-invalidate-filter}
      @about-symbol{list-box-update-header-func}
      @about-function{list-box-set-header-func}
      @about-function{list-box-invalidate-headers}
      @about-symbol{list-box-sort-func}
      @about-function{list-box-set-sort-func}
      @about-function{list-box-invalidate-sort}
      @about-function{list-box-drag-highlight-row}
      @about-function{list-box-drag-unhighlight-row}
      @about-symbol{list-box-create-widget-func}
      @about-function{list-box-bind-model}
    @end{subsection}
    @begin[GtkFlowBox]{subsection}
      @about-class{flow-box-child}
      @about-generic{flow-box-child-child}
      @about-function{flow-box-child-new}
      @about-function{flow-box-child-index}
      @about-function{flow-box-child-is-selected}
      @about-function{flow-box-child-changed}
      @about-class{flow-box}
      @about-generic{flow-box-accept-unpaired-release}
      @about-generic{flow-box-activate-on-single-click}
      @about-generic{flow-box-column-spacing}
      @about-generic{flow-box-homogeneous}
      @about-generic{flow-box-max-children-per-line}
      @about-generic{flow-box-min-children-per-line}
      @about-generic{flow-box-row-spacing}
      @about-generic{flow-box-selection-mode}
      @about-function{flow-box-new}
      @about-function{flow-box-insert}
      @about-function{flow-box-append}
      @about-function{flow-box-prepend}
      @about-function{flow-box-remove}
      @about-function{flow-box-remove-all}
      @about-function{flow-box-child-at-index}
      @about-function{flow-box-child-at-pos}
      @about-function{flow-box-set-hadjustment}
      @about-function{flow-box-set-vadjustment}
      @about-symbol{flow-box-foreach-func}
      @about-function{flow-box-selected-foreach}
      @about-function{flow-box-selected-children}
      @about-function{flow-box-select-child}
      @about-function{flow-box-unselect-child}
      @about-function{flow-box-select-all}
      @about-function{flow-box-unselect-all}
      @about-symbol{flow-box-filter-func}
      @about-function{flow-box-set-filter-func}
      @about-function{flow-box-invalidate-filter}
      @about-symbol{flow-box-sort-func}
      @about-function{flow-box-set-sort-func}
      @about-function{flow-box-invalidate-sort}
      @about-symbol{flow-box-create-widget-func}
      @about-function{flow-box-bind-model}
    @end{subsection}
    @begin[GtkStack]{subsection}
      @about-symbol{stack-transition-type}
      @about-class{stack-page}
      @about-generic{stack-page-child}
      @about-generic{stack-page-icon-name}
      @about-generic{stack-page-name}
      @about-generic{stack-page-needs-attention}
      @about-generic{stack-page-title}
      @about-generic{stack-page-use-underline}
      @about-generic{stack-page-visible}
      @about-class{stack}
      @about-generic{stack-hhomogeneous}
      @about-generic{stack-interpolate-size}
      @about-generic{stack-pages}
      @about-generic{stack-transition-duration}
      @about-generic{stack-transition-running}
      @about-generic{stack-transition-type}
      @about-generic{stack-vhomogeneous}
      @about-generic{stack-visible-child}
      @about-generic{stack-visible-child-name}
      @about-function{stack-new}
      @about-function{stack-add-child}
      @about-function{stack-add-named}
      @about-function{stack-add-titled}
      @about-function{stack-remove}
      @about-function{stack-child-by-name}
      @about-function{stack-page}
      @about-function{stack-set-visible-child-full}
    @end{subsection}
    @begin[GtkStackSwitcher]{subsection}
      @about-class{stack-switcher}
      @about-generic{stack-switcher-stack}
      @about-function{stack-switcher-new}
    @end{subsection}
    @begin[GtkStackSidebar]{subsection}
      @about-class{stack-sidebar}
      @about-generic{stack-sidebar-stack}
      @about-function{stack-sidebar-new}
    @end{subsection}
    @begin[GtkActionBar]{subsection}
      @about-class{action-bar}
      @about-generic{action-bar-revealed}
      @about-function{action-bar-new}
      @about-function{action-bar-pack-start}
      @about-function{action-bar-pack-end}
      @about-function{action-bar-remove}
      @about-function{action-bar-center-widget}
    @end{subsection}
    @begin[GtkHeaderBar]{subsection}
      @about-class{header-bar}
      @about-generic{header-bar-decoration-layout}
      @about-generic{header-bar-show-title-buttons}
      @about-generic{header-bar-title-widget}
      @about-generic{header-bar-use-native-controls}
      @about-function{header-bar-new}
      @about-function{header-bar-pack-start}
      @about-function{header-bar-pack-end}
      @about-function{header-bar-remove}
    @end{subsection}
    @begin[GtkOverlay]{subsection}
      @about-class{overlay}
      @about-generic{overlay-child}
      @about-function{overlay-new}
      @about-function{overlay-add-overlay}
      @about-function{overlay-remove-overlay}
      @about-function{overlay-measure-overlay}
      @about-function{overlay-clip-overlay}
    @end{subsection}
    @begin[GtkPaned]{subsection}
      @about-class{paned}
      @about-generic{paned-end-child}
      @about-generic{paned-max-position}
      @about-generic{paned-min-position}
      @about-generic{paned-position}
      @about-generic{paned-position-set}
      @about-generic{paned-resize-end-child}
      @about-generic{paned-resize-start-child}
      @about-generic{paned-shrink-end-child}
      @about-generic{paned-shrink-start-child}
      @about-generic{paned-start-child}
      @about-generic{paned-wide-handle}
      @about-function{paned-new}
    @end{subsection}
    @begin[GtkNotebook]{subsection}
      @about-symbol{notebook-tab}
      @about-class{notebook-page}
      @about-generic{notebook-page-child}
      @about-generic{notebook-page-detachable}
      @about-generic{notebook-page-menu}
      @about-generic{notebook-page-menu-label}
      @about-generic{notebook-page-position}
      @about-generic{notebook-page-reorderable}
      @about-generic{notebook-page-tab}
      @about-generic{notebook-page-tab-expand}
      @about-generic{notebook-page-tab-fill}
      @about-generic{notebook-page-tab-label}
      @about-class{notebook-pages}
      @about-class{notebook}
      @about-generic{notebook-enable-popup}
      @about-generic{notebook-group-name}
      @about-generic{notebook-page}
      @about-function{notebook-pages}
      @about-generic{notebook-scrollable}
      @about-generic{notebook-show-border}
      @about-generic{notebook-show-tabs}
      @about-generic{notebook-tab-pos}
      @about-function{notebook-new}
      @about-function{notebook-n-pages}
      @about-function{notebook-nth-page}
      @about-function{notebook-page-num}
      @about-function{notebook-current-page}
      @about-function{notebook-next-page}
      @about-function{notebook-prev-page}
      @about-function{notebook-add-page}
      @about-function{notebook-remove-page}
      @about-function{notebook-detach-tab}
      @about-function{notebook-reorder-child}
      @about-function{notebook-popup-enable}
      @about-function{notebook-popup-disable}
      @about-function{notebook-tab-detachable}
      @about-function{notebook-tab-reorderable}
      @about-function{notebook-menu-label}
      @about-function{notebook-menu-label-text}
      @about-function{notebook-tab-label}
      @about-function{notebook-tab-label-text}
      @about-function{notebook-action-widget}
    @end{subsection}
    @begin[GtkExpander]{subsection}
      @about-class{expander}
      @about-generic{expander-child}
      @about-generic{expander-expanded}
      @about-generic{expander-label}
      @about-generic{expander-label-widget}
      @about-generic{expander-resize-toplevel}
      @about-generic{expander-use-markup}
      @about-generic{expander-use-underline}
      @about-function{expander-new}
      @about-function{expander-new-with-mnemonic}
    @end{subsection}
    @begin[GtkAspectFrame]{subsection}
      @about-class{aspect-frame}
      @about-generic{aspect-frame-child}
      @about-generic{aspect-frame-obey-child}
      @about-generic{aspect-frame-ratio}
      @about-generic{aspect-frame-xalign}
      @about-generic{aspect-frame-yalign}
      @about-function{aspect-frame-new}
    @end{subsection}
    @begin[GtkFixed]{subsection}
      @about-class{fixed}
      @about-function{fixed-new}
      @about-function{fixed-put}
      @about-function{fixed-remove}
      @about-function{fixed-move}
      @about-function{fixed-child-position}
      @about-function{fixed-child-transform}
    @end{subsection}
  @end{section}
  @begin[Layout Managers]{section}
    @begin[GtkLayoutManager]{subsection}
      @about-class{layout-manager}
      @about-function{layout-manager-measure}
      @about-function{layout-manager-allocate}
      @about-function{layout-manager-request-mode}
      @about-function{layout-manager-widget}
      @about-function{layout-manager-layout-child}
      @about-function{layout-manager-layout-changed}
    @end{subsection}
    @begin[GtkLayoutChild]{subsection}
      @about-class{layout-child}
      @about-generic{layout-child-child-widget}
      @about-generic{layout-child-layout-manager}
    @end{subsection}
    @begin[GtkBinLayout]{subsection}
      @about-class{bin-layout}
      @about-function{bin-layout-new}
    @end{subsection}
    @begin[GtkBoxLayout]{subsection}
      @about-class{box-layout}
      @about-generic{box-layout-child}
      @about-generic{box-layout-baseline-position}
      @about-generic{box-layout-homogeneous}
      @about-generic{box-layout-spacing}
      @about-function{box-layout-new}
    @end{subsection}
    @begin[GtkCenterLayout]{subsection}
      @about-class{center-layout}
      @about-generic{center-layout-shrink-center-last}
      @about-function{center-layout-new}
      @about-function{center-layout-orientation}
      @about-function{center-layout-baseline-position}
      @about-function{center-layout-start-widget}
      @about-function{center-layout-center-widget}
      @about-function{center-layout-end-widget}
    @end{subsection}
    @begin[GtkFixedLayout]{subsection}
      @about-class{fixed-layout-child}
      @about-generic{fixed-layout-child-transform}
      @about-class{fixed-layout}
      @about-function{fixed-layout-new}
    @end{subsection}
    @begin[GtkGridLayout]{subsection}
      @about-class{grid-layout-child}
      @about-generic{grid-layout-child-column}
      @about-generic{grid-layout-child-column-span}
      @about-generic{grid-layout-child-row}
      @about-generic{grid-layout-child-row-span}
      @about-class{grid-layout}
      @about-generic{grid-layout-baseline-row}
      @about-generic{grid-layout-column-homogeneous}
      @about-generic{grid-layout-column-spacing}
      @about-generic{grid-layout-row-homogeneous}
      @about-generic{grid-layout-row-spacing}
      @about-function{grid-layout-new}
      @about-function{grid-layout-row-baseline-position}
    @end{subsection}
    @begin[GtkOverlayLayout]{subsection}
      @about-class{overlay-layout-child}
      @about-generic{overlay-layout-child-clip-overlay}
      @about-generic{overlay-layout-child-measure}
      @about-class{overlay-layout}
      @about-function{overlay-layout-new}
    @end{subsection}
    @begin[GtkCustomLayout]{subsection}
      @about-class{custom-layout}
      @about-symbol{custom-request-mode-func}
      @about-symbol{custom-measure-func}
      @about-symbol{custom-allocate-func}
      @about-function{custom-layout-new}
    @end{subsection}
    @begin[GtkConstraint]{subsection}
      @about-class{constraint-target}
      @about-class{constraint}
      @about-generic{constraint-constant}
      @about-generic{constraint-multiplier}
      @about-generic{constraint-relation}
      @about-generic{constraint-source}
      @about-generic{constraint-source-attribute}
      @about-generic{constraint-strength}
      @about-generic{constraint-target}
      @about-generic{constraint-target-attribute}
      @about-function{constraint-new}
      @about-function{constraint-new-constant}
      @about-function{constraint-is-required}
      @about-function{constraint-is-attached}
      @about-function{constraint-is-constant}
    @end{subsection}
    @begin[GtkConstraintGuide]{subsection}
      @about-class{constraint-guide}
      @about-generic{constraint-guide-max-height}
      @about-generic{constraint-guide-max-width}
      @about-generic{constraint-guide-min-height}
      @about-generic{constraint-guide-min-width}
      @about-generic{constraint-guide-name}
      @about-generic{constraint-guide-nat-height}
      @about-generic{constraint-guide-nat-width}
      @about-generic{constraint-guide-strength}
      @about-function{constraint-guide-new}
      @about-function{constraint-guide-min-size}
      @about-function{constraint-guide-nat-size}
      @about-function{constraint-guide-max-size}
    @end{subsection}
    @begin[GtkConstraintLayout]{subsection}
      @about-class{constraint-layout-child}
      @about-class{constraint-layout}
      @about-function{constraint-layout-new}
      @about-function{constraint-layout-add-constraint}
      @about-function{constraint-layout-remove-constraint}
      @about-function{constraint-layout-remove-all-constraints}
      @about-function{constraint-layout-add-guide}
      @about-function{constraint-layout-remove-guide}
      @about-function{constraint-layout-add-constraints-from-description}
      @about-function{constraint-layout-add-constraints-from-descriptionv}
      @about-function{constraint-layout-observe-constraints}
      @about-function{constraint-layout-observe-guides}
    @end{subsection}
  @end{section}
  @begin[Display Widgets]{section}
    @begin[GtkLabel]{subsection}
      @about-class{label}
      @about-generic{label-attributes}
      @about-generic{label-ellipsize}
      @about-generic{label-extra-menu}
      @about-generic{label-justify}
      @about-generic{label-label}
      @about-generic{label-lines}
      @about-generic{label-max-width-chars}
      @about-generic{label-mnemonic-keyval}
      @about-generic{label-mnemonic-widget}
      @about-generic{label-natural-wrap-mode}
      @about-generic{label-selectable}
      @about-generic{label-single-line-mode}
      @about-generic{label-tabs}
      @about-generic{label-use-markup}
      @about-generic{label-use-underline}
      @about-generic{label-width-chars}
      @about-generic{label-wrap}
      @about-generic{label-wrap-mode}
      @about-generic{label-xalign}
      @about-generic{label-yalign}
      @about-function{label-new}
      @about-function{label-new-with-mnemonic}
      @about-function{label-text}
      @about-function{label-set-markup}
      @about-function{label-set-text-with-mnemonic}
      @about-function{label-set-markup-with-mnemonic}
      @about-function{label-layout}
      @about-function{label-layout-offsets}
      @about-function{label-select-region}
      @about-function{label-selection-bounds}
      @about-function{label-current-uri}
    @end{subsection}
    @begin[GtkInscription]{subsection}
      @about-symbol{inscription-overflow}
      @about-class{inscription}
      @about-generic{inscription-attributes}
      @about-generic{inscription-markup}
      @about-generic{inscription-min-chars}
      @about-generic{inscription-min-lines}
      @about-generic{inscription-nat-chars}
      @about-generic{inscription-nat-lines}
      @about-generic{inscription-text}
      @about-generic{inscription-text-overflow}
      @about-generic{inscription-wrap-mode}
      @about-generic{inscription-xalign}
      @about-generic{inscription-yalign}
      @about-function{inscription-new}
    @end{subsection}
    @begin[GtkImage]{subsection}
      @about-symbol{image-type}
      @about-class{image}
      @about-generic{image-file}
      @about-generic{image-gicon}
      @about-generic{image-icon-name}
      @about-generic{image-icon-size}
      @about-generic{image-paintable}
      @about-generic{image-pixel-size}
      @about-generic{image-resource}
      @about-generic{image-storage-type}
      @about-generic{image-use-fallback}
      @about-function{image-new}
      @about-function{image-new-from-file}
      @about-function{image-new-from-resource}
      @about-function{image-new-from-pixbuf}
      @about-function{image-new-from-paintable}
      @about-function{image-new-from-icon-name}
      @about-function{image-new-from-gicon}
      @about-function{image-clear}
      @about-function{image-set-from-file}
      @about-function{image-set-from-resource}
      @about-function{image-set-from-pixbuf}
      @about-function{image-set-from-paintable}
      @about-function{image-set-from-icon-name}
      @about-function{image-set-from-gicon}
    @end{subsection}
    @begin[GtkPicture]{subsection}
      @about-symbol{content-fit}
      @about-class{picture}
      @about-generic{picture-alternative-text}
      @about-generic{picture-can-shrink}
      @about-generic{picture-content-fit}
      @about-generic{picture-file}
      @about-generic{picture-keep-aspect-ratio}
      @about-generic{picture-paintable}
      @about-function{picture-new}
      @about-function{picture-new-for-paintable}
      @about-function{picture-new-for-pixbuf}
      @about-function{picture-new-for-file}
      @about-function{picture-new-for-filename}
      @about-function{picture-new-for-resource}
      @about-function{picture-set-pixbuf}
      @about-function{picture-set-filename}
      @about-function{picture-set-resource}
    @end{subsection}
    @begin[GtkSpinner]{subsection}
      @about-class{spinner}
      @about-generic{spinner-spinning}
      @about-function{spinner-new}
      @about-function{spinner-start}
      @about-function{spinner-stop}
    @end{subsection}
    @begin[GtkProgressBar]{subsection}
      @about-class{progress-bar}
      @about-generic{progress-bar-ellipsize}
      @about-generic{progress-bar-fraction}
      @about-generic{progress-bar-inverted}
      @about-generic{progress-bar-pulse-step}
      @about-generic{progress-bar-show-text}
      @about-generic{progress-bar-text}
      @about-function{progress-bar-new}
      @about-function{progress-bar-pulse}
    @end{subsection}
    @begin[GtkLevelBar]{subsection}
      @about-symbol{level-bar-mode}
      @about-class{level-bar}
      @about-generic{level-bar-inverted}
      @about-generic{level-bar-max-value}
      @about-generic{level-bar-min-value}
      @about-generic{level-bar-mode}
      @about-generic{level-bar-value}
      @about-function{level-bar-new}
      @about-function{level-bar-new-for-interval}
      @about-function{level-bar-add-offset-value}
      @about-function{level-bar-remove-offset-value}
      @about-function{level-bar-offset-value}
    @end{subsection}
    @begin[GtkCalendar]{subsection}
      @about-class{calendar}
      @about-generic{calendar-day}
      @about-generic{calendar-month}
      @about-generic{calendar-show-day-names}
      @about-generic{calendar-show-heading}
      @about-generic{calendar-show-week-numbers}
      @about-generic{calendar-year}
      @about-function{calendar-new}
      @about-function{calendar-select-day}
      @about-function{calendar-mark-day}
      @about-function{calendar-unmark-day}
      @about-function{calendar-day-is-marked}
      @about-function{calendar-clear-marks}
      @about-function{calendar-date}
    @end{subsection}
  @end{section}
  @begin[Media Support]{section}
    @begin[GtkGraphicsOffload]{subsection}
      @about-symbol{graphics-offload-enabled}
      @about-class{graphics-offload}
      @about-generic{graphics-offload-black-background}
      @about-generic{graphics-offload-child}
      @about-generic{graphics-offload-enabled}
      @about-function{graphics-offload-new}
    @end{subsection}
    @begin[GtkVideo]{subsection}
      @about-class{video}
      @about-generic{video-autoplay}
      @about-generic{video-file}
      @about-generic{video-graphics-offload}
      @about-generic{video-loop}
      @about-generic{video-media-stream}
      @about-function{video-new}
      @about-function{video-new-for-media-stream}
      @about-function{video-new-for-file}
      @about-function{video-new-for-filename}
      @about-function{video-new-for-resource}
      @about-function{video-set-filename}
      @about-function{video-set-resource}
    @end{subsection}
    @begin[GtkMediaControls]{subsection}
      @about-class{media-controls}
      @about-generic{media-controls-media-stream}
      @about-function{media-controls-new}
    @end{subsection}
    @begin[GtkMediaStream]{subsection}
      @about-class{media-stream}
      @about-generic{media-stream-duration}
      @about-generic{media-stream-ended}
      @about-generic{media-stream-error}
      @about-generic{media-stream-has-audio}
      @about-generic{media-stream-has-video}
      @about-generic{media-stream-loop}
      @about-generic{media-stream-muted}
      @about-generic{media-stream-playing}
      @about-generic{media-stream-prepared}
      @about-generic{media-stream-seekable}
      @about-generic{media-stream-seeking}
      @about-generic{media-stream-timestamp}
      @about-generic{media-stream-volume}
      @about-function{media-stream-is-prepared}
      @about-function{media-stream-play}
      @about-function{media-stream-pause}
      @about-function{media-stream-is-seekable}
      @about-function{media-stream-is-seeking}
      @about-function{media-stream-seek}
      @about-function{media-stream-realize}
      @about-function{media-stream-unrealize}
      @about-function{media-stream-prepared}
      @about-function{media-stream-unprepared}
      @about-function{media-stream-update}
      @about-function{media-stream-ended}
      @about-function{media-stream-seek-success}
      @about-function{media-stream-seek-failed}
      @about-function{media-stream-gerror}
      @about-function{media-stream-error}
      @about-function{media-stream-error-valist}
    @end{subsection}
    @begin[GtkMediaFile]{subsection}
      @about-class{media-file}
      @about-generic{media-file-file}
      @about-generic{media-file-input-stream}
      @about-function{media-file-new}
      @about-function{media-file-new-for-filename}
      @about-function{media-file-new-for-resource}
      @about-function{media-file-new-for-file}
      @about-function{media-file-new-for-input-stream}
      @about-function{media-file-clear}
      @about-function{media-file-set-filename}
      @about-function{media-file-set-resource}
      @about-function{media-file-file}
      @about-function{media-file-input-stream}
    @end{subsection}
  @end{section}
  @begin[Buttons and Toggles]{section}
    @begin[GtkButton]{subsection}
      @about-class{button}
      @about-generic{button-child}
      @about-generic{button-has-frame}
      @about-generic{button-icon-name}
      @about-generic{button-label}
      @about-generic{button-use-underline}
      @about-function{button-new}
      @about-function{button-new-with-label}
      @about-function{button-new-with-mnemonic}
      @about-function{button-new-from-icon-name}
    @end{subsection}
    @begin[GtkToggleButton]{subsection}
      @about-class{toggle-button}
      @about-generic{toggle-button-active}
      @about-generic{toggle-button-group}
      @about-function{toggle-button-new}
      @about-function{toggle-button-new-with-label}
      @about-function{toggle-button-new-with-mnemonic}
      @about-function{toggle-button-toggled}
    @end{subsection}
    @begin[GtkCheckButton]{subsection}
      @about-class{check-button}
      @about-generic{check-button-active}
      @about-generic{check-button-child}
      @about-generic{check-button-group}
      @about-generic{check-button-inconsistent}
      @about-generic{check-button-label}
      @about-generic{check-button-use-underline}
      @about-function{check-button-new}
      @about-function{check-button-new-with-label}
      @about-function{check-button-new-with-mnemonic}
      @about-function{check-button-set-group}
    @end{subsection}
    @begin[GtkMenuButton]{subsection}
      @about-symbol{arrow-type}
      @about-class{menu-button}
      @about-generic{menu-button-active}
      @about-generic{menu-button-always-show-arrow}
      @about-generic{menu-button-child}
      @about-generic{menu-button-direction}
      @about-generic{menu-button-has-frame}
      @about-generic{menu-button-icon-name}
      @about-generic{menu-button-label}
      @about-generic{menu-button-menu-model}
      @about-generic{menu-button-popover}
      @about-generic{menu-button-primary}
      @about-generic{menu-button-use-underline}
      @about-function{menu-button-new}
      @about-function{menu-button-popup}
      @about-function{menu-button-popdown}
      @about-symbol{menu-button-create-popup-func}
      @about-function{menu-button-set-create-popup-func}
    @end{subsection}
    @begin[GtkLinkButton]{subsection}
      @about-class{link-button}
      @about-generic{link-button-uri}
      @about-generic{link-button-visited}
      @about-function{link-button-new}
      @about-function{link-button-new-with-label}
    @end{subsection}
    @begin[GtkScaleButton]{subsection}
      @about-class{scale-button}
      @about-generic{scale-button-active}
      @about-generic{scale-button-adjustment}
      @about-generic{scale-button-has-frame}
      @about-generic{scale-button-icons}
      @about-generic{scale-button-value}
      @about-function{scale-button-new}
      @about-function{scale-button-popup}
      @about-function{scale-button-plus-button}
      @about-function{scale-button-minus-button}
    @end{subsection}
    @begin[GtkSwitch]{subsection}
      @about-class{switch}
      @about-generic{switch-active}
      @about-generic{switch-state}
      @about-function{switch-new}
    @end{subsection}
  @end{section}
  @begin[Numeric and Text Data Entry]{section}
    @begin[GtkEditable]{subsection}
      @about-symbol{editable-properties}
      @about-class{editable}
      @about-generic{editable-cursor-position}
      @about-generic{editable-editable}
      @about-generic{editable-enable-undo}
      @about-generic{editable-max-width-chars}
      @about-generic{editable-selection-bound}
      @about-generic{editable-text}
      @about-generic{editable-width-chars}
      @about-generic{editable-xalign}
      @about-function{editable-chars}
      @about-function{editable-insert-text}
      @about-function{editable-delete-text}
      @about-function{editable-selection-bounds}
      @about-function{editable-select-region}
      @about-function{editable-delete-selection}
      @about-function{editable-position}
      @about-function{editable-alignment}
      @about-function{editable-install-properties}
      @about-function{editable-delegate}
      @about-function{editable-init-delegate}
      @about-function{editable-finish-delegate}
      @about-function{editable-property}
    @end{subsection}
    @begin[GtkEntryBuffer]{subsection}
      @about-class{entry-buffer}
      @about-generic{entry-buffer-length}
      @about-generic{entry-buffer-max-length}
      @about-generic{entry-buffer-text}
      @about-function{entry-buffer-new}
      @about-function{entry-buffer-bytes}
      @about-function{entry-buffer-insert-text}
      @about-function{entry-buffer-delete-text}
      @about-function{entry-buffer-emit-deleted-text}
      @about-function{entry-buffer-emit-inserted-text}
    @end{subsection}
    @begin[GtkText]{subsection}
      @about-class{text}
      @about-generic{text-activates-default}
      @about-generic{text-attributes}
      @about-generic{text-buffer}
      @about-generic{text-enable-emoji-completion}
      @about-generic{text-extra-menu}
      @about-generic{text-im-module}
      @about-generic{text-input-hints}
      @about-generic{text-input-purpose}
      @about-generic{text-invisible-char}
      @about-generic{text-invisible-char-set}
      @about-generic{text-max-length}
      @about-generic{text-overwrite-mode}
      @about-generic{text-placeholder-text}
      @about-generic{text-propagate-text-width}
      @about-generic{text-scroll-offset}
      @about-generic{text-tabs}
      @about-generic{text-truncate-multiline}
      @about-generic{text-visibility}
      @about-function{text-new}
      @about-function{text-new-with-buffer}
      @about-function{text-unset-invisible-char}
      @about-function{text-text-length}
      @about-function{text-grab-focus-without-selecting}
      @about-function{text-compute-cursor-extents}
    @end{subsection}
    @begin[GtkEntry]{subsection}
      @about-symbol{entry-icon-position}
      @about-class{entry}
      @about-generic{entry-activates-default}
      @about-generic{entry-attributes}
      @about-generic{entry-buffer}
      @about-generic{entry-completion}
      @about-generic{entry-enable-emoji-completion}
      @about-generic{entry-extra-menu}
      @about-generic{entry-has-frame}
      @about-generic{entry-im-module}
      @about-generic{entry-input-hints}
      @about-generic{entry-input-purpose}
      @about-generic{entry-invisible-char}
      @about-generic{entry-invisible-char-set}
      @about-generic{entry-max-length}
      @about-generic{entry-overwrite-mode}
      @about-generic{entry-placeholder-text}
      @about-generic{entry-primary-icon-activatable}
      @about-generic{entry-primary-icon-gicon}
      @about-generic{entry-primary-icon-name}
      @about-generic{entry-primary-icon-paintable}
      @about-generic{entry-primary-icon-sensitive}
      @about-generic{entry-primary-icon-storage-type}
      @about-generic{entry-primary-icon-tooltip-markup}
      @about-generic{entry-primary-icon-tooltip-text}
      @about-generic{entry-progress-fraction}
      @about-generic{entry-progress-pulse-step}
      @about-generic{entry-scroll-offset}
      @about-generic{entry-secondary-icon-activatable}
      @about-generic{entry-secondary-icon-gicon}
      @about-generic{entry-secondary-icon-name}
      @about-generic{entry-secondary-icon-paintable}
      @about-generic{entry-secondary-icon-sensitive}
      @about-generic{entry-secondary-icon-storage-type}
      @about-generic{entry-secondary-icon-tooltip-markup}
      @about-generic{entry-secondary-icon-tooltip-text}
      @about-generic{entry-show-emoji-icon}
      @about-generic{entry-tabs}
      @about-generic{entry-text-length}
      @about-generic{entry-truncate-multiline}
      @about-generic{entry-visibility}
      @about-function{entry-new}
      @about-function{entry-new-with-buffer}
      @about-function{entry-unset-invisible-char}
      @about-function{entry-alignment}
      @about-function{entry-progress-pulse}
      @about-function{entry-reset-im-context}
      @about-function{entry-set-icon-from-paintable}
      @about-function{entry-set-icon-from-icon-name}
      @about-function{entry-set-icon-from-gicon}
      @about-function{entry-icon-storage-type}
      @about-function{entry-icon-paintable}
      @about-function{entry-icon-name}
      @about-function{entry-icon-gicon}
      @about-function{entry-icon-activatable}
      @about-function{entry-icon-sensitive}
      @about-function{entry-icon-at-pos}
      @about-function{entry-icon-tooltip-text}
      @about-function{entry-icon-tooltip-markup}
      @about-function{entry-set-icon-drag-source}
      @about-function{entry-current-icon-drag-source}
      @about-function{entry-icon-area}
      @about-function{entry-grab-focus-without-selecting}
    @end{subsection}
    @begin[GtkPasswordEntry]{subsection}
      @about-class{password-entry-buffer}
      @about-function{password-entry-buffer-new}
      @about-class{password-entry}
      @about-generic{password-entry-activates-default}
      @about-generic{password-entry-extra-menu}
      @about-generic{password-entry-placeholder-text}
      @about-generic{password-entry-show-peek-icon}
      @about-function{password-entry-new}
    @end{subsection}
    @begin[GtkScale]{subsection}
      @about-class{scale}
      @about-generic{scale-digits}
      @about-generic{scale-draw-value}
      @about-generic{scale-has-origin}
      @about-generic{scale-value-pos}
      @about-function{scale-new}
      @about-function{scale-new-with-range}
      @about-symbol{scale-format-value-func}
      @about-function{scale-set-format-value-func}
      @about-function{scale-layout}
      @about-function{scale-layout-offsets}
      @about-function{scale-add-mark}
      @about-function{scale-clear-marks}
    @end{subsection}
    @begin[GtkSpinButton]{subsection}
      @about-symbol{spin-button-update-policy}
      @about-symbol{spin-type}
      @about-class{spin-button}
      @about-generic{spin-button-activates-default}
      @about-generic{spin-button-adjustment}
      @about-generic{spin-button-climb-rate}
      @about-generic{spin-button-digits}
      @about-generic{spin-button-numeric}
      @about-generic{spin-button-snap-to-ticks}
      @about-generic{spin-button-update-policy}
      @about-generic{spin-button-value}
      @about-generic{spin-button-wrap}
      @about-function{spin-button-new}
      @about-function{spin-button-new-with-range}
      @about-function{spin-button-increments}
      @about-function{spin-button-range}
      @about-function{spin-button-value-as-int}
      @about-function{spin-button-configure}
      @about-function{spin-button-spin}
      @about-function{spin-button-update}
    @end{subsection}
    @begin[GtkSearchEntry]{subsection}
      @about-class{search-entry}
      @about-generic{search-entry-activates-default}
      @about-generic{search-entry-input-hints}
      @about-generic{search-entry-input-purpose}
      @about-generic{search-entry-placeholder-text}
      @about-generic{search-entry-search-delay}
      @about-function{search-entry-new}
      @about-function{search-entry-key-capture-widget}
    @end{subsection}
    @begin[GtkSearchBar]{subsection}
      @about-class{search-bar}
      @about-generic{search-bar-child}
      @about-generic{search-bar-key-capture-widget}
      @about-generic{search-bar-search-mode-enabled}
      @about-generic{search-bar-show-close-button}
      @about-function{search-bar-new}
      @about-function{search-bar-connect-entry}
      @about-function{search-bar-search-mode}
    @end{subsection}
    @begin[GtkEditableLabel]{subsection}
      @about-class{editable-label}
      @about-generic{editable-label-editing}
      @about-function{editable-label-new}
      @about-function{editable-label-start-editing}
      @about-function{editable-label-stop-editing}
    @end{subsection}
  @end{section}
  @begin[Multiline Text Editor]{section}
    @begin[Conceptual Overview]{subsection}
      GTK has a powerful framework for multiline text editing. The primary
      objects involved in the process are the @class{gtk:text-buffer} object,
      which represents the text being edited, and the @class{gtk:text-view}
      widget, a widget which can display a @class{gtk:text-buffer} object. Each
      text buffer can be displayed by any number of views.

      One of the important things to remember about text in GTK is that it is
      in the UTF-8 encoding. This means that one character can be encoded as
      multiple bytes. Character counts are usually referred to as offsets, while
      byte counts are called indexes. If you confuse these two, things will work
      fine with ASCII, but as soon as your text buffer contains multibyte
      characters, bad things will happen.

      Text in a text buffer can be marked with tags. A tag is an attribute that
      can be applied to some range of text. For example, a tag might be called
      \"bold\" and make the text inside the tag bold. However, the tag concept
      is more general than that. Tags do not have to affect appearance. They can
      instead affect the behavior of mouse and key presses, \"lock\" a range of
      text so the user cannot edit it, or countless other things. A tag is
      represented by a @class{gtk:text-tag} object. One @class{gtk:text-tag}
      object can be applied to any number of text ranges in any number of
      text buffers.

      Each tag is stored in a @class{gtk:text-tag-table} object. A tag table
      defines a set of tags that can be used together. Each text buffer has one
      tag table associated with it. Only tags from that tag table can be used
      with the text buffer. A single tag table can be shared between multiple
      text buffers, however. Tags can have names, which is convenient sometimes.
      For example, you can name your tag that makes things bold @code{\"bold\"},
      but they can also be anonymous, which is convenient if you are creating
      tags on-the-fly.

      Most text manipulation is accomplished with iterators, represented by a
      @class{gtk:text-iter} instance. An iterator represents a position between
      two characters in the text buffer. The @class{gtk:text-iter} structure is
      a structure designed to be allocated on the stack. It is guaranteed to be
      copiable by value and never contain any heap-allocated data. Iterators are
      not valid indefinitely. Whenever the text buffer is modified in a way that
      affects the number of characters in the text buffer, all outstanding
      iterators become invalid. Note that deleting 5 characters and then
      reinserting 5 still invalidates iterators, though you end up with the same
      number of characters you pass through a state with a different number.

      Because of this, iterators cannot be used to preserve positions across
      buffer modifications. To preserve a position, the @class{gtk:text-mark}
      object is ideal. You can think of a mark as an invisible cursor or
      insertion point. It floats in the text buffer, saving a position. If the
      text surrounding the mark is deleted, the mark remains in the position
      the text once occupied. If text is inserted at the mark, the mark ends up
      either to the left or to the right of the new text, depending on its
      gravity. The standard text cursor in left-to-right languages is a mark
      with right gravity, because it stays to the right of inserted text.

      Like tags, marks can be either named or anonymous. There are two marks
      built-in to the @class{gtk:text-buffer} class. These are named
      @code{\"insert\"} and @code{\"selection_bound\"} and refer to the
      insertion point and the boundary of the selection which is not the
      insertion point, respectively. If no text is selected, these two marks
      will be in the same position. You can manipulate what is selected and
      where the cursor appears by moving these marks around. If you want to
      place the cursor in response to a user action, be sure to use the
      @fun{gtk:text-buffer-place-cursor} function, which moves both at once
      without causing a temporary selection. Moving one then the other
      temporarily selects the range in between the old and new positions.

      Text buffers always contain at least one line, but may be empty, that is,
      buffers can contain zero characters. The last line in the text buffer
      never ends in a line separator (such as newline). The other lines in the
      text buffer always end in a line separator. Line separators count as
      characters when computing character counts and character offsets. Note
      that some Unicode line separators are represented with multiple bytes in
      UTF-8, and the two-character sequence @code{\"\\r\\n\"} is also
      considered a line separator.

      @subheading{Simple Example}
      A simple usage of the @class{gtk:text-view} widget might look like this:
      @begin{pre}
(defun do-text-view-simple (&optional application)
  (let* ((textview (make-instance 'gtk:text-view
                                  :wrap-mode :word
                                  :top-margin 6
                                  :top-bottom 6
                                  :left-margin 6
                                  :right-margin 6))
         (window (make-instance 'gtk:window
                                :application application
                                :child textview
                                :title \"Simple Text View\"
                                :default-width 350
                                :default-height 200))
         (buffer (gtk:text-view-buffer textview)))
    (g:signal-connect window \"close-request\"
        (lambda (widget)
          (declare (ignore widget))
          (let ((start (gtk:text-buffer-start-iter buffer))
                (end (gtk:text-buffer-end-iter buffer))
                (include-hidden-chars t))
            (print (gtk:text-buffer-get-text buffer
                                             start
                                             end
                                             include-hidden-chars))
            (terpri))))
    (setf (gtk:text-buffer-text buffer) *lorem-ipsum-short*)
    (gtk:window-present window)))
      @end{pre}
      In many cases it is also convenient to first create the text buffer with
      the @fun{gtk:text-buffer-new} function, then create a text view for that
      text buffer with the @fun{gtk:text-view-new-with-buffer} function. Or you
      can change the text buffer the text view displays after the text view is
      created with the @fun{gtk:text-view-buffer} function.

      @subheading{Example of Changing Text Attributes}
      The way to affect text attributes in the @class{gtk:text-view} widget is
      to apply tags that change the attributes for a region of text. For text
      features that come from the theme - such as font and foreground color -
      use CSS to override their default values.
      @begin{pre}
(defun do-text-view-attributes (&optional application)
  (let* ((textview (make-instance 'gtk:text-view
                                  ;; Change left margin throughout the widget
                                  :left-margin 24
                                  ;; Change top margin
                                  :top-margin 12))
         (window (make-instance 'gtk:window
                                :application application
                                :child textview
                                :title \"Text View Attributes\"
                                :default-width 350
                                :default-height 200))
         (provider (gtk:css-provider-new))
         (buffer (gtk:text-view-buffer textview)))
    (setf (gtk:text-buffer-text buffer) \"Hello, this is some text.\")
    ;; Load CSS from data into the provider and apply CSS
    (gtk:css-provider-load-from-data provider
                                     \".viewstyle textview {
                                        color : Green;
                                        font : 20px Purisa; @}\")
    (gtk:widget-add-css-class window \"viewstyle\")
    (gtk:widget-add-provider window provider)
    ;; Use a tag to change the color for just one part of the text view
    (let ((tag (gtk:text-buffer-create-tag buffer
                                           \"blue_foreground\"
                                           :foreground \"blue\"))
          (start (gtk:text-buffer-iter-at-offset buffer 7))
          (end (gtk:text-buffer-iter-at-offset buffer 12)))
      ;; Apply the tag to a region of the text in the buffer
      (gtk:text-buffer-apply-tag buffer tag start end))
    ;; Show the window
    (gtk:window-present window)))
      @end{pre}
      The GTK4 demo that comes with GTK contains more example code for the
      @class{gtk:text-view} widget.
    @end{subsection}
    @begin[GtkTextIter]{subsection}
      @about-symbol{text-search-flags}
      @about-class{text-iter}
      @about-function{text-iter-new}
      @about-function{text-iter-buffer}
      @about-function{text-iter-copy}
      @about-function{text-iter-assign}
      @about-function{text-iter-offset}
      @about-function{text-iter-line}
      @about-function{text-iter-line-offset}
      @about-function{text-iter-line-index}
      @about-function{text-iter-char}
      @about-function{text-iter-slice}
      @about-function{text-iter-text}
      @about-function{text-iter-paintable}
      @about-function{text-iter-marks}
      @about-function{text-iter-toggled-tags}
      @about-function{text-iter-child-anchor}
      @about-function{text-iter-starts-tag}
      @about-function{text-iter-ends-tag}
      @about-function{text-iter-toggles-tag}
      @about-function{text-iter-has-tag}
      @about-function{text-iter-tags}
      @about-function{text-iter-editable}
      @about-function{text-iter-can-insert}
      @about-function{text-iter-starts-word}
      @about-function{text-iter-ends-word}
      @about-function{text-iter-inside-word}
      @about-function{text-iter-starts-line}
      @about-function{text-iter-ends-line}
      @about-function{text-iter-starts-sentence}
      @about-function{text-iter-ends-sentence}
      @about-function{text-iter-inside-sentence}
      @about-function{text-iter-is-cursor-position}
      @about-function{text-iter-chars-in-line}
      @about-function{text-iter-bytes-in-line}
      @about-function{text-iter-language}
      @about-function{text-iter-is-end}
      @about-function{text-iter-is-start}
      @about-function{text-iter-move}
      @about-function{text-iter-forward-to-end}
      @about-function{text-iter-forward-to-line-end}
      @about-function{text-iter-forward-to-tag-toggle}
      @about-function{text-iter-backward-to-tag-toggle}
      @about-symbol{text-char-predicate}
      @about-function{text-iter-find-char}
      @about-function{text-iter-search}
      @about-function{text-iter-equal}
      @about-function{text-iter-compare}
      @about-function{text-iter-in-range}
      @about-function{text-iter-order}
    @end{subsection}
    @begin[GtkTextTag]{subsection}
      @about-class{text-tag}
      @about-generic{text-tag-accumulative-margin}
      @about-generic{text-tag-allow-breaks}
      @about-generic{text-tag-allow-breaks-set}
      @about-generic{text-tag-background}
      @about-generic{text-tag-background-full-height}
      @about-generic{text-tag-background-full-height-set}
      @about-generic{text-tag-background-rgba}
      @about-generic{text-tag-background-set}
      @about-generic{text-tag-direction}
      @about-generic{text-tag-editable}
      @about-generic{text-tag-editable-set}
      @about-generic{text-tag-fallback}
      @about-generic{text-tag-fallback-set}
      @about-generic{text-tag-family}
      @about-generic{text-tag-family-set}
      @about-generic{text-tag-font}
      @about-generic{text-tag-font-desc}
      @about-generic{text-tag-font-features}
      @about-generic{text-tag-font-features-set}
      @about-generic{text-tag-foreground}
      @about-generic{text-tag-foreground-rgba}
      @about-generic{text-tag-foreground-set}
      @about-generic{text-tag-indent}
      @about-generic{text-tag-indent-set}
      @about-generic{text-tag-insert-hyphens}
      @about-generic{text-tag-insert-hyphens-set}
      @about-generic{text-tag-invisible}
      @about-generic{text-tag-invisible-set}
      @about-generic{text-tag-justification}
      @about-generic{text-tag-justification-set}
      @about-generic{text-tag-language}
      @about-generic{text-tag-language-set}
      @about-generic{text-tag-left-margin}
      @about-generic{text-tag-left-margin-set}
      @about-generic{text-tag-letter-spacing}
      @about-generic{text-tag-letter-spacing-set}
      @about-generic{text-tag-line-height}
      @about-generic{text-tag-line-height-set}
      @about-generic{text-tag-name}
      @about-generic{text-tag-overline}
      @about-generic{text-tag-overline-rgba}
      @about-generic{text-tag-overline-rgba-set}
      @about-generic{text-tag-overline-set}
      @about-generic{text-tag-paragraph-background}
      @about-generic{text-tag-paragraph-background-rgba}
      @about-generic{text-tag-paragraph-background-set}
      @about-generic{text-tag-pixels-above-lines}
      @about-generic{text-tag-pixels-above-lines-set}
      @about-generic{text-tag-pixels-below-lines}
      @about-generic{text-tag-pixels-below-lines-set}
      @about-generic{text-tag-pixels-inside-wrap}
      @about-generic{text-tag-pixels-inside-wrap-set}
      @about-generic{text-tag-right-margin}
      @about-generic{text-tag-right-margin-set}
      @about-generic{text-tag-rise}
      @about-generic{text-tag-rise-set}
      @about-generic{text-tag-scale}
      @about-generic{text-tag-scale-set}
      @about-generic{text-tag-sentence}
      @about-generic{text-tag-sentence-set}
      @about-generic{text-tag-show-spaces}
      @about-generic{text-tag-show-spaces-set}
      @about-generic{text-tag-size}
      @about-generic{text-tag-size-points}
      @about-generic{text-tag-size-set}
      @about-generic{text-tag-stretch}
      @about-generic{text-tag-stretch-set}
      @about-generic{text-tag-strikethrough}
      @about-generic{text-tag-strikethrough-rgba}
      @about-generic{text-tag-strikethrough-rgba-set}
      @about-generic{text-tag-strikethrough-set}
      @about-generic{text-tag-style}
      @about-generic{text-tag-style-set}
      @about-generic{text-tag-tabs}
      @about-generic{text-tag-tabs-set}
      @about-generic{text-tag-text-transform}
      @about-generic{text-tag-text-transform-set}
      @about-generic{text-tag-underline}
      @about-generic{text-tag-underline-rgba}
      @about-generic{text-tag-underline-rgba-set}
      @about-generic{text-tag-underline-set}
      @about-generic{text-tag-variant}
      @about-generic{text-tag-variant-set}
      @about-generic{text-tag-weight}
      @about-generic{text-tag-weight-set}
      @about-generic{text-tag-word}
      @about-generic{text-tag-word-set}
      @about-generic{text-tag-wrap-mode}
      @about-generic{text-tag-wrap-mode-set}
      @about-function{text-tag-new}
      @about-function{text-tag-priority}
      @about-function{text-tag-changed}
    @end{subsection}
    @begin[GtkTextTagTable]{subsection}
      @about-class{text-tag-table}
      @about-function{text-tag-table-new}
      @about-function{text-tag-table-add}
      @about-function{text-tag-table-remove}
      @about-function{text-tag-table-lookup}
      @about-symbol{text-tag-table-foreach-func}
      @about-function{text-tag-table-foreach}
      @about-function{text-tag-table-size}
    @end{subsection}
    @begin[GtkTextMark]{subsection}
      @about-class{text-mark}
      @about-generic{text-mark-left-gravity}
      @about-generic{text-mark-name}
      @about-function{text-mark-new}
      @about-function{text-mark-visible}
      @about-function{text-mark-deleted}
      @about-function{text-mark-buffer}
    @end{subsection}
    @begin[GtkTextBuffer]{subsection}
      @about-symbol{text-buffer-notify-flags}
      @about-class{text-buffer}
      @about-generic{text-buffer-can-redo}
      @about-generic{text-buffer-can-undo}
      @about-generic{text-buffer-cursor-position}
      @about-generic{text-buffer-enable-undo}
      @about-generic{text-buffer-has-selection}
      @about-generic{text-buffer-tag-table}
      @about-generic{text-buffer-text}
      @about-function{text-buffer-new}
      @about-function{text-buffer-load-file}
      @about-function{text-buffer-line-count}
      @about-function{text-buffer-char-count}
      @about-function{text-buffer-insert}
      @about-function{text-buffer-insert-at-cursor}
      @about-function{text-buffer-insert-interactive}
      @about-function{text-buffer-insert-interactive-at-cursor}
      @about-function{text-buffer-insert-range}
      @about-function{text-buffer-insert-range-interactive}
      @about-function{text-buffer-insert-with-tags}
      @about-function{text-buffer-insert-with-tags-by-name}
      @about-function{text-buffer-insert-markup}
      @about-function{text-buffer-insert-paintable}
      @about-function{text-buffer-delete}
      @about-function{text-buffer-delete-interactive}
      @about-function{text-buffer-backspace}
      @about-function{text-buffer-get-text}
      @about-function{text-buffer-get-slice}
      @about-function{text-buffer-insert-child-anchor}
      @about-function{text-buffer-create-child-anchor}
      @about-function{text-buffer-create-mark}
      @about-function{text-buffer-move-mark}
      @about-function{text-buffer-move-mark-by-name}
      @about-function{text-buffer-add-mark}
      @about-function{text-buffer-delete-mark}
      @about-function{text-buffer-delete-mark-by-name}
      @about-function{text-buffer-mark}
      @about-function{text-buffer-get-insert}
      @about-function{text-buffer-selection-bound}
      @about-function{text-buffer-place-cursor}
      @about-function{text-buffer-select-range}
      @about-function{text-buffer-apply-tag}
      @about-function{text-buffer-remove-tag}
      @about-function{text-buffer-apply-tag-by-name}
      @about-function{text-buffer-remove-tag-by-name}
      @about-function{text-buffer-remove-all-tags}
      @about-function{text-buffer-create-tag}
      @about-function{text-buffer-iter-at-line-offset}
      @about-function{text-buffer-iter-at-offset}
      @about-function{text-buffer-iter-at-line}
      @about-function{text-buffer-iter-at-line-index}
      @about-function{text-buffer-iter-at-mark}
      @about-function{text-buffer-iter-at-child-anchor}
      @about-function{text-buffer-start-iter}
      @about-function{text-buffer-end-iter}
      @about-function{text-buffer-bounds}
      @about-function{text-buffer-modified}
      @about-function{text-buffer-delete-selection}
      @about-function{text-buffer-paste-clipboard}
      @about-function{text-buffer-copy-clipboard}
      @about-function{text-buffer-cut-clipboard}
      @about-function{text-buffer-selection-bounds}
      @about-function{text-buffer-selection-content}
      @about-function{text-buffer-begin-user-action}
      @about-function{text-buffer-end-user-action}
      @about-function{text-buffer-add-selection-clipboard}
      @about-function{text-buffer-remove-selection-clipboard}
      @about-function{text-buffer-max-undo-levels}
      @about-function{text-buffer-undo}
      @about-function{text-buffer-redo}
      @about-function{text-buffer-begin-irreversible-action}
      @about-function{text-buffer-end-irreversible-action}
      @about-symbol{text-buffer-commit-notify}
      @about-function{text-buffer-add-commit-notify}
      @about-function{text-buffer-remove-commit-notify}
    @end{subsection}
    @begin[GtkChildAnchor]{subsection}
      @about-class{text-child-anchor}
      @about-function{text-child-anchor-new}
      @about-function{text-child-anchor-new-with-replacement}
      @about-function{text-child-anchor-widgets}
      @about-function{text-child-anchor-deleted}
    @end{subsection}
    @begin[GtkTextView]{subsection}
      @about-symbol{text-view-layer}
      @about-symbol{text-window-type}
      @about-symbol{text-extend-selection}
      @about-function{TEXT-VIEW-PRIORITY-VALIDATE}
      @about-class{text-view}
      @about-generic{text-view-accepts-tab}
      @about-generic{text-view-bottom-margin}
      @about-generic{text-view-buffer}
      @about-generic{text-view-cursor-visible}
      @about-generic{text-view-editable}
      @about-generic{text-view-extra-menu}
      @about-generic{text-view-im-module}
      @about-generic{text-view-indent}
      @about-generic{text-view-input-hints}
      @about-generic{text-view-input-purpose}
      @about-generic{text-view-justification}
      @about-generic{text-view-left-margin}
      @about-generic{text-view-monospace}
      @about-generic{text-view-overwrite}
      @about-generic{text-view-pixels-above-lines}
      @about-generic{text-view-pixels-below-lines}
      @about-generic{text-view-pixels-inside-wrap}
      @about-generic{text-view-populate-all}
      @about-generic{text-view-right-margin}
      @about-generic{text-view-tabs}
      @about-generic{text-view-top-margin}
      @about-generic{text-view-wrap-mode}
      @about-function{text-view-new}
      @about-function{text-view-new-with-buffer}
      @about-function{text-view-scroll-to-mark}
      @about-function{text-view-scroll-to-iter}
      @about-function{text-view-scroll-mark-onscreen}
      @about-function{text-view-move-mark-onscreen}
      @about-function{text-view-place-cursor-onscreen}
      @about-function{text-view-visible-rect}
      @about-function{text-view-iter-location}
      @about-function{text-view-cursor-locations}
      @about-function{text-view-line-at-y}
      @about-function{text-view-line-yrange}
      @about-function{text-view-iter-at-location}
      @about-function{text-view-iter-at-position}
      @about-function{text-view-buffer-to-window-coords}
      @about-function{text-view-window-to-buffer-coords}
      @about-function{text-view-move-display-line}
      @about-function{text-view-forward-display-line}
      @about-function{text-view-backward-display-line}
      @about-function{text-view-forward-display-line-end}
      @about-function{text-view-backward-display-line-start}
      @about-function{text-view-starts-display-line}
      @about-function{text-view-move-visually}
      @about-function{text-view-add-child-at-anchor}
      @about-function{text-view-remove}
      @about-function{text-view-gutter}
      @about-function{text-view-add-overlay}
      @about-function{text-view-move-overlay}
      @about-function{text-view-reset-cursor-blink}
      @about-function{text-view-im-context-filter-keypress}
      @about-function{text-view-reset-im-context}
      @about-function{text-view-ltr-context}
      @about-function{text-view-rtl-context}
    @end{subsection}
  @end{section}
  @begin[Popovers]{section}
    @begin[GtkPopover]{subsection}
      @about-class{popover}
      @about-generic{popover-autohide}
      @about-generic{popover-cascade-popdown}
      @about-generic{popover-child}
      @about-generic{popover-default-widget}
      @about-generic{popover-has-arrow}
      @about-generic{popover-mnemonics-visible}
      @about-generic{popover-pointing-to}
      @about-generic{popover-position}
      @about-function{popover-new}
      @about-function{popover-popup}
      @about-function{popover-popdown}
      @about-function{popover-present}
      @about-function{popover-offset}
    @end{subsection}
    @begin[GtkPopoverMenu]{subsection}
      @about-symbol{popover-menu-flags}
      @about-class{popover-menu}
      @about-generic{popover-menu-flags}
      @about-generic{popover-menu-menu-model}
      @about-generic{popover-menu-visible-submenu}
      @about-function{popover-menu-new-from-model}
      @about-function{popover-menu-new-from-model-full}
      @about-function{popover-menu-add-child}
      @about-function{popover-menu-remove-child}
    @end{subsection}
    @begin[GtkPopoverMenuBar]{subsection}
      @about-class{popover-menu-bar}
      @about-generic{popover-menu-bar-menu-model}
      @about-function{popover-menu-bar-new-from-model}
      @about-function{popover-menu-bar-add-child}
      @about-function{popover-menu-bar-remove-child}
    @end{subsection}
  @end{section}
  @begin[Selector Widgets and Dialogs]{section}
    @begin[GtkColorDialog]{subsection}
      @about-class{color-dialog}
      @about-generic{color-dialog-modal}
      @about-generic{color-dialog-title}
      @about-generic{color-dialog-with-alpha}
      @about-function{color-dialog-new}
      @about-function{color-dialog-choose-rgba}
      @about-function{color-dialog-choose-rgba-finish}
    @end{subsection}
    @begin[GtkColorDialogButton]{subsection}
      @about-class{color-dialog-button}
      @about-generic{color-dialog-button-dialog}
      @about-generic{color-dialog-button-rgba}
      @about-function{color-dialog-button-new}
    @end{subsection}
    @begin[GtkFileDialog]{subsection}
      @about-class{file-dialog}
      @about-generic{file-dialog-accept-label}
      @about-generic{file-dialog-default-filter}
      @about-generic{file-dialog-filters}
      @about-generic{file-dialog-initial-file}
      @about-generic{file-dialog-initial-folder}
      @about-generic{file-dialog-initial-name}
      @about-generic{file-dialog-modal}
      @about-generic{file-dialog-title}
      @about-function{file-dialog-new}
      @about-function{file-dialog-open}
      @about-function{file-dialog-open-finish}
      @about-function{file-dialog-open-multiple}
      @about-function{file-dialog-open-multiple-finish}
      @about-function{file-dialog-save}
      @about-function{file-dialog-save-finish}
      @about-function{file-dialog-select-folder}
      @about-function{file-dialog-select-folder-finish}
      @about-function{file-dialog-select-multiple-folders}
      @about-function{file-dialog-select-multiple-folders-finish}
    @end{subsection}
    @begin[GtkFileLauncher]{subsection}
      @about-class{file-launcher}
      @about-generic{file-launcher-always-ask}
      @about-generic{file-launcher-file}
      @about-generic{file-launcher-writable}
      @about-function{file-launcher-new}
      @about-function{file-launcher-launch}
      @about-function{file-launcher-launch-finish}
      @about-function{file-launcher-open-containing-folder}
      @about-function{file-launcher-open-containing-folder-finish}
    @end{subsection}
    @begin[GtkUriLauncher]{subsection}
      @about-class{uri-launcher}
      @about-generic{uri-launcher-uri}
      @about-function{uri-launcher-new}
      @about-function{uri-launcher-launch}
      @about-function{uri-launcher-launch-finish}
    @end{subsection}
    @begin[GtkFontDialog]{subsection}
      @about-class{font-dialog}
      @about-generic{font-dialog-filter}
      @about-generic{font-dialog-font-map}
      @about-generic{font-dialog-language}
      @about-generic{font-dialog-modal}
      @about-generic{font-dialog-title}
      @about-function{font-dialog-new}
      @about-function{font-dialog-choose-face}
      @about-function{font-dialog-choose-face-finish}
      @about-function{font-dialog-choose-family}
      @about-function{font-dialog-choose-family-finish}
      @about-function{font-dialog-choose-font}
      @about-function{font-dialog-choose-font-finish}
      @about-function{font-dialog-choose-font-and-features}
      @about-function{font-dialog-choose-font-and-features-finish}
    @end{subsection}
    @begin[GtkFontDialogButton]{subsection}
      @about-symbol{font-level}
      @about-class{font-dialog-button}
      @about-generic{font-dialog-button-dialog}
      @about-generic{font-dialog-button-font-desc}
      @about-generic{font-dialog-button-font-features}
      @about-generic{font-dialog-button-language}
      @about-generic{font-dialog-button-level}
      @about-generic{font-dialog-button-use-font}
      @about-generic{font-dialog-button-use-size}
      @about-function{font-dialog-button-new}
    @end{subsection}
    @begin[GtkEmojiChooser]{subsection}
      @about-class{emoji-chooser}
      @about-function{emoji-chooser-new}
    @end{subsection}
  @end{section}
  @begin[Widgets for custom drawing]{section}
    @begin[GtkDrawingArea]{subsection}
      @about-class{drawing-area}
      @about-generic{drawing-area-content-height}
      @about-generic{drawing-area-content-width}
      @about-function{drawing-area-new}
      @about-symbol{drawing-area-draw-func}
      @about-function{drawing-area-set-draw-func}
    @end{subsection}
    @begin[GtkGlArea]{subsection}
      @about-class{gl-area}
      @about-generic{gl-area-allowed-apis}
      @about-generic{gl-area-api}
      @about-generic{gl-area-auto-render}
      @about-generic{gl-area-context}
      @about-generic{gl-area-has-depth-buffer}
      @about-generic{gl-area-has-stencil-buffer}
      @about-generic{gl-area-use-es}
      @about-function{gl-area-new}
      @about-function{gl-area-make-current}
      @about-function{gl-area-queue-render}
      @about-function{gl-area-attach-buffers}
      @about-function{gl-area-error}
      @about-function{gl-area-required-version}
    @end{subsection}
  @end{section}
  @begin[Scrolling]{section}
    @begin[GtkScrollable]{subsection}
      @about-symbol{scrollable-policy}
      @about-class{scrollable}
      @about-generic{scrollable-hadjustment}
      @about-generic{scrollable-hscroll-policy}
      @about-generic{scrollable-vadjustment}
      @about-generic{scrollable-vscroll-policy}
      @about-function{scrollable-border}
    @end{subsection}
    @begin[GtkScrollbar]{subsection}
      @about-class{scrollbar}
      @about-generic{scrollbar-adjustment}
      @about-function{scrollbar-new}
    @end{subsection}
    @begin[GtkScrolledWindow]{subsection}
      @about-symbol{policy-type}
      @about-symbol{corner-type}
      @about-class{scrolled-window}
      @about-generic{scrolled-window-child}
      @about-generic{scrolled-window-hadjustment}
      @about-generic{scrolled-window-has-frame}
      @about-generic{scrolled-window-hscrollbar-policy}
      @about-generic{scrolled-window-kinetic-scrolling}
      @about-generic{scrolled-window-max-content-height}
      @about-generic{scrolled-window-max-content-width}
      @about-generic{scrolled-window-min-content-height}
      @about-generic{scrolled-window-min-content-width}
      @about-generic{scrolled-window-overlay-scrolling}
      @about-generic{scrolled-window-propagate-natural-height}
      @about-generic{scrolled-window-propagate-natural-width}
      @about-generic{scrolled-window-vadjustment}
      @about-generic{scrolled-window-vscrollbar-policy}
      @about-generic{scrolled-window-window-placement}
      @about-function{scrolled-window-new}
      @about-function{scrolled-window-hscrollbar}
      @about-function{scrolled-window-vscrollbar}
      @about-function{scrolled-window-policy}
      @about-function{scrolled-window-placement}
      @about-function{scrolled-window-unset-placement}
    @end{subsection}
    @begin[GtkViewport]{subsection}
      @about-class{viewport}
      @about-generic{viewport-child}
      @about-generic{viewport-scroll-to-focus}
      @about-function{viewport-new}
      @about-function{viewport-scroll-to}
    @end{subsection}
  @end{section}
  @begin[Keyboard shortcuts]{section}
    @begin[Utilities for accelerators]{subsection}
      We have various utility functions to parse and generate textual
      representations of keyboard accelerators. If you want to set up keyboard
      accelerators for widgets, the @class{gtk:shortcut-trigger} object is
      probably more convenient than the functions in this section.
      @about-function{accelerator-valid}
      @about-function{accelerator-parse}
      @about-function{accelerator-name}
      @about-function{accelerator-label}
      @about-function{accelerator-parse-with-keycode}
      @about-function{accelerator-name-with-keycode}
      @about-function{accelerator-label-with-keycode}
      @about-function{accelerator-default-mod-mask}
    @end{subsection}
    @begin[GtkShortcutManager]{subsection}
      @about-class{shortcut-manager}
      @about-function{shortcut-manager-add-controller}
      @about-function{shortcut-manager-remove-controller}
    @end{subsection}
    @begin[GtkShortcut]{subsection}
      @about-class{shortcut}
      @about-generic{shortcut-action}
      @about-generic{shortcut-arguments}
      @about-generic{shortcut-trigger}
      @about-function{shortcut-new}
      @about-function{shortcut-new-with-arguments}
    @end{subsection}
    @begin[GtkShortcutTrigger]{subsection}
      @about-class{shortcut-trigger}
      @about-function{shortcut-trigger-parse-string}
      @about-function{shortcut-trigger-trigger}
      @about-function{shortcut-trigger-hash}
      @about-function{shortcut-trigger-equal}
      @about-function{shortcut-trigger-compare}
      @about-function{shortcut-trigger-to-string}
      @about-function{shortcut-trigger-to-label}
      @about-class{keyval-trigger}
      @about-generic{keyval-trigger-keyval}
      @about-generic{keyval-trigger-modifiers}
      @about-function{keyval-trigger-new}
      @about-class{mnemonic-trigger}
      @about-generic{mnemonic-trigger-keyval}
      @about-function{mnemonic-trigger-new}
      @about-class{alternative-trigger}
      @about-generic{alternative-trigger-first}
      @about-generic{alternative-trigger-second}
      @about-function{alternative-trigger-new}
      @about-class{never-trigger}
      @about-function{never-trigger-get}
    @end{subsection}
    @begin[GtkShortcutAction]{subsection}
      @about-symbol{shortcut-action-flags}
      @about-class{shortcut-action}
      @about-function{shortcut-action-parse-string}
      @about-function{shortcut-action-to-string}
      @about-function{shortcut-action-activate}
      @about-class{nothing-action}
      @about-function{nothing-action-get}
      @about-class{callback-action}
      @about-symbol{shortcut-func}
      @about-function{callback-action-new}
      @about-class{mnemonic-action}
      @about-function{mnemonic-action-get}
      @about-class{activate-action}
      @about-function{activate-action-get}
      @about-class{signal-action}
      @about-generic{signal-action-signal-name}
      @about-function{signal-action-new}
      @about-class{named-action}
      @about-generic{named-action-action-name}
      @about-function{named-action-new}
    @end{subsection}
  @end{section}
  @begin[Interfaces]{section}
    @begin[GtkActionable]{subsection}
      @about-class{actionable}
      @about-generic{actionable-action-name}
      @about-generic{actionable-action-target}
      @about-function{actionable-set-action-target}
      @about-function{actionable-set-detailed-action-name}
    @end{subsection}
    @begin[GtkOrientable]{subsection}
      @about-class{orientable}
      @about-generic{orientable-orientation}
    @end{subsection}
  @end{section}
  @begin[Abstract Base Classes]{section}
    @begin[GtkWidget]{subsection}
      @about-class{cairo-context}
      @about-symbol{requested-size}
      @about-struct{requisition}
      @about-function{requisition-width}
      @about-function{requisition-height}
      @about-function{requisition-new}
      @about-function{requisition-copy}
      @about-class{widget}
      @about-generic{widget-can-focus}
      @about-generic{widget-can-target}
      @about-generic{widget-css-classes}
      @about-generic{widget-css-name}
      @about-generic{widget-cursor}
      @about-generic{widget-focus-on-click}
      @about-generic{widget-focusable}
      @about-generic{widget-halign}
      @about-generic{widget-has-default}
      @about-generic{widget-has-focus}
      @about-generic{widget-has-tooltip}
      @about-generic{widget-height-request}
      @about-generic{widget-hexpand}
      @about-generic{widget-hexpand-set}
      @about-generic{widget-layout-manager}
      @about-generic{widget-margin-bottom}
      @about-generic{widget-margin-end}
      @about-generic{widget-margin-start}
      @about-generic{widget-margin-top}
      @about-generic{widget-name}
      @about-generic{widget-opacity}
      @about-generic{widget-overflow}
      @about-generic{widget-parent}
      @about-generic{widget-receives-default}
      @about-generic{widget-root}
      @about-generic{widget-scale-factor}
      @about-generic{widget-sensitive}
      @about-generic{widget-tooltip-markup}
      @about-generic{widget-tooltip-text}
      @about-generic{widget-valign}
      @about-generic{widget-vexpand}
      @about-generic{widget-vexpand-set}
      @about-generic{widget-visible}
      @about-generic{widget-width-request}
      @about-function{widget-in-destruction}
      @about-function{widget-show}
      @about-function{widget-hide}
      @about-function{widget-map}
      @about-function{widget-unmap}
      @about-function{widget-mapped}
      @about-function{widget-realize}
      @about-function{widget-unrealize}
      @about-function{widget-realized}
      @about-function{widget-queue-draw}
      @about-function{widget-queue-resize}
      @about-function{widget-queue-allocate}
      @about-function{widget-frame-clock}
      @about-symbol{tick-callback}
      @about-function{widget-add-tick-callback}
      @about-function{widget-remove-tick-callback}
      @about-function{widget-size-allocate}
      @about-function{widget-allocate}
      @about-function{widget-class-add-shortcut}
      @about-function{widget-class-add-binding}
      @about-function{widget-class-add-binding-signal}
      @about-function{widget-class-add-binding-action}
      @about-function{widget-class-layout-manager-type}
      @about-function{widget-class-set-activate-signal-from-name}
      @about-function{widget-class-activate-signal}
      @about-function{widget-activate}
      @about-function{widget-is-focus}
      @about-function{widget-grab-focus}
      @about-function{widget-get-parent}
      @about-function{widget-set-parent}
      @about-function{widget-unparent}
      @about-function{widget-native}
      @about-function{widget-ancestor}
      @about-function{widget-is-ancestor}
      @about-function{widget-translate-coordinates}
      @about-function{widget-add-controller}
      @about-function{widget-remove-controller}
      @about-function{widget-direction}
      @about-function{widget-default-direction}
      @about-function{widget-create-pango-context}
      @about-function{widget-pango-context}
      @about-function{widget-font-options}
      @about-function{widget-font-map}
      @about-function{widget-create-pango-layout}
      @about-function{widget-set-cursor-from-name}
      @about-function{widget-class-accessible-role}
      @about-function{widget-child-focus}
      @about-function{widget-child-visible}
      @about-function{widget-settings}
      @about-function{widget-clipboard}
      @about-function{widget-primary-clipboard}
      @about-function{widget-display}
      @about-function{widget-size-request}
      @about-function{widget-set-child-visible}
      @about-function{widget-list-mnemonic-labels}
      @about-function{widget-add-mnemonic-label}
      @about-function{widget-remove-mnemonic-label}
      @about-function{widget-mnemonic-activate}
      @about-function{widget-error-bell}
      @about-function{widget-keynav-failed}
      @about-function{widget-trigger-tooltip-query}
      @about-function{widget-allocated-width}
      @about-function{widget-allocated-height}
      @about-function{widget-allocation}
      @about-function{widget-allocated-baseline}
      @about-function{widget-width}
      @about-function{widget-height}
      @about-function{widget-size}
      @about-function{widget-baseline}
      @about-function{widget-compute-bounds}
      @about-function{widget-compute-transform}
      @about-function{widget-compute-point}
      @about-function{widget-contains}
      @about-function{widget-pick}
      @about-function{widget-focus-child}
      @about-function{widget-is-sensitive}
      @about-function{widget-is-visible}
      @about-function{widget-state-flags}
      @about-function{widget-unset-state-flags}
      @about-function{widget-has-visible-focus}
      @about-function{widget-is-drawable}
      @about-function{widget-measure}
      @about-function{widget-snapshot}
      @about-function{widget-snapshot-child}
      @about-function{widget-next-sibling}
      @about-function{widget-prev-sibling}
      @about-function{widget-first-child}
      @about-function{widget-last-child}
      @about-function{widget-insert-before}
      @about-function{widget-insert-after}
      @about-function{widget-should-layout}
      @about-function{widget-color}
      @about-function{widget-add-css-class}
      @about-function{widget-remove-css-class}
      @about-function{widget-has-css-class}
      @about-function{widget-class-css-name}
      @about-function{widget-style-context}
      @about-function{widget-add-provider}
      @about-function{widget-remove-provider}
      @about-function{widget-request-mode}
      @about-function{widget-preferred-size}
      @about-function{distribute-natural-allocation}
      @about-function{widget-compute-expand}
      @about-function{widget-init-template}
      @about-function{widget-dispose-template}
      @about-function{widget-class-set-template}
      @about-function{widget-class-set-template-from-resource}
      @about-function{widget-template-child}
      @about-function{widget-class-bind-template-child}
      @about-function{widget-class-bind-template-child-internal}
      @about-function{widget-class-bind-template-child-private}
      @about-function{widget-class-bind-template-child-internal-private}
      @about-function{widget-class-bind-template-child-full}
      @about-function{widget-class-bind-template-callback}
      @about-function{widget-class-bind-template-callback-full}
      @about-function{widget-class-set-template-scope}
      @about-function{widget-observe-children}
      @about-function{widget-observe-controllers}
      @about-function{widget-insert-action-group}
      @about-function{widget-activate-action}
      @about-function{widget-activate-action-variant}
      @about-function{widget-activate-default}
      @about-symbol{widget-action-activate-func}
      @about-function{widget-class-install-action}
      @about-function{widget-class-install-property-action}
      @about-function{widget-class-query-action}
      @about-function{widget-action-set-enabled}
    @end{subsection}
    @begin[GtkRange]{subsection}
      @about-class{range}
      @about-generic{range-adjustment}
      @about-generic{range-fill-level}
      @about-generic{range-inverted}
      @about-generic{range-restrict-to-fill-level}
      @about-generic{range-round-digits}
      @about-generic{range-show-fill-level}
      @about-function{range-value}
      @about-function{range-set-increments}
      @about-function{range-set-range}
      @about-function{range-flippable}
      @about-function{range-range-rect}
      @about-function{range-slider-range}
      @about-function{range-slider-size-fixed}
    @end{subsection}
  @end{section}
  @begin[Printing]{section}
    @begin[GtkPrintOperationPreview]{subsection}
      @about-class{print-operation-preview}
      @about-function{print-operation-preview-end-preview}
      @about-function{print-operation-preview-is-selected}
      @about-function{print-operation-preview-render-page}
    @end{subsection}
    @begin[GtkPrintOperation]{subsection}
      @about-symbol{print-status}
      @about-symbol{print-operation-action}
      @about-symbol{print-operation-result}
      @about-symbol{print-error}
      @about-class{print-operation}
      @about-generic{print-operation-allow-async}
      @about-generic{print-operation-current-page}
      @about-generic{print-operation-custom-tab-label}
      @about-generic{print-operation-default-page-setup}
      @about-generic{print-operation-embed-page-setup}
      @about-generic{print-operation-export-filename}
      @about-generic{print-operation-has-selection}
      @about-generic{print-operation-job-name}
      @about-generic{print-operation-n-pages}
      @about-generic{print-operation-n-pages-to-print}
      @about-generic{print-operation-print-settings}
      @about-generic{print-operation-show-progress}
      @about-generic{print-operation-status}
      @about-generic{print-operation-status-string}
      @about-generic{print-operation-support-selection}
      @about-generic{print-operation-track-print-status}
      @about-generic{print-operation-unit}
      @about-generic{print-operation-use-full-page}
      @about-function{print-operation-new}
      @about-function{print-operation-error}
      @about-function{print-operation-run}
      @about-function{print-operation-cancel}
      @about-function{print-operation-draw-page-finish}
      @about-function{print-operation-set-defer-drawing}
      @about-function{print-operation-is-finished}
      @about-function{print-run-page-setup-dialog}
      @about-symbol{page-setup-done-func}
      @about-function{print-run-page-setup-dialog-async}
    @end{subsection}
    @begin[GtkPrintContext]{subsection}
      @about-class{print-context}
      @about-function{print-context-cairo-context}
      @about-function{print-context-set-cairo-context}
      @about-function{print-context-page-setup}
      @about-function{print-context-width}
      @about-function{print-context-height}
      @about-function{print-context-dpi-x}
      @about-function{print-context-dpi-y}
      @about-function{print-context-pango-fontmap}
      @about-function{print-context-create-pango-context}
      @about-function{print-context-create-pango-layout}
      @about-function{print-context-hard-margins}
    @end{subsection}
    @begin[GtkPaperSize]{subsection}
      @about-symbol{unit}
      @about-class{paper-size}
      @about-function{paper-size-new}
      @about-function{paper-size-new-from-ppd}
      @about-function{paper-size-new-from-ipp}
      @about-function{paper-size-new-custom}
      @about-function{paper-size-copy}
      @about-function{paper-size-free}
      @about-function{paper-size-is-equal}
      @about-function{paper-size-paper-sizes}
      @about-function{paper-size-name}
      @about-function{paper-size-display-name}
      @about-function{paper-size-ppd-name}
      @about-function{paper-size-width}
      @about-function{paper-size-height}
      @about-function{paper-size-is-ipp}
      @about-function{paper-size-is-custom}
      @about-function{paper-size-set-size}
      @about-function{paper-size-default-top-margin}
      @about-function{paper-size-default-bottom-margin}
      @about-function{paper-size-default-left-margin}
      @about-function{paper-size-default-right-margin}
      @about-function{paper-size-default}
      @about-function{paper-size-new-from-key-file}
      @about-function{paper-size-new-from-gvariant}
      @about-function{paper-size-to-key-file}
      @about-function{paper-size-to-gvariant}
    @end{subsection}
    @begin[GtkPrintSettings]{subsection}
      @about-symbol{page-orientation}
      @about-symbol{print-duplex}
      @about-symbol{print-quality}
      @about-symbol{number-up-layout}
      @about-symbol{print-pages}
      @about-symbol{page-set}
      @about-class{print-settings}
      @about-function{print-settings-new}
      @about-function{print-settings-copy}
      @about-function{print-settings-has-key}
      @about-function{print-settings-get}
      @about-function{print-settings-set}
      @about-function{print-settings-unset}
      @about-symbol{print-settings-func}
      @about-function{print-settings-foreach}
      @about-function{print-settings-bool}
      @about-function{print-settings-double}
      @about-function{print-settings-double-with-default}
      @about-function{print-settings-length}
      @about-function{print-settings-int}
      @about-function{print-settings-int-with-default}
      @about-function{print-settings-printer}
      @about-function{print-settings-orientation}
      @about-function{print-settings-paper-size}
      @about-function{print-settings-paper-width}
      @about-function{print-settings-paper-height}
      @about-function{print-settings-use-color}
      @about-function{print-settings-collate}
      @about-function{print-settings-reverse}
      @about-function{print-settings-duplex}
      @about-function{print-settings-quality}
      @about-function{print-settings-n-copies}
      @about-function{print-settings-number-up}
      @about-function{print-settings-number-up-layout}
      @about-function{print-settings-resolution}
      @about-function{print-settings-set-resolution-xy}
      @about-function{print-settings-resolution-x}
      @about-function{print-settings-resolution-y}
      @about-function{print-settings-printer-lpi}
      @about-function{print-settings-scale}
      @about-function{print-settings-print-pages}
      @about-function{print-settings-page-ranges}
      @about-function{print-settings-page-set}
      @about-function{print-settings-default-source}
      @about-function{print-settings-media-type}
      @about-function{print-settings-dither}
      @about-function{print-settings-finishings}
      @about-function{print-settings-output-bin}
      @about-function{print-settings-new-from-file}
      @about-function{print-settings-new-from-key-file}
      @about-function{print-settings-new-from-gvariant}
      @about-function{print-settings-load-file}
      @about-function{print-settings-load-key-file}
      @about-function{print-settings-to-file}
      @about-function{print-settings-to-key-file}
      @about-function{print-settings-to-gvariant}
    @end{subsection}
    @begin[GtkPageSetup]{subsection}
      @about-class{page-setup}
      @about-function{page-setup-new}
      @about-function{page-setup-new-from-file}
      @about-function{page-setup-new-from-key-file}
      @about-function{page-setup-new-from-gvariant}
      @about-function{page-setup-copy}
      @about-function{page-setup-orientation}
      @about-function{page-setup-paper-size}
      @about-function{page-setup-top-margin}
      @about-function{page-setup-bottom-margin}
      @about-function{page-setup-left-margin}
      @about-function{page-setup-right-margin}
      @about-function{page-setup-set-paper-size-and-default-margins}
      @about-function{page-setup-paper-width}
      @about-function{page-setup-paper-height}
      @about-function{page-setup-page-width}
      @about-function{page-setup-page-height}
      @about-function{page-setup-load-file}
      @about-function{page-setup-load-key-file}
      @about-function{page-setup-to-file}
      @about-function{page-setup-to-key-file}
      @about-function{page-setup-to-gvariant}
    @end{subsection}
    @begin[GtkPrinter]{subsection}
      @about-class{print-backend}
      @about-class{printer}
      @about-generic{printer-accepting-jobs}
      @about-generic{printer-accepts-pdf}
      @about-generic{printer-accepts-ps}
      @about-generic{printer-backend}
      @about-generic{printer-icon-name}
      @about-generic{printer-is-virtual}
      @about-generic{printer-job-count}
      @about-generic{printer-location}
      @about-generic{printer-name}
      @about-generic{printer-paused}
      @about-generic{printer-state-message}
      @about-function{printer-new}
      @about-function{printer-description}
      @about-function{printer-is-active}
      @about-function{printer-is-paused}
      @about-function{printer-is-accepting-jobs}
      @about-function{printer-is-default}
      @about-function{printer-list-papers}
      @about-function{printer-compare}
      @about-function{printer-has-details}
      @about-function{printer-request-details}
      @about-function{printer-capabilities}
      @about-function{printer-default-page-size}
      @about-function{printer-hard-margins}
      @about-function{printer-hard-margins-for-paper-size}
      @about-symbol{printer-func}
      @about-function{enumerate-printers}
    @end{subsection}
    @begin[GtkPrintJob]{subsection}
      @about-class{print-job}
      @about-generic{print-job-page-setup}
      @about-generic{print-job-printer}
      @about-generic{print-job-settings}
      @about-generic{print-job-title}
      @about-generic{print-job-track-print-status}
      @about-function{print-job-new}
      @about-function{print-job-status}
      @about-function{print-job-set-source-file}
      @about-function{print-job-surface}
      @about-symbol{print-job-complete-func}
      @about-function{print-job-send}
      @about-function{print-job-pages}
      @about-function{print-job-page-ranges}
      @about-function{print-job-page-set}
      @about-function{print-job-num-copies}
      @about-function{print-job-scale}
      @about-function{print-job-n-up}
      @about-function{print-job-n-up-layout}
      @about-function{print-job-rotate}
      @about-function{print-job-collate}
      @about-function{print-job-reverse}
    @end{subsection}
    @begin[GtkPrintUnixDialog]{subsection}
      @about-symbol{print-capabilities}
      @about-class{print-unix-dialog}
      @about-generic{print-unix-dialog-current-page}
      @about-generic{print-unix-dialog-embed-page-setup}
      @about-generic{print-unix-dialog-has-selection}
      @about-generic{print-unix-dialog-manual-capabilities}
      @about-generic{print-unix-dialog-page-setup}
      @about-generic{print-unix-dialog-print-settings}
      @about-generic{print-unix-dialog-selected-printer}
      @about-generic{print-unix-dialog-support-selection}
      @about-function{print-unix-dialog-new}
      @about-function{print-unix-dialog-settings}
      @about-function{print-unix-dialog-add-custom-tab}
      @about-function{print-unix-dialog-page-setup-set}
    @end{subsection}
    @begin[GtkPageSetupUnixDialog]{subsection}
      @about-class{page-setup-unix-dialog}
      @about-function{page-setup-unix-dialog-new}
      @about-function{page-setup-unix-dialog-page-setup}
      @about-function{page-setup-unix-dialog-print-settings}
    @end{subsection}
    @begin[GtkPrintSetup]{subsection}
      @about-class{print-setup}
      @about-function{print-setup-page-setup}
      @about-function{print-setup-print-settings}
    @end{subsection}
    @begin[GtkPrintDialog]{subsection}
      @about-class{print-dialog}
      @about-generic{print-dialog-accept-label}
      @about-generic{print-dialog-modal}
      @about-generic{print-dialog-page-setup}
      @about-generic{print-dialog-print-settings}
      @about-generic{print-dialog-title}
      @about-function{print-dialog-new}
      @about-function{print-dialog-print}
      @about-function{print-dialog-print-file}
      @about-function{print-dialog-print-file-finish}
      @about-function{print-dialog-print-finish}
      @about-function{print-dialog-setup}
      @about-function{print-dialog-setup-finish}
    @end{subsection}
  @end{section}
  @begin[Accessibility]{section}
    @begin[GtkAccessibleRange]{subsection}
      @about-class{accessible-range}
      @about-function{accessible-range-set-current-value}
    @end{subsection}
    @begin[GtkAccessibleText]{subsection}
      @about-symbol{accessible-text-range}
      @about-symbol{accessible-text-content-change}
      @about-symbol{accessible-text-granularity}
      @about-class{accessible-text}
      @about-function{accessible-text-attributes}
      @about-function{accessible-text-caret-position}
      @about-function{accessible-text-contents}
      @about-function{accessible-text-contents-at}
      @about-function{accessible-text-default-attributes}
      @about-function{accessible-text-extents}
      @about-function{accessible-text-offset}
      @about-function{accessible-text-selection}
      @about-function{accessible-text-update-caret-position}
      @about-function{accessible-text-update-contents}
      @about-function{accessible-text-update-selection-bound}
    @end{subsection}
    @begin[GtkAccessibleList]{subsection}
      @about-class{accessible-list}
      @about-function{accessible-list-new-from-list}
      @about-function{accessible-list-objects}
    @end{subsection}
    @begin[GtkAccessible]{subsection}
      @about-symbol{accessible-role}
      @about-symbol{accessible-state}
      @about-symbol{accessible-property}
      @about-symbol{accessible-relation}
      @about-symbol{accessible-tristate}
      @about-symbol{accessible-invalid-state}
      @about-symbol{accessible-autocomplete}
      @about-symbol{accessible-sort}
      @about-symbol{accessible-platform-state}
      @about-symbol{accessible-announcement-priority}
      @about-class{accessible}
      @about-generic{accessible-accessible-role}
      @about-function{accessible-property-init-value}
      @about-function{accessible-relation-init-value}
      @about-function{accessible-state-init-value}
      @about-function{accessible-accessible-parent}
      @about-function{accessible-at-context}
      @about-function{accessible-bounds}
      @about-function{accessible-first-accessible-child}
      @about-function{accessible-next-accessible-sibling}
      @about-function{accessible-platform-state}
      @about-function{accessible-reset-property}
      @about-function{accessible-reset-relation}
      @about-function{accessible-reset-state}
      @about-function{accessible-update-next-accessible-sibling}
      @about-function{accessible-update-property}
      @about-function{accessible-update-relation}
      @about-function{accessible-update-state}
      @about-function{accessible-announce}
    @end{subsection}
    @begin[GtkATContext]{subsection}
      @about-class{at-context}
      @about-generic{at-context-accessible}
      @about-generic{at-context-accessible-role}
      @about-generic{at-context-display}
      @about-function{at-context-create}
    @end{subsection}
  @end{section}
  @begin[Input Methods]{section}
    @begin[GtkIMContext]{subsection}
      @about-class{im-context}
      @about-generic{im-context-input-hints}
      @about-generic{im-context-input-purpose}
      @about-function{im-context-preedit-string}
      @about-function{im-context-filter-keypress}
      @about-function{im-context-filter-key}
      @about-function{im-context-focus-in}
      @about-function{im-context-focus-out}
      @about-function{im-context-reset}
      @about-function{im-context-set-client-widget}
      @about-function{im-context-set-cursor-location}
      @about-function{im-context-set-use-preedit}
      @about-function{im-context-surrounding}
      @about-function{im-context-delete-surrounding}
    @end{subsection}
    @begin[GtkIMContextSimple]{subsection}
      @about-class{im-context-simple}
      @about-symbol{GTK_MAX_COMPOSE_LEN}
      @about-function{im-context-simple-new}
      @about-function{im-context-simple-add-table}
      @about-function{im-context-simple-add-compose-file}
    @end{subsection}
    @begin[GtkIMMulticontext]{subsection}
      @about-class{im-multicontext}
      @about-function{im-multicontext-new}
      @about-function{im-multicontext-context-id}
    @end{subsection}
  @end{section}
  @begin[Recently Used Documents]{section}
    @begin[GtkRecentInfo]{subsection}
      @about-class{recent-info}
      @about-function{recent-info-ref}
      @about-function{recent-info-unref}
      @about-function{recent-info-uri}
      @about-function{recent-info-display-name}
      @about-function{recent-info-description}
      @about-function{recent-info-mime-type}
      @about-function{recent-info-added}
      @about-function{recent-info-modified}
      @about-function{recent-info-visited}
      @about-function{recent-info-private-hint}
      @about-function{recent-info-application-info}
      @about-function{recent-info-applications}
      @about-function{recent-info-last-application}
      @about-function{recent-info-has-application}
      @about-function{recent-info-create-app-info}
      @about-function{recent-info-groups}
      @about-function{recent-info-has-group}
      @about-function{recent-info-gicon}
      @about-function{recent-info-short-name}
      @about-function{recent-info-uri-display}
      @about-function{recent-info-age}
      @about-function{recent-info-is-local}
      @about-function{recent-info-exists}
      @about-function{recent-info-match}
    @end{subsection}
    @begin[GtkRecentManager]{subsection}
      @about-symbol{recent-data}
      @about-class{recent-manager}
      @about-generic{recent-manager-filename}
      @about-generic{recent-manager-size}
      @about-function{recent-manager-new}
      @about-function{recent-manager-default}
      @about-function{recent-manager-add-item}
      @about-function{recent-manager-add-full}
      @about-function{recent-manager-remove-item}
      @about-function{recent-manager-lookup-item}
      @about-function{recent-manager-has-item}
      @about-function{recent-manager-move-item}
      @about-function{recent-manager-items}
      @about-function{recent-manager-purge-items}
    @end{subsection}
  @end{section}
  @begin[Gestures and event handling]{section}
    @begin[GtkEventController]{subsection}
      @about-symbol{propagation-phase}
      @about-symbol{propagation-limit}
      @about-class{event-controller}
      @about-generic{event-controller-name}
      @about-generic{event-controller-propagation-limit}
      @about-generic{event-controller-propagation-phase}
      @about-generic{event-controller-widget}
      @about-function{event-controller-reset}
      @about-function{event-controller-current-event}
      @about-function{event-controller-current-event-device}
      @about-function{event-controller-current-event-state}
      @about-function{event-controller-current-event-time}
    @end{subsection}
    @begin[GtkEventControllerKey]{subsection}
      @about-class{event-controller-key}
      @about-function{event-controller-key-new}
      @about-function{event-controller-key-im-context}
      @about-function{event-controller-key-forward}
      @about-function{event-controller-key-group}
    @end{subsection}
    @begin[GtkEventControllerFocus]{subsection}
      @about-class{event-controller-focus}
      @about-generic{event-controller-focus-contains-focus}
      @about-generic{event-controller-focus-is-focus}
      @about-function{event-controller-focus-new}
    @end{subsection}
    @begin[GtkEventControllerLegacy]{subsection}
      @about-class{event-controller-legacy}
      @about-function{event-controller-legacy-new}
    @end{subsection}
    @begin[GtkEventControllerScroll]{subsection}
      @about-symbol{event-controller-scroll-flags}
      @about-class{event-controller-scroll}
      @about-generic{event-controller-scroll-flags}
      @about-function{event-controller-scroll-new}
      @about-function{event-controller-scroll-unit}
    @end{subsection}
    @begin[GtkEventControllerMotion]{subsection}
      @about-class{event-controller-motion}
      @about-generic{event-controller-motion-contains-pointer}
      @about-generic{event-controller-motion-is-pointer}
      @about-function{event-controller-motion-new}
    @end{subsection}
    @begin[GtkGesture]{subsection}
      @about-symbol{event-sequence-state}
      @about-class{gesture}
      @about-generic{gesture-n-points}
      @about-function{gesture-device}
      @about-function{gesture-is-active}
      @about-function{gesture-is-recognized}
      @about-function{gesture-sequence-state}
      @about-function{gesture-set-state}
      @about-function{gesture-sequences}
      @about-function{gesture-handles-sequence}
      @about-function{gesture-last-updated-sequence}
      @about-function{gesture-last-event}
      @about-function{gesture-point}
      @about-function{gesture-bounding-box}
      @about-function{gesture-bounding-box-center}
      @about-function{gesture-group}
      @about-function{gesture-ungroup}
      @about-function{gesture-get-group}
      @about-function{gesture-is-grouped-with}
    @end{subsection}
    @begin[GtkGestureSingle]{subsection}
      @about-class{gesture-single}
      @about-generic{gesture-single-button}
      @about-generic{gesture-single-exclusive}
      @about-generic{gesture-single-touch-only}
      @about-function{gesture-single-current-button}
      @about-function{gesture-single-current-sequence}
    @end{subsection}
    @begin[GtkGestureDrag]{subsection}
      @about-class{gesture-drag}
      @about-function{gesture-drag-new}
      @about-function{gesture-drag-start-point}
      @about-function{gesture-drag-offset}
    @end{subsection}
    @begin[GtkGestureLongPress]{subsection}
      @about-class{gesture-long-press}
      @about-generic{gesture-long-press-delay-factor}
      @about-function{gesture-long-press-new}
    @end{subsection}
    @begin[GtkGestureClick]{subsection}
      @about-class{gesture-click}
      @about-function{gesture-click-new}
    @end{subsection}
    @begin[GtkGesturePan]{subsection}
      @about-symbol{pan-direction}
      @about-class{gesture-pan}
      @about-generic{gesture-pan-orientation}
      @about-function{gesture-pan-new}
    @end{subsection}
    @begin[GtkGestureSwipe]{subsection}
      @about-class{gesture-swipe}
      @about-function{gesture-swipe-new}
      @about-function{gesture-swipe-velocity}
    @end{subsection}
    @begin[GtkGestureRotate]{subsection}
      @about-class{gesture-rotate}
      @about-function{gesture-rotate-new}
      @about-function{gesture-rotate-angle-delta}
    @end{subsection}
    @begin[GtkGestureZoom]{subsection}
      @about-class{gesture-zoom}
      @about-function{gesture-zoom-new}
      @about-function{gesture-zoom-scale-delta}
    @end{subsection}
    @begin[GtkGestureStylus]{subsection}
      @about-class{gesture-stylus}
      @about-generic{gesture-stylus-stylus-only}
      @about-function{gesture-stylus-new}
      @about-function{gesture-stylus-axis}
      @about-function{gesture-stylus-axes}
      @about-function{gesture-stylus-backlog}
      @about-function{gesture-stylus-device-tool}
    @end{subsection}
    @begin[GtkPadController]{subsection}
      @about-symbol{pad-action-type}
      @about-class{pad-controller}
      @about-generic{pad-controller-action-group}
      @about-generic{pad-controller-pad}
      @about-function{pad-controller-new}
      @about-function{pad-controller-set-action-entries}
      @about-function{pad-controller-set-action}
    @end{subsection}
    @begin[GtkShortcutController]{subsection}
      @about-symbol{shortcut-scope}
      @about-class{shortcut-controller}
      @about-generic{shortcut-controller-item-type}
      @about-generic{shortcut-controller-mnemonic-modifiers}
      @about-generic{shortcut-controller-model}
      @about-generic{shortcut-controller-n-items}
      @about-generic{shortcut-controller-scope}
      @about-function{shortcut-controller-new}
      @about-function{shortcut-controller-new-for-model}
      @about-function{shortcut-controller-add-shortcut}
      @about-function{shortcut-controller-remove-shortcut}
    @end{subsection}
  @end{section}
  @begin[Data exchange, Clipboards, Drag and Drop]{section}
    @begin[GtkDragSource]{subsection}
      @about-class{drag-source}
      @about-generic{drag-source-actions}
      @about-generic{drag-source-content}
      @about-function{drag-source-new}
      @about-function{drag-source-set-icon}
      @about-function{drag-source-drag-cancel}
      @about-function{drag-source-drag}
      @about-function{drag-check-threshold}
    @end{subsection}
    @begin[GtkDragIcon]{subsection}
      @about-class{drag-icon}
      @about-generic{drag-icon-child}
      @about-function{drag-icon-for-drag}
      @about-function{drag-icon-set-from-paintable}
      @about-function{drag-icon-create-widget-for-value}
    @end{subsection}
    @begin[GtkDropTarget]{subsection}
      @about-class{drop-target}
      @about-generic{drop-target-actions}
      @about-generic{drop-target-current-drop}
      @about-generic{drop-target-drop}
      @about-generic{drop-target-formats}
      @about-generic{drop-target-preload}
      @about-generic{drop-target-value}
      @about-function{drop-target-new}
      @about-function{drop-target-gtypes}
      @about-function{drop-target-reject}
    @end{subsection}
    @begin[GtkDropTargetAsync]{subsection}
      @about-class{drop-target-async}
      @about-generic{drop-target-async-actions}
      @about-generic{drop-target-async-formats}
      @about-function{drop-target-async-new}
      @about-function{drop-target-async-reject-drop}
    @end{subsection}
    @begin[GtkDropControllerMotion]{subsection}
      @about-class{drop-controller-motion}
      @about-generic{drop-controller-motion-contains-pointer}
      @about-generic{drop-controller-motion-drop}
      @about-generic{drop-controller-motion-is-pointer}
      @about-function{drop-controller-motion-new}
    @end{subsection}
  @end{section}
  @begin[Miscellaneous]{section}
    @begin[GtkAdjustment]{subsection}
      @about-class{adjustment}
      @about-generic{adjustment-lower}
      @about-generic{adjustment-page-increment}
      @about-generic{adjustment-page-size}
      @about-generic{adjustment-step-increment}
      @about-generic{adjustment-upper}
      @about-generic{adjustment-value}
      @about-function{adjustment-new}
      @about-function{adjustment-clamp-page}
      @about-function{adjustment-configure}
      @about-function{adjustment-minimum-increment}
    @end{subsection}
    @begin[GtkSizeGroup]{subsection}
      @about-symbol{size-group-mode}
      @about-class{size-group}
      @about-generic{size-group-mode}
      @about-function{size-group-new}
      @about-function{size-group-add-widget}
      @about-function{size-group-remove-widget}
      @about-function{size-group-widgets}
    @end{subsection}
    @begin[GtkFrame]{subsection}
      @about-class{frame}
      @about-generic{frame-child}
      @about-generic{frame-label}
      @about-generic{frame-label-widget}
      @about-generic{frame-label-xalign}
      @about-function{frame-new}
      @about-function{frame-label-align}
    @end{subsection}
    @begin[GtkSeparator]{subsection}
      @about-class{separator}
      @about-function{separator-new}
    @end{subsection}
    @begin[GtkSnapshot]{subsection}
      @about-class{snapshot}
      @about-function{snapshot-new}
      @about-function{snapshot-to-node}
      @about-function{snapshot-to-paintable}
      @about-function{snapshot-free-to-node}
      @about-function{snapshot-free-to-paintable}
      @about-function{snapshot-push-opacity}
      @about-function{snapshot-push-color-matrix}
      @about-function{snapshot-push-repeat}
      @about-function{snapshot-push-clip}
      @about-function{snapshot-push-rounded-clip}
      @about-function{snapshot-push-cross-fade}
      @about-function{snapshot-push-blend}
      @about-function{snapshot-push-blur}
      @about-function{snapshot-push-shadow}
      @about-function{snapshot-push-debug}
      @about-function{snapshot-push-gl-shader}
      @about-function{snapshot-push-mask}
      @about-function{snapshot-push-fill}
      @about-function{snapshot-push-stroke}
      @about-function{snapshot-pop}
      @about-function{snapshot-gl-shader-pop-texture}
      @about-function{snapshot-save}
      @about-function{snapshot-restore}
      @about-function{snapshot-transform}
      @about-function{snapshot-transform-matrix}
      @about-function{snapshot-translate}
      @about-function{snapshot-translate-3d}
      @about-function{snapshot-rotate}
      @about-function{snapshot-rotate-3d}
      @about-function{snapshot-scale}
      @about-function{snapshot-scale-3d}
      @about-function{snapshot-perspective}
      @about-function{snapshot-append-node}
      @about-function{snapshot-append-cairo}
      @about-function{snapshot-append-color}
      @about-function{snapshot-append-layout}
      @about-function{snapshot-append-border}
      @about-function{snapshot-append-linear-gradient}
      @about-function{snapshot-append-repeating-linear-gradient}
      @about-function{snapshot-append-radial-gradient}
      @about-function{snapshot-append-repeating-radial-gradient}
      @about-function{snapshot-append-conic-gradient}
      @about-function{snapshot-append-inset-shadow}
      @about-function{snapshot-append-outset-shadow}
      @about-function{snapshot-append-texture}
      @about-function{snapshot-append-scaled-texture}
      @about-function{snapshot-append-fill}
      @about-function{snapshot-append-stroke}
    @end{subsection}
    @begin[GtkTooltip]{subsection}
      @about-class{tooltip}
      @about-function{tooltip-set-markup}
      @about-function{tooltip-set-text}
      @about-function{tooltip-set-icon}
      @about-function{tooltip-set-icon-from-icon-name}
      @about-function{tooltip-set-icon-from-gicon}
      @about-function{tooltip-set-custom}
      @about-function{tooltip-set-tip-area}
    @end{subsection}
    @begin[GtkWidgetPaintable]{subsection}
      @about-class{widget-paintable}
      @about-function{widget-paintable-new}
      @about-function{widget-paintable-widget}
    @end{subsection}
    @begin[GtkWindowControls]{subsection}
      @about-class{window-controls}
      @about-generic{window-controls-decoration-layout}
      @about-generic{window-controls-empty}
      @about-generic{window-controls-side}
      @about-generic{window-controls-use-native-controls}
      @about-function{window-controls-new}
    @end{subsection}
    @begin[GtkWindowHandle]{subsection}
      @about-class{window-handle}
      @about-generic{window-handle-child}
      @about-function{window-handle-new}
    @end{subsection}
  @end{section}
  @begin[GTK Core Reference]{section}
    @begin[Library initialization and main loop]{subsection}
      Before using GTK, you need to initialize it using the @fun{gtk:init}
      function. This connects to the windowing system, sets up the locale and
      performs other initialization tasks. The @fun{gtk:init} function exits
      the application if errors occur. To avoid this, you can use the
      @fun{gtk:init-check} function, which allows you to recover from a failed
      GTK initialization. For instance, you might start up your application in
      text mode instead.

      Like most GUI toolkits, GTK uses an event-driven programming model. When
      the application is doing nothing, GTK sits in the \"main loop\" and waits
      for input. If the user performs some action - say, a mouse click - then
      the main loop \"wakes up\" and delivers an event to GTK. GTK forwards the
      event to one or more widgets.

      When widgets receive an event, they frequently emit one or more
      \"signals\". Signals notify your program that something interesting
      happened by invoking functions you have connected to the signal with the
      @fun{g:signal-connect} function. Functions connected to a signal are
      often called \"callbacks\".

      When your callbacks are invoked, you would typically take some action -
      for example, when an Open button is clicked you might display a
      @class{gtk:file-chooser-dialog} widget. After a callback finishes, GTK
      will return to the main loop and await more user input.

      It is important to note that if you use the @class{gtk:application} class,
      the application class will take care of initializing GTK for you, as well
      as spinning the main loop.
    @end{subsection}
    @begin[Functions for library initialization]{subsection}
      @about-function{init}
      @about-function{init-check}
      @about-function{is-initialized}
      @about-function{disable-setlocale}
      @about-function{default-language}
      @about-function{locale-direction}
    @end{subsection}
    @begin[Version Information]{subsection}
      GTK provides version information, primarily useful in configure checks
      for builds that have a configure script. Applications will not typically
      use the features described here.
    @end{subsection}
    @begin[Functions for version information]{subsection}
      @about-function{major-version}
      @about-function{minor-version}
      @about-function{micro-version}
      @about-function{check-version}
      @about-function{cl-cffi-gtk-build-info}
    @end{subsection}
    @begin[GtkSettings]{subsection}
      @about-symbol{settings-value}
      @about-symbol{system-setting}
      @about-symbol{font-rendering}
      @about-class{settings}
      @about-generic{settings-gtk-alternative-button-order}
      @about-generic{settings-gtk-alternative-sort-arrows}
      @about-generic{settings-gtk-application-prefer-dark-theme}
      @about-generic{settings-gtk-cursor-aspect-ratio}
      @about-generic{settings-gtk-cursor-blink}
      @about-generic{settings-gtk-cursor-blink-time}
      @about-generic{settings-gtk-cursor-blink-timeout}
      @about-generic{settings-gtk-cursor-theme-name}
      @about-generic{settings-gtk-cursor-theme-size}
      @about-generic{settings-gtk-decoration-layout}
      @about-generic{settings-gtk-dialogs-use-header}
      @about-generic{settings-gtk-dnd-drag-threshold}
      @about-generic{settings-gtk-double-click-distance}
      @about-generic{settings-gtk-double-click-time}
      @about-generic{settings-gtk-enable-accels}
      @about-generic{settings-gtk-enable-animations}
      @about-generic{settings-gtk-enable-event-sounds}
      @about-generic{settings-gtk-enable-input-feedback-sounds}
      @about-generic{settings-gtk-enable-primary-paste}
      @about-generic{settings-gtk-entry-password-hint-timeout}
      @about-generic{settings-gtk-entry-select-on-focus}
      @about-generic{settings-gtk-error-bell}
      @about-generic{settings-gtk-font-name}
      @about-generic{settings-gtk-fontconfig-timestamp}
      @about-generic{settings-gtk-hint-font-metrics}
      @about-generic{settings-gtk-icon-theme-name}
      @about-generic{settings-gtk-im-module}
      @about-generic{settings-gtk-keynav-use-caret}
      @about-generic{settings-gtk-label-select-on-focus}
      @about-generic{settings-gtk-long-press-time}
      @about-generic{settings-gtk-overlay-scrolling}
      @about-generic{settings-gtk-primary-button-warps-slider}
      @about-generic{settings-gtk-print-backends}
      @about-generic{settings-gtk-print-preview-command}
      @about-generic{settings-gtk-recent-files-enabled}
      @about-generic{settings-gtk-recent-files-max-age}
      @about-generic{settings-gtk-shell-shows-app-menu}
      @about-generic{settings-gtk-shell-shows-desktop}
      @about-generic{settings-gtk-shell-shows-menubar}
      @about-generic{settings-gtk-show-status-shapes}
      @about-generic{settings-gtk-sound-theme-name}
      @about-generic{settings-gtk-split-cursor}
      @about-generic{settings-gtk-theme-name}
      @about-generic{settings-gtk-titlebar-double-click}
      @about-generic{settings-gtk-titlebar-middle-click}
      @about-generic{settings-gtk-titlebar-right-click}
      @about-generic{settings-gtk-xft-antialias}
      @about-generic{settings-gtk-xft-dpi}
      @about-generic{settings-gtk-xft-hinting}
      @about-generic{settings-gtk-xft-hintstyle}
      @about-generic{settings-gtk-xft-rgba}
      @about-function{settings-default}
      @about-function{settings-for-display}
      @about-function{settings-reset-property}
    @end{subsection}
    @begin[Standard Enumerations]{subsection}
      @about-symbol{align}
      @about-symbol{baseline-position}
      @about-symbol{delete-type}
      @about-symbol{direction-type}
      @about-symbol{icon-size}
      @about-symbol{response-type}
      @about-function{response-type-keyword}
      @about-symbol{sensitivity-type}
      @about-symbol{text-direction}
      @about-symbol{justification}
      @about-symbol{message-type}
      @about-symbol{movement-step}
      @about-symbol{natural-wrap-mode}
      @about-symbol{scroll-step}
      @about-symbol{orientation}
      @about-symbol{overflow}
      @about-symbol{pack-type}
      @about-symbol{position-type}
      @about-symbol{scroll-type}
      @about-symbol{selection-mode}
      @about-symbol{wrap-mode}
      @about-symbol{sort-type}
      @about-symbol{ordering}
      @about-symbol{size-request-mode}
      @about-symbol{state-flags}
      @about-symbol{border-style}
      @about-symbol{input-purpose}
      @about-symbol{input-hints}
      @about-symbol{pick-flags}
      @about-symbol{constraint-relation}
      @about-symbol{constraint-strength}
      @about-symbol{constraint-attribute}
      @about-symbol{constraint-vfl-parser-error}
      @about-symbol{symbolic-color}
    @end{subsection}
  @end{section}
  @begin[Theming in GTK]{section}
    @begin[GtkStyleProvider]{subsection}
      @about-variable{+priority-fallback+}
      @about-variable{+priority-theme+}
      @about-variable{+priority-settings+}
      @about-variable{+priority-application+}
      @about-variable{+priority-user+}
      @about-class{style-provider}
    @end{subsection}
    @begin[GtkCssLocation]{subsection}
      @about-symbol{css-location}
      @about-function{css-location-bytes}
      @about-function{css-location-chars}
      @about-function{css-location-lines}
      @about-function{css-location-line-bytes}
      @about-function{css-location-line-chars}
    @end{subsection}
    @begin[GtkCssSection]{subsection}
      @about-class{css-section}
      @about-function{css-section-new}
      @about-function{css-section-new-with-bytes}
      @about-function{css-section-to-string}
      @about-function{css-section-bytes}
      @about-function{css-section-file}
      @about-function{css-section-parent}
      @about-function{css-section-start-location}
      @about-function{css-section-end-location}
    @end{subsection}
    @begin[GtkCssProvider]{subsection}
      @about-class{css-provider}
      @about-function{css-provider-new}
      @about-function{css-provider-load-named}
      @about-function{css-provider-load-from-bytes}
      @about-function{css-provider-load-from-data}
      @about-function{css-provider-load-from-file}
      @about-function{css-provider-load-from-path}
      @about-function{css-provider-load-from-resource}
      @about-function{css-provider-load-from-string}
      @about-function{css-provider-to-string}
    @end{subsection}
    @begin[GtkIconPaintable]{subsection}
      @about-class{symbolic-paintable}
      @about-function{symbolic-paintable-snapshot-symbolic}
      @about-class{icon-paintable}
      @about-generic{icon-paintable-file}
      @about-generic{icon-paintable-icon-name}
      @about-generic{icon-paintable-is-symbolic}
      @about-function{icon-paintable-new-for-file}
    @end{subsection}
    @begin[GtkIconTheme]{subsection}
      @about-symbol{icon-lookup-flags}
      @about-symbol{GTK_ICON_THEME_ERROR}
      @about-symbol{GTK_TYPE_ICON_THEME_ERROR}
      @about-symbol{GTK_TYPE_ICON_LOOKUP_FLAGS}
      @about-symbol{icon-theme-error}
      @about-class{icon-theme}
      @about-generic{icon-theme-display}
      @about-generic{icon-theme-icon-names}
      @about-generic{icon-theme-resource-path}
      @about-generic{icon-theme-search-path}
      @about-generic{icon-theme-theme-name}
      @about-function{icon-theme-new}
      @about-function{icon-theme-for-display}
      @about-function{icon-theme-add-search-path}
      @about-function{icon-theme-add-resource-path}
      @about-function{icon-theme-has-icon}
      @about-function{icon-theme-has-gicon}
      @about-function{icon-theme-lookup-icon}
      @about-function{icon-theme-lookup-by-gicon}
      @about-function{icon-theme-icon-sizes}
    @end{subsection}
  @end{section}
  @begin[Deprecated]{section}
    @begin[Deprecated since GTK 4.10]{subsection}@end{subsection}
    @begin[GtkDialog]{subsection}
      @about-symbol{dialog-flags}
      @about-class{dialog}
      @about-generic{dialog-use-header-bar}
      @about-function{dialog-new}
      @about-function{dialog-new-with-buttons}
      @about-function{dialog-response}
      @about-function{dialog-add-button}
      @about-function{dialog-add-buttons}
      @about-function{dialog-add-action-widget}
      @about-function{dialog-set-default-response}
      @about-function{dialog-set-response-sensitive}
      @about-function{dialog-response-for-widget}
      @about-function{dialog-widget-for-response}
      @about-function{dialog-content-area}
      @about-function{dialog-header-bar}
    @end{subsection}
    @begin[GtkMessageDialog]{subsection}
      @about-symbol{buttons-type}
      @about-class{message-dialog}
      @about-generic{message-dialog-buttons}
      @about-generic{message-dialog-message-area}
      @about-generic{message-dialog-message-type}
      @about-generic{message-dialog-secondary-text}
      @about-generic{message-dialog-secondary-use-markup}
      @about-generic{message-dialog-text}
      @about-generic{message-dialog-use-markup}
      @about-function{message-dialog-new}
      @about-function{message-dialog-new-with-markup}
      @about-function{message-dialog-set-markup}
      @about-function{message-dialog-format-secondary-text}
      @about-function{message-dialog-format-secondary-markup}
    @end{subsection}
    @begin[GtkAssistant]{subsection}
      @about-symbol{assistant-page-type}
      @about-class{assistant-page}
      @about-generic{assistant-page-child}
      @about-generic{assistant-page-complete}
      @about-generic{assistant-page-page-type}
      @about-generic{assistant-page-title}
      @about-class{assistant}
      @about-generic{assistant-pages}
      @about-generic{assistant-use-header-bar}
      @about-function{assistant-new}
      @about-function{assistant-page}
      @about-function{assistant-current-page}
      @about-function{assistant-n-pages}
      @about-function{assistant-nth-page}
      @about-function{assistant-prepend-page}
      @about-function{assistant-append-page}
      @about-function{assistant-insert-page}
      @about-function{assistant-remove-page}
      @about-symbol{assistant-page-func}
      @about-function{assistant-set-forward-page-func}
      @about-function{assistant-page-type}
      @about-function{assistant-page-title}
      @about-function{assistant-page-complete}
      @about-function{assistant-add-action-widget}
      @about-function{assistant-remove-action-widget}
      @about-function{assistant-update-buttons-state}
      @about-function{assistant-commit}
      @about-function{assistant-next-page}
      @about-function{assistant-previous-page}
    @end{subsection}
    @begin[GtkInfoBar]{subsection}
      @about-class{info-bar}
      @about-generic{info-bar-message-type}
      @about-generic{info-bar-revealed}
      @about-generic{info-bar-show-close-button}
      @about-function{info-bar-new}
      @about-function{info-bar-new-with-buttons}
      @about-function{info-bar-add-action-widget}
      @about-function{info-bar-remove-action-widget}
      @about-function{info-bar-add-button}
      @about-function{info-bar-add-buttons}
      @about-function{info-bar-set-response-sensitive}
      @about-function{info-bar-set-default-response}
      @about-function{info-bar-response}
      @about-function{info-bar-add-child}
      @about-function{info-bar-remove-child}
    @end{subsection}
    @begin[GtkStatusbar]{subsection}
      @about-class{statusbar}
      @about-function{statusbar-new}
      @about-function{statusbar-context-id}
      @about-function{statusbar-push}
      @about-function{statusbar-pop}
      @about-function{statusbar-remove}
      @about-function{statusbar-remove-all}
      @about-function{statusbar-message}
    @end{subsection}
    @begin[GtkLockButton]{subsection}
      @about-class{lock-button}
      @about-generic{lock-button-permission}
      @about-generic{lock-button-text-lock}
      @about-generic{lock-button-text-unlock}
      @about-generic{lock-button-tooltip-lock}
      @about-generic{lock-button-tooltip-not-authorized}
      @about-generic{lock-button-tooltip-unlock}
      @about-function{lock-button-new}
    @end{subsection}
    @begin[GtkVolumeButton]{subsection}
      @about-class{volume-button}
      @about-generic{volume-button-use-symbolic}
      @about-function{volume-button-new}
    @end{subsection}
    @begin[GtkEntryCompletion]{subsection}
      @about-class{entry-completion}
      @about-generic{entry-completion-cell-area}
      @about-generic{entry-completion-inline-completion}
      @about-generic{entry-completion-inline-selection}
      @about-generic{entry-completion-minimum-key-length}
      @about-generic{entry-completion-model}
      @about-generic{entry-completion-popup-completion}
      @about-generic{entry-completion-popup-set-width}
      @about-generic{entry-completion-popup-single-match}
      @about-generic{entry-completion-text-column}
      @about-function{entry-completion-new}
      @about-function{entry-completion-new-with-area}
      @about-function{entry-completion-entry}
      @about-symbol{entry-completion-match-func}
      @about-function{entry-completion-set-match-func}
      @about-function{entry-completion-compute-prefix}
      @about-function{entry-completion-complete}
      @about-function{entry-completion-completion-prefix}
      @about-function{entry-completion-insert-prefix}
    @end{subsection}
    @begin[GtkTreePath]{subsection}
      @about-struct{tree-iter}
      @about-function{tree-iter-copy}
      @about-function{tree-iter-free}
      @about-class{tree-path}
      @about-function{tree-path-new}
      @about-function{tree-path-new-first}
      @about-function{tree-path-new-from-indices}
      @about-function{tree-path-new-from-string}
      @about-function{tree-path-copy}
      @about-function{tree-path-append-index}
      @about-function{tree-path-prepend-index}
      @about-function{tree-path-depth}
      @about-function{tree-path-indices}
      @about-function{tree-path-compare}
      @about-function{tree-path-next}
      @about-function{tree-path-prev}
      @about-function{tree-path-up}
      @about-function{tree-path-down}
      @about-function{tree-path-is-ancestor}
      @about-function{tree-path-is-descendant}
      @about-function{tree-path-to-string}
    @end{subsection}
    @begin[GtkTreeRowReference]{subsection}
      @about-class{tree-row-reference}
      @about-function{tree-row-reference-new}
      @about-function{tree-row-reference-copy}
      @about-function{tree-row-reference-new-proxy}
      @about-function{tree-row-reference-model}
      @about-function{tree-row-reference-path}
      @about-function{tree-row-reference-valid}
      @about-function{tree-row-reference-inserted}
      @about-function{tree-row-reference-deleted}
      @about-function{tree-row-reference-reordered}
    @end{subsection}
    @begin[GtkTreeModel]{subsection}
      @about-symbol{tree-model-flags}
      @about-class{tree-model}
      @about-function{tree-model-flags}
      @about-function{tree-model-n-columns}
      @about-function{tree-model-column-type}
      @about-function{tree-model-iter}
      @about-function{tree-model-iter-from-string}
      @about-function{tree-model-iter-first}
      @about-function{tree-model-path}
      @about-function{tree-model-value}
      @about-function{tree-model-iter-next}
      @about-function{tree-model-iter-previous}
      @about-function{tree-model-iter-children}
      @about-function{tree-model-iter-has-child}
      @about-function{tree-model-iter-n-children}
      @about-function{tree-model-iter-nth-child}
      @about-function{tree-model-iter-parent}
      @about-function{tree-model-string-from-iter}
      @about-function{tree-model-ref-node}
      @about-function{tree-model-unref-node}
      @about-function{tree-model-get}
      @about-function{tree-model-get-valist}
      @about-symbol{tree-model-foreach-func}
      @about-function{tree-model-foreach}
      @about-function{tree-model-row-changed}
      @about-function{tree-model-row-inserted}
      @about-function{tree-model-row-has-child-toggled}
      @about-function{tree-model-row-deleted}
      @about-function{tree-model-rows-reordered}
      @about-function{tree-model-rows-reordered-with-length}
    @end{subsection}
    @begin[GtkTreeSelection]{subsection}
      @about-class{tree-selection}
      @about-generic{tree-selection-mode}
      @about-symbol{tree-selection-func}
      @about-function{tree-selection-set-select-function}
      @about-function{tree-selection-get-select-function}
      @about-function{tree-selection-user-data}
      @about-function{tree-selection-tree-view}
      @about-function{tree-selection-selected}
      @about-symbol{tree-selection-foreach-func}
      @about-function{tree-selection-selected-foreach}
      @about-function{tree-selection-selected-rows}
      @about-function{tree-selection-count-selected-rows}
      @about-function{tree-selection-select-path}
      @about-function{tree-selection-unselect-path}
      @about-function{tree-selection-path-is-selected}
      @about-function{tree-selection-select-iter}
      @about-function{tree-selection-unselect-iter}
      @about-function{tree-selection-iter-is-selected}
      @about-function{tree-selection-select-all}
      @about-function{tree-selection-unselect-all}
      @about-function{tree-selection-select-range}
      @about-function{tree-selection-unselect-range}
    @end{subsection}
    @begin[GtkTreeViewColumn]{subsection}
      @about-symbol{tree-view-column-sizing}
      @about-class{tree-view-column}
      @about-generic{tree-view-column-alignment}
      @about-generic{tree-view-column-cell-area}
      @about-generic{tree-view-column-clickable}
      @about-generic{tree-view-column-expand}
      @about-generic{tree-view-column-fixed-width}
      @about-generic{tree-view-column-max-width}
      @about-generic{tree-view-column-min-width}
      @about-generic{tree-view-column-reorderable}
      @about-generic{tree-view-column-resizable}
      @about-generic{tree-view-column-sizing}
      @about-generic{tree-view-column-sort-column-id}
      @about-generic{tree-view-column-sort-indicator}
      @about-generic{tree-view-column-sort-order}
      @about-generic{tree-view-column-spacing}
      @about-generic{tree-view-column-title}
      @about-generic{tree-view-column-visible}
      @about-generic{tree-view-column-widget}
      @about-generic{tree-view-column-width}
      @about-generic{tree-view-column-x-offset}
      @about-function{tree-view-column-new}
      @about-function{tree-view-column-new-with-area}
      @about-function{tree-view-column-new-with-attributes}
      @about-function{tree-view-column-pack-start}
      @about-function{tree-view-column-pack-end}
      @about-function{tree-view-column-clear}
      @about-function{tree-view-column-add-attribute}
      @about-function{tree-view-column-set-attributes}
      @about-symbol{tree-cell-data-func}
      @about-function{tree-view-column-set-cell-data-func}
      @about-function{tree-view-column-clear-attributes}
      @about-function{tree-view-column-clicked}
      @about-function{tree-view-column-button}
      @about-function{tree-view-column-cell-set-cell-data}
      @about-function{tree-view-column-cell-size}
      @about-function{tree-view-column-cell-position}
      @about-function{tree-view-column-cell-is-visible}
      @about-function{tree-view-column-focus-cell}
      @about-function{tree-view-column-queue-resize}
      @about-function{tree-view-column-tree-view}
    @end{subsection}
    @begin[GtkTreeView]{subsection}
      @about-symbol{tree-view-drop-position}
      @about-symbol{tree-view-grid-lines}
      @about-class{tree-view}
      @about-generic{tree-view-activate-on-single-click}
      @about-generic{tree-view-enable-grid-lines}
      @about-generic{tree-view-enable-search}
      @about-generic{tree-view-enable-tree-lines}
      @about-generic{tree-view-expander-column}
      @about-generic{tree-view-fixed-height-mode}
      @about-generic{tree-view-headers-clickable}
      @about-generic{tree-view-headers-visible}
      @about-generic{tree-view-hover-expand}
      @about-generic{tree-view-hover-selection}
      @about-generic{tree-view-level-indentation}
      @about-generic{tree-view-model}
      @about-generic{tree-view-reorderable}
      @about-generic{tree-view-rubber-banding}
      @about-generic{tree-view-search-column}
      @about-generic{tree-view-show-expanders}
      @about-generic{tree-view-tooltip-column}
      @about-function{tree-view-new}
      @about-function{tree-view-new-with-model}
      @about-function{tree-view-selection}
      @about-function{tree-view-columns-autosize}
      @about-function{tree-view-append-column}
      @about-function{tree-view-remove-column}
      @about-function{tree-view-insert-column}
      @about-function{tree-view-insert-column-with-attributes}
      @about-function{tree-view-insert-column-with-data-func}
      @about-function{tree-view-n-columns}
      @about-function{tree-view-column}
      @about-function{tree-view-columns}
      @about-function{tree-view-move-column-after}
      @about-symbol{tree-view-column-drop-func}
      @about-function{tree-view-set-column-drag-function}
      @about-function{tree-view-scroll-to-point}
      @about-function{tree-view-scroll-to-cell}
      @about-function{tree-view-set-cursor}
      @about-function{tree-view-set-cursor-on-cell}
      @about-function{tree-view-get-cursor}
      @about-function{tree-view-row-activated}
      @about-function{tree-view-expand-all}
      @about-function{tree-view-collapse-all}
      @about-function{tree-view-expand-to-path}
      @about-function{tree-view-expand-row}
      @about-function{tree-view-collapse-row}
      @about-symbol{tree-view-mapping-func}
      @about-function{tree-view-map-expanded-rows}
      @about-function{tree-view-row-expanded}
      @about-function{tree-view-path-at-pos}
      @about-function{tree-view-is-blank-at-pos}
      @about-function{tree-view-cell-area}
      @about-function{tree-view-background-area}
      @about-function{tree-view-visible-rect}
      @about-function{tree-view-visible-range}
      @about-function{tree-view-convert-bin-window-to-tree-coords}
      @about-function{tree-view-convert-bin-window-to-widget-coords}
      @about-function{tree-view-convert-tree-to-bin-window-coords}
      @about-function{tree-view-convert-tree-to-widget-coords}
      @about-function{tree-view-convert-widget-to-bin-window-coords}
      @about-function{tree-view-convert-widget-to-tree-coords}
      @about-function{tree-view-enable-model-drag-dest}
      @about-function{tree-view-enable-model-drag-source}
      @about-function{tree-view-unset-rows-drag-source}
      @about-function{tree-view-unset-rows-drag-dest}
      @about-function{tree-view-set-drag-dest-row}
      @about-function{tree-view-get-drag-dest-row}
      @about-function{tree-view-get-dest-row-at-pos}
      @about-function{tree-view-create-row-drag-icon}
      @about-symbol{tree-view-search-equal-func}
      @about-function{tree-view-get-search-equal-func}
      @about-function{tree-view-set-search-equal-func}
      @about-function{tree-view-search-entry}
      @about-symbol{tree-view-row-separator-func}
      @about-function{tree-view-get-row-separator-func}
      @about-function{tree-view-set-row-separator-func}
      @about-function{tree-view-is-rubber-banding-active}
      @about-function{tree-view-grid-lines}
      @about-function{tree-view-set-tooltip-row}
      @about-function{tree-view-set-tooltip-cell}
      @about-function{tree-view-tooltip-context}
    @end{subsection}
    @begin[GtkTreeView Drag & Drop]{subsection}
      @about-class{tree-drag-source}
      @about-symbol{GtkTreeDragSourceIface}
      @about-function{tree-drag-source-drag-data-delete}
      @about-function{tree-drag-source-drag-data-get}
      @about-function{tree-drag-source-row-draggable}
      @about-class{tree-drag-dest}
      @about-symbol{GtkTreeDragDestIface}
      @about-function{tree-drag-dest-drag-data-received}
      @about-function{tree-drag-dest-row-drop-possible}
      @about-function{tree-create-row-drag-content}
      @about-function{tree-get-row-drag-data}
    @end{subsection}
    @begin[GtkCellLayout]{subsection}
      @about-class{cell-layout}
      @about-function{cell-layout-pack-start}
      @about-function{cell-layout-pack-end}
      @about-function{cell-layout-area}
      @about-function{cell-layout-cells}
      @about-function{cell-layout-reorder}
      @about-function{cell-layout-clear}
      @about-function{cell-layout-set-attributes}
      @about-function{cell-layout-add-attribute}
      @about-symbol{cell-layout-data-func}
      @about-function{cell-layout-set-cell-data-func}
      @about-function{cell-layout-clear-attributes}
    @end{subsection}
    @begin[GtkCellView]{subsection}
      @about-class{cell-view}
      @about-generic{cell-view-cell-area}
      @about-generic{cell-view-cell-area-context}
      @about-generic{cell-view-draw-sensitive}
      @about-generic{cell-view-fit-model}
      @about-generic{cell-view-model}
      @about-function{cell-view-new}
      @about-function{cell-view-new-with-context}
      @about-function{cell-view-new-with-text}
      @about-function{cell-view-new-with-markup}
      @about-function{cell-view-new-with-texture}
      @about-function{cell-view-displayed-row}
    @end{subsection}
    @begin[GtkIconView]{subsection}
      @about-symbol{icon-view-drop-position}
      @about-class{icon-view}
      @about-generic{icon-view-activate-on-single-click}
      @about-generic{icon-view-cell-area}
      @about-generic{icon-view-column-spacing}
      @about-generic{icon-view-columns}
      @about-generic{icon-view-item-orientation}
      @about-generic{icon-view-item-padding}
      @about-generic{icon-view-item-width}
      @about-generic{icon-view-margin}
      @about-generic{icon-view-markup-column}
      @about-generic{icon-view-model}
      @about-generic{icon-view-pixbuf-column}
      @about-generic{icon-view-reorderable}
      @about-generic{icon-view-row-spacing}
      @about-generic{icon-view-selection-mode}
      @about-generic{icon-view-spacing}
      @about-generic{icon-view-text-column}
      @about-generic{icon-view-tooltip-column}
      @about-function{icon-view-new}
      @about-function{icon-view-new-with-area}
      @about-function{icon-view-new-with-model}
      @about-function{icon-view-path-at-pos}
      @about-function{icon-view-item-at-pos}
      @about-function{icon-view-set-cursor}
      @about-function{icon-view-get-cursor}
      @about-symbol{icon-view-foreach-func}
      @about-function{icon-view-selected-foreach}
      @about-function{icon-view-cell-rect}
      @about-function{icon-view-select-path}
      @about-function{icon-view-unselect-path}
      @about-function{icon-view-path-is-selected}
      @about-function{icon-view-selected-items}
      @about-function{icon-view-select-all}
      @about-function{icon-view-unselect-all}
      @about-function{icon-view-item-activated}
      @about-function{icon-view-scroll-to-path}
      @about-function{icon-view-visible-range}
      @about-function{icon-view-set-tooltip-item}
      @about-function{icon-view-set-tooltip-cell}
      @about-function{icon-view-tooltip-context}
      @about-function{icon-view-item-row}
      @about-function{icon-view-item-column}
      @about-function{icon-view-enable-model-drag-source}
      @about-function{icon-view-enable-model-drag-dest}
      @about-function{icon-view-unset-model-drag-source}
      @about-function{icon-view-unset-model-drag-dest}
      @about-function{icon-view-set-drag-dest-item}
      @about-function{icon-view-get-drag-dest-item}
      @about-function{icon-view-dest-item-at-pos}
      @about-function{icon-view-create-drag-icon}
    @end{subsection}
    @begin[GtkTreeSortable]{subsection}
      @about-variable{+tree-sortable-default-sort-column-id+}
      @about-variable{+tree-sortable-unsorted-sort-column-id+}
      @about-class{tree-sortable}
      @about-symbol{GtkTreeSortableIface}
      @about-function{tree-sortable-sort-column-changed}
      @about-function{tree-sortable-sort-column-id}
      @about-symbol{tree-iter-compare-func}
      @about-function{tree-sortable-set-sort-func}
      @about-function{tree-sortable-set-default-sort-func}
      @about-function{tree-sortable-has-default-sort-func}
    @end{subsection}
    @begin[GtkTreeModelSort]{subsection}
      @about-class{tree-model-sort}
      @about-generic{tree-model-sort-model}
      @about-function{tree-model-sort-new-with-model}
      @about-function{tree-model-sort-convert-child-path-to-path}
      @about-function{tree-model-sort-convert-child-iter-to-iter}
      @about-function{tree-model-sort-convert-path-to-child-path}
      @about-function{tree-model-sort-convert-iter-to-child-iter}
      @about-function{tree-model-sort-reset-default-sort-func}
      @about-function{tree-model-sort-clear-cache}
      @about-function{tree-model-sort-iter-is-valid}
    @end{subsection}
    @begin[GtkTreeModelFilter]{subsection}
      @about-class{tree-model-filter}
      @about-generic{tree-model-filter-child-model}
      @about-generic{tree-model-filter-virtual-root}
      @about-function{tree-model-filter-new}
      @about-symbol{tree-model-filter-visible-func}
      @about-function{tree-model-filter-set-visible-func}
      @about-symbol{tree-model-filter-modify-func}
      @about-function{tree-model-filter-set-modify-func}
      @about-function{tree-model-filter-set-visible-column}
      @about-function{tree-model-filter-model}
      @about-function{tree-model-filter-convert-child-iter-to-iter}
      @about-function{tree-model-filter-convert-iter-to-child-iter}
      @about-function{tree-model-filter-convert-child-path-to-path}
      @about-function{tree-model-filter-convert-path-to-child-path}
      @about-function{tree-model-filter-refilter}
      @about-function{tree-model-filter-clear-cache}
    @end{subsection}
    @begin[GtkCellArea]{subsection}
      @about-class{cell-area}
      @about-generic{cell-area-edit-widget}
      @about-generic{cell-area-edited-cell}
      @about-generic{cell-area-focus-cell}
      @about-symbol{GTK_CELL_AREA_WARN_INVALID_CELL_PROPERTY_ID}
      @about-function{cell-area-add}
      @about-function{cell-area-remove}
      @about-function{cell-area-has-renderer}
      @about-symbol{cell-callback}
      @about-function{cell-area-foreach}
      @about-symbol{cell-alloc-callback}
      @about-function{cell-area-foreach-alloc}
      @about-function{cell-area-event}
      @about-function{cell-area-snapshot}
      @about-function{cell-area-cell-allocation}
      @about-function{cell-area-cell-at-position}
      @about-function{cell-area-create-context}
      @about-function{cell-area-copy-context}
      @about-function{cell-area-request-mode}
      @about-function{cell-area-preferred-width}
      @about-function{cell-area-preferred-height-for-width}
      @about-function{cell-area-preferred-height}
      @about-function{cell-area-preferred-width-for-height}
      @about-function{cell-area-current-path-string}
      @about-function{cell-area-apply-attributes}
      @about-function{cell-area-attribute-connect}
      @about-function{cell-area-attribute-disconnect}
      @about-function{cell-area-attribute-column}
      @about-function{cell-area-class-install-cell-property}
      @about-function{cell-area-class-find-cell-property}
      @about-function{cell-area-class-list-cell-properties}
      @about-function{cell-area-add-with-properties}
      @about-function{cell-area-cell-set}
      @about-function{cell-area-cell-get}
      @about-function{cell-area-valist}
      @about-function{cell-area-cell-property}
      @about-function{cell-area-is-activatable}
      @about-function{cell-area-activate}
      @about-function{cell-area-focus}
      @about-function{cell-area-add-focus-sibling}
      @about-function{cell-area-remove-focus-sibling}
      @about-function{cell-area-is-focus-sibling}
      @about-function{cell-area-focus-siblings}
      @about-function{cell-area-focus-from-sibling}
      @about-function{cell-area-activate-cell}
      @about-function{cell-area-stop-editing}
      @about-function{cell-area-inner-cell-area}
      @about-function{cell-area-request-renderer}
    @end{subsection}
    @begin[GtkCellAreaBox]{subsection}
      @about-class{cell-area-box}
      @about-generic{cell-area-box-spacing}
      @about-function{cell-area-box-new}
      @about-function{cell-area-box-pack-start}
      @about-function{cell-area-box-pack-end}
    @end{subsection}
    @begin[GtkCellAreaContext]{subsection}
      @about-class{cell-area-context}
      @about-generic{cell-area-context-area}
      @about-generic{cell-area-context-minimum-height}
      @about-generic{cell-area-context-minimum-width}
      @about-generic{cell-area-context-natural-height}
      @about-generic{cell-area-context-natural-width}
      @about-function{cell-area-context-allocate}
      @about-function{cell-area-context-reset}
      @about-function{cell-area-context-preferred-width}
      @about-function{cell-area-context-preferred-height}
      @about-function{cell-area-context-preferred-height-for-width}
      @about-function{cell-area-context-preferred-width-for-height}
      @about-function{cell-area-context-allocation}
      @about-function{cell-area-context-push-preferred-width}
      @about-function{cell-area-context-push-preferred-height}
    @end{subsection}
    @begin[GtkCellEditable]{subsection}
      @about-class{cell-editable}
      @about-generic{cell-editable-editing-canceled}
      @about-function{cell-editable-start-editing}
      @about-function{cell-editable-editing-done}
      @about-function{cell-editable-remove-widget}
    @end{subsection}
    @begin[GtkCellRenderer]{subsection}
      @about-symbol{cell-renderer-state}
      @about-symbol{cell-renderer-mode}
      @about-class{cell-renderer}
      @about-generic{cell-renderer-cell-background}
      @about-generic{cell-renderer-cell-background-rgba}
      @about-generic{cell-renderer-cell-background-set}
      @about-generic{cell-renderer-editing}
      @about-generic{cell-renderer-height}
      @about-generic{cell-renderer-is-expanded}
      @about-generic{cell-renderer-is-expander}
      @about-generic{cell-renderer-mode}
      @about-generic{cell-renderer-sensitive}
      @about-generic{cell-renderer-visible}
      @about-generic{cell-renderer-width}
      @about-generic{cell-renderer-xalign}
      @about-generic{cell-renderer-xpad}
      @about-generic{cell-renderer-yalign}
      @about-generic{cell-renderer-ypad}
      @about-function{cell-renderer-aligned-area}
      @about-function{cell-renderer-snapshot}
      @about-function{cell-renderer-activate}
      @about-function{cell-renderer-start-editing}
      @about-function{cell-renderer-stop-editing}
      @about-function{cell-renderer-fixed-size}
      @about-function{cell-renderer-alignment}
      @about-function{cell-renderer-padding}
      @about-function{cell-renderer-state}
      @about-function{cell-renderer-is-activatable}
      @about-function{cell-renderer-preferred-height}
      @about-function{cell-renderer-preferred-height-for-width}
      @about-function{cell-renderer-preferred-size}
      @about-function{cell-renderer-preferred-width}
      @about-function{cell-renderer-preferred-width-for-height}
      @about-function{cell-renderer-request-mode}
    @end{subsection}
    @begin[GtkCellRendererText]{subsection}
      @about-class{cell-renderer-text}
      @about-generic{cell-renderer-text-align-set}
      @about-generic{cell-renderer-text-alignment}
      @about-generic{cell-renderer-text-attributes}
      @about-generic{cell-renderer-text-background}
      @about-generic{cell-renderer-text-background-rgba}
      @about-generic{cell-renderer-text-background-set}
      @about-generic{cell-renderer-text-editable}
      @about-generic{cell-renderer-text-editable-set}
      @about-generic{cell-renderer-text-ellipsize}
      @about-generic{cell-renderer-text-ellipsize-set}
      @about-generic{cell-renderer-text-family}
      @about-generic{cell-renderer-text-family-set}
      @about-generic{cell-renderer-text-font}
      @about-generic{cell-renderer-text-font-desc}
      @about-generic{cell-renderer-text-foreground}
      @about-generic{cell-renderer-text-foreground-rgba}
      @about-generic{cell-renderer-text-foreground-set}
      @about-generic{cell-renderer-text-language}
      @about-generic{cell-renderer-text-language-set}
      @about-generic{cell-renderer-text-markup}
      @about-generic{cell-renderer-text-max-width-chars}
      @about-generic{cell-renderer-text-placeholder-text}
      @about-generic{cell-renderer-text-rise}
      @about-generic{cell-renderer-text-rise-set}
      @about-generic{cell-renderer-text-scale}
      @about-generic{cell-renderer-text-scale-set}
      @about-generic{cell-renderer-text-single-paragraph-mode}
      @about-generic{cell-renderer-text-size}
      @about-generic{cell-renderer-text-size-points}
      @about-generic{cell-renderer-text-size-set}
      @about-generic{cell-renderer-text-stretch}
      @about-generic{cell-renderer-text-stretch-set}
      @about-generic{cell-renderer-text-strikethrough}
      @about-generic{cell-renderer-text-strikethrough-set}
      @about-generic{cell-renderer-text-style}
      @about-generic{cell-renderer-text-style-set}
      @about-generic{cell-renderer-text-text}
      @about-generic{cell-renderer-text-underline}
      @about-generic{cell-renderer-text-underline-set}
      @about-generic{cell-renderer-text-variant}
      @about-generic{cell-renderer-text-variant-set}
      @about-generic{cell-renderer-text-weight}
      @about-generic{cell-renderer-text-weight-set}
      @about-generic{cell-renderer-text-width-chars}
      @about-generic{cell-renderer-text-wrap-mode}
      @about-generic{cell-renderer-text-wrap-width}
      @about-function{cell-renderer-text-new}
      @about-function{cell-renderer-text-set-fixed-height-from-font}
    @end{subsection}
    @begin[GtkCellRendererAccel]{subsection}
      @about-symbol{cell-renderer-accel-mode}
      @about-class{cell-renderer-accel}
      @about-generic{cell-renderer-accel-accel-key}
      @about-generic{cell-renderer-accel-accel-mode}
      @about-generic{cell-renderer-accel-accel-mods}
      @about-generic{cell-renderer-accel-keycode}
      @about-function{cell-renderer-accel-new}
    @end{subsection}
    @begin[GtkCellRendererCombo]{subsection}
      @about-class{cell-renderer-combo}
      @about-generic{cell-renderer-combo-has-entry}
      @about-generic{cell-renderer-combo-model}
      @about-generic{cell-renderer-combo-text-column}
      @about-function{cell-renderer-combo-new}
    @end{subsection}
    @begin[GtkCellRendererPixbuf]{subsection}
      @about-class{cell-renderer-pixbuf}
      @about-generic{cell-renderer-pixbuf-gicon}
      @about-generic{cell-renderer-pixbuf-icon-name}
      @about-generic{cell-renderer-pixbuf-icon-size}
      @about-generic{cell-renderer-pixbuf-pixbuf}
      @about-generic{cell-renderer-pixbuf-pixbuf-expander-closed}
      @about-generic{cell-renderer-pixbuf-pixbuf-expander-open}
      @about-generic{cell-renderer-pixbuf-texture}
      @about-function{cell-renderer-pixbuf-new}
    @end{subsection}
    @begin[GtkCellRendererProgress]{subsection}
      @about-class{cell-renderer-progress}
      @about-generic{cell-renderer-progress-inverted}
      @about-generic{cell-renderer-progress-pulse}
      @about-generic{cell-renderer-progress-text}
      @about-generic{cell-renderer-progress-text-xalign}
      @about-generic{cell-renderer-progress-text-yalign}
      @about-generic{cell-renderer-progress-value}
      @about-function{cell-renderer-progress-new}
    @end{subsection}
    @begin[GtkCellRendererSpin]{subsection}
      @about-class{cell-renderer-spin}
      @about-generic{cell-renderer-spin-adjustment}
      @about-generic{cell-renderer-spin-climb-rate}
      @about-generic{cell-renderer-spin-digits}
      @about-function{cell-renderer-spin-new}
    @end{subsection}
    @begin[GtkCellRendererToggle]{subsection}
      @about-class{cell-renderer-toggle}
      @about-generic{cell-renderer-toggle-activatable}
      @about-generic{cell-renderer-toggle-active}
      @about-generic{cell-renderer-toggle-inconsistent}
      @about-generic{cell-renderer-toggle-radio}
      @about-function{cell-renderer-toggle-new}
    @end{subsection}
    @begin[GtkCellRendererSpinner]{subsection}
      @about-class{cell-renderer-spinner}
      @about-generic{cell-renderer-spinner-active}
      @about-generic{cell-renderer-spinner-pulse}
      @about-generic{cell-renderer-spinner-size}
      @about-function{cell-renderer-spinner-new}
    @end{subsection}
    @begin[GtkListStore]{subsection}
      @about-class{list-store}
      @about-function{list-store-new}
      @about-function{list-store-newv}
      @about-function{list-store-set-column-types}
      @about-function{list-store-set}
      @about-function{list-store-set-valist}
      @about-function{list-store-set-value}
      @about-function{list-store-set-valuesv}
      @about-function{list-store-remove}
      @about-function{list-store-insert}
      @about-function{list-store-insert-before}
      @about-function{list-store-insert-after}
      @about-function{list-store-insert-with-values}
      @about-function{list-store-insert-with-valuesv}
      @about-function{list-store-prepend}
      @about-function{list-store-append}
      @about-function{list-store-clear}
      @about-function{list-store-iter-is-valid}
      @about-function{list-store-reorder}
      @about-function{list-store-swap}
      @about-function{list-store-move-before}
      @about-function{list-store-move-after}
    @end{subsection}
    @begin[GtkTreeStore]{subsection}
      @about-class{tree-store}
      @about-function{tree-store-new}
      @about-function{tree-store-newv}
      @about-function{tree-store-set-column-types}
      @about-function{tree-store-set-value}
      @about-function{tree-store-set}
      @about-function{tree-store-set-valist}
      @about-function{tree-store-set-valuesv}
      @about-function{tree-store-remove}
      @about-function{tree-store-insert}
      @about-function{tree-store-insert-before}
      @about-function{tree-store-insert-after}
      @about-function{tree-store-insert-with-values}
      @about-function{tree-store-insert-with-valuesv}
      @about-function{tree-store-prepend}
      @about-function{tree-store-append}
      @about-function{tree-store-is-ancestor}
      @about-function{tree-store-iter-depth}
      @about-function{tree-store-clear}
      @about-function{tree-store-iter-is-valid}
      @about-function{tree-store-reorder}
      @about-function{tree-store-swap}
      @about-function{tree-store-move-before}
      @about-function{tree-store-move-after}
    @end{subsection}
    @begin[GtkComboBox]{subsection}
      @about-class{combo-box}
      @about-generic{combo-box-active}
      @about-generic{combo-box-active-id}
      @about-generic{combo-box-button-sensitivity}
      @about-generic{combo-box-child}
      @about-generic{combo-box-entry-text-column}
      @about-generic{combo-box-has-entry}
      @about-generic{combo-box-has-frame}
      @about-generic{combo-box-id-column}
      @about-generic{combo-box-model}
      @about-generic{combo-box-popup-fixed-width}
      @about-generic{combo-box-popup-shown}
      @about-function{combo-box-new}
      @about-function{combo-box-new-with-entry}
      @about-function{combo-box-new-with-model}
      @about-function{combo-box-new-with-model-and-entry}
      @about-function{combo-box-active-iter}
      @about-function{combo-box-popup}
      @about-function{combo-box-popup-for-device}
      @about-function{combo-box-popdown}
      @about-function{combo-box-get-row-separator-func}
      @about-function{combo-box-set-row-separator-func}
    @end{subsection}
    @begin[GtkComboBoxText]{subsection}
      @about-class{combo-box-text}
      @about-function{combo-box-text-new}
      @about-function{combo-box-text-new-with-entry}
      @about-function{combo-box-text-append}
      @about-function{combo-box-text-prepend}
      @about-function{combo-box-text-insert}
      @about-function{combo-box-text-append-text}
      @about-function{combo-box-text-prepend-text}
      @about-function{combo-box-text-insert-text}
      @about-function{combo-box-text-remove}
      @about-function{combo-box-text-remove-all}
      @about-function{combo-box-text-active-text}
    @end{subsection}
    @begin[GtkAppChooser]{subsection}
      @about-class{app-chooser}
      @about-generic{app-chooser-content-type}
      @about-function{app-chooser-app-info}
      @about-function{app-chooser-refresh}
    @end{subsection}
    @begin[GtkAppChooserWidget]{subsection}
      @about-class{app-chooser-widget}
      @about-generic{app-chooser-widget-default-text}
      @about-generic{app-chooser-widget-show-all}
      @about-generic{app-chooser-widget-show-default}
      @about-generic{app-chooser-widget-show-fallback}
      @about-generic{app-chooser-widget-show-other}
      @about-generic{app-chooser-widget-show-recommended}
      @about-function{app-chooser-widget-new}
    @end{subsection}
    @begin[GtkAppChooserDialog]{subsection}
      @about-class{app-chooser-dialog}
      @about-generic{app-chooser-dialog-gfile}
      @about-generic{app-chooser-dialog-heading}
      @about-function{app-chooser-dialog-new}
      @about-function{app-chooser-dialog-new-for-content-type}
      @about-function{app-chooser-dialog-widget}
    @end{subsection}
    @begin[GtkAppChooserButton]{subsection}
      @about-class{app-chooser-button}
      @about-generic{app-chooser-button-heading}
      @about-generic{app-chooser-button-modal}
      @about-generic{app-chooser-button-show-default-item}
      @about-generic{app-chooser-button-show-dialog-item}
      @about-function{app-chooser-button-new}
      @about-function{app-chooser-button-append-custom-item}
      @about-function{app-chooser-button-append-separator}
      @about-function{app-chooser-button-set-active-custom-item}
    @end{subsection}
    @begin[GtkColorChooser]{subsection}
      @about-class{color-chooser}
      @about-generic{color-chooser-rgba}
      @about-generic{color-chooser-use-alpha}
      @about-function{color-chooser-add-palette}
      @about-function{hsv-to-rgb}
      @about-function{rgb-to-hsv}
    @end{subsection}
    @begin[GtkColorChooserWidget]{subsection}
      @about-class{color-chooser-widget}
      @about-generic{color-chooser-widget-show-editor}
      @about-function{color-chooser-widget-new}
    @end{subsection}
    @begin[GtkColorChooserDialog]{subsection}
      @about-class{color-chooser-dialog}
      @about-generic{color-chooser-dialog-show-editor}
      @about-function{color-chooser-dialog-new}
    @end{subsection}
    @begin[GtkColorButton]{subsection}
      @about-class{color-button}
      @about-generic{color-button-modal}
      @about-generic{color-button-show-editor}
      @about-generic{color-button-title}
      @about-function{color-button-new}
      @about-function{color-button-new-with-rgba}
    @end{subsection}
    @begin[GtkFileChooser]{subsection}
      @about-symbol{file-chooser-action}
      @about-class{file-chooser}
      @about-generic{file-chooser-action}
      @about-generic{file-chooser-create-folders}
      @about-generic{file-chooser-filter}
      @about-generic{file-chooser-filters}
      @about-generic{file-chooser-select-multiple}
      @about-generic{file-chooser-shortcut-folders}
      @about-function{file-chooser-current-name}
      @about-function{file-chooser-file}
      @about-function{file-chooser-namestring}
      @about-function{file-chooser-files}
      @about-function{file-chooser-current-folder}
      @about-function{file-chooser-add-filter}
      @about-function{file-chooser-remove-filter}
      @about-function{file-chooser-add-shortcut-folder}
      @about-function{file-chooser-remove-shortcut-folder}
      @about-function{file-chooser-add-choice}
      @about-function{file-chooser-remove-choice}
      @about-function{file-chooser-choice}
    @end{subsection}
    @begin[GtkFileChooserWidget]{subsection}
      @about-class{file-chooser-widget}
      @about-generic{file-chooser-widget-search-mode}
      @about-generic{file-chooser-widget-show-time}
      @about-generic{file-chooser-widget-subtitle}
      @about-function{file-chooser-widget-new}
    @end{subsection}
    @begin[GtkFileChooserDialog]{subsection}
      @about-class{file-chooser-dialog}
      @about-function{file-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFileChooserNative]{subsection}
      @about-class{file-chooser-native}
      @about-generic{file-chooser-native-accept-label}
      @about-generic{file-chooser-native-cancel-label}
      @about-function{file-chooser-native-new}
    @end{subsection}
    @begin[GtkFontChooser]{subsection}
      @about-symbol{font-chooser-level}
      @about-class{font-chooser}
      @about-generic{font-chooser-font}
      @about-generic{font-chooser-font-desc}
      @about-generic{font-chooser-font-features}
      @about-generic{font-chooser-language}
      @about-generic{font-chooser-level}
      @about-generic{font-chooser-preview-text}
      @about-generic{font-chooser-show-preview-entry}
      @about-function{font-chooser-font-family}
      @about-function{font-chooser-font-face}
      @about-function{font-chooser-font-size}
      @about-symbol{font-filter-func}
      @about-function{font-chooser-set-filter-func}
      @about-function{font-chooser-font-map}
    @end{subsection}
    @begin[GtkFontChooserWidget]{subsection}
      @about-class{font-chooser-widget}
      @about-generic{font-chooser-widget-tweak-action}
      @about-function{font-chooser-widget-new}
    @end{subsection}
    @begin[GtkFontChooserDialog]{subsection}
      @about-class{font-chooser-dialog}
      @about-function{font-chooser-dialog-new}
    @end{subsection}
    @begin[GtkFontButton]{subsection}
      @about-class{font-button}
      @about-generic{font-button-modal}
      @about-generic{font-button-title}
      @about-generic{font-button-use-font}
      @about-generic{font-button-use-size}
      @about-function{font-button-new}
      @about-function{font-button-new-with-font}
    @end{subsection}
    @begin[GtkStyleContext]{subsection}
      @about-symbol{style-context-print-flags}
      @about-class{style-context}
      @about-generic{style-context-display}
      @about-function{style-context-add-provider}
      @about-function{style-context-add-provider-for-display}
      @about-function{style-context-state}
      @about-function{style-context-color}
      @about-function{style-context-border}
      @about-function{style-context-padding}
      @about-function{style-context-margin}
      @about-function{style-context-lookup-color}
      @about-function{style-context-remove-provider}
      @about-function{style-context-remove-provider-for-display}
      @about-function{style-context-restore}
      @about-function{style-context-save}
      @about-function{style-context-add-class}
      @about-function{style-context-remove-class}
      @about-function{style-context-has-class}
      @about-function{style-context-scale}
      @about-function{style-context-to-string}
      @about-struct{border}
      @about-function{border-left}
      @about-function{border-right}
      @about-function{border-top}
      @about-function{border-bottom}
      @about-function{border-new}
      @about-function{border-copy}
      @about-function{render-activity}
      @about-function{render-arrow}
      @about-function{render-background}
      @about-function{render-check}
      @about-function{render-expander}
      @about-function{render-focus}
      @about-function{render-frame}
      @about-function{render-handle}
      @about-function{render-icon}
      @about-function{render-layout}
      @about-function{render-line}
      @about-function{render-option}
    @end{subsection}
    @begin[Deprecated since GTK 4.18]{subsection}@end{subsection}
    @begin[GtkShortcutsWindow]{subsection}
      @about-class{shortcuts-window}
      @about-generic{shortcuts-window-section-name}
      @about-generic{shortcuts-window-view-name}
      @about-function{shortcuts-window-add-section}
    @end{subsection}
    @begin[GtkShortcutsSection]{subsection}
      @about-class{shortcuts-section}
      @about-generic{shortcuts-section-max-height}
      @about-generic{shortcuts-section-section-name}
      @about-generic{shortcuts-section-title}
      @about-generic{shortcuts-section-view-name}
      @about-function{shortcuts-section-add-group}
    @end{subsection}
    @begin[GtkShortcutsGroup]{subsection}
      @about-class{shortcuts-group}
      @about-generic{shortcuts-group-accel-size-group}
      @about-generic{shortcuts-group-height}
      @about-generic{shortcuts-group-title}
      @about-generic{shortcuts-group-title-size-group}
      @about-generic{shortcuts-group-view}
      @about-function{shortcuts-group-add-shortcut}
    @end{subsection}
    @begin[GtkShortcutsShortcut]{subsection}
      @about-symbol{shortcut-type}
      @about-class{shortcuts-shortcut}
      @about-generic{shortcuts-shortcut-accel-size-group}
      @about-generic{shortcuts-shortcut-accelerator}
      @about-generic{shortcuts-shortcut-action-name}
      @about-generic{shortcuts-shortcut-direction}
      @about-generic{shortcuts-shortcut-icon}
      @about-generic{shortcuts-shortcut-icon-set}
      @about-generic{shortcuts-shortcut-shortcut-type}
      @about-generic{shortcuts-shortcut-subtitle}
      @about-generic{shortcuts-shortcut-subtitle-set}
      @about-generic{shortcuts-shortcut-title}
      @about-generic{shortcuts-shortcut-title-size-group}
    @end{subsection}
    @begin[GtkShortcutLabel]{subsection}
      @about-class{shortcut-label}
      @about-generic{shortcut-label-accelerator}
      @about-generic{shortcut-label-disabled-text}
      @about-function{shortcut-label-new}
    @end{subsection}
  @end{section}")

;;; --- End of file gtk4.package.lisp ------------------------------------------
