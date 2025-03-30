;;; ----------------------------------------------------------------------------
;;; gtk4.list-item-factory.lisp
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
;;; GtkListItemFactory
;;;
;;;     Mapping list items to widgets
;;;
;;; Types and Values
;;;
;;;     GtkListItemFactory
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListItemFactory
;;;         ├── GtkBuilderListItemFactory
;;;         ╰── GtkSignalListItemFactory
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListItemFactory
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListItemFactory" list-item-factory
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_list_item_factory_get_type")
  nil)

#+liber-documentation
(setf (documentation 'list-item-factory 'type)
 "@version{2025-3-16}
  @begin{short}
    The @class{gtk:list-item-factory} object creates widgets for the items
    taken from a @class{g:list-model} object.
  @end{short}
  It is one of the core concepts of handling list widgets such as the
  @class{gtk:list-view} or @class{gtk:grid-view} widgets.

  The @class{gtk:list-item-factory} object is tasked with creating widgets for
  items taken from the model when the views need them and updating them as the
  items displayed by the view change.

  A view is usually only able to display anything after both a factory and a
  model have been set on the view. So it is important that you do not skip this
  step when setting up your first view.

  Because views do not display the whole list at once but only a few items,
  they only need to maintain a few widgets at a time. They will instruct the
  @class{gtk:list-item-factory} object to create these widgets and bind them to
  the items that are currently displayed. As the list model changes or the user
  scrolls to the list, the items will change and the view will instruct the
  factory to bind the widgets to those new items.

  The actual widgets used for displaying those widgets is provided by you.

  When the factory needs widgets created, it will create a @class{gtk:list-item}
  and hand it to your code to set up a widget for. This list item will provide
  various properties with information about what item to display and provide
  you with some opportunities to configure its behavior. See the
  @class{gtk:list-item} documentation for further details.

  Various implementations of the @class{gtk:list-item-factory} object exist to
  allow you different ways to provide those widgets. The most common
  implementations are the @class{gtk:builder-list-item-factory} object which
  takes a @class{gtk:builder} UI file and then creates widgets and manages
  everything automatically from the information in that file and the
  @class{gtk:signal-list-item-factory} object which allows you to connect to
  signals with your own code and retain full control over how the widgets are
  setup and managed.

  The @class{gtk:list-item-factory} object is supposed to be final - that means
  its behavior should not change and the first widget created from it should
  behave the same way as the last widget created from it. If you intend to do
  changes to the behavior, it is recommended that you create a new
  @class{gtk:list-item-factory} object which will allow the views to recreate
  its widgets.

  Once you have chosen your factory and created it, you need to set it on the
  view widget you want to use it with, such as via the
  @fun{gtk:list-view-factory} function. Reusing factories across different
  views is allowed, but very uncommon.
  @see-class{gtk:builder-list-item-factory}
  @see-class{gtk:signal-list-item-factory}
  @see-class{g:list-model}
  @see-class{gtk:list-view}
  @see-class{gtk:grid-view}")

;;; --- End of file gtk4.list-item-factory.lisp --------------------------------
