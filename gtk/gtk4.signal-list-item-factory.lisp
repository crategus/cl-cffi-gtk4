;;; ----------------------------------------------------------------------------
;;; gtk4.signal-list-item-factory.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
;;; GtkSignalListItemFactory
;;;
;;;     A listitem factory providing signals
;;;
;;; Types and Values
;;;
;;;     GtkSignalListItemFactory
;;;
;;; Functions
;;;
;;;     gtk_signal_list_item_factory_new
;;;
;;; Signals
;;;
;;;     bind
;;;     setup
;;;     teardown
;;;     unbind
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListItemFactory
;;;         ╰── GtkSignalListItemFactory
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSignalListItemFactory
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSignalListItemFactory" signal-list-item-factory
  (:superclass list-item-factory
   :export t
   :interfaces nil
   :type-initializer "gtk_signal_list_item_factory_get_type")
  nil)

#+liber-documentation
(setf (documentation 'signal-list-item-factory 'type)
 "@version{2025-07-16}
  @begin{short}
    The @class{gtk:signal-list-item-factory} object is a
    @class{gtk:list-item-factory} object that emits signals to manage list
    items.
  @end{short}

  Signals are emitted for every list item in the same order:
  @begin{enumerate}
    @item{The @sig[gtk:signal-list-item-factory]{setup} signal is emitted to set
      up permanent things on the list item. This usually means constructing the
      widgets used in the row and adding them to the list item.}
    @item{The @sig[gtk:signal-list-item-factory]{bind} signal is emitted to bind
      the item passed via the @slot[gtk:list-item]{item} property to the widgets
      that have been created in step 1 or to add item specific widgets. Signals
      are connected to listen to changes - both to changes in the item to update
      the widgets or to changes in the widgets to update the item. After this
      signal has been called, the list item may be shown in a list widget.}
    @item{The @sig[gtk:signal-list-item-factory]{unbind} signal is emitted to
      undo everything done in step 2. Usually this means disconnecting signal
      handlers. Once this signal has been called, the list item will no longer
      be used in a list widget.}
    @item{The @sig[gtk:signal-list-item-factory]{bind} and
      @sig[gtk:signal-list-item-factory]{unbind} signals may be emitted multiple
      times again to bind the list item for use with new items. By reusing list
      items, potentially costly setup can be avoided. However, it means code
      needs to make sure to properly clean up the list item in step 3 so that no
      information from the previous use leaks into the next use.}
    @item{The @sig[gtk:signal-list-item-factory]{teardown} signal is emitted to
      allow undoing the effects of the @sig[gtk:signal-list-item-factory]{setup}
      signal. After this signal was emitted on a list item, the list item will
      be destroyed and not be used again.}
  @end{enumerate}
  Note that during the signal emissions, changing properties on the list items
  passed will not trigger notify signals as the list item’s notifications are
  frozen. See the @fun{g:object-freeze-notify} function for details.

  For tracking changes in other properties in the list item, the
  @sig[g:object]{notify} signal is recommended. The signal can be connected in
  the @sig[gtk:signal-list-item-factory]{setup} signal and removed again during
  the @sig[gtk:signal-list-item-factory]{teardown} signal.
  @begin[Signal Details]{dictionary}
    @begin[signal-list-item-factory::bind]{signal}
      @begin{pre}
lambda (factory item)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[factory]{The @class{gtk:signal-list-item-factory} object.}
        @entry[item]{The @class{gtk:list-item} object to bind.}
      @end{simple-table}
      The signal is emitted when a new item has been set on the list item and
      should be bound for use. After this signal was emitted, the list item
      might be shown in a @class{gtk:list-view} widget or other list widget.
      The @sig[gtk:signal-list-item-factory]{unbind} signal is the opposite of
      this signal and can be used to undo everything done in this signal.
    @end{signal}
    @begin[signal-list-item-factory::setup]{signal}
      @begin{pre}
lambda (factory item)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[factory]{The @class{gtk:signal-list-item-factory} object.}
        @entry[item]{The @class{gtk:list-item} object to set up.}
      @end{simple-table}
      The signal is emitted when a new list item has been created and needs to
      be setup for use. It is the first signal emitted for every list item.
      The @sig[gtk:signal-list-item-factory]{teardown} signal is the opposite of
      this signal and can be used to undo everything done in this signal.
    @end{signal}
    @begin[signal-list-item-factory::teardown]{signal}
      @begin{pre}
lambda (factory item)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[factory]{The @class{gtk:signal-list-item-factory} object.}
        @entry[item]{The @class{gtk:list-item} object to teardown.}
      @end{simple-table}
      The signal is emitted when a list item is about to be destroyed. It is the
      last signal ever emitted for this list item. This signal is the opposite
      of the @sig[signal-list-item-factory]{setup} signal and should be used to
      undo everything done in that signal.
    @end{signal}
    @begin[signal-list-item-factory::unbind]{signal}
      @begin{pre}
lambda (factory item)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[factory]{The @class{gtk:signal-list-item-factory} object.}
        @entry[item]{The @class{gtk:list-item} object to unbind.}
      @end{simple-table}
      The signal is emitted when a list item has been removed from use in a list
      widget and its new item is about to be unset. This signal is the opposite
      of the @sig[gtk:signal-list-item-factory]{bind} signal and should be used
      to undo everything done in that signal.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:list-item-factory}")

;;; ----------------------------------------------------------------------------
;;; gtk_signal_list_item_factory_new
;;; ----------------------------------------------------------------------------

(declaim (inline signal-list-item-factory-new))

(defun signal-list-item-factory-new ()
 "@version{2025-07-16}
  @return{The new @class{gtk:signal-list-item-factory} object.}
  @begin{short}
    Creates a new @class{gtk:signal-list-item-factory} object.
  @end{short}
  You need to connect signal handlers to the
  @sig[gtk:signal-list-item-factory]{setup} and
  @sig[gtk:signal-list-item-factory]{bind} signals before you use it. See the
  @class{gtk:list-view} documentation for a complete example.
  @see-class{gtk:signal-list-item-factory}
  @see-class{gtk:list-view}"
  (make-instance 'signal-list-item-factory))

(export 'signal-list-item-factory-new)

;;; --- End of file gtk4.signal-list-item-factory.lisp -------------------------
