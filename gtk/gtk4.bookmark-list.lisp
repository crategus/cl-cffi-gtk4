;;; ----------------------------------------------------------------------------
;;; gtk4.bookmark-list.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; GtkBookmarkList
;;;
;;;     A list model for recently used files
;;;
;;; Types and Values
;;;
;;;     GtkBookmarkList
;;;
;;; Accessors
;;;
;;;     gtk_bookmark_list_get_attributes
;;;     gtk_bookmark_list_set_attributes
;;;     gtk_bookmark_list_get_filename
;;;     gtk_bookmark_list_set_io_priority
;;;     gtk_bookmark_list_get_io_priority

;;;
;;; Functions
;;;
;;;     gtk_bookmark_list_new
;;;     gtk_bookmark_list_is_loading
;;;
;;; Properties
;;;
;;;     attributes
;;;     filename
;;;     io-priority
;;;     item-type                                          Since 4.8
;;;     loading
;;;     n-items                                            Since 4,8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkBookmarkList
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBookmarkList
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkBookmarkList" bookmark-list
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_bookmark_list_get_type")
  ((attributes
    bookmark-list-attributes
    "attributes" "gchararray" t t)
   (filename
    bookmark-list-filename
    "filename" "gchararray" t t)
   (io-priority
    bookmark-list-io-priority
    "io-priority" "gint" t t)
   #+gtk-4-8
   (item-type
    %bookmark-list-item-type
    "item-type" "GType" t nil)
   (loading
    bookmark-list-loading
    "loading" "gboolean" t nil)
   #+gtk-4-8
   (n-items
    bookmark-list-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'bookmark-list 'type)
 "@version{#2023-9-7}
  @begin{short}
    The @class{gtk:bookmark-list} class is a list model that wraps
    the @class{g:bookmark-file} class.
  @end{short}
  It presents a @class{g:list-model} object and fills it asynchronously with
  the @class{g:file-info} objects returned from that function.

  The @class{g:file-info} objects in the list have some attributes in the
  recent namespace added: @code{recent::private (boolean)} and
  @code{recent:applications (stringv)}.
  @see-constructor{gtk:bookmark-list-new}
  @see-slot{gtk:bookmark-list-attributes}
  @see-slot{gtk:bookmark-list-filename}
  @see-slot{gtk:bookmark-list-io-priority}
  @see-slot{gtk:bookmark-list-item-type}
  @see-slot{gtk:bookmark-list-loading}
  @see-slot{gtk:bookmark-list-n-items}
  @see-class{g:list-model}
  @see-class{g:bookmark-file}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:bookmark-list-attributes -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'bookmark-list) t)
 "The @code{attributes} property of type @code{:string} (Read / Write) @br{}
  The attributes to query. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'bookmark-list-attributes)
      "Accessor"
      (documentation 'bookmark-list-attributes 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-attributes object) => attributes}
  @syntax{(setf (gtk:bookmark-list-attributes object) attributes)}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[attributes]{a string with the attributes}
  @begin{short}
    Accessor of the @slot[gtk:bookmark]{attributes} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  The @fun{gtk:bookmark-list-attributes} function gets the attributes queried
  on the children. The @setf{gtk:bookmark-list-attributes} function sets the
  attributes to be enumerated and starts the enumeration.

  If @arg{attributes} is @code{nil}, no attributes will be queried, but a list
  of @class{g:file-info} objects will still be created.
  @see-class{gtk:bookmark-list}
  @see-class{g:file-info}")

;;; --- gtk:bookmark-list-filename ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filename" 'bookmark-list) t)
 "The @code{filename} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  Bookmark file to load. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'bookmark-list-filename)
      "Accessor"
      (documentation 'bookmark-list-filename 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-filename object) => filename}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[filename]{a string with the filename of the @file{.xbel} file}
  @begin{short}
    Accessor of the @slot[gtk:bookmark]{filename} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  The @fun{gtk:bookmark-list-filename} function returns the filename of the
  bookmark file that this list is loading.
  @see-class{gtk:bookmark-list}")

;;; --- gtk:bookmark-list-io-priortiy ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "io-priority" 'bookmark-list) t)
 "The @code{io-priority} property of type @code{:int} (Read / Write) @br{}
  Priority used when loading. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'bookmark-list-io-priority)
      "Accessor"
      (documentation 'bookmark-list-io-priority 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-io-priority object) => priority}
  @syntax{(setf (gtk:bookmark-list-io-priority object) priority)}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[priority]{an integer with the IO priority to use}
  @begin{short}
    Accessor of the @slot[gtk:bookmark]{io-priority} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  The @fun{gtk:bookmark-list-io-priority} function gets the IO priority. The
  @setf{gtk:bookmark-list-io-priority} function sets the IO priority to use
  while loading files. The default IO priority is the @var{g:+priority-default+}
  value.
  @see-class{gtk:bookmark-list}")

;;; --- gtk:bookmark-list-item-type --------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'bookmark-list) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+gtk-4-8
(declaim (inline bookmark-list-item-type))

#+gtk-4-8
(defun bookmark-list-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'bookmark-list-item-type)
      "Accessor"
      (documentation 'bookmark-list-item-type 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-item-type object) => gtype}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:bookmark]{item-type} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:bookmark-list}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:bookmark-list-loading ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loading" 'bookmark-list) t)
 "The @code{loading} property of type @code{:boolean} (Read) @br{}
  @em{True} if files are being loaded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'bookmark-list-loading)
      "Accessor"
      (documentation 'bookmark-list-loading 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-loading object) => loading}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[loading]{@em{true} if files are being loaded}
  @begin{short}
    Accessor of the @slot[gtk:bookmark]{loading} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  @see-class{gtk:bookmark-list}")

;;; --- gtk:bookmark-list-n-items ----------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'bookmark-list) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'bookmark-list-n-items)
      "Accessor"
      (documentation 'bookmark-list-n-items 'function)
 "@version{#2023-9-7}
  @syntax{(gtk:bookmark-list-n-items object) => n-items}
  @argument[object]{a @class{gtk:bookmark-list} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:bookmark-list]{n-items} slot of the
    @class{gtk:bookmark-list} class.
  @end{short}
  @see-class{gtk:bookmark-list}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_bookmark_list_new
;;; ----------------------------------------------------------------------------

(declaim (inline bookmark-list-new))

(defun bookmark-list-new (filename attributes)
 #+liber-documentation
 "@version{#2023-9-21}
  @argument[filename]{a string with the bookmark file to load}
  @argument[attributes]{a string with the attributes to query}
  @return{The new @class{gtk:bookmark-list} object.}
  @begin{short}
    Creates a new bookmark list with the given @arg{attributes}.
  @end{short}
  @see-class{gtk:bookmark-list}"
  (make-instance 'bookmark-list
                 :filename filename
                 :attributes attributes))

(export 'bookmark-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_bookmark_list_is_loading
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bookmark_list_is_loading" bookmark-list-is-loading) :boolean
 #+liber-documentation
 "@version{#2023-9-21}
  @argument[bookmarklist]{a @class{gtk:bookmark-list} object}
  @return{@em{True} if @arg{bookmarklist} is loading.}
  @begin{short}
    Returns @em{true} if the files are currently being loaded.
  @end{short}
  Files will be added to @arg{bookmarklist} from time to time while loading is
  going on. The order in which are added is undefined and may change in between
  runs.
  @see-class{gtk:bookmark-list}"
  (object (g:object bookmark-list)))

(export 'bookmark-list-is-loading)

;;; --- End of file gtk4.bookmark-list.lisp ------------------------------------
