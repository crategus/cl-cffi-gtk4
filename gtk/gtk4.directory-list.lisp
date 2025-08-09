;;; ----------------------------------------------------------------------------
;;; gtk4.directory-list.lisp
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
;;; GtkDirectoryList
;;;
;;;     A list model for directory listings
;;;
;;; Types and Values
;;;
;;;     GtkDirectoryList
;;;
;;; Accessors
;;;
;;;     gtk_directory_list_get_attributes
;;;     gtk_directory_list_set_attributes
;;;     gtk_directory_list_get_file
;;;     gtk_directory_list_set_file
;;;     gtk_directory_list_get_io_priority
;;;     gtk_directory_list_set_io_priority
;;;     gtk_directory_list_get_monitored
;;;     gtk_directory_list_set_monitored
;;;     gtk_directory_list_get_error
;;;
;;; Functions
;;;
;;;     gtk_directory_list_new
;;;     gtk_directory_list_is_loading
;;;
;;; Properties
;;;
;;;     attributes
;;;     error
;;;     file
;;;     io-priority
;;;     item-type                                           Since 4.8
;;;     loading
;;;     monitored
;;;     n-items                                             Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkDirectoryList
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDirectoryList
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDirectoryList" directory-list
  (:superclass g:object
   :export t
   :interfaces ("GListModel")
   :type-initializer "gtk_directory_list_get_type")
  ((attributes
    directory-list-attributes
    "attributes" "gchararray" t t)
   (error
    directory-list-error
    "error" "GError" t nil)
   (file
    directory-list-file
    "file" "GFile" t t)
   (io-priority
    directory-list-io-priority
    "io-priority" "gint" t t)
   #+gtk-4-8
   (item-type
    directory-list-item-type
    "item-type" "GType" t nil)
   (loading
    directory-list-loading
    "loading" "gboolean" t nil)
   (monitored
    directory-list-monitored
    "monitored" "gboolean" t t)
   #+gtk-4-8
   (n-items
    directory-list-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'directory-list 'type)
 "@version{2025-06-15}
  @begin{short}
    The @class{gtk:directory-list} class is a list model that wraps the
    requested information from the @code{g_file_enumerate_children_async()}
    function.
  @end{short}
  It presents a @class{g:list-model} object and fills it asynchronously with
  the @class{g:file-info} objects returned from that function. Enumeration will
  start automatically when the @slot[gtk:directory-list]{file} property is set.

  While the @class{gtk:directory-list} object is being filled, the
  @slot[gtk:directory-list]{loading} property will be set to @em{true}. You can
  listen to that property if you want to show information like a
  @class{gtk:spinner} widget or a @code{\"Loading...\"} text.

  If loading fails at any point, the @slot[gtk:directory-list]{error} property
  will be set to give more indication about the failure.

  The @class{g:file-info} objects returned from a @class{gtk:directory-list}
  object have the @code{\"standard::file\"} attribute set to the @class{g:file}
  object they refer to. This way you can get the file that is referred to in the
  same way you would via the @code{g_file_enumerator_child()} function. This
  means you do not need access to the @class{gtk:directory-list} object but can
  access the @class{g:file} object directly from the @class{g:file-info} object
  when operating with a @class{gtk:list-view} widget or similar.
  @see-class{g:list-model}
  @see-constructor{gtk:directory-list-new}
  @see-slot{gtk:directory-list-attributes}
  @see-slot{gtk:directory-list-error}
  @see-slot{gtk:directory-list-file}
  @see-slot{gtk:directory-list-io-priority}
  @see-slot{gtk:directory-list-item-type}
  @see-slot{gtk:directory-list-loading}
  @see-slot{gtk:directory-list-monitored}
  @see-slot{gtk:directory-list-n-items}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:directory-list-attributes ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'directory-list) t)
 "The @code{attributes} property of type @code{:string} (Read / Write) @br{}
  The attributes to query. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-attributes)
      "Accessor"
      (documentation 'directory-list-attributes 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-attributes object) => attributes}
  @syntax{(setf (gtk:directory-list-attributes object) attributes)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[attributes]{a string for the attributes}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{attributes} slot of the
    @class{gtk:directory-list} class gets or sets the attributes to query.
  @end{short}
  Setting the attributes starts the enumeration.

  If the @arg{attributes} argument is @code{nil}, no attributes will be queried,
  but a list of @class{g:file-info} objects will still be created.
  @see-class{gtk:directory-list}
  @see-class{g:file-info}")

;;; --- gtk:directory-list-error -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "error" 'directory-list) t)
 "The @code{error} property of type @class{glib:error} (Read) @br{}
  The error encountered while loading files.")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-error)
      "Accessor"
      (documentation 'directory-list-error 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-error object) => error}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[error]{a @class{glib:error} instance for the loading error or
  @code{nil} if loading finished successfully}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{error} slot of the
    @class{gtk:directory-list} class gets the loading error, if any.
  @end{short}

  If an error occurs during the loading process, the loading process will finish
  and this property allows querying the error that happened. This error will
  persist until a file is loaded again. An error being set does not mean that
  no files were loaded, and all successfully queried files will remain in the
  list.
  @see-class{gtk:directory-list}
  @see-class{glib:error}")

;;; --- gtk:directory-list-file ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'directory-list) t)
 "The @code{file} property of type @class{g:file} (Read / Write) @br{}
  The file to query.")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-file)
      "Accessor"
      (documentation 'directory-list-file 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-file object) => file}
  @syntax{(setf (gtk:directory-list-file object) file)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[file]{a @class{g:file} object to be enumerated}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{file} slot of the
    @class{gtk:directory-list} class gets or sets the file whose children are
    currently enumerated.
  @end{short}
  Setting the file starts the enumeration. If the @arg{file} argument is
  @code{nil}, the result will be an empty list.
  @see-class{gtk:directory-list}
  @see-class{g:file}")

;;; --- gtk:directory-list-io-priortiy -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "io-priority" 'directory-list) t)
 "The @code{io-priority} property of type @code{:int} (Read / Write) @br{}
  The priority used when loading. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: @var{g:+priority-default+}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-io-priority)
      "Accessor"
      (documentation 'directory-list-io-priority 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-io-priority object) => priority}
  @syntax{(setf (gtk:directory-list-io-priority object) priority)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[priority]{an integer for the IO priority to use}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{io-priority} slot of the
    @class{gtk:directory-list} class gets or sets the IO priority to use while
    loading files.
  @end{short}

  Setting the priority while @arg{object} is loading will reprioritize the
  ongoing load as soon as possible. The default IO priority is the
  @var{g:+priority-default+} value, which is higher than the GTK redraw
  priority. If you are loading a lot of directories in parallel, lowering it to
  something like the @var{g:+priority-default-idle+} value may increase
  responsiveness.
  @see-class{gtk:directory-list}
  @see-variable{g:+priority-default+}
  @see-variable{g:+priority-default-idle+}")

;;; --- gtk:directory-list-item-type -------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'directory-list) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'directory-list-item-type)
      "Accessor"
      (documentation 'directory-list-item-type 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-item-type object) => gtype}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{item-type} slot of the
    @class{gtk:directory-list} class returns the type of items in the list
    model.
  @end{short}
  Items must be subclasses of the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:directory-list}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:directory-list-loading ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loading" 'directory-list) t)
 "The @code{loading} property of type @code{:boolean} (Read) @br{}
  @em{True} if files are being loaded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-loading)
      "Accessor"
      (documentation 'directory-list-loading 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-loading object) => loading}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[loading]{@em{true} if files are being loaded}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{loading} slot of the
    @class{gtk:directory-list} class returns whether files are being loaded.
  @end{short}
  @see-class{gtk:directory-list}")

;;; --- gtk:directory-list-monitored -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "monitored" 'directory-list) t)
 "The @code{monitored} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the directory is monitored for changed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-monitored)
      "Accessor"
      (documentation 'directory-list-monitored 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-monitored object) => monitored}
  @syntax{(setf (gtk:directory-list-monitored object) monitored)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[monitored]{@em{true} to monitor the directory for changes}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{monitored} slot of the
    @class{gtk:directory-list} class gets or sets whether the directory list is
    monitoring the directory for changes.
  @end{short}
  If monitoring is enabled, the @sig[g:list-model]{items-changed} signal will
  be emitted when the directory contents change.

  When monitoring is turned on after the initial creation of the directory list,
  the directory is reloaded to avoid missing files that appeared between the
  initial loading and when monitoring was turned on.
  @see-class{gtk:directory-list}")

;;; --- gtk:directory-list-n-items ---------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'directory-list) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'directory-list-n-items)
      "Accessor"
      (documentation 'directory-list-n-items 'function)
 "@version{2025-08-05}
  @syntax{(gtk:directory-list-n-items object) => n-items}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    The accessor for the @slot[gtk:directory-list]{n-items} slot of the
    @class{gtk:directory-list} class returns the number of items contained in
    the model.
  @end{short}
  @see-class{gtk:directory-list}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_directory_list_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_directory_list_new" %directory-list-new)
    (gobject:object directory-list :return)
  (attributes :string)
  (file gio:file-as-namestring))

(defun directory-list-new (attributes file)
 #+liber-documentation
 "@version{2025-06-15}
  @argument[attributes]{a string for the attributes to query with}
  @argument[path]{a pathname or namestring for the directory to query}
  @return{The @class{gtk:directory-list} object.}
  @begin{short}
    Creates a new directory list querying the given @arg{path} with the given
    @arg{attributes}.
  @end{short}
  @begin[Notes]{dictionary}
    The C function takes a @class{g:file} instance for the @arg{path} argument.
    In the Lisp implementation the @type{g:file-as-namestring} type specifier
    is used to convert the pathname or namestring to a @class{g:file} instance.
  @end{dictionary}
  @see-class{gtk:directory-list}
  @see-class{g:file}
  @see-type{g:file-as-namestring}"
  (%directory-list-new attributes file))

(export 'directory-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_directory_list_is_loading
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_directory_list_is_loading" directory-list-is-loading)
    :boolean
 #+liber-documentation
 "@version{2024-12-15}
  @argument[model]{a @class{gtk:directory-list} object}
  @return{@em{True} if @arg{model} is loading.}
  @begin{short}
    Returns @em{true} if the children enumeration is currently in progress.
  @end{short}
  Files will be added to @arg{model} from time to time while loading is going
  on. The order in which the files are added is undefined and may change in
  between runs.
  @see-class{gtk:directory-list}"
  (model (g:object directory-list)))

(export 'directory-list-is-loading)

;;; --- End of file gtk4.directory-list.lisp -----------------------------------
