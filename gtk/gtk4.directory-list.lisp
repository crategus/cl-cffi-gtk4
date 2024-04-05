;;; ----------------------------------------------------------------------------
;;; gtk4.directory-list.lisp
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
;;;     item-type                                          Since 4.8
;;;     loading
;;;     monitored
;;;     n-items                                            Since 4.8
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

(gobject:define-g-object-class "GtkDirectoryList" directory-list
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
    %directory-list-item-type
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
 "@version{2023-9-7}
  @begin{short}
    The @class{gtk:directory-list} class is a list model that wraps the
    requested information from the @fun{g:file-enumerate-children-async}
    function.
  @end{short}
  It presents a @class{g:list-model} object and fills it asynchronously with
  the @class{g:file-info} objects returned from that function. Enumeration will
  start automatically when a the @slot[gtk:directory-list]{file} property is
  set.

  While the @class{gtk:directory-list} object is being filled, the
  @slot[gtk:directory-list]{loading} property will be set to @em{true}. You can
  listen to that property if you want to show information like a
  @class{gtk:spinner} widget or a \"Loading...\" text.

  If loading fails at any point, the @slot[directory-list]{error} property will
  be set to give more indication about the failure.

  The @class{g:file-info} objects returned from a @class{gtk:directory-list}
  object have the \"standard::file\" attribute set to the @class{g:file} object
  they refer to. This way you can get at the file that is referred to in the
  same way you would via the @fun{g:file-enumerator-child} function. This means
  you do not need access to the @class{gtk:directory-list} object but can access
  the @class{g:file} object directly from the @class{g:file-info} object when
  operating with a @class{gtk:list-view} widget or similar.
  @see-class{g:list-model}
  @see-function{g:file-enumerate-children}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- directory-list-attributes ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'directory-list) t)
 "The @code{attributes} property of type @code{:string} (Read / Write) @br{}
  The attributes to query. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-attributes)
      "Accessor"
      (documentation 'directory-list-attributes 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-attributes object) => attributes}
  @syntax[]{(setf gtk:directory-list-attributes object) attributes)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[attributes]{a string with the attributes}
  @begin{short}
    Accessor of the @slot[gtk:directory]{attributes} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The @fun{gtk:directory-list-attributes} function gets the attributes queried
  on the children. The @setf{gtk:directory-list-attributes} function sets the
  attributes to be enumerated and starts the enumeration.

  If @arg{attributes} is @code{nil}, no attributes will be queried, but a list
  of @class{g:file-info} objects will still be created.
  @see-class{gtk:directory-list}
  @see-class{g:file-info}")

;;; --- directory-list-error ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "error" 'directory-list) t)
 "The @code{error} property of type @class{g:error} (Read) @br{}
  Error encountered while loading files.")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-error)
      "Accessor"
      (documentation 'directory-list-error 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-error object) => error}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[error]{a @class{g:error} instance with the loading error or
  @code{nil} if loading finished successfully}
  @begin{short}
    Accessor of the @slot[gtk:directory-list]{error} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The @fun{gtk:directory-list-error} function gets the loading error, if any.

  If an error occurs during the loading process, the loading process will finish
  and this property allows querying the error that happened. This error will
  persist until a file is loaded again. An error being set does not mean that
  no files were loaded, and all successfully queried files will remain in the
  list.
  @see-class{gtk:directory-list}
  @see-class{g:error}")

;;; --- directory-list-file ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'directory-list) t)
 "The @code{file} property of type @class{g:file} (Read / Write) @br{}
  File to query.")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-file)
      "Accessor"
      (documentation 'directory-list-file 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-file object) => file}
  @syntax[]{(setf gtk:directory-list-file object) file)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[file]{a @class{g:file} object to be enumerated}
  @begin{short}
    Accessor of the @slot[gtk:directory]{file} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The @fun{gtk:directory-list-file} function gets the file whose children are
  currently enumerated. The @setf{gtk:directory-list-file} function sets the
  file to be enumerated and starts the enumeration. If @arg{file} is
  @code{nil}, the result will be an empty list.
  @see-class{gtk:directory-list}
  @see-class{g:file}")

;;; --- directory-list-io-priortiy ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "io-priority" 'directory-list) t)
 "The @code{io-priority} property of type @code{:int} (Read / Write) @br{}
  Priority used when loading. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-io-priority)
      "Accessor"
      (documentation 'directory-list-io-priority 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-io-priority object) => priority}
  @syntax[]{(setf gtk:directory-list-io-priority object) priority)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[priority]{an integer with the IO priority to use}
  @begin{short}
    Accessor of the @slot[gtk:directory]{io-priority} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The @fun{gtk:directory-list-io-priority} function gets the IO priority. The
  @setf{gtk:directory-list-io-priority} function sets the IO priority to use
  while loading files.

  Setting the priority while @arg{object} is loading will reprioritize the
  ongoing load as soon as possible. The default IO priority is
  @variable{g:+priority-default+}, which is higher than the GTK redraw
  priority. If you are loading a lot of directories in parallel, lowering it to
  something like the @variable{g:+priority-default-idle+} value may increase
  responsiveness.
  @see-class{gtk:directory-list}
  @see-variable{g:+priority-default}")

;;; --- directory-list-item-type ------------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'directory-list) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+gtk-4-8
(declaim (inline directory-list-item-type))

#+gtk-4-8
(defun directory-list-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'directory-list-item-type)
      "Accessor"
      (documentation 'directory-list-item-type 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-item-type object) => gtype}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[gtype]{a @class{g:type-t} type}
  @begin{short}
    Accessor of the @slot[gtk:directory]{item-type} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:directory-list}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- directory-list-loading -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "loading" 'directory-list) t)
 "The @code{loading} property of type @code{:boolean} (Read) @br{}
  @em{True} if files are being loaded. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-loading)
      "Accessor"
      (documentation 'directory-list-loading 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-loading object) => loading}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[loading]{@em{true} if files are being loaded}
  @begin{short}
    Accessor of the @slot[gtk:directory]{loading} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  @see-class{gtk:directory-list}")

;;; --- directory-list-monitored -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "monitored" 'directory-list) t)
 "The @code{monitored} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the directory is monitored for changed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'directory-list-monitored)
      "Accessor"
      (documentation 'directory-list-monitored 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-monitored object) => monitored}
  @syntax[]{(setf gtk:directory-list-monitored object) monitored)}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[monitored]{@em{true} to monitor the directory for changes}
  @begin{short}
    Accessor of the @slot[gtk:directory]{monitored} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  The @fun{gtk:directory-list-monitored} function returns whether the directory
  list is monitoring the directory for changes. The
  @setf{gtk:directory-list-monitored} function sets whether the directory list
  will monitor the directory for changes. If monitoring is enabled, the
  @code{\"items-changed\"} signal will be emitted when the directory contents
  change.

  When monitoring is turned on after the initial creation of the directory list,
  the directory is reloaded to avoid missing files that appeared between the
  initial loading and when monitoring was turned on.
  @see-class{gtk:directory-list}")

;;; --- directory-list-n-items -------------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'directory-list) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'directory-list-n-items)
      "Accessor"
      (documentation 'directory-list-n-items 'function)
 "@version{#2023-9-7}
  @syntax[]{(gtk:directory-list-n-items object) => n-items}
  @argument[object]{a @class{gtk:directory-list} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:directory-list]{n-items} slot of the
    @class{gtk:directory-list} class.
  @end{short}
  @see-class{gtk:directory-list}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_directory_list_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline directory-list-new))

(defun directory-list-new (attributes file)
 #+liber-documentation
 "@version{#2023-9-21}
  @argument[attributes]{a string with the attributes to query with}
  @argument[file]{a @class{g:file} object to query}
  @return{A @class{gtk:directory-list} object.}
  @begin{short}
    Creates a new directory list querying the given @arg{file} with the given
    @arg{attributes}.
  @end{short}
  @see-class{gtk:directory-list}"
  (make-instance 'directory-list
                 :attributes attributes
                 :file file))

(export 'directory-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_directory_list_is_loading ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_directory_list_is_loading" directory-list-is-loading)
    :boolean
 #+liber-documentation
 "@version{#2023-9-21}
  @argument[directorylist]{a @class{gtk:directory-list} object}
  @return{@em{True} if @arg{directorylist} is loading.}
  @begin{short}
    Returns @em{true} if the children enumeration is currently in progress.
  @end{short}
  Files will be added to @arg{directorylist} from time to time while loading is
  going on. The order in which are added is undefined and may change in between
  runs.
  @see-class{gtk:directory-list}"
  (model (g:object directory-list)))

(export 'directory-list-is-loading)

;;; --- End of file gtk4.directory-list.lisp -----------------------------------
