;;; ----------------------------------------------------------------------------
;;; gtk4.directory-list.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; Functions
;;;
;;;     gtk_directory_list_new
;;;     gtk_directory_list_get_attributes
;;;     gtk_directory_list_set_attributes
;;;     gtk_directory_list_get_file
;;;     gtk_directory_list_set_file
;;;     gtk_directory_list_get_io_priority
;;;     gtk_directory_list_set_io_priority
;;;     gtk_directory_list_get_monitored
;;;     gtk_directory_list_set_monitored
;;;     gtk_directory_list_is_loading
;;;     gtk_directory_list_get_error
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



;;;Property Details
;;;The “attributes” property
;;;  “attributes”               char *
;;;The attributes to query

;;;Owner: GtkDirectoryList

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “error” property
;;;  “error”                    GError *
;;;Error encountered while loading files

;;;Owner: GtkDirectoryList

;;;Flags: Read

;;;The “file” property
;;;  “file”                     GFile *
;;;File to query

;;;Owner: GtkDirectoryList

;;;Flags: Read / Write

;;;The “io-priority” property
;;;  “io-priority”              int
;;;Priority used when loading

;;;Owner: GtkDirectoryList

;;;Flags: Read / Write

;;;Allowed values: >= -2147483647

;;;Default value: 0

;;;The “loading” property
;;;  “loading”                  gboolean
;;;TRUE if files are being loaded

;;;Owner: GtkDirectoryList

;;;Flags: Read

;;;Default value: FALSE

;;;The “monitored” property
;;;  “monitored”                gboolean
;;;TRUE if the directory is monitored for changed

;;;Owner: GtkDirectoryList

;;;Flags: Read / Write

;;;Default value: TRUE

;;;See Also
;;;GListModel, g_file_enumerate_children()



;;;Description
;;;GtkDirectoryList is a list model that wraps g_file_enumerate_children_async(). It presents a GListModel and fills it asynchronously with the GFileInfos returned from that function.

;;;Enumeration will start automatically when a the “file” property is set.

;;;While the GtkDirectoryList is being filled, the “loading” property will be set to TRUE. You can listen to that property if you want to show information like a GtkSpinner or a "Loading..." text.

;;;If loading fails at any point, the “error” property will be set to give more indication about the failure.

;;;The GFileInfos returned from a GtkDirectoryList have the "standard::file" attribute set to the GFile they refer to. This way you can get at the file that is referred to in the same way you would via g_file_enumerator_get_child(). This means you do not need access to the GtkDirectoryList but can access the GFile directly from the GFileInfo when operating with a GtkListView or similar.

;;;Functions
;;;gtk_directory_list_new ()
;;;GtkDirectoryList *
;;;gtk_directory_list_new (const char *attributes,
;;;                        GFile *file);
;;;Creates a new GtkDirectoryList querying the given file with the given attributes .

;;;Parameters
;;;file

;;;The file to query.

;;;[allow-none]
;;;attributes

;;;The attributes to query with.

;;;[allow-none]
;;;Returns
;;;a new GtkDirectoryList

;;;gtk_directory_list_get_attributes ()
;;;const char *
;;;gtk_directory_list_get_attributes (GtkDirectoryList *self);
;;;Gets the attributes queried on the children.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;The queried attributes.

;;;[nullable][transfer none]

;;;gtk_directory_list_set_attributes ()
;;;void
;;;gtk_directory_list_set_attributes (GtkDirectoryList *self,
;;;                                   const char *attributes);
;;;Sets the attributes to be enumerated and starts the enumeration.

;;;If attributes is NULL, no attributes will be queried, but a list of GFileInfos will still be created.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;attributes

;;;the attributes to enumerate.

;;;[allow-none]
;;;gtk_directory_list_get_file ()
;;;GFile *
;;;gtk_directory_list_get_file (GtkDirectoryList *self);
;;;Gets the file whose children are currently enumerated.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;The file whose children are enumerated.

;;;[nullable][transfer none]

;;;gtk_directory_list_set_file ()
;;;void
;;;gtk_directory_list_set_file (GtkDirectoryList *self,
;;;                             GFile *file);
;;;Sets the file to be enumerated and starts the enumeration.

;;;If file is NULL, the result will be an empty list.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;file

;;;the GFile to be enumerated.

;;;[allow-none]
;;;gtk_directory_list_get_io_priority ()
;;;int
;;;gtk_directory_list_get_io_priority (GtkDirectoryList *self);
;;;Gets the IO priority set via gtk_directory_list_set_io_priority().

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;The IO priority.

;;;gtk_directory_list_set_io_priority ()
;;;void
;;;gtk_directory_list_set_io_priority (GtkDirectoryList *self,
;;;                                    int io_priority);
;;;Sets the IO priority to use while loading directories.

;;;Setting the priority while self is loading will reprioritize the ongoing load as soon as possible.

;;;The default IO priority is G_PRIORITY_DEFAULT, which is higher than the GTK redraw priority. If you are loading a lot of directories in parallel, lowering it to something like G_PRIORITY_DEFAULT_IDLE may increase responsiveness.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;io_priority

;;;IO priority to use

;;;gtk_directory_list_get_monitored ()
;;;gboolean
;;;gtk_directory_list_get_monitored (GtkDirectoryList *self);
;;;Returns whether the directory list is monitoring the directory for changes.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;TRUE if the directory is monitored

;;;gtk_directory_list_set_monitored ()
;;;void
;;;gtk_directory_list_set_monitored (GtkDirectoryList *self,
;;;                                  gboolean monitored);
;;;Sets whether the directory list will monitor the directory for changes. If monitoring is enabled, the “items-changed” signal will be emitted when the directory contents change.

;;;When monitoring is turned on after the initial creation of the directory list, the directory is reloaded to avoid missing files that appeared between the initial loading and when monitoring was turned on.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;monitored

;;;TRUE to monitor the directory for changes

;;;gtk_directory_list_is_loading ()
;;;gboolean
;;;gtk_directory_list_is_loading (GtkDirectoryList *self);
;;;Returns TRUE if the children enumeration is currently in progress.

;;;Files will be added to self from time to time while loading is going on. The order in which are added is undefined and may change in between runs.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;TRUE if self is loading

;;;gtk_directory_list_get_error ()
;;;const GError *
;;;gtk_directory_list_get_error (GtkDirectoryList *self);
;;;Gets the loading error, if any.

;;;If an error occurs during the loading process, the loading process will finish and this property allows querying the error that happened. This error will persist until a file is loaded again.

;;;An error being set does not mean that no files were loaded, and all successfully queried files will remain in the list.

;;;Parameters
;;;self

;;;a GtkDirectoryList

;;;Returns
;;;The loading error or NULL if loading finished successfully.

;;;[nullable][transfer none]


;;; --- End of file gtk4.directory-list.lisp -----------------------------------
