;;; ----------------------------------------------------------------------------
;;; gtk4.bookmark-list.lisp
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
;;; GtkBookmarkList
;;;
;;;     A list model for recently used files
;;;
;;; Types and Values
;;;
;;;     GtkBookmarkList
;;;
;;; Functions
;;;
;;;     gtk_bookmark_list_new
;;;     gtk_bookmark_list_get_filename
;;;     gtk_bookmark_list_set_attributes
;;;     gtk_bookmark_list_get_attributes
;;;     gtk_bookmark_list_set_io_priority
;;;     gtk_bookmark_list_get_io_priority
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

(gobject:define-g-object-class "GtkBookmarkList" bookmark-list
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
    bookmark-list-item-type
    "item-type" "GType" t nil)
   (loading
    bookmark-list-loading
    "loading" "gboolean" t nil)
   #+gtk-4-8
   (n-items
    bookmark-list-n-items
    "n-items" "guint" t nil)))



;;;Property Details
;;;The “attributes” property
;;;  “attributes”               char *
;;;The attributes to query

;;;Owner: GtkBookmarkList

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “filename” property
;;;  “filename”                 char *
;;;Bookmark file to load.

;;;Owner: GtkBookmarkList

;;;Flags: Read / Write / Construct Only

;;;Default value: NULL

;;;The “io-priority” property
;;;  “io-priority”              int
;;;Priority used when loading

;;;Owner: GtkBookmarkList

;;;Flags: Read / Write

;;;Allowed values: >= -2147483647

;;;Default value: 0

;;;The “loading” property
;;;  “loading”                  gboolean
;;;TRUE if files are being loaded

;;;Owner: GtkBookmarkList

;;;Flags: Read

;;;Default value: FALSE

;;;See Also
;;;GListModel, GBookmarkFile



;;;Description
;;;GtkBookmarkList is a list model that wraps GBookmarkFile. It presents a GListModel and fills it asynchronously with the GFileInfos returned from that function.

;;;The GFileInfos in the list have some attributes in the recent namespace added: recent::private (boolean) and recent:applications (stringv).

;;;Functions
;;;gtk_bookmark_list_new ()
;;;GtkBookmarkList *
;;;gtk_bookmark_list_new (const char *filename,
;;;                       const char *attributes);
;;;Creates a new GtkBookmarkList with the given attributes .

;;;Parameters
;;;filename

;;;The bookmark file to load.

;;;[allow-none]
;;;attributes

;;;The attributes to query.

;;;[allow-none]
;;;Returns
;;;a new GtkBookmarkList

;;;gtk_bookmark_list_get_filename ()
;;;const char *
;;;gtk_bookmark_list_get_filename (GtkBookmarkList *self);
;;;Returns the filename of the bookmark file that this list is loading.

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;Returns
;;;the filename of the .xbel file

;;;gtk_bookmark_list_set_attributes ()
;;;void
;;;gtk_bookmark_list_set_attributes (GtkBookmarkList *self,
;;;                                  const char *attributes);
;;;Sets the attributes to be enumerated and starts the enumeration.

;;;If attributes is NULL, no attributes will be queried, but a list of GFileInfos will still be created.

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;attributes

;;;the attributes to enumerate.

;;;[allow-none]
;;;gtk_bookmark_list_get_attributes ()
;;;const char *
;;;gtk_bookmark_list_get_attributes (GtkBookmarkList *self);
;;;Gets the attributes queried on the children.

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;Returns
;;;The queried attributes.

;;;[nullable][transfer none]

;;;gtk_bookmark_list_set_io_priority ()
;;;void
;;;gtk_bookmark_list_set_io_priority (GtkBookmarkList *self,
;;;                                   int io_priority);
;;;Sets the IO priority to use while loading files.

;;;The default IO priority is G_PRIORITY_DEFAULT.

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;io_priority

;;;IO priority to use

;;;gtk_bookmark_list_get_io_priority ()
;;;int
;;;gtk_bookmark_list_get_io_priority (GtkBookmarkList *self);
;;;Gets the IO priority set via gtk_bookmark_list_set_io_priority().

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;Returns
;;;The IO priority.

;;;gtk_bookmark_list_is_loading ()
;;;gboolean
;;;gtk_bookmark_list_is_loading (GtkBookmarkList *self);
;;;Returns TRUE if the files are currently being loaded.

;;;Files will be added to self from time to time while loading is going on. The order in which are added is undefined and may change in between runs.

;;;Parameters
;;;self

;;;a GtkBookmarkList

;;;Returns
;;;TRUE if self is loading



;;; --- End of file gtk4.bookmark-list.lisp ------------------------------------
