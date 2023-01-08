;;; ----------------------------------------------------------------------------
;;; gtk.recent-manager.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkRecentManager
;;;
;;;     Managing recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentManager
;;;     GtkRecentInfo
;;;     GtkRecentData
;;;     GtkRecentManagerError
;;;
;;;     GTK_RECENT_MANAGER_ERROR
;;;
;;; Functions
;;;
;;;     gtk_recent_manager_new
;;;     gtk_recent_manager_get_default
;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_add_full
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_lookup_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_move_item
;;;     gtk_recent_manager_get_items
;;;     gtk_recent_manager_purge_items

;;;     gtk_recent_info_ref
;;;     gtk_recent_info_unref
;;;     gtk_recent_info_get_uri
;;;     gtk_recent_info_get_display_name
;;;     gtk_recent_info_get_description
;;;     gtk_recent_info_get_mime_type
;;;     gtk_recent_info_get_added
;;;     gtk_recent_info_get_modified
;;;     gtk_recent_info_get_visited
;;;     gtk_recent_info_get_private_hint
;;;     gtk_recent_info_get_application_info
;;;     gtk_recent_info_get_applications
;;;     gtk_recent_info_last_application
;;;     gtk_recent_info_has_application
;;;     gtk_recent_info_create_app_info
;;;     gtk_recent_info_get_groups
;;;     gtk_recent_info_has_group
;;;     gtk_recent_info_get_gicon
;;;     gtk_recent_info_get_short_name
;;;     gtk_recent_info_get_uri_display
;;;     gtk_recent_info_get_age
;;;     gtk_recent_info_is_local
;;;     gtk_recent_info_exists
;;;     gtk_recent_info_match
;;;
;;; Properties
;;;
;;;     filename
;;;     size
;;;
;;; Signals
;;;
;;;     changed
;;;
;;;Object Hierarchy
;;;    GObject
;;;    ╰── GtkRecentManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentInfo
;;;
;;; GtkRecentInfo contains private data only, and should be accessed using the
;;; provided API.
;;;
;;; GtkRecentInfo contains all the meta-data associated with an entry in the
;;; recently used files list.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkRecentData
;;;
;;; struct GtkRecentData {
;;;   char *display_name;
;;;   char *description;
;;;
;;;   char *mime_type;
;;;
;;;   char *app_name;
;;;   char *app_exec;
;;;
;;;   char **groups;
;;;
;;;   gboolean is_private;
;;; };
;;;
;;; Meta-data to be passed to gtk_recent_manager_add_full() when registering a
;;; recently used resource.
;;;
;;; char *display_name;
;;;     a UTF-8 encoded string, containing the name of the recently used
;;;     resource to be displayed, or NULL;
;;;
;;; char *description;
;;;     a UTF-8 encoded string, containing a short description of the resource,
;;;     or NULL;
;;;
;;; char *mime_type;
;;;     the MIME type of the resource;
;;;
;;; char *app_name;
;;;     the name of the application that is registering this recently used
;;;     resource;
;;;
;;; char *app_exec;
;;;     command line used to launch this resource; may contain the “%f” and “%u”
;;;     escape characters which will be expanded to the resource file path and
;;;     URI respectively when the command line is retrieved;
;;;
;;; char **groups;
;;;     a vector of strings containing groups names;.
;;;
;;; gboolean is_private;
;;;     whether this resource should be displayed only by the applications that
;;;     have registered it or not.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_RECENT_MANAGER_ERROR
;;;
;;; #define GTK_RECENT_MANAGER_ERROR (gtk_recent_manager_error_quark ())
;;;
;;; The GError domain for GtkRecentManager errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkRecentManagerError
;;;
;;; Error codes for GtkRecentManager operations
;;;
;;; GTK_RECENT_MANAGER_ERROR_NOT_FOUND
;;;     the URI specified does not exists in the recently used resources list.
;;;
;;; GTK_RECENT_MANAGER_ERROR_INVALID_URI
;;;     the URI specified is not valid.
;;;
;;; GTK_RECENT_MANAGER_ERROR_INVALID_ENCODING
;;;     the supplied string is not UTF-8 encoded.
;;;
;;; GTK_RECENT_MANAGER_ERROR_NOT_REGISTERED
;;;     no application has registered the specified item.
;;;
;;; GTK_RECENT_MANAGER_ERROR_READ
;;;     failure while reading the recently used resources file.
;;;
;;; GTK_RECENT_MANAGER_ERROR_WRITE
;;;     failure while writing the recently used resources file.
;;;
;;; GTK_RECENT_MANAGER_ERROR_UNKNOWN
;;;     unspecified error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;struct GtkRecentManager
;;;
;;; GtkRecentManager contains only private data and should be accessed using the
;;; provided API.

;;;GtkRecentManager provides a facility for adding, removing and looking up recently used files. Each recently used file is identified by its URI, and has meta-data associated to it, like the names and command lines of the applications that have registered it, the number of time each application has registered the same file, the mime type of the file and whether the file should be displayed only by the applications that have registered it.

;;;The recently used files list is per user.

;;;The GtkRecentManager acts like a database of all the recently used files. You can create new GtkRecentManager objects, but it is more efficient to use the default manager created by GTK

;;;Adding a new recently used file is as simple as:

;;;The GtkRecentManager will try to gather all the needed information from the file itself through GIO.

;;;Looking up the meta-data associated with a recently used file given its URI requires calling gtk_recent_manager_lookup_item():

;;;GtkRecentManager *manager;

;;;manager = gtk_recent_manager_get_default ();
;;;gtk_recent_manager_add_item (manager, file_uri);
;;;In order to retrieve the list of recently used files, you can use gtk_recent_manager_get_items(), which returns a list of GtkRecentInfo.

;;;A GtkRecentManager is the model used to populate the contents of one, or more GtkRecentChooser implementations.

;;;Note that the maximum age of the recently used files list is controllable through the “gtk-recent-files-max-age” property.


;;;Signal Details
;;;The “changed” signal
;;;void
;;;user_function (GtkRecentManager *recent_manager,
;;;               gpointer          user_data)
;;;Emitted when the current recently used resources manager changes its contents, either by calling gtk_recent_manager_add_item() or by another application.

;;;Parameters
;;;recent_manager

;;;the recent manager

;;;user_data

;;;user data set when the signal handler was connected.

;;;Flags: Run First

;;;See Also
;;;GBookmarkFile, GtkSettings, GtkRecentChooser
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkRecentManager" recent-manager
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_recent_manager_get_type")
  ((filename
    recent-manager-filename
    "filename" "gchararray" t nil)
   (size
    recent-manager-size
    "size" "gint" t nil)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “filename” property
;;;
;;;  “filename”                 char *
;;;
;;; The full path to the file to be used to store and read the recently used
;;; resources list
;;;
;;; Owner: GtkRecentManager
;;;
;;; Flags: Read / Write / Construct Only
;;;
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “size” property
;;;
;;;  “size”                     int
;;;
;;; The size of the recently used resources list.
;;;
;;; Owner: GtkRecentManager
;;;
;;; Flags: Read
;;;
;;; Allowed values: >= -1
;;;
;;; Default value: 0
;;; ----------------------------------------------------------------------------










;;;Functions
;;;gtk_recent_manager_new ()
;;;GtkRecentManager *
;;;gtk_recent_manager_new (void);
;;;Creates a new recent manager object. Recent manager objects are used to handle the list of recently used resources. A GtkRecentManager object monitors the recently used resources list, and emits the “changed” signal each time something inside the list changes.

;;;GtkRecentManager objects are expensive: be sure to create them only when needed. You should use gtk_recent_manager_get_default() instead.

;;;Returns
;;;A newly created GtkRecentManager object

;;;gtk_recent_manager_get_default ()
;;;GtkRecentManager *
;;;gtk_recent_manager_get_default (void);
;;;Gets a unique instance of GtkRecentManager, that you can share in your application without caring about memory management.

;;;Returns
;;;A unique GtkRecentManager. Do not ref or unref it.

;;;[transfer none]

;;;gtk_recent_manager_add_item ()
;;;gboolean
;;;gtk_recent_manager_add_item (GtkRecentManager *manager,
;;;                             const char *uri);
;;;Adds a new resource, pointed by uri , into the recently used resources list.

;;;This function automatically retrieves some of the needed metadata and setting other metadata to common default values; it then feeds the data to gtk_recent_manager_add_full().

;;;See gtk_recent_manager_add_full() if you want to explicitly define the metadata for the resource pointed by uri .

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;a valid URI

;;;Returns
;;;TRUE if the new item was successfully added to the recently used resources list

;;;gtk_recent_manager_add_full ()
;;;gboolean
;;;gtk_recent_manager_add_full (GtkRecentManager *manager,
;;;                             const char *uri,
;;;                             const GtkRecentData *recent_data);
;;;Adds a new resource, pointed by uri , into the recently used resources list, using the metadata specified inside the GtkRecentData passed in recent_data .

;;;The passed URI will be used to identify this resource inside the list.

;;;In order to register the new recently used resource, metadata about the resource must be passed as well as the URI; the metadata is stored in a GtkRecentData, which must contain the MIME type of the resource pointed by the URI; the name of the application that is registering the item, and a command line to be used when launching the item.

;;;Optionally, a GtkRecentData might contain a UTF-8 string to be used when viewing the item instead of the last component of the URI; a short description of the item; whether the item should be considered private - that is, should be displayed only by the applications that have registered it.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;a valid URI

;;;recent_data

;;;metadata of the resource

;;;Returns
;;;TRUE if the new item was successfully added to the recently used resources list, FALSE otherwise

;;;gtk_recent_manager_remove_item ()
;;;gboolean
;;;gtk_recent_manager_remove_item (GtkRecentManager *manager,
;;;                                const char *uri,
;;;                                GError **error);
;;;Removes a resource pointed by uri from the recently used resources list handled by a recent manager.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;the URI of the item you wish to remove

;;;error

;;;return location for a GError, or NULL.

;;;[allow-none]
;;;Returns
;;;TRUE if the item pointed by uri has been successfully removed by the recently used resources list, and FALSE otherwise

;;;gtk_recent_manager_lookup_item ()
;;;GtkRecentInfo *
;;;gtk_recent_manager_lookup_item (GtkRecentManager *manager,
;;;                                const char *uri,
;;;                                GError **error);
;;;Searches for a URI inside the recently used resources list, and returns a GtkRecentInfo containing information about the resource like its MIME type, or its display name.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;a URI

;;;error

;;;a return location for a GError, or NULL.

;;;[allow-none]
;;;Returns
;;;a GtkRecentInfo containing information about the resource pointed by uri , or NULL if the URI was not registered in the recently used resources list. Free with gtk_recent_info_unref().

;;;[nullable]

;;;gtk_recent_manager_has_item ()
;;;gboolean
;;;gtk_recent_manager_has_item (GtkRecentManager *manager,
;;;                             const char *uri);
;;;Checks whether there is a recently used resource registered with uri inside the recent manager.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;a URI

;;;Returns
;;;TRUE if the resource was found, FALSE otherwise

;;;gtk_recent_manager_move_item ()
;;;gboolean
;;;gtk_recent_manager_move_item (GtkRecentManager *manager,
;;;                              const char *uri,
;;;                              const char *new_uri,
;;;                              GError **error);
;;;Changes the location of a recently used resource from uri to new_uri .

;;;Please note that this function will not affect the resource pointed by the URIs, but only the URI used in the recently used resources list.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;uri

;;;the URI of a recently used resource

;;;new_uri

;;;the new URI of the recently used resource, or NULL to remove the item pointed by uri in the list.

;;;[allow-none]
;;;error

;;;a return location for a GError, or NULL.

;;;[allow-none]
;;;Returns
;;;TRUE on success

;;;gtk_recent_manager_get_items ()
;;;GList *
;;;gtk_recent_manager_get_items (GtkRecentManager *manager);
;;;Gets the list of recently used resources.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;Returns
;;;a list of newly allocated GtkRecentInfo objects. Use gtk_recent_info_unref() on each item inside the list, and then free the list itself using g_list_free().

;;;[element-type GtkRecentInfo][transfer full]

;;;gtk_recent_manager_purge_items ()
;;;int
;;;gtk_recent_manager_purge_items (GtkRecentManager *manager,
;;;                                GError **error);
;;;Purges every item from the recently used resources list.

;;;Parameters
;;;manager

;;;a GtkRecentManager

;;;error

;;;a return location for a GError, or NULL.

;;;[allow-none]
;;;Returns
;;;the number of items that have been removed from the recently used resources list

;;;gtk_recent_info_ref ()
;;;GtkRecentInfo *
;;;gtk_recent_info_ref (GtkRecentInfo *info);
;;;Increases the reference count of recent_info by one.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;the recent info object with its reference count increased by one

;;;gtk_recent_info_unref ()
;;;void
;;;gtk_recent_info_unref (GtkRecentInfo *info);
;;;Decreases the reference count of info by one. If the reference count reaches zero, info is deallocated, and the memory freed.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;gtk_recent_info_get_uri ()
;;;const char *
;;;gtk_recent_info_get_uri (GtkRecentInfo *info);
;;;Gets the URI of the resource.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;the URI of the resource. The returned string is owned by the recent manager, and should not be freed.

;;;gtk_recent_info_get_display_name ()
;;;const char *
;;;gtk_recent_info_get_display_name (GtkRecentInfo *info);
;;;Gets the name of the resource. If none has been defined, the basename of the resource is obtained.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;the display name of the resource. The returned string is owned by the recent manager, and should not be freed.

;;;gtk_recent_info_get_description ()
;;;const char *
;;;gtk_recent_info_get_description (GtkRecentInfo *info);
;;;Gets the (short) description of the resource.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;the description of the resource. The returned string is owned by the recent manager, and should not be freed.

;;;gtk_recent_info_get_mime_type ()
;;;const char *
;;;gtk_recent_info_get_mime_type (GtkRecentInfo *info);
;;;Gets the MIME type of the resource.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;the MIME type of the resource. The returned string is owned by the recent manager, and should not be freed.

;;;gtk_recent_info_get_added ()
;;;GDateTime *
;;;gtk_recent_info_get_added (GtkRecentInfo *info);
;;;Gets the the time when the resource was added to the recently used resources list.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a GDateTime for the time when the resource was added.

;;;[transfer none]

;;;gtk_recent_info_get_modified ()
;;;GDateTime *
;;;gtk_recent_info_get_modified (GtkRecentInfo *info);
;;;Gets the time when the meta-data for the resource was last modified.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a GDateTime for the time when the resource was last modified.

;;;[transfer none]

;;;gtk_recent_info_get_visited ()
;;;GDateTime *
;;;gtk_recent_info_get_visited (GtkRecentInfo *info);
;;;Gets the time when the meta-data for the resource was last visited.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a GDateTime for the time when the resource was last visited.

;;;[transfer none]

;;;gtk_recent_info_get_private_hint ()
;;;gboolean
;;;gtk_recent_info_get_private_hint (GtkRecentInfo *info);
;;;Gets the value of the “private” flag. Resources in the recently used list that have this flag set to TRUE should only be displayed by the applications that have registered them.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;TRUE if the private flag was found, FALSE otherwise

;;;gtk_recent_info_get_application_info ()
;;;gboolean
;;;gtk_recent_info_get_application_info (GtkRecentInfo *info,
;;;                                      const char *app_name,
;;;                                      const char **app_exec,
;;;                                      guint *count,
;;;                                      GDateTime **stamp);
;;;Gets the data regarding the application that has registered the resource pointed by info .

;;;If the command line contains any escape characters defined inside the storage specification, they will be expanded.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;app_name

;;;the name of the application that has registered this item

;;;app_exec

;;;return location for the string containing the command line.

;;;[transfer none][out]
;;;count

;;;return location for the number of times this item was registered.

;;;[out]
;;;stamp

;;;return location for the time this item was last registered for this application.

;;;[out][transfer none]
;;;Returns
;;;TRUE if an application with app_name has registered this resource inside the recently used list, or FALSE otherwise. The app_exec string is owned by the GtkRecentInfo and should not be modified or freed

;;;gtk_recent_info_get_applications ()
;;;char **
;;;gtk_recent_info_get_applications (GtkRecentInfo *info,
;;;                                  gsize *length);
;;;Retrieves the list of applications that have registered this resource.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;length

;;;return location for the length of the returned list.

;;;[out][allow-none]
;;;Returns
;;;a newly allocated NULL-terminated array of strings. Use g_strfreev() to free it.

;;;[array length=length zero-terminated=1][transfer full]

;;;gtk_recent_info_last_application ()
;;;char *
;;;gtk_recent_info_last_application (GtkRecentInfo *info);
;;;Gets the name of the last application that have registered the recently used resource represented by info .

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;an application name. Use g_free() to free it.

;;;gtk_recent_info_has_application ()
;;;gboolean
;;;gtk_recent_info_has_application (GtkRecentInfo *info,
;;;                                 const char *app_name);
;;;Checks whether an application registered this resource using app_name .

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;app_name

;;;a string containing an application name

;;;Returns
;;;TRUE if an application with name app_name was found, FALSE otherwise

;;;gtk_recent_info_create_app_info ()
;;;GAppInfo *
;;;gtk_recent_info_create_app_info (GtkRecentInfo *info,
;;;                                 const char *app_name,
;;;                                 GError **error);
;;;Creates a GAppInfo for the specified GtkRecentInfo

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;app_name

;;;the name of the application that should be mapped to a GAppInfo; if NULL is used then the default application for the MIME type is used.

;;;[allow-none]
;;;error

;;;return location for a GError, or NULL.

;;;[allow-none]
;;;Returns
;;;the newly created GAppInfo, or NULL. In case of error, error will be set either with a GTK_RECENT_MANAGER_ERROR or a G_IO_ERROR.

;;;[nullable][transfer full]

;;;gtk_recent_info_get_groups ()
;;;char **
;;;gtk_recent_info_get_groups (GtkRecentInfo *info,
;;;                            gsize *length);
;;;Returns all groups registered for the recently used item info . The array of returned group names will be NULL terminated, so length might optionally be NULL.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;length

;;;return location for the number of groups returned.

;;;[out][allow-none]
;;;Returns
;;;a newly allocated NULL terminated array of strings. Use g_strfreev() to free it.

;;;[array length=length zero-terminated=1][transfer full]

;;;gtk_recent_info_has_group ()
;;;gboolean
;;;gtk_recent_info_has_group (GtkRecentInfo *info,
;;;                           const char *group_name);
;;;Checks whether group_name appears inside the groups registered for the recently used item info .

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;group_name

;;;name of a group

;;;Returns
;;;TRUE if the group was found

;;;gtk_recent_info_get_gicon ()
;;;GIcon *
;;;gtk_recent_info_get_gicon (GtkRecentInfo *info);
;;;Retrieves the icon associated to the resource MIME type.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a GIcon containing the icon, or NULL. Use g_object_unref() when finished using the icon.

;;;[nullable][transfer full]

;;;gtk_recent_info_get_short_name ()
;;;char *
;;;gtk_recent_info_get_short_name (GtkRecentInfo *info);
;;;Computes a valid UTF-8 string that can be used as the name of the item in a menu or list. For example, calling this function on an item that refers to “file:///foo/bar.txt” will yield “bar.txt”.

;;;Parameters
;;;info

;;;an GtkRecentInfo

;;;Returns
;;;A newly-allocated string in UTF-8 encoding free it with g_free()

;;;gtk_recent_info_get_uri_display ()
;;;char *
;;;gtk_recent_info_get_uri_display (GtkRecentInfo *info);
;;;Gets a displayable version of the resource’s URI. If the resource is local, it returns a local path; if the resource is not local, it returns the UTF-8 encoded content of gtk_recent_info_get_uri().

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a newly allocated UTF-8 string containing the resource’s URI or NULL. Use g_free() when done using it.

;;;[nullable]

;;;gtk_recent_info_get_age ()
;;;int
;;;gtk_recent_info_get_age (GtkRecentInfo *info);
;;;Gets the number of days elapsed since the last update of the resource pointed by info .

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;a positive integer containing the number of days elapsed since the time this resource was last modified

;;;gtk_recent_info_is_local ()
;;;gboolean
;;;gtk_recent_info_is_local (GtkRecentInfo *info);
;;;Checks whether the resource is local or not by looking at the scheme of its URI.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;TRUE if the resource is local

;;;gtk_recent_info_exists ()
;;;gboolean
;;;gtk_recent_info_exists (GtkRecentInfo *info);
;;;Checks whether the resource pointed by info still exists. At the moment this check is done only on resources pointing to local files.

;;;Parameters
;;;info

;;;a GtkRecentInfo

;;;Returns
;;;TRUE if the resource exists

;;;gtk_recent_info_match ()
;;;gboolean
;;;gtk_recent_info_match (GtkRecentInfo *info_a,
;;;                       GtkRecentInfo *info_b);
;;;Checks whether two GtkRecentInfo point to the same resource.

;;;Parameters
;;;info_a

;;;a GtkRecentInfo

;;;info_b

;;;a GtkRecentInfo

;;;Returns
;;;TRUE if both GtkRecentInfo point to the same resource, FALSE otherwise



;;; --- End of file gtk.recent-manager.lisp ------------------------------------
