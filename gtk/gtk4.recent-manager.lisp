;;; ----------------------------------------------------------------------------
;;; gtk4.recent-manager.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
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
;;;
;;; GtkRecentManager
;;;
;;;     Managing recently used files
;;;
;;; Types and Values
;;;
;;;     GtkRecentManager
;;;     GtkRecentInfo
;;;
;;;     GtkRecentData                                       not implemented
;;;     GtkRecentManagerError                               not implemented
;;;
;;; Functions
;;;
;;;     gtk_recent_manager_new
;;;     gtk_recent_manager_get_default
;;;     gtk_recent_manager_add_item
;;;     gtk_recent_manager_add_full                         not implemented
;;;     gtk_recent_manager_remove_item
;;;     gtk_recent_manager_lookup_item
;;;     gtk_recent_manager_has_item
;;;     gtk_recent_manager_move_item
;;;     gtk_recent_manager_get_items
;;;     gtk_recent_manager_purge_items
;;;
;;;     gtk_recent_info_ref                                 not needed
;;;     gtk_recent_info_unref                               not needed
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
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkRecentManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRecentInfo
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque recent-info "GtkRecentInfo"
  :export t
  :type-initializer "gtk_recent_info_get_type"
  :alloc (error "GtkRecentInfo cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'recent-info)
      "GBoxed"
      (documentation 'recent-info 'type)
 "@version{2025-01-11}
  @begin{declaration}
(glib:define-gboxed-opaque gtk:recent-info \"GtkRecentInfo\"
  :type-initializer \"gtk_recent_info_get_type\"
  :alloc (error \"GtkRecentInfo cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{gtk:recent-info} structure constains all the meta-data
    associated with an entry in the recently used files list.
  @end{short}
  The @class{gtk:recent-info} structure is an opaque data structure whose
  members can only be accessed using the provided API.
  @see-class{gtk:recent-manager}")

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
;;; GtkRecentManager
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkRecentManager" recent-manager
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

#+liber-documentation
(setf (documentation 'recent-manager 'type)
 "@version{2025-07-16}
  @begin{short}
    The @class{gtk:recent-manager} object provides a facility for adding,
    removing and looking up recently used files.
  @end{short}
  Each recently used file is identified by its URI, and has meta-data associated
  to it, like the names and command lines of the applications that have
  registered it, the number of time each application has registered the same
  file, the MIME type of the file and whether the file should be displayed only
  by the applications that have registered it. The recently used files list is
  per user.

  The @class{gtk:recent-manager} object acts like a database of all the recently
  used files. You can create new @class{gtk:recent-manager} objects, but it is
  more efficient to use the default manager created by GTK. Adding a new
  recently used file is as simple as:
  @begin{pre}
(let ((uri \"file:///home/ ... uri to add ...\")
      (manager (gtk:recent-manager-default)))
  (gtk:recent-manager-add-item manager uri)
  ... )
  @end{pre}
  The @class{gtk:recent-manager} will try to gather all the needed information
  from the file itself through GIO. Looking up the meta-data associated with a
  recently used file given its URI requires calling the
  @fun{gtk:recent-manager-lookup-item} function:
  @begin{pre}
(let* ((uri \"file:///home/ ... uri to look up ...\")
       (manager (gtk:recent-manager-default))
       (info (gtk:recent-manager-lookup-item manager uri)))
    (when info
      ;; Use the object
      ... ))
  @end{pre}
  In order to retrieve the list of recently used files, you can use the
  @fun{gtk:recent-manager-items} function, which returns a list of
  @class{gtk:recent-info} instances.

  Note that the maximum age of the recently used files list is controllable
  through the @slot[gtk:settings]{gtk-recent-files-max-age} setting of the
  @class{gtk:settings} object.
  @begin[Signal Details]{dictionary}
    @begin[recent-manager::changed]{signal}
      @begin{pre}
lambda (manager)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[manager]{The @class{gtk:recent-manager} object that received
          the signal.}
      @end{simple-table}
      Emitted when the current recently used resources manager changes its
      contents, either by calling the @fun{gtk:recent-manager-add-item} function
      or by another application.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:recent-manager-new}
  @see-constructor{gtk:recent-manager-default}
  @see-slot{gtk:recent-manager-filename}
  @see-slot{gtk:recent-manager-size}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:recent-manager-filename --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filename" 'recent-manager) t)
 "The @code{filename} property of type @code{:string} (Read / Write / Construct)
  @br{}
  The full path to the file to be used to store and read the recently used
  resources list. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'recent-manager-filename)
      "Accessor"
      (documentation 'recent-manager-filename 'function)
 "@version{2025-09-21}
  @syntax{(gtk:recent-manager-filename object) => filename}
  @argument[object]{a @class{gtk:recent-manager} object}
  @argument[filename]{a string for the full path to the file}
  @begin{short}
    The accessor for the @slot[gtk:recent-manager]{filename} slot of the
    @class{gtk:recent-manager} class returns the full path to the file to be
    used to store and read the recently used resources list.
  @end[short}
  @see-class{gtk:recent-manager}")

;;; --- gtk:recent-manager-size ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'recent-manager) t)
 "The @code{size} property of type @code{:int} (Read) @br{}
  The size of the recently used resources list. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'recent-manager-size)
      "Accessor"
      (documentation 'recent-manager-size 'function)
 "@version{2025-09-21}
  @syntax{(gtk:recent-manager-size object) => size}
  @argument[object]{a @class{gtk:recent-manager} object}
  @argument[size]{an integer for the size of the resources list}
  @begin{short}
    The accessor for the @slot[gtk:recent-manager]{size} slot of the
    @class{gtk:recent-manager} class returns the size of the recently used
    resources list.
  @end{short}
  @see-class{gtk:recent-manager}")

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_new
;;; ----------------------------------------------------------------------------

(declaim (inline recent-manager-new))

(defun recent-manager-new ()
 #+liber-documentation
 "@version{2025-07-24}
  @return{The newly created @class{gtk:recent-manager} object.}
  @begin{short}
    Creates a new recent manager object.
  @end{short}
  Recent manager objects are used to handle the list of recently used resources.
  A @class{gtk:recent-manager} object monitors the recently used resources list,
  and emits the @sig[gtk:recent-manager]{changed} signal each time something
  inside the list changes.

  The @class{gtk:recent-manager} object is expensive. Be sure to create it
  only when needed. You should use the @fun{gtk:recent-manager-default} function
  instead.
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-manager-default}"
  (make-instance 'recent-manager))

(export 'recent-manager-new)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_get_default" recent-manager-default)
    (g:object recent-manager)
 #+liber-documentation
 "@version{2024-01-06}
  @return{The unique @class{gtk:recent-manager} object.}
  @begin{short}
    Gets a unique instance of the default recent manager.
  @end{short}
  @see-class{gtk:recent-manager}")

(export 'recent-manager-default)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_add_item" recent-manager-add-item) :boolean
 #+liber-documentation
 "@version{2025-01-06}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[uri]{a string for a valid URI}
  @begin{return}
    @em{True} if the new item was successfully added to the recently used
    resources list.
  @end{return}
  @begin{short}
    Adds a new resource, pointed by @arg{uri}, into the recently used resources
    list.
  @end{short}
  This function automatically retrieves some of the needed metadata and
  setting other metadata to common default values.
  @see-class{gtk:recent-manager}"
  (manager (g:object recent-manager))
  (uri :string))

(export 'recent-manager-add-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_add_full                             not implemened
;;;
;;; Adds a new resource, pointed by uri, into the recently used resources list,
;;; using the metadata specified inside the GtkRecentData structure passed in
;;; recent_data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_remove_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_remove_item" %recent-manager-remove-item)
    :boolean
  (manager (g:object recent-manager))
  (uri :string)
  (err :pointer))

(defun recent-manager-remove-item (manager uri)
 #+liber-documentation
 "@version{2025-02-24}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[uri]{a string for the URI of the item you wish to remove}
  @begin{return}
    @em{True} if the item pointed by @arg{uri} has been successfully removed by
    the recently used resources list, and @em{false} otherwise.
  @end{return}
  @begin{short}
    Removes a resource pointed by @arg{uri} from the recently used resources
    list handled by a recent manager.
  @end{short}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-manager-add-item}"
  (glib:with-error (err)
    (%recent-manager-remove-item manager uri err)))

(export 'recent-manager-remove-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_lookup_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_lookup_item" %recent-manager-lookup-item)
    (g:boxed recent-info :return)
  (manager (g:object recent-manager))
  (uri :string)
  (err :pointer))

(defun recent-manager-lookup-item (manager uri)
 #+liber-documentation
 "@version{2025-01-06}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[uri]{a string for the URI}
  @begin{return}
    The @class{gtk:recent-info} instance containing information about the
    resource pointed by @arg{uri}, or @code{nil} if the URI was not registered
    in the recently used resources list.
  @end{return}
  @begin{short}
    Searches for a URI inside the recently used resources list, and returns a
    structure containing informations about the resource like its MIME type, or
    its display name.
  @end{short}
  @see-class{gtk:recent-manager}
  @see-class{gtk:recent-info}"
  (glib:with-error (err)
    (%recent-manager-lookup-item manager uri err)))

(export 'recent-manager-lookup-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_has_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_has_item" recent-manager-has-item) :boolean
 #+liber-documentation
 "@version{2025-01-06}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[uri]{a string for the URI}
  @return{@em{True} if the resource was found, @em{false} otherwise.}
  @begin{short}
    Checks whether there is a recently used resource registered with @arg{uri}
    inside the recent manager.
  @end{short}
  @see-class{gtk:recent-manager}
  @see-function{gtk:recent-manager-add-item}"
  (manager (g:object recent-manager))
  (uri :string))

(export 'recent-manager-has-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_move_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_move_item" %recent-manager-move-item)
    :boolean
  (manager (g:object recent-manager))
  (uri :string)
  (newuri :string)
  (err :pointer))

(defun recent-manager-move-item (manager uri newuri)
 #+liber-documentation
 "@version{2025-02-24}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @argument[uri]{a string for the URI of a recently used resource}
  @argument[newuri]{a string for the new URI of the recently used resource, or
    @code{nil} to remove the item pointed by @arg{uri} in the list}
  @return{@em{True} on success.}
  @begin{short}
    Changes the location of a recently used resource from @arg{uri}
    to @arg{newuri}.
  @end{short}
  Please note that this function will not affect the resource pointed by the
  URIs, but only the URI used in the recently used resources list.
  @see-class{gtk:recent-manager}"
  (glib:with-error (err)
    (%recent-manager-move-item manager
                               uri
                               (or newuri (cffi:null-pointer))
                               err)))

(export 'recent-manager-move-item)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_get_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_get_items" recent-manager-items)
    (g:list-t (g:boxed recent-info :return) :free-from-foreign t)
 #+liber-documentation
 "@version{2025-01-06}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @begin{return}
    The list of newly allocated @class{gtk:recent-info} instances.
  @end{return}
  @begin{short}
    Gets the list of recently used resources.
  @end{short}
  @see-class{gtk:recent-manager}
  @see-class{gtk:recent-info}"
  (manager (g:object recent-manager)))

(export 'recent-manager-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_manager_purge_items
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_manager_purge_items" %recent-manager-purge-items)
    :int
  (manager (g:object recent-manager))
  (err :pointer))

(defun recent-manager-purge-items (manager)
 #+liber-documentation
 "@version{#2025-07-15}
  @argument[manager]{a @class{gtk:recent-manager} object}
  @begin{return}
    The integer for the number of items that have been removed from the
    recently used resources list.
  @end{return}
  @begin{short}
    Purges every item from the recently used resources list.
  @end{short}
  @see-class{gtk:recent-manager}"
  (glib:with-error (err)
    (%recent-manager-purge-items manager err)))

(export 'recent-manager-purge-items)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_ref                                     not needed
;;;
;;; Increases the reference count of recent_info by one.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_unref                                   not needed
;;;
;;; Decreases the reference count of info by one.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_uri" recent-info-uri) :string
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string for the URI of the resource.}
  @short{Gets the URI of the resource.}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-uri)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_display_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_display_name" recent-info-display-name)
    :string
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string for the display name of the resource.}
  @begin{short}
    Gets the name of the resource.
  @end{short}
  If none has been defined, the basename of the resource is obtained.
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-display-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_description
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_description" recent-info-description)
    :string
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string for the description of the resource.}
  @short{Gets the (short) description of the resource.}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-description)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_mime_type" recent-info-mime-type) :string
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string for the MIME type of the resource.}
  @short{Gets the MIME type of the resource.}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_added
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_added" recent-info-added) g:date-time
 #+liber-documentation
 "@version{2025-07-19}
  @argument[info]{a @class{gtk:recent-info} instance}
  @begin{return}
    The long integer for the number of seconds elapsed from system's Epoch
    when the resource was added to the list, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was
    added to the recently used resources list.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-added)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_modified
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_modified" recent-info-modified) g:date-time
 #+liber-documentation
 "@version{2025-07-19}
  @argument[info]{a @class{gtk:recent-info} instance}
  @begin{return}
    The long integer for the number of seconds elapsed from system's Epoch
    when the resource was last modified, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was
    last modified.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-modified)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_visited
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_visited" recent-info-visited) g:date-time
 #+liber-documentation
 "@version{2025-07-19}
  @argument[info]{a @class{gtk:recent-info} instance}
  @begin{return}
    The long integer for the number of seconds elapsed from system's Epoch
    when the resource was last visited, or -1 on failure.
  @end{return}
  @begin{short}
    Gets the timestamp, seconds from system's Epoch, when the resource was
    last visited.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-visited)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_private_hint
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_private_hint" recent-info-private-hint)
    :boolean
 #+liber-documentation
 "@version{2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{@em{True} if the private flag was found, @em{false} otherwise.}
  @begin{short}
    Gets the value of the \"private\" flag.
  @end{short}
  Resources in the recently used list that have this flag set to @em{true}
  should only be displayed by the applications that have registered them.
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-private-hint)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_application_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_application_info"
               %recent-info-application-info) :boolean
  (info (g:boxed recent-info))
  (name :string)
  (exec (:pointer (:string :free-from-foreign nil)))
  (count (:pointer :int))
  (time (:pointer :long)))

(defun recent-info-application-info (info name)
 #+liber-documentation
 "@version{2025-07-19}
  @syntax{(gtk:recent-info-application-info info name) => exec, count, time}
  @argument[info]{a @class{gtk:recent-info} instance}
  @argument[name]{a string for the name of the application that has registered
    this item}
  @argument[exec]{a string containing the command line}
  @argument[count]{an integer for the number of times this item was registered}
  @argument[time]{a long integer for the timestamp this item was last
    registered for this application}
  @begin{short}
    Gets the data regarding the application that has registered the resource
    pointed by @arg{info}.
  @end{short}
  If the command line contains any escape characters defined inside the
  storage specification, they will be expanded.
  @see-class{gtk:recent-info}"
  (cffi:with-foreign-objects ((exec :string) (count :uint) (time :long))
    (%recent-info-application-info info name exec count time)
    (values (cffi:mem-ref exec :string)
            (cffi:mem-ref count :uint)
            (cffi:mem-ref time :long))))

(export 'recent-info-application-info)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_applications
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_applications" %recent-info-applications)
    g:strv-t
  (info (g:boxed recent-info))
  (length (:pointer :size)))

(defun recent-info-applications (info)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The list of strings for the applications.}
  @begin{short}
    Retrieves the list of applications that have registered this resource.
  @end{short}
  @see-class{gtk:recent-info}"
  (cffi:with-foreign-object (length :size)
    (%recent-info-applications info length)))

(export 'recent-info-applications)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_last_application
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_last_application" recent-info-last-application)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string for an application name.}
  @begin{short}
    Gets the name of the last application that have registered the recently used
    resource represented by @arg{info}.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-last-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_application
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_has_application" recent-info-has-application)
    :boolean
 #+liber-documentation
 "@version{2025-05-03}
  @argument[info]{a @class{gtk:recent-info} instance}
  @argument[name]{a string containing an application name}
  @begin{return}
    @em{True} if an application with name @arg{name} was found, @em{false}
    otherwise.
  @end{return}
  @begin{short}
    Checks whether an application registered this resource using @arg{name}.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info))
  (name :string))

(export 'recent-info-has-application)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_create_app_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_create_app_info" %recent-info-create-app-info)
    (g:object g:app-info)
  (info (g:boxed recent-info))
  (name :string)
  (err :pointer))

(defun recent-info-create-app-info (info name)
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @argument[name]{a string for the name of the application that should be
    mapped to a @class{g:app-info} object, if @code{nil} is used then the
    default application for the MIME type is used}
  @begin{return}
    The newly created @class{g:app-info} object, or @code{nil}.
  @end{return}
  @begin{short}
    Creates a @class{g:app-info} object for the specified
    @class{gtk:recent-info} instance.
  @end{short}
  @see-class{gtk:recent-info}
  @see-class{g:app-info}"
  (glib:with-error (err)
    (%recent-info-create-app-info info name err)))

(export 'recent-info-create-app-info)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_groups
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_groups" %recent-info-groups) g:strv-t
  (info (g:boxed recent-info))
  (length (:pointer :size)))

(defun recent-info-groups (info)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The list of strings for the groups.}
  @begin{short}
    Returns all groups registered for the recently used item @arg{info}.
  @end{short}
  @see-class{gtk:recent-info}"
  (cffi:with-foreign-object (length :size)
    (%recent-info-groups info length)))

(export 'recent-info-groups)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_has_group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_has_group" recent-info-has-group) :boolean
 #+liber-documentation
 "@version{#2025-05-03}
  @argument[info]{a @class{gtk:recent-info} instance}
  @argument[group]{a string for the name of a group}
  @return{@em{True} if the group was found.}
  @begin{short}
    Checks whether @arg{group} appears inside the groups registered for the
    recently used item @arg{info}.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info))
  (group :string))

(export 'recent-info-has-group)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_gicon" recent-info-gicon) (g:object g:icon)
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The @class{g:icon} icon containing the icon, or @code{nil}.}
  @begin{short}
    Retrieves the icon associated to the resource MIME type.
  @end{short}
  @see-class{gtk:recent-info}
  @see-class{g:icon}"
  (info (g:boxed recent-info)))

(export 'recent-info-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_short_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_short_name" recent-info-short-name)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{The string in UTF-8 encoding.}
  @begin{short}
    Computes a valid UTF-8 string that can be used as the name of the item in a
    menu or list.
  @end{short}
  For example, calling this function on an item that refers to
  @file{\"file:///foo/bar.txt\"} will yield @file{\"bar.txt\"}.
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-short-name)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_uri_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_uri_display" recent-info-uri-display)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2025-02-24}
  @argument[info]{a @class{gtk:recent-info} instance}
  @begin{return}
    The UTF-8 string containing the URI of the resource or @code{nil}.
  @end{return}
  @begin{short}
    Gets a displayable version of the URI of the resource.
  @end{short}
  If the resource is local, it returns a local path. If the resource is not
  local, it returns the UTF-8 encoded content of the function
  @fun{gtk:recent-info-uri}.
  @see-class{gtk:recent-info}
  @see-function{gtk:recent-info-uri}"
  (info (g:boxed recent-info)))

(export 'recent-info-uri-display)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_get_age
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_get_age" recent-info-age) :int
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @begin{return}
    The positive integer containing the number of days elapsed since the time
    this resource was last modified.
  @end{return}
  @begin{short}
    Gets the number of days elapsed since the last update of the resource
    pointed by info.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-age)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_is_local
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_is_local" recent-info-is-local) :boolean
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{@em{True} if the resource is local.}
  @begin{short}
    Checks whether the resource is local or not by looking at the scheme of its
    URI.
  @end{short}
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-is-local)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_exists
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_exists" recent-info-exists) :boolean
 #+liber-documentation
 "@version{#2025-02-24}
  @argument[info]{a @class{gtk:recent-info} instance}
  @return{@em{True} if the resource exists.}
  @begin{short}
    Checks whether the resource pointed by @arg{info} still exists.
  @end{short}
  At the moment this check is done only on resources pointing to local files.
  @see-class{gtk:recent-info}"
  (info (g:boxed recent-info)))

(export 'recent-info-exists)

;;; ----------------------------------------------------------------------------
;;; gtk_recent_info_match
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_recent_info_match" recent-info-match) :boolean
 #+liber-documentation
 "@version{#2025-01-11}
  @argument[info1]{a @class{gtk:recent-info}}
  @argument[info2]{a @class{gtk:recent-info}}
  @begin{return}
    @em{True} if both @class{gtk:recent-info} instances point to the same
    resource, @em{false} otherwise.
  @end{return}
  @begin{short}
    Checks whether two @class{gtk:recent-info} instances point to the same
    resource.
  @end{short}
  @see-class{gtk:recent-info}"
  (info1 (g:boxed recent-info))
  (info2 (g:boxed recent-info)))

(export 'recent-info-match)

;;; --- End of file gtk4.recent-manager.lisp -----------------------------------
