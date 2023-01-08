;;; ----------------------------------------------------------------------------
;;; gtk.file-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkFileFilter
;;;
;;;     Filtering files
;;;
;;; Types and Values
;;;
;;;     GtkFileFilter
;;;
;;; Accessors
;;;
;;;     gtk_file_filter_set_name
;;;     gtk_file_filter_get_name
;;;
;;; Functions
;;;
;;;     gtk_file_filter_new
;;;     gtk_file_filter_add_mime_type
;;;     gtk_file_filter_add_pattern
;;;     gtk_file_filter_add_pixbuf_formats
;;;     gtk_file_filter_add_suffix                         Since 4.4
;;;     gtk_file_filter_get_attributes
;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_to_gvariant
;;;
;;; Properties
;;;
;;;     name
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkFileFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilter
;;;
;;; A GtkFileFilter can be used to restrict the files being shown in a
;;; GtkFileChooser. Files can be filtered based on their name (with
;;; gtk_file_filter_add_pattern()) or on their mime type (with
;;; gtk_file_filter_add_mime_type()).
;;;
;;; Filtering by mime types handles aliasing and subclassing of mime types; e.g.
;;; a filter for text/plain also matches a file with mime type application/rtf,
;;; since application/rtf is a subclass of text/plain. Note that GtkFileFilter
;;; allows wildcards for the subtype of a mime type, so you can e.g. filter for
;;; image/*.
;;;
;;; Normally, file filters are used by adding them to a GtkFileChooser (see
;;; gtk_file_chooser_add_filter()), but it is also possible to manually use a
;;; file filter on any GtkFilterListModel containing GFileInfo objects.
;;;
;;; GtkFileFilter as GtkBuildable
;;;
;;; The GtkFileFilter implementation of the GtkBuildable interface supports
;;; adding rules using the <mime-types> and <patterns> elements and listing the
;;; rules within. Specifying a <mime-type> or <pattern> has the same effect as
;;; as calling gtk_file_filter_add_mime_type() or gtk_file_filter_add_pattern().
;;;
;;; An example of a UI definition fragment specifying GtkFileFilter rules:
;;;
;;; <object class="GtkFileFilter">
;;;   <property name="name" translatable="yes">Text and Images</property>
;;;   <mime-types>
;;;     <mime-type>text/plain</mime-type>
;;;     <mime-type>image/ *</mime-type>
;;;   </mime-types>
;;;   <patterns>
;;;     <pattern>*.txt</pattern>
;;;   </patterns>
;;;   <suffixes>
;;;     <suffix>png</suffix>
;;;   </suffixes>
;;; </object>
;;;
;;; See Also
;;;     GtkFileChooser
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFileFilter" file-filter
  (:superclass filter
   :export t
   :interfaces nil
   :type-initializer "gtk_file_filter_get_type")
  ((name
    file-filter-name
    "name" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “name” property
;;;
;;;  “name”                     char *
;;;
;;; The human-readable name of the filter.
;;;
;;; This is the string that will be displayed in the file selector user
;;; interface if there is a selectable list of filters.
;;;
;;; Owner: GtkFileFilter
;;;
;;; Flags: Read / Write
;;;
;;; Default value: NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new ()
;;;
;;; GtkFileFilter *
;;; gtk_file_filter_new (void);
;;;
;;; Creates a new GtkFileFilter with no rules added to it.
;;;
;;; Such a filter doesn’t accept any files, so is not particularly useful until
;;; you add rules with gtk_file_filter_add_mime_type(),
;;; gtk_file_filter_add_pattern(), or gtk_file_filter_add_pixbuf_formats().
;;;
;;; To create a filter that accepts any file, use:
;;;
;;; GtkFileFilter *filter = gtk_file_filter_new ();
;;; gtk_file_filter_add_pattern (filter, "*");
;;;
;;; Returns :
;;;     a new GtkFileFilter
;;; ----------------------------------------------------------------------------

(declaim (inline file-filter-new))

(defun file-filter-new ()
  (make-instance 'file-filter))

(export 'file-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_set_name ()
;;;
;;; void
;;; gtk_file_filter_set_name (GtkFileFilter *filter,
;;;                           const char *name);
;;;
;;; Sets a human-readable name of the filter; this is the string that will be
;;; displayed in the file chooser if there is a selectable list of filters.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; name :
;;;     the human-readable-name for the filter, or NULL to remove any existing
;;;     name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_name ()
;;;
;;; const char *
;;; gtk_file_filter_get_name (GtkFileFilter *filter);
;;;
;;; Gets the human-readable name for the filter. See gtk_file_filter_set_name().
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     The human-readable name of the filter, or NULL. This value is owned by
;;;     GTK and must not be modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_mime_type ()
;;;
;;; void
;;; gtk_file_filter_add_mime_type (GtkFileFilter *filter,
;;;                                const char *mime_type);
;;;
;;; Adds a rule allowing a given mime type to filter .
;;;
;;; filter :
;;;     A GtkFileFilter
;;;
;;; mime_type :
;;;     name of a MIME type
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_mime_type" file-filter-add-mime-type) :void
  (filter (g:object file-filter))
  (mime-type :string))

(export 'file-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;;
;;; void
;;; gtk_file_filter_add_pattern (GtkFileFilter *filter,
;;;                              const char *pattern);
;;;
;;; Adds a rule allowing a shell style glob to a filter.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; pattern :
;;;     a shell style glob
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pattern" file-filter-add-pattern) :void
  (filter (g:object file-filter))
  (pattern :string))

(export 'file-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;;
;;; void
;;; gtk_file_filter_add_pixbuf_formats (GtkFileFilter *filter);
;;;
;;; Adds a rule allowing image files in the formats supported by GdkPixbuf.
;;;
;;; This is equivalent to calling gtk_file_filter_add_mime_type() for all the
;;; supported mime types.
;;;
;;; filter :
;;;     a GtkFileFilter
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_add_pixbuf_formats" file-filter-add-pixbuf-formats)
    :void
  (filter (g:object file-filter)))

(export 'file-filter-add-pixbuf-formats)


;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_suffix
;;;
;;; void
;;; gtk_file_filter_add_suffix (GtkFileFilter* filter, const char* suffix)
;;;
;;; Adds a suffix match rule to a filter.
;;;
;;; This is similar to adding a match for the pattern “*.suffix“.
;;;
;;; In contrast to pattern matches, suffix matches are always case-insensitive.
;;;
;;; suffix :
;;;     Filename suffix to match. The data is owned by the caller of the
;;;     function. The value is a NUL terminated UTF-8 string.
;;;
;;; Since 4.4
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(defcfun ("gtk_file_filter_add_suffix" file-filter-add-suffix) :void
  (filter (g:object file-filter))
  (suffix :string))

#+gtk-4-4
(export 'file-filter-add-suffix)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_attributes ()
;;;
;;; const char **
;;; gtk_file_filter_get_attributes (GtkFileFilter *filter);
;;;
;;; Gets the attributes that need to be filled in for the GFileInfo passed to
;;; this filter.
;;;
;;; This function will not typically be used by applications; it is intended
;;; principally for use in the implementation of GtkFileChooser.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     the attributes.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_get_attributes" file-filter-attributes)
    (g:strv-t :free-from-foreign nil)
  (filter (g:object file-filter)))

(export 'file-filter-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new_from_gvariant ()
;;;
;;; GtkFileFilter *
;;; gtk_file_filter_new_from_gvariant (GVariant *variant);
;;;
;;; Deserialize a file filter from an a{sv} variant in the format produced by
;;; gtk_file_filter_to_gvariant().
;;;
;;; variant :
;;;     an a{sv} GVariant
;;;
;;; Returns :
;;;     a new GtkFileFilter object.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_new_from_gvariant" file-filter-new-from-gvariant)
    (g:object file-filter)
  (variant (:pointer (:struct g:variant))))

(export 'file-filter-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_to_gvariant ()
;;;
;;; GVariant *
;;; gtk_file_filter_to_gvariant (GtkFileFilter *filter);
;;;
;;; Serialize a file filter to an a{sv} variant.
;;;
;;; filter :
;;;     a GtkFileFilter
;;;
;;; Returns :
;;;     a new, floating, GVariant.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_file_filter_to_gvariant" file-filter-to-gvariant)
    (:pointer (:struct g:variant))
  (filter (g:object file-filter)))

(export 'file-filter-to-gvariant)

;;; --- End of file gtk.file-filter.lisp ---------------------------------------
