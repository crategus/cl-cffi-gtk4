;;; ----------------------------------------------------------------------------
;;; gdk.content-formats.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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
;;; Content Formats
;;;
;;;     Advertising and negotiating of content exchange formats
;;;
;;; Types and Values
;;;
;;;     GdkContentFormats
;;;     GdkContentFormatsBuilder
;;;
;;; Functions
;;;
;;;     gdk_intern_mime_type 
;;;     gdk_content_formats_new 
;;;     gdk_content_formats_new_for_gtype 
;;;     gdk_content_formats_ref 
;;;     gdk_content_formats_unref 
;;;     gdk_content_formats_print 
;;;     gdk_content_formats_to_string 
;;;     gdk_content_formats_get_gtypes 
;;;     gdk_content_formats_get_mime_types 
;;;     gdk_content_formats_union 
;;;     gdk_content_formats_match 
;;;     gdk_content_formats_match_gtype 
;;;     gdk_content_formats_match_mime_type 
;;;     gdk_content_formats_contain_gtype 
;;;     gdk_content_formats_contain_mime_type 
;;;     gdk_content_formats_union_serialize_gtypes 
;;;     gdk_content_formats_union_deserialize_gtypes 
;;;     gdk_content_formats_union_serialize_mime_types 
;;;     gdk_content_formats_union_deserialize_mime_types 
;;;     gdk_content_formats_builder_new 
;;;     gdk_content_formats_builder_free_to_formats 
;;;     gdk_content_formats_builder_add_formats 
;;;     gdk_content_formats_builder_add_gtype 
;;;     gdk_content_formats_builder_add_mime_type 
;;;     gdk_content_formats_builder_ref 
;;;     gdk_content_formats_builder_unref 
;;;     gdk_content_formats_builder_to_formats 
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkContentFormats
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkContentFormatsBuilder
;;;
;;; typedef struct _GdkContentFormatsBuilder GdkContentFormatsBuilder;
;;;
;;; A GdkContentFormatsBuilder struct is an opaque struct. It is meant to not 
;;; be kept around and only be used to create new GdkContentFormats objects.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkContentFormats 
;;;
;;; typedef struct _GdkContentFormats GdkContentFormats;
;;;
;;; A GdkContentFormats struct is a reference counted struct and should be 
;;; treated as opaque.
;;;
;;; This section describes the GdkContentFormats structure that is used to 
;;; advertise and negotiate the format of content passed between different 
;;; widgets, windows or applications using for example the clipboard or 
;;; drag'n'drop.
;;;
;;; GDK supports content in 2 forms: GType and mime type. Using GTypes is meant 
;;; only for in-process content transfers. Mime types are meant to be used for 
;;; data passing both in-process and out-of-process. The details of how data is 
;;; passed is described in the documentation of the actual implementations.
;;;
;;; A GdkContentFormats describes a set of possible formats content can be 
;;; exchanged in. It is assumed that this set is ordered. GTypes are more 
;;; important than mime types. Order between different GTypes or mime types is 
;;; the order they were added in, most important first. Functions that care 
;;; about order, such as gdk_content_formats_union() will describe in their 
;;; documentation how they interpret that order, though in general the order of 
;;; the first argument is considered the primary order of the result, followed 
;;; by the order of further arguments.
;;;
;;; For debugging purposes, the function gdk_content_formats_to_string() exists. 
;;; It will print a comma-seperated formats of formats from most important to 
;;; least important.
;;;
;;; GdkContentFormats is an immutable struct. After creation, you cannot change 
;;; the types it represents. Instead, new GdkContentFormats have to be created. 
;;; The GdkContentFormatsBuilder structure is meant to help in this endeavor.
;;;
;;; See Also
;;;
;;;     GdkDrag, GdkDrop, GdkClipboard, GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_intern_mime_type ()
;;;
;;; const char *
;;; gdk_intern_mime_type (const char *string);
;;;
;;; Canonicalizes the given mime type and interns the result.
;;;
;;; If string is not a valid mime type, NULL is returned instead. See RFC 2048 
;;; for the syntax of mime types.
;;;
;;; string :
;;;     string of a potential mime type.
;;;
;;; Returns :
;;;     An interned string for the canonicalized mime type or NULL if the string 
;;;     wasn't a valid mime type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_new ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_new (const char **mime_types,
;;;                          guint n_mime_types);
;;;
;;; Creates a new GdkContentFormats from an array of mime types.
;;;
;;; The mime types must be valid and different from each other or the behavior 
;;; of the return value is undefined. If you cannot guarantee this, use 
;;; GdkContentFormatsBuilder instead.
;;;
;;; mime_types :
;;;     Pointer to an array of mime types.
;;;
;;; n_mime_types :
;;;     number of entries in mime_types .
;;;
;;; Returns :
;;;     the new GdkContentFormats.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_new_for_gtype ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_new_for_gtype (GType type);
;;;
;;; Creates a new GdkContentFormats for a given GType.
;;;
;;; type :
;;;     a $GType
;;;
;;; Returns :
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_ref () 
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_ref (GdkContentFormats *formats);
;;;
;;; Increases the reference count of a GdkContentFormats by one.
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; Returns :
;;;     the passed in GdkContentFormats.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_unref ()
;;;
;;; void
;;; gdk_content_formats_unref (GdkContentFormats *formats);
;;;
;;; Decreases the reference count of a GdkContentFormats by one. If the 
;;; resulting reference count is zero, frees the formats.
;;;
;;; formats :
;;;     a GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_print ()
;;;
;;; void
;;; gdk_content_formats_print (GdkContentFormats *formats,
;;;                            GString *string);
;;;
;;; Prints the given formats into a string for human consumption. This is meant 
;;; for debugging and logging.
;;;
;;; The form of the representation may change at any time and is not guaranteed 
;;; to stay identical.
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; string :
;;;     a GString to print into
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_to_string ()
;;;
;;; char *
;;; gdk_content_formats_to_string (GdkContentFormats *formats);
;;;
;;; Prints the given formats into a human-readable string. This is a small 
;;; wrapper around gdk_content_formats_print() to help when debugging.
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; Returns :
;;;     a new string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_get_gtypes ()
;;;
;;; const GType *
;;; gdk_content_formats_get_gtypes (const GdkContentFormats *formats,
;;;                                 gsize *n_gtypes);
;;;
;;; Gets the GTypes included in formats . Note that formats may not contain any 
;;; GTypes, in particular when they are empty. In that case NULL will be 
;;; returned.
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; n_gtypes :
;;;     optional pointer to take the number of GTypes contained in the return 
;;;     value.
;;;
;;; Returns :
;;;     G_TYPE_INVALID-terminated array of types included in formats or NULL if 
;;;     none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_get_mime_types ()
;;;
;;; const char * const *
;;; gdk_content_formats_get_mime_types (const GdkContentFormats *formats,
;;;                                     gsize *n_mime_types);
;;;
;;; Gets the mime types included in formats . Note that formats may not contain 
;;; any mime types, in particular when they are empty. In that case NULL will be 
;;; returned.
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; n_mime_types :
;;;     optional pointer to take the number of mime types contained in the 
;;;     return value.
;;;
;;; Returns :
;;;     NULL-terminated array of interned strings of mime types included in 
;;;     formats or NULL if none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_union (GdkContentFormats *first,
;;;                            const GdkContentFormats *second);
;;;
;;; Append all missing types from second to first , in the order they had in 
;;; second .
;;;
;;; first :
;;;     the GdkContentFormats to merge into.
;;;
;;; second :
;;;     the GdkContentFormats to merge from.
;;;
;;; Returns :
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match ()
;;;
;;; gboolean
;;; gdk_content_formats_match (const GdkContentFormats *first,
;;;                            const GdkContentFormats *second);
;;;
;;; Checks if first and second have any matching formats.
;;;
;;; first :
;;;     the primary GdkContentFormats to intersect
;;;
;;; second :
;;;     the GdkContentFormats to intersect with
;;;
;;; Returns :
;;;     TRUE if a matching format was found.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match_gtype ()
;;;
;;; GType
;;; gdk_content_formats_match_gtype (const GdkContentFormats *first,
;;;                                  const GdkContentFormats *second);
;;;
;;; Finds the first GType from first that is also contained in second . If no 
;;; matching GType is found, G_TYPE_INVALID is returned.
;;;
;;; first :
;;;     the primary GdkContentFormats to intersect
;;;
;;; second :
;;;     the GdkContentFormats to intersect with
;;;
;;; Returns :
;;;     The first common GType or G_TYPE_INVALID if none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match_mime_type ()
;;;
;;; const char *
;;; gdk_content_formats_match_mime_type (const GdkContentFormats *first,
;;;                                      const GdkContentFormats *second);
;;;
;;; Finds the first mime type from first that is also contained in second . If 
;;; no matching mime type is found, NULL is returned.
;;;
;;; first :
;;;     the primary GdkContentFormats to intersect
;;;
;;; second :
;;;     the GdkContentFormats to intersect with
;;;
;;; Returns :
;;;     The first common mime type or NULL if none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_contain_gtype () 
;;;
;;; gboolean
;;; gdk_content_formats_contain_gtype (const GdkContentFormats *formats,
;;;                                    GType type);
;;;
;;; Checks if a given GType is part of the given formats .
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; type :
;;;     the GType to search for
;;;
;;; Returns :
;;;     TRUE if the GType was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_contain_mime_type ()
;;;
;;; gboolean
;;; gdk_content_formats_contain_mime_type (const GdkContentFormats *formats,
;;;                                        const char *mime_type);
;;;
;;; Checks if a given mime type is part of the given formats .
;;;
;;; formats :
;;;     a GdkContentFormats
;;;
;;; mime_type :
;;;     the mime type to search for
;;;
;;; Returns :
;;;     TRUE if the mime_type was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_serialize_gtypes ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_union_serialize_gtypes
;;;                                (GdkContentFormats *formats);
;;;
;;; Add GTypes for the mime types in formats for which serializers are 
;;; registered.
;;;
;;; formats :
;;;     a GdkContentFormats.
;;;
;;; Return : 
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_deserialize_gtypes ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_union_deserialize_gtypes
;;;                                (GdkContentFormats *formats);
;;;
;;; Add GTypes for mime types in formats for which deserializers are registered.
;;;
;;; formats :
;;;     a GdkContentFormats.
;;;
;;; Return : 
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_serialize_mime_types ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_union_serialize_mime_types
;;;                                (GdkContentFormats *formats);
;;;
;;; Add mime types for GTypes in formats for which serializers are registered.
;;;
;;; formats :
;;;     a GdkContentFormats.
;;;
;;; Return : 
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_deserialize_mime_types ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_union_deserialize_mime_types
;;;                                (GdkContentFormats *formats);
;;;
;;; Add mime types for GTypes in formats for which deserializers are registered.
;;;
;;; formats :
;;;     a GdkContentFormats.
;;;
;;; Return : 
;;;     a new GdkContentFormats
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_new ()
;;;
;;; GdkContentFormatsBuilder *
;;; gdk_content_formats_builder_new (void);
;;;
;;; Create a new GdkContentFormatsBuilder object. The resulting builder would 
;;; create an empty GdkContentFormats. Use addition functions to add types to 
;;; it.
;;;
;;; Returns :
;;;     a new GdkContentFormatsBuilder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_free_to_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_builder_free_to_formats
;;;                                (GdkContentFormatsBuilder *builder);
;;;
;;; Creates a new GdkContentFormats from the current state of the given 
;;; builder , and frees the builder instance.
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; Returns :
;;;     the newly created GdkContentFormats with all the formats added to 
;;;     builder .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_formats ()
;;;
;;; void
;;; gdk_content_formats_builder_add_formats
;;;                                (GdkContentFormatsBuilder *builder,
;;;                                 const GdkContentFormats *formats);
;;;
;;; Appends all formats from formats to builder , skipping those that already 
;;; exist.
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; formats :
;;;     the formats to add
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_gtype ()
;;;
;;; void
;;; gdk_content_formats_builder_add_gtype (GdkContentFormatsBuilder *builder,
;;;                                        GType type);
;;;
;;; Appends gtype to builder if it has not already been added.
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; type :
;;;     a GType
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_add_mime_type ()
;;;
;;; void
;;; gdk_content_formats_builder_add_mime_type
;;;                                (GdkContentFormatsBuilder *builder,
;;;                                 const char *mime_type);
;;;
;;; Appends mime_type to builder if it has not already been added.
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; mime_type :
;;;     a mime type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_ref ()
;;;
;;; GdkContentFormatsBuilder *
;;; gdk_content_formats_builder_ref (GdkContentFormatsBuilder *builder);
;;;
;;; Acquires a reference on the given builder .
;;;
;;; This function is intended primarily for bindings. GdkContentFormatsBuilder 
;;; objects should not be kept around.
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; Returns :
;;;     the given GdkContentFormatsBuilder with its reference count increased.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_unref ()
;;;
;;; void
;;; gdk_content_formats_builder_unref (GdkContentFormatsBuilder *builder);
;;;
;;; Releases a reference on the given builder .
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_builder_to_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_content_formats_builder_to_formats
;;;                                (GdkContentFormatsBuilder *builder);
;;;
;;; Creates a new GdkContentFormats from the given builder .
;;;
;;; The given GdkContentFormatsBuilder is reset once this function returns; you 
;;; cannot call this function multiple times on the same builder instance.
;;;
;;; This function is intended primarily for bindings. C code should use 
;;; gdk_content_formats_builder_free_to_formats().
;;;
;;; builder :
;;;     a GdkContentFormatsBuilder
;;;
;;; Returns :
;;;     the newly created GdkContentFormats with all the formats added to 
;;;     builder .
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.content-formats.lisp -----------------------------------
