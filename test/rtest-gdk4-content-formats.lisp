(in-package :gtk-test)

(def-suite gdk-content-formats :in gdk-suite)
(in-suite gdk-content-formats)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkFileList                                         Since 4.6

;;;     GdkContentFormats

(test gdk-content-formats-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkContentFormats"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkContentFormats")
          (g:gtype (cffi:foreign-funcall "gdk_content_formats_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:content-formats
          (glib:symbol-for-gtype "GdkContentFormats"))))

;;;     GdkContentFormatsBuilder                            not implemented

;;; --- Functions --------------------------------------------------------------

;;;     gdk_file_list_new_from_list                         Since 4.8
;;;     gdk_file_list_new_from_array                        Since 4.8
;;;     gdk_file_list_get_files                             Since 4.6

;;;     gdk_intern_mime_type

;;;     gdk_content_formats_new
;;;     gdk_content_formats_new_for_gtype
;;;     gdk_content_formats_ref                             not implemented
;;;     gdk_content_formats_unref                           not needed
;;;     gdk_content_formats_print                           not needed
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
;;;     gdk_content_formats_parse                           Since 4.4

;;;     gdk_content_formats_builder_new
;;;     gdk_content_formats_builder_free_to_formats
;;;     gdk_content_formats_builder_add_formats
;;;     gdk_content_formats_builder_add_gtype
;;;     gdk_content_formats_builder_add_mime_type
;;;     gdk_content_formats_builder_ref
;;;     gdk_content_formats_builder_unref
;;;     gdk_content_formats_builder_to_formats

;;; 2024-11-29
