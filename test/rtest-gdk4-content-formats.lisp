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

(test gdk-intern-mime-type
  (is (string= "text/plain" (gdk:intern-mime-type "TEXT/PLAIN")))
  (is (string= "text/" (gdk:intern-mime-type "TEXT/")))
  (is-false (gdk:intern-mime-type "TEXT")))

;;;     gdk_content_formats_new

(test gdk-content-formats-new.1
  (is (string= ""
               (gdk:content-formats-to-string
                   (gdk:content-formats-new '())))))

(test gdk-content-formats-new.2
  (is (string= "text/plain"
               (gdk:content-formats-to-string
                   (gdk:content-formats-new "text/plain")))))

(test gdk-content-formats-new.3
  (is (string= "text/plain"
               (gdk:content-formats-to-string
                   (gdk:content-formats-new '("text/plain"))))))

;;;     gdk_content_formats_new_for_gtype

(test gdk-content-formats-new-for-gtype
  (is (string= "GtkButton"
               (gdk:content-formats-to-string
                   (gdk:content-formats-new-for-gtype "GtkButton")))))

;;;     gdk_content_formats_ref                             not needed
;;;     gdk_content_formats_unref                           not needed
;;;     gdk_content_formats_print                           not needed

;;;     gdk_content_formats_parse                           Since 4.4

(test gdk-content-formats-parse
  (is (string= "text/plain image/png"
               (gdk:content-formats-to-string
                   (gdk:content-formats-parse "text/plain image/png")))))

;;;     gdk_content_formats_to_string

(test gdk-content-formats-to-string
  (is (string= "text/plain image/png"
               (gdk:content-formats-to-string
                   (gdk:content-formats-new '("text/plain" "image/png"))))))

;;;     gdk_content_formats_is_empty                        Since 4.18

(test gdk-content-formats-is-empty
  (is-false (gdk:content-formats-is-empty (gdk:content-formats-new "text/plain")))
  (is (gdk:content-formats-is-empty (gdk:content-formats-new nil))))

;;;     gdk_content_formats_get_gtypes

(test gdk-content-formats-gtypes
  (let ((formats (gdk:content-formats-new-for-gtype "GtkButton")))
    (is (equal (list (g:gtype "GtkButton"))
               (gdk:content-formats-gtypes formats)))))

;;;     gdk_content_formats_get_mime_types

(test gdk-content-formats-mime-types
  (let ((formats (gdk:content-formats-new '("text/plain" "image/png"))))
    (is (equal '("text/plain" "image/png")
               (gdk:content-formats-mime-types formats)))))

;;;     gdk_content_formats_union

(test gdk-content-formats-union
  (let ((first (gdk:content-formats-new "text/plain"))
        (second (gdk:content-formats-new "image/png")))
    (is (string= "text/plain image/png"
                 (gdk:content-formats-to-string
                     (gdk:content-formats-union first second))))))

;;;     gdk_content_formats_match

(test gdk-content-formats-match
  (let ((first (gdk:content-formats-new "text/plain"))
        (second (gdk:content-formats-new "image/png")))
    (is-false (gdk:content-formats-match first second))
    (is-true (gdk:content-formats-match first
                                        (gdk:content-formats-union first second)))))

;;;     gdk_content_formats_match_gtype

(test gdk-content-formats-match-gtype
  (let ((first (gdk:content-formats-new-for-gtype "GtkButton"))
        (second (gdk:content-formats-new-for-gtype "GtkLabel")))
    (is-false (gdk:content-formats-match-gtype first second))
    (is (eq (g:gtype "GtkButton")
            (gdk:content-formats-match-gtype first first)))
    (is (eq (g:gtype "GtkLabel")
            (gdk:content-formats-match-gtype second second)))))

;;;     gdk_content_formats_match_mime_type

(test gdk-content-formats-match-mime-type
  (let* ((first (gdk:content-formats-new "text/plain"))
         (second (gdk:content-formats-new "image/png"))
         (third (gdk:content-formats-union first second)))
    ;; FIXME: Why is this true?
    (is-true (gdk:content-formats-match-mime-type first second))
    (is (string= "text/plain"
                (gdk:content-formats-match-mime-type first third)))
    (is (string= "image/png"
                (gdk:content-formats-match-mime-type second third)))))

;;;     gdk_content_formats_contain_gtype

(test gdk-content-formats-contain-gtype
  (let ((formats (gdk:content-formats-new-for-gtype "GtkButton")))
    (is-true (gdk:content-formats-contain-gtype formats "GtkButton"))
    (is-false (gdk:content-formats-contain-gtype formats "GtkLabel"))))

;;;     gdk_content_formats_contain_mime_type

(test gdk-content-formats-contain-mime-type
  (let ((formats (gdk:content-formats-new "text/plain")))
    (is-true (gdk:content-formats-contain-mime-type formats "text/plain"))
    (is-false (gdk:content-formats-contain-mime-type formats "image/png"))))

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

;;; 2026-01-20
