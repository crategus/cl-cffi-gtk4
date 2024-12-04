(in-package :gtk-test)

(def-suite gdk-content-provider :in gdk-suite)
(in-suite gdk-content-provider)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkContentProvider

(test gdk-content-provider-class
  ;; Check type
  (is (g:type-is-object "GdkContentProvider"))
  ;; Check registered name
  (is (eq 'gdk:content-provider
          (glib:symbol-for-gtype "GdkContentProvider")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkContentProvider")
          (g:gtype (cffi:foreign-funcall "gdk_content_provider_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkContentProvider")))
  ;; Check children
  (if *first-run-gtk-test*
      (is (equal '()
                 (glib-test:list-children "GdkContentProvider")))
      (is (equal '("GdkContentProviderValue")
                 (glib-test:list-children "GdkContentProvider"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkContentProvider")))
  ;; Check properties
  (is (equal '("formats" "storable-formats")
             (glib-test:list-properties "GdkContentProvider")))
  ;; Check signals
  (is (equal '("content-changed")
             (glib-test:list-signals "GdkContentProvider")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkContentProvider" GDK:CONTENT-PROVIDER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_content_provider_get_type")
                       ((FORMATS CONTENT-PROVIDER-FORMATS "formats"
                         "GdkContentFormats" T NIL)
                        (STORABLE-FORMATS CONTENT-PROVIDER-STORABLE-FORMATS
                         "storable-formats" "GdkContentFormats" T NIL)))
             (gobject:get-gtype-definition "GdkContentProvider"))))

;;; --- Signals ----------------------------------------------------------------

;;;     content-changed

;;; --- Properties -------------------------------------------------------------

;;;     formats
;;;     storable-formats

;;; --- Functions --------------------------------------------------------------

;;;     gdk_content_provider_new_for_value
;;;     gdk_content_provider_new_typed
;;;     gdk_content_provider_new_for_bytes
;;;     gdk_content_provider_new_union
;;;     gdk_content_provider_ref_formats
;;;     gdk_content_provider_ref_storable_formats
;;;     gdk_content_provider_content_changed
;;;     gdk_content_provider_write_mime_type_async
;;;     gdk_content_provider_write_mime_type_finish
;;;     gdk_content_provider_get_value

;;; 2024-11-29
