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
  (if *first-run-testsuite*
      (is (equal '()
                 (glib-test:list-children "GdkContentProvider")))
      (is (equal '("GdkContentProviderValue" "GtkLabelContent" "GtkLinkContent"
                   "GtkTextBufferContent" "GtkTextContent")
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

(test gdk-content-provider-content-changed-signal
  (let* ((name "content-changed")
         (gtype (g:gtype "GdkContentProvider"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     formats
;;;     storable-formats

;; FIXME: What is wrong?!
;; Unexpected Error: #<SIMPLE-ERROR "CLASS-PROPERTY-TYPE:
;; Property ~a not registered for ~a object" {1002247213}>
;; CLASS-PROPERTY-TYPE: Property storeable-formats not registered for
;; #<GTYPE :name "GdkContentProviderValue" :id 100263820346704> object.

(test gdk-content-provider-properties
  (glib-test:with-check-memory (provider)
    (is (typep (setf provider
                     (make-instance 'gdk:content-provider))
               'gdk:content-provider))
    (is (typep (gdk:content-provider-formats provider) 'gdk:content-formats))
;   (is-false (gdk:content-provider-storable-formats provider))
))

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

;;; 2025-11-16
