(in-package :gtk-test)

(def-suite gdk-content-serializer :in gdk-suite)
(in-suite gdk-content-serializer)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkContentSerializer

(test gdk-content-serializer-class
  ;; Check type
  (is (g:type-is-object "GdkContentSerializer"))
  ;; Check registered name
  (is (eq 'gdk:content-serializer
          (glib:symbol-for-gtype "GdkContentSerializer")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkContentSerializer")
          (g:gtype (cffi:foreign-funcall "gdk_content_serializer_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkContentSerializer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkContentSerializer")))
  ;; Check interfaces
  (is (equal '("GAsyncResult")
             (glib-test:list-interfaces "GdkContentSerializer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkContentSerializer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkContentSerializer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkContentSerializer"
                                      GDK:CONTENT-SERIALIZER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GAsyncResult")
                        :TYPE-INITIALIZER "gdk_content_serializer_get_type")
                       NIL)
             (gobject:get-gtype-definition "GdkContentSerializer"))))

;;; --- Functions --------------------------------------------------------------

;;;     GdkContentSerializeFunc

;;;     gdk_content_serializer_get_mime_type
;;;     gdk_content_serializer_get_gtype
;;;     gdk_content_serializer_get_value
;;;     gdk_content_serializer_get_output_stream
;;;     gdk_content_serializer_get_priority
;;;     gdk_content_serializer_get_cancellable
;;;     gdk_content_serializer_get_user_data
;;;     gdk_content_serializer_set_task_data
;;;     gdk_content_serializer_get_task_data
;;;     gdk_content_serializer_return_success
;;;     gdk_content_serializer_return_error
;;;     gdk_content_register_serializer
;;;     gdk_content_serialize_async
;;;     gdk_content_serialize_finish

;;; 2024-11-29
