(in-package :gtk-test)

(def-suite gdk-content-deserializer :in gdk-suite)
(in-suite gdk-content-deserializer)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkContentDeserializer

(test gdk-content-deserializer-class
  ;; Check type
  (is (g:type-is-object "GdkContentDeserializer"))
  ;; Check registered name
  (is (eq 'gdk:content-deserializer
          (glib:symbol-for-gtype "GdkContentDeserializer")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkContentDeserializer")
          (g:gtype (cffi:foreign-funcall "gdk_content_deserializer_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkContentDeserializer")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkContentDeserializer")))
  ;; Check interfaces
  (is (equal '("GAsyncResult")
             (glib-test:list-interfaces "GdkContentDeserializer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkContentDeserializer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkContentDeserializer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkContentDeserializer"
                       GDK:CONTENT-DESERIALIZER
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES ("GAsyncResult")
                        :TYPE-INITIALIZER "gdk_content_deserializer_get_type")
                       NIL)
             (gobject:get-gtype-definition "GdkContentDeserializer"))))

;;; --- Functions --------------------------------------------------------------

;;;     GdkContentDeserializeFunc

;;;     gdk_content_deserializer_get_mime_type
;;;     gdk_content_deserializer_get_gtype
;;;     gdk_content_deserializer_get_value
;;;     gdk_content_deserializer_get_input_stream
;;;     gdk_content_deserializer_get_priority
;;;     gdk_content_deserializer_get_cancellable
;;;     gdk_content_deserializer_get_user_data
;;;     gdk_content_deserializer_set_task_data
;;;     gdk_content_deserializer_get_task_data

;;;     gdk_content_deserializer_return_success
;;;     gdk_content_deserializer_return_error

;;;     gdk_content_register_deserializer
;;;     gdk_content_deserialize_async
;;;     gdk_content_deserialize_finish

;;; 2024-11-29
