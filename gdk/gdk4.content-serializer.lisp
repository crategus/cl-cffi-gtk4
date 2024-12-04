;;; ----------------------------------------------------------------------------
;;; gdk4.content-serializer.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;; GdkContentSerializer
;;;
;;;     Serialize content for transfer
;;;
;;; Types and Values
;;;
;;;     GdkContentSerializer
;;;
;;; Functions
;;;
;;;     GdkContentSerializeFunc
;;;
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
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkContentSerializer
;;;
;;; Implemented Interfaces
;;;
;;;     GAsyncResult
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkContentSerializer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkContentSerializer" content-serializer
  (:superclass g:object
   :export t
   :interfaces ("GAsyncResult")
   :type-initializer "gdk_content_serializer_get_type")
  nil)

#+liber-documentation
(setf (documentation 'content-serializer 'type)
 "@version{2024-12-2}
  @begin{short}
    The @class{gdk:content-serializer} object is used to serialize content for
    inter-application data transfers.
  @end{short}
  The @class{gdk:content-serializer} object transforms an object that is
  identified by a @class{g:type-t} type ID into a serialized form, that is a
  byte stream, that is identified by a mime type.

  GTK provides serializers and deserializers for common data types such as text,
  colors, images or file lists. To register your own serialization functions,
  use the @fun{gdk:content-register-serializer} function.

  Also see the @class{gdk:content-deserializer} object.
  @see-class{gdk:content-deserializer}
  @see-class{gdk:content-provider}")

;;; ----------------------------------------------------------------------------
;;; GdkContentSerializeFunc ()
;;;
;;; void
;;; (*GdkContentSerializeFunc) (GdkContentSerializer *serializer);
;;;
;;; The type of a function that can be registered with
;;; gdk_content_register_serializer(). When the function gets called to operate
;;; on content, it can call functions on the serializer object to obtain the
;;; mime type, output stream, user data, etc. for its operation.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_mime_type ()
;;;
;;; const char *
;;; gdk_content_serializer_get_mime_type (GdkContentSerializer *serializer);
;;;
;;; Gets the mime type to serialize to.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the mime type for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_gtype ()
;;;
;;; GType
;;; gdk_content_serializer_get_gtype (GdkContentSerializer *serializer);
;;;
;;; Gets the GType to of the object to serialize.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the GType for the current operation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_value ()
;;;
;;; const GValue *
;;; gdk_content_serializer_get_value (GdkContentSerializer *serializer);
;;;
;;; Gets the GValue to read the object to serialize from.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the GValue for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_output_stream ()
;;;
;;; GOutputStream *
;;; gdk_content_serializer_get_output_stream
;;;                                (GdkContentSerializer *serializer);
;;;
;;; Gets the output stream that was passed to gdk_content_serialize_async().
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the output stream for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_priority ()
;;;
;;; int
;;; gdk_content_serializer_get_priority (GdkContentSerializer *serializer);
;;;
;;; Gets the io priority that was passed to gdk_content_serialize_async().
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the io priority for the current operation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_cancellable ()
;;;
;;; GCancellable *
;;; gdk_content_serializer_get_cancellable
;;;                                (GdkContentSerializer *serializer);
;;;
;;; Gets the cancellable that was passed to gdk_content_serialize_async().
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the cancellable for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_user_data ()
;;;
;;; gpointer
;;; gdk_content_serializer_get_user_data (GdkContentSerializer *serializer);
;;;
;;; Gets the user data that was passed when the serializer was registered.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the user data for this serializer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_set_task_data ()
;;;
;;; void
;;; gdk_content_serializer_set_task_data (GdkContentSerializer *serializer,
;;;                                       gpointer data,
;;;                                       GDestroyNotify notify);
;;;
;;; Associate data with the current serialization operation.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; data :
;;;     data to associate with this operation
;;;
;;; notify :
;;;     destroy notify for data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_get_task_data ()
;;;
;;; gpointer
;;; gdk_content_serializer_get_task_data (GdkContentSerializer *serializer);
;;;
;;; Gets the data that was associated with serializer via
;;; gdk_content_serializer_set_task_data().
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; Returns :
;;;     the task data for serializer .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_return_success ()
;;;
;;; void
;;; gdk_content_serializer_return_success (GdkContentSerializer *serializer);
;;;
;;; Indicate that the serialization has been successfully completed.
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serializer_return_error ()
;;;
;;; void
;;; gdk_content_serializer_return_error (GdkContentSerializer *serializer,
;;;                                      GError *error);
;;;
;;; Indicate that the serialization has ended with an error. This function
;;; consumes error .
;;;
;;; serializer :
;;;     a GdkContentSerializer
;;;
;;; error :
;;;     a GError
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_register_serializer ()
;;;
;;; void
;;; gdk_content_register_serializer (GType type,
;;;                                  const char *mime_type,
;;;                                  GdkContentSerializeFunc serialize,
;;;                                  gpointer data,
;;;                                  GDestroyNotify notify);
;;;
;;; Registers a function to convert objects of the given type to a serialized
;;; representation with the given mime type.
;;;
;;; type :
;;;     the type of objects that the function can serialize
;;;
;;; mime_type :
;;;     the mime type to serialize to
;;;
;;; serialize :
;;;     the callback
;;;
;;; data :
;;;     data that serialize can access
;;;
;;; notify :
;;;     destroy notify for data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serialize_async ()
;;;
;;; void
;;; gdk_content_serialize_async (GOutputStream *stream,
;;;                              const char *mime_type,
;;;                              const GValue *value,
;;;                              int io_priority,
;;;                              GCancellable *cancellable,
;;;                              GAsyncReadyCallback callback,
;;;                              gpointer user_data);
;;;
;;; Serialize content and write it to the given output stream, asynchronously.
;;; When the operation is finished, callback will be called. You can then call
;;; gdk_content_serialize_finish() to get the result of the operation.
;;;
;;; stream :
;;;     a GOutputStream to write the serialized content to
;;;
;;; mime_type :
;;;     the mime type to serialize to
;;;
;;; value :
;;;     the content to serialize
;;;
;;; io_priority :
;;;     the io priority of the operation
;;;
;;; cancellable :
;;;     optional GCancellable object.
;;;
;;; callback :
;;;     callback to call when the operation is done.
;;;
;;; user_data :
;;;     data to pass to the callback function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_serialize_finish ()
;;;
;;; gboolean
;;; gdk_content_serialize_finish (GAsyncResult *result,
;;;                               GError **error);
;;;
;;; Finishes a content serialization operation.
;;;
;;; result :
;;;     the GAsyncResult
;;;
;;; error :
;;;     return location for an error
;;;
;;; Returns :
;;;     TRUE if the operation was successful, FALSE if an error occurred. In
;;;     this case, error is set
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk4.content-serializer.lisp -------------------------------
