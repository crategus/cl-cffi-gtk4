;;; ----------------------------------------------------------------------------
;;; gdk.content-serializer.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GDK library.
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
;;;
;;; Should not be accessed directly.
;;;
;;; A GdkContentSerializer is used to serialize content for inter-application 
;;; data transfers.
;;;
;;; See Also
;;;
;;;     GdkContentDeserializer, GdkContentProvider
;;; ----------------------------------------------------------------------------

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

;;; --- End of file gdk.content-serializer.lisp --------------------------------
