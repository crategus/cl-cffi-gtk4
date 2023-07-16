;;; ----------------------------------------------------------------------------
;;; gdk.content-deserializer.lisp
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
;;; GdkContentDeserializer
;;;
;;;     Deserialize content for transfer
;;;
;;; Types and Values
;;;
;;;     GdkContentDeserializer
;;;
;;; Functions
;;;
;;;     GdkContentDeserializeFunc
;;;
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
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkContentDeserializer
;;;
;;; Implemented Interfaces
;;;
;;;     GAsyncResult
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkContentDeserializer
;;;
;;; Should not be accessed directly.
;;;
;;; A GdkContentDeserializer is used to deserialize content received via
;;; inter-application data transfers.
;;;
;;; See Also
;;;
;;;     GdkContentSerializer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkContentDeserializeFunc
;;;
;;; void
;;; (*GdkContentDeserializeFunc) (GdkContentDeserializer *deserializer);
;;;
;;; The type of a function that can be registered with
;;; gdk_content_register_deserializer(). When the function gets called to
;;; operate on content, it can call functions on the deserializer object to
;;; obtain the mime type, input stream, user data, etc. for its operation.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_mime_type ()
;;;
;;; const char *
;;; gdk_content_deserializer_get_mime_type
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Gets the mime type to deserialize from.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the mime type for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_gtype ()
;;;
;;; GType
;;; gdk_content_deserializer_get_gtype (GdkContentDeserializer *deserializer);
;;;
;;; Gets the GType to create an instance of.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the GType for the current operation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_value ()
;;;
;;; GValue *
;;; gdk_content_deserializer_get_value (GdkContentDeserializer *deserializer);
;;;
;;; Gets the GValue to store the deserialized object in.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the GValue for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_input_stream ()
;;;
;;; GInputStream *
;;; gdk_content_deserializer_get_input_stream
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Gets the input stream that was passed to gdk_content_deserialize_async().
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the input stream for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_priority ()
;;;
;;; int
;;; gdk_content_deserializer_get_priority (GdkContentDeserializer *deserializer)
;;;
;;; Gets the io priority that was passed to gdk_content_deserialize_async().
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the io priority for the current operation
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_cancellable ()
;;;
;;; GCancellable *
;;; gdk_content_deserializer_get_cancellable
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Gets the cancellable that was passed to gdk_content_deserialize_async().
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the cancellable for the current operation.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_user_data ()
;;;
;;; gpointer
;;; gdk_content_deserializer_get_user_data
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Gets the user data that was passed when the deserializer was registered.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the user data for this deserializer.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_set_task_data ()
;;;
;;; void
;;; gdk_content_deserializer_set_task_data
;;;                                (GdkContentDeserializer *deserializer,
;;;                                 gpointer data,
;;;                                 GDestroyNotify notify);
;;;
;;; Associate data with the current deserialization operation.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; data :
;;;     data to associate with this operation
;;;
;;; notify :
;;;     destroy notify for data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_get_task_data ()
;;;
;;; gpointer
;;; gdk_content_deserializer_get_task_data
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Gets the data that was associated with deserializer via
;;; gdk_content_deserializer_set_task_data().
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; Returns :
;;;     the task data for deserializer .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_return_success ()
;;;
;;; void
;;; gdk_content_deserializer_return_success
;;;                                (GdkContentDeserializer *deserializer);
;;;
;;; Indicate that the deserialization has been successfully completed.
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserializer_return_error ()
;;;
;;; void
;;; gdk_content_deserializer_return_error (GdkContentDeserializer *deserializer,
;;;                                        GError *error);
;;;
;;; Indicate that the deserialization has ended with an error. This function
;;; consumes error .
;;;
;;; deserializer :
;;;     a GdkContentDeserializer
;;;
;;; error :
;;;     a GError
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_register_deserializer ()
;;;
;;; void
;;; gdk_content_register_deserializer (const char *mime_type,
;;;                                    GType type,
;;;                                    GdkContentDeserializeFunc deserialize,
;;;                                    gpointer data,
;;;                                    GDestroyNotify notify);
;;;
;;; Registers a function to create objects of a given type from a serialized
;;; representation with the given mime type.
;;;
;;; mime_type :
;;;     the mime type which the function can deserialize from
;;;
;;; type :
;;;     the type of objects that the function creates
;;;
;;;  deserialize :
;;;     the callback
;;;
;;; data :
;;;     data that deserialize can access
;;;
;;; notify :
;;;     destroy notify for data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_deserialize_async ()
;;;
;;; void
;;; gdk_content_deserialize_async (GInputStream *stream,
;;;                                const char *mime_type,
;;;                                GType type,
;;;                                int io_priority,
;;;                                GCancellable *cancellable,
;;;                                GAsyncReadyCallback callback,
;;;                                gpointer user_data);
;;;
;;; Read content from the given input stream and deserialize it, asynchronously.
;;; When the operation is finished, callback will be called. You can then call
;;; gdk_content_deserialize_finish() to get the result of the operation.
;;;
;;; stream :
;;;     a GInputStream to read the serialized content from
;;;
;;; mime_type :
;;;     the mime type to deserialize from
;;;
;;; type :
;;;     the GType to deserialize from
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
;;; gdk_content_deserialize_finish ()
;;;
;;; gboolean
;;; gdk_content_deserialize_finish (GAsyncResult *result,
;;;                                 GValue *value,
;;;                                 GError **error);
;;;
;;; Finishes a content deserialization operation.
;;;
;;; result :
;;;     the GAsyncResult
;;;
;;; value :
;;;     return location for the result of the operation
;;;
;;; error :
;;;     return location for an error
;;;
;;; Returns :
;;;     TRUE if the operation was successful. In this case, value is set. FALSE
;;;     if an error occurred. In this case, error is set
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.content-deserializer.lisp ------------------------------
