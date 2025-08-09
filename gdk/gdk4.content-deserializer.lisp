;;; ----------------------------------------------------------------------------
;;; gdk4.content-deserializer.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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
;;;
;;;     gdk_content_deserializer_return_success
;;;     gdk_content_deserializer_return_error
;;;
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkContentDeserializer" content-deserializer
  (:superclass g:object
   :export t
   :interfaces ("GAsyncResult")
   :type-initializer "gdk_content_deserializer_get_type")
  nil)

#+liber-documentation
(setf (documentation 'content-deserializer 'type)
 "@version{2024-12-02}
  @begin{short}
    The @class{gdk:content-deserializer} object is used to deserialize content
    received via inter-application data transfers.
  @end{short}
  The @class{gdk:content-deserializer} object transforms serialized content that
  is identified by a mime type into an object identified by a @class{g:type-t}
  type ID.

  GTK provides serializers and deserializers for common data types such as text,
  colors, images or file lists. To register your own deserialization functions,
  use the @fun{gdk:content-register-deserializer} function.

  Also see the @class{gdk:content-serializer} object.
  @see-class{gdk:content-serializer}
  @see-class{gdk:content-provider}")

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

;;; --- End of file gdk4.content-deserializer.lisp -----------------------------
