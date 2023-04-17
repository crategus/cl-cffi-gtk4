;;; ----------------------------------------------------------------------------
;;; gdk4.content-provider.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GdkContentProvider
;;;
;;; Functions
;;;
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
;;;
;;; Properties
;;;
;;;     formats
;;;     storable-formats
;;;
;;; Signals
;;;
;;;     content-changed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkContentProvider
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkContentProvider
;;;
;;; A GdkContentProvider is used to provide content for the clipboard in a
;;; number of formats.
;;;
;;; To create a GdkContentProvider, use gdk_content_provider_new_for_value() or
;;; gdk_content_provider_new_for_bytes().
;;;
;;; GDK knows how to handle common text and image formats out-of-the-box. See
;;; GdkContentSerializer and GdkContentDeserializer if you want to add support
;;; for application-specific data formats.
;;;
;;; Signal Details
;;;
;;; The “content-changed” signal
;;;
;;; void
;;; user_function (GdkContentProvider *gdkcontentprovider,
;;;                gpointer            user_data)
;;;
;;; Emitted whenever the content provided by this provider has changed.
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;;
;;; See Also
;;;     GdkContentSerializer, GdkContentDeserializer
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkContentProvider" content-provider
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_content_provider_get_type")
  ((formats
    content-provider-formats
    "formats" "GdkContentFormats" t nil)
   (storable-formats
    content-provider-storable-formats
    "storeable-formats" "GdkContentFormats" t nil)))

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The possible formats that the provider can provide its data in.
;;;
;;; Owner: GdkContentProvider
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “storable-formats” property
;;;
;;;  “storable-formats”         GdkContentFormats *
;;;
;;; The subset of formats that clipboard managers should store this provider's
;;; data in.
;;;
;;; Owner: GdkContentProvider
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_new_for_value ()
;;;
;;; GdkContentProvider *
;;; gdk_content_provider_new_for_value (const GValue *value);
;;;
;;; Create a content provider that provides the given value .
;;;
;;; value :
;;;     a GValue
;;;
;;; Returns :
;;;     a new GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_new_typed ()
;;;
;;; GdkContentProvider *
;;; gdk_content_provider_new_typed (GType type,
;;;                                 ...);
;;;
;;; Create a content provider that provides the value of the given type .
;;;
;;; The value is provided using G_VALUE_COLLECT(), so the same rules apply as
;;; when calling g_object_new() or g_object_set().
;;;
;;; type :
;;;     Type of value to follow
;;;
;;; ... :
;;;     value
;;;
;;; Returns :
;;;     a new GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_new_for_bytes ()
;;;
;;; GdkContentProvider *
;;; gdk_content_provider_new_for_bytes (const char *mime_type,
;;;                                     GBytes *bytes);
;;;
;;; Create a content provider that provides the given bytes as data for the
;;; given mime_type .
;;;
;;; mime_type :
;;;     the mime type
;;;
;;; bytes :
;;;     a GBytes with the data for mime_type .
;;;
;;; Returns :
;;;     a new GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_new_union ()
;;;
;;; GdkContentProvider *
;;; gdk_content_provider_new_union (GdkContentProvider **providers,
;;;                                 gsize n_providers);
;;;
;;; Creates a content provider that represents all the given providers .
;;;
;;; Whenever data needs to be written, the union provider will try the given
;;; providers in the given order and the first one supporting a format will be
;;; chosen to provide it.
;;;
;;; This allows an easy way to support providing data in different formats. For
;;; example, an image may be provided by its file and by the image contents with
;;; a call such as
;;;
;;; gdk_content_provider_new_union ((GdkContentProvider *[2])
;;;    {
;;;     gdk_content_provider_new_typed
;;;                    (G_TYPE_FILE, file),
;;;                     gdk_content_provider_new_typed (G_TYPE_TEXTURE, texture)
;;;    }, 2);
;;;
;;; providers :
;;;     The GdkContentProviders to present the union of.
;;;
;;; n_providers :
;;;     the number of providers
;;;
;;; Returns :
;;;     a new GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_ref_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_content_provider_ref_formats (GdkContentProvider *provider);
;;;
;;; Gets the formats that the provider can provide its current contents in.
;;;
;;; provider :
;;;     a GdkContentProvider
;;;
;;; Returns :
;;;     The formats of the provider.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_ref_storable_formats ()
;;;
;;; GdkContentFormats *
;;; gdk_content_provider_ref_storable_formats
;;;                                (GdkContentProvider *provider);
;;;
;;; Gets the formats that the provider suggests other applications to store the
;;; data in. An example of such an application would be a clipboard manager.
;;;
;;; This can be assumed to be a subset of gdk_content_provider_ref_formats().
;;;
;;; provider :
;;;     a GdkContentProvider
;;;
;;; Returns :
;;;     The storable formats of the provider.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_content_changed ()
;;;
;;; void
;;; gdk_content_provider_content_changed (GdkContentProvider *provider);
;;;
;;; Emits the “content-changed” signal.
;;;
;;; provider :
;;;     a GdkContentProvider
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_write_mime_type_async ()
;;;
;;; void
;;; gdk_content_provider_write_mime_type_async
;;;                                (GdkContentProvider *provider,
;;;                                 const char *mime_type,
;;;                                 GOutputStream *stream,
;;;                                 int io_priority,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Asynchronously writes the contents of provider to stream in the given
;;; mime_type . When the operation is finished callback will be called. You can
;;; then call gdk_content_provider_write_mime_type_finish() to get the result of
;;; the operation.
;;;
;;; The given mime type does not need to be listed in the formats returned by
;;; gdk_content_provider_ref_formats(). However, if the given GType is not
;;; supported, G_IO_ERROR_NOT_SUPPORTED will be reported.
;;;
;;; The given stream will not be closed.
;;;
;;; provider :
;;;     a GdkContentProvider
;;;
;;; mime_type :
;;;     the mime type to provide the data in
;;;
;;; stream :
;;;     the GOutputStream to write to
;;;
;;; io_priority :
;;;     the I/O priority of the request.
;;;
;;; cancellable :
;;;     optional GCancellable object, NULL to ignore.
;;;
;;; callback :
;;;     callback to call when the request is satisfied.
;;;
;;; user_data :
;;;     the data to pass to callback function.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_write_mime_type_finish ()
;;;
;;; gboolean
;;; gdk_content_provider_write_mime_type_finish
;;;                                (GdkContentProvider *provider,
;;;                                 GAsyncResult *result,
;;;                                 GError **error);
;;;
;;; Finishes an asynchronous write operation started with
;;; gdk_content_provider_write_mime_type_async().
;;;
;;; provider :
;;;     a GdkContentProvider
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     TRUE if the operation was completed successfully. Otherwise error will
;;;     be set to describe the failure.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_provider_get_value ()
;;;
;;; gboolean
;;; gdk_content_provider_get_value (GdkContentProvider *provider,
;;;                                 GValue *value,
;;;                                 GError **error);
;;;
;;; Gets the contents of provider stored in value .
;;;
;;; The value will have been initialized to the GType the value should be
;;; provided in. This given GType does not need to be listed in the formats
;;; returned by gdk_content_provider_ref_formats(). However, if the given GType
;;; is not supported, this operation can fail and G_IO_ERROR_NOT_SUPPORTED will
;;; be reported.
;;;
;;; provider :
;;;     a GdkContentProvider
;;;
;;; value :
;;;     the GValue to fill
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     TRUE if the value was set successfully. Otherwise error will be set to
;;;     describe the failure.
;;; ----------------------------------------------------------------------------

;;; --- gdk4.content-provider.lisp ---------------------------------------------
