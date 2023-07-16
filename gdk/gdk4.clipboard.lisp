;;; ----------------------------------------------------------------------------
;;; gdk.clipboard.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
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
;;; Clipboards
;;;
;;;     Share data between applications for Copy-and-Paste
;;;
;;; Types and Values
;;;
;;;     GdkClipboard
;;;
;;; Accessors
;;;
;;;     gdk_clipboard_get_content
;;;     gdk_clipboard_set_content
;;;     gdk_clipboard_get_display
;;;     gdk_clipboard_get_formats
;;;
;;; Functions
;;;
;;;     gdk_clipboard_is_local
;;;     gdk_clipboard_store_async
;;;     gdk_clipboard_store_finish
;;;     gdk_clipboard_read_async
;;;     gdk_clipboard_read_finish
;;;     gdk_clipboard_read_value_async
;;;     gdk_clipboard_read_value_finish
;;;     gdk_clipboard_read_texture_async
;;;     gdk_clipboard_read_texture_finish
;;;     gdk_clipboard_read_text_async
;;;     gdk_clipboard_read_text_finish
;;;     gdk_clipboard_set
;;;     gdk_clipboard_set_valist
;;;     gdk_clipboard_set_value
;;;     gdk_clipboard_set_text
;;;     gdk_clipboard_set_texture
;;;
;;; Properties
;;;
;;;     content
;;;     display
;;;     formats
;;;     local
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkClipboard
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkClipboard
;;;
;;; The GdkClipboard struct contains only private fields and should not be
;;; accessed directly.
;;;
;;; The GdkClipboard object represents a clipboard of data shared between
;;; different applications or between different parts of the same application.
;;;
;;; To get a GdkClipboard object, use gdk_display_get_clipboard() or
;;; gdk_display_get_primary_clipboard(). You can find out about the data that
;;; is currently available in a clipboard using gdk_clipboard_get_formats().
;;;
;;; To make text or image data available in a clipboard, use
;;; gdk_clipboard_set_text() or gdk_clipboard_set_texture(). For other data,
;;; you can use gdk_clipboard_set_content(), which takes a GdkContentProvider
;;; object.
;;;
;;; To read textual or image data from a clipboard, use
;;; gdk_clipboard_read_text_async() or gdk_clipboard_read_texture_async(). For
;;; other data, use gdk_clipboard_read_async(), which provides a GInputStream
;;; object.
;;;
;;; See Also
;;;     GdkContentProvider, GdkContentFormats
;;;
;;; Signal Details
;;;
;;; The “changed” signal
;;;
;;; void
;;; user_function (GdkClipboard *clipboard,
;;;                gpointer      user_data)
;;;
;;; The ::changed signal is emitted when the clipboard changes ownership.
;;;
;;; clipboard :
;;;     the object on which the signal was emitted
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkClipboard" clipboard
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_clipboard_get_type")
  ((content
    clipboard-content
    "content" "GdkContentProvider" t nil)
   (display
    clipboard-display
    "display" "GdkDisplay" t t)
   (formats
    clipboard-formats
    "formats" "GdkContentFormats" t nil)
   (local
    clipboard-local
    "local" "gboolean" t nil)))

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “content” property
;;;
;;;  “content”                  GdkContentProvider *
;;;
;;; The GdkContentProvider or NULL if the clipboard is empty or contents are
;;; provided otherwise.
;;;
;;; Owner: GdkClipboard
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “display” property
;;;
;;;  “display”                  GdkDisplay *
;;;
;;; The GdkDisplay that the clipboard belongs to.
;;;
;;; Owner: GdkClipboard
;;;
;;; Flags: Read / Write / Construct Only
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “formats” property
;;;
;;;  “formats”                  GdkContentFormats *
;;;
;;; The possible formats that the clipboard can provide its data in.
;;;
;;; Owner: GdkClipboard
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “local” property
;;;
;;;  “local”                    gboolean
;;;
;;; TRUE if the contents of the clipboard are owned by this process.
;;;
;;; Owner: GdkClipboard
;;;
;;; Flags: Read
;;;
;;; Default value: TRUE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_get_display ()                           Accessor
;;;
;;; GdkDisplay *
;;; gdk_clipboard_get_display (GdkClipboard *clipboard);
;;;
;;; Gets the GdkDisplay that the clipboard was created for.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; Returns :
;;;     a GdkDisplay.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_get_formats ()                           Accessor
;;;
;;; GdkContentFormats *
;;; gdk_clipboard_get_formats (GdkClipboard *clipboard);
;;;
;;; Gets the formats that the clipboard can provide its current contents in.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; Returns :
;;;     The formats of the clipboard.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_is_local ()
;;;
;;; gboolean
;;; gdk_clipboard_is_local (GdkClipboard *clipboard);
;;;
;;; Returns if the clipboard is local. A clipboard is considered local if it was
;;; last claimed by the running application.
;;;
;;; Note that gdk_clipboard_get_content() may return NULL even on a local
;;; clipboard. In this case the clipboard is empty.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; Returns :
;;;     TRUE if the clipboard is local
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_is_local" clipboard-is-local) :boolean
  (clipboard (g:object clipboard)))

(export 'clipboard-is-local)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_get_content ()                           Accessor
;;;
;;; GdkContentProvider *
;;; gdk_clipboard_get_content (GdkClipboard *clipboard);
;;;
;;; Returns the GdkContentProvider currently set on clipboard . If the clipboard
;;; is empty or its contents are not owned by the current process, NULL will be
;;; returned.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; Returns :
;;;     The content of a clipboard or NULL if the clipboard does not maintain
;;;     any content.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_store_async ()
;;;
;;; void
;;; gdk_clipboard_store_async (GdkClipboard *clipboard,
;;;                            int io_priority,
;;;                            GCancellable *cancellable,
;;;                            GAsyncReadyCallback callback,
;;;                            gpointer user_data);
;;;
;;; Asynchronously instructs the clipboard to store its contents remotely to
;;; preserve them for later usage. If the clipboard is not local, this function
;;; does nothing but report success.
;;;
;;; This function is called automatically when gtk_main() or GtkApplication
;;; exit, so you likely don't need to call it.
;;;
;;; clipboard :
;;;     a GdkClipboard
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
;;; gdk_clipboard_store_finish ()
;;;
;;; gboolean
;;; gdk_clipboard_store_finish (GdkClipboard *clipboard,
;;;                             GAsyncResult *result,
;;;                             GError **error);
;;;
;;; Finishes an asynchronous clipboard store started with
;;; gdk_clipboard_store_async().
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     TRUE if storing was successful.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_async ()
;;;
;;; void
;;; gdk_clipboard_read_async (GdkClipboard *clipboard,
;;;                           const char **mime_types,
;;;                           int io_priority,
;;;                           GCancellable *cancellable,
;;;                           GAsyncReadyCallback callback,
;;;                           gpointer user_data);
;;;
;;; Asynchronously requests an input stream to read the clipboard 's contents
;;; from. When the operation is finished callback will be called. You can then
;;; call gdk_clipboard_read_finish() to get the result of the operation.
;;;
;;; The clipboard will choose the most suitable mime type from the given list to
;;; fulfill the request, preferring the ones listed first.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; mime_types :
;;;     a NULL-terminated array of mime types to choose from
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
;;; gdk_clipboard_read_finish ()
;;;
;;; GInputStream *
;;; gdk_clipboard_read_finish (GdkClipboard *clipboard,
;;;                            GAsyncResult *result,
;;;                            const char **out_mime_type,
;;;                            GError **error);
;;;
;;; Finishes an asynchronous clipboard read started with
;;; gdk_clipboard_read_async().
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; out_mime_type :
;;;     pointer to store the chosen mime type in or NULL.
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     a GInputStream or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_value_async ()
;;;
;;; void
;;; gdk_clipboard_read_value_async (GdkClipboard *clipboard,
;;;                                 GType type,
;;;                                 int io_priority,
;;;                                 GCancellable *cancellable,
;;;                                 GAsyncReadyCallback callback,
;;;                                 gpointer user_data);
;;;
;;; Asynchronously request the clipboard contents converted to the given type .
;;; When the operation is finished callback will be called. You can then call
;;; gdk_clipboard_read_value_finish() to get the resulting GValue.
;;;
;;; For local clipboard contents that are available in the given GType, the
;;; value will be copied directly. Otherwise, GDK will try to use
;;; gdk_content_deserialize_async() to convert the clipboard's data.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; type :
;;;     a GType to read
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
;;; gdk_clipboard_read_value_finish ()
;;;
;;; const GValue *
;;; gdk_clipboard_read_value_finish (GdkClipboard *clipboard,
;;;                                  GAsyncResult *result,
;;;                                  GError **error);
;;;
;;; Finishes an asynchronous clipboard read started with
;;; gdk_clipboard_read_value_async().
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     a GValue containing the result.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_texture_async ()
;;;
;;; void
;;; gdk_clipboard_read_texture_async (GdkClipboard *clipboard,
;;;                                   GCancellable *cancellable,
;;;                                   GAsyncReadyCallback callback,
;;;                                   gpointer user_data);
;;;
;;; Asynchronously request the clipboard contents converted to a GdkPixbuf. When
;;; the operation is finished callback will be called. You can then call
;;; gdk_clipboard_read_texture_finish() to get the result.
;;;
;;; This is a simple wrapper around gdk_clipboard_read_value_async(). Use that
;;; function or gdk_clipboard_read_async() directly if you need more control
;;; over the operation.
;;;
;;; clipboard :
;;;     a GdkClipboard
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
;;; gdk_clipboard_read_texture_finish ()
;;;
;;; GdkTexture *
;;; gdk_clipboard_read_texture_finish (GdkClipboard *clipboard,
;;;                                    GAsyncResult *result,
;;;                                    GError **error);
;;;
;;; Finishes an asynchronous clipboard read started with
;;; gdk_clipboard_read_texture_async().
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     a new GdkTexture or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_text_async ()
;;;
;;; void
;;; gdk_clipboard_read_text_async (GdkClipboard *clipboard,
;;;                                GCancellable *cancellable,
;;;                                GAsyncReadyCallback callback,
;;;                                gpointer user_data);
;;;
;;; Asynchronously request the clipboard contents converted to a string. When
;;; the operation is finished callback will be called. You can then call
;;; gdk_clipboard_read_text_finish() to get the result.
;;;
;;; This is a simple wrapper around gdk_clipboard_read_value_async(). Use that
;;; function or gdk_clipboard_read_async() directly if you need more control
;;; over the operation.
;;;
;;; clipboard :
;;;     a GdkClipboard
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
;;; gdk_clipboard_read_text_finish ()
;;;
;;; char *
;;; gdk_clipboard_read_text_finish (GdkClipboard *clipboard,
;;;                                 GAsyncResult *result,
;;;                                 GError **error);
;;;
;;; Finishes an asynchronous clipboard read started with
;;; gdk_clipboard_read_text_async().
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; result :
;;;     a GAsyncResult
;;;
;;; error :
;;;     a GError location to store the error occurring, or NULL to ignore.
;;;
;;; Returns :
;;;     a new string or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_content ()                           Accessor
;;;
;;; gboolean
;;; gdk_clipboard_set_content (GdkClipboard *clipboard,
;;;                            GdkContentProvider *provider);
;;;
;;; Sets a new content provider on clipboard . The clipboard will claim the
;;; GdkDisplay's resources and advertise these new contents to other
;;; applications.
;;;
;;; In the rare case of a failure, this function will return FALSE. The
;;; clipboard will then continue reporting its old contents and ignore
;;; provider .
;;;
;;; If the contents are read by either an external application or the
;;; clipboard's read functions, clipboard will select the best format to
;;; transfer the contents and then request that format from provider .
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; provider :
;;;     the new contents of clipboard or NULL to clear the clipboard.
;;;
;;; Returns :
;;;     TRUE if setting the clipboard succeeded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gdk_clipboard_set ()
;;;
;;; void
;;; gdk_clipboard_set (GdkClipboard *clipboard,
;;;                    GType type,
;;;                    ...);
;;;
;;; Sets the clipboard to contain the value collected from the given varargs.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; type :
;;;     type of value to set
;;;
;;; ... :
;;;     value contents conforming to type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_valist ()
;;;
;;; void
;;; gdk_clipboard_set_valist (GdkClipboard *clipboard,
;;;                           GType type,
;;;                           va_list args);
;;;
;;; Sets the clipboard to contain the value collected from the given args .
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; type :
;;;     type of value to set
;;;
;;; args :
;;;     varargs containing the value of type
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_value ()
;;;
;;; void
;;; gdk_clipboard_set_value (GdkClipboard *clipboard,
;;;                          const GValue *value);
;;;
;;; Sets the clipboard to contain the given value .
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; value :
;;;     a GValue to set
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_text ()
;;;
;;; void
;;; gdk_clipboard_set_text (GdkClipboard *clipboard,
;;;                         const char *text);
;;;
;;; Puts the given text into the clipboard.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; text :
;;;     Text to put into the clipboard
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_texture ()
;;;
;;; void
;;; gdk_clipboard_set_texture (GdkClipboard *clipboard,
;;;                            GdkTexture *texture);
;;;
;;; Puts the given texture into the clipboard.
;;;
;;; clipboard :
;;;     a GdkClipboard
;;;
;;; texture :
;;;     a GdkTexture to put into the clipboard
;;; ----------------------------------------------------------------------------

;;; --- End of file gdk.clipboard.lisp -----------------------------------------
