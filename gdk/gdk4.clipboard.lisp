;;; ----------------------------------------------------------------------------
;;; gdk4.clipboard.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;;     gdk_clipboard_set_content
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
    "display" "GdkDisplay" t nil)
   (formats
    clipboard-formats
    "formats" "GdkContentFormats" t nil)
   (local
    clipboard-local
    "local" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'clipboard 'type)
 "@version{2023-7-30}
  @begin{short}
    The @class{gdk:clipboard} object represents data shared between applications
    or inside an application.
  @end{short}

  To get a @class{gdk:clipboard} object, use the @fun{gdk:display-clipboard}
  or @fun{gdk:display-primary-clipboard} functions. You can find out about the
  data that is currently available in a clipboard using the
  @fun{gdk:clipboard-formats} function.

  To make text or image data available in a clipboard, use the
  @fun{gdk:clipboard-set-text} or @fun{gdk:clipboard-set-texture} functions.
  For other data, you can use the @fun{gdk:clipboard-set-content} function,
  which takes a @class{gdk:content-provider} object.

  To read textual or image data from a clipboard, use the
  @fun{gdk:clipboard-read-text-async} or @fun{gdk:clipboard-read-texture-async}
  functions.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (clipboard)    :run-last
      @end{pre}
      The signal is emitted when the clipboard changes ownership.
      @begin[code]{table}
       @entry[clipboard]{The @class{gdk:clipboard} object on which the signal
         was emitted.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk:clipboard-content}
  @see-slot{gdk:clipboard-display}
  @see-slot{gdk:clipboard-formats}
  @see-slot{gdk:clipboard-local}
  @see-class{gdk:content-provider}
  @see-class{gdk:content-formats}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:clipboard-content --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content" 'clipboard) t)
 "The @code{content} property of type @class{gdk:content-provider} (Read) @br{}
  The content provider or @code{nil} if the clipboard is empty or contents are
  provided otherwise.")

#+liber-documentation
(setf (liber:alias-for-function 'clipboard-content)
      "Accessor"
      (documentation 'clipboard-content 'function)
 "@version{2023-7-30}
  @syntax{(gdk:clipboard-content object) => content}
  @argument[object]{a @class{gdk:clipboard} object}
  @argument[content]{a @class{gdk:content-provider} instance}
  @begin{short}
    Accessor of the @slot[gdk:clipboard]{content} slot of the
    @class{gdk:clipboard} class.
  @end{short}
  The @fun{gdk:clipboard-content} function returns the
  @class{gdk:content-provider} object currently set on @arg{clipboard}. If the
  clipboard is empty or its contents are not owned by the current process,
  @code{nil} will be returned.
  @begin[Note]{dictionary}
    The @slot[gdk:clipboard]{content} property is not writeable. Use the
    @fun{gdk:clipboard-set-content} function to set the content provider.
  @end{dictionary}
  @see-class{gdk:clipboard}
  @see-class{gdk:content-provider}
  @see-function{gdk:clipboard-set-content}")

;;; --- gdk:clipboard-display --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'clipboard) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct only) @br{}
  The display that the clipboard belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'clipboard-display)
      "Accessor"
      (documentation 'clipboard-display 'function)
 "@version{2023-7-30}
  @syntax{(gdk:clipboard-display object) => display}
  @argument[object]{a @class{gdk:clipboard} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:clipboard]{display} slot of the
    @class{gdk:clipboard} class.
  @end{short}
  The @fun{gdk:clipboard-display} function returns the display that the
  clipboard was created for.
  @see-class{gdk:clipboard}
  @see-class{gdk:display}")

;;; --- gdk:clipboard-formats --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "formats" 'clipboard) t)
 "The @code{formats} property of type @class{gdk:content-formats} (Read) @br{}
  The possible formats that the clipboard can provide its data in.")

#+liber-documentation
(setf (liber:alias-for-function 'clipboard-formats)
      "Accessor"
      (documentation 'clipboard-formats 'function)
 "@version{2023-7-30}
  @syntax{(gdk:clipboard-formats object) => formats}
  @argument[object]{a @class{gdk:clipboard} object}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @begin{short}
    Accessor of the @slot[gdk:clipboard]{formats} slot of the
    @class{gdk:clipboard} class.
  @end{short}
  The @fun{gdk:clipboard-formats} function gets the formats that the clipboard
  can provide its current contents in.
  @see-class{gdk:clipboard}
  @see-class{gdk:content-formats}")

;;; --- gdk:clipboard-local ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "local" 'clipboard) t)
 "The @code{local} property of type @code{:boolean} (Read) @br{}
  @em{True} if the contents of the clipboard are owned by this process. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'clipboard-local)
      "Accessor"
      (documentation 'clipboard-local 'function)
 "@version{2023-7-30}
  @syntax{(gdk:clipboard-local object) => local}
  @argument[object]{a @class{gdk:clipboard} object}
  @argument[local]{a boolean whether the contents of the clipboard are owned
    by this process}
  @begin{short}
    Accessor of the @slot[gdk:clipboard]{local} slot of the
    @class{gdk:clipboard} class.
  @end{short}
  The @fun{gdk:clipboard-local} function returns whether the clipboard is local.
  See also the @fun{gdk:clipboard-is-local} function.
  @see-class{gdk:clipboard}
  @see-function{gdk:clipboard-is-local}")

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_is_local
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_is_local" clipboard-is-local) :boolean
 #+liber-documentation
 "@version{2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @return{@em{True} if the clipboard is local.}
  @begin{short}
    Returns if the clipboard is local.
  @end{short}
  A clipboard is considered local if it was last claimed by the running
  application. Note that the @fun{gdk:clipboard-content} function may return
  @code{nil} even on a local clipboard. In this case the clipboard is empty.
  @see-class{gdk:clipboard}
  @see-function{gdk:clipboard-content}"
  (clipboard (g:object clipboard)))

(export 'clipboard-is-local)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_store_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_store_async" %clipboard-store-async) :void
  (clipboard (g:object clipboard))
  (priority :int)
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun clipboard-store-async (clipboard priority cancellable func)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[priority]{an integer with the I/O priority of the request}
  @argument[cancellable]{an optional @class{g:cancellable} instance, @code{nil}
    to ignore}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the request is satisfied}
  @begin{short}
    Asynchronously instructs the clipboard to store its contents remotely to
    preserve them for later usage.
  @end{short}
  If the clipboard is not local, this function does nothing but report success.

  This function is called automatically when the @class{gtk:application} object
  exit, so you likely do not need to call it.
  @see-class{gdk:clipboard}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-class{gtk:application}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%clipboard-store-async clipboard
                            priority
                            cancellable
                            (cffi:callback g:async-ready-callback)
                            ptr)))

(export 'clipboard-store-async)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_store_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipoard_store_finish" %clipboard-store-finish) :boolean
  (clipboard (g:object clipboard))
  (result (g:object g:async-result))
  (err :pointer))

(defun clipboard-store-finish (clipboard result)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[result]{a @class{g:async-result} instance}
  @return{@em{True} if storing was successful.}
  @begin{short}
    Finishes an asynchronous clipboard store started with the
    @fun{gdk:clipboard-store-async} function.
  @end{short}
  @see-class{gdk:clipboard}
  @see-class{g:async-result}
  @see-function{gdk:clipboard-store-async}"
  (glib:with-ignore-g-error (err)
    (%clipboard-store-finish clipboard result err)))

(export 'clipboard-store-finish)

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
;;; gdk_clipboard_read_value_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_read_value_async" %clipboard-read-value-async)
    :void
  (clipboard (g:object clipboard))
  (gtype g:type-t)
  (priority :int)
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun clipboard-read-value-async (clipboard gtype priority cancellable func)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[gtype]{a @class{g:type-t} type to read}
  @argument[priority]{an integer with the I/O priority of the request}
  @argument[cancellable]{an optional @class{g:cancellable} instance, @code{nil}
    to ignore}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the request is satisfied}
  @begin{short}
    Asynchronously request the clipboard contents converted to the given
    @arg{gtype}.
  @end{short}
  When the operation is finished the @arg{func} callback function will be
  called. You can then call the @fun{gdk:clipboard-read-value-finish} function
  to get the resulting value.

  For local clipboard contents that are available in the given @arg{gtype}, the
  value will be copied directly. Otherwise, GDK will try to use the
  @fun{gdk:content-deserialize-async} function to convert the clipboard's data.
  @see-class{gdk:clipboard}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gdk:clipboard-read-value-finish}
  @see-function{gdk:content-deserialize-async}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%clipboard-read-value-async clipboard
                                 gtype
                                 priority
                                 cancellable
                                 (cffi:callback g:async-ready-callback)
                                 ptr)))

(export 'clipboard-read-value-async)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_value_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_read_value_finish" %clipboard-read-value-finish)
    (:pointer (:struct g:value))
  (clipboard (g:object clipboard))
  (result (g:object g:async-result))
  (err :pointer))

(defun clipboard-read-value-finish (clipboard result)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[result]{a @class{g:async-result} instance}
  @return{The @symbol{g:value} instance which holds the value of the content of
    @arg{clipboard}.}
  @begin{short}
    Finishes an asynchronous clipboard read started with the
    @fun{gdk:clipboard-read-value-async} function.
  @end{short}
  @see-class{gdk:clipboard}
  @see-class{g:async-result}
  @see-function{gdk:clipboard-read-value-async}"
  (glib:with-ignore-g-error (err)
    (%clipboard-read-value-finish clipboard result err)))

(export 'clipboard-read-value-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_texture_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_read_texture_async"
               %clipboard-read-texture-async) :void
  (clipboard (g:object clipboard))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun clipboard-read-texture-async (clipboard cancellable func)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[cancellable]{an optional @class{g:cancellable} instance, @code{nil}
    to ignore}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the request is satisfied}
  @begin{short}
    Asynchronously request the clipboard contents converted to a
    @class{gdk:pixbuf} object.
  @end{short}
  When the operation is finished the @arg{func} callback function will be
  called. You can then call the @fun{gdk:clipboard-read-texture-finish}
  function to get the result.

  This is a simple wrapper around the @fun{gdk:clipboard-read-value-async}
  function. Use that function directly if you need more control over the
  operation.
  @see-class{gdk:clipboard}
  @see-class{g:cancellable}
  @see-class{gdk:pixbuf}
  @see-symbol{g:async-ready-callback}
  @see-function{gdk:clipboard-read-texture-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%clipboard-read-texture-async clipboard
                                   cancellable
                                   (cffi:callback g:async-ready-callback)
                                   ptr)))

(export 'clipboard-read-texture-async)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_texture_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipoard_read_texture_finish"
               %clipboard-read-texture-finish) (g:object texture)
  (clipboard (g:object clipboard))
  (result (g:object g:async-result))
  (err :pointer))

(defun clipboard-read-texture-finish (clipboard result)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[result]{a @class{g:async-result} instance}
  @return{The @class{gdk:texture} object which holds the value of the content of
    @arg{clipboard}.}
  @begin{short}
    Finishes an asynchronous clipboard read started with the
    @fun{gdk:clipboard-read-texture-async} function.
  @end{short}
  @see-class{gdk:clipboard}
  @see-class{g:async-result}
  @see-function{gdk:clipboard-read-texture-async}"
  (glib:with-ignore-g-error (err)
    (%clipboard-read-texture-finish clipboard result err)))

(export 'clipboard-read-texture-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_text_async
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_read_text_async" %clipboard-read-text-async) :void
  (clipboard (g:object clipboard))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun clipboard-read-text-async (clipboard cancellable func)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[cancellable]{an optional @class{g:cancellable} instance, @code{nil}
    to ignore}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the request is satisfied}
  @begin{short}
    Asynchronously request the clipboard contents converted to a string.
  @end{short}
  When the operation is finished the @arg{func} callback function will be
  called. You can then call the @fun{gdk:clipboard-read-text-finish} function
  to get the result.

  This is a simple wrapper around the @fun{gdk:clipboard-read-value-async}
  function. Use that function directly if you need more control over the
  operation.
  @see-class{gdk:clipboard}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gdk:clipboard-read-text-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%clipboard-read-text-async clipboard
                                   cancellable
                                   (cffi:callback g:async-ready-callback)
                                   ptr)))

(export 'clipboard-read-text-async)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_read_text_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipoard_read_text_finish"
               %clipboard-read-text-finish) :string
  (clipboard (g:object clipboard))
  (result (g:object g:async-result))
  (err :pointer))

(defun clipboard-read-text-finish (clipboard result)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[result]{a @class{g:async-result} instance}
  @return{The string which holds the value of the content of @arg{clipboard}.}
  @begin{short}
    Finishes an asynchronous clipboard read started with the
    @fun{gdk:clipboard-read-text-async} function.
  @end{short}
  @see-class{gdk:clipboard}
  @see-class{g:async-result}
  @see-function{gdk:clipboard-read-text-async}"
  (glib:with-ignore-g-error (err)
    (%clipboard-read-text-finish clipboard result err)))

(export 'clipboard-read-text-finish)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_content
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_set_content" clipboard-set-content) :boolean
 #+liber-documentation
 "@version{2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[content]{a new @class{gdk:content-provider} object or @code{nil}
    to clear the clipboard}
  @return{@em{True} if setting the clipboard succeeded.}
  @begin{short}
    Sets a new content provider on @arg{clipboard}.
  @end{short}
  The clipboard will claim the resources of the @class{gdk:display} object and
  advertise these new contents to other applications.

  In the rare case of a failure, this function will return @em{false}. The
  clipboard will then continue reporting its old contents and ignore
  @arg{provider}.

  If the contents are read by either an external application or the read
  functions of the clipboard, @arg{clipboard} will select the best format to
  transfer the contents and then request that format from @arg{provider}.
  @see-class{gdk:clipboard}
  @see-class{gdk:display}
  @see-class{gdk:content-provider}"
  (clipboard (g:object clipboard))
  (provider (g:object content-provider)))

(export 'clipboard-set-content)

;;; ----------------------------------------------------------------------------
;;;gdk_clipboard_set
;;; ----------------------------------------------------------------------------

(defun clipboard-set (clipboard gtype value)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[gtype]{a @class{g:type-t} type of value to set}
  @argument[value]{a Lisp value to be set}
  @begin{short}
    Sets the clipboard to contain the given @arg{value}.
  @end{short}
  @begin[Note]{dictionary}
    This function intializes a @symbol{g:value} instance  of the given
    @arg{gtype}, stores @arg{value} in the @symbol{g:value} instance and calls
    the @fun{gdk:clipboard-set-value} function.
  @end{dictionary}
  @see-class{gdk:clipboard}
  @see-class{g:type-t}"
  (cffi:with-foreign-object (gvalue '(:pointer (:struct g:value)))
    (g:value-set gvalue value gtype)
    (clipboard-set-value clipboard gvalue)))

(export 'clipboard-set)

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
;;; gdk_clipboard_set_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_set_value" clipboard-set-value) :void
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipoboard]{a @class{gdk:clipboard} object}
  @argument[gvalue]{a @symbol{g:value} instance}
  @short{Sets the clipboard to contain the given @arg{gvalue}.}
  @begin[Note]{dictionary}
    This function is called from the @fun{gdk:clipboard-set} function to store
    the @symbol{g:value} instance into the clipboard.
  @end{dictionary}
  @see-class{gdk:clipboard}
  @see-symbol{g:value}
  @see-function{gdk:clipboard-set}"
  (clipboard (g:object clipboard))
  (gvalue (:pointer (:struct g:value))))

(export 'clipboard-set-value)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_set_text" clipboard-set-text) :void
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[text]{a string with the text to put into the clipboard}
  @short{Puts the given @arg{text} into the clipboard.}
  @see-class{gdk:clipboard}"
  (clipboard (g:object clipboard))
  (text :string))

(export 'clipboard-set-text)

;;; ----------------------------------------------------------------------------
;;; gdk_clipboard_set_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_clipboard_set_texture" clipboard-set-texture) :void
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @argument[texture]{a @class{gdk:texture} object to put into the clipboard}
  @short{Puts the given @arg{texture} into the clipboard.}
  @see-class{gdk:clipboard}
  @see-class{gdk:texture}"
  (clipboard (g:object clipboard))
  (texture (g:object texture)))

(export 'clipboard-set-texture)

;;; --- End of file gdk4.clipboard.lisp ----------------------------------------
