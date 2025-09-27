;;; ----------------------------------------------------------------------------
;;; gtk4.im-context.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkIMContext
;;;
;;;     Base class for input method contexts
;;;
;;; Types and Values
;;;
;;;     GtkIMContext
;;;
;;; Functions
;;;
;;;     gtk_im_context_get_preedit_string
;;;     gtk_im_context_filter_keypress
;;;     gtk_im_context_filter_key
;;;     gtk_im_context_focus_in
;;;     gtk_im_context_focus_out
;;;     gtk_im_context_reset
;;;     gtk_im_context_set_client_widget
;;;     gtk_im_context_set_cursor_location
;;;     gtk_im_context_set_use_preedit
;;;     gtk_im_context_set_surrounding                      Deprecated 4.2
;;;     gtk_im_context_get_surrounding                      Deprecated 4.2
;;;     gtk_im_context_delete_surrounding
;;;     gtk_im_context_get_surrounding_with_selection       Since 4.2
;;;     gtk_im_context_set_surrounding_with_selection       Since 4.2
;;;     gtk_im_context_activate_osk                         Since 4.14
;;;
;;; Properties
;;;
;;;     input-hints
;;;     input-purpose
;;;
;;; Signals
;;;
;;;     commit
;;;     delete-surrounding
;;;     preedit-changed
;;;     preedit-end
;;;     preedit-start
;;;     retrieve-surrounding
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIMContext
;;;         ├── GtkIMContextSimple
;;;         ╰── GtkIMMulticontext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkIMContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkIMContext" im-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_im_context_get_type")
   ((input-hints
    im-context-input-hints
    "input-hints" "GtkInputHints" t t)
    (input-purpose
     im-context-input-purpose
     "input-purpose" "GtkInputPurpose" t t)))

#+liber-documentation
(setf (documentation 'im-context 'type)
 "@version{2025-09-25}
  @begin{short}
    The @class{gtk:im-context} class defines the interface for GTK input
    methods.
  @end{short}
  An input method is used by GTK text input widgets like the @class{gtk:entry}
  widget to map from key events to Unicode character strings.

  An input method may consume multiple key events in sequence before finally
  outputting the composed result. This is called preediting, and an input
  method may provide feedback about this process by displaying the intermediate
  composition states as preedit text. To do so, the @class{gtk:im-context}
  object will emit the @sig[gtk:im-context]{preedit-start},
  @sig[gtk:im-context]{preedit-changed} and @sig[gtk:im-context]{preedit-end}
  signals.

  For instance, the built-in GTK @class{gtk:im-context-simple} input method
  implements the input of arbitrary Unicode code points by holding down the
  @kbd{Control} and @kbd{Shift} keys and then typing @kbd{u} followed by the
  hexadecimal digits of the code point. When releasing the @kbd{Control} and
  @kbd{Shift} keys, preediting ends and the character is inserted as text. For
  example,
  @begin{pre}
Ctrl+Shift+u 2 0 A C
  @end{pre}
  results in the € sign.

  Additional input methods can be made available for use by GTK widgets as
  loadable modules. An input method module is a small shared library which
  provides a @code{GIOExtension} for the extension point named
  @file{gtk-im-module}.

  To connect a widget to the users preferred input method, you should use the
  @class{gtk:im-multicontext} class.
  @begin[Signal Details]{dictionary}
    @begin[im-context::commit]{signal}
      @begin{pre}
lambda (context str)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
        @entry[str]{The string for the completed character(s) entered by the
          user.}
      @end{simple-table}
      The signal is emitted when a complete input sequence has been entered by
      the user. This can be a single character immediately after a key press or
      the final result of preediting.
    @end{signal}
    @begin[im-context::delete-surrounding]{signal}
      @begin{pre}
lambda (context offset n-chars)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
        @entry[offset]{The integer for the character offset from the cursor
          position of the text to be deleted. A negative value indicates a
          position before the cursor.}
        @entry[n-chars]{The integer for the number of characters to be
          deleted.}
        @entry[Returns]{@em{True} if the signal was handled.}
      @end{simple-table}
      The signal is emitted when the input method needs to delete all or part
      of the context surrounding the cursor.
    @end{signal}
    @begin[im-context::preedit-changed]{signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
      @end{simple-table}
      The signal is emitted whenever the preedit sequence currently being
      entered has changed. It is also emitted at the end of a preedit sequence,
      in which case the @fun{gtk:im-context-preedit-string} function returns
      the empty string.
    @end{signal}
    @begin[im-context::preedit-end]{signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
      @end{simple-table}
      The signal is emitted when a preediting sequence has been completed or
      canceled.
    @end{signal}
    @begin[im-context::preedit-start]{signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
      @end{simple-table}
      The signal is emitted when a new preediting sequence starts.
    @end{signal}
    @begin[im-context::retrieve-surrounding]{signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[context]{The @class{gtk:im-context} object on which the signal
          is emitted.}
        @entry[Returns]{@em{True} if the signal was handled.}
      @end{simple-table}
      The signal is emitted when the input method requires the context
      surrounding the cursor. The callback should set the input method
      surrounding context by calling the @fun{gtk:im-context-surrounding}
      function.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:im-context-input-hints}
  @see-slot{gtk:im-context-input-purpose}
  @see-class{gtk:im-context-simple}
  @see-class{gtk:im-multicontext}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:im-context-input-hints ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'im-context) t)
 "The @code{input-hints} property of type @sym{gtk:input-hints} (Read / Write)
  @br{}
  The hints for the text field behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'im-context-input-hints)
      "Accessor"
      (documentation 'im-context-input-hints 'function)
 "@version{2025-09-25}
  @syntax{(gtk:im-context-input-hints object) => hints}
  @syntax{(setf (gtk:im-context-input-hints object) hints)}
  @argument[object]{a @class{gtk:im-context} object}
  @argument[hints]{a value of the @sym{gtk:input-hints} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:im-context]{input-hints} slot of the
    @class{gtk:im-context} class gets or sets the hints for the text field
    behaviour.
  @end{short}
  @see-class{gtk:im-context}")

;;; --- gtk:im-context-input-purpose -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose" 'im-context) t)
 "The @code{input-purpose} property of type @sym{gtk:input-purpose}
  (Read / Write) @br{}
  The purpose of the text field. @br{}
  Default value: @val[gtk:input-purpose]{:free-from}")

#+liber-documentation
(setf (liber:alias-for-function 'im-context-input-purpose)
      "Accessor"
      (documentation 'im-context-input-purpose 'function)
 "@version{2025-09-25}
  @syntax{(gtk:im-context-input-purpose object) => purpose}
  @syntax{(setf (gtk:im-context-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:im-context} object}
  @argument[purpose]{a value of the @sym{gtk:input-purpose} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:im-context]{input-purpose} slot of the
    @class{gtk:im-context} class gets or sets the purpose of the text field.
  @end{short}
  @see-class{gtk:im-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_get_preedit_string ()
;;;
;;; void
;;; gtk_im_context_get_preedit_string (GtkIMContext *context,
;;;                                    char **str,
;;;                                    PangoAttrList **attrs,
;;;                                    int *cursor_pos);
;;;
;;; Retrieve the current preedit string for the input context, and a list of
;;; attributes to apply to the string. This string should be displayed inserted
;;; at the insertion point.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; str :
;;;     location to store the retrieved string. The string retrieved must be
;;;     freed with g_free().
;;;
;;; attrs :
;;;     location to store the retrieved attribute list. When you are done with
;;;     this list, you must unreference it with pango_attr_list_unref().
;;;
;;; cursor_pos :
;;;     location to store position of cursor (in characters) within the preedit
;;;     string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_filter_keypress ()
;;;
;;; gboolean
;;; gtk_im_context_filter_keypress (GtkIMContext *context,
;;;                                 GdkEvent *event);
;;;
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; event :
;;;     the key event
;;;
;;; Returns :
;;;
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_im_context_filter-keypress" im-context-filter-keypress)
    :boolean
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:im-context} object}
  @argument[event]{a @class{gdk:event} instance for the key event}
  @return{@em{True} if the input method handled the key event.}
  @begin{short}
    Allow an input method to internally handle key press and release events.
  @end{short}
  If this function returns @em{true}, then no further processing should be done
  for this key event.
  @see-class{gtk:im-context}
  @see-class{gdk:event}"
  (context (g:object im-context))
  (event gdk:event))

(export 'im-context-filter-keypress)

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_filter_key ()
;;;
;;; gboolean
;;; gtk_im_context_filter_key (GtkIMContext *context,
;;;                            gboolean press,
;;;                            GdkSurface *surface,
;;;                            GdkDevice *device,
;;;                            guint32 time,
;;;                            guint keycode,
;;;                            GdkModifierType state,
;;;                            int group);
;;;
;;; Allow an input method to forward key press and release events to another
;;; input method, without necessarily having a GdkEvent available.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; press :
;;;     whether to forward a key press or release event
;;;
;;; surface :
;;;     the surface the event is for
;;;
;;; device :
;;;     the device that the event is for
;;;
;;; time :
;;;     the timestamp for the event
;;;
;;; keycode :
;;;     the keycode for the event
;;;
;;; state :
;;;     modifier state for the event
;;;
;;; group :
;;;     the active keyboard group for the event
;;;
;;; Returns :
;;;     TRUE if the input method handled the key event.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_focus_in ()
;;;
;;; void
;;; gtk_im_context_focus_in (GtkIMContext *context);
;;;
;;; Notify the input method that the widget to which this input context
;;; corresponds has gained focus. The input method may, for example, change the
;;; displayed feedback to reflect this change.
;;;
;;; context :
;;;     a GtkIMContext
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_focus_out ()
;;;
;;; void
;;; gtk_im_context_focus_out (GtkIMContext *context);
;;;
;;; Notify the input method that the widget to which this input context
;;; corresponds has lost focus. The input method may, for example, change the
;;; displayed feedback or reset the contexts state to reflect this change.
;;;
;;; context :
;;;     a GtkIMContext
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_reset ()
;;;
;;; void
;;; gtk_im_context_reset (GtkIMContext *context);
;;;
;;; Notify the input method that a change such as a change in cursor position
;;; has been made. This will typically cause the input method to clear the
;;; preedit state.
;;;
;;; context :
;;;     a GtkIMContext
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_set_client_widget ()
;;;
;;; void
;;; gtk_im_context_set_client_widget (GtkIMContext *context,
;;;                                   GtkWidget *widget);
;;;
;;; Set the client window for the input context; this is the GtkWidget holding
;;; the input focus. This widget is used in order to correctly position status
;;; windows, and may also be used for purposes internal to the input method.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; widget :
;;;     the client widget. This may be NULL to indicate that the previous
;;;     client widget no longer exists.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_set_cursor_location ()
;;;
;;; void
;;; gtk_im_context_set_cursor_location (GtkIMContext *context,
;;;                                     const GdkRectangle *area);
;;;
;;; Notify the input method that a change in cursor position has been made. The
;;; location is relative to the client window.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; area :
;;;     new location
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_set_use_preedit ()
;;;
;;; void
;;; gtk_im_context_set_use_preedit (GtkIMContext *context,
;;;                                 gboolean use_preedit);
;;;
;;; Sets whether the IM context should use the preedit string to display
;;; feedback. If use_preedit is FALSE (default is TRUE), then the IM context
;;; may use some other method to display feedback, such as displaying it in a
;;; child of the root window.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; use_preedit :
;;;     whether the IM context should use the preedit string.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_set_surrounding ()
;;;
;;; void
;;; gtk_im_context_set_surrounding (GtkIMContext *context,
;;;                                 const char *text,
;;;                                 int len,
;;;                                 int cursor_index);
;;;
;;; Sets surrounding context around the insertion point and preedit string.
;;; This function is expected to be called in response to the
;;; GtkIMContext::retrieve_surrounding signal, and will likely have no effect
;;; if called at other times.
;;;
;;; Deprecated since: 4.2
;;; Use gtk_im_context_set_surrounding_with_selection() instead.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; text :
;;;     text surrounding the insertion point, as UTF-8. the preedit string
;;;     should not be included within text .
;;;
;;; len :
;;;     the length of text , or -1 if text is nul-terminated
;;;
;;; cursor_index :
;;;     the byte index of the insertion cursor within text .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_get_surrounding ()
;;;
;;; gboolean
;;; gtk_im_context_get_surrounding (GtkIMContext *context,
;;;                                 char **text,
;;;                                 int *cursor_index);
;;;
;;; Retrieves context around the insertion point. Input methods typically want
;;; context in order to constrain input text based on existing text; this is
;;; important for languages such as Thai where only some sequences of characters
;;; are allowed.
;;;
;;; This function is implemented by emitting the
;;; GtkIMContext::retrieve_surrounding signal on the input method; in response
;;; to this signal, a widget should provide as much context as is available, up
;;; to an entire paragraph, by calling gtk_im_context_set_surrounding(). Note
;;; that there is no obligation for a widget to respond to
;;; the ::retrieve_surrounding signal, so input methods must be prepared to
;;; function without context.
;;;
;;; Deprecated since: 4.2
;;; Use gtk_im_context_get_surrounding_with_selection() instead.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; text :
;;;     location to store a UTF-8 encoded string of text holding context around
;;;     the insertion point. If the function returns TRUE, then you must free
;;;     the result stored in this location with g_free().
;;;
;;; cursor_index :
;;;     location to store byte index of the insertion cursor within text .
;;;
;;; Returns :
;;;     TRUE if surrounding text was provided; in this case you must free the
;;;     result stored in *text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_delete_surrounding ()
;;;
;;; gboolean
;;; gtk_im_context_delete_surrounding (GtkIMContext *context,
;;;                                    int offset,
;;;                                    int n_chars);
;;;
;;; Asks the widget that the input context is attached to delete characters
;;; around the cursor position by emitting the GtkIMContext::delete_surrounding
;;; signal. Note that offset and n_chars are in characters not in bytes which
;;; differs from the usage other places in GtkIMContext.
;;;
;;; In order to use this function, you should first call
;;; gtk_im_context_get_surrounding() to get the current context, and call this
;;; function immediately afterwards to make sure that you know what you are
;;; deleting. You should also account for the fact that even if the signal was
;;; handled, the input context might not have deleted all the characters that
;;; were requested to be deleted.
;;;
;;; This function is used by an input method that wants to make subsitutions in
;;; the existing text in response to new input. It is not useful for
;;; applications.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; offset :
;;;     offset from cursor position in chars; a negative value means start
;;;     before the cursor.
;;;
;;; n_chars :
;;;     number of characters to delete.
;;;
;;; Returns :
;;;     TRUE if the signal was handled.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_get_surrounding_with_selection
;;;
;;; Retrieves context around the insertion point.
;;;
;;; Since 4.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_set_surrounding_with_selection
;;;
;;; Sets surrounding context around the insertion point and preedit string.
;;; This function is expected to be called in response to the
;;; GtkIMContext::retrieve-surrounding signal, and will likely have no effect
;;; if called at other times.
;;;
;;; Since 4.2
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_im_context_activate_osk
;;;
;;; Requests the platform to show an on-screen keyboard for user input.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.im-context.lisp ---------------------------------------
