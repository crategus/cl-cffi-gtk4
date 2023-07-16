;;; ----------------------------------------------------------------------------
;;; gtk4.im-context.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;;     gtk_im_context_set_surrounding
;;;     gtk_im_context_get_surrounding
;;;     gtk_im_context_delete_surrounding
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
;;; struct GtkIMContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkIMContext" im-context
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
 "@version{#2020-9-15}
  @begin{short}
    The @sym{gtk:im-context} object defines the interface for GTK input methods.
  @end{short}
  An input method is used by GTK text input widgets like @class{gtk:entry} to
  map from key events to Unicode character strings.

  The default input method can be set programmatically via the
  @slot[gtk:settings]{im-module} property. Alternatively, you may set the
  @code{GTK_IM_MODULE} environment variable as documented in Running GTK
  Applications.

  The @slot[gtk:entry]{im-module} and @slot[gtk:text-view]{im-module} properties
  may also be used to set input methods for specific widget instances. For
  instance, a certain entry widget might be expected to contain certain
  characters which would be easier to input with a certain input method.

  An input method may consume multiple key events in sequence and finally
  output the composed result. This is called preediting, and an input method
  may provide feedback about this process by displaying the intermediate
  composition states as preedit text. For instance, the default GTK input
  method implements the input of arbitrary Unicode code points by holding down
  the @kbd{Control} and @kbd{Shift} keys and then typing \"U\" followed by the
  hexadecimal digits of the code point. When releasing the @bdk{Control} and
  @kbd{Shift} keys, preediting ends and the character is inserted as text.
  The @kbd{Ctrl+Shift+u20AC} key for example results in the Euro sign.

  Additional input methods can be made available for use by GTK widgets as
  loadable modules. An input method module is a small shared library which
  implements a subclass of @sym{gtk:im-context} or @class{gtk:im-context-simple}
  and exports these four functions:
  @begin{pre}
void im_module_init(<GTKDOCLINK HREF=\"GTypeModule\">
                      GTypeModule</GTKDOCLINK> *module);
  @end{pre}
  This function should register the @class{g:type-t} of the @sym{gtk:im-context}
  subclass which implements the input method by means of the
  @code{g_type_module_register_type()} function. Note that the
  @code{g_type_register_static()} function cannot be used as the type needs to
  be registered dynamically.
  @begin{pre}
void im_module_exit(void);
  @end{pre}
  Here goes any cleanup code your input method might require on module unload.
  @begin{pre}
void im_module_list(const <a class=\"link\"
                      href=\"GtkIMContext.html#GtkIMContextInfo\"
                      title=\"struct GtkIMContextInfo\">GtkIMContextInfo</a>
                      ***contexts, int *n_contexts)
{
  *contexts = info_list;
  *n_contexts = G_N_ELEMENTS (info_list);
@}
  @end{pre}
  This function returns the list of input methods provided by the module. The
  example implementation above shows a common solution and simply returns a
  pointer to statically defined array of @symbol{gtk:im-context-info} items for
  each provided input method.
  @begin{pre}
<a class=\"link\" href=\"GtkIMContext.html\"
                title=\"GtkIMContext\">GtkIMContext</a> *
                im_module_create(const <GTKDOCLINK HREF=\"gchar\">
                                    gchar</GTKDOCLINK> *context_id);
  @end{pre}
  This function should return a pointer to a newly created instance of the
  @sym{gtk:im-context} subclass identified by @code{context-id}. The context ID
  is the same as specified in the @symbol{gtk:im-context-info} array returned by
  @code{im_module_list()}.

  After a new loadable input method module has been installed on the system,
  the configuration file @code{gtk.immodules} needs to be regenerated by
  @code{gtk-query-immodules-3.0}, in order for the new input method to become
  available to GTK applications.
  @begin[Signal Details]{dictionary}
    @subheading{The \"commit\" signal}
      @begin{pre}
lambda (context str)    :run-last
      @end{pre}
      The signal is emitted when a complete input sequence has been entered by
      the user. This can be a single character immediately after a key press or
      the final result of preediting.
      @begin[code]{table}
        @entry[context]{The @sym{gtk:im-context} object on which the signal is
          emitted.}
        @entry[str]{The completed character(s) entered by the user.}
      @end{table}
    @subheading{The \"delete-surrounding\" signal}
      @begin{pre}
lambda (context offset n-chars)    :run-last
      @end{pre}
      The signal is emitted when the input method needs to delete all or part of
      the context surrounding the cursor.
      @begin[code]{table}
        @entry[context]{The @sym{gtk:im-context} object on which the signal is
          emitted.}
        @entry[offset]{The character offset from the cursor position of the text
          to be deleted. A negative value indicates a position before the
          cursor.}
        @entry[n-chars]{The number of characters to be deleted.}
        @entry[Returns]{@em{True} if the signal was handled.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      The signal is emitted whenever the preedit sequence currently being
      entered has changed. It is also emitted at the end of a preedit sequence,
      in which case the @fun{gtk:im-context-preedit-string} function returns the
      empty string.
      @begin[code]{table}
        @entry[context]{The @sym{gtk:im-context} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"preedit-end\" signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      The signal is emitted when a preediting sequence has been completed or
      canceled.
      @begin[code]{table}
        @entry[context]{The @sym{gtk:im-context} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"preedit-start\" signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      The signal is emitted when a new preediting sequence starts.
      @begin[code]{table}
        @entry[context]{The @sym{gtk:im-context} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"retrieve-surrounding\" signal}
      @begin{pre}
lambda (context)    :run-last
      @end{pre}
      The signal is emitted when the input method requires the context
      surrounding the cursor. The callback should set the input method
      surrounding context by calling the @fun{gtk:im-context-surrounding}
      function.
      @begin[code]{table}
        @entry[context]{The object on which the signal is emitted.}
        @entry[Returns]{@em{True} if the signal was handled.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:im-context-input-hints}
  @see-slot{gtk:im-context-input-purpose}
  @see-class{gtk:im-context-simple}
  @see-class{gtk:im-multicontext}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- im-context-input-hints ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'im-context) t)
 "The @code{input-hints} property of type @symbol{gtk:input-hints}
  (Read / Write) @br{}
  Hints for the text field behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'im-context-input-hints)
      "Accessor"
      (documentation 'im-context-input-hints 'function)
 "@version{#2020-9-15}
  @syntax[]{(gtk:im-context-input-hints object) => hints}
  @syntax[]{(setf (gtk:im-context-input-hints object) hints)}
  @argument[object]{a @class{gtk:im-context} object}
  @argument[hints]{a value of the @symbol{gtk:input-hints} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:im-context]{input-hints} slot of the
    @class{gtk:im-context} class.
  @end{short}

  Hints for the text field behaviour.
  @see-class{gtk:im-context}")

;;; --- im-context-input-purpose -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose"
                                               'im-context) t)
 "The @code{input-purpose} property of type @symbol{gtk:input-purpose}
  (Read / Write) @br{}
  Purpose of the text field. @br{}
  Default value: @code{:free-from}")

#+liber-documentation
(setf (liber:alias-for-function 'im-context-input-purpose)
      "Accessor"
      (documentation 'im-context-input-purpose 'function)
 "@version{#2020-9-15}
  @syntax[]{(gtk:im-context-input-purpose object) => purpose}
  @syntax[]{(setf (gtk:im-context-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:im-context} object}
  @argument[purpose]{a value of the @symbol{gtk:input-purpose}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:im-context]{input-purpose} slot of the
    @class{gtk:im-context} class.
  @end{short}

  Purpose of the text field.
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
;;; Allow an input method to internally handle key press and release events. If
;;; this function returns TRUE, then no further processing should be done for
;;; this key event.
;;;
;;; context :
;;;     a GtkIMContext
;;;
;;; event :
;;;     the key event
;;;
;;; Returns :
;;;     TRUE if the input method handled the key event.
;;; ----------------------------------------------------------------------------

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
;;;     the client widget. This may be NULL to indicate that the previous client
;;;     widget no longer exists.
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
;;; feedback. If use_preedit is FALSE (default is TRUE), then the IM context may
;;; use some other method to display feedback, such as displaying it in a child
;;; of the root window.
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
;;; Sets surrounding context around the insertion point and preedit string. This
;;; function is expected to be called in response to the
;;; GtkIMContext::retrieve_surrounding signal, and will likely have no effect if
;;; called at other times.
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

;;; --- End of file gtk4.im-context.lisp ---------------------------------------
