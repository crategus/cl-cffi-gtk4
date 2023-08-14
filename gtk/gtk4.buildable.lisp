;;; ----------------------------------------------------------------------------
;;; gtk4.buildable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkBuildable
;;;
;;;     Interface for objects that can be built by GtkBuilder
;;;
;;; Types and Values
;;;
;;;     GtkBuildable
;;;     GtkBuildableParser                                 not implemented
;;;
;;; Functions
;;;
;;;     gtk_buildable_get_buildable_id
;;;
;;;     gtk_buildable_parse_context_get_element            not implemented
;;;     gtk_buildable_parse_context_get_element_stack      not implemented
;;;     gtk_buildable_parse_context_get_position           not implemented
;;;     gtk_buildable_parse_context_pop                    not implemented
;;;     gtk_buildable_parse_context_push                   not implemented
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBuildable
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkBuildable" buildable
  (:export t
   :type-initializer "gtk_buildable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'buildable)
      "Interface"
      (documentation 'buildable 'type)
 "@version{2023-8-7}
  @begin{short}
    The @sym{gtk:buildable} interface allows objects to extend and customize
    their deserialization from @class{gtk:builder} UI descriptions.
  @end{short}
  The interface includes methods for setting names and properties of objects,
  parsing custom tags and constructing child objects.

  The @sym{gtk:buildable} interface is implemented by all widgets and many of
  the non-widget objects that are provided by GTK. The main user of this
  interface is the @class{gtk:builder} class. There should be very little need
  for applications to call any of these functions directly.

  An object only needs to implement this interface if it needs to extend the
  @class{gtk:builder} format or run any extra routines at deserialization time.
  @see-class{gtk:builder}")

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_get_buildable_id -> buildable-buildable-id
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_buildable_get_buildable_id" buildable-buildable-id) :string
 #+liber-documentation
 "@version{2023-8-7}
  @argument[buildable]{a @class{gtk:buildable} object}
  @return{A string with the ID of the buildable object.}
  @begin{short}
    Gets the ID of the buildable object.
  @end{short}
  The @class{gtk:builder} class sets the name based on the ID attribute of the
  tag used to construct the buildable object.
  @see-class{gtk:buildable}"
  (buildable (g:object buildable)))

(export 'buildable-buildable-id)

;;; ----------------------------------------------------------------------------
;;; struct GtkBuildableParser
;;;
;;; struct GtkBuildableParser {
;;;   /* Called for open tags <foo bar="baz"> */
;;;   void (*start_element)  (GtkBuildableParseContext *context,
;;;                           const char               *element_name,
;;;                           const char              **attribute_names,
;;;                           const char              **attribute_values,
;;;                           gpointer                  user_data,
;;;                           GError                  **error);
;;;
;;;   /* Called for close tags </foo> */
;;;   void (*end_element)    (GtkBuildableParseContext *context,
;;;                           const char               *element_name,
;;;                           gpointer                  user_data,
;;;                           GError                  **error);
;;;
;;;   /* Called for character data */
;;;   /* text is not nul-terminated */
;;;   void (*text)           (GtkBuildableParseContext *context,
;;;                           const char               *text,
;;;                           gsize                     text_len,
;;;                           gpointer                  user_data,
;;;                           GError                  **error);
;;;
;;;   /* Called on error, including one set by other
;;;    * methods in the vtable. The GError should not be freed.
;;;    */
;;;   void (*error)          (GtkBuildableParseContext *context,
;;;                           GError                   *error,
;;;                           gpointer                 user_data);
;;; };
;;;
;;; A sub-parser for GtkBuildable implementations.
;;;
;;; start_element () :
;;;     function called for open elements
;;;
;;; end_element () :
;;;     function called for close elements
;;;
;;; text () :
;;;     function called for character data
;;;
;;; error () :
;;;     function called on error
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parse_context_get_element ()
;;;
;;; const char *
;;; gtk_buildable_parse_context_get_element
;;;                                (GtkBuildableParseContext *context);
;;;
;;; Retrieves the name of the currently open element.
;;;
;;; If called from the start_element or end_element handlers this will give the
;;; element_name as passed to those functions. For the parent elements, see
;;; gtk_buildable_parse_context_get_element_stack().
;;;
;;; context :
;;;     a GtkBuildablParseContext
;;;
;;; Returns
;;;     the name of the currently open element, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parse_context_get_element_stack ()
;;;
;;; GPtrArray *
;;; gtk_buildable_parse_context_get_element_stack
;;;                                (GtkBuildableParseContext *context);
;;;
;;; Retrieves the element stack from the internal state of the parser.
;;;
;;; The returned GPtrArray is an array of strings where the last item is the
;;; currently open tag (as would be returned by
;;; gtk_buildable_parse_context_get_element()) and the previous item is its
;;; immediate parent.
;;;
;;; This function is intended to be used in the start_element and end_element
;;; handlers where gtk_buildable_parse_context_get_element() would merely
;;; return the name of the element that is being processed.
;;;
;;; context :
;;;     a GtkBuildableParseContext
;;;
;;; Returns :
;;;     the element stack, which must not be modified.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parse_context_get_position ()
;;;
;;; void
;;; gtk_buildable_parse_context_get_position
;;;                                (GtkBuildableParseContext *context,
;;;                                 int *line_number,
;;;                                 int *char_number);
;;;
;;; Retrieves the current line number and the number of the character on that
;;; line. Intended for use in error messages; there are no strict semantics for
;;; what constitutes the "current" line number other than "the best number we
;;; could come up with for error messages."
;;;
;;; context :
;;;     a GtkBuildableParseContext
;;;
;;; line_number :
;;;     return location for a line number, or NULL.
;;;
;;; char_number :
;;;     return location for a char-on-line number, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parse_context_pop ()
;;;
;;; gpointer
;;; gtk_buildable_parse_context_pop (GtkBuildableParseContext *context);
;;;
;;; Completes the process of a temporary sub-parser redirection.
;;;
;;; This function exists to collect the user_data allocated by a matching call
;;; to gtk_buildable_parse_context_push(). It must be called in the end_element
;;; handler corresponding to the start_element handler during which
;;; gtk_buildable_parse_context_push() was called. You must not call this
;;; function from the error callback -- the user_data is provided directly to
;;; the callback in that case.
;;;
;;; This function is not intended to be directly called by users interested in
;;; invoking subparsers. Instead, it is intended to be used by the subparsers
;;; themselves to implement a higher-level interface.
;;;
;;; context :
;;;     a GtkBuildableParseContext
;;;
;;; Returns :
;;;     the user data passed to gtk_buildable_parse_context_push()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_buildable_parse_context_push ()
;;;
;;; void
;;; gtk_buildable_parse_context_push (GtkBuildableParseContext *context,
;;;                                   const GtkBuildableParser *parser,
;;;                                   gpointer user_data);
;;;
;;; Temporarily redirects markup data to a sub-parser.
;;;
;;; This function may only be called from the start_element handler of a
;;; GtkBuildableParser. It must be matched with a corresponding call to
;;; gtk_buildable_parse_context_pop() in the matching end_element handler
;;; (except in the case that the parser aborts due to an error).
;;;
;;; All tags, text and other data between the matching tags is redirected to
;;; the subparser given by parser . user_data is used as the user_data for that
;;; parser. user_data is also passed to the error callback in the event that an
;;; error occurs. This includes errors that occur in subparsers of the
;;; subparser.
;;;
;;; The end tag matching the start tag for which this call was made is handled
;;; by the previous parser (which is given its own user_data) which is why
;;; gtk_buildable_parse_context_pop() is provided to allow "one last access" to
;;; the user_data provided to this function. In the case of error, the user_data
;;; provided here is passed directly to the error callback of the subparser and
;;; gtk_buildable_parse_context_pop() should not be called. In either case, if
;;; user_data was allocated then it ought to be freed from both of these
;;; locations.
;;;
;;; This function is not intended to be directly called by users interested in
;;; invoking subparsers. Instead, it is intended to be used by the subparsers
;;; themselves to implement a higher-level interface.
;;;
;;; For an example of how to use this, see g_markup_parse_context_push() which
;;; has the same kind of API.
;;;
;;; context :
;;;     a GtkBuildableParseContext
;;;
;;; parser :
;;;     a GtkBuildableParser
;;;
;;; user_data :
;;;     user data to pass to GtkBuildableParser functions
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.buildable.lisp ----------------------------------------
