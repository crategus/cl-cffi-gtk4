;;; ----------------------------------------------------------------------------
;;; gtk4.style-context.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
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
;;; GtkStyleContext
;;;
;;;     Stores styling information affecting a widget
;;;
;;; Types and Values
;;;
;;;     GtkStyleContext
;;;     GtkStyleContextPrintFlags
;;;
;;;     GtkBorderStyle                                     gtk.enumerations.lisp
;;;     GtkBorder
;;;
;;; Accessors
;;;
;;;     gtk_style_context_get_display
;;;     gtk_style_context_set_display
;;;
;;; Functions
;;;
;;;     gtk_style_context_add_provider
;;;     gtk_style_context_add_provider_for_display
;;;     gtk_style_context_get_state
;;;     gtk_style_context_set_state
;;;     gtk_style_context_get_color
;;;     gtk_style_context_get_border
;;;     gtk_style_context_get_padding
;;;     gtk_style_context_get_margin
;;;     gtk_style_context_lookup_color
;;;     gtk_style_context_remove_provider
;;;     gtk_style_context_remove_provider_for_display
;;;     gtk_style_context_restore
;;;     gtk_style_context_save
;;;     gtk_style_context_add_class
;;;     gtk_style_context_remove_class
;;;     gtk_style_context_has_class
;;;     gtk_style_context_set_scale
;;;     gtk_style_context_get_scale
;;;     gtk_style_context_to_string
;;;
;;;     gtk_border_new
;;;     gtk_border_copy
;;;     gtk_border_free
;;;
;;;     gtk_render_activity
;;;     gtk_render_arrow
;;;     gtk_render_background
;;;     gtk_render_check
;;;     gtk_render_expander
;;;     gtk_render_focus
;;;     gtk_render_frame
;;;     gtk_render_handle
;;;     gtk_render_icon
;;;     gtk_render_layout
;;;     gtk_render_line
;;;     gtk_render_option
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkStyleContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBorder
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct border "GtkBorder"
  (:export t
   :type-initializer "gtk_border_get_type")
  (left :int16 :initform 0)
  (right :int16 :initform 0)
  (top :int16 :initform 0)
  (bottom :int16 :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'border)
      "GBoxed"
      (documentation 'border 'type)
 "@version{2025-07-22}
  @begin{declaration}
(glib:define-gboxed-cstruct border \"GtkBorder\"
  (:export t
   :type-initializer \"gtk_border_get_type\")
  (left :int16 :initform 0)
  (right :int16 :initform 0)
  (top :int16 :initform 0)
  (bottom :int16 :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[left]{The width of the left border.}
      @entry[right]{The width of the right border.}
      @entry[top]{The width of the top border.}
      @entry[bottom]{The width of the bottom border.}
    @end{simple-table}
  @end{values}
  @begin{short}
    A structure that specifies a border around a rectangular area that can be
    of different width on each side.
  @end{short}
  @see-slot{gtk:border-left}
  @see-slot{gtk:border-right}
  @see-slot{gtk:border-top}
  @see-slot{gtk:border-bottom}
  @see-constructor{gtk:border-new}
  @see-constructor{gtk:border-copy}")

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkBorder
;;; ----------------------------------------------------------------------------

;;; --- gtk:border-left --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-left)
      "Accessor"
      (documentation 'border-left 'function)
 "@version{2025-07-19}
  @syntax{(gtk:border-left instance) => left}
  @syntax{(setf (gtk:border-left instance) left)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[left]{an integer for the width of the left border}
  @begin{short}
    Accessor of the @code{left} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-left)

;;; --- gtk:border-right -------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-right)
      "Accessor"
      (documentation 'border-right 'function)
 "@version{2025-07-19}
  @syntax{(gtk:border-right instance) => right}
  @syntax{(setf (gtk:border-right instance) right)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[right]{an integer for the width of the right border}
  @begin{short}
    Accessor of the @code{right} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-right)

;;; --- gtk:border-top ---------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-top)
      "Accessor"
      (documentation 'border-top 'function)
 "@version{2025-07-19}
  @syntax{(gtk:border-top instance) => top}
  @syntax{(setf (gtk:border-top instance) top)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[top]{an integer for the width of the top border}
  @begin{short}
    Accessor of the @code{top} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-top)

;;; --- gtk:border-bottom ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-bottom)
      "Accessor"
      (documentation 'border-bottom 'function)
 "@version{2025-07-19}
  @syntax{(gtk:border-top instance) => bottom}
  @syntax{(setf (gtk:border-top instance) bottom)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[bottom]{an integer for the width of the bottom border}
  @begin{short}
    Accessor of the @code{bottom} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-bottom)

;;; ----------------------------------------------------------------------------
;;; gtk_border_new
;;; ----------------------------------------------------------------------------

(declaim (inline border-new))

(defun border-new (&key (left 0) (right 0) (top 0) (bottom 0))
 #+liber-documentation
 "@version{2025-07-19}
  @argument[left]{an integer for the width of the left border}
  @argument[right]{an integer for the width of the right border}
  @argument[top]{an integer for the width of the top border}
  @argument[bottom]{an integer for the width of the bottom border}
  @return{The newly allocated @class{gtk:border} instance.}
  @begin{short}
    Allocates a new @class{gtk:border} instance and initializes its elements.
  @end{short}
  @see-class{gtk:border}"
  (make-border :left left :right right :top top :bottom bottom))

(export 'border-new)

;;; ----------------------------------------------------------------------------
;;; gtk_border_copy
;;; ----------------------------------------------------------------------------

(declaim (inline border-copy))

(defun border-copy (border)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[border]{a @class{gtk:border} instance}
  @return{The @class{gtk:border} instance for the copy of @arg{border}.}
  @short{Copies a @class{gtk:border} instance.}
  @see-class{gtk:border}"
  (copy-border border))

(export 'border-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_border_free ()
;;;
;;; void gtk_border_free (GtkBorder *border_);
;;;
;;; Frees a GtkBorder structure.
;;;
;;; border_ :
;;;     a GtkBorder
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkStyleContextPrintFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkStyleContextPrintFlags" style-context-print-flags
  (:export t
   :type-initializer "gtk_style_context_print_flags_get_type")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1))
  (:show-change #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'style-context-print-flags)
      "GFlags"
      (liber:symbol-documentation 'style-context-print-flags)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-gflags \"GtkStyleContextPrintFlags\" style-context-print-flags
  (:export t
   :type-initializer \"gtk_style_context_print_flags_get_type\")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1))
  (:show-change #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{}
      @entry[:recurse]{Print the entire tree of CSS nodes starting at the node
        of the style context.}
      @entry[:show-style]{Show the values of the CSS properties for each node.}
      @entry[:show-change]{Show information about what changes affect the
        styles.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Flags that modify the behavior of the @fun{gtk:style-context-to-string}
    function.
  @end{short}
  New values may be added to this enumeration.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-to-string}")

;;; ----------------------------------------------------------------------------
;;; GtkStyleContext
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStyleContext" style-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_style_context_get_type")
  ((display
    style-context-display
    "display" "GdkDisplay" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj style-context) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT is deprecated since 4.10.")))

#+liber-documentation
(setf (documentation 'style-context 'type)
 "@version{2023-08-30}
  @begin{short}
    The @class{gtk:style-context} object stores styling information affecting a
    widget.
  @end{short}
  In order to construct the final style information, the
  @class{gtk:style-context} object queries information from all attached
  @class{gtk:style-provider} objects. Style providers can be either attached
  explicitly to the style context through the
  @fun{gtk:style-context-add-provider} function, or to the display through the
  @fun{gtk:style-context-add-provider-for-display} function. The resulting style
  is a combination of all information of the style provider in priority order.

  For GTK widgets, any @class{gtk:style-context} object returned by the
  @fun{gtk:widget-style-context} function will already have a
  @class{gdk:display} instance and a text direction information set. The style
  context will be also updated automatically if any of these settings change on
  the widget.

  @subheading{Style Classes}
  Widgets can add style classes to their style context, which can be used to
  associate different styles by class. The documentation for individual widgets
  lists which style classes it uses itself, and which style classes may be added
  by applications to affect their appearance.

  @subheading{Custom styling in UI libraries and applications}
  If you are developing a library with custom widgets that render differently
  than standard components, you may need to add a @class{gtk:style-provider}
  object yourself with the @var{gtk:+priority-fallback+} priority, either a
  @class{gtk:css-provider} object or a custom object implementing the
  @class{gtk:style-provider} interface. This way themes may still attempt to
  style your UI elements in a different way if needed so.

  If you are using custom styling on an application, you probably want then to
  make your style information prevail to the style information of the theme, so
  you must use a @class{gtk:style-provider} object with the
  @var{gtk:+priority-application+} priority. Keep in mind that the user settings
  in @file{XDG_CONFIG_HOME/gtk-4.0/gtk.css} will still take precedence over your
  changes, as it uses the @var{gtk:+priority-user+} priority.
  @begin[Warning]{dictionary}
    The @class{gtk:style-context} implementation is deprecated since 4.10.
    The relevant API has been moved to the @class{gtk:widget} implementation
    where applicable. Otherwise, there is no replacement for querying the style
    machinery. Stylable UI elements should use widgets.
  @end{dictionary}
  @see-slot{gtk:style-context-display}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-provider}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:style-context-display ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'style-context) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The associated display of the style context.")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-display)
      "Accessor"
      (documentation 'style-context-display 'function)
 "@version{2023-09-18}
  @syntax{(gtk:style-context-display object) => display}
  @syntax{(setf (gtk:style-context-display object) display)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{display} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The @fun{gtk:style-context-display} function returns the display to which the
  style context is attached. The @setf{gtk:style-context-display} function
  attaches the style context to the given display.

  The display is used to add style information from 'global' style providers,
  such as the @class{gtk:settings} instance of the display. If you are using a
  @class{gtk:style-context} object returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the @fun{gtk:widget-display}
    function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:display}
  @see-class{gtk:settings}
  @see-function{gtk:widget-style-context}
  @see-function{gtk:widget-display}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-add-provider))

(cffi:defcfun ("gtk_style_context_add_provider" %style-context-add-provider)
    :void
  (context (g:object style-context))
  (provider (g:object style-provider))
  (priority :uint))

(defun style-context-add-provider (context provider priority)
 #+liber-documentation
 "@version{2025-07-19}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an unsigned integer for the priority of the style
    provider}
  @begin{short}
    Adds a style provider to the style context, to be used in style
    construction.
  @end{short}
  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{gtk:+priority-fallback+} and @var{gtk:+priority-user+} priorities.

  Note that a style provider added by this function only affects the style of
  the widget to which @arg{context} belongs. If you want to affect the style of
  all widgets, use the @fun{gtk:style-context-add-provider-for-display}
  function.
  @begin[Notes]{dictionary}
    If both priorities are the same, a style provider object added through this
    function takes precedence over another added through the
    @fun{gtk:style-context-add-provider-for-display} function.
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use style classes instead. See
    the @fun{gtk:style-context-add-provider-for-display} documentation for an
    example.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider-for-display}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-ADD-PROVIDER is deprecated since 4.10."))
  (%style-context-add-provider context provider priority))

(export 'style-context-add-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_add_provider_for_display"
               %style-context-add-provider-for-display) :void
  (display (g:object gdk:display))
  (provider (g:object style-provider))
  (priority :uint))

(defun style-context-add-provider-for-display
    (display provider &optional (priority +priority-application+))
 #+liber-documentation
 "@version{2025-07-19}
  @argument[display]{a @class{gdk:display} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an optional unsigned integer for the priority of the
    style provider, the default value is @var{gtk:+priority-application+}}
  @begin{short}
    Adds a global style provider to the display, which will be used in style
    construction for all style contexts under the display.
  @end{short}
  GTK uses this to make styling information from the @class{gtk:settings}
  object available.
  @begin[Examples]{dictionary}
    Change the color and the font in a text view: Create a provider, load CSS
    into the provider, add the style class and the provider to the parent
    window.
    @begin{pre}
(let ((provider (gtk:css-provider-new)))
  (gtk:css-provider-load-from-data provider
                                   \".viewstyle textview {
                                      color : Green;
                                      font : 20px Purisa; @}\")
  (gtk:widget-add-css-class window \"viewstyle\")
  (gtk:style-context-add-provider-for-display (gtk:widget-display window)
                                              provider)
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Notes]{dictionary}
    If both priorities are the same, a style provider object added through the
    @fun{gtk:style-context-add-provider} function takes precedence over another
    added through this function.
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-function{gtk:style-context-add-provider}"
  (%style-context-add-provider-for-display display provider priority))

(export 'style-context-add-provider-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_state
;;; gtk_style_context_set_state
;;; ----------------------------------------------------------------------------

(defun (setf style-context-state) (state context)
  (cffi:foreign-funcall "gtk_style_context_set_state"
                        (g:object style-context) context
                        state-flags state
                        :void)
  state)

(cffi:defcfun ("gtk_style_context_get_state" %style-context-state) state-flags
  (context (g:object style-context)))

(defun style-context-state (context)
 #+liber-documentation
 "@version{#2025-07-26}
  @syntax{(gtk:style-context-state context) => state}
  @syntax{(setf (gtk:style-context-state context) state)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @sym{gtk:state-flags} flags to represent}
  @begin{short}
    Accessor of the state used when rendering.
  @end{short}
  The @fun{gtk:style-context-state} function returns the state used for style
  matching. The @setf{gtk:style-context-state} function sets the state.

  This function should only be used to retrieve the @sym{gtk:state-flags}
  values to pass to the @class{gtk:style-context} functions, like the
  @fun{gtk:style-context-padding} function. If you need to retrieve the current
  state of a @class{gtk:widget} object, use the @fun{gtk:widget-state-flags}
  function.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the @fun{gtk:widget-state-flags}
    function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:widget}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-state-flags}
  @see-function{gtk:style-context-padding}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-STATE is deprecated since 4.10."))
  (%style-context-state context))

(export 'style-context-state)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_color" %style-context-color) :void
  (context (g:object style-context))
  (color (g:boxed gdk:rgba)))

(defun style-context-color (context)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @return{The @class{gdk:rgba} instance for the foreground color.}
  @begin{short}
    Gets the foreground color for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the @fun{gtk:widget-color}
    function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:rgba}
  @see-function{gtk:widget-color}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-COLOR is deprecated since 4.10."))
  (let ((color (gdk:rgba-new)))
    (%style-context-color context color)
    color))

(export 'style-context-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_border" %style-context-border) :void
  (context (g:object style-context))
  (state state-flags)
  (border (g:boxed border)))

(defun style-context-border (context state)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @sym{gtk:state-flags} value to retrieve the border for}
  @return{Returns border settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the border settings for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-BORDER is deprecated since 4.10."))
  (let ((border (border-new)))
    (%style-context-border context state border)
    border))

(export 'style-context-border)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_padding
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_padding" %style-context-padding) :void
  (context (g:object style-context))
  (state state-flags)
  (padding (g:boxed border)))

(defun style-context-padding (context state)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @sym{gtk:state-flags} value to retrieve the padding for}
  @return{Returns padding settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the padding settings for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-PADDING is deprecated since 4.10."))
  (let ((padding (border-new)))
    (%style-context-padding context state padding)
    padding))

(export 'style-context-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_margin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_get_margin" %style-context-margin) :void
  (context (g:object style-context))
  (state state-flags)
  (margin (g:boxed border)))

(defun style-context-margin (context state)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @sym{gtk:state-flags} value to retrieve the margin for}
  @return{Returns margin settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the margin settings for a given state.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-MARGIN is deprecated since 4.10."))
  (let ((margin (border-new)))
    (%style-context-margin context state margin)
    margin))

(export 'style-context-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_lookup_color" %style-context-lookup-color)
    :boolean
  (context (g:object style-context))
  (name :string)
  (color (g:boxed gdk:rgba)))

(defun style-context-lookup-color (context name)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[name]{a string for a color name to lookup}
  @return{The looked up @class{gdk:rgba} color, or @code{nil}.}
  @begin{short}
    Looks up and resolves a color name in the style context color map.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:rgba}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-LOOKUP-COLOR is deprecated since 4.10."))
  (let ((color (gdk:rgba-new)))
    (when (%style-context-lookup-color context name color)
      color)))

(export 'style-context-lookup-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-remove-provider))

(cffi:defcfun ("gtk_style_context_remove_provider"
               %style-context-remove-provider) :void
  (context (g:object style-context))
  (provider (g:object style-provider)))

(defun style-context-remove-provider (context provider)
 #+liber-documentation
 "@version{#2023-09-17}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @begin{short}
    Removes the style provider from the style providers list in the style
    context.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-REMOVE-PROVIDER is deprecated since 4.10."))
  (%style-context-remove-provider context provider))

(export 'style-context-remove-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider_for_display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_style_context_remove_provider_for_display"
               style-context-remove-provider-for-display) :void
 #+liber-documentation
 "@version{2023-09-17}
  @argument[display]{a @class{gdk:display} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @begin{short}
    Removes the style provider from the global style providers list in the
    display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider-for-display}"
  (display (g:object gdk:display))
  (provider (g:object style-provider)))

(export 'style-context-remove-provider-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_restore
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-restore))

(cffi:defcfun ("gtk_style_context_restore" %style-context-restore) :void
  (context (g:object style-context)))

(defun style-context-restore (context)
 #+liber-documentation
 "@version{#2023-09-17}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Restores the style context state to a previous stage.
  @end{short}
  See the @fun{gtk:style-context-save} function.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-save}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-RESTORE is deprecated since 4.10."))
  (%style-context-restore context))

(export 'style-context-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-save))

(cffi:defcfun ("gtk_style_context_save" %style-context-save) :void
  (context (g:object style-context)))

(defun style-context-save (context)
 #+liber-documentation
 "@version{#2023-09-17}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Saves the style context state.
  @end{short}
  So all modifications done through the @fun{gtk:style-context-add-class},
  @fun{gtk:style-context-remove-class} or @fun{gtk:style-context-state}
  functions can be reverted in one go through the
  @fun{gtk:style-context-restore} function.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-add-class}
  @see-function{gtk:style-context-remove-class}
  @see-function{gtk:style-context-state}
  @see-function{gtk:style-context-restore}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-SAVE is deprecated since 4.10."))
  (%style-context-save context))

(export 'style-context-save)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_class
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-add-class))

(cffi:defcfun ("gtk_style_context_add_class" %style-context-add-class) :void
  (context (g:object style-context))
  (classname :string))

(defun style-context-add-class (context classname)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string for a class name to use in styling}
  @begin{short}
    Adds a style class to the style context, so later uses of the style context
    will make use of this new class for styling.
  @end{short}
  @begin[Examples]{dictionary}
    In the CSS file format, a GtkEntry defining an \"entry\" class, would be
    matched by:
    @begin{pre}
GtkEntry.entry { ... @}
  @end{pre}
  While any widget defining an \"entry\" class would be matched by:
  @begin{pre}
.entry { ... @}
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the
    @fun{gtk:widget-add-css-class} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:widget-add-css-class}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-ADD-CLASS is deprecated since 4.10."))
  (%style-context-add-class context classname))

(export 'style-context-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-remove-class))

(cffi:defcfun ("gtk_style_context_remove_class" %style-context-remove-class)
    :void
  (context (g:object style-context))
  (classname :string))

(defun style-context-remove-class (context classname)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string for a class name to remove}
  @begin{short}
    Removes a class name from the style context.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the
    @fun{gtk:widget-remove-css-class} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:widget-remove-css-class}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-REMOVE-CLASS is deprecated since 4.10."))
  (%style-context-remove-class context classname))

(export 'style-context-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-has-class))

(cffi:defcfun ("gtk_style_context_has_class" %style-context-has-class) :boolean
  (context (g:object style-context))
  (classname :string))

(defun style-context-has-class (context classname)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string for a class name}
  @return{@em{True} if the style context has @arg{classname} defined.}
  @begin{short}
    Returns @em{true} if the style context currently has defined the given
    class name.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the
    @fun{gtk:widget-has-css-class} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:widget-hass-css-class}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-HAS-CLASS is deprecated since 4.10."))
  (%style-context-has-class context classname))

(export 'style-context-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_scale
;;; gtk_style_context_set_scale
;;; ----------------------------------------------------------------------------

(defun (setf style-context-scale) (scale context)
  (cffi:foreign-funcall "gtk_style_context_set_scale"
                        (g:object style-context) context
                        :int scale
                        :void)
  scale)

(cffi:defcfun ("gtk_style_context_get_scale" %style-context-scale) :int
  (context (g:object style-context)))

(defun style-context-scale (context)
 #+liber-documentation
 "@version{#2025-07-19}
  @syntax{(gtk:style-context-scale context) => scale}
  @syntax{(setf (gtk:style-context-scale context) scale)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[scale]{an integer for a scale}
  @begin{short}
    Accessor of the scale used for image assets for the style context.
  @end{short}
  The @fun{gtk:style-context-scale} function returns the scale to use when
  getting image assets for the style. The @setf{gtk:style-context-scale}
  function sets the scale.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Use the
    @fun{gtk:widget-scale-factor} function instead.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-function{gtk:widget-scale-factor}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-SCALE is deprecated since 4.10."))
  (%style-context-scale context))

(export 'style-context-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_to_string
;;; ----------------------------------------------------------------------------

(declaim (inline style-context-to-string))

(cffi:defcfun ("gtk_style_context_to_string" %style-context-to-string) :string
  (context (g:object style-context))
  (flags style-context-print-flags))

(defun style-context-to-string (context flags)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[flags]{a @sym{gtk:style-context-print-flags} value that
    determine what to print}
  @return{The string representing the style context.}
  @begin{short}
    Converts the style context into a string representation.
  @end{short}
  The string representation always includes information about the name, state,
  ID, visibility and style classes of the CSS node that is backing the style
  context. Depending on the flags, more information may be included.

  This function is intended for testing and debugging of the CSS implementation
  in GTK. There are no guarantees about the format of the returned string, it
  may change.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq context
      (gtk:widget-style-context (make-instance 'gtk:dialog)))
=> #<GTK:STYLE-CONTEXT {1001C70663@}>
(gtk:style-context-to-string context :recurse)
=>
\"[window.background.dialog:dir(ltr)]
  box.dialog-vbox.vertical:dir(ltr)
    box.vertical:dir(ltr)
    box.dialog-action-box.horizontal:dir(ltr)
      box.dialog-action-area.horizontal:dir(ltr)
\"
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. This API will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:style-context-print-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:STYLE-CONTEXT-TO-STRING is deprecated since 4.10."))
  (%style-context-to-string context flags))

(export 'style-context-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_activity" %render-activity) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-activity (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders an activity area such as in the @class{gtk:spinner} widget or the
    fill line in the @class{gtk:range} widget.
  @end{short}
  The @val[gtk:state-flags]{:active} state of the @sym{gtk:state-flags} flags
  determines whether there is activity going on.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:spinner}
  @see-class{gtk:range}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-ACTIVITY is deprecated since 4.10."))
  (%render-activity context cr (coerce x 'double-float)
                               (coerce y 'double-float)
                               (coerce width 'double-float)
                               (coerce height 'double-float)))

(export 'render-activity)

;;; ----------------------------------------------------------------------------
;;; gtk_render_arrow
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_arrow" %render-arrow) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (angle :double)
  (x :double)
  (y :double)
  (size :double))

(defun render-arrow (context cr angle x y size)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[angle]{a number coerced to a double float for the arrow angle
    from 0 to 2 * Pi, being 0 the arrow pointing to the north}
  @argument[x]{a number coerced to a double float for the x origin of the
    render area}
  @argument[y]{a number coerced to a double float for the y origin of the
    render area}
  @argument[size]{a number coerced to a double float for the square side of
    the render area}
  @begin{short}
    Renders an arrow pointing to an angle.
  @end{short}

  Typical arrow rendering at 0, 1/2 Pi, Pi, and 3/2 Pi: @br{}
  @image[render-arrow]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-ARROW is deprecated since 4.10."))
  (%render-arrow context cr (coerce angle 'double-float)
                            (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce size 'double-float)))

(export 'render-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_background" %render-background) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-background (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    render area}
  @argument[y]{a number coerced to a double float for the y origin of the
    render area}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders the background of an element.
  @end{short}

  Typical background rendering, showing the effect of @code{background-image},
  @code{border-width} and @code{border-radius}: @br{}
  @image[render-background]{}
  @begin[Warning]{dictionary}
    The function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-BACKGROUND is deprecated since 4.10."))
  (%render-background context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'render-background)

;;; ----------------------------------------------------------------------------
;;; gtk_render_check
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_check" %render-check) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-check (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders a checkmark as in a @class{gtk:check-button} widget.
  @end{short}
  The @val[gtk:state-flags]{:checked} state of the @sym{gtk:state-flags} flags
  determines whether the check is on or off, and the
  @val[gtk:state-flags]{:inconsistent} state determines whether it should be
  marked as undefined.

  Typical checkmark rendering: @br{}
  @image[render-check]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:check-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-CHECK is deprecated since 4.10."))
  (%render-check context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-check)

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_expander" %render-expander) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-expander (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders an expander as used in the @class{gtk:tree-view} widget and
    @class{gtk:expander} widget in the area defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  The @val[gtk:state-flags]{:active} state of the @sym{gtk:state-flags} flags
  determines whether the expander is collapsed or expanded.

  Typical expander rendering: @br{}
  @image[render-expander]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:expander}
  @see-class{gtk:tree-view}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-EXPANDER is deprecated since 4.10."))
  (%render-expander context cr (coerce x 'double-float)
                               (coerce y 'double-float)
                               (coerce width 'double-float)
                               (coerce height 'double-float)))

(export 'render-expander)

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_focus" %render-focus) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-focus (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders a focus indicator on the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Typical focus rendering: @br{}
  @image[render-focus]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-FOCUS is deprecated since 4.10."))
  (%render-focus context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_frame" %render-frame) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-frame (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Examples of frame rendering, showing the effect of @code{border-image},
  @code{border-color}, @code{border-width}, @code{border-radius} and
  @code{junctions}: @br{}
  @image[render-frame]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-FRAME is deprecated since 4.10."))
  (%render-frame context cr (coerce x 'double-float)
                            (coerce y 'double-float)
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'render-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_handle" %render-handle) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-handle (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders a handle, as the resize grip of the @class{gtk:paned} widget and
    @class{gtk:window} widget, in the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Handles rendered for the paned and grip classes: @br{}
  @image[render-handle]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}  @see-class{gtk:style-context}
  @see-class{gtk:paned}
  @see-class{gtk:window}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-HANDLE is deprecated since 4.10."))
  (%render-handle context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             (coerce width 'double-float)
                             (coerce height 'double-float)))

(export 'render-handle)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_icon" %render-icon) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (texture (g:object gdk:texture))
  (x :double)
  (y :double))

(defun render-icon (context cr texture x y)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[texture]{a @class{gdk:texture} object containing the icon to draw}
  @argument[x]{a number coerced to a double float for the x position for the
    texture}
  @argument[y]{a number coerced to a double float for the y position for the
    texture}
  @begin{short}
    Renders the icon in a texture at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  This function will render the icon in texture at exactly its size, regardless
  of scaling factors, which may not be appropriate when drawing on displays with
  high pixel densities.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-class{gdk:texture}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-ICON is deprecated since 4.10."))
  (%render-icon context cr texture (coerce x 'double-float)
                                   (coerce y 'double-float)))

(export 'render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_layout" %render-layout) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (layout (g:object pango:layout)))

(defun render-layout (context cr x y layout)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double float for the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[layout]{a @sym{pango:layout} object to render}
  @begin{short}
    Renders a Pango layout on the coordinates @arg{x}, @arg{y}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-symbol{pango:layout}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-LAYOUT is deprecated since 4.10."))
  (%render-layout context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             layout))

(export 'render-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_render_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_line" %render-line) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun render-line (context cr x0 y0 x1 y1)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x0]{a number coerced to a double float for the x coordinate of
    the origin of the line}
  @argument[y0]{a number coerced to a double float for the y coordinate of
    the origin of the line}
  @argument[x1]{a number coerced to a double float for the x coordinate of
    the end of the line}
  @argument[y1]{a number coerced to a double float for the y coordinate of
    the end of the line}
  @begin{short}
    Renders a line from @code{(x0,y0)} to @code{(x1,y1)}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-LINE is deprecated since 4.10."))
  (%render-line context cr (coerce x0 'double-float)
                           (coerce y0 'double-float)
                           (coerce x1 'double-float)
                           (coerce y1 'double-float)))

(export 'render-line)

;;; ----------------------------------------------------------------------------
;;; gtk_render_option
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_render_option" %render-option) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-option (context cr x y width height)
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @sym{cairo:context-t} context}
  @argument[x]{a number coerced to a double floatf or the x origin of the
    rectangle}
  @argument[y]{a number coerced to a double float for the y origin of the
    rectangle}
  @argument[width]{a number coerced to a double float for the rectangle width}
  @argument[height]{a number coerced to a double float for the rectangle height}
  @begin{short}
    Renders an option mark as in a @class{gtk:check-button} widget.
  @end{short}
  The @val[gtk:state-flags]{:checked} state of the @sym{gtk:state-flags} flags
  will determine whether the option is on or off, and the
  @val[gtk:state-flags]{:inconsistent} state whether it should be marked as
  undefined.

  Typical option mark rendering: @br{}
  @image[render-option]{}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.10. Please do not use it in newly
    written code.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:check-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:RENDER-OPTION is deprecated since 4.10."))
  (%render-option context cr (coerce x 'double-float)
                             (coerce y 'double-float)
                             (coerce width 'double-float)
                             (coerce height 'double-float)))

(export 'render-option)

;;; --- End of file gtk4.style-context.lisp ------------------------------------
