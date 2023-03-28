;;; ----------------------------------------------------------------------------
;;; gtk4.style-context.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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

(define-g-boxed-cstruct border "GtkBorder"
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
 "@version{#2022-8-1}
  @begin{short}
    A structure that specifies a border around a rectangular area that can be
    of different width on each side.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct border \"GtkBorder\"
  (:export t
   :type-initializer \"gtk_border_get_type\")
  (left :int16 :initform 0)
  (right :int16 :initform 0)
  (top :int16 :initform 0)
  (bottom :int16 :initform 0))
  @end{pre}
  @begin[code]{table}
    @entry[left]{The width of the left border.}
    @entry[right]{The width of the right border.}
    @entry[top]{The width of the top border.}
    @entry[bottom]{The width of the bottom border.}
  @end{table}
  @see-slot{gtk:border-left}
  @see-slot{gtk:border-right}
  @see-slot{gtk:border-top}
  @see-slot{gtk:border-bottom}
  @see-constructor{gtk:border-new}
  @see-constructor{gtk:border-copy}")

;;; ----------------------------------------------------------------------------
;;; Accessors for GtkBorder
;;; ----------------------------------------------------------------------------

;;; --- border-left ------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-left)
      "Accessor"
      (documentation 'border-left 'function)
 "@version{#2022-8-1}
  @syntax[]{(gtk:border-left instance) => left}
  @syntax[]{(setf gtk:border-left instance) left)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[left]{an integer with the width of the left border}
  @begin{short}
    Accessor of the @code{left} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-left)

;;; --- border-right -----------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-right)
      "Accessor"
      (documentation 'border-right 'function)
 "@version{#2022-8-1}
  @syntax[]{(gtk:border-right instance) => right}
  @syntax[]{(setf gtk:border-right instance) right)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[right]{an integer with the width of the right border}
  @begin{short}
    Accessor of the @code{right} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-right)

;;; --- border-top -------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-top)
      "Accessor"
      (documentation 'border-top 'function)
 "@version{#2022-8-1}
  @syntax[]{(gtk:border-top instance) => top}
  @syntax[]{(setf gtk:border-top instance) top)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[top]{an integer with the width of the top border}
  @begin{short}
    Accessor of the @code{top} slot of the @class{gtk:border} structure.
  @end{short}
  @see-class{gtk:border}")

(export 'border-top)

;;; --- border-bottom ----------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'border-bottom)
      "Accessor"
      (documentation 'border-bottom 'function)
 "@version{#2022-8-1}
  @syntax[]{(gtk:border-top instance) => bottom}
  @syntax[]{(setf gtk:border-top instance) bottom)}
  @argument[instance]{a @class{gtk:border} instance}
  @argument[bottom]{an integer with the width of the bottom border}
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
 "@version{#2022-8-1}
  @argument[left]{an integer with the width of the left border}
  @argument[right]{an integer with the width of the right border}
  @argument[top]{an integer with the width of the top border}
  @argument[bottom]{an integer with the width of the bottom border}
  @return{A newly allocated @class{gtk:border} instance.}
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
 "@version{#2022-8-1}
  @argument[border]{a @class{gtk:border} instance}
  @return{A copy of @arg{border}.}
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

(define-g-flags "GtkStyleContextPrintFlags" style-context-print-flags
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
 "@version{#2022-8-1}
  @begin{short}
    Flags that modify the behavior of the @fun{gtk:style-context-to-string}
    function.
  @end{short}
  New values may be added to this enumeration.
  @begin{pre}
(define-g-flags \"GtkStyleContextPrintFlags\" style-context-print-flags
  (:export t
   :type-initializer \"gtk_style_context_print_flags_get_type\")
  (:none 0)
  (:recurse #.(ash 1 0))
  (:show-style #.(ash 1 1))
  (:show-change #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{}
    @entry[:recurse]{Print the entire tree of CSS nodes starting at the node
    of the style context.}
    @entry[:show-style]{Show the values of the CSS properties for each node.}
    @entry[:show-change]{Show information about what changes affect the styles.}
  @end{table}
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-to-string}")

;;; ----------------------------------------------------------------------------
;;; GtkStyleContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStyleContext" style-context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_style_context_get_type")
  ((display
    style-context-display
    "display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'style-context 'type)
 "@version{#2022-8-1}
  @begin{short}
    The @sym{gtk:style-context} object stores styling information affecting a
    widget.
  @end{short}

  In order to construct the final style information, the @sym{gtk:style-context}
  object queries information from all attached @class{gtk:style-provider}
  objects. Style providers can be either attached explicitly to the style
  context through the @fun{gtk:style-context-add-provider} function, or to the
  display through the @fun{gtk:style-context-add-provider-for-display} function.
  The resulting style is a combination of all information of the style provider
  in priority order.

  For GTK widgets, any @sym{gtk:style-context} object returned by the
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
  object yourself with the @var{+gtk-priority-fallback+} priority, either a
  @class{gtk:css-provider} object or a custom object implementing the
  @class{gtk:style-provider} interface. This way themes may still attempt to
  style your UI elements in a different way if needed so.

  If you are using custom styling on an application, you probably want then to
  make your style information prevail to the style information of the theme, so
  you must use a @class{gtk:style-provider} object with the
  @var{+gtk-priority-application+} priority. Keep in mind that the user settings
  in @file{XDG_CONFIG_HOME/gtk-4.0/gtk.css} will still take precedence over your
  changes, as it uses the @var{+gtk-priority-user+} priority.
  @see-slot{gtk:style-context-display}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-provider}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- style-context-display --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'style-context) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The associated display of the style context.")

#+liber-documentation
(setf (liber:alias-for-function 'style-context-display)
      "Accessor"
      (documentation 'style-context-display 'function)
 "@version{#2022-8-1}
  @syntax[]{(gtk:style-context-display object) => display}
  @syntax[]{(setf (gtk:style-context-display object) display)}
  @argument[object]{a @class{gtk:style-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gtk:style-context]{display} slot of the
    @class{gtk:style-context} class.
  @end{short}
  The @sym{gtk:style-context-display} function returns the display to which the
  style context is attached. The @sym{(setf gtk:style-context-display)} function
  attaches the style context to the given display.

  The display is used to add style information from 'global' style providers,
  such as the @class{gtk:settings} instance of the display. If you are using a
  @class{gtk:style-context} object returned from the
  @fun{gtk:widget-style-context} function, you do not need to call this
  yourself.
  @see-class{gtk:style-context}
  @see-class{gdk:display}
  @see-class{gtk:settings}
  @see-function{gtk:widget-style-context}")

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_provider" style-context-add-provider) :void
 #+liber-documentation
 "@version{2022-11-25}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a style provider to the style context, to be used in style
    construction.
  @end{short}
  The lower the priority of the style provider is, the earlier it will be used
  in the style construction. Typically this will be in the range between the
  @var{+gtk-priority-fallback+} and @var{+gtk-priority-user+} priorities.

  Note that a style provider added by this function only affects the style of
  the widget to which @arg{context} belongs. If you want to affect the style of
  all widgets, use the @fun{gtk:style-context-add-provider-for-display}
  function.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through this
    function takes precedence over another added through the
    @fun{gtk:style-context-add-provider-for-display} function.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider-for-display}"
  (context (g:object style-context))
  (provider (g:object style-provider))
  (priority :uint))

(export 'style-context-add-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_provider_for_display
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_provider_for_display"
           style-context-add-provider-for-display) :void
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[display]{a @class{gdk:display} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @argument[priority]{an unsigned integer with the priority of the style
    provider}
  @begin{short}
    Adds a global style provider to the display, which will be used in style
    construction for all style contexts under the display.
  @end{short}

  GTK uses this to make styling information from the @class{gtk:settings}
  object available.
  @begin[Note]{dictionary}
    If both priorities are the same, a style provider object added through the
    @fun{gtk:style-context-add-provider} function takes precedence over another
    added through this function.
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-class{gdk:display}
  @see-class{gtk:style-provider}
  @see-class{gtk:settings}
  @see-function{gtk:style-context-add-provider}"
  (display (g:object gdk:display))
  (provider (g:object style-provider))
  (priority :uint))

(export 'style-context-add-provider-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_state
;;; gtk_style_context_set_state -> style-context-state
;;; ----------------------------------------------------------------------------

(defun (setf style-context-state) (state context)
  (cffi:foreign-funcall "gtk_style_context_set_state"
                        (g:object style-context) context
                        state-flags state
                        :void)
  state)

(defcfun ("gtk_style_context_get_state" style-context-state) state-flags
 #+liber-documentation
 "@version{#2022-8-3}
  @syntax[]{(gtk:style-context-state context) => state}
  @syntax[]{(setf (gtk:style-context-state context) state)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a value of the @symbol{gtk:state-flags} flags to represent}
  @begin{short}
    Accessor of the state used when rendering.
  @end{short}
  The @sym{gtk:style-context-state} function returns the state used for style
  matching. The @sym{(setf gtk:style-context-state)} function sets the state.

  This function should only be used to retrieve the @symbol{gtk:state-flag}
  values to pass to the @class{gtk:style-context} functions, like the
  @fun{gtk:style-context-padding} function. If you need to retrieve the current
  state of a @class{gtk:widget} object, use the @fun{gtk:widget-state-flags}
  function.
  @see-class{gtk:style-context}
  @see-class{gtk:widget}
  @see-symbol{gtk:state-flags}
  @see-function{gtk:widget-state-flags}
  @see-function{gtk:style-context-padding}"
  (context (g:object style-context)))

(export 'style-context-state)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_color -> style-context-color
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_color" %style-context-color) :void
  (context (g:object style-context))
  (color (g:boxed gdk:rgba)))

(defun style-context-color (context)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @return{The @class{gdk:rgba} foreground color.}
  @begin{short}
    Gets the foreground color for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gdk:rgba}"
  (let ((color (gdk:rgba-new)))
    (%style-context-color context color)
    color))

(export 'style-context-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_border -> style-context-border
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_border" %style-context-border) :void
  (context (g:object style-context))
  (state state-flags)
  (border (g:boxed border)))

(defun style-context-border (context state)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @symbol{gtk:state-flags} value to retrieve the border for}
  @return{Returns border settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the border settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((border (border-new)))
    (%style-context-border context state border)
    border))

(export 'style-context-border)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_padding -> style-context-padding
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_padding" %style-context-padding) :void
  (context (g:object style-context))
  (state state-flags)
  (padding (g:boxed border)))

(defun style-context-padding (context state)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @symbol{gtk:state-flags} value to retrieve the padding for}
  @return{Returns padding settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the padding settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((padding (border-new)))
    (%style-context-padding context state padding)
    padding))

(export 'style-context-padding)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_margin -> style-context-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_get_margin" %style-context-margin) :void
  (context (g:object style-context))
  (state state-flags)
  (margin (g:boxed border)))

(defun style-context-margin (context state)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[state]{a @symbol{gtk:state-flags} value to retrieve the margin for}
  @return{Returns margin settings as a @class{gtk:border} instance.}
  @begin{short}
    Gets the value for the margin settings for a given state.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:border}
  @see-symbol{gtk:state-flags}"
  (let ((margin (border-new)))
    (%style-context-margin context state margin)
    margin))

(export 'style-context-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_lookup_color
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_lookup_color" %style-context-lookup-color)
    :boolean
  (context (g:object style-context))
  (name :string)
  (color (g:boxed gdk:rgba)))

(defun style-context-lookup-color (context name)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[name]{a string with a color name to lookup}
  @return{The looked up @class{gdk:rgba} color, or @code{nil}.}
  @begin{short}
    Looks up and resolves a color name in the style context color map.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gdk:rgba}"
  (let ((color (gdk:rgba-new)))
    (when (%style-context-lookup-color context name color)
      color)))

(export 'style-context-lookup-color)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_provider" style-context-remove-provider)
    :void
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[provider]{a @class{gtk:style-provider} object}
  @begin{short}
    Removes the style provider from the style providers list in the style
    context.
  @end{short}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-function{gtk:style-context-add-provider}"
  (context (g:object style-context))
  (provider (g:object style-provider)))

(export 'style-context-remove-provider)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_provider_for_display
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_provider_for_display"
           style-context-remove-provider-for-display) :void
 #+liber-documentation
 "@version{#2022-8-3}
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

(defcfun ("gtk_style_context_restore" style-context-restore) :void
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Restores the style context state to a previous stage.
  @end{short}
  See the @fun{gtk:style-context-save} function.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-save}"
  (context (g:object style-context)))

(export 'style-context-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_save
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_save" style-context-save) :void
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @begin{short}
    Saves the style context state.
  @end{short}
  So all modifications done through the @fun{gtk:style-context-add-class},
  @fun{gtk:style-context-remove-class} or @fun{gtk:style-context-state}
  functions can be reverted in one go through the
  @fun{gtk:style-context-restore} function.
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-add-class}
  @see-function{gtk:style-context-remove-class}
  @see-function{gtk:style-context-state}
  @see-function{gtk:style-context-restore}"
  (context (g:object style-context)))

(export 'style-context-save)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_add_class
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_add_class" style-context-add-class) :void
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name to use in styling}
  @begin{short}
    Adds a style class to the style context, so later uses of the style context
    will make use of this new class for styling.
  @end{short}
  @begin[Example]{dictionary}
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
  @see-class{gtk:style-context}
  @see-function{gtk:style-context-property}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-add-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_remove_class
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_remove_class" style-context-remove-class) :void
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name to remove}
  @begin{short}
    Removes a class name from the style context.
  @end{short}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-remove-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_has_class
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_has_class" style-context-has-class) :boolean
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[classname]{a string with a class name}
  @return{@em{True} if the style context has @arg{classname} defined.}
  @begin{short}
    Returns @em{true} if the style context currently has defined the given
    class name.
  @end{short}
  @see-class{gtk:style-context}"
  (context (g:object style-context))
  (classname :string))

(export 'style-context-has-class)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_get_scale
;;; gtk_style_context_set_scale -> style-context-scale
;;; ----------------------------------------------------------------------------

(defun (setf style-context-scale) (scale context)
  (cffi:foreign-funcall "gtk_style_context_set_scale"
                        (g:object style-context) context
                        :int scale
                        :void)
  scale)

(defcfun ("gtk_style_context_get_scale" style-context-scale) :int
 #+liber-documentation
 "@version{#2022-8-3}
  @syntax[]{(gtk:style-context-scale context) => scale}
  @syntax[]{(setf (gtk:style-context-scale context) scale)}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[scale]{an integer with a scale}
  @begin{short}
    Accessor of the scale used for image assets for the style context.
  @end{short}
  The @sym{gtk:style-context-scale} function returns the scale to use when
  getting image assets for the style. The @sym{(setf gtk:style-context-scale)}
  function sets the scale.
  @see-class{gtk:style-context}"
  (context (g:object style-context)))

(export 'style-context-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_style_context_to_string
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_style_context_to_string" style-context-to-string) :string
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[flags]{a @symbol{gtk:style-context-print-flags} value that
    determine what to print}
  @return{A string representing the style context.}
  @begin{short}
    Converts the style context into a string representation.
  @end{short}
  The string representation always includes information about the name, state,
  ID, visibility and style classes of the CSS node that is backing the style
  context. Depending on the flags, more information may be included.

  This function is intended for testing and debugging of the CSS implementation
  in GTK. There are no guarantees about the format of the returned string, it
  may change.
  @begin[Example]{dictionary}
    @begin{pre}
(setq context
      (gtk:widget-style-context (make-instance 'gtk:dialog)))
=> #<GTK-STYLE-CONTEXT {1001C70663@}>
(gtk:style-context-to-string context :recurse)
=>
\"[window.background.csd.dialog.message:dir(ltr)]
  box.dialog-vbox.vertical:dir(ltr)
    box.vertical:dir(ltr)
      box.horizontal:dir(ltr)
        box.vertical:dir(ltr)
          label:dir(ltr)
          [label:dir(ltr)]
    box.dialog-action-box.horizontal:dir(ltr)
      box.dialog-action-area.horizontal:dir(ltr)
  box.horizontal.titlebar:dir(ltr)
    [label.title:dir(ltr)]
\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:style-context}
  @see-symbol{gtk:style-context-print-flags}"
  (context (g:object style-context))
  (flags style-context-print-flags))

(export 'style-context-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_render_activity
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_activity" %render-activity) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-activity (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with the rectangle
    width}
  @argument[height]{a number, coerced to a double float, with the rectangle
    height}
  @begin{short}
    Renders an activity area such as in the @class{gtk:spinner} widget or the
    fill line in the @class{gtk:range} widget.
  @end{short}
  The @code{:active} state of the @symbol{gtk:stage-flags} flags determines
  whether there is activity going on.
  @see-class{gtk:style-context}
  @see-class{gtk:spinner}
  @see-class{gtk:range}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-activity context cr (coerce x 'double-float)
                                   (coerce y 'double-float)
                                   (coerce width 'double-float)
                                   (coerce height 'double-float)))

(export 'render-activity)

;;; ----------------------------------------------------------------------------
;;; gtk_render_arrow
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_arrow" %render-arrow) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (angle :double)
  (x :double)
  (y :double)
  (size :double))

(defun render-arrow (context cr angle x y size)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[angle]{a number, coerced to a double float, with an arrow angle
    from 0 to 2 * Pi, being 0 the arrow pointing to the north}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[size]{a number, coerced to a double float, with the square side for
    render area}
  @begin{short}
    Renders an arrow pointing to an angle.
  @end{short}

  Typical arrow rendering at 0, 1/2 Pi, Pi, and 3/2 Pi: @image[render-arrow]{}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-arrow context cr (coerce angle 'double-float)
                                (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce size 'double-float)))

(export 'render-arrow)

;;; ----------------------------------------------------------------------------
;;; gtk_render_background
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_background" %render-background) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-background (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    render area}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    render area}
  @argument[width]{a number, coerced to a double float, with the rectangle
    width}
  @argument[height]{a number, coerced to a double float, with the rectangle
    height}
  @begin{short}
    Renders the background of an element.
  @end{short}

  Typical background rendering, showing the effect of @code{background-image},
  @code{border-width} and @code{border-radius}: @image[render-background]{}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-background context cr (coerce x 'double-float)
                                     (coerce y 'double-float)
                                     (coerce width 'double-float)
                                     (coerce height 'double-float)))

(export 'render-background)

;;; ----------------------------------------------------------------------------
;;; gtk_render_check
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_check" %render-check) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-check (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a checkmark as in a @class{gtk:check-button} widget.
  @end{short}

  The @code{:checked} state of the @symbol{gtk:state-flags} flags determines
  whether the check is on or off, and the @code{:inconsistent} state determines
  whether it should be marked as undefined.

  Typical checkmark rendering: @image[render-check]{}
  @see-class{gtk:style-context}
  @see-class{gtk:check-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-check context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'render-check)

;;; ----------------------------------------------------------------------------
;;; gtk_render_expander
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_expander" %render-expander) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-expander (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an expander as used in the @class{gtk:tree-view} widget and
    @class{gtk:expander} widget in the area defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}
  The @code{:active} state of the @symbol{gtk:state-flags} flags determines
  whether the expander is collapsed or expanded.

  Typical expander rendering: @image[render-expander]{}
  @see-class{gtk:style-context}
  @see-class{gtk:expander}
  @see-class{gtk:tree-view}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-expander context cr (coerce x 'double-float)
                                   (coerce y 'double-float)
                                   (coerce width 'double-float)
                                   (coerce height 'double-float)))

(export 'render-expander)

;;; ----------------------------------------------------------------------------
;;; gtk_render_focus
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_focus" %render-focus) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-focus (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a focus indicator on the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Typical focus rendering: @image[render-focus]{}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-focus context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'render-focus)

;;; ----------------------------------------------------------------------------
;;; gtk_render_frame
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_frame" %render-frame) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-frame (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with the rectangle
    width}
  @argument[height]{a number, coerced to a double float, with the rectangle
    height}
  @begin{short}
    Renders a frame around the rectangle defined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Examples of frame rendering, showing the effect of @code{border-image},
  @code{border-color}, @code{border-width}, @code{border-radius} and
  @code{junctions}: @image[render-frame]{}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-frame context cr (coerce x 'double-float)
                                (coerce y 'double-float)
                                (coerce width 'double-float)
                                (coerce height 'double-float)))

(export 'render-frame)

;;; ----------------------------------------------------------------------------
;;; gtk_render_handle
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_handle" %render-handle) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-handle (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders a handle, as the resize grip of the @class{gtk:paned} widget and
    @class{gtk:window} widget, in the rectangle determined by @arg{x}, @arg{y},
    @arg{width}, @arg{height}.
  @end{short}

  Handles rendered for the paned and grip classes: @image[render-handle]{}
  @see-class{gtk:style-context}
  @see-class{gtk:paned}
  @see-class{gtk:window}
  @see-symbol{cairo:context-t}"
  (%render-handle context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'render-handle)

;;; ----------------------------------------------------------------------------
;;; gtk_render_icon
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_icon" %render-icon) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (texture (g:object gdk-texture))
  (x :double)
  (y :double))

(defun render-icon (context cr texture x y)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[texture]{a @class{gdk-texture} object containing the icon to draw}
  @argument[x]{a number, coerced to a double float, with the x position for the
    texture}
  @argument[y]{a number, coerced to a double float, with the y position for the
    texture}
  @begin{short}
    Renders the icon in a texture at the specified @arg{x} and @arg{y}
    coordinates.
  @end{short}
  This function will render the icon in texture at exactly its size, regardless
  of scaling factors, which may not be appropriate when drawing on displays with
  high pixel densities.
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-class{gdk-texture}"
  (%render-icon context cr texture (coerce x 'double-float)
                                       (coerce y 'double-float)))

(export 'render-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_render_layout
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_layout" %render-layout) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (layout (g:object pango:layout)))

(defun render-layout (context cr x y layout)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with the x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with the y origin of the
    rectangle}
  @argument[layout]{a @symbol{pango:layout} object to render}
  @begin{short}
    Renders a Pango layout on the coordinates @arg{x}, @arg{y}.
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}
  @see-symbol{pango:layout}"
  (%render-layout context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 layout))

(export 'render-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_render_line
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_line" %render-line) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defun render-line (context cr x0 y0 x1 y1)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x0]{a number, coerced to a double float, with the x coordinate for
    the origin of the line}
  @argument[y0]{a number, coerced to a double float, with the y coordinate for
    the origin of the line}
  @argument[x1]{a number, coerced to a double float, with the x coordinate for
    the end of the line}
  @argument[y1]{a number, coerced to a double float, with the y coordinate for
    the end of the line}
  @begin{short}
    Renders a line from (@arg{x0}, @arg{y0}) to (@arg{x1}, @arg{y1}).
  @end{short}
  @see-class{gtk:style-context}
  @see-symbol{cairo:context-t}"
  (%render-line context cr (coerce x0 'double-float)
                               (coerce y0 'double-float)
                               (coerce x1 'double-float)
                               (coerce y1 'double-float)))

(export 'render-line)

;;; ----------------------------------------------------------------------------
;;; gtk_render_option
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_render_option" %render-option) :void
  (context (g:object style-context))
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun render-option (context cr x y width height)
 #+liber-documentation
 "@version{#2022-8-1}
  @argument[context]{a @class{gtk:style-context} object}
  @argument[cr]{a @symbol{cairo:context-t} context}
  @argument[x]{a number, coerced to a double float, with a x origin of the
    rectangle}
  @argument[y]{a number, coerced to a double float, with a y origin of the
    rectangle}
  @argument[width]{a number, coerced to a double float, with a rectangle width}
  @argument[height]{a number, coerced to a double float, with a rectangle
    height}
  @begin{short}
    Renders an option mark as in a @class{gtk:radio-button} widget.
  @end{short}
  The @code{:checked} state of the @symbol{gtk:state-flags} flags will determine
  whether the option is on or off, and the @code{:inconsistent} state whether
  it should be marked as undefined.

  Typical option mark rendering: @image[render-option]{}
  @see-class{gtk:style-context}
  @see-class{gtk:radio-button}
  @see-symbol{cairo:context-t}
  @see-symbol{gtk:state-flags}"
  (%render-option context cr (coerce x 'double-float)
                                 (coerce y 'double-float)
                                 (coerce width 'double-float)
                                 (coerce height 'double-float)))

(export 'render-option)

;;; --- End of file gtk4.style-context.lisp ------------------------------------
