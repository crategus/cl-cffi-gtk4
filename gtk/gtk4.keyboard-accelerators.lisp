;;; ----------------------------------------------------------------------------
;;; gtk4.keyboard-accelerators.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; Keyboard Accelerators
;;;
;;;     Utilities for accelerators
;;;
;;; Functions
;;;
;;;     gtk_accelerator_valid
;;;     gtk_accelerator_parse
;;;     gtk_accelerator_name
;;;     gtk_accelerator_get_label
;;;     gtk_accelerator_parse_with_keycode
;;;     gtk_accelerator_name_with_keycode
;;;     gtk_accelerator_get_label_with_keycode
;;;     gtk_accelerator_get_default_mod_mask
;;;
;;; Description
;;;
;;;     We have various utility functions to parse and generate textual
;;;     representations of keyboard accelerators.
;;;
;;;     If you want to set up keyboard accelerators for widgets,
;;;     GtkShortcutTrigger is probably more convenient than the functions in
;;;     this section.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_valid ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accelerator_valid" accelerator-valid) :boolean
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[keyval]{an unsigned integer with a GDK keyval}
  @argument[mask]{a value of the @symbol{gdk:modifier-type} flags}
  @return{@em{True} if the accelerator is valid.}
  @begin{short}
    Determines whether a given @arg{keyval} and modifier mask constitute a
    valid keyboard accelerator.
  @end{short}
  For example, the @code{GDK_KEY_a} keyval plus @code{GDK_CONTROL_MASK} is
  valid - this is a \"Ctrl+a\" accelerator. But, you cannot, for instance, use
  the @code{GDK_KEY_Control_L} keyval as an accelerator.
  @see-symbol{gdk:modifier-type}"
  (keyval :uint)
  (mask gdk:modifier-type))

(export 'accelerator-valid)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse ()
;;; ----------------------------------------------------------------------------

(defun accelerator-parse (accelerator)
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[accelerator]{a string representing an accelerator}
  @begin{return}
    @arg{key}  -- an unsigned integer with an accelerator keyval @br{}
    @arg{mask} -- a @symbol{gdk:modifier-type} accelerator modifier mask,
      or @code{nil}
  @end{return}
  @begin{short}
    Parses a string representing an accelerator.
  @end{short}
  The format looks like \"<Control>a\" or \"<Shift><Alt>F1\" or \"<Release>z\".
  The last one is for key release.

  The parser is fairly liberal and allows lower or upper case, and also
  abbreviations such as \"<Ctl>\" and \"<Ctrl>\". Key names are parsed using
  the @fun{gdk-keyval-from-name} function. For character keys the name is not
  the symbol, but the lowercase name, e.g. one would use \"<Ctrl>minus\"
  instead of \"<Ctrl>-\".

  If the parse fails, the @arg{key} argument will be set to 0.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:accelerator-parse \"<Control>a\")
=> 97
=> (:CONTROL-MASK)
(gtk:accelerator-parse \"<Shift><Alt>F1\")
=> 65470
=> (:SHIFT-MASK :ALT-MASK)
(gtk:accelerator-parse \"<Control>minus\")
=> 45
=> (:CONTROL-MASK)
(gtk:accelerator-parse \"not valid\")
=> 0
=> NIL
    @end{pre}
  @end{dictionary}
  @see-symbol{gdk:modifier-type}
  @see-function{gdk-keyval-from-name}"
  (cffi:with-foreign-objects ((key :uint) (mask 'gdk:modifier-type))
    (cffi:foreign-funcall "gtk_accelerator_parse"
                          :string accelerator
                          (:pointer :uint) key
                          (:pointer gdk:modifier-type) mask
                          :void)
    (values (cffi:mem-ref key :uint)
            (cffi:mem-ref mask 'gdk:modifier-type))))

(export 'accelerator-parse)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accelerator_name" accelerator-name) :string
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[key]{an unsigned integer with the accelerator keyval}
  @argument[mask]{a @symbol{gdk:modifier-type} accelerator modifier mask}
  @return{A string with the accelerator name.}
  @begin{short}
    Converts an accelerator keyval and modifier mask into a string parseable by
    the @fun{gtk:accelerator-parse} function.
  @end{short}

  If you need to display accelerators in the user interface, see the
  @fun{gtk:accelerator-label} function.
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:accelerator-name 65470 '(:shift-mask :alt-mask))
=> \"<Shift><Alt>F1\"
    @end{pre}
  @end{dictionary}
  @see-symbol{gdk:modifier-type}
  @see-function{gtk:accelerator-parse}
  @see-function{gtk:accelerator-label}"
  (key :uint)
  (mask gdk:modifier-type))

(export 'accelerator-name)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label () -> accelerator-label
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accelerator_get_label" accelerator-label) :string
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[key]{an unsigned integer with the accelerator keyval}
  @argument[mask]{a @symbol{gdk:modifier-type} accelerator modifier mask}
  @return{A string representing the accelerator.}
  @begin{short}
    Converts an accelerator keyval and modifier mask into a string which can be
    used to represent the accelerator to the user.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:accelerator-label 65470 '(:shift-mask :mod1-mask))
=> \"Shift+Alt+F1\"
    @end{pre}
  @end{dictionary}
  @see-function{gtk:accelerator-parse}
  @see-function{gtk:accelerator-name}"
  (key :uint)
  (mask gdk:modifier-type))

(export 'accelerator-label)

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_parse_with_keycode ()
;;;
;;; gboolean
;;; gtk_accelerator_parse_with_keycode (const char *accelerator,
;;;                                     GdkDisplay *display,
;;;                                     guint *accelerator_key,
;;;                                     guint **accelerator_codes,
;;;                                     GdkModifierType *accelerator_mods);
;;;
;;; Parses a string representing an accelerator, similarly to
;;; gtk_accelerator_parse() but handles keycodes as well. This is only useful
;;; for system-level components, applications should use gtk_accelerator_parse()
;;; instead.
;;;
;;; If accelerator_codes is given and the result stored in it is non-NULL, the
;;; result must be freed with g_free().
;;;
;;; If a keycode is present in the accelerator and no accelerator_codes is
;;; given, the parse will fail.
;;;
;;; If the parse fails, accelerator_key , accelerator_mods and accelerator_codes
;;; will be set to 0 (zero).
;;;
;;; accelerator :
;;;     string representing an accelerator
;;;
;;; display :
;;;     the GdkDisplay to look up accelerator_codes in.
;;;
;;; accelerator_key :
;;;     return location for accelerator keyval, or NULL.
;;;
;;; accelerator_codes :
;;;     return location for accelerator keycodes, or NULL.
;;;
;;; accelerator_mods :
;;;     return location for accelerator modifier mask, NULL.
;;;
;;; Returns :
;;;     TRUE if parsing succeeded
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_name_with_keycode ()
;;;
;;; char *
;;; gtk_accelerator_name_with_keycode (GdkDisplay *display,
;;;                                    guint accelerator_key,
;;;                                    guint keycode,
;;;                                    GdkModifierType accelerator_mods);
;;;
;;; Converts an accelerator keyval and modifier mask into a string parseable by
;;; gtk_accelerator_parse_with_keycode(), similarly to gtk_accelerator_name()
;;; but handling keycodes. This is only useful for system-level components,
;;; applications should use gtk_accelerator_parse() instead.
;;;
;;; display :
;;;     a GdkDisplay or NULL to use the default display.
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; keycode :
;;;     accelerator keycode
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly allocated accelerator name.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_label_with_keycode ()
;;;
;;; char *
;;; gtk_accelerator_get_label_with_keycode
;;;                                (GdkDisplay *display,
;;;                                 guint accelerator_key,
;;;                                 guint keycode,
;;;                                 GdkModifierType accelerator_mods);
;;;
;;; Converts an accelerator keyval and modifier mask into a (possibly
;;; translated) string that can be displayed to a user, similarly to
;;; gtk_accelerator_get_label(), but handling keycodes.
;;;
;;; This is only useful for system-level components, applications should use
;;; gtk_accelerator_parse() instead.
;;;
;;; display :
;;;     a GdkDisplay or NULL to use the default display.
;;;
;;; accelerator_key :
;;;     accelerator keyval
;;;
;;; keycode :
;;;     accelerator keycode
;;;
;;; accelerator_mods :
;;;     accelerator modifier mask
;;;
;;; Returns :
;;;     a newly-allocated string representing the accelerator.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accelerator_get_default_mod_mask ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accelerator_get_default_mod_mask"
               accelerator-default-mod-mask) gdk:modifier-type
 #+liber-documentation
 "@version{#2022-11-13}
  @return{A @symbol{gdk:modifier-type} accelerator modifier mask.}
  @begin{short}
    Gets the default modifier mask.
  @end{short}
  The modifier mask determines which modifiers are considered significant for
  keyboard accelerators. This includes all keyboard modifiers except for
  @code{GDK_LOCK_MASK}.
  @see-symbol{gdk:modifier-type}")

(export 'accelerator-default-mod-mask)

;;; --- End of file gtk4.keyboard-accelerators.lisp ----------------------------
