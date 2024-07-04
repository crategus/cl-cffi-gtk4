;;; ----------------------------------------------------------------------------
;;; gdk4.keyval.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
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
;;; Key Values
;;;
;;;     Functions for manipulating keyboard codes
;;;
;;; Functions
;;;
;;;     gdk_keyval_name
;;;     gdk_keyval_from_name
;;;     gdk_keyval_convert_case
;;;     gdk_keyval_to_upper
;;;     gdk_keyval_to_lower
;;;     gdk_keyval_is_upper
;;;     gdk_keyval_is_lower
;;;     gdk_keyval_to_unicode
;;;     gdk_unicode_to_keyval
;;;
;;; Description
;;;
;;; Key values are the codes which are sent whenever a key is pressed or
;;; released. They are included in the data contained in a key press or release
;;; GdkEvent. The complete list of key values can be found in the
;;; gdk/gdkkeysyms.h header file.
;;;
;;; Key values are regularly updated from the upstream X.org X11 implementation,
;;; so new values are added regularly. They will be prefixed with GDK_KEY_
;;; rather than XF86XK_ or XK_ (for older symbols).
;;;
;;; Key values can be converted into a string representation using
;;; gdk_keyval_name(). The reverse function, converting a string to a key value,
;;; is provided by gdk_keyval_from_name().
;;;
;;; The case of key values can be determined using gdk_keyval_is_upper() and
;;; gdk_keyval_is_lower(). Key values can be converted to upper or lower case
;;; using gdk_keyval_to_upper() and gdk_keyval_to_lower().
;;;
;;; When it makes sense, key values can be converted to and from Unicode
;;; characters with gdk_keyval_to_unicode() and gdk_unicode_to_keyval().
;;;
;;; Groups
;;;
;;; At the lowest level, physical keys on the keyboard are represented by
;;; numeric keycodes, and GDK knows how to translate these keycodes into key
;;; values according to the configured keyboard layout and the current state of
;;; the keyboard. In the GDK api, the mapping from keycodes to key values is
;;; available via gdk_display_map_keycode(), and the reverse mapping is
;;; available via gdk_display_map_keyval(). The results of these functions are
;;; returned in GdkKeymapKey structs.
;;;
;;; You can think of a GdkKeymapKey as a representation of a symbol printed on
;;; a physical keyboard key. That is, it contains three pieces of information.
;;; First, it contains the hardware keycode; this is an identifying number for
;;; a physical key. Second, it contains the “level” of the key. The level
;;; indicates which symbol on the key will be used, in a vertical direction. So
;;; on a standard US keyboard, the key with the number “1“ on it also has the
;;; exclamation point (”!”) character on it. The level indicates whether to use
;;; the “1” or the “!” symbol. The letter keys are considered to have a
;;; lowercase letter at level 0, and an uppercase letter at level 1, though
;;; normally only the uppercase letter is printed on the key. Third, the
;;; GdkKeymapKey contains a group; groups are not used on standard US keyboards,
;;; but are used in many other countries. On a keyboard with groups, there can
;;; be 3 or 4 symbols printed on a single key. The group indicates movement in
;;; a horizontal direction. Usually groups are used for two different languages.
;;; In group 0, a key might have two English characters, and in group 1 it might
;;; have two Hebrew characters. The Hebrew characters will be printed on the key
;;; next to the English characters.
;;;
;;; When GDK creates a key event in order to deliver a key press or release, it
;;; first converts the current keyboard state into an effective group and level.
;;; This is done via a set of rules that varies widely according to type of
;;; keyboard and user configuration. The input to this translation consists of
;;; the hardware keycode pressed, the active modifiers, and the active group.
;;; It then applies the appropriate rules, and returns the group/level to be
;;; used to index the keymap, along with the modifiers which did not affect the
;;; group and level. i.e. it returns “unconsumed modifiers.” The keyboard group
;;; may differ from the effective group used for lookups because some keys
;;; don't have multiple groups - e.g. the Enter key is always in group 0
;;; regardless of keyboard state.
;;;
;;; The results of the translation, including the keyval, are all included in
;;; the key event and can be obtained via GdkEvent getters.
;;;
;;; Consumed modifiers
;;;
;;; The consumed_modifiers in a key event are modifiers that should be masked
;;; out from state when comparing this key press to a hot key. For instance, on
;;; a US keyboard, the plus symbol is shifted, so when comparing a key press to
;;; a <Control>plus accelerator <Shift> should be masked out.
;;;
;;; // We want to ignore irrelevant modifiers like ScrollLock
;;; #define ALL_ACCELS_MASK (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_ALT_MASK)
;;; state = gdk_event_get_modifier_state (event);
;;; gdk_keymap_translate_keyboard_state (keymap,
;;;                                      gdk_key_event_get_keycode (event),
;;;                                      state,
;;;                                      gdk_key_event_get_group (event),
;;;                                      &keyval, NULL, NULL, &consumed);
;;; if (keyval == GDK_PLUS &&
;;;     (state & ~consumed & ALL_ACCELS_MASK) == GDK_CONTROL_MASK)
;;;   // Control was pressed
;;;
;;; An older interpretation consumed_modifiers was that it contained all
;;; modifiers that might affect the translation of the key; this allowed
;;; accelerators to be stored with irrelevant consumed modifiers, by doing:
;;;
;;; // XXX Don’t do this XXX
;;; if (keyval == accel_keyval &&
;;;     (state & ~consumed & ALL_ACCELS_MASK) == (accel_mods & ~consumed))
;;;   // Accelerator was pressed
;;;
;;; However, this did not work if multi-modifier combinations were used in the
;;; keymap, since, for instance, <Control> would be masked out even if only
;;; <Control>&lt;Alt> was used in the keymap. To support this usage as well as
;;; well as possible, all single modifier combinations that could affect the key
;;; for any combination of modifiers will be returned in consumed_modifiers ;
;;; multi-modifier combinations are returned only when actually found in state .
;;; When you store accelerators, you should always store them with consumed
;;; modifiers removed. Store <Control>plus, not <Control>&lt;Shift>plus.
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_name" keyval-name) :string
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{A string containing the name of the key, or @code{nil} if @arg{keyval}
    is not a valid key. The string should not be modified.}
  @begin{short}
    Converts a key value into a symbolic name.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{GDK_KEY_}.
  @see-function{gdk:keyval-from-name}"
  (keyval :uint))

(export 'keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_from_name" keyval-from-name) :uint
 #+liber-documentation
 "@version{2023-4-14}
  @argument[name]{a string with the key name}
  @return{An unsigned integer with the corresponding key value, or
    @code{#xffffff} if the key name is not a valid key}
  @begin{short}
    Converts a key name to a key value.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{GDK_KEY_}.
  @see-function{gdk:keyval-name}"
  (name :string))

(export 'keyval-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_convert_case ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_convert_case" %keyval-convert-case) :void
  (keyval :uint)
  (lower (:pointer :uint))
  (upper (:pointer :uint)))

(defun keyval-convert-case (keyval)
 #+liber-documentation
 "@version{2023-4-14}
  @syntax{(gdk:keyval-convert-case keyval) => lower, upper}
  @argument[keyval]{an unsigned integer with the key value}
  @argument[lower]{an unsigned integer with the lowercase version of
    @arg{keyval}}
  @argument[upper]{an unsigned integer with the uppercase verson of
    @arg{keyval}}
  @begin{short}
    Obtains the upper-case and lower-case versions of the key value.
  @end{short}
  @see-function{gdk:keyval-to-upper}
  @see-function{gdk:keyval-to-lower}"
  (cffi:with-foreign-objects ((lower :uint) (upper :uint))
    (%keyval-convert-case keyval lower upper)
    (values (cffi:mem-ref lower :uint)
            (cffi:mem-ref upper :uint))))

(export 'keyval-convert-case)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_upper ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_upper" keyval-to-upper) :uint
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{An unsigned integer with the upper case form of @arg{keyval}, or
    @arg{keyval} itself if it is already in upper case or it is not subject to
    case conversion.}
  @begin{short}
    Converts a key value to upper case, if applicable.
  @end{short}
  @see-function{gdk:keyval-convert-case}"
  (keyval :uint))

(export 'keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_lower" keyval-to-lower) :uint
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{An unsigned integer with the lower case form of @arg{keyval}, or
    @arg{keyval} itself if it is already in lower case or it is not subject to
    case conversion.}
  @begin{short}
    Converts a key value to lower case, if applicable.
  @end{short}
  @see-function{gdk:keyval-convert-case}"
  (keyval :uint))

(export 'keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_upper" keyval-is-upper) :boolean
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{@em{True} if @arg{keyval} is in upper case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in upper case.
  @end{short}
  @see-function{gdk:keyval-to-upper}"
  (keyval :uint))

(export 'keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_lower" keyval-is-lower) :boolean
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{@em{True} if @arg{keyval} is in lower case, or if @arg{keyval} is not
    subject to case conversion.}
  @begin{short}
    Returns @em{true} if the given key value is in lower case.
  @end{short}
  @see-function{gdk:keyval-to-lower}"
  (keval :uint))

(export 'keyval-is-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_unicode ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_unicode" keyval-to-unicode) :uint32
 #+liber-documentation
 "@version{2023-4-14}
  @argument[keyval]{an unsigned integer with the key value}
  @return{An unsigned integer with the corresponding unicode character, or 0 if
    there is no corresponding character.}
  @begin{short}
    Convert from a GDK key value to the corresponding ISO10646 (Unicode)
    character.
  @end{short}
  Note that the conversion does not take the current locale into consideration,
  which might be expected for particular keyvals, such as
  @code{GDK_KEY_KP_Decimal}.
  @see-function{gdk:unicode-to-keyval}"
  (keyval :uint))

(export 'keyval-to-unicode)

;;; ----------------------------------------------------------------------------
;;; gdk_unicode_to_keyval ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_unicode_to_keyval" unicode-to-keyval) :uint
 #+liber-documentation
 "@version{2023-4-14}
  @argument[unicode]{an unsigned integer with a ISO10646 encoded character}
  @return{An unsigned integer with the corresponding GDK key value, if one
    exists, or, if there is no corresponding symbol,
    @code{wc | 0x01000000}}
  @begin{short}
    Convert from a ISO10646 character to a GDK key value.
  @end{short}
  @see-function{gdk:keyval-to-unicode}"
  (unicode :uint32))

(export 'unicode-to-keyval)

;;; --- End of file gdk4.keyval.lisp -------------------------------------------
