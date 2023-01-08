;;; ----------------------------------------------------------------------------
;;; gdk.event.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
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

;;;Key Values
;;;Key Values — Functions for manipulating keyboard codes

;;;Functions
;;;const char *	gdk_keyval_name ()
;;;guint	gdk_keyval_from_name ()
;;;void	gdk_keyval_convert_case ()
;;;guint	gdk_keyval_to_upper ()
;;;guint	gdk_keyval_to_lower ()
;;;gboolean	gdk_keyval_is_upper ()
;;;gboolean	gdk_keyval_is_lower ()
;;;guint32	gdk_keyval_to_unicode ()
;;;guint	gdk_unicode_to_keyval ()

(in-package :gdk)

;;;Description
;;;Key values are the codes which are sent whenever a key is pressed or released. They are included in the data contained in a key press or release GdkEvent. The complete list of key values can be found in the gdk/gdkkeysyms.h header file.

;;;Key values are regularly updated from the upstream X.org X11 implementation, so new values are added regularly. They will be prefixed with GDK_KEY_ rather than XF86XK_ or XK_ (for older symbols).

;;;Key values can be converted into a string representation using gdk_keyval_name(). The reverse function, converting a string to a key value, is provided by gdk_keyval_from_name().

;;;The case of key values can be determined using gdk_keyval_is_upper() and gdk_keyval_is_lower(). Key values can be converted to upper or lower case using gdk_keyval_to_upper() and gdk_keyval_to_lower().

;;;When it makes sense, key values can be converted to and from Unicode characters with gdk_keyval_to_unicode() and gdk_unicode_to_keyval().

;;;Groups
;;;At the lowest level, physical keys on the keyboard are represented by numeric keycodes, and GDK knows how to translate these keycodes into key values according to the configured keyboard layout and the current state of the keyboard. In the GDK api, the mapping from keycodes to key values is available via gdk_display_map_keycode(), and the reverse mapping is available via gdk_display_map_keyval(). The results of these functions are returned in GdkKeymapKey structs.

;;;You can think of a GdkKeymapKey as a representation of a symbol printed on a physical keyboard key. That is, it contains three pieces of information. First, it contains the hardware keycode; this is an identifying number for a physical key. Second, it contains the “level” of the key. The level indicates which symbol on the key will be used, in a vertical direction. So on a standard US keyboard, the key with the number “1“ on it also has the exclamation point (”!”) character on it. The level indicates whether to use the “1” or the “!” symbol. The letter keys are considered to have a lowercase letter at level 0, and an uppercase letter at level 1, though normally only the uppercase letter is printed on the key. Third, the GdkKeymapKey contains a group; groups are not used on standard US keyboards, but are used in many other countries. On a keyboard with groups, there can be 3 or 4 symbols printed on a single key. The group indicates movement in a horizontal direction. Usually groups are used for two different languages. In group 0, a key might have two English characters, and in group 1 it might have two Hebrew characters. The Hebrew characters will be printed on the key next to the English characters.

;;;When GDK creates a key event in order to deliver a key press or release, it first converts the current keyboard state into an effective group and level. This is done via a set of rules that varies widely according to type of keyboard and user configuration. The input to this translation consists of the hardware keycode pressed, the active modifiers, and the active group. It then applies the appropriate rules, and returns the group/level to be used to index the keymap, along with the modifiers which did not affect the group and level. i.e. it returns “unconsumed modifiers.” The keyboard group may differ from the effective group used for lookups because some keys don't have multiple groups - e.g. the Enter key is always in group 0 regardless of keyboard state.

;;;The results of the translation, including the keyval, are all included in the key event and can be obtained via GdkEvent getters.

;;;Consumed modifiers
;;;The consumed_modifiers in a key event are modifiers that should be masked out from state when comparing this key press to a hot key. For instance, on a US keyboard, the plus symbol is shifted, so when comparing a key press to a <Control>plus accelerator <Shift> should be masked out.

;;;// We want to ignore irrelevant modifiers like ScrollLock
;;;#define ALL_ACCELS_MASK (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_ALT_MASK)
;;;state = gdk_event_get_modifier_state (event);
;;;gdk_keymap_translate_keyboard_state (keymap,
;;;                                     gdk_key_event_get_keycode (event),
;;;                                     state,
;;;                                     gdk_key_event_get_group (event),
;;;                                     &keyval, NULL, NULL, &consumed);
;;;if (keyval == GDK_PLUS &&
;;;    (state & ~consumed & ALL_ACCELS_MASK) == GDK_CONTROL_MASK)
;;;  // Control was pressed
;;;An older interpretation consumed_modifiers was that it contained all modifiers that might affect the translation of the key; this allowed accelerators to be stored with irrelevant consumed modifiers, by doing:

;;;// XXX Don’t do this XXX
;;;if (keyval == accel_keyval &&
;;;    (state & ~consumed & ALL_ACCELS_MASK) == (accel_mods & ~consumed))
;;;  // Accelerator was pressed
;;;However, this did not work if multi-modifier combinations were used in the keymap, since, for instance, <Control> would be masked out even if only <Control>&lt;Alt> was used in the keymap. To support this usage as well as well as possible, all single modifier combinations that could affect the key for any combination of modifiers will be returned in consumed_modifiers ; multi-modifier combinations are returned only when actually found in state . When you store accelerators, you should always store them with consumed modifiers removed. Store <Control>plus, not <Control>&lt;Shift>plus.

;;;Functions
;;;gdk_keyval_name ()
;;;const char *
;;;gdk_keyval_name (guint keyval);
;;;Converts a key value into a symbolic name.

;;;The names are the same as those in the gdk/gdkkeysyms.h header file but without the leading “GDK_KEY_”.

;;;Parameters
;;;keyval

;;;a key value

;;;
;;;Returns
;;;a string containing the name of the key, or NULL if keyval is not a valid key. The string should not be modified.

;;;[nullable][transfer none]

;;;gdk_keyval_from_name ()
;;;guint
;;;gdk_keyval_from_name (const char *keyval_name);
;;;Converts a key name to a key value.

;;;The names are the same as those in the gdk/gdkkeysyms.h header file but without the leading “GDK_KEY_”.

;;;Parameters
;;;keyval_name

;;;a key name

;;;
;;;Returns
;;;the corresponding key value, or GDK_KEY_VoidSymbol if the key name is not a valid key

;;;gdk_keyval_convert_case ()
;;;void
;;;gdk_keyval_convert_case (guint symbol,
;;;                         guint *lower,
;;;                         guint *upper);
;;;Obtains the upper- and lower-case versions of the keyval symbol . Examples of keyvals are GDK_KEY_a, GDK_KEY_Enter, GDK_KEY_F1, etc.

;;;Parameters
;;;symbol

;;;a keyval

;;;
;;;lower

;;;return location for lowercase version of symbol .

;;;[out]
;;;upper

;;;return location for uppercase version of symbol .

;;;[out]
;;;gdk_keyval_to_upper ()
;;;guint
;;;gdk_keyval_to_upper (guint keyval);
;;;Converts a key value to upper case, if applicable.

;;;Parameters
;;;keyval

;;;a key value.

;;;
;;;Returns
;;;the upper case form of keyval , or keyval itself if it is already in upper case or it is not subject to case conversion.

;;;gdk_keyval_to_lower ()
;;;guint
;;;gdk_keyval_to_lower (guint keyval);
;;;Converts a key value to lower case, if applicable.

;;;Parameters
;;;keyval

;;;a key value.

;;;
;;;Returns
;;;the lower case form of keyval , or keyval itself if it is already in lower case or it is not subject to case conversion.

;;;gdk_keyval_is_upper ()
;;;gboolean
;;;gdk_keyval_is_upper (guint keyval);
;;;Returns TRUE if the given key value is in upper case.

;;;Parameters
;;;keyval

;;;a key value.

;;;
;;;Returns
;;;TRUE if keyval is in upper case, or if keyval is not subject to case conversion.

;;;gdk_keyval_is_lower ()
;;;gboolean
;;;gdk_keyval_is_lower (guint keyval);
;;;Returns TRUE if the given key value is in lower case.

;;;Parameters
;;;keyval

;;;a key value.

;;;
;;;Returns
;;;TRUE if keyval is in lower case, or if keyval is not subject to case conversion.

;;;gdk_keyval_to_unicode ()
;;;guint32
;;;gdk_keyval_to_unicode (guint keyval);
;;;Convert from a GDK key symbol to the corresponding ISO10646 (Unicode) character.

;;;Note that the conversion does not take the current locale into consideration, which might be expected for particular keyvals, such as GDK_KEY_KP_Decimal.

;;;Parameters
;;;keyval

;;;a GDK key symbol

;;;
;;;Returns
;;;the corresponding unicode character, or 0 if there is no corresponding character.

;;;gdk_unicode_to_keyval ()
;;;guint
;;;gdk_unicode_to_keyval (guint32 wc);
;;;Convert from a ISO10646 character to a key symbol.

;;;Parameters
;;;wc

;;;a ISO10646 encoded character

;;;
;;;Returns
;;;the corresponding GDK key symbol, if one exists. or, if there is no corresponding symbol, wc | 0x01000000

;;; --- End of file gdk.keyval.lisp --------------------------------------------
