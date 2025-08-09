;;; ----------------------------------------------------------------------------
;;; gdk4.keyval.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_name" keyval-name) :string
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    The string containing the name of the key, or @code{nil} if @arg{keyval} is
    not a valid key.
  @end{return}
  @begin{short}
    Converts a key value into a symbolic name.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{GDK_KEY_}.
  @see-function{gdk:keyval-from-name}"
  (keyval :uint))

(export 'keyval-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_from_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_from_name" keyval-from-name) :uint
 #+liber-documentation
 "@version{2025-08-02}
  @argument[name]{a string for the key name}
  @begin{return}
    The unsigned integer for the corresponding key value, or @code{#xffffff} if
    the key name is not a valid key
  @end{return}
  @begin{short}
    Converts a key name to a key value.
  @end{short}
  The names are the same as those in the @file{gdk/gdkkeysyms.h} header file
  but without the leading @code{GDK_KEY_}.
  @see-function{gdk:keyval-name}"
  (name :string))

(export 'keyval-from-name)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_convert_case
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_convert_case" %keyval-convert-case) :void
  (keyval :uint)
  (lower (:pointer :uint))
  (upper (:pointer :uint)))

(defun keyval-convert-case (keyval)
 #+liber-documentation
 "@version{2025-08-02}
  @syntax{(gdk:keyval-convert-case keyval) => lower, upper}
  @argument[keyval]{an unsigned integer for the key value}
  @argument[lower]{an unsigned integer for the lowercase version of
    @arg{keyval}}
  @argument[upper]{an unsigned integer for the uppercase verson of @arg{keyval}}
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
;;; gdk_keyval_to_upper
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_upper" keyval-to-upper) :uint
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    The unsigned integer for the upper case form of @arg{keyval}, or
    @arg{keyval} itself if it is already in upper case or it is not subject to
    case conversion.
  @end{return}
  @begin{short}
    Converts a key value to upper case, if applicable.
  @end{short}
  @see-function{gdk:keyval-convert-case}"
  (keyval :uint))

(export 'keyval-to-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_lower
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_lower" keyval-to-lower) :uint
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    The unsigned integer for the lower case form of @arg{keyval}, or
    @arg{keyval} itself if it is already in lower case or it is not subject to
    case conversion.
  @end{return}
  @begin{short}
    Converts a key value to lower case, if applicable.
  @end{short}
  @see-function{gdk:keyval-convert-case}"
  (keyval :uint))

(export 'keyval-to-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_upper
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_upper" keyval-is-upper) :boolean
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    @em{True} if @arg{keyval} is in upper case, or if @arg{keyval} is not
    subject to case conversion.
  @end{return}
  @begin{short}
    Returns @em{true} if the given key value is in upper case.
  @end{short}
  @see-function{gdk:keyval-to-upper}"
  (keyval :uint))

(export 'keyval-is-upper)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_is_lower
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_is_lower" keyval-is-lower) :boolean
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    @em{True} if @arg{keyval} is in lower case, or if @arg{keyval} is not
    subject to case conversion.
  @end{return}
  @begin{short}
    Returns @em{true} if the given key value is in lower case.
  @end{short}
  @see-function{gdk:keyval-to-lower}"
  (keval :uint))

(export 'keyval-is-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_keyval_to_unicode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_keyval_to_unicode" keyval-to-unicode) :uint32
 #+liber-documentation
 "@version{2025-08-02}
  @argument[keyval]{an unsigned integer for the key value}
  @begin{return}
    The unsigned integer for the corresponding unicode character, or 0 if there
    is no corresponding character.
  @end{return}
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
;;; gdk_unicode_to_keyval
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_unicode_to_keyval" unicode-to-keyval) :uint
 #+liber-documentation
 "@version{2025-08-02}
  @argument[unicode]{an unsigned integer for a ISO10646 encoded character}
  @begin{return}
    The unsigned integer for the corresponding GDK key value, if one exists,
    or, if there is no corresponding symbol, @code{wc | 0x01000000}.
  @end{return}
  @begin{short}
    Convert from a ISO10646 character to a GDK key value.
  @end{short}
  @see-function{gdk:keyval-to-unicode}"
  (unicode :uint32))

(export 'unicode-to-keyval)

;;; --- End of file gdk4.keyval.lisp -------------------------------------------
