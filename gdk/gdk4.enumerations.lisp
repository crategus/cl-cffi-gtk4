;;; ----------------------------------------------------------------------------
;;; gdk4.enumerations.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GdkGravity
;;;     GDK_MODIFIER_MASK
;;;     GdkModifierType
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkGravity
;;; ----------------------------------------------------------------------------

(define-g-enum "GdkGravity" gravity
  (:export t
   :type-initializer "gdk_gravity_get_type")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:center 4)
  (:east 5)
  (:south-west 6)
  (:south 7)
  (:south-east 8)
  (:static 9))

#+liber-documentation
(setf (liber:alias-for-symbol 'gravity)
      "GEnum"
      (liber:symbol-documentation 'gravity)
 "@version{2023-4-9}
  @begin{short}
    Defines the reference point of a surface and is used in
    the @class{gdk:popup-layout} implementation.
  @end{short}
  @begin{pre}
(define-g-enum \"GdkGravity\" gravity
  (:export t
   :type-initializer \"gdk_gravity_get_type\")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:center 4)
  (:east 5)
  (:south-west 6)
  (:south 7)
  (:south-east 8)
  (:static 9))
  @end{pre}
  @begin[code]{table}
    @entry[:north-west]{The reference point is at the top left corner.}
    @entry[:north]{The reference point is in the middle of the top edge.}
    @entry[:north-east]{The reference point is at the top right corner.}
    @entry[:west]{The reference point is at the middle of the left edge.}
    @entry[:center]{The reference point is at the center of the surface.}
    @entry[:east]{The reference point is at the middle of the right edge.}
    @entry[:south-west]{The reference point is at the lower left corner.}
    @entry[:south]{The reference point is at the middle of the lower edge.}
    @entry[:sout-east]{The reference point is at the lower right corner.}
    @entry[:static]{The reference point is at the top left corner of the
      surface itself, ignoring window manager decorations.}
  @end{table}
  @see-class{gdk:popup-layout}")

;;; ----------------------------------------------------------------------------
;;; GDK_MODIFIER_MASK
;;;
;;; #define GDK_MODIFIER_MASK
;;;
;;; A mask covering all entries in GdkModifierType.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkModifierType" modifier-type
  (:export t
   :type-initializer "gdk_modifier_type_get_type")
  (:none         0)
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:alt-mask     #.(ash 1 3))

  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))

  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28)))

#+liber-documentation
(setf (liber:alias-for-symbol 'modifier-type)
      "GFlags"
      (liber:symbol-documentation 'modifier-type)
 "@version{2022-7-15}
  @begin{short}
    Flags to indicate the state of modifier keys and mouse buttons in events.
  @end{short}
  Typical modifier keys are @kbd{Shift}, @kbd{Control}, @kbd{Meta}, @kbd{Super},
  @kbd{Hyper}, @kbd{Alt}, @kbd{Compose}, @kbd{Apple}, @kbd{CapsLock} or
  @kbd{ShiftLock} keys.

  Note that GDK may add internal values to events which include  values
  outside this enumeration. Your code should
  preserve and ignore them. You can use the @variable{gdk:modifier-mask} value
  to remove all private values.
  @begin{pre}
(define-g-flags \"GdkModifierType\" modifier-type
  (:export t
   :type-initializer \"gdk_modifier_type_get_type\")
  (:none         0)
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:alt-mask     #.(ash 1 3))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No modifier key.}
    @entry[:shift-mask]{The @kbd{Shift} key.}
    @entry[:lock-mask]{A Lock key, depending on the modifier mapping of the
      X server this may either be the @kbd{CapsLock} or @kbd{ShiftLock} key.}
    @entry[:control-mask]{The @kbd{Control} key.}
    @entry[:alt-mask]{The fourth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier, but
      normally it is the @kbd{Alt} key.}
    @entry[:button1-mask]{The first mouse button.}
    @entry[:button2-mask]{The second mouse button.}
    @entry[:button3-mask]{The third mouse button.}
    @entry[:button4-mask]{The fourth mouse button.}
    @entry[:button5-mask]{The fifth mouse button.}
    @entry[:super-mask]{The Super modifier.}
    @entry[:hyper-mask]{The Hyper modifier.}
    @entry[:meta-mask]{The Meta modifier.}
  @end{table}
  @see-class{gdk:event}")

;;; --- End of file gdk4.enumerations.lisp -------------------------------------
