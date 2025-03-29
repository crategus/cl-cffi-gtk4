;;; ----------------------------------------------------------------------------
;;; gtk4.bitset.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; GtkBitset
;;;
;;;     Sets of integers
;;;
;;; Types and Values
;;;
;;;     GtkBitset
;;;     GtkBitsetIter
;;;
;;; Functions
;;;
;;;     gtk_bitset_ref                                      not needed
;;;     gtk_bitset_unref                                    not needed
;;;     gtk_bitset_new_empty
;;;     gtk_bitset_new_range
;;;     gtk_bitset_copy
;;;     gtk_bitset_contains
;;;     gtk_bitset_is_empty
;;;     gtk_bitset_equals
;;;     gtk_bitset_get_minimum
;;;     gtk_bitset_get_maximum
;;;     gtk_bitset_get_size
;;;     gtk_bitset_get_size_in_range
;;;     gtk_bitset_get_nth
;;;     gtk_bitset_remove_all
;;;     gtk_bitset_add
;;;     gtk_bitset_remove
;;;     gtk_bitset_add_range
;;;     gtk_bitset_remove_range
;;;     gtk_bitset_add_range_closed
;;;     gtk_bitset_remove_range_closed
;;;     gtk_bitset_add_rectangle
;;;     gtk_bitset_remove_rectangle
;;;     gtk_bitset_union
;;;     gtk_bitset_intersect
;;;     gtk_bitset_subtract
;;;     gtk_bitset_difference
;;;     gtk_bitset_shift_left
;;;     gtk_bitset_shift_right
;;;     gtk_bitset_splice
;;;
;;;     gtk_bitset_iter_init_first
;;;     gtk_bitset_iter_init_last
;;;     gtk_bitset_iter_init_at
;;;     gtk_bitset_iter_next
;;;     gtk_bitset_iter_previous
;;;     gtk_bitset_iter_get_value
;;;     gtk_bitset_iter_is_valid
;;;
;;; Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkBitset
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkBitset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_new_empty" %bitset-new-empty) :pointer)

(glib:define-gboxed-opaque bitset "GtkBitset"
  :export t
  :type-initializer "gtk_bitset_get_type"
  :alloc (%bitset-new-empty))

#+liber-documentation
(setf (liber:alias-for-class 'bitset)
      "GBoxed"
      (documentation 'bitset 'type)
 "@version{2025-3-13}
  @begin{declaration}
(glib:define-gboxed-opaque bitset \"GtkBitset\"
  :export t
  :type-initializer \"gtk_bitset_get_type\"
  :alloc (%bitset-new-empty))
  @end{declaration}
  @begin{short}
    The @class{gtk:bitset} structure is a data structure for representing a set
    of unsigned integers.
  @end{short}
  Another name for this data structure is \"bitmap\". The current implementation
  is based on roaring bitmaps.

  A bitset allows adding a set of integers and provides support for set
  operations like unions, intersections and checks for equality or if a value
  is contained in the bitset. The @class{gtk:bitset} implementation also
  contains various functions to query metadata about the bitset, such as the
  minimum or maximum values or its size.

  The fastest way to iterate values in a bitset is a @symbol{gtk:bitset-iter}
  iterator.

  The main use case for the @class{gtk:bitset} structure is implementing
  complex selections for the @class{gtk:selection-model} class.
  @see-constructor{gtk:bitset-new-empty}
  @see-constructor{gtk:bitset-new-range}
  @see-constructor{gtk:bitset-copy}
  @see-class{gtk:selection-model}
  @see-symbol{gtk:bitset-iter}")

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_ref                                          not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_unref                                        not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_new_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_new_empty" bitset-new-empty) (g:boxed bitset :return)
 #+liber-documentation
 "@version{2025-3-13}
  @return{The new empty @class{gtk:bitset} instance.}
  @short{Creates a new empty bitset.}
  @see-class{gtk:bitset}")

(export 'bitset-new-empty)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_new_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_new_range" bitset-new-range) (g:boxed bitset :return)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[start]{an unsigned integer for the first value to add}
  @argument[n]{an unsigned integer for the number of consecutive values to add}
  @return{The new @class{gtk:bitset} instance.}
  @short{Creates a bitset with the given range set.}
  @see-class{gtk:bitset}"
  (start :uint)
  (n :uint))

(export 'bitset-new-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_copy" bitset-copy) (g:boxed bitset :return)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @begin{return}
    The new @class{gtk:bitset} instance that contains the same values as
    @arg{bitset}.
  @end{return}
  @short{Creates a copy of the bitset.}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset)))

(export 'bitset-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_contains
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_contains" bitset-contains) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[value]{an unsigned integer for the value to check}
  @return{@em{True} if @arg{bitset} contains @arg{value}.}
  @short{Checks if the given @arg{value} has been added to the bitset.}
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar bitset (gtk:bitset-new-range 100 50)) => BITSET
(gtk:bitset-contains bitset 99) => NIL
(gtk:bitset-contains bitset 100) => T
(gtk:bitset-contains bitset 149) => T
(gtk:bitset-contains bitset 150) => NIL
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-contains)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_is_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_is_empty" bitset-is-empty) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @return{@em{True} if @arg{bitset} is empty.}
  @short{Checks if no value is contained in the bitset.}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset)))

(export 'bitset-is-empty)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_equals
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_equals" bitset-equals) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[other]{another @class{gtk:bitset} instance}
  @return{@em{True} if @arg{bitset} and @arg{other} contain the same values.}
  @begin{short}
    Returns @em{true} if @arg{bitset} and @arg{other} contain the same values.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-equals)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_minimum
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_minimum" bitset-minimum) :uint
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @return{The unsigned integer with the smallest value in @arg{bitset}.}
  @begin{short}
    Returns the smallest value in the bitset.
  @end{short}
  If the bitset is empty, the maximum value for an unsigned 32-bit integer
  variable is returned, that is the @code{#xffffffff} value.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar bitset (gtk:bitset-new-range 100 50)) => BITSET
(gtk:bitset-minimum bitset) => 100
(gtk:bitset-maximum bitset) => 149
(gtk:bitset-minimum (gtk:bitset-new-empty)) => 4294967295
(gtk:bitset-maximum (gtk:bitset-new-empty)) => 0
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}
  @see-function{gtk:bitset-maximum}"
  (bitset (g:boxed bitset)))

(export 'bitset-minimum)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_maximum
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_maximum" bitset-maximum) :uint
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @return{The unsigned integer with the largest value in @arg{bitset}.}
  @begin{short}
    Returns the largest value in the bitset.
  @end{short}
  If the bitset is empty, 0 is returned. See the @fun{gtk:bitset-minimum}
  documentation for examples.
  @see-class{gtk:bitset}
  @see-function{gtk:bitset-minimum}"
  (bitset (g:boxed bitset)))

(export 'bitset-maximum)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_size" bitset-size) :uint64
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @return{The unsigned integer with the number of values in @arg{bitset}.}
  @begin{short}
    Gets the number of values that were added to the bitset.
  @end{short}
  For example, if the bitset is empty, 0 is returned.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar bitset (gtk:bitset-new-range 100 50)) => BITSET
(gtk:bitset-size bitset) => 50
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}
  @see-class{g:list-model}"
  (bitset (g:boxed bitset)))

(export 'bitset-size)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_size_in_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_size_in_range" bitset-size-in-range) :uint64
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[first]{an unsigned integer for the first element to include}
  @argument[last]{an unsigned integer for the last element to include}
  @begin{return}
    The unsigned integer with the number of values in the set from @arg{first}
    to @arg{last}.
  @end{return}
  @begin{short}
    Gets the number of values that are part of the bitset from @arg{first} to
    @arg{last} (inclusive).
  @end{short}
  @see-class{gtk:bitset}
  @see-class{g:list-model}"
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-size-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_nth
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_nth" bitset-nth) :uint
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[nth]{an unsigned integer for the index of the item to get}
  @begin{return}
    The unsigned integer for the value of the @arg{nth} item in @arg{bitset}.
  @end{return}
  @begin{short}
    Returns the value of the @arg{nth} item in the bitset.
  @end{short}
  If @arg{nth} is greater than or equal the size of the bitset, 0 is returned.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar bitset (gtk:bitset-new-range 100 50)) => BITSET
(gtk:bitset-size bitset) => 50
(gtk:bitset-nth bitset 0) => 100
(gtk:bitset-nth bitset 49) => 149
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (nth :uint))

(export 'bitset-nth)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_all
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_all" bitset-remove-all) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @begin{short}
    Removes all values from the bitset so that it is empty again.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset)))

(export 'bitset-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add" bitset-add) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[value]{an unsigned integer for the value to add}
  @begin{return}
    @em{True} if @arg{value} was not part of @arg{bitset} and @arg{bitset} was
    changed.
  @end{return}
  @begin{short}
    Adds @arg{value} to the bitset if it was not part of it before.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-add)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove" bitset-remove) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[value]{an unsigned integer for the value to remove}
  @begin{return}
    @em{True} if @arg{value} was part of @arg{bitset} and @arg{bitset} was
    changed.
  @end{return}
  @begin{short}
    Removes @arg{value} from the bitset if it was part of it before.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_range" bitset-add-range) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[start]{an unsigned integer for the first value to add}
  @argument[n]{an unsigned integer for the number of consecutive values to add}
  @begin{short}
    Adds all values from @arg{start} (inclusive) to @arg{start} + @arg{n}
    (exclusive) in the bitset.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (start :uint)
  (n :uint))

(export 'bitset-add-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_range" bitset-remove-range) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[start]{an unsigned integer for the first value to remove}
  @argument[n]{an unsigned integer for the number of consecutive values to
    remove}
  @begin{short}
    Removes all values from @arg{start} (inclusive) to @arg{start} +
    @arg{n} (exclusive) in the bitset.
  @end{short}
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (start :uint)
  (n :uint))

(export 'bitset-remove-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_range_closed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_range_closed" bitset-add-range-closed) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[first]{an unsigned integer for the first value to add}
  @argument[last]{an unsigned integer for the last value to add}
  @begin{short}
    Adds the closed range @code{[@arg{first},@arg{last}]}, so the @arg{first}
    value, the @arg{last} value and all values in between.
  @end{short}
  The @arg{first} value must be smaller than the @arg{last} value.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-add-range-closed)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_range_closed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_range_closed" bitset-remove-range-closed)
    :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[first]{an unsigned integer for the first value to remove}
  @argument[last]{an unsigned integer for the last value to remove}
  @begin{short}
    Removes the closed range @code{[@arg{first},@arg{last}]}, so the @arg{first}
    value, the @arg{last} value and all values in between.
  @end{short}
  The @arg{first} value must be smaller than the @arg{last} value.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-remove-range-closed)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_rectangle" bitset-add-rectangle) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[start]{an unsigned integer for the first value to add}
  @argument[width]{an unsigned integer for the width of the rectangle}
  @argument[height]{an unsigned integer for the height of the rectangle}
  @argument[stride]{an unsigned integer for the row stride of the grid}
  @begin{short}
    Interprets the values as a 2-dimensional boolean grid with the given
    @arg{stride} and inside that grid, adds a rectangle with the given
    @arg{width} and @arg{height}.
  @end{short}
  @begin[Notes]{dictionary}
    This funtion is equivalent to:
    @begin{pre}
(dotimes (i height)
  (gtk:bitset-add-range bitset (+ (* i stride) start) width))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}
  @see-function{gtk:bitset-add-range}"
  (bitset (g:boxed bitset))
  (start :uint)
  (width :uint)
  (height :uint)
  (stride :uint))

(export 'bitset-add-rectangle)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_rectangle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_rectangle" bitset-remove-rectangle) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[start]{an unsigned integer for the first value to remove}
  @argument[width]{an unsigned integer for the width of the rectangle}
  @argument[height]{an unsigned integer for the height of the rectangle}
  @argument[stride]{an unsigned integer for the row stride of the grid}
  @begin{short}
    Interprets the values as a 2-dimensional boolean grid with the given
    @arg{stride} and inside that grid, removes a rectangle with the given
    @arg{width} and @arg{height}.
  @end{short}
  @begin[Notes]{dictionary}
    This funtion is equivalent to:
    @begin{pre}
(dotimes (i height)
  (gtk:bitset-remove-range bitset (+ (* i stride) start) width))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:bitset}
  @see-function{gtk:bitset-remove-range}"
  (bitset (g:boxed bitset))
  (start :uint)
  (width :uint)
  (height :uint)
  (stride :uint))

(export 'bitset-remove-rectangle)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_union
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_union" bitset-union) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[other]{a @class{gtk:bitset} instance to union with}
  @begin{short}
    Sets @arg{bitset} to be the union of @arg{bitset} and @arg{other}, that is
    add all values from @arg{other} into @arg{bitset} that were not part of it.
  @end{short}
  It is allowed for @arg{bitset} and @arg{other} to be the same bitset. Nothing
  will happen in that case.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-union)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_intersect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_intersect" bitset-intersect) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[other]{a @class{gtk:bitset} instance to intersect with}
  @begin{short}
    Sets @arg{bitset} to be the intersection of @arg{bitset} and @arg{other},
    that is remove all values from @arg{bitset} that are not part of
    @arg{other}.
  @end{short}
  It is allowed for @arg{bitset} and @arg{other} to be the same bitset. Nothing
  will happen in that case.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_subtract
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_subtract" bitset-subtract) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[other]{a @class{gtk:bitset} instance to subtract}
  @begin{short}
    Sets @arg{bitset} to be the subtraction of @arg{bitset} from @arg{other},
    that is remove all values from @arg{bitset} that are part of @arg{other}.
  @end{short}
  It is allowed for @arg{bitset} and @arg{other} to be the same bitset. The
  bitset will be emptied in that case.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-subtract)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_difference
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_difference" bitset-difference) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[other]{a @class{gtk:bitset} instance to compute the difference from}
  @begin{short}
    Sets @arg{bitset} to be the symmetric difference of @arg{bitset} and
    @arg{other}, that is set @arg{bitset} to contain all values that were either
    contained in @arg{bitset} or in @arg{other}, but not in both. This operation
    is also called an XOR.
  @end{short}
  It is allowed for @arg{bitset} and @arg{other} to be the same bitset. The
  bitset will be emptied in that case.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-difference)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_shift_left
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_shift_left" bitset-shift-left) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[amount]{an unsigned integer for the amount to shift all values to
    left}
  @begin{short}
    Shifts all values in @arg{bitset} to the left by @arg{amount}.
  @end{short}
  Values smaller than @arg{amount} are discarded.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (amount :uint))

(export 'bitset-shift-left)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_shift_right
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_shift_right" bitset-shift-right) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[amount]{an unsigned integer for the amount to shift all values to
    right}
  @begin{short}
    Shifts all values in @arg{bitset} to the right by @arg{amount}.
  @end{short}
  Values that end up too large to be held in an unsigned 32-bit integer are
  discarded.
  @see-class{gtk:bitset}"
  (bitset (g:boxed bitset))
  (amount :uint))

(export 'bitset-shift-right)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_splice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_splice" bitset-splice) :void
 #+liber-documentation
 "@version{2025-3-13}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[position]{an unsigned integer for the position at which to slice}
  @argument[removed]{an unsigned integer for the number of values to remove}
  @argument[added]{an unsigned integer for the number of values to add}
  @begin{short}
    This is a support function for @class{g:list-model} handling, by mirroring
    the @code{\"items-changed\"} signal.
  @end{short}
  First, it \"cuts\" the values from @arg{position} to be removed from the
  bitset. That is, it removes all those values and shifts all larger values to
  the left by removed places. Then, it \"pastes\" new room into the bitset by
  shifting all values larger than @arg{position} by added spaces to the right.
  This frees up space that can then be filled.
  @see-class{gtk:bitset}
  @see-class{g:list-model}"
  (bitset (g:boxed bitset))
  (position :uint)
  (removed :uint)
  (added :uint))

(export 'bitset-splice)

;;; ----------------------------------------------------------------------------
;;; GtkBitsetIter
;;; ----------------------------------------------------------------------------

(cffi:defctype bitset-iter :pointer)

#+liber-documentation
(setf (liber:alias-for-symbol 'bitset-iter)
      "CStruct"
      (liber:symbol-documentation 'bitset-iter)
 "@version{2025-3-13}
  @begin{declaration}
(cffi:defctype bitset-iter :pointer)
  @end{declaration}
  @begin{short}
    An opaque, stack-allocated structure for iterating over the elements of a
    @class{gtk:bitset} instance.
  @end{short}
  Before a @symbol{gtk:bitset-iter} instance can be used, it needs to be
  initialized with the @fun{gtk:bitset-iter-init-first},
  @fun{gtk:bitset-iter-init-last} or @fun{gtk:bitset-iter-init-at} function.
  @see-class{gtk:bitset}
  @see-function{gtk:bitset-iter-init-first}
  @see-function{gtk:bitset-iter-init-last}
  @see-function{gtk:bitset-iter-init-at}")

(export 'bitset-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_first
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_first" %bitset-iter-init-first) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (value (:pointer :uint)))

(defun bitset-iter-init-first (iter bitset)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{an uninitialized @class{gtk:bitset-iter} instance}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @begin{return}
    The unsigned integer with the first value in @arg{bitset}, or @code{nil}
    if @arg{bitset} is empty.
  @end{return}
  @begin{short}
    Initializes an iterator for the bitset and points it to the first value in
    the bitset.
  @end{short}
  If the bitset is empty, @code{nil} is returned.
  @see-class{gtk:bitset-iter}
  @see-class{gtk:bitset}"
  (cffi:with-foreign-object (value :uint)
    (when (%bitset-iter-init-first iter bitset value)
      (cffi:mem-ref value :uint))))

(export 'bitset-iter-init-first)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_last
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_last" %bitset-iter-init-last) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (value (:pointer :uint)))

(defun bitset-iter-init-last (iter bitset)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{an uninitialized @class{gtk:bitset-iter} instance}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @begin{return}
    The unsigned integer with the last value in @arg{bitset}, or @code{nil}
    if the bitset is empty.
  @end{return}
  @begin{short}
    Initializes an iterator for the bitset and points it to the last value in
    the bitset.
  @end{short}
  If the bitset is empty, @code{nil} is returned.
  @see-class{gtk:bitset-iter}
  @see-class{gtk:bitset}"
  (cffi:with-foreign-object (value :uint)
    (when (%bitset-iter-init-last iter bitset value)
      (cffi:mem-ref value :uint))))

(export 'bitset-iter-init-last)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_at
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_at" %bitset-iter-init-at) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (target :uint)
  (value (:pointer :uint)))

(defun bitset-iter-init-at (iter bitset target)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{an uninitialized @class{gtk:bitset-iter} instance}
  @argument[bitset]{a @class{gtk:bitset} instance}
  @argument[target]{an unsigned integer for the target value to start iterating
    at}
  @begin{return}
    The unsigned integer with the found value in @arg{bitset}, or @code{nil}.
  @end{return}
  @begin{short}
    Initializes an iterator for the bitset to point to @arg{target}.
  @end{short}
  If @arg{target} is not found, finds the next value after it. If no value
  greater than or equal @arg{target} exists in the bitset, this function
  returns @code{nil}.
  @see-class{gtk:bitset-iter}
  @see-class{gtk:bitset}"
  (cffi:with-foreign-object (value :uint)
    (when (%bitset-iter-init-at iter bitset target value)
      (cffi:mem-ref value :uint))))

(export 'bitset-iter-init-at)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_next" %bitset-iter-next) :boolean
  (iter bitset-iter)
  (value (:pointer :uint)))

(defun bitset-iter-next (iter)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{a valid @class{gtk:bitset-iter} instance}
  @begin{return}
    The unsigned integer with the next value in @arg{bitset}, or @code{nil}
    if no next value existed.
  @end{return}
  @begin{short}
    Moves @arg{iter} to the next value in the bitset.
  @end{short}
  If it was already pointing to the last value in the bitset, @code{nil} is
  returned and @arg{iter} is invalidated.
  @see-class{gtk:bitset-iter}"
  (cffi:with-foreign-object (value :uint)
    (when (%bitset-iter-next iter value)
      (cffi:mem-ref value :uint))))

(export 'bitset-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_previous
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_previous" %bitset-iter-previous) :boolean
  (iter bitset-iter)
  (value (:pointer :uint)))

(defun bitset-iter-previous (iter)
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{a valid @class{gtk:bitset-iter} instance}
  @begin{return}
    The unsigned integer with the previous value in @arg{bitset}, or @code{nil}
    if @arg{iter} was already pointing to the first value.
  @end{return}
  @begin{short}
    Moves @arg{iter} to the previous value in the bitset.
  @end{short}
  If it was already pointing to the first value in the bitset, @code{nil} is
  returned and @arg{iter} is invalidated.
  @see-class{gtk:bitset-iter}"
  (cffi:with-foreign-object (value :uint)
    (when (%bitset-iter-previous iter value)
      (cffi:mem-ref value :uint))))

(export 'bitset-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_get_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_get_value" bitset-iter-value) :uint
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{a valid @class{gtk:bitset-iter} instance}
  @return{The unsigned integer with the current value @arg{iter} points to.}
  @begin{short}
    Gets the current value that @arg{iter} points to.
  @end{short}
  If @arg{iter} is not valid and the @fun{gtk:bitset-iter-is-valid} function
  returns @em{false}, this function returns 0.
  @see-class{gtk:bitset-iter}
  @see-function{gtk:bitset-iter-is-valid}"
  (iter bitset-iter))

(export 'bitset-iter-value)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_is_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_is_valid" bitset-iter-is-valid) :boolean
 #+liber-documentation
 "@version{2025-3-13}
  @argument[iter]{a @class{gtk:bitset-iter} instance}
  @return{@em{True} if @arg{iter} points to a valid value.}
  @begin{short}
    Checks if @arg{iter} points to a valid value.
  @end{short}
  @see-class{gtk:bitset-iter}"
  (iter bitset-iter))

(export 'bitset-iter-is-valid)

;;; --- End of file gtk4.bitset.lisp -------------------------------------------
