;;; ----------------------------------------------------------------------------
;;; gtk4.bitset.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;;     gtk_bitset_ref
;;;     gtk_bitset_unref
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

(glib:define-g-boxed-opaque bitset "GtkBitset"
  :export t
  :type-initializer "gtk_bitset_get_type"
  :alloc (%bitset-new-empty))

#+liber-documentation
(setf (liber:alias-for-class 'bitset)
      "GBoxed"
      (documentation 'bitset 'type)
 "@version{2023-8-9}
  @begin{short}
    The @class{gtk:bitset} structure is a data structure for representing a set
    of unsigned integers.
  @end{short}
  Another name for this data structure is \"bitmap\". The current implementation
  is based on roaring bitmaps.

  A bitset allows adding a set of integers and provides support for set
  operations like unions, intersections and checks for equality or if a value
  is contained in the set. The @class{gtk:bitset} implementation also contains
  various functions to query metadata about the bitset, such as the minimum or
  maximum values or its size.

  The fastest way to iterate values in a bitset is @symbol{gtk:bitset-iter}
  iterator.

  The main use case for the @class{gtk:bitset} structure is implementing
  complex selections for the @class{gtk:selection-model} class.
  @see-class{gtk:selection-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_ref ()
;;;
;;; GtkBitset *
;;; gtk_bitset_ref (GtkBitset *self);
;;;
;;; Acquires a reference on the given GtkBitset.
;;;
;;; self :
;;;     a GtkBitset.
;;;
;;; Returns :
;;;     the GtkBitset with an additional reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_unref ()
;;;
;;; void
;;; gtk_bitset_unref (GtkBitset *self);
;;;
;;; Releases a reference on the given GtkBitset.
;;;
;;; If the reference was the last, the resources associated to the self are
;;; freed.
;;;
;;; self :
;;;     a GtkBitset.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_new_empty ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_new_empty" bitset-new-empty)
    (g:boxed bitset :return)
 #+liber-documentation
 "@version{#2023-8-30}
  @return{A new empty @class{gtk:bitset} object.}
  @short{Creates a new empty bitset.}
  @see-class{gtk:bitset}")

(export 'bitset-new-empty)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_new_range ()
;;;
;;; GtkBitset *
;;; gtk_bitset_new_range (guint start,
;;;                       guint n_items);
;;;
;;; Creates a bitset with the given range set.
;;;
;;; start :
;;;     first value to add
;;;
;;; n_items :
;;;     number of consecutive values to add
;;;
;;; Returns :
;;;     A new bitset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_new_range" bitset-new-range) (g:boxed bitset :return)
  (start :uint)
  (n-items :uint))

(export 'bitset-new-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_copy ()
;;;
;;; GtkBitset *
;;; gtk_bitset_copy (const GtkBitset *self);
;;;
;;; Creates a copy of self .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; Returns :
;;;     A new bitset that contains the same values as self .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_copy" bitset-copy) (g:boxed bitset :return)
  (bitset (g:boxed bitset)))

(export 'bitset-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_contains ()
;;;
;;; gboolean
;;; gtk_bitset_contains (const GtkBitset *self,
;;;                      guint value);
;;;
;;; Checks if the given value has been added to self
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; value :
;;;     the value to check
;;;
;;; Returns :
;;;     TRUE if self contains value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_contains" bitset-contains) :boolean
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-contains)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_is_empty ()
;;;
;;; gboolean
;;; gtk_bitset_is_empty (const GtkBitset *self);
;;;
;;; Check if no value is contained in bitset.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; Returns :
;;;     TRUE if self is empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_is_empty" bitset-is-empty) :boolean
  (bitset (g:boxed bitset)))

(export 'bitset-is-empty)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_equals ()
;;;
;;; gboolean
;;; gtk_bitset_equals (const GtkBitset *self,
;;;                    const GtkBitset *other);
;;;
;;; Returns TRUE if self and other contain the same values.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; other :
;;;     another GtkBitset
;;;
;;; Returns :
;;;     TRUE if self and other contain the same values
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_equals" bitset-equals) :boolean
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-equals)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_minimum ()
;;;
;;; guint
;;; gtk_bitset_get_minimum (const GtkBitset *self);
;;;
;;; Returns the smallest value in self . If self is empty, G_MAXUINT is
;;; returned.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; Returns :
;;;     The smallest value in self
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_minimum" bitset-minimum) :uint
  (bitset (g:boxed bitset)))

(export 'bitset-minimum)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_maximum ()
;;;
;;; guint
;;; gtk_bitset_get_maximum (const GtkBitset *self);
;;;
;;; Returns the largest value in self . If self is empty, 0 is returned.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; Returns :
;;;     The largest value in self
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_maximum" bitset-maximum) :uint
  (bitset (g:boxed bitset)))

(export 'bitset-maximum)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_size ()
;;;
;;; guint64
;;; gtk_bitset_get_size (const GtkBitset *self);
;;;
;;; Gets the number of values that were added to the set. For example, if the
;;; set is empty, 0 is returned.
;;;
;;; Note that this function returns a guint64, because when all values are set,
;;; the return value is G_MAXUINT + 1. Unless you are sure this cannot happen
;;; (it can't with GListModel), be sure to use a 64bit type.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; Returns :
;;;     The number of values in the set.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_size" bitset-size) :uint64
  (bitset (g:boxed bitset)))

(export 'bitset-size)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_size_in_range ()
;;;
;;; guint64
;;; gtk_bitset_get_size_in_range (const GtkBitset *self,
;;;                               guint first,
;;;                               guint last);
;;;
;;; Gets the number of values that are part of the set from first to last
;;; (inclusive).
;;;
;;; Note that this function returns a guint64, because when all values are set,
;;; the return value is G_MAXUINT + 1. Unless you are sure this cannot happen
;;; (it can't with GListModel), be sure to use a 64bit type.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; first :
;;;     the first element to include
;;;
;;; last :
;;;     the last element to include
;;;
;;; Returns :
;;;     The number of values in the set from first to last .
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_size_in_range" bitset-size-in-range) :uint64
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-size-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_get_nth ()
;;;
;;; guint
;;; gtk_bitset_get_nth (const GtkBitset *self,
;;;                     guint nth);
;;;
;;; Returns the value of the nth item in self.
;;;
;;; If nth is >= the size of self , 0 is returned.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; nth :
;;;     index of the item to get
;;;
;;; Returns :
;;;     the value of the nth item in self
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_get_nth" bitset-nth) :uint
  (bitset (g:boxed bitset))
  (nth :uint))

(export 'bitset-nth)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_all ()
;;;
;;; void
;;; gtk_bitset_remove_all (GtkBitset *self);
;;;
;;; Removes all values from the bitset so that it is empty again.
;;;
;;; self :
;;;     a GtkBitset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_all" bitset-remove-all) :void
  (bitset (g:boxed bitset)))

(export 'bitset-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add ()
;;;
;;; gboolean
;;; gtk_bitset_add (GtkBitset *self,
;;;                 guint value);
;;;
;;; Adds value to self if it wasn't part of it before.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; value :
;;;     value to add
;;;
;;; Returns :
;;;     TRUE if value was not part of self and self was changed.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add" bitset-add) :boolean
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-add)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove ()
;;;
;;; gboolean
;;; gtk_bitset_remove (GtkBitset *self,
;;;                    guint value);
;;;
;;; Removes value from self if it was part of it before.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; value :
;;;     value to add
;;;
;;; Returns :
;;;     TRUE if value was part of self and self was changed.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove" bitset-remove) :boolean
  (bitset (g:boxed bitset))
  (value :uint))

(export 'bitset-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_range ()
;;;
;;; void
;;; gtk_bitset_add_range (GtkBitset *self,
;;;                       guint start,
;;;                       guint n_items);
;;;
;;; Adds all values from start (inclusive) to start + n_items (exclusive) in
;;; self .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; start :
;;;     first value to add
;;;
;;; n_items :
;;;     number of consecutive values to add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_range" bitset-add-range) :void
  (bitset (g:boxed bitset))
  (start :uint)
  (n-items :uint))

(export 'bitset-add-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_range ()
;;;
;;; void
;;; gtk_bitset_remove_range (GtkBitset *self,
;;;                          guint start,
;;;                          guint n_items);
;;;
;;; Removes all values from start (inclusive) to start + n_items (exclusive) in
;;; self .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; start :
;;;     first value to remove
;;;
;;; n_items :
;;;     number of consecutive values to remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_range" bitset-remove-range) :void
  (bitset (g:boxed bitset))
  (start :uint)
  (n-items :uint))

(export 'bitset-remove-range)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_range_closed ()
;;;
;;; void
;;; gtk_bitset_add_range_closed (GtkBitset *self,
;;;                              guint first,
;;;                              guint last);
;;;
;;; Adds the closed range [first , last ], so first , last and all values in
;;; between. first must be smaller than last .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; first :
;;;     first value to add
;;;
;;; last :
;;;     last value to add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_range_closed" bitset-add-range-closed) :void
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-add-range-closed)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_range_closed ()
;;;
;;; void
;;; gtk_bitset_remove_range_closed (GtkBitset *self,
;;;                                 guint first,
;;;                                 guint last);
;;;
;;; Removes the closed range [first , last ], so first , last and all values in
;;; between. first must be smaller than last .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; first :
;;;     first value to remove
;;;
;;; last :
;;;     last value to remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_range_closed" bitset-remove-range-closed)
    :void
  (bitset (g:boxed bitset))
  (first :uint)
  (last :uint))

(export 'bitset-remove-range-closed)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_add_rectangle ()
;;;
;;; void
;;; gtk_bitset_add_rectangle (GtkBitset *self,
;;;                           guint start,
;;;                           guint width,
;;;                           guint height,
;;;                           guint stride);
;;;
;;; Interprets the values as a 2-dimensional boolean grid with the given stride
;;; and inside that grid, adds a rectangle with the given width and height .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; start :
;;;     first value to add
;;;
;;; width :
;;;     width of the rectangle
;;;
;;; height :
;;;     height of the rectangle
;;;
;;; stride :
;;;     row stride of the grid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_add_rectangle" bitset-add-rectangle) :void
  (bitset (g:boxed bitset))
  (start :uint)
  (width :uint)
  (height :uint)
  (stride :uint))

(export 'bitset-add-rectangle)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_remove_rectangle ()
;;;
;;; void
;;; gtk_bitset_remove_rectangle (GtkBitset *self,
;;;                              guint start,
;;;                              guint width,
;;;                              guint height,
;;;                              guint stride);
;;;
;;; Interprets the values as a 2-dimensional boolean grid with the given stride
;;; and inside that grid, removes a rectangle with the given width and height .
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; start :
;;;     first value to remove
;;;
;;; width :
;;;     width of the rectangle
;;;
;;; height :
;;;     height of the rectangle
;;;
;;; stride :
;;;     row stride of the grid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_remove_rectangle" bitset-remove-rectangle) :void
  (bitset (g:boxed bitset))
  (start :uint)
  (width :uint)
  (height :uint)
  (stride :uint))

(export 'bitset-remove-rectangle)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_union ()
;;;
;;; void
;;; gtk_bitset_union (GtkBitset *self,
;;;                   const GtkBitset *other);
;;;
;;; Sets self to be the union of self and other , that is add all values from
;;; other into self that weren't part of it.
;;;
;;; It is allowed for self and other to be the same bitset. Nothing will happen
;;; in that case.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; other :
;;;     the GtkBitset to union with
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_union" bitset-union) :void
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-union)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_intersect ()
;;;
;;; void
;;; gtk_bitset_intersect (GtkBitset *self,
;;;                       const GtkBitset *other);
;;;
;;; Sets self to be the intersection of self and other , that is remove all
;;; values from self that are not part of other .
;;;
;;; It is allowed for self and other to be the same bitset. Nothing will happen
;;; in that case.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; other :
;;;     the GtkBitset to intersect with
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_intersect" bitset-intersect) :void
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-intersect)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_subtract ()
;;;
;;; void
;;; gtk_bitset_subtract (GtkBitset *self,
;;;                      const GtkBitset *other);
;;;
;;; Sets self to be the subtraction of other from self , that is remove all
;;; values from self that are part of other .
;;;
;;; It is allowed for self and other to be the same bitset. The bitset will be
;;; emptied in that case.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; other :
;;;     the GtkBitset to subtract
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_subtract" bitset-subtract) :void
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-subtract)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_difference ()
;;;
;;; void
;;; gtk_bitset_difference (GtkBitset *self,
;;;                        const GtkBitset *other);
;;;
;;; Sets self to be the symmetric difference of self and other , that is set
;;; self to contain all values that were either contained in self or in other ,
;;; but not in both. This operation is also called an XOR.
;;;
;;; It is allowed for self and other to be the same bitset. The bitset will be
;;; emptied in that case.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; other :
;;;     the GtkBitset to compute the difference from
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_difference" bitset-difference) :void
  (bitset (g:boxed bitset))
  (other (g:boxed bitset)))

(export 'bitset-difference)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_shift_left ()
;;;
;;; void
;;; gtk_bitset_shift_left (GtkBitset *self,
;;;                        guint amount);
;;;
;;; Shifts all values in self to the left by amount . Values smaller than amount
;;; are discarded.
;;;
;;; self :
;;;     a $GtkBitset
;;;
;;; amount :
;;;     amount to shift all values to the left
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_shift_left" bitset-shift-left) :void
  (bitset (g:boxed bitset))
  (amount :uint))

(export 'bitset-shift-left)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_shift_right ()
;;;
;;; void
;;; gtk_bitset_shift_right (GtkBitset *self,
;;;                         guint amount);
;;;
;;; Shifts all values in self to the right by amount . Values that end up too
;;; large to be held in a guint are discarded.
;;;
;;; self :
;;;     a $GtkBitset
;;;
;;; amount :
;;;     amount to shift all values to the right
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_shift_right" bitset-shift-right) :void
  (bitset (g:boxed bitset))
  (amount :uint))

(export 'bitset-shift-right)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_splice ()
;;;
;;; void
;;; gtk_bitset_splice (GtkBitset *self,
;;;                    guint position,
;;;                    guint removed,
;;;                    guint added);
;;;
;;; This is a support function for GListModel handling, by mirroring the
;;; “items-changed” signal.
;;;
;;; First, it "cuts" the values from position to removed from the bitset. That
;;; is, it removes all those values and shifts all larger values to the left by
;;; removed places.
;;;
;;; Then, it "pastes" new room into the bitset by shifting all values larger
;;; than position by added spaces to the right. This frees up space that can
;;; then be filled.
;;;
;;; self :
;;;     a GtkBitset
;;;
;;; position :
;;;     position at which to slice
;;;
;;; removed :
;;;     number of values to remove
;;;
;;; added :
;;;     number of values to add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_splice" bitset-splice) :void
  (bitset (g:boxed bitset))
  (position :uint)
  (removed :uint)
  (added :uint))

(export 'bitset-splice)

;;; ----------------------------------------------------------------------------
;;; struct GtkBitsetIter
;;;
;;; struct GtkBitsetIter {
;;; };
;;;
;;; An opaque, stack-allocated struct for iterating over the elements of a
;;; GtkBitset. Before a GtkBitsetIter can be used, it needs to be initialized
;;; with gtk_bitset_iter_init_first(), gtk_bitset_iter_init_last() or
;;; gtk_bitset_iter_init_at().
;;;
;;; See Also
;;; GtkSelectionModel
;;; ----------------------------------------------------------------------------

(cffi:defctype bitset-iter :pointer)

(export 'bitset-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_first ()
;;;
;;; gboolean
;;; gtk_bitset_iter_init_first (GtkBitsetIter *iter,
;;;                             const GtkBitset *set,
;;;                             guint *value);
;;;
;;; Initializes an iterator for set and points it to the first value in set .
;;; If set is empty, FALSE is returned and value is set to G_MAXUINT.
;;;
;;; iter :
;;;     a pointer to an uninitialized GtkBitsetIter.
;;;
;;; set :
;;;     a GtkBitset
;;;
;;; value :
;;;     Set to the first value in set .
;;;
;;; Returns :
;;;     TRUE if set isn't empty.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_first" bitset-iter-init-first) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (value (:pointer :uint)))

(export 'bitset-iter-init-first)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_last ()
;;;
;;; gboolean
;;; gtk_bitset_iter_init_last (GtkBitsetIter *iter,
;;;                            const GtkBitset *set,
;;;                            guint *value);
;;;
;;; Initializes an iterator for set and points it to the last value in set . If
;;; set is empty, FALSE is returned.
;;;
;;; iter :
;;;     a pointer to an uninitialized GtkBitsetIter.
;;;
;;; set :
;;;     a GtkBitset
;;;
;;; value :
;;;     Set to the last value in set .
;;;
;;; Returns :
;;;     TRUE if set isn't empty.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_last" bitset-iter-init-last) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (value (:pointer :uint)))

(export 'bitset-iter-init-last)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_init_at ()
;;;
;;; gboolean
;;; gtk_bitset_iter_init_at (GtkBitsetIter *iter,
;;;                          const GtkBitset *set,
;;;                          guint target,
;;;                          guint *value);
;;;
;;; Initializes iter to point to target . If target is not found, finds the next
;;; value after it. If no value >= target exists in set , this function returns
;;; FALSE.
;;;
;;; iter :
;;;     a pointer to an uninitialized GtkBitsetIter.
;;;
;;; set :
;;;     a GtkBitset
;;;
;;; target :
;;;     target value to start iterating at
;;;
;;; value :
;;;     Set to the found value in set .
;;;
;;; Returns :
;;;     TRUE if a value was found.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_init_at" bitset-iter-init-at) :boolean
  (iter bitset-iter)
  (bitset (g:boxed bitset))
  (target :uint)
  (value (:pointer :uint)))

(export 'bitset-iter-init-at)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_next ()
;;;
;;; gboolean
;;; gtk_bitset_iter_next (GtkBitsetIter *iter,
;;;                       guint *value);
;;;
;;; Moves iter to the next value in the set. If it was already pointing to the
;;; last value in the set, FALSE is returned and iter is invalidated.
;;;
;;; iter :
;;;     a pointer to a valid GtkBitsetIter
;;;
;;; value :
;;;     Set to the next value.
;;;
;;; Returns :
;;;     TRUE if a next value existed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_next" bitset-iter-next) :boolean
  (iter bitset-iter)
  (value (:pointer :uint)))

(export 'bitset-iter-next)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_previous ()
;;;
;;; gboolean
;;; gtk_bitset_iter_previous (GtkBitsetIter *iter,
;;;                           guint *value);
;;;
;;; Moves iter to the previous value in the set. If it was already pointing to
;;; the first value in the set, FALSE is returned and iter is invalidated.
;;;
;;; iter :
;;;     a pointer to a valid GtkBitsetIter
;;;
;;; value :
;;;     Set to the previous value.
;;;
;;; Returns :
;;;     TRUE if a previous value existed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_previous" bitset-iter-previous) :boolean
  (iter bitset-iter)
  (value (:pointer :uint)))

(export 'bitset-iter-previous)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_get_value ()
;;;
;;; guint
;;; gtk_bitset_iter_get_value (const GtkBitsetIter *iter);
;;;
;;; Gets the current value that iter points to.
;;;
;;; If iter is not valid and gtk_bitset_iter_is_valid() returns FALSE, this
;;; function returns 0.
;;;
;;; iter :
;;;     a GtkBitsetIter
;;;
;;; Returns :
;;;     The current value pointer to by iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_get_value" bitset-iter-value) :uint
  (iter bitset-iter))

(export 'bitset-iter-value)

;;; ----------------------------------------------------------------------------
;;; gtk_bitset_iter_is_valid ()
;;;
;;; gboolean
;;; gtk_bitset_iter_is_valid (const GtkBitsetIter *iter);
;;;
;;; Checks if iter points to a valid value.
;;;
;;; iter :
;;;     a GtkBitsetIter
;;;
;;; Returns :
;;;     TRUE if iter points to a valid value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_bitset_iter_is_valid" bitset-iter-is-valid) :boolean
  (iter bitset-iter))

(export 'bitset-iter-is-valid)

;;; --- End of file gtk4.bitset.lisp -------------------------------------------
