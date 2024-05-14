;;; ----------------------------------------------------------------------------
;;; gtk4.filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; GtkFilter
;;;
;;;     Filtering items
;;;
;;; Types and Values
;;;
;;;     GtkFilter
;;;     GtkFilterMatch
;;;     GtkFilterChange
;;;
;;; Functions
;;;
;;;     gtk_filter_match
;;;     gtk_filter_get_strictness
;;;     gtk_filter_changed
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ├── GtkMultiFilter
;;;         ├── GtkBoolFilter
;;;         ├── GtkCustomFilter
;;;         ├── GtkFileFilter
;;;         ╰── GtkStringFilter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFilterMatch
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkFilterMatch" filter-match
  (:export t
   :type-initializer "gtk_filter_match_get_type")
  :some
  :none
  :all)

#+liber-documentation
(setf (liber:alias-for-symbol 'filter-match)
      "GEnum"
      (liber:symbol-documentation 'filter-match)
 "@version{2023-8-16}
  @begin{declaration}
(gobject:define-g-enum \"GtkFilterMatch\" filter-match
  (:export t
   :type-initializer \"gtk_filter_match_get_type\")
  :some
  :none
  :all)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:some]{The filter matches some items, the @fun{gtk:filter-match}
        function may return @em{true} or @em{false}.}
      @entry[:none]{The filter does not match any item, the
        @fun{gtk:filter-match} function will always return @em{false}.}
      @entry[:all]{The filter matches all items, the @fun{gtk:filter-match}
        function will alays return @em{true}.}
    @end{table}
  @end{values}
  @begin{short}
    Describes the known strictness of a filter.
  @end{short}
  Note that for filters where the strictness is not known, the @code{:some}
  value is always an acceptable value, even if a filter does match all or no
  items.
  @see-class{gtk:filter}
  @see-function{gtk:filter-match}")

;;; ----------------------------------------------------------------------------
;;; GtkFilterChange
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkFilterChange" filter-change
  (:export t
   :type-initializer "gtk_filter_change_get_type")
  :different
  :less-strict
  :more-strict)

#+liber-documentation
(setf (liber:alias-for-symbol 'filter-change)
      "GEnum"
      (liber:symbol-documentation 'filter-change)
 "@version{2023-8-16}
  @begin{declaration}
(gobject:define-g-enum \"GtkFilterChange\" filter-change
  (:export t
   :type-initializer \"gtk_filter_change_get_type\")
  :different
  :less-strict
  :more-strict)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:different]{The filter change cannot be described with any of the
        other enumeration values.}
      @entry[:less-strict]{The filter is less strict than it was before. All
        items that it used to return @em{true} for still return @em{true},
        others now may, too.}
      @entry[:more-strict]{The filter is more strict than it was before. All
        items that it used to return @em{false} for still return @em{false},
        others now may, too.}
    @end{table}
  @end{values}
  @begin{short}
    Describes changes in a filter in more detail and allows objects using the
    filter to optimize refiltering items.
  @end{short}
  If you are writing an implementation and are not sure which value to pass,
  the @code{:different} value is always a correct choice.
  @see-class{gtk:filter}")

;;; ----------------------------------------------------------------------------
;;; GtkFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFilter" filter
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_filter_get_type")
  nil)

#+liber-documentation
(setf (documentation 'filter 'type)
 "@version{2023-11-3}
  @begin{short}
    The @class{gtk:filter} object describes the filtering to be performed by a
    @class{gtk:filter-list-model} object.
  @end{short}
  The model will use the filter to determine if it should include items or not
  by calling the @fun{gtk:filter-match} function for each item and only keeping
  the ones that the function returns @em{true} for.

  Filters may change what items they match through their lifetime. In that
  case, they will emit the @code{\"changed\"} signal to notify that previous
  filter results are no longer valid and that items should be checked again via
  the @fun{gtk:filter-match} function.

  GTK provides various pre-made filter implementations for common filtering
  operations. These filters often include properties that can be linked to
  various widgets to easily allow searches. However, in particular for large
  lists or complex search methods, it is also possible to subclass the
  @class{gtk:filter} class and provide one's own filter.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (filter change)    :run-last
      @end{pre}
      The signal is emitted whenever the filter changed. Users of the filter
      should then check items again via the @fun{gtk:filter-match} function.
      The @class{gtk:filter-list-model} object handles this signal
      automatically. Depending on the @arg{change} parameter, not all items
      need to be changed, but only some. Refer to the @symbol{gtk:filter-change}
      documentation for details.
      @begin[code]{table}
        @entry[filter]{The @class{gtk:filter} object.}
        @entry[change]{The @symbol{gtk:filter-change} value.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:filter-list-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_filter_match
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_filter_match" %filter-match) :boolean
  (filter (g:object filter))
  (item :pointer))

(defun filter-match (filter item)
 #+liber-documentation
 "@version{#2023-11-3}
  @argument[filter]{a @class{gtk:filter} object}
  @argument[item]{a @class{g:object} object with or a pointer to the item to
    check}
  @return{@em{True} if the filter matches the item and a filter model should
    keep it, @em{false} if not.}
  @begin{short}
    Checks if the given @arg{item} is matched by the filter or not.
  @end{short}
  @begin[Note]{dictionary}
    The C library takes a pointer as an @arg{item} argument. In the Lisp
    implementation the function is generalized to take an object. The pointer
    is retrieved from the object with the @fun{gobject:object-poiner} function.
  @end{dictionary}
  @see-class{gtk:filter}"
  (%filter-match filter (if (cffi:pointerp item)
                            item
                            (gobject:object-pointer item))))

(export 'filter-match)

;;; ----------------------------------------------------------------------------
;;; gtk_filter_get_strictness
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_filter_get_strictness" filter-strictness) filter-match
 #+liber-documentation
 "@version{2023-11-3}
  @argument[filter]{a @class{gtk:filter} object}
  @return{The @symbol{gtk:filter-match} value with the strictness of
    @arg{filter}.}
  @begin{short}
    Gets the known strictness of the filter.
  @end{short}
  If the strictness is not known, the @code{:match-some} value is returned. The
  value may change after emission of the @code{\"changed\"} signal. This
  function is meant purely for optimization purposes, filters can choose to
  omit implementing it, but the @class{gtk:filter-list-model} class uses it.
  @see-class{gtk:filter}
  @see-class{gtk:filter-list-model}
  @see-symbol{gtk:filter-match}"
  (filter (g:object filter)))

(export 'filter-strictness)

;;; ----------------------------------------------------------------------------
;;; gtk_filter_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_filter_changed" filter-changed) :void
 #+liber-documentation
 "@version{2023-11-3}
  @argument[filter]{a @class{gtk:filter} object}
  @argument[change]{a @symbol{gtk:filter-change} value}
  @begin{short}
    Emits the @code{\"changed\"} signal to notify all users of the filter that
    the filter changed.
  @end{short}
  Users of the filter should then check items again via the
  @fun{gtk:filter-match} function.
  Depending on the @arg{change} parameter, not all items need to be changed, but
  only some. Refer to the @symbol{gtk:filter-change} documentation for details.
  This function is intended for implementors of @class{gtk:filter} subclasses
  and should not be called from other functions.
  @see-class{gtk:filter}
  @see-symbol{gtk:filter-change}
  @see-function{gtk:filter-match}"
  (filter (g:object filter))
  (change filter-change))

(export 'filter-changed)

;;; --- Enf of file gtk4.filter.lisp -------------------------------------------
