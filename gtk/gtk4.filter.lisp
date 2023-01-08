;;; ----------------------------------------------------------------------------
;;; gtk.filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;
;;; GtkFilter
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
;;; enum GtkFilterMatch
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFilterMatch" filter-match
  (:export t
   :type-initializer "gtk_filter_match_get_type")
  :some
  :none
  :all)

#+liber-documentation
(setf (liber:alias-for-symbol 'filter-match)
      "GEnum"
      (liber:symbol-documentation 'filter-match)
 "@version{#2022-11-20}
  @begin{short}
    Describes the known strictness of a filter.
  @end{short}
  Note that for filters where the strictness is not known, the @code{:some}
  value is always an acceptable value, even if a filter does match all or no
  items.
  @begin{pre}
(define-g-enum \"GtkFilterMatch\" filter-match
  (:export t
   :type-initializer \"gtk_filter_match_get_type\")
  :some
  :none
  :all)
  @end{pre}
  @begin[code]{table}
    @entry[:some]{The filter matches some items, the @fun{gtk:filter-match}
      function may return @em{true} or @em{false}.}
    @entry[:none]{The filter does not match any item, the @fun{gtk:filter-match}
      function will always return @em{false}.}
    @entry[:all]{The filter matches all items, the @fun{gtk:filter-match}
    function will alays return @em{true}.}
  @end{table}
  @see-class{gtk:filter}
  @see-function{gtk:filter-match}")

;;; ----------------------------------------------------------------------------
;;; enum GtkFilterChange
;;;
;;; Describes changes in a filter in more detail and allows objects using the
;;; filter to optimize refiltering items.
;;;
;;; If you are writing an implementation and are not sure which value to pass,
;;; GTK_FILTER_CHANGE_DIFFERENT is always a correct choice.
;;;
;;; GTK_FILTER_CHANGE_DIFFERENT :
;;;     The filter change cannot be described with any of the other enumeration
;;;     values.
;;;
;;; GTK_FILTER_CHANGE_LESS_STRICT :
;;;     The filter is less strict than it was before: All items that it used to
;;;     return TRUE for still return TRUE, others now may, too.
;;;
;;; GTK_FILTER_CHANGE_MORE_STRICT :
;;;     The filter is more strict than it was before: All items that it used to
;;;     return FALSE for still return FALSE, others now may, too.
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkFilterChange" filter-change
  (:export t
   :type-initializer "gtk_filter_change_get_type")
  :different
  :less-strict
  :more-strict)

;;; ----------------------------------------------------------------------------
;;; GtkFilter
;;;
;;; A GtkFilter object describes the filtering to be performed by a
;;; GtkFilterListModel.
;;;
;;; The model will use the filter to determine if it should include items or not
;;; by calling gtk_filter_match() for each item and only keeping the ones that
;;; the function returns TRUE for.
;;;
;;; Filters may change what items they match through their lifetime. In that
;;; case, they will emit the “changed” signal to notify that previous filter
;;; results are no longer valid and that items should be checked again via
;;; gtk_filter_match().
;;;
;;; GTK provides various pre-made filter implementations for common filtering
;;; operations. These filters often include properties that can be linked to
;;; various widgets to easily allow searches.
;;;
;;; However, in particular for large lists or complex search methods, it is also
;;; possible to subclass GtkFilter and provide one's own filter.
;;;
;;; See Also
;;;     GtkFilterListModel
;;;
;;; Signal Details
;;;
;;; The “changed” signal
;;;
;;; void
;;; user_function (GtkFilter      *self,
;;;                GtkFilterChange change,
;;;                gpointer        user_data)
;;;
;;; This signal is emitted whenever the filter changed. Users of the filter
;;; should then check items again via gtk_filter_match().
;;;
;;; GtkFilterListModel handles this signal automatically.
;;;
;;; Depending on the change parameter, not all items need to be changed, but
;;; only some. Refer to the GtkFilterChange documentation for details.
;;;
;;; self :
;;;     The GtkFilter
;;;
;;; change :
;;;     how the filter changed
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run Last
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFilter" filter
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_filter_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_filter_match ()
;;;
;;; gboolean
;;; gtk_filter_match (GtkFilter *self,
;;;                   gpointer item);
;;;
;;; Checks if the given item is matched by the filter or not.
;;;
;;; self :
;;;     a GtkFilter
;;;
;;; item :
;;;     The item to check.
;;;
;;; Returns :
;;;     TRUE if the filter matches the item and a filter model should keep it,
;;;     FALSE if not.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_filter_match" filter-match) :boolean
  (filter (g:object filter))
  (item :pointer))

(export 'filter-match)

;;; ----------------------------------------------------------------------------
;;; gtk_filter_get_strictness ()
;;;
;;; GtkFilterMatch
;;; gtk_filter_get_strictness (GtkFilter *self);
;;;
;;; Gets the known strictness of filters . If the strictness is not known,
;;; GTK_FILTER_MATCH_SOME is returned.
;;;
;;; This value may change after emission of the “changed” signal.
;;;
;;; This function is meant purely for optimization purposes, filters can choose
;;; to omit implementing it, but GtkFilterListModel uses it.
;;;
;;; self :
;;;     a GtkFilter
;;;
;;; Returns :
;;;     the strictness of self
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_filter_get_strictness" filter-strictness) filter-match
  (filter (g:object filter)))

(export 'filter-strictness)

;;; ----------------------------------------------------------------------------
;;; gtk_filter_changed ()
;;;
;;; void
;;; gtk_filter_changed (GtkFilter *self,
;;;                     GtkFilterChange change);
;;;
;;; Emits the “changed” signal to notify all users of the filter that the filter
;;; changed. Users of the filter should then check items again via
;;; gtk_filter_match().
;;;
;;; Depending on the change parameter, not all items need to be changed, but
;;; only some. Refer to the GtkFilterChange documentation for details.
;;;
;;; This function is intended for implementors of GtkFilter subclasses and
;;; should not be called from other functions.
;;;
;;; self :
;;;     a GtkFilter
;;;
;;; change :
;;;     How the filter changed
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_filter_changed" filter-changed) :void
  (filter (g:object filter))
  (change filter-change))

(export 'filter-changed)

;;; --- Enf of file gtk.filter.lisp --------------------------------------------
