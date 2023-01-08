;;; ----------------------------------------------------------------------------
;;; gtk.entry-completion.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkEntryCompletion
;;;
;;;     Completion functionality for GtkEntry
;;;
;;; Types and Values
;;;
;;;     GtkEntryCompletion
;;;
;;; Accessors
;;;
;;;     gtk_entry_completion_set_inline_completion
;;;     gtk_entry_completion_get_inline_completion
;;;     gtk_entry_completion_set_inline_selection
;;;     gtk_entry_completion_get_inline_selection
;;;     gtk_entry_completion_set_minimum_key_length
;;;     gtk_entry_completion_get_minimum_key_length
;;;     gtk_entry_completion_set_model
;;;     gtk_entry_completion_get_model
;;;     gtk_entry_completion_set_popup_completion
;;;     gtk_entry_completion_get_popup_completion
;;;     gtk_entry_completion_set_popup_set_width
;;;     gtk_entry_completion_get_popup_set_width
;;;     gtk_entry_completion_set_popup_single_match
;;;     gtk_entry_completion_get_popup_single_match
;;;     gtk_entry_completion_set_text_column
;;;     gtk_entry_completion_get_text_column
;;;
;;; Functions
;;;
;;;     GtkEntryCompletionMatchFunc
;;;     gtk_entry_completion_new
;;;     gtk_entry_completion_new_with_area
;;;     gtk_entry_completion_get_entry
;;;     gtk_entry_completion_set_match_func
;;;     gtk_entry_completion_compute_prefix
;;;     gtk_entry_completion_complete
;;;     gtk_entry_completion_get_completion_prefix
;;;     gtk_entry_completion_insert_prefix
;;;
;;; Properties
;;;
;;;     cell-area
;;;     inline-completion
;;;     inline-selection
;;;     minimum-key-length
;;;     model
;;;     popup-completion
;;;     popup-set-width
;;;     popup-single-match
;;;     text-column
;;;
;;; Signals
;;;
;;;     cursor-on-match
;;;     insert-prefix
;;;     match-selected
;;;     no-matches
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEntryCompletion
;;;
;;; Implemented Interfaces
;;;
;;;     GtkCellLayout
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEntryCompletion
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEntryCompletion" entry-completion
  (:superclass g:object
   :export t
   :interfaces("GtkBuildable"
               "GtkCellLayout")
   :type-initializer "gtk_entry_completion_get_type")
  ((cell-area
    entry-completion-cell-area
    "cell-area" "GtkCellArea" t t)
   (inline-completion
    entry-completion-inline-completion
    "inline-completion" "gboolean" t t)
   (inline-selection
    entry-completion-inline-selection
    "inline-selection" "gboolean" t t)
   (minimum-key-length
    entry-completion-minimum-key-length
    "minimum-key-length" "gint" t t)
   (model
    entry-completion-model
    "model" "GtkTreeModel" t t)
   (popup-completion
    entry-completion-popup-completion
    "popup-completion" "gboolean" t t)
   (popup-set-width
    entry-completion-popup-set-width
    "popup-set-width" "gboolean" t t)
   (popup-single-match
    entry-completion-popup-single-match
    "popup-single-match" "gboolean" t t)
   (text-column
    entry-completion-text-column
    "text-column" "gint" t t)))

#+liber-documentation
(setf (documentation 'entry-completion 'type)
 "@version{2022-9-10}
  @begin{short}
    The @sym{gtk:entry-completion} object is an auxiliary object to be used in
    conjunction with the @class{gtk:entry} widget to provide the completion
    functionality.
  @end{short}
  It implements the @class{gtk:cell-layout} interface, to allow the user to add
  extra cells to the @class{gtk:tree-view} widget with completion matches.

  \"Completion functionality\" means that when the user modifies the text in
  the entry, the @sym{gtk:entry-completion} object checks which rows in the
  model match the current content of the entry, and displays a list of matches.
  By default, the matching is done by comparing the entry text
  case-insensitively against the text column of the model, see the
  @fun{gtk:entry-completion-text-column} function, but this can be overridden
  with a custom match function, see the
  @fun{gtk:entry-completion-set-match-func} function.

  When the user selects a completion, the content of the entry is updated. By
  default, the content of the entry is replaced by the text column of the
  model, but this can be overridden by connecting to the \"match-selected\"
  signal and updating the entry in the signal handler. Note that you should
  return @em{true} from the signal handler to suppress the default behaviour.

  To add completion functionality to an entry, use the
  @fun{gtk:entry-completion} function.

  The @sym{gtk:entry-completion} object uses a @class{gtk:tree-model-filter}
  model to represent the subset of the entire model that is currently matching.
  While the @sym{gtk:entry-completion} signals \"match-selected\" and
  \"cursor-on-match\" take the original model and an iterator pointing to that
  model as arguments, other callbacks and signals, such as a
  @symbol{gtk:cell-layoutdata-funcs} function or the \"apply-attributes\"
  signal, will generally take the filter model as argument. As long as you are
  only calling the @fun{gtk:tree-model-get} function, this will make no
  difference to you. If for some reason, you need the original model, use the
  @fun{gtk:tree-model-filter-model} function. Do not forget to use the
  @fun{gtk:tree-model-filter-convert-iter-to-child-iter} function to obtain a
  matching iterator.
  @begin[Signal Details]{dictionary}
    @subheading{The \"cursor-on-match\" signal}
      @begin{pre}
lambda (widget model iter)    :run-last
      @end{pre}
      Gets emitted when a match from the cursor is on a match of the list. The
      default behaviour is to replace the contents of the entry with the
      contents of the text column in the row pointed to by @arg{iter}. Note
      that @arg{model} is the model that was passed to the
      @fun{gtk:entry-completion-model} function.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:entry-completion} object which received
          the signal.}
        @entry[model]{The @class{gtk:tree-model} object containing the matches.}
        @entry[iter]{A @class{gtk:tree-iter} instance positioned at the
          selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"insert-prefix\" signal}
      @begin{pre}
lambda (widget prefix)    :run-last
      @end{pre}
      Gets emitted when the inline autocompletion is triggered. The default
      behaviour is to make the entry display the whole prefix and select the
      newly inserted part. Applications may connect to this signal in order to
      insert only a smaller part of the prefix into the entry - e.g. the entry
      used in the @class{gtk:file-chooser} widget inserts only the part of the
      prefix up to the next '/'.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:entry-completion} object which received
          the signal.}
        @entry[prefix]{A string with the common prefix of all possible
          completions.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"match-selected\" signal}
      @begin{pre}
lambda (widget model iter)    :run-last
      @end{pre}
      Gets emitted when a match from the list is selected. The default behaviour
      is to replace the contents of the entry with the contents of the text
      column in the row pointed to by @arg{iter}. Note that @arg{model} is the
      model that was passed to the @fun{gtk:entry-completion-model} function.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:entry-completion} object which received the
          signal.}
        @entry[model]{The @class{gtk:tree-model} object containing the matches.}
        @entry[iter]{A @class{gtk:tree-iter} instance positioned at the
          selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{table}
    @subheading{The \"no-matches\" signal}
      @begin{pre}
lambda (widget)    :run-last
      @end{pre}
      Gets emitted when the entry completion is out of suggestions.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:entry-completion} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:entry-completion-cell-area}
  @see-slot{gtk:entry-completion-inline-completion}
  @see-slot{gtk:entry-completion-inline-selection}
  @see-slot{gtk:entry-completion-minimum-key-length}
  @see-slot{gtk:entry-completion-model}
  @see-slot{gtk:entry-completion-popup-completion}
  @see-slot{gtk:entry-completion-popup-set-width}
  @see-slot{gtk:entry-completion-popup-single-match}
  @see-slot{gtk:entry-completion-text-column}
  @see-constructor{gtk:entry-completion-new}
  @see-constructor{gtk:entry-completion-new-with-area}
  @see-class{gtk:entry}
  @see-class{gtk:cell-layout}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model-filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- entry-completion-cell-area ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area" 'entry-completion) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers in the treeview column. If no area
  is specified when creating the entry completion with the
  @fun{gtk:entry-completion-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-cell-area)
      "Accessor"
      (documentation 'entry-completion-cell-area 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-cell-area object) => cell-area}
  @syntax[]{(setf (gtk:entry-completion-cell-area object) cell-area)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[cell-area]{a @class{gtk:cell-area} object}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{cell-area} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The cell area used to layout cell renderers in the treeview column. If no area
  is specified when creating the entry completion with the
  @fun{gtk:entry-completion-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-inline-completion -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inline-completion"
                                               'entry-completion) t)
 "The @code{inline-completion} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the common prefix of the possible completions should be
  inserted automatically in the entry. Note that this requires the
  @code{text-column} property to be set, even if you are using a custom
  match function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-inline-completion)
      "Accessor"
      (documentation 'entry-completion-inline-completion 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-inline-completion object)
            => inline-completion}
  @syntax[]{(setf (gtk:entry-completion-inline-completion object)
                  inline-completion)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[inline-completion]{@em{true} to do inline completion}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{inline-completion} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-inline-completion} function returns whether the
  common prefix of the possible completions should be automatically inserted in
  the entry. The @sym{(setf gtk:entry-completion-inline-completion)} function
  sets whether the common prefix of the possible completions should be
  automatically inserted in the entry.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-inline-selection --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inline-selection"
                                               'entry-completion) t)
 "The @code{inline-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the possible completions on the popup will appear in the
  entry as you navigate through them. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-inline-selection)
      "Accessor"
      (documentation 'entry-completion-inline-selection 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-inline-selection object)
            => inline-selection}
  @syntax[]{(setf (gtk:entry-completion-inline-selection object)
                  inline-selection)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[inline-selection]{@em{true} to do inline selection}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{inline-selection} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-inline-selection} function returns @em{true} if
  inline selection mode is turned on. The
  @sym{(setf gtk:entry-completion-inline-selection)} function sets whether it
  is possible to cycle through the possible completions inside the entry.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-minimum-key-length ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "minimum-key-length"
                                               'entry-completion) t)
 "The @code{minimum-key-length} property of type @code{:int} (Read / Write)
  @br{}
  Minimum length of the search key in order to look up matches. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-minimum-key-length)
      "Accessor"
      (documentation 'entry-completion-minimum-key-length 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-minimum-key-length object) => length}
  @syntax[]{(setf (gtk:entry-completion-minimum-key-length object) length)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[length]{an integer with the minimum length of the key in order to
    start completing}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{minimum-key-length} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion} function returns the minimum key length as set
  for the entry completion. The
  @sym{(setf gtk:entry-completion-minimum-key-length)} function sets the length
  of the search key for completion to be at least @arg{length}.

  This is useful for long lists, where completing using a small key takes a
  lot of time and will come up with meaningless results anyway, i.e., a too
  large dataset.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-model -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'entry-completion) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model to find matches in.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-model)
      "Accessor"
      (documentation 'entry-completion-model 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-model object) => model}
  @syntax[]{(setf (gtk:entry-completion-model object) model)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[model]{the @class{gtk:tree-model} object}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{model} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-model} function returns the
  @class{gtk:tree-model} object, or @code{nil} if none is currently being used.
  The @sym{(setf gtk:entry-completion-model)} function sets the model for a
  entry completion. If the entry completion already has a model set, it will
  remove it before setting the new model. If @arg{model} is @code{nil}, then it
  will unset the model.
  @see-class{gtk:entry-completion}
  @see-class{gtk:tree-model}")

;;; --- entry-completion-popup-completion --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-completion"
                                               'entry-completion) t)
 "The @code{popup-completion} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the possible completions should be shown in a popup
  window. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-popup-completion)
      "Accessor"
      (documentation 'entry-completion-popup-completion 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-popup object) => popup-completion}
  @syntax[]{(setf (gtk:entry-completion-popup object) popup-completion)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[popup-completion]{@em{true} to do popup completion}
  @begin{short}
    Accessor of the @slot[gtk:entry-completition]{popup-completion} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-popup-completion} function returns whether the
  completions should be presented in a popup window. The
  @sym{(setf gtk:entry-completion-popup-completion)} function sets whether the
  completions should be presented in a popup window.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-popup-set-width ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-set-width"
                                               'entry-completion) t)
 "The @code{popup-set-width} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the completions popup window will be resized to the width
  of the entry. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-popup-set-width)
      "Accessor"
      (documentation 'entry-completion-popup-set-width 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-popup-set-width object) => popup-set-width}
  @syntax[]{(setf (gtk:entry-completion-popup-set-width object) popup-set-width)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[popup-set-width]{@em{true} to make the width of the popup the same
    as the entry}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{popup-set-width} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-popup-set-width} function returns whether the
  completion popup window will be resized to the width of the entry. The
  @sym{(setf gtk:entry-completion-popup-set-width)} function sets whether the
  completion popup window will be resized to be the same width as the entry.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-popup-single-match ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "popup-single-match"
                                               'entry-completion) t)
 "The @code{popup-single-match} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the completions popup window will shown for a single
  possible completion. You probably want to set this to @em{false} if you are
  using inline completion. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-popup-single-match)
      "Accessor"
      (documentation 'entry-completion-popup-single-match 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-popup-single-match object) => single-match}
  @syntax[]{(setf (gtk:entry-completion-popup-single-match object) single-match)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[single-match]{@em{true} if the popup should appear even for a single
    match}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{popup-single-match} slot
    of the @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-popup-single-match} function returns @em{true}
  if the popup window will appear regardless of the number of matches. The
  @sym{(setf gtk:entry-completion-popup-single-match)} function sets whether
  the completion popup window will appear even if there is only a single match.

  You may want to set this to @em{false} if you are using inline completion.
  @see-class{gtk:entry-completion}")

;;; --- entry-completion-text-column -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-column"
                                               'entry-completion) t)
 "The @code{text-column} property of type @code{:int} (Read / Write) @br{}
  The column of the model containing the strings. Note that the strings must
  be UTF-8. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-text-column)
      "Accessor"
      (documentation 'entry-completion-text-column 'function)
 "@version{#2020-5-31}
  @syntax[]{(gtk:entry-completion-text-column) => column}
  @syntax[]{(setf (gtk:entry-completion-text-column object) column)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[column]{an integer with the column in the model of the completion
    to get strings from}
  @begin{short}
    Accessor of the @slot[gtk:entry-completion]{text-column} slot of the
    @class{gtk:entry-completion} class.
  @end{short}

  The @sym{gtk:entry-completion-text-column} function returns the column in the
  model of the completion to get strings from. The
  @sym{(setf gtk:entry-completion-text-column)} function is a convenience
  function for setting up the most used case: a completion list with just
  strings.

  This function will set up completion to have a list displaying all, and just,
  strings in the completion list, and to get those strings from column in the
  model of completion.

  This functions creates and adds a @class{gtk:cell-renderer-text} object for
  the selected column. If you need to set the text column, but do not want the
  cell renderer, use the @fun{g:object-property} function to set the
  @slot[gtk:entry-completion]{text-column} property directly.
  @see-class{gtk:entry-completion}
  @see-class{gtk:cell-renderer-text}
  @see-function{g:object-property}
  @see-function{gtk:entry-completion-inline-completion}")

;;; ----------------------------------------------------------------------------
;;; GtkEntryCompletionMatchFunc ()
;;; ----------------------------------------------------------------------------

(define-cb-methods entry-completion-match-func :boolean
  ((completion (g:object entry-completion))
   (key :string)
   (iter (g:boxed tree-iter))))

#+liber-documentation
(setf (liber:alias-for-symbol 'entry-completion-match-func)
      "Callback"
      (liber:symbol-documentation 'entry-completion-match-func)
 "@version{#2021-10-26}
  @begin{short}
    A callback function which decides whether the row indicated by @arg{iter}
    matches a given key, and should be displayed as a possible completion for
    @arg{key}.
  @end{short}
  Note that @arg{key} is normalized and case-folded, see the
  @code{g_utf8_normalize()} and @code{g_utf8_casefold()} functions. If this is
  not appropriate, match functions have access to the unmodified key via the
  @code{(gtk:entry-text (gtk:entry-completion-entry completion))} call.
  @begin{pre}
 lambda (completion key iter)
  @end{pre}
  @begin[code]{table}
    @entry[completion]{A @class{gtk:entry-completion} object.}
    @entry[key]{A string to match, normalized and case-folded.}
    @entry[iter]{A @class{gtk:tree-iter} iterator indicating the row to match.}
    @entry[Returns]{@em{True} if @arg{iter} should be displayed as a possible
      completion for @arg{key}.}
  @end{table}
  @see-class{gtk:entry-completion}
  @see-class{gtk:tree-iter}
  @see-function{gtk:entry-completion-set-match-func}")

(export 'entry-completion-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline entry-completion-new))

(defun entry-completion-new ()
 #+liber-documentation
 "@version{#2020-5-31}
  @return{A newly created @class{gtk:entry-completion} object.}
  @short{Creates a new entry completion.}
  @see-class{gtk:entry-completion}
  @see-function{gtk:entry-completion-new-with-area}"
  (make-instance 'entry-completion))

(export 'entry-completion-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new_with_area ()
;;; ----------------------------------------------------------------------------

(declaim (inline entry-completion-new-with-area))

(defun entry-completion-new-with-area (area)
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[area]{the @class{gtk:cell-area} used to layout cells}
  @return{A newly created @class{gtk:entry-completion} object.}
  @begin{short}
    Creates a new entry completion using the specified area to layout cells in
    the underlying @class{gtk:tree-view-column} for the drop-down menu.
  @end{short}
  @see-class{gtk:entry-completion}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-view-column}
  @see-function{gtk:entry-completion-new}"
  (make-instance 'entry-completion
                 :cell-area area))

(export 'entry-completion-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_entry () -> entry-completion-entry
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_entry" entry-completion-entry)
    (g:object widget)
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @return{The @class{gtk:widget} entry the entry completion has been attached
    to.}
  @short{Gets the entry the entry completion has been attached to.}
  @see-class{gtk:entry-completion}
  @see-class{gtk:entry}"
  (completion (g:object entry-completion)))

(export 'entry-completion-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_match_func ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_set_match_func"
          %entry-completion-set-match-func) :void
  (completion (g:object entry-completion))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun entry-completion-set-match-func (completion func)
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @argument[func]{the @symbol{gtk:entry-completion-match-func} callback
    function to use}
  @begin{short}
    Sets the match function for the entry completion to be @arg{func}.
  @end{short}
  The match function is used to determine if a row should or should not be in
  the completion list.
  @see-class{gtk:entry-completion}
  @see-symbol{gtk:entry-completion-match-func}"
  (if func
      (%entry-completion-set-match-func
          completion
          (cffi:callback entry-completion-match-func)
          (gobject:create-fn-ref completion func)
          (cffi:callback entry-completion-match-func-destroy-notify))
      (%entry-completion-set-match-func completion (null-pointer)
                                                   (null-pointer)
                                                   (null-pointer))))

(export 'entry-completion-set-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_compute_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_compute_prefix" entry-completion-compute-prefix)
    :string
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @argument[key]{a string with the text to complete for}
  @begin{return}
    The common prefix all rows starting with key or @code{nil} if no row matches
    key.
  @end{return}
  @begin{short}
    Computes the common prefix that is shared by all rows in completion that
    start with @arg{key}.
  @end{short}
  If no row matches @arg{key}, @code{nil} will be returned. Note that a text
  column must have been set for this function to work, see the function
  @fun{gtk:entry-completion-text-column} for details.
  @see-class{gtk:entry-completion}
  @see-function{gtk:entry-completion-text-column}"
  (completion (g:object entry-completion))
  (key :string))

(export 'entry-completion-compute-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_complete ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_complete" entry-completion-complete) :void
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @begin{short}
    Requests a completion operation, or in other words a refiltering of the
    current list with completions, using the current key.
  @end{short}
  The completion list view will be updated accordingly.
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_completion_prefix ()
;;; -> entry-completion-completion-prefix
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_get_completion_prefix"
           entry-completion-completion-prefix) (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @return{A string with the prefix for the current completion.}
  @begin{short}
    Get the original text entered by the user that triggered the completion or
    @code{nil} if there is no completion ongoing.
  @end{short}
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_prefix ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_entry_completion_insert_prefix" entry-completion-insert-prefix)
    :void
 #+liber-documentation
 "@version{#2020-5-31}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @begin{short}
    Requests a prefix insertion.
  @end{short}
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-insert-prefix)

;;; --- End of file gtk.entry-completion.lisp ----------------------------------
