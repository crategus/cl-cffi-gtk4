;;; ----------------------------------------------------------------------------
;;; gtk4.entry-completion.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkEntryCompletion" entry-completion
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

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj entry-completion) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:ENTRY-COMPLETION is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'entry-completion 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:entry-completion} object is an auxiliary object to be used in
    conjunction with the @class{gtk:entry} widget to provide the completion
    functionality.
  @end{short}
  It implements the @class{gtk:cell-layout} interface, to allow the user to add
  extra cells to the @class{gtk:tree-view} widget with completion matches.

  \"Completion functionality\" means that when the user modifies the text in
  the text entry, the @class{gtk:entry-completion} object checks which rows in
  the model match the current content of the text entry, and displays a list of
  matches. By default, the matching is done by comparing the text
  case-insensitively against the text column of the model, see the
  @fun{gtk:entry-completion-text-column} function, but this can be overridden
  with a custom match function, see the
  @fun{gtk:entry-completion-set-match-func} function.

  When the user selects a completion, the content of the text entry is updated.
  By default, the content of the text entry is replaced by the text column of
  the model, but this can be overridden by connecting to the
  @sig[gtk:entry-completion]{match-selected} signal and updating the text entry
  in the signal handler. Note that you should return @em{true} from the signal
  handler to suppress the default behaviour.

  To add completion functionality to a text entry, use the
  @fun{gtk:entry-completion} function.

  The @class{gtk:entry-completion} object uses a @class{gtk:tree-model-filter}
  model to represent the subset of the entire model that is currently matching.
  While the @class{gtk:entry-completion} widget
  @sig[gtk:entry-completion]{match-selected} and
  @sig[gtk:entry-completion]{cursor-on-match} signals take the original model
  and an iterator pointing to that model as arguments, other callbacks and
  signals, such as a @sym{gtk:cell-layout-data-func} function or the
  @sig[gtk:cell-area]{apply-attributes} signal, will generally take the filter
  model as argument. As long as you are only calling the
  @fun{gtk:tree-model-get} function, this will make no difference to you. If
  for some reason, you need the original model, use the
  @fun{gtk:tree-model-filter-model} function. Do not forget to use the
  @fun{gtk:tree-model-filter-convert-iter-to-child-iter} function to obtain a
  matching iterator.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[entry-completion::cursor-on-match]{signal}
      @begin{pre}
lambda (widget model iter)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:entry-completion} object that received
          the signal.}
        @entry[model]{The @class{gtk:tree-model} object containing the matches.}
        @entry[iter]{The @class{gtk:tree-iter} instance positioned at the
          selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{simple-table}
      Gets emitted when a match from the cursor is on a match of the list. The
      default behaviour is to replace the contents of the text entry with the
      contents of the text column in the row pointed to by @arg{iter}. Note
      that @arg{model} is the model that was passed to the
      @fun{gtk:entry-completion-model} function.
    @end{signal}
    @begin[entry-completion::insert-prefix]{signal}
      @begin{pre}
lambda (widget prefix)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:entry-completion} object that received
          the signal.}
        @entry[prefix]{The string for the common prefix of all possible
          completions.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{simple-table}
      Gets emitted when the inline autocompletion is triggered. The default
      behaviour is to make the text entry display the whole prefix and select
      the newly inserted part. Applications may connect to this signal in order
      to insert only a smaller part of the prefix into the text entry, for
      example, the text entry used in the @class{gtk:file-chooser} widget
      inserts only the part of the prefix up to the next '/'.
    @end{signal}
    @begin[entry-completion::match-selected]{signal}
      @begin{pre}
lambda (widget model iter)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:entry-completion} object that received
          the signal.}
        @entry[model]{The @class{gtk:tree-model} object containing the matches.}
        @entry[iter]{The @class{gtk:tree-iter} instance positioned at the
          selected match.}
        @entry[Returns]{@em{True} if the signal has been handled.}
      @end{simple-table}
      Gets emitted when a match from the list is selected. The default behaviour
      is to replace the contents of the text entry with the contents of the text
      column in the row pointed to by @arg{iter}. Note that @arg{model} is the
      model that was passed to the @fun{gtk:entry-completion-model} function.
    @end{signal}
    @begin[entry-completion::no-matches]{signal}
      @begin{pre}
lambda (widget)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[widget]{The @class{gtk:entry-completion} object that received
          the signal.}
      @end{simple-table}
      Gets emitted when the entry completion is out of suggestions.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:entry-completion-new}
  @see-constructor{gtk:entry-completion-new-with-area}
  @see-slot{gtk:entry-completion-cell-area}
  @see-slot{gtk:entry-completion-inline-completion}
  @see-slot{gtk:entry-completion-inline-selection}
  @see-slot{gtk:entry-completion-minimum-key-length}
  @see-slot{gtk:entry-completion-model}
  @see-slot{gtk:entry-completion-popup-completion}
  @see-slot{gtk:entry-completion-popup-set-width}
  @see-slot{gtk:entry-completion-popup-single-match}
  @see-slot{gtk:entry-completion-text-column}
  @see-class{gtk:entry}
  @see-class{gtk:cell-layout}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-model-filter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:entry-completion-cell-area -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cell-area" 'entry-completion) t)
 "The @code{cell-area} property of type @class{gtk:cell-area}
  (Read / Write / Construct) @br{}
  The cell area used to layout cell renderers in the tree view column. If no
  area is specified when creating the entry completion with the
  @fun{gtk:entry-completion-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-cell-area)
      "Accessor"
      (documentation 'entry-completion-cell-area 'function)
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-cell-area object) => area}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[area]{a @class{gtk:cell-area} object}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{cell-area} slot of the
    @class{gtk:entry-completion} class returns the cell area used to layout cell
    renderers in the tree view column.
  @end{short}
  If no area is specified when creating the entry completion with the
  @fun{gtk:entry-completion-new-with-area} function a horizontally oriented
  @class{gtk:cell-area-box} object will be used.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:cell-area}
  @see-class{gtk:cell-area-box}
  @see-function{gtk:entry-completion-new-with-area}")

;;; --- gtk:entry-completion-inline-completion ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inline-completion"
                                               'entry-completion) t)
 "The @code{inline-completion} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the common prefix of the possible completions should be
  inserted automatically in the text entry. Note that this requires the
  @slot[gtk:entry-completion]{text-column} property to be set, even if you are
  using a custom match function. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-inline-completion)
      "Accessor"
      (documentation 'entry-completion-inline-completion 'function)
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-inline-completion object) => setting}
  @syntax{(setf (gtk:entry-completion-inline-completion object) setting)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[setting]{@em{true} to do inline completion}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{inline-completion} slot of
    the @class{gtk:entry-completion} class gets or sets whether the common
    prefix of the possible completions should be automatically inserted in the
    text entry.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-inline-selection ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inline-selection"
                                               'entry-completion) t)
 "The @code{inline-selection} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether the possible completions on the popup will appear in the
  text entry as you navigate through them. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-inline-selection)
      "Accessor"
      (documentation 'entry-completion-inline-selection 'function)
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-inline-selection object) => setting}
  @syntax{(setf (gtk:entry-completion-inline-selection object) setting)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[setting]{@em{true} to do inline selection}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{inline-selection} slot of
    the @class{gtk:entry-completion} class gets or sets whether the possible
    completions on the popup will appear in the text entry as you navigate
    through them.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-minimum-key-length --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "minimum-key-length"
                                               'entry-completion) t)
 "The @code{minimum-key-length} property of type @code{:int} (Read / Write)
  @br{}
  The minimum length of the search key in order to look up matches. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-minimum-key-length)
      "Accessor"
      (documentation 'entry-completion-minimum-key-length 'function)
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-minimum-key-length object) => length}
  @syntax{(setf (gtk:entry-completion-minimum-key-length object) length)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[length]{an integer for the minimum length of the key in order to
    start completing}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{minimum-key-length} slot of
    the @class{gtk:entry-completion} class gets or sets the minimum key length
    as set for the entry completion.
  @end{short}

  This is useful for long lists, where completing using a small key takes a
  lot of time and will come up with meaningless results anyway, that is, a too
  large dataset.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-model ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'entry-completion) t)
 "The @code{model} property of type @class{gtk:tree-model} (Read / Write) @br{}
  The model to find matches in.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion-model)
      "Accessor"
      (documentation 'entry-completion-model 'function)
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-model object) => model}
  @syntax{(setf (gtk:entry-completion-model object) model)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[model]{a @class{gtk:tree-model} object}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{model} slot of the
    @class{gtk:entry-completion} class gets or sets the model for a entry
    completion.
  @end{short}
  Returns @code{nil} if none is currently being used. If the entry completion
  already has a model set, it will remove it before setting the new model. If
  @arg{model} is @code{nil}, then it will unset the model.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:tree-model}")

;;; --- gtk:entry-completion-popup-completion ----------------------------------

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
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-popup object) => setting}
  @syntax{(setf (gtk:entry-completion-popup object) setting)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[setting]{@em{true} to do popup completion}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{popup-completion} slot of
    the @class{gtk:entry-completion} class gets or sets whether the completions
    should be presented in a popup window.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-popup-set-width -----------------------------------

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
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-popup-set-width object) => setting}
  @syntax{(setf (gtk:entry-completion-popup-set-width object) setting)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[setting]{@em{true} to make the width of the popup the same as the
    text entry}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{popup-set-width} slot of
    the @class{gtk:entry-completion} class gets or sets whether the completion
    popup window will be resized to the width of the text entry.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-popup-single-match --------------------------------

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
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-popup-single-match object) => setting}
  @syntax{(setf (gtk:entry-completion-popup-single-match object) setting)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[setting]{@em{true} if the popup should appear even for a single
    match}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{popup-single-match} slot
    of the @class{gtk:entry-completion} class gets or sets whether the
    completions popup window will shown for a single possible completion.
  @end{short}
  You may want to set this to @em{false} if you are using inline completion.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-completion-text-column ---------------------------------------

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
 "@version{2025-09-24}
  @syntax{(gtk:entry-completion-text-column) => column}
  @syntax{(setf (gtk:entry-completion-text-column object) column)}
  @argument[object]{a @class{gtk:entry-completion} object}
  @argument[column]{an integer for the column in the model of the completion
    to get strings from}
  @begin{short}
    The accessor for the @slot[gtk:entry-completion]{text-column} slot of the
    @class{gtk:entry-completion} class gets or sets the column in the model of
    the completion to get strings from.
  @end{short}
  This function will set up completion to have a list displaying all, and just,
  strings in the completion list, and to get those strings from column in the
  model of completion.

  This functions creates and adds a @class{gtk:cell-renderer-text} object for
  the selected column. If you need to set the text column, but do not want the
  cell renderer, use the @fun{g:object-property} function to set the
  @slot[gtk:entry-completion]{text-column} property directly.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:cell-renderer-text}
  @see-function{g:object-property}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new
;;; ----------------------------------------------------------------------------

(declaim (inline entry-completion-new))

(defun entry-completion-new ()
 #+liber-documentation
 "@version{#2024-05-02}
  @return{The newly created @class{gtk:entry-completion} object.}
  @short{Creates a new entry completion.}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}"
  (make-instance 'entry-completion))

(export 'entry-completion-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_new_with_area
;;; ----------------------------------------------------------------------------

(declaim (inline entry-completion-new-with-area))

(defun entry-completion-new-with-area (area)
 #+liber-documentation
 "@version{2024-02-19}
  @argument[area]{a @class{gtk:cell-area} object used to layout cells}
  @return{The newly created @class{gtk:entry-completion} object.}
  @begin{short}
    Creates a new entry completion using the specified area to layout cells in
    the underlying @class{gtk:tree-view-column} object for the drop-down menu.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:cell-area}
  @see-class{gtk:tree-view-column}"
  (make-instance 'entry-completion
                 :cell-area area))

(export 'entry-completion-new-with-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_entry
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_get_entry" entry-completion-entry)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-03-19}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @begin{return}
    The @class{gtk:widget} object the entry completion has been attached to.
  @end{return}
  @short{Gets the widget the entry completion has been attached to.}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:widget}"
  (completion (g:object entry-completion)))

(export 'entry-completion-entry)

;;; ----------------------------------------------------------------------------
;;; GtkEntryCompletionMatchFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback entry-completion-match-func :boolean
  ((completion (g:object entry-completion))
   (key :string)
   (iter (g:boxed tree-iter))
   (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func completion key iter)))

#+liber-documentation
(setf (liber:alias-for-symbol 'entry-completion-match-func)
      "Callback"
      (liber:symbol-documentation 'entry-completion-match-func)
 "@version{#2024-05-03}
  @syntax{lambda (completion key iter) => result}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @argument[key]{a string to match, normalized and case-folded}
  @argument[iter]{a @class{gtk:tree-iter} iterator indicating the row to match}
  @argument[result]{@em{true} if @arg{iter} should be displayed as a possible
    completion for @arg{key}}
  @begin{short}
    A callback function which decides whether the row indicated by @arg{iter}
    matches a given @code{key}, and should be displayed as a possible completion
    for @arg{key}.
  @end{short}
  Note that @arg{key} is normalized and case-folded, see the
  @code{g_utf8_normalize()} and @code{g_utf8_casefold()} functions. If this is
  not appropriate, match functions have access to the unmodified key via the
  @code{(gtk:entry-text (gtk:entry-completion-entry completion))} function call.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-class{gtk:tree-iter}
  @see-function{gtk:entry-completion-set-match-func}")

(export 'entry-completion-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_set_match_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_set_match_func"
               %entry-completion-set-match-func) :void
  (completion (g:object entry-completion))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun entry-completion-set-match-func (completion func)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @argument[func]{a @sym{gtk:entry-completion-match-func} callback function
    to use}
  @begin{short}
    Sets the match function for the entry completion to be @arg{func}.
  @end{short}
  The match function is used to determine if a row should or should not be in
  the completion list.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-symbol{gtk:entry-completion-match-func}"
  (if func
      (%entry-completion-set-match-func
              completion
              (cffi:callback entry-completion-match-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%entry-completion-set-match-func
              completion
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'entry-completion-set-match-func)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_compute_prefix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_compute_prefix"
               entry-completion-compute-prefix) :string
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @argument[key]{a string for the text to complete for}
  @begin{return}
    The string for the common prefix all rows starting with @arg{key} or
    @code{nil} if no row matches @arg{key}.
  @end{return}
  @begin{short}
    Computes the common prefix that is shared by all rows in @arg{completion}
    that start with @arg{key}.
  @end{short}
  If no row matches @arg{key}, the @code{nil} values will be returned. Note that
  a text column must have been set for this function to work, see the
  @fun{gtk:entry-completion-text-column} function for details.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}
  @see-function{gtk:entry-completion-text-column}"
  (completion (g:object entry-completion))
  (key :string))

(export 'entry-completion-compute-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_complete
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_complete" entry-completion-complete) :void
 #+liber-documentation
 "@version{#2025-03-19}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @begin{short}
    Requests a completion operation, or in other words a refiltering of the
    current list with completions, using the current key.
  @end{short}
  The completion list view will be updated accordingly.
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-complete)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_get_completion_prefix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_get_completion_prefix"
               entry-completion-completion-prefix)
    (:string :free-from-foreign t)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @return{The string for the prefix for the current completion.}
  @begin{short}
    Get the original text entered by the user that triggered the completion or
    @code{nil} if there is no completion ongoing.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-completion-prefix)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_completion_insert_prefix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_completion_insert_prefix"
               entry-completion-insert-prefix) :void
 #+liber-documentation
 "@version{#2025-03-19}
  @argument[completion]{a @class{gtk:entry-completion} object}
  @begin{short}
    Requests a prefix insertion.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:entry-completion} implementation is deprecated since 4.10.
    This object will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry-completion}"
  (completion (g:object entry-completion)))

(export 'entry-completion-insert-prefix)

;;; --- End of file gtk4.entry-completion.lisp ---------------------------------
