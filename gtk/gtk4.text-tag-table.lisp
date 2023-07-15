;;; ----------------------------------------------------------------------------
;;; gtk4.text-tag-table.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkTextTagTable
;;;
;;;     Collection of tags that can be used together
;;;
;;; Types and Values
;;;
;;;     GtkTextTagTable
;;;
;;; Functions
;;;
;;;     GtkTextTagTableForeach
;;;
;;;     gtk_text_tag_table_new
;;;     gtk_text_tag_table_add
;;;     gtk_text_tag_table_remove
;;;     gtk_text_tag_table_lookup
;;;     gtk_text_tag_table_foreach
;;;     gtk_text_tag_table_get_size
;;;
;;; Signals
;;;
;;;     tag-added
;;;     tag-changed
;;;     tag-removed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextTagTable
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable.
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTable
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTextTagTable" text-tag-table
  (:superclass g:object
   :export t
   :interfaces ("GtkBuildable")
   :type-initializer "gtk_text_tag_table_get_type")
  nil)

#+liber-documentation
(setf (documentation 'text-tag-table 'type)
 "@version{#2021-11-18}
  @begin{short}
    A tag table defines a set of tags that can be used together.
  @end{short}
  Each tag is stored in a @sym{gtk:text-tag-table} object. Each text buffer has
  one tag table associated with it. Only tags from that tag table can be used
  with the text buffer. A single tag table can be shared between multiple text
  buffers, however.
  @begin[GtkTextTagTable as GtkBuildable]{dictionary}
    The @sym{gtk:text-tag-table} implementation of the @class{gtk:buildable}
    interface supports adding tags by specifying @code{\"tag\"} as the
    @code{type} attribute of a @code{<child>} element.

    @b{Example:} A UI definition fragment specifying tags.
    @begin{pre}
<object class=\"GtkTextTagTable\">
 <child type=\"tag\">
   <object class=\"GtkTextTag\"/>
 </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"tag-added\" signal}
      @begin{pre}
lambda (table tag)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk:text-tag-table} object which received the
          signal.}
        @entry[tag]{The added @class{gtk:text-tag} object.}
      @end{table}
    @subheading{The \"tag-changed\" signal}
      @begin{pre}
lambda (table tag changed)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk:text-tag-table} object which received the
          signal.}
        @entry[tag]{The changed @class{gtk:text-tag} object.}
        @entry[changed]{A boolean whether the size has been changed.}
      @end{table}
    @subheading{The \"tag-removed\" signal}
      @begin{pre}
lambda (table tag)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[table]{The @sym{gtk:text-tag-table} object which received the
          signal.}
        @entry[tag]{The removed @class{gtk:text-tag} object.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}")

;;; ----------------------------------------------------------------------------
;;; GtkTextTagTableForeach
;;; ----------------------------------------------------------------------------

(cffi:defcallback text-tag-table-foreach-func :void
    ((tag (g:object text-tag))
     (data :pointer))
  (funcall (glib:get-stable-pointer-value data) tag))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-tag-table-foreach-func)
      "Callback"
      (liber:symbol-documentation 'text-tag-table-foreach-func)
 "@version{#2021-11-18}
  @begin{short}
    The type of callback function passed to the @fun{gtk:text-table-foreach}
    function.
  @end{short}
  @begin{pre}
lambda (tag)
  @end{pre}
  @begin[code]{table}
    @entry[tag]{The @class{gtk:text-trag} object.}
  @end{table}
  @see-class{gtk:text-tag-table}
  @see-function{gtk:text-tag-table-foreach}")

(export 'text-tag-table-foreach-func)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_new
;;; ----------------------------------------------------------------------------

(declaim (inline text-tag-table-new))

(defun text-tag-table-new ()
 #+liber-documentation
 "@version{#2021-11-18}
  @return{A new @class{gtk:text-tag-table} object.}
  @begin{short}
    Creates a new tag table.
  @end{short}
  The tag table contains no tags by default.
  @see-class{gtk:text-tag-table}"
  (make-instance 'text-tag-table))

(export 'text-tag-table-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_add
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_table_add" text-tag-table-add) :boolean
 #+liber-documentation
 "@version{2022-12-4}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @argument[tag]{a @class{gtk:text-tag} object}
  @return{A boolean which is @em{true} on success.}
  @begin{short}
    Adds a tag to the tag table.
  @end{short}
  The tag is assigned the highest priority in the tag table. The tag must not
  be in a tag table already, and may not have the same name as an already added
  tag.
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-tag}"
  (table (g:object text-tag-table))
  (tag (g:object text-tag)))

(export 'text-tag-table-add)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_table_remove" text-tag-table-remove) :void
 #+liber-documentation
 "@version{#2021-11-18}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @argument[tag]{a @class{gtk:text-tag} object}
  @begin{short}
    Remove a tag from the tag table.
  @end{short}
  This will remove the reference of the tag table to the tag, so be careful -
  the tag will end up destroyed if you do not have a reference to it.
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-tag}"
  (table (g:object text-tag-table))
  (tag (g:object text-tag)))

(export 'text-tag-table-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_lookup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_table_lookup" text-tag-table-lookup)
    (g:object gtk:text-tag)
 #+liber-documentation
 "@version{#2021-11-18}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @argument[name]{a string with the name of a tag}
  @return{The tag, or @code{nil} if none by that @arg{name} is in the tag
  table.}
  @begin{short}
    Look up a named tag.
  @end{short}
  @see-class{gtk:text-tag-table}"
  (table (g:object text-tag-table))
  (name (:string :free-to-foreign t)))

(export 'text-tag-table-lookup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_table_foreach" %text-tag-table-foreach) :void
  (table (g:object text-tag-table))
  (func :pointer)
  (data :pointer))

(defun text-tag-table-foreach (table func)
 #+liber-documentation
 "@version{#2021-11-18}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @argument[func]{a @symbol{gtk:text-tag-table-foreach-func} callback function
    to call on each tag}
  @begin{short}
    Calls @arg{func} on each tag in the tag table.
  @end{short}
  Note that the tag table may not be modified while iterating over it, you
  cannot add/remove tags.
  @see-class{gtk:text-tag-table}
  @see-symbol{gtk:text-tag-table-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%text-tag-table-foreach table
                             (cffi:callback text-tag-table-foreach-func)
                             ptr)))

(export 'text-tag-table-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_table_get_size -> text-table-size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_table_get_size" text-tag-table-size) :int
 #+liber-documentation
 "@version{#2021-11-18}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @return{An integer with the number of tags in @arg{table}.}
  @begin{short}
    Returns the size of the number of tags in the tag table.
  @end{short}
  @see-class{gtk:text-tag-table}"
  (table (g:object text-tag-table)))

(export 'text-tag-table-size)

;;; --- End of file gtk4.text-tag-table.lisp -----------------------------------
