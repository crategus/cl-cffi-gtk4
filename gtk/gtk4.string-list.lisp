;;; ----------------------------------------------------------------------------
;;; gtk4.string-list.lisp
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
;;; GtkStringList
;;;
;;;     A list model for strings
;;;
;;; Types and Values
;;;
;;;     GtkStringList
;;;     GtkStringObject
;;;
;;; Functions
;;;
;;;     gtk_string_list_new
;;;     gtk_string_list_append
;;;     gtk_string_list_take                                not needed
;;;     gtk_string_list_remove
;;;     gtk_string_list_splice
;;;     gtk_string_list_get_string
;;;
;;;     gtk_string_object_new
;;;     gtk_string_object_get_string
;;;
;;; Properties (GtkStringList)
;;;
;;;     item-type                                           Since 4.14
;;;     n-items                                             Since 4.14
;;;     strings                                             Since 4.10
;;;
;;; Properties (GtkStringObject)
;;;
;;;     string
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ├── GtkStringList
;;;     ╰── GtkStringObject
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;;     GListModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkStringObject
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStringObject" string-object
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_string_object_get_type")
  ((string
    string-object-string
    "string" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'string-object 'type)
 "@version{2025-3-29}
  @begin{short}
    The @class{gtk:string-object} class is the type of items in a
    @class{gtk:string-list} object.
  @end{short}
  A @class{gtk:string-object} object is a wrapper around a string. It has a
  @slot[gtk:string-object]{string} property that can be used for property
  bindings and expressions.
  @see-constructor{gtk:string-object-new}
  @see-slot{gtk:string-object-string}
  @see-class{gtk:string-list}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:string-object-string -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "string" 'string-object) t)
 "The @code{string} property of type @code{:string} (Read) @br{}
  The string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'string-object-string)
      "Accessor"
      (documentation 'string-object-string 'function)
 "@version{2025-3-29}
  @syntax{(gtk:string-object-string object) => string}
  @argument[object]{a @class{gtk:string-object} object}
  @argument[string]{a string}
  @begin{short}
    Accessor of the @slot[gtk:string-object]{string} slot of the
    @class{gtk:string-object} class.
  @end{short}
  The @fun{gtk:string-object-string} function returns the string contained in a
  @class{gtk:string-object} object.
  @see-class{gtk:string-object}")

;;; ----------------------------------------------------------------------------
;;; gtk_string_object_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_string_object_new" %string-object-new)
    (g:object string-object :return)
  (string :string))

(defun string-object-new (&optional string)
 #+liber-documentation
 "@version{2025-3-29}
  @argument[string]{a string to wrap, or @code{nil}}
  @return{The new @class{gtk:string-object} object.}
  @begin{short}
    Wraps a string in an object for use with a @class{g:list-model} object.
  @end{short}
  @begin[Examples]{dictionary}
    Create string objects:
    @begin{pre}
(gtk:string-object-new \"abcdef\") => #<GTK:STRING-OBJECT {1003E79813@}>
(gtk:string-object-string *) => \"abcdef\"
(gtk:string-object-new nil) => #<GTK:STRING-OBJECT {1003E79813@}>
(gtk:string-object-string *) => nil
    @end{pre}
  @end{dictionary}
  @see-class{gtk:string-object}
  @see-class{g:list-model}"
  (%string-object-new (or string (cffi:null-pointer))))

(export 'string-object-new)

;;; ----------------------------------------------------------------------------
;;; GtkStringList
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkStringList" string-list
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_string_list_get_type")
  (#+gtk-4-14
   (item-type
    string-list-item-type
    "item-type" "GType" t nil)
   #+gtk-4-14
   (n-items
    string-list-n-items
    "n-items" "guint" t nil)
   #+gtk-4-10
   (strings
    %string-list-strings
    "strings" "GStrv" nil nil)))

#+liber-documentation
(setf (documentation 'string-list 'type)
 "@version{2025-3-29}
  @begin{short}
    The @class{gtk:string-list} class is a list model that wraps an array of
    strings.
  @end{short}
  The objects in the model have a @slot[gtk:string-object]{string} property. The
  @class{gtk:string-list} object is well-suited for any place where you would
  typically use a string, but need a list model.
  @begin[GtkStringList as GtkBuildable]{dictionary}
    The @class{gtk:string-list} implementation of the @class{gtk:buildable}
    interface supports adding items directly using the @code{<items>} element
    and specifying @code{<item>} elements for each item. Each @code{<item>}
    element supports the regular translation attributes @code{\"translatable\"},
    @code{\"context\"} and @code{\"comments\"}. Here is a UI definition fragment
    specifying a @class{gtk:string-list} object:
    @begin{pre}
<object class=\"GtkStringList\">
  <items>
    <item translatable=\"yes\">Factory</item>
    <item translatable=\"yes\">Home</item>
    <item translatable=\"yes\">Subway</item>
  </items>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Examples]{dictionary}
    Create a list of strings with the external symbols of the GTK library:
    @begin{pre}
(create-list-of-gtk-symbols ()
  (let ((model (gtk:string-list-new '())))
    (do-external-symbols (symbol (find-package \"GTK\"))
      (gtk:string-list-append model
                              (string-downcase (format nil \"~a\" symbol))))
    ...))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:string-list-new}
  @see-slot{gtk:string-list-item-type}
  @see-slot{gtk:string-list-n-items}
  @see-class{gtk:string-object}
  @see-class{g:list-model}")

(unexport 'string-list-strings)

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:string-list-item-type ----------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'string-list) t)
 "The @code{item-type} property of type @class{g:type-t} (read) @br{}
  The type of items contained in the string list. Since 4.14")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'string-list-item-type)
      "Accessor"
      (documentation 'string-list-item-type 'function)
 "@version{2025-3-29}
  @syntax{(gtk:string-list-item-type object) => type}
  @syntax{(setf (gtk:string-list-item-type object) type)}
  @argument[object]{a @class{gtk:string-list} object}
  @argument[type]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:string-list]{item-type} slot of the
    @class{gtk:string-list} class.
  @end{short}

  Since 4.14
  @see-class{gtk:string-list}
  @see-class{g:type-t}")

(export 'string-list-item-type)

;;; --- gtk:string-list-n-items ------------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'string-list) t)
 "The @code{n-items} property of type @code{:uint} (read) @br{}
  The number of items contained in the string list. Since 4.14 @br{}
  Default value: 0")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'string-list-n-items)
      "Accessor"
      (documentation 'string-list-n-items 'function)
 "@version{2025-3-29}
  @syntax{(gtk:string-list-n-items object) => n-items}
  @syntax{(setf (gtk:string-list-n-items object) n-items)}
  @argument[object]{a @class{gtk:string-list} object}
  @argument[n-items]{an unsigned integer}
  @begin{short}
    Accessor of the @slot[gtk:string-list]{n-items} slot of the
    @class{gtk:string-list} class.
  @end{short}
  @see-class{gtk:string-list}")

;;; --- gtk:string-list-strings ------------------------------------------------

;; Note: The property is not readable and not writable, no accessor is exported

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "strings" 'string-list) t)
 "The @code{strings} property of type @code{:string} (Construct only) @br{}
  An array of strings. Since 4.10 @br{}
  @em{Note:} This property is not readable and not writable. You cannot
  initialize it in a @code{make-instance} method. Therefore, no accessor is
  exported.")

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_string_list_new" string-list-new)
    (g:object string-list :return)
 #+liber-documentation
 "@version{2025-3-29}
  @argument[strings]{a list of strings to put in the model}
  @return{The new @class{gtk:string-list} object.}
  @begin{short}
    Creates a new string list with the given @arg{strings}.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:string-list-new '(\"Factory\" \"Home\" \"Subway\"))
=> #<GTK:STRING-LIST {1003E7BC63@}>
(gtk:string-list-string * 0) => \"Factory\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:string-list}"
  (strings g:strv-t))

(export 'string-list-new)

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_append
;;; ----------------------------------------------------------------------------

;; FIXME: Is this implementation correct?

(cffi:defcfun ("gtk_string_list_append" string-list-append) :void
 #+liber-documentation
 "@version{2025-3-29}
  @argument[model]{a @class{gtk:string-list} object}
  @argument[string]{a string to insert}
  @begin{short}
    Appends a string to the string list.
  @end{short}
  @see-class{gtk:string-list}"
  (model (g:object string-list))
  (string :string))

(export 'string-list-append)

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_take                                    not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_string_list_remove" string-list-remove) :void
 #+liber-documentation
 "@version{2025-3-29}
  @argument[model]{a @class{gtk:string-list} object}
  @argument[pos]{an unsigned integer for the position of the string that is
    to be removed}
  @begin{short}
    Removes the string at @arg{pos} from the string list.
  @end{short}
  The @arg{pos} argument must be smaller than the current length of the string
  list.
  @see-class{gtk:string-list}"
  (model (g:object string-list))
  (pos :uint))

(export 'string-list-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_splice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_string_list_splice" %string-list-splice) :void
  (model (gobject:object string-list))
  (pos :uint)
  (n :uint)
  (additions g:strv-t))

(defun string-list-splice (model pos n additions)
 #+liber-documentation
 "@version{2025-3-29}
  @argument[model]{a @class{gtk:string-list} object}
  @argument[pos]{an unsigned integer for the position at which to make the
    change}
  @argument[n]{an unsigned integer for the number of strings to remove}
  @argument[additions]{a list of strings to add}
  @begin{short}
    Changes the string list by removing @arg{n} strings and adding
    @arg{additions} to it.
  @end{short}
  This function is more efficient than the @fun{gtk:string-list-append}
  function and the @fun{gtk:string-list-remove} function, because it only emits
  the @code{\"items-changed\"} signal once for the change.

  The @arg{pos} and @arg{n} parameters must be correct, that is,
  @arg{pos} + @arg{n} must be less than or equal to the length of the string
  list at the time this function is called.
  @see-class{gtk:string-list}
  @see-function{gtk:string-list-append}
  @see-function{gtk:string-list-remove}"
  (%string-list-splice model pos n additions))

(export 'string-list-splice)

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_get_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_string_list_get_string" string-list-string) :string
 #+liber-documentation
 "@version{2025-05-05}
  @argument[model]{a @class{gtk:string-list} object}
  @argument[pos]{an unsigned integer for the position to get the string for}
  @return{The string at the given @arg{pos}.}
  @begin{short}
    Gets the string that is at @arg{pos} in the string list.
  @end{short}
  If the string list does not contain @arg{pos} items, @code{nil} is returned.
  This function returns the string. To get the object wrapping it, use the
  @fun{g:list-model-item} function.
  @see-class{gtk:string-list}
  @see-function{g:list-model-item}"
  (model (g:object string-list))
  (pos :uint))

(export 'string-list-string)

;;; ----------------------------------------------------------------------------
;;; gtk_string_list_find
;;; ----------------------------------------------------------------------------

#+gtk-4-18
(cffi:defcfun ("gtk_string_list_find" string-list-find) :uint
  (model (g:object string-list))
  (str :string))

#+gtk-4-18
(export 'string-list-find)

;;; --- End of file gtk4.string-list.lisp --------------------------------------
