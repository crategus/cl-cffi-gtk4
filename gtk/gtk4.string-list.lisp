;;; ----------------------------------------------------------------------------
;;; gtk4.string-list.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;;     gtk_string_list_take
;;;     gtk_string_list_remove
;;;     gtk_string_list_splice
;;;     gtk_string_list_get_string
;;;
;;;     gtk_string_object_new
;;;     gtk_string_object_get_string
;;;
;;; Properties (GtkStringList)
;;;
;;;     strings                                            Since 4.10
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

(gobject:define-g-object-class "GtkStringObject" string-object
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_string_object_get_type")
  ((string
    string-object-string
    "string" "gchararray" t nil)))

;;; ----------------------------------------------------------------------------
;;; GtkStringList
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkStringList" string-list
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_string_list_get_type")
  (#+gtk-4-10
   (strings
    string-list-strings
    "strings" "GStrv" nil t)))


;;;Property Details
;;;The “string” property
;;;  “string”                   char *
;;;String.

;;;Owner: GtkStringObject

;;;Flags: Read

;;;Default value: NULL

;;;See Also
;;;GListModel


;;;Description
;;;GtkStringList is a list model that wraps an array of strings.

;;;The objects in the model have a "string" property.

;;;GtkStringList is well-suited for any place where you would typically use a char*[], but need a list model.

;;;GtkStringList as GtkBuildable
;;;The GtkStringList implementation of the GtkBuildable interface supports adding items directly using the <items> element and specifying <item> elements for each item. Each <item> element supports the regular translation attributes “translatable”, “context” and “comments”.

;;;Here is a UI definition fragment specifying a GtkStringList

;;;Functions
;;;gtk_string_list_new ()
;;;GtkStringList *
;;;gtk_string_list_new (const char * const *strings);
;;;Creates a new GtkStringList with the given strings .

;;;Parameters
;;;strings

;;;The strings to put in the model.

;;;[array zero-terminated=1][nullable]
;;;Returns
;;;a new GtkStringList

;;;gtk_string_list_append ()
;;;void
;;;gtk_string_list_append (GtkStringList *self,
;;;                        const char *string);
;;;Appends string to self .

;;;The string will be copied. See gtk_string_list_take() for a way to avoid that.

;;;Parameters
;;;self

;;;a GtkStringList

;;;string

;;;the string to insert

;;;gtk_string_list_take ()
;;;void
;;;gtk_string_list_take (GtkStringList *self,
;;;                      char *string);
;;;Adds string to self at the end, and takes ownership of it.

;;;This variant of gtk_string_list_append() is convenient for formatting strings:

;;;<object class="GtkStringList">
;;;  <items>
;;;    <item translatable="yes">Factory</item>
;;;    <item translatable="yes">Home</item>
;;;    <item translatable="yes">Subway</item>
;;;  </items>
;;;</object>
;;;Parameters
;;;self

;;;a GtkStringList

;;;string

;;;the string to insert.

;;;[transfer full]
;;;gtk_string_list_remove ()
;;;void
;;;gtk_string_list_remove (GtkStringList *self,
;;;                        guint position);
;;;Removes the string at position from self . position must be smaller than the current length of the list.

;;;Parameters
;;;self

;;;a GtkStringList

;;;position

;;;the position of the string that is to be removed

;;;gtk_string_list_splice ()
;;;void
;;;gtk_string_list_splice (GtkStringList *self,
;;;                        guint position,
;;;                        guint n_removals,
;;;                        const char * const *additions);
;;;Changes self by removing n_removals strings and adding additions to it.

;;;This function is more efficient than gtk_string_list_append() and gtk_string_list_remove(), because it only emits “items-changed” once for the change.

;;;This function copies the strings in additions .

;;;The parameters position and n_removals must be correct (ie: position + n_removals must be less than or equal to the length of the list at the time this function is called).

;;;Parameters
;;;self

;;;a GtkStringList

;;;position

;;;the position at which to make the change

;;;n_removals

;;;the number of strings to remove

;;;additions

;;;The strings to add.

;;;[array zero-terminated=1][nullable]
;;;gtk_string_list_get_string ()
;;;const char *
;;;gtk_string_list_get_string (GtkStringList *self,
;;;                            guint position);
;;;Gets the string that is at position in self . If self does not contain position items, NULL is returned.

;;;This function returns the const char *. To get the object wrapping it, use g_list_model_get_item().

;;;Parameters
;;;self

;;;a GtkStringList

;;;position

;;;the position to get the string for

;;;Returns
;;;the string at the given position.

;;;[nullable]

;;;gtk_string_object_new ()
;;;GtkStringObject *
;;;gtk_string_object_new (const char *string);
;;;Wraps a string in an object for use with GListModel

;;;Parameters
;;;string

;;;The string to wrap.

;;;[not nullable]
;;;Returns
;;;a new GtkStringObject

;;;gtk_string_object_get_string ()
;;;const char *
;;;gtk_string_object_get_string (GtkStringObject *self);
;;;Returns the string contained in a GtkStringObject.

;;;Parameters
;;;self

;;;a GtkStringObject

;;;Returns
;;;the string of self


;;; --- End of file gtk4.string-list.lisp --------------------------------------
