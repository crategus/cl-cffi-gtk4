;;; ----------------------------------------------------------------------------
;;; gtk4.file-filter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkFileFilter
;;;
;;;     Filtering files
;;;
;;; Types and Values
;;;
;;;     GtkFileFilter
;;;
;;; Accessors
;;;
;;;     gtk_file_filter_set_name
;;;     gtk_file_filter_get_name
;;;
;;; Functions
;;;
;;;     gtk_file_filter_new
;;;     gtk_file_filter_new_from_gvariant
;;;     gtk_file_filter_add_mime_type
;;;     gtk_file_filter_add_pattern
;;;     gtk_file_filter_add_pixbuf_formats
;;;     gtk_file_filter_add_suffix                         Since 4.4
;;;     gtk_file_filter_get_attributes
;;;     gtk_file_filter_to_gvariant
;;;
;;; Properties
;;;
;;;     mime-types                                         Since 4.10
;;;     name
;;;     patterns                                           Since 4.10
;;;     suffixes                                           Since 4.10
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkFilter
;;;         ╰── GtkFileFilter
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFileFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFileFilter" file-filter
  (:superclass filter
   :export t
   :interfaces nil
   :type-initializer "gtk_file_filter_get_type")
  (#+gtk-4-10
   (mime-types
    file-filter-mime-types
    "mime-types" "gchararray" nil nil)
   (name
    file-filter-name
    "name" "gchararray" t t)
   #+gtk-4-10
   (patterns
    file-filter-patterns
    "patterns" "gchararray" nil nil)
   #+gtk-4-10
   (suffixes
    file-filter-suffixes
    "suffixes" "gchararray" nil nil)))

#+liber-documentation
(setf (documentation 'file-filter 'type)
 "@version{#2023-5-5}
  @begin{short}
    A @sym{gtk:file-filter} object can be used to restrict the files being shown
    in a @class{gtk:file-chooser} widget.
  @end{short}
  Files can be filtered based on their name with the
  @fun{gtk:file-filter-add-pattern} function or on their mime type with the
  @fun{gtk:file-filter-add-mime-type} function.

  Filtering by mime types handles aliasing and subclassing of mime types. E.g.
  a filter for @file{text/plain} also matches a file with mime type
  @file{application/rtf}, since @file{application/rtf} is a subclass of
  @file{text/plain}. Note that the @sym{gtk:file-filter} object allows wildcards
  for the subtype of a mime type, so you can e.g. filter for @file{image/*}.

  Normally, file filters are used by adding them to a @class{gtk:file-chooser}
  widget, see the @fun{gtk:file-chooser-add-filter} function, but it is also
  possible to manually use a file filter on any @class{gtk:filter-list-model}
  object containing @class{g:file-info} objects.
  @begin[GtkFileFilter as GtkBuildable]{dictionary}
    The @sym{gtk:file-filter} implementation of the @class{gtk:buildable}
    interface supports adding rules using the @code{<mime-types>} and
    @code{<patterns>} elements and listing the rules within. Specifying a
    @code{<mime-type>} or @code{<pattern>} has the same effect as calling
    the @fun{gtk:file-filter-add-mime-type} or @fun{gtk:file-filter-add-pattern}
    functions.
  @end{dictionary}
  @begin[Example]{dictionary}
    An example of a UI definition fragment specifying @sym{gtk:file-filter}
    rules:
    @begin{pre}
<object class=\"GtkFileFilter\">
  <property name=\"name\" translatable=\"yes\">Text and Images</property>
  <mime-types>
    <mime-type>text/plain</mime-type>
    <mime-type>image/ *</mime-type>
  </mime-types>
  <patterns>
    <pattern>*.txt</pattern>
  </patterns>
  <suffixes>
    <suffix>png</suffix>
  </suffixes>
</object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:file-filter-new}
  @see-constructor{gtk:file-filter-new-from-gvariant}
  @see-slot{gtk:file-filter-name}
  @see-class{gtk:file-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- file-filter-mime-types -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mime-types" 'file-filter) t)
 "The @code{mime-types} property of type @code{gchararray}
  (Write / Construct only) @br{}
  The MIME types that this filter matches. Since 4.10")

(unexport 'file-filter-mime-types)

;;; --- file-filter-name -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'file-filter) t)
 "The @code{name} property of type @code{:string} (Read / Write) @br{}
  The human-readable name of the filter. This is the string that will be
  displayed in the file selector user interface if there is a selectable list
  of filters. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'file-filter-name)
      "Accessor"
      (documentation 'file-filter-name 'function)
 "@version{#2023-5-5}
  @syntax[]{(gtk:file-filter-name object) => name}
  @syntax[]{(setf (gtk:file-filter-name object) name)}
  @argument[object]{a @class{gtk:file-filter} object}
  @argument[name]{a string with the human-readable-name for the filter, or
    @code{nil} to remove any existing name}
  @begin{short}
    Accessor of the @slot[gtk:file-filter]{name} slot of the
    @class{gtk:file-filter} class.
  @end{short}
  The @sym{gtk:file-filter-name} function gets the human-readable name for the
  filter. The @sym{(setf gtk:file-filter-name)} function sets a human-readable
  name of the filter. This is the string that will be displayed in the file
  chooser if there is a selectable list of filters.
  @see-class{gtk:file-filter}")

;;; --- file-filter-patterns ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "patterns" 'file-filter) t)
 "The @code{patterns} property of type @code{gchararray}
  (Write / Construct only) @br{}
  The patterns that this filter matches. Since 4.10")

(unexport 'file-filter-patterns)

;;; --- file-filter-suffixes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "suffixes" 'file-filter) t)
 "The @code{suffixes} property of type @code{gchararray}
  (Write / Construct only) @br{}
  The suffixes that this filter matches. Since 4.10")

(unexport 'file-filter-suffixes)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline file-filter-new))

(defun file-filter-new ()
 #+liber-documentation
 "@version{#2023-5-5}
  @return{A new @class{gtk:file-filter} object.}
  @begin{short}
    Creates a new @class{gtk:file-filter} instance with no rules added to it.
  @end{short}
  Such a filter does not accept any files, so is not particularly useful until
  you add rules with the @fun{gtk:file-filter-add-mime-type}, the
  @fun{gtk:file-filter-add-pattern}, or @fun{gtk:file-filter-add-pixbuf-formats}
  functions.

  To create a filter that accepts any file, use:
  @begin{pre}
(let ((filter (gtk:file-filter-new)))
  (gtk:file-filter-add-pattern filter \"*\")
  ... )
  @end{pre}
  @see-class{gtk:file-filter}
  @see-function{gtk:file-filter-add-mime-type}
  @see-function{gtk:file-filter-add-pattern}
  @see-function{gtk:file-filter-add-pixbuf-formats}"
  (make-instance 'file-filter))

(export 'file-filter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_new_from_gvariant"
               file-filter-new-from-gvariant) (g:object file-filter)
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[variant]{a @type{g:variant} instance of @code{a{sv@}} type}
  @return{A new @class{gtk:file-filter} object.}
  @begin{short}
    Deserialize a file filter from an @code{a{sv@}} variant in the format
    produced by the @fun{gtk:file-filter-to-gvariant} function.
  @end{short}
  @see-class{gtk:file-filter}
  @see-type{g:variant}
  @see-function{gtk:file-filter-to-gvariant}"
  (variant (:pointer (:struct g:variant))))

(export 'file-filter-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_mime_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_mime_type" file-filter-add-mime-type) :void
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[mime-type]{a string with the name of MIME type}
  @short{Adds a rule allowing a given mime type to @arg{filter}.}
  @see-class{gtk:file-filter}"
  (filter (g:object file-filter))
  (mime-type :string))

(export 'file-filter-add-mime-type)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pattern ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_pattern" file-filter-add-pattern) :void
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[pattern]{a string with a shell style glob}
  @short{Adds a rule allowing a shell style glob to a filter.}
  @see-class{gtk:file-filter}"
  (filter (g:object file-filter))
  (pattern :string))

(export 'file-filter-add-pattern)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_pixbuf_formats ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_add_pixbuf_formats"
               file-filter-add-pixbuf-formats) :void
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @begin{short}
    Adds a rule allowing image files in the formats supported by the
    @class{gdk-pixbuf:pixbuf} object.
  @end{short}
  This is equivalent to calling the @fun{gtk:file-filter-add-mime-type}
  function for all the supported mime types.
  @see-class{gtk:file-filter}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:file-filter-add-mime-type}"
  (filter (g:object file-filter)))

(export 'file-filter-add-pixbuf-formats)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_add_suffix
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gtk_file_filter_add_suffix" file-filter-add-suffix) :void
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @argument[suffix]{a string with the filename suffix to match}
  @begin{short}
    Adds a suffix match rule to a filter.
  @end{short}
  This is similar to adding a match for the @code{*.suffix} pattern. In
  contrast to pattern matches, suffix matches are always case-insensitive.

  Since 4.4
  @see-class{gtk:file-filter}"
  (filter (g:object file-filter))
  (suffix (:string :free-to-foreign nil)))

#+gtk-4-4
(export 'file-filter-add-suffix)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_get_attributes ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_get_attributes" file-filter-attributes)
    (g:strv-t :free-from-foreign nil)
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @return{A list of strings with the attributes.}
  @begin{short}
    Gets the attributes that need to be filled in for the @class{g:file-info}
    object passed to this filter.
  @end{short}
  This function will not typically be used by applications. It is intended
  principally for use in the implementation of the @class{gtk:file-chooser}
  widget.
  @see-class{gtk:file-filter}
  @see-class{g:file-info}
  @see-class{gtk:file-chooser}"
  (filter (g:object file-filter)))

(export 'file-filter-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_file_filter_to_gvariant ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_file_filter_to_gvariant" file-filter-to-gvariant)
    (:pointer (:struct g:variant))
 #+liber-documentation
 "@version{#2023-5-5}
  @argument[filter]{a @class{gtk:file-filter} object}
  @return{A new @type{g:variant} instance.}
  @short{Serialize a file filter to an @code{a{sv@}} variant.}
  @see-class{gtk:file-filter}
  @see-type{g:variant}"
  (filter (g:object file-filter)))

(export 'file-filter-to-gvariant)

;;; --- End of file gtk4.file-filter.lisp --------------------------------------
