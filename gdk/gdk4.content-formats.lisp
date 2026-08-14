;;; ----------------------------------------------------------------------------
;;; gdk4.content-formats.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.22 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
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
;;; Content Formats
;;;
;;;     Advertising and negotiating of content exchange formats
;;;
;;; Types and Values
;;;
;;;     GdkContentFormats
;;;     GdkContentFormatsBuilder
;;;
;;;     GdkFileList                                         Since 4.6
;;;
;;; Functions
;;;
;;;     gdk_file_list_new_from_list                         Since 4.8
;;;     gdk_file_list_new_from_array                        Since 4.8
;;;     gdk_file_list_get_files                             Since 4.6
;;;
;;;     gdk_intern_mime_type
;;;
;;;     gdk_content_formats_new
;;;     gdk_content_formats_new_for_gtype
;;;     gdk_content_formats_ref                             not needed
;;;     gdk_content_formats_unref                           not needed
;;;     gdk_content_formats_print                           not needed
;;;     gdk_content_formats_to_string
;;;     gdk_content_formats_is_empty                        Since 4.18
;;;     gdk_content_formats_get_gtypes
;;;     gdk_content_formats_get_mime_types
;;;     gdk_content_formats_union
;;;     gdk_content_formats_match
;;;     gdk_content_formats_match_gtype
;;;     gdk_content_formats_match_mime_type
;;;     gdk_content_formats_contain_gtype
;;;     gdk_content_formats_contain_mime_type
;;;     gdk_content_formats_union_serialize_gtypes
;;;     gdk_content_formats_union_deserialize_gtypes
;;;     gdk_content_formats_union_serialize_mime_types
;;;     gdk_content_formats_union_deserialize_mime_types
;;;     gdk_content_formats_parse                           Since 4.4
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkContentFormats
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; Struct GdkFileList
;;;
;;; struct GdkFileList {
;;;   /* No available fields */
;;; }
;;;
;;; An opaque type representing a list of files.
;;;
;;; Since 4.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_file_list_new_from_array
;;;
;;; Creates a new GdkFileList for the given array of files.
;;;
;;; Since 4.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_file_list_new_from_list
;;;
;;; Creates a new files list container from a singly linked list of GFile
;;; instances.
;;;
;;; Since 4.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_file_list_get_files
;;;
;;; Retrieves the list of files inside a GdkFileList.
;;;
;;; Since 4.6
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GdkContentFormats
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque content-formats "GdkContentFormats"
  :export t
  :type-initializer "gdk_content_formats_get_type"
  :alloc
  (error "GdkContentFormats must be created with gdk:content-formats-new"))

#+liber-documentation
(setf (liber:alias-for-class 'content-formats)
      "GBoxed"
      (documentation 'content-formats 'type)
 "@version{2026-04-02}
  @begin{declaration}
(glib:define-gboxed-opaque content-formats \"GdkContentFormats\"
  :export t
  :type-initializer \"gdk_content_formats_get_type\"
  :alloc
  (error \"GdkContentFormats must be created with gdk:content-formats-new\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:content-formats} structure is used to advertise and negotiate
    the format of content.
  @end{short}
  You will encounter @class{gdk:content-formats} instances when interacting
  with objects controlling operations that pass data between different widgets,
  window or application, like @class{gdk:drag}, @class{gdk:drop},
  @class{gdk:clipboard} or @class{gdk:content-provider} objects.

  GDK supports content in two forms: GType and MIME type. Using GTypes is meant
  only for in-process content transfers. MIME types are meant to be used for
  data passing both in-process and out-of-process. The details of how data is
  passed is described in the documentation of the actual implementations. To
  transform between the two forms, the @class{gdk:content-serializer} and
  @class{gdk:content-deserializer} objects are used.

  The @class{gdk:content-formats} instance describes a set of possible formats
  content can be exchanged in. It is assumed that this set is ordered. GTypes
  are more important than MIME types. Order between different GTypes or MIME
  types is the order they were added in, most important first. Functions that
  care about order, such as the @fun{gdk:content-formats-union} function, will
  describe in their documentation how they interpret that order, though in
  general the order of the first argument is considered the primary order of
  the result, followed by the order of further arguments.

  For debugging purposes, the @fun{gdk:content-formats-to-string} function
  exists. It will print a comma-separated list of formats from most important
  to least important.

  The @class{gdk:content-formats} structure is an immutable structure. After
  creation, you cannot change the types it represents. Instead, new
  @class{gdk:content-formats} instances have to be created. The
  @class{gdk:content-formats-builder} structure is meant to help in this
  endeavor.
  @see-constructor{gdk:content-formats-new}
  @see-constructor{gdk:content-formats-new-for-gtype}
  @see-class{gdk:drag}
  @see-class{gdk:drop}
  @see-class{gdk:clipboard}
  @see-class{gdk:content-provider}")

;;; ----------------------------------------------------------------------------
;;; gdk_intern_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_intern_mime_type" intern-mime-type)
    (:string :free-from-foreign nil)
 #+liber-documentation
 "@version{2026-01-20}
  @argument[str]{a string for the potential MIME type}
  @begin{return}
    The string for the canonicalized MIME type or @code{nil} if the string
    is not a valid MIME type.
  @end{return}
  @begin{short}
    Canonicalizes the given MIME type and interns the result.
  @end{short}
  If @arg{str} is not a valid MIME type, @code{nil} is returned instead. See
  RFC 2048 for the syntax of MIME types."
  (str :string))

(export 'intern-mime-type)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_new" %content-formats-new)
    (g:boxed content-formats :return)
  (mtypes :pointer)
  (n-mtypes :uint))

(defun content-formats-new (&rest mtypes)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[mtypes]{strings for the MIME types}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Creates a new @class{gdk:content-formats} instance from the given
    MIME types.
  @end{short}
  The MIME types must be valid and different from each other or the behavior
  of the return value is undefined.
  @see-class{gdk:content-formats}"
  (let ((n (length mtypes)))
    (glib-sys:with-foreign-string-array (strptr mtypes)
      (%content-formats-new strptr n))))

(export 'content-formats-new)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_new_for_gtype
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_new_for_gtype"
               content-formats-new-for-gtype) (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-01-20}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Creates a new @class{gdk:content-formats} instance for a given @arg{gtype}.
  @end{short}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (gtype g:type-t))

(export 'content-formats-new-for-gtype)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_parse                              Since 4.4
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gdk_content_formats_parse" content-formats-parse)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[str]{a string to parse}
  @begin{return}
    The @class{gdk:content-formats} instance for the content formats if
    @arg{str} is valid.
  @end{return}
  @begin{short}
    Parses the given @arg{str} into a @class{gdk:content-formats} instance and
    returns the formats.
  @end{short}
  Strings printed via the @fun{gdk:content-formats-to-string} function can be
  read in again successfully using this function. If @arg{str} does not describe
  valid content formats, @code{nil} is returned.

  Since 4.4
  @see-class{gdk:content-formats}
  @see-function{gdk:content-formats-to-string}"
  (str :string))

#+gtk-4-4
(export 'content-formats-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_ref                                 not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_unref                               not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_print                               not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_to_string" content-formats-to-string)
    :string
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The string representing the content formats.}
  @begin{short}
    Prints the given @arg{formats} into a human-readable string.
  @end{short}
  The string can be read in again successfully using the
  @fun{gdk:content-formats-parse} function.
  @see-class{gdk:content-formats}
  @see-function{gdk:content-formats-parse}"
  (formats (g:boxed content-formats)))

(export 'content-formats-to-string)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_is_empty                            Since 4.18
;;; ----------------------------------------------------------------------------

#+gtk-4-18
(cffi:defcfun ("gdk_content_formats_is_empty" content-formats-is-empty) :boolean
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @begin{return}
    @em{True} if @arg{formats} contains no MIME types and no GTypes.
  @end{return}
  @begin{short}
    Returns whether the content formats contain any formats.
  @end{short}

  Since 4.18
  @see-class{gdk:content-formats}"
  (formats (g:boxed content-formats)))

#+gtk-4-18
(export 'content-formats-is-empty)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_get_gtypes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_get_gtypes" %content-formats-gtypes)
    :pointer
  (formats (g:boxed content-formats))
  (n-gtypes (:pointer :size)))

(defun content-formats-gtypes (formats)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The list of @class{g:type-t} type IDs included in @arg{formats}.}
  @begin{short}
    Gets the @class{g:type-t} type IDs included in @arg{formats}.
  @end{short}
  Note that @arg{formats} may not contain any @class{g:type-t} type IDs, in
  particular it is empty. In this case @code{nil} will be returned.
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (cffi:with-foreign-object (n :size)
    (let ((ptr (%content-formats-gtypes formats n)))
      (iter (for i from 0 below (cffi:mem-ref n :size))
            (collect (cffi:mem-aref ptr 'g:type-t i))))))

(export 'content-formats-gtypes)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_get_mime_types
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_get_mime_types" %content-formats-mime-types)
    (g:strv-t :free-from-foreign nil)
  (formats (g:boxed content-formats))
  (n-mtypes :pointer))

(defun content-formats-mime-types (formats)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The list of strings with the MIME types included in @arg{formats}.}
  @begin{short}
    Gets the MIME types included in @arg{formats}.
  @end{short}
  Note that @arg{formats} may not contain any MIME types, in particular when
  it is empty. In this case @code{nil} will be returned.
  @see-class{gdk:content-formats}"
  (%content-formats-mime-types formats (cffi:null-pointer)))

(export 'content-formats-mime-types)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union
;;; ----------------------------------------------------------------------------

;; The C function takes ownership of the first argument and modifies it.
;; Therefore, we pass in a copy of the first argument.
;;
;; TODO: Improve the GLIB::BOXED-COPY function and consider to export it.

(cffi:defcfun ("gdk_content_formats_union" %content-formats-union)
    (g:boxed content-formats :return)
  (first (g:boxed content-formats))
  (second (g:boxed content-formats)))

(defun content-formats-union (first second)
 #+liber-documentation
 "@version{2026-08-14}
  @argument[first]{a @class{gdk:content-formats} instance to merge into}
  @argument[second]{a @class{gdk:content-formats} instance to merge from}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Append all missing types from @arg{second} to @arg{first}, in the order
    they had in @arg{second}.
  @end{short}
  @see-class{gdk:content-formats}"
  (let ((first (glib::boxed-copy "GdkContentFormats" (glib:pointer first))))
    (%content-formats-union first second)))

(export 'content-formats-union)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_match" content-formats-match) :boolean
 #+liber-documentation
 "@version{2026-01-20}
  @argument[first]{a @class{gdk:content-formats} instance to intersect}
  @argument[second]{a @class{gdk:content-formats} instance to intersect with}
  @return{@em{True} if a matching format was found.}
  @begin{short}
    Checks if @arg{first} and @arg{second} have any matching formats.
  @end{short}
  @see-class{gdk:content-formats}"
  (first (g:boxed content-formats))
  (second (g:boxed content-formats)))

(export 'content-formats-match)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match_gtype
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_match_gtype" content-formats-match-gtype)
    g:type-t
 #+liber-documentation
 "@version{2026-01-20}
  @argument[first]{a @class{gdk:content-formats} instance to intersect}
  @argument[second]{a @class{gdk:content-formats} instance to intersect with}
  @return{The first common @class{g:type-t} type ID or @code{nil} if none.}
  @begin{short}
    Finds the first @class{g:type-t} type ID from @arg{first} that is also
    contained in @arg{second}.
  @end{short}
  If no matching @class{g:type-t} type ID is found, @code{nil} is returned.
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (first (g:boxed content-formats))
  (second (g:boxed content-formats)))

(export 'content-formats-match-gtype)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_match_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_match_mime_type"
               content-formats-match-mime-type) :string
 #+liber-documentation
 "@version{2026-08-14}
  @argument[first]{a @class{gdk:content-formats} instance to intersect}
  @argument[second]{a @class{gdk:content-formats} instance to intersect with}
  @begin{return}
    The string for the first common MIME type or @code{nil} if none.
  @end{return}
  @begin{short}
    Finds the first MIME type from @arg{first} that is also contained in
    @arg{second}.
  @end{short}
  If no matching @class{g:type-t} type ID is found, @code{nil} is returned.
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (first (g:boxed content-formats))
  (second (g:boxed content-formats)))

(export 'content-formats-match-mime-type)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_contain_gtype
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_contain_gtype"
               content-formats-contain-gtype) :boolean
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @argument[gtype]{a @class{g:type-t} type ID}
  @return{@em{True} if the given @arg{gtype} was found.}
  @short{Checks if a given @arg{gtype} is part of the given @arg{formats}.}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (formats (g:boxed content-formats))
  (gype g:type-t))

(export 'content-formats-contain-gtype)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_contain_mime_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_contain_mime_type"
               content-formats-contain-mime-type) :boolean
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @argument[mtype]{a string for the MIME type to search for}
  @return{@em{True} if the given @arg{mtype} was found.}
  @short{Checks if a given MIME type is part of the given @arg{formats}.}
  @see-class{gdk:content-formats}"
  (formats (g:boxed content-formats))
  (mtype :string))

(export 'content-formats-contain-mime-type)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_serialize_gtypes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_union_serialize_gtypes"
               content-formats-union-serialize-gtypes)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Add @class{g:type-t} type IDs for the MIME types in formats for which
    serializers are registered.
  @end{short}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (formats (g:boxed content-formats)))

(export 'content-formats-union-serialize-gtypes)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_deserialize_gtypes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_union_deserialize_gtypes"
               content-formats-union-deserialize-gtypes)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Add @class{g:type-t} type IDs for the MIME types in formats for which
    deserializers are registered.
  @end{short}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (formats (g:boxed content-formats)))

(export 'content-formats-union-deserialize-gtypes)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_serialize_mime_types
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_union_serialize_mime_types"
               content-formats-union-serialize-mime-types)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Add MIME types for @class{g:type-t} type IDs in formats for which
    serializers are registered.
  @end{short}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (formats (g:boxed content-formats)))

(export 'content-formats-union-serialize-mime-types)

;;; ----------------------------------------------------------------------------
;;; gdk_content_formats_union_deserialize_mime_types
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_content_formats_union_deserialize_mime_types"
               content-formats-union-deserialize-mime-types)
    (g:boxed content-formats :return)
 #+liber-documentation
 "@version{2026-04-02}
  @argument[formats]{a @class{gdk:content-formats} instance}
  @return{The new @class{gdk:content-formats} instance.}
  @begin{short}
    Add MIME types for @class{g:type-t} type IDs in formats for which
    deserializers are registered.
  @end{short}
  @see-class{gdk:content-formats}
  @see-class{g:type-t}"
  (formats (g:boxed content-formats)))

(export 'content-formats-union-deserialize-mime-types)

;;; --- End of file gdk4.content-formats.lisp ----------------------------------
