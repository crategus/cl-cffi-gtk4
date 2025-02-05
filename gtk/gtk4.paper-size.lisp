;;; ----------------------------------------------------------------------------
;;; gtk4.paper-size.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkPaperSize
;;;
;;;     Support for named paper sizes
;;;
;;; Types and Values
;;;
;;;     GtkUnit
;;;     GtkPaperSize
;;;
;;; Functions
;;;
;;;     gtk_paper_size_new
;;;     gtk_paper_size_new_from_ppd
;;;     gtk_paper_size_new_from_ipp
;;;     gtk_paper_size_new_custom
;;;
;;;     gtk_paper_size_copy
;;;     gtk_paper_size_free                                 not needed
;;;     gtk_paper_size_is_equal
;;;
;;;     gtk_paper_size_get_paper_sizes
;;;     gtk_paper_size_get_name
;;;     gtk_paper_size_get_display_name
;;;     gtk_paper_size_get_ppd_name
;;;     gtk_paper_size_get_width
;;;     gtk_paper_size_get_height
;;;
;;;     gtk_paper_size_is_ipp
;;;     gtk_paper_size_is_custom
;;;
;;;     gtk_paper_size_set_size
;;;     gtk_paper_size_get_default_top_margin
;;;     gtk_paper_size_get_default_bottom_margin
;;;     gtk_paper_size_get_default_left_margin
;;;     gtk_paper_size_get_default_right_margin
;;;     gtk_paper_size_get_default
;;;
;;;     gtk_paper_size_new_from_key_file
;;;     gtk_paper_size_new_from_gvariant
;;;     gtk_paper_size_to_key_file
;;;     gtk_paper_size_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkPaperSize
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkUnit
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkUnit" unit
  (:export t
   :type-initializer "gtk_unit_get_type")
  (:none 0)
  (:pixel 0) ; alias for :none
  (:points 1)
  (:inch 2)
  (:mm 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'unit)
      "GEnum"
      (liber:symbol-documentation 'unit)
 "@version{2025-1-6}
  @begin{declaration}
(gobject:define-genum \"GtkUnit\" unit
  (:export t
   :type-initializer \"gtk_unit_get_type\")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No units.}
      @entry[:points]{Dimensions in points.}
      @entry[:inch]{Dimensions in inches.}
      @entry[:mm]{Dimensions in millimeters.}
    @end{table}
  @end{values}
  @short{Enumeration for dimensions of paper sizes.}
  @see-class{gtk:paper-size}")

;;; ----------------------------------------------------------------------------
;;; GtkPaperSize
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque paper-size "GtkPaperSize"
  :type-initializer "gtk_paper_size_get_type"
  :alloc (cffi:foreign-funcall "gtk_paper_size_new"
                               :pointer (cffi:null-pointer)
                               :pointer))

#+liber-documentation
(setf (liber:alias-for-class 'paper-size)
      "GBoxed"
      (documentation 'paper-size 'type)
 "@version{2025-1-6}
  @begin{declaration}
(glib:define-gboxed-opaque paper-size \"GtkPaperSize\"
  :type-initializer \"gtk_paper_size_get_type\"
  :alloc (cffi:foreign-funcall \"gtk_paper_size_new\"
                               :pointer (cffi:null-pointer)
                               :pointer))
  @end{declaration}
  @begin{short}
    The @class{gtk:paper-size} structure handles paper sizes.
  @end{short}
  It uses the standard called \"PWG 5101.1-2002 PWG: Standard for Media
  Standardized Names\" to name the paper sizes and to get the data for the page
  sizes. In addition to standard paper sizes, the @class{gtk:paper-size}
  structure allows to construct custom paper sizes with arbitrary dimensions.

  The @class{gtk:paper-size} instance stores not only the dimensions (width and
  height) of a paper size and its name, it also provides default print margins.
  @see-constructor{gtk:paper-size-new}
  @see-constructor{gtk:paper-size-new-custom}
  @see-constructor{gtk:paper-size-new-from-gvariant}
  @see-constructor{gtk:paper-size-new-from-ipp}
  @see-constructor{gtk:paper-size-new-from-key-file}
  @see-constructor{gtk:paper-size-new-from-ppd}
  @see-class{gtk:page-setup}")

(export 'paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_new" %paper-size-new)
    (g:boxed paper-size :return)
  (name :string))

(defun paper-size-new (&optional name)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[name]{a string for the paper size name, or @code{nil}}
  @return{The new @class{gtk:paper-size} instance.}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by parsing a PWG 5101.1-2002
    paper name.
  @end{short}
  If the @arg{name} argument is @code{nil}, the default paper size is returned,
  see the @fun{gtk:paper-size-default} function.
  @see-class{gtk:paper-size}
  @see-function{gtk:paper-size-default}"
  (let ((name (or name (cffi:null-pointer))))
    (%paper-size-new name)))

(export 'paper-size-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ppd
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_new_from_ppd" %paper-size-new-from-ppd)
    (g:boxed paper-size :return)
  (name :string)
  (displayname :string)
  (width :double)
  (height :double))

(defun paper-size-new-from-ppd (name &optional (displayname "")
                                               (width 0.0d0)
                                               (height 0.0d0))
 #+liber-documentation
 "@version{2025-1-6}
  @argument[name]{a string for the PPD paper name}
  @argument[displayname]{a string for the corresponding human readable name}
  @argument[width]{a number coerced to a double float for the paper width,
    in points}
  @argument[height]{a number coerced to a double float for the paper height,
    in points}
  @begin{return}
    The new @class{gtk:paper-size} instance.
  @end{return}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by using PPD information.
  @end{short}
  If the @arg{name} argument is not a recognized PPD paper name, the
  @arg{displayname}, @arg{width} and @arg{height} arguments are used to
  construct a custom @class{gtk:paper-size} instance.
  @see-class{gtk:paper-size}"
  (%paper-size-new-from-ppd name
                            displayname
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'paper-size-new-from-ppd)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ipp
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_new_from_ipp" %paper-size-new-from-ipp)
    (g:boxed paper-size :return)
  (name :string)
  (width :double)
  (height :double))

(defun paper-size-new-from-ipp (name &optional (width 0.0d0) (height 0.0d0))
 #+liber-documentation
 "@version{2025-1-6}
  @argument[name]{a string for the IPP paper name}
  @argument[width]{a number coerced to a double float for the paper width,
    in points}
  @argument[height]{a number coerced to a double float for the paper height,
    in points}
  @begin{return}
    The new @class{gtk:paper-size} instance.
  @end{return}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by using PPD information.
  @end{short}
  If the @arg{name} argument is not a recognized IPP paper name, the @arg{width}
  and @arg{height} arguments are used to construct a custom
  @class{gtk:paper-size} instance.
  @see-class{gtk:paper-size}"
  (%paper-size-new-from-ipp name
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'paper-size-new-from-ipp)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_custom
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_new_custom" %paper-size-new-custom)
    (g:boxed paper-size :return)
  (name :string)
  (displayname :string)
  (width :double)
  (height :double)
  (unit unit))

(defun paper-size-new-custom (name displayname width height unit)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[name]{a string for the paper name}
  @argument[displayname]{a string for the human readable name}
  @argument[width]{a number coerced to a double float for the paper width,
    in units of @arg{unit}}
  @argument[height]{a number coerced to a double float for the paper height,
    in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk:unit} value for @arg{width} and @arg{height},
    not @code{:none}}
  @return{The new @class{gtk:paper-size} instance.}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance with the given parameters.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}"
  (%paper-size-new-custom name
                          displayname
                          (coerce width 'double-float)
                          (coerce height 'double-float)
                          unit))

(export 'paper-size-new-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_copy" paper-size-copy)
    (g:boxed paper-size :return)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The @class{gtk:paper-size} instance with the copy of @arg{size}.}
  @begin{short}
    Copies an existing @class{gtk:paper-size} instance.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_free                                     not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_is_equal" paper-size-is-equal) :boolean
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size1]{a @class{gtk:paper-size} instance}
  @argument[size2]{another @class{gtk:paper-size} instance}
  @begin{return}
    @em{True}, if @arg{size1} and @arg{size2} represent the same paper size.
  @end{return}
  @begin{short}
    Compares two @class{gtk:paper-size} instances.
  @end{short}
  @see-class{gtk:paper-size}"
  (size1 (g:boxed paper-size))
  (size2 (g:boxed paper-size)))

(export 'paper-size-is-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_paper_sizes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_paper_sizes" paper-size-paper-sizes)
    (g:list-t (g:boxed paper-size :return))
 #+liber-documentation
 "@version{2025-1-6}
  @argument[custom]{a boolean whether to include custom paper sizes as defined
    in the page setup dialog}
  @begin{return}
    The list of  @class{gtk:paper-size} instances.
  @end{return}
  @begin{short}
    Creates a list of known paper sizes.
  @end{short}
  @see-class{gtk:paper-size}"
  (custom :boolean))

(export 'paper-size-paper-sizes)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_name" paper-size-name) :string
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The string with the name of the paper size.}
  @begin{short}
    Gets the name of the paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_display_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_display_name" paper-size-display-name)
    :string
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The string with the human readable name of the paper size.}
  @begin{short}
    Gets the human readable name of the paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-display-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_ppd_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_ppd_name" paper-size-ppd-name) :string
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The string with the PPD name of the paper size.}
  @begin{short}
    Gets the PPD name of the paper size, which may be @code{nil}.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-ppd-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_width" paper-size-width) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value,
    not @code{:none}}
  @return{The double float with the paper width.}
  @begin{short}
    Gets the paper width of the paper size, in units of @arg{unit}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-height}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-width)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_height" paper-size-height) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value,
    not @code{:none}}
  @return{The double float with the paper height.}
  @begin{short}
    Gets the paper height of the paper size, in units of @arg{unit}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-width}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-height)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_ipp
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_is_ipp" paper-size-is-ipp) :boolean
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The boolean whether the paper size is an IPP paper size.}
  @begin{short}
    Returns @em{true} if the paper size is an IPP standard paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-is-ipp)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_custom
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_is_custom" paper-size-is-custom) :boolean
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The boolean whether @arg{size} is a custom paper size.}
  @short{Returns @em{true} if @arg{size} is not a standard paper size.}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-is-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_set_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_set_size" %paper-size-set-size) :void
  (size (g:boxed paper-size))
  (width :double)
  (height :double)
  (unit unit))

(defun paper-size-set-size (size width height unit)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a custom @class{gtk:paper-size} instance}
  @argument[width]{a number coerced to a double float for the new width
    in units of @arg{unit}}
  @argument[height]{a number coerced to a double float for the new height
    in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk:unit} value for @arg{width} and @arg{height}}
  @begin{short}
    Changes the dimensions of a paper size to @arg{width} @code{x} @arg{height}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}"
  (%paper-size-set-size size
                        (coerce width 'double-float)
                        (coerce height 'double-float)
                        unit))

(export 'paper-size-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_top_margin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_default_top_margin"
               paper-size-default-top-margin) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{The double float with the default top margin.}
  @begin{short}
    Gets the default top margin for the paper size.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-default-bottom-margin}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-default-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_bottom_margin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_default_bottom_margin"
               paper-size-default-bottom-margin) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{The double float with the default bottom margin.}
  @begin{short}
    Gets the default bottom margin for the paper size.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-default-top-margin}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-default-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_left_margin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_default_left_margin"
               paper-size-default-left-margin) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{The double float with the default left margin.}
  @begin{short}
    Gets the default left margin for the paper size.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-default-right-margin}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-default-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_right_margin
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_default_right_margin"
               paper-size-default-right-margin) :double
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{The double float with the default right margin.}
  @begin{short}
    Gets the default right margin for the paper size.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}
  @see-function{gtk:paper-size-default-left-margin}"
  (size (g:boxed paper-size))
  (unit unit))

(export 'paper-size-default-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_get_default" paper-size-default) :string
 #+liber-documentation
 "@version{2025-1-6}
  @return{The string with the name of the default paper size.}
  @begin{short}
    Returns the name of the default paper size, which depends on the current
    locale.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:paper-size-default) => \"iso_a4\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:paper-size}")

(export 'paper-size-default)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_key_file
;;; ----------------------------------------------------------------------------

;; TODO: Consider to change the error handling: use glib:with-ignore-error?

(cffi:defcfun ("gtk_paper_size_new_from_key_file" %paper-size-new-from-key-file)
    (g:boxed paper-size :return)
  (keyfile (:pointer (:struct g:key-file)))
  (group :string)
  (err :pointer))

(defun paper-size-new-from-key-file (keyfile group)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[keyfile]{a @type{g:key-file} instance to retrieve the paper size
    from}
  @argument[group]{a string for the name of the group in the key file to read,
    or @code{nil} to read the first group}
  @begin{return}
    The new @class{gtk:paper-size} instance with the restored paper size, or
    @code{nil} if an error occurred.
  @end{return}
  @begin{short}
    Reads a paper size from the group @arg{group} in the key file @arg{keyfile}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-type{g:key-file}"
  (glib:with-error (err)
    (let ((group (or group (cffi:null-pointer))))
      (%paper-size-new-from-key-file keyfile group err))))

(export 'paper-size-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_gvariant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_new_from_gvariant" paper-size-new-from-gvariant)
    (g:boxed paper-size :return)
 #+liber-documentation
 "@version{2025-1-6}
  @argument[value]{a @symbol{g:variant} parameter of @code{\"a{sv@}\"} type}
  @return{The @class{gtk:paper-size} instance.}
  @begin{short}
    Deserialize a paper size from a @symbol{g:variant} parameter of
    @code{\"a{sv@}\"} type in the format produced by the
    @fun{gtk:paper-size-to-gvariant} function.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{g:variant}
  @see-function{gtk:paper-size-to-gvariant}"
  (value (:pointer (:struct g:variant))))

(export 'paper-size-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_key_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_to_key_file" paper-size-to-key-file) :void
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[keyfile]{a @type{g:key-file} instance to save the paper size to}
  @argument[groupname]{a string for the group name to add the settings to in
    @arg{keyfile}}
  @begin{short}
    This function adds the paper size from @arg{size} to @arg{keyfile}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-type{g:key-file}"
  (size (g:boxed paper-size))
  (keyfile (:pointer (:struct g:key-file)))
  (groupname :string))

(export 'paper-size-to-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_gvariant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_paper_size_to_gvariant" paper-size-to-gvariant)
    (:pointer (:struct g:variant))
 #+liber-documentation
 "@version{2025-1-6}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{The new @symbol{g:variant} parameter of @code{\"a{av@}\"} type.}
  @begin{short}
    Serialize a paper size to a @symbol{g:variant} parameter of
    @code{\"a{sv@}\"} type.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:paper-size-to-gvariant (gtk:paper-size-new))
=> #.(SB-SYS:INT-SAP #X00F02070)
(g:variant-print * nil)
=> \"{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>@}\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:paper-size}
  @see-symbol{g:variant}"
  (size (g:boxed paper-size)))

(export 'paper-size-to-gvariant)

;;; --- End of file gtk4.paper-size.lisp ---------------------------------------
