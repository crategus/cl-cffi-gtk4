;;; ----------------------------------------------------------------------------
;;; gtk4.paper-size.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkPaperSize
;;;
;;;     Support for named paper sizes
;;;
;;; Types and Values
;;;
;;;     GtkUnit
;;;     GtkPaperSize
;;;
;;;     GTK_UNIT_PIXEL
;;;
;;;     GTK_PAPER_NAME_A3
;;;     GTK_PAPER_NAME_A4
;;;     GTK_PAPER_NAME_A5
;;;     GTK_PAPER_NAME_B5
;;;     GTK_PAPER_NAME_LETTER
;;;     GTK_PAPER_NAME_EXECUTIVE
;;;     GTK_PAPER_NAME_LEGAL
;;;
;;; Functions
;;;
;;;     gtk_paper_size_new
;;;     gtk_paper_size_new_from_ppd
;;;     gtk_paper_size_new_from_ipp
;;;     gtk_paper_size_new_custom
;;;
;;;     gtk_paper_size_copy
;;;     gtk_paper_size_free
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
;;; enum GtkUnit
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkUnit" unit
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
 "@version{2023-1-31}
  @short{Enumeration for dimenstions of paper sizes.}
  @begin{pre}
(define-g-enum \"GtkUnit\" unit
  (:export t
   :type-initializer \"gtk_unit_get_type\")
  (:none 0)
  (:points 1)
  (:inch 2)
  (:mm 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No units.}
    @entry[:points]{Dimensions in points.}
    @entry[:inch]{Dimensions in inches.}
    @entry[:mm]{Dimensions in millimeters.}
  @end{table}
  @see-class{gtk:paper-size}")

;;; ----------------------------------------------------------------------------
;;; GtkPaperSize
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque paper-size "GtkPaperSize"
  :type-initializer "gtk_paper_size_get_type"
  :alloc (%paper-size-new (cffi:null-pointer)))

#+liber-documentation
(setf (liber:alias-for-class 'paper-size)
      "Boxed CStruct"
      (documentation 'paper-size 'type)
 "@version{2023-1-31}
  @begin{short}
    The @sym{gtk:paper-size} instance handles paper sizes.
  @end{short}
  It uses the standard called \"PWG 5101.1-2002 PWG: Standard for Media
  Standardized Names\" to name the paper sizes and to get the data for the page
  sizes. In addition to standard paper sizes, the @sym{gtk:paper-size} structure
  allows to construct custom paper sizes with arbitrary dimensions.

  The @sym{gtk:paper-size} structure stores not only the dimensions (width and
  height) of a paper size and its name, it also provides default print margins.
  @see-class{gtk:page-setup}")

(export 'paper-size)

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A3
;;;
;;; #define GTK_PAPER_NAME_A3 "iso_a3"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A4
;;;
;;; #define GTK_PAPER_NAME_A4 "iso_a4"
;;;
;;; Name for the A4 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_A5
;;;
;;; #define GTK_PAPER_NAME_A5 "iso_a5"
;;;
;;; Name for the A5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_B5
;;;
;;; #define GTK_PAPER_NAME_B5 "iso_b5"
;;;
;;; Name for the B5 paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LETTER
;;;
;;; #define GTK_PAPER_NAME_LETTER "na_letter"
;;;
;;; Name for the Letter paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_EXECUTIVE
;;;
;;; #define GTK_PAPER_NAME_EXECUTIVE "na_executive"
;;;
;;; Name for the Executive paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_PAPER_NAME_LEGAL
;;;
;;; #define GTK_PAPER_NAME_LEGAL "na_legal"
;;;
;;; Name for the Legal paper size.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new" %paper-size-new) (g:boxed paper-size :return)
  (name :string))

(defun paper-size-new (&optional name)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[name]{a string with the paper size name, or @code{nil}}
  @return{A new @class{gtk:paper-size} instance.}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by parsing a PWG 5101.1-2002
    paper name.
  @end{short}
  If the @arg{name} argument is @code{nil}, the default paper size is returned,
  see the @fun{gtk:paper-size-default} function.
  @see-class{gtk:paper-size}
  @see-function{gtk:paper-size-default}"
  (%paper-size-new (if name name (cffi:null-pointer))))

(export 'paper-size-new)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ppd ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_from_ppd" %paper-size-new-from-ppd)
    (g:boxed paper-size :return)
  (name :string)
  (displayname :string)
  (width :double)
  (height :double))

(defun paper-size-new-from-ppd (name &optional (displayname "")
                                               (width 0.0d0)
                                               (height 0.0d0))
 #+liber-documentation
 "@version{2023-1-31}
  @argument[name]{a string with the PPD paper name}
  @argument[displayname]{a string with the corresponding human readable name}
  @argument[width]{a number coerced to a double float with the paper width, in
    points}
  @argument[height]{a number coerced to a double float with the paper height in
    points}
  @begin{return}
    A new @class{gtk:paper-size} instance.
  @end{return}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by using PPD information.
  @end{short}
  If the @arg{name} argument is not a recognized PPD paper name,
  @arg{displayname}, @arg{width} and @arg{height} are used to construct a
  custom @class{gtk:paper-size} instance.
  @see-class{gtk:paper-size}"
  (%paper-size-new-from-ppd name
                            displayname
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'paper-size-new-from-ppd)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_ipp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_from_ipp" %paper-size-new-from-ipp)
    (g:boxed paper-size :return)
  (name :string)
  (width :double)
  (height :double))

(defun paper-size-new-from-ipp (name &optional (width 0.0d0) (height 0.0d0))
 #+liber-documentation
 "@version{2023-1-31}
  @argument[name]{a string with the IPP paper name}
  @argument[width]{a number coerced to a double float with the paper width, in
    points}
  @argument[height]{a number coerced to a double float with the paper height in
    points}
  @begin{return}
    A new @class{gtk:paper-size} instance.
  @end{return}
  @begin{short}
    Creates a new @class{gtk:paper-size} instance by using PPD information.
  @end{short}
  If the @arg{name} argument is not a recognized IPP paper name, @arg{width} and
  @arg{height} are used to construct a custom @class{gtk:paper-size} instance.
  @see-class{gtk:paper-size}"
  (%paper-size-new-from-ipp name
                            (coerce width 'double-float)
                            (coerce height 'double-float)))

(export 'paper-size-new-from-ipp)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_custom" %paper-size-new-custom)
    (g:boxed paper-size :return)
  (name :string)
  (displayname :string)
  (width :double)
  (height :double)
  (unit unit))

(defun paper-size-new-custom (name displayname width height unit)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[name]{a string with the paper name}
  @argument[displayname]{a string with the human readable name}
  @argument[width]{a number coerced to a double float with the paper width, in
    units of @arg{unit}}
  @argument[height]{a number coerced to a double float with the paper height,
    in units of @arg{unit}}
  @argument[unit]{a @symbol{gtk:unit} value for @arg{width} and @arg{height},
    not @code{:none}}
  @return{A new @class{gtk:paper-size} instance.}
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
;;; gtk_paper_size_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_copy" paper-size-copy) (g:boxed paper-size :return)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A copy of @arg{size}.}
  @begin{short}
    Copies an existing @class{gtk:paper-size} instance.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_free ()                                 not exported
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_free" %paper-size-free) :void
 #+liber-documentation
 "@version{#2013-11-13}
  @argument[size]{a @class{gtk:paper-size} structure}
  @begin{short}
    Free the given @class{gtk:paper-size} structure.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_is_equal" paper-size-is-equal) :boolean
 #+liber-documentation
 "@version{2023-1-31}
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
;;; gtk_paper_size_get_paper_sizes () -> paper-size-paper-sizes
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_paper_sizes" paper-size-paper-sizes)
    (g:list-t (g:boxed paper-size :return))
 #+liber-documentation
 "@version{2023-1-31}
  @argument[custom]{a boolean whether to include custom paper sizes as defined
    in the page setup dialog}
  @begin{return}
    A list of  @class{gtk:paper-size} instances.
  @end{return}
  @begin{short}
    Creates a list of known paper sizes.
  @end{short}
  @see-class{gtk:paper-size}"
  (custom :boolean))

(export 'paper-size-paper-sizes)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_name () -> paper-size-name
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_name" paper-size-name) :string
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A string with the name of the paper size.}
  @begin{short}
    Gets the name of the paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_display_name () -> paper-size-display-name
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_display_name" paper-size-display-name) :string
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A string with the human readable name of the paper size.}
  @begin{short}
    Gets the human readable name of the paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-display-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_ppd_name () -> paper-size-ppd-name
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_ppd_name" paper-size-ppd-name) :string
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A string with the PPD name of the paper size.}
  @begin{short}
    Gets the PPD name of the paper size, which may be @code{nil}.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-ppd-name)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_width () -> paper-size-width
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_width" paper-size-width) :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value,
    not @code{:none}}
  @return{A double float with the paper width.}
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
;;; gtk_paper_size_get_height () -> paper-size-height
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_height" paper-size-height) :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value,
    not @code{:none}}
  @return{A double float with the paper height.}
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
;;; gtk_paper_size_is_ipp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_is_ipp" paper-size-is-ipp) :boolean
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A boolean whether the paper size is an IPP paper size.}
  @begin{short}
    Returns @em{true} if the paper size is an IPP standard paper size.
  @end{short}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-is-ipp)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_is_custom ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_is_custom" paper-size-is-custom) :boolean
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A boolean whether @arg{size} is a custom paper size.}
  @short{Returns @em{true} if @arg{size} is not a standard paper size.}
  @see-class{gtk:paper-size}"
  (size (g:boxed paper-size)))

(export 'paper-size-is-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_set_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_set_size" %paper-size-set-size) :void
  (size (g:boxed paper-size))
  (width :double)
  (height :double)
  (unit unit))

(defun paper-size-set-size (size width height unit)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a custom @class{gtk:paper-size} instance}
  @argument[width]{a number coerced to a double float with the new width in
    units of @arg{unit}}
  @argument[height]{a number coerced to a double float with the new height in
    units of @arg{unit}}
  @argument[unit]{the @symbol{gtk:unit} value for @arg{width} and @arg{height}}
  @begin{short}
    Changes the dimensions of a paper size to @arg{width} x @arg{height}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-symbol{gtk:unit}"
  (%paper-size-set-size size
                        (coerce width 'double-float)
                        (coerce height 'double-float)
                        unit))

(export 'paper-size-set-size)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_get_default_top_margin ()
;;; -> paper-size-default-top-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_top_margin" paper-size-default-top-margin)
    :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{A double float with the default top margin.}
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
;;; gtk_paper_size_get_default_bottom_margin ()
;;; -> paper-size-default-bottom-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_bottom_margin"
           paper-size-default-bottom-margin) :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{A double float with the default bottom margin.}
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
;;; gtk_paper_size_get_default_left_margin ()
;;; -> paper-size-default-left-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_left_margin"
           paper-size-default-left-margin) :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{A double float with the default left margin.}
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
;;; gtk_paper_size_get_default_right_margin ()
;;; -> paper-size-default-right-margin
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default_right_margin"
           paper-size-default-right-margin) :double
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[unit]{a @symbol{gtk:unit} value for the return value, not
    @code{:none}}
  @return{A double float with the default right margin.}
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
;;; gtk_paper_size_get_default () -> paper-size-default
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_get_default" paper-size-default) :string
 #+liber-documentation
 "@version{2023-1-31}
  @begin{return}
    A string with the name of the default paper size.
  @end{return}
  @begin{short}
    Returns the name of the default paper size, which depends on the current
    locale.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:paper-size-default) => \"iso_a4\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:paper-size}")

(export 'paper-size-default)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_key_file ()
;;; ----------------------------------------------------------------------------

;; TODO: Consider to change the error handling: use with-ignore-g-error?

(defcfun ("gtk_paper_size_new_from_key_file" %paper-size-new-from-key-file)
    (g:boxed paper-size :return)
  (keyfile (:pointer (:struct g:key-file)))
  (groupname :string)
  (err :pointer))

(defun paper-size-new-from-key-file (keyfile groupname)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[keyfile]{the @type{g:key-file} instance to retrieve the paper size
    from}
  @argument[groupname]{a string with the name of the group in the key file to
    read, or @code{nil} to read the first group}
  @begin{return}
    A new @class{gtk:paper-size} instance with the restored paper size, or
    @code{nil} if an error occurred.
  @end{return}
  @begin{short}
    Reads a paper size from the group @arg{groupname} in the key file
    @arg{keyfile}.
  @end{short}
  @see-class{gtk:paper-size}
  @see-type{g:key-file}"
  (with-g-error (err)
    (%paper-size-new-from-key-file keyfile groupname err)))

(export 'paper-size-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_new_from_gvariant" paper-size-new-from-gvariant)
    (g:boxed paper-size :return)
 #+liber-documentation
 "@version{2023-1-31}
  @argument[value]{a @code{a{sv@}} @type{g:variant} instance}
  @return{A @class{gtk:paper-size} instance.}
  @begin{short}
    Deserialize a paper size from a @code{a{sv@}} variant in the format
    produced by the @fun{gtk:paper-size-to-gvariant} function.
  @end{short}

  Since 3.22
  @see-class{gtk:paper-size}
  @see-type{g:variant}
  @see-function{gtk:paper-size-to-gvariant}"
  (value (:pointer (:struct g:variant))))

(export 'paper-size-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_paper_size_to_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_to_key_file" paper-size-to-key-file) :void
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @argument[keyfile]{the @type{g:key-file} instance to save the paper size to}
  @argument[groupname]{a string with the group name to add the settings to in
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
;;; gtk_paper_size_to_gvariant ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_paper_size_to_gvariant" paper-size-to-gvariant)
    (:pointer (:struct g:variant))
 #+liber-documentation
 "@version{2023-1-31}
  @argument[size]{a @class{gtk:paper-size} instance}
  @return{A new @type{g:variant} instance.}
  @begin{short}
    Serialize a paper size to a @code{a{sv@}} variant instance.
  @end{short}

  Since 3.22
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:paper-size-to-gvariant (gtk:paper-size-new))
=> #.(SB-SYS:INT-SAP #X00F02070)
(g:variant-print * nil)
=> \"{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>@}\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:paper-size}
  @see-type{g:variant}"
  (size (g:boxed paper-size)))

(export 'paper-size-to-gvariant)

;;; --- End of file gtk4.paper-size.lisp ---------------------------------------
