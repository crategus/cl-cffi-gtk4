;;; ----------------------------------------------------------------------------
;;; gtk4.page-setup.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkPageSetup
;;;
;;;     Stores page setup information
;;;
;;; Types and Values
;;;
;;;     GtkPageSetup
;;;
;;; Functions
;;;
;;;     gtk_page_setup_new
;;;     gtk_page_setup_new_from_file
;;;     gtk_page_setup_new_from_key_file
;;;     gtk_page_setup_new_from_gvariant
;;;
;;;     gtk_page_setup_copy
;;;     gtk_page_setup_get_orientation
;;;     gtk_page_setup_set_orientation
;;;     gtk_page_setup_get_paper_size
;;;     gtk_page_setup_set_paper_size
;;;     gtk_page_setup_get_top_margin
;;;     gtk_page_setup_set_top_margin
;;;     gtk_page_setup_get_bottom_margin
;;;     gtk_page_setup_set_bottom_margin
;;;     gtk_page_setup_get_left_margin
;;;     gtk_page_setup_set_left_margin
;;;     gtk_page_setup_get_right_margin
;;;     gtk_page_setup_set_right_margin
;;;     gtk_page_setup_set_paper_size_and_default_margins
;;;     gtk_page_setup_get_paper_width
;;;     gtk_page_setup_get_paper_height
;;;     gtk_page_setup_get_page_width
;;;     gtk_page_setup_get_page_height
;;;     gtk_page_setup_load_file
;;;     gtk_page_setup_load_key_file
;;;     gtk_page_setup_to_file
;;;     gtk_page_setup_to_key_file
;;;     gtk_page_setup_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPageSetup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPageSetup
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkPageSetup" page-setup
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_page_setup_get_type")
  nil)

#+liber-documentation
(setf (documentation 'page-setup 'type)
 "@version{2024-4-30}
  @begin{short}
    The @class{gtk:page-setup} object stores the page size, orientation and
    margins.
  @end{short}
  The idea is that you can get one of these from the page setup dialog and then
  pass it to the @class{gtk:print-operation} object when printing. The benefit
  of splitting this out of the @class{gtk:print-settings} object is that these
  affect the actual layout of the page, and thus need to be set long before
  user prints.

  The margins specified in this object are the \"print margins\", i.e. the
  parts of the page that the printer cannot print on. These are different from
  the layout margins that a word processor uses. They are typically used to
  determine the minimal size for the layout margins.

  To obtain a @class{gtk:page-setup} object use the function
  @fun{gtk:page-setup-new} to get the defaults, or use the function
  @fun{gtk:print-run-page-setup-dialog} to show the page setup dialog and
  receive the resulting page setup.
  @begin{examples}
    A page setup dialog.
    @begin{pre}
(defun do-page-setup (settings page-setup)
  (when (not settings)
    ;; Set default print settings
    (setf settings (gtk:print-settings-new)))
  ;; Return the new page setup from the dialog
  (gtk:print-run-page-setup-dialog window page-setup settings))
    @end{pre}
  @end{examples}
  @see-constructor{gtk:page-setup-new}
  @see-constructor{gtk:page-setup-new-from-file}
  @see-constructor{gtk:page-setup-new-from-gvariant}
  @see-constructor{gtk:page-setup-new-from-key-file}
  @see-constructor{gtk:page-setup-copy}
  @see-class{gtk:print-operation}
  @see-class{gtk:print-settings}")

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new
;;; ----------------------------------------------------------------------------

(declaim (inline page-setup-new))

(defun page-setup-new ()
 #+liber-documentation
 "@version{2024-4-30}
  @return{The new @class{gtk:page-setup} object.}
  @short{Creates a new page setup.}
  @see-class{gtk:page-setup}"
  (make-instance 'page-setup))

(export 'page-setup-new)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_new_from_file" %page-setup-new-from-file)
    (g:object page-setup :already-referenced)
  (filename :string)
  (err :pointer))

(defun page-setup-new-from-file (path)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[path]{a pathname or namestring with the file to read the page setup
    from}
  @return{The restored @class{gtk:page-setup} object.}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  Returns a new @class{gtk:page-setup} object with the restored page setup,
  or @code{nil} if an error occurred. See the @fun{gtk:page-setup-to-file}
  function.
  @see-class{gtk:page-setup}
  @see-function{gtk:page-setup-to-file}"
  (glib:with-ignore-g-error (err)
    (%page-setup-new-from-file (namestring path) err)))

(export 'page-setup-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_key_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_new_from_key_file" %page-setup-new-from-key-file)
    (g:object page-setup :already-referenced)
  (keyfile (:pointer (:struct g:key-file)))
  (group :string)
  (err :pointer))

(defun page-setup-new-from-key-file (keyfile group)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[keyfile]{a @type{g:key-file} instance to retrieve the page setup
    from}
  @argument[group]{a string with the name of the group in the key file to
    read, or @code{nil} to use the default name \"Page Setup\"}
  @return{The restored @class{gtk:page-setup} object.}
  @begin{short}
    Reads the page setup from the group @arg{group} in the key file.
  @end{short}
  Returns a new @class{gtk:page-setup} object with the restored page setup, or
  @code{nil} if an error occurred.
  @see-class{gtk:page-setup}
  @see-type{g:key-file}"
  (glib:with-ignore-g-error (err)
    (%page-setup-new-from-key-file keyfile
                                   (or group (cffi:null-pointer))
                                   err)))

(export 'page-setup-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_new_from_gvariant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_new_from_gvariant" page-setup-new-from-gvariant)
    (g:object page-setup :already-referenced)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[variant]{a @type{g:variant} instance of @code{a{sv@}} type}
  @return{The new @class{gtk:page-setup} object.}
  @begin{short}
    Deserialize a page setup from a @code{a{sv@}} variant in the format
    produced by the @fun{gtk:page-setup-to-gvariant} function.
  @end{short}
  @see-class{gtk:page-setup}
  @see-type{g:variant}
  @see-function{gtk:page-setup-to-gvariant}"
  (variant (:pointer (:struct g:variant))))

(export 'page-setup-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_copy" page-setup-copy)
    (g:object page-setup :already-referenced)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object to copy}
  @return{The @class{gtk:page-setup} object with the copy of @arg{setup}.}
  @short{Copies a page setup.}
  @see-class{gtk:page-setup}"
  (setup (g:object page-setup)))

(export 'page-setup-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_orientation
;;; gtk_page_setup_set_orientation
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-orientation) (orientation setup)
  (cffi:foreign-funcall "gtk_page_setup_set_orientation"
                        (g:object page-setup) setup
                        page-orientation orientation
                        :void)
  orientation)

(cffi:defcfun ("gtk_page_setup_get_orientation" page-setup-orientation)
    page-orientation
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-orientation setup) => orientation}
  @syntax{(setf (gtk:page-setup-orientation setup) orientation)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[orientation]{a @symbol{gtk:page-orientation} value}
  @begin{short}
      The @fun{gtk:page-setup-orientation} function gets the page orientation
      of the page setup object.
  @end{short}
  The @setf{gtk:page-setup-orientation} function sets the page orientation.
  Possible values are @code{:portrait} and @code{:landscape}.
  @begin{examples}
    Get the default page orientation.
    @begin{pre}
(let ((setup (gtk:page-setup-new)))
  (gtk:page-setup-orientation setup))
=> :PORTRAIT
    @end{pre}
  @end{examples}
  @see-class{gtk:page-setup}
  @see-symbol{gtk:page-orientation}"
  (setup (g:object page-setup)))

(export 'page-setup-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_size
;;; gtk_page_setup_set_paper_size
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-paper-size) (size setup)
  (cffi:foreign-funcall "gtk_page_setup_set_paper_size"
                        (g:object page-setup) setup
                        (g:boxed paper-size) size
                        :void)
  size)

(cffi:defcfun ("gtk_page_setup_get_paper_size" page-setup-paper-size)
    (g:boxed paper-size)
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-paper-size setup) => size}
  @syntax{(setf (gtk:page-setup-paper-size setup) size)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[size]{a @class{gtk:paper-size} instance}
  @begin{short}
    The @fun{gtk:page-setup-paper-size} function gets the paper size of the
    page setup object.
  @end{short}
  The @setf{gtk:page-setup-paper-size} function sets the paper size without
  changing the margins. See also the
  @fun{gtk:page-setup-set-paper-size-and-default-margins} function.
  @see-class{gtk:page-setup}
  @see-class{gtk:paper-size}
  @see-function{gtk:page-setup-set-paper-size-and-default-margins}"
  (setup (g:object page-setup)))

(export 'page-setup-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_top_margin
;;; gtk_page_setup_set_top_margin
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-top-margin) (margin setup unit)
  (let ((margin (coerce margin 'double-float)))
    (cffi:foreign-funcall "gtk_page_setup_set_top_margin"
                          (g:object page-setup) setup
                          :double margin
                          unit unit
                          :void)
    margin))

(cffi:defcfun ("gtk_page_setup_get_top_margin" page-setup-top-margin) :double
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-top-margin setup unit) => margin}
  @syntax{(setf (gtk:page-setup-top-margin setup unit) margin)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value}
  @argument[margin]{a number coerced to a double float with the top margin in
    units of @arg{unit}}
  @begin{short}
    The @fun{gtk:page-setup-top-margin} function gets the top margin of the
    page setup in units of @arg{unit}.
  @end{short}
  The @setf{gtk:page-setup-top-margin} function sets the top margin.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-top-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_bottom_margin
;;; gtk_page_setup_set_bottom_margin
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-bottom-margin) (margin setup unit)
  (let ((margin (coerce margin 'double-float)))
    (cffi:foreign-funcall "gtk_page_setup_set_bottom_margin"
                          (g:object page-setup) setup
                          :double margin
                          unit unit
                          :void)
    margin))

(cffi:defcfun ("gtk_page_setup_get_bottom_margin" page-setup-bottom-margin)
    :double
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-bottom-margin setup unit) => margin}
  @syntax{(setf (gtk:page-setup-bottom-margin setup unit) margin)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value}
  @argument[margin]{a number coerced to a double float with the bottom margin
    in units of @arg{unit}}
  @begin{short}
    The @fun{gtk:page-setup-bottom-margin} function gets the bottom margin of
    the page setup in units of @arg{unit}.
  @end{short}
  The @setf{gtk:page-setup-bottom-margin} function sets the bottom margin.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-bottom-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_left_margin
;;; gtk_page_setup_set_left_margin
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-left-margin) (margin setup unit)
  (let ((margin (coerce margin 'double-float)))
    (cffi:foreign-funcall "gtk_page_setup_set_left_margin"
                          (g:object page-setup) setup
                          :double margin
                          unit unit
                          :void)
    margin))

(cffi:defcfun ("gtk_page_setup_get_left_margin" page-setup-left-margin) :double
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-left-margin setup unit) => margin}
  @syntax{(setf (gtk:page-setup-left-margin setup unit) margin)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value}
  @argument[margin]{a number coerced to a double float with the left margin in
    units of @arg{unit}}
  @begin{short}
    The @fun{gtk:page-setup-left-margin} function gets the left margin of the
    page setup in units of @arg{unit}.
  @end{short}
  The @setf{gtk:page-setup-left-margin} function sets the left margin.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-left-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_right_margin
;;; gtk_page_setup_set_right_margin
;;; ----------------------------------------------------------------------------

(defun (setf page-setup-right-margin) (margin setup unit)
  (let ((margin (coerce margin 'double-float)))
    (cffi:foreign-funcall "gtk_page_setup_set_right_margin"
                          (g:object page-setup) setup
                          :double margin
                          unit unit
                          :void)
    margin))

(cffi:defcfun ("gtk_page_setup_get_right_margin" page-setup-right-margin)
    :double
 #+liber-documentation
 "@version{2024-4-30}
  @syntax{(gtk:page-setup-right-margin setup unit) => margin}
  @syntax{(setf (gtk:page-setup-right-margin setup unit) margin)}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value}
  @argument[margin]{a number coerced to a double float with the right margin in
    units of @arg{unit}}
  @begin{short}
    The @fun{gtk:page-setup-right-margin} function gets the right margin of the
    page setup in units of @arg{unit}.
  @end{short}
  The @setf{gtk:page-setup-right-margin} function sets the right margin.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-right-margin)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_set_paper_size_and_default_margins
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_set_paper_size_and_default_margins"
               page-setup-set-paper-size-and-default-margins) :void
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[size]{a @class{gtk:paper-size} instance}
  @begin{short}
    Sets the paper size of the page setup and modifies the margins according
    to the new paper size.
  @end{short}
  @see-class{gtk:page-setup}
  @see-class{gtk:paper-size}"
  (setup (g:object page-setup))
  (size (g:boxed paper-size)))

(export 'page-setup-set-paper-size-and-default-margins)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_get_paper_width" page-setup-paper-width) :double
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value for the return value}
  @return{The double float with the paper width.}
  @begin{short}
    Returns the paper width of the page setup in units of @arg{unit}.
  @end{short}
  Note that this function takes orientation, but not margins into consideration.
  See also the @fun{gtk:page-setup-page-width} function.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}
  @see-function{gtk:page-setup-page-width}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_paper_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_get_paper_height" page-setup-paper-height)
    :double
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value for the return value}
  @return{The double float with the paper height.}
  @begin{short}
    Returns the paper height of the page setup in units of @arg{unit}.
  @end{short}
  Note that this function takes orientation, but not margins into consideration.
  See also the @fun{gtk:page-setup-page-height} function.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}
  @see-function{gtk:page-setup-page-height}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_get_page_width" page-setup-page-width) :double
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value for the return value}
  @return{The double float with the page width.}
  @begin{short}
    Returns the page width of the page setup in units of @arg{unit}.
  @end{short}
  Note that this function takes orientation and margins into consideration.
  See also the @fun{gtk:page-setup-paper-width} function.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}
  @see-function{gtk:page-setup-paper-width}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-page-width)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_get_page_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_get_page_height" page-setup-page-height) :double
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[unit]{a @symbol{gtk:unit} value for the return value}
  @return{The double float with the page height.}
  @begin{short}
    Returns the page height of the page setup in units of @arg{unit}.
  @end{short}
  Note that this function takes orientation and margins into consideration.
  See also the @fun{gtk:page-setup-paper-height} function.
  @see-class{gtk:page-setup}
  @see-symbol{gtk:unit}
  @see-function{gtk:page-setup-paper-height}"
  (setup (g:object page-setup))
  (unit unit))

(export 'page-setup-page-height)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_load_file" %page-setup-load-file) :boolean
  (page-setup (g:object page-setup))
  (filename :string)
  (err :pointer))

(defun page-setup-load-file (setup path)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[path]{a pathname or namestring with the file to read the page setup
    from}
  @return{@em{True} on success.}
  @begin{short}
    Reads the page setup from a file.
  @end{short}
  See also the @fun{gtk:page-setup-to-file} function.
  @see-class{gtk:page-setup}
  @see-function{gtk:page-setup-to-file}"
  (glib:with-ignore-g-error (err)
    (%page-setup-load-file setup (namestring path) err)))

(export 'page-setup-load-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_load_key_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_load_key_file" %page-setup-load-key-file)
    :boolean
  (setup (g:object page-setup))
  (keyfile (:pointer (:struct g:key-file)))
  (group :string)
  (err :pointer))

(defun page-setup-load-key-file (setup keyfile group)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[keyfile]{a @type{g:key-file} instance to retrieve the page setup
    from}
  @argument[group]{a string with the name of the group in the key file to read,
    or @code{nil} to use the default name \"Page Setup\"}
  @return{@em{True} on success.}
  @begin{short}
    Reads the page setup from the group @arg{group} in the key file.
  @end{short}
  @see-class{gtk:page-setup}
  @see-type{g:key-file}"
  (glib:with-ignore-g-error (err)
    (%page-setup-load-key-file setup
                               keyfile
                               (or group (cffi:null-pointer))
                               err)))

(export 'page-setup-load-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_to_file" %page-setup-to-file) :boolean
  (setup (g:object page-setup))
  (filename :string)
  (err :pointer))

(defun page-setup-to-file (setup path)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[path]{a pathname or namestring with the file to save to}
  @return{@em{True} on success.}
  @begin{short}
    The function saves the information from the page setup to a file.
  @end{short}
  @see-class{gtk:page-setup}"
  (glib:with-ignore-g-error (err)
    (%page-setup-to-file setup (namestring path) err)))

(export 'page-setup-to-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_key_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_to_key_file" %page-setup-to-key-file) :void
  (setup (g:object page-setup))
  (keyfile (:pointer (:struct g:key-file)))
  (group :string))

(defun page-setup-to-key-file (setup keyfile group)
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @argument[keyfile]{a @type{g:key-file} instance to save the page setup to}
  @argument[group]{a string with the group to add the settings to in the key
    file, or @code{nil} to use the default name \"Page Setup\"}
  @begin{short}
    The function adds the page setup from the page setup to a key file.
  @end{short}
  @see-class{gtk:page-setup}
  @see-type{g:key-file}"
  (%page-setup-to-key-file setup
                           keyfile
                           (or group (cffi:null-pointer))))

(export 'page-setup-to-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_page_setup_to_gvariant
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_page_setup_to_gvariant" page-setup-to-gvariant)
    (:pointer (:struct g:variant))
 #+liber-documentation
 "@version{2024-4-30}
  @argument[setup]{a @class{gtk:page-setup} object}
  @return{The new @type{g:variant} instance of @code{a{sv@}} type.}
  @begin{short}
    Serialize the page setup to a @code{a{sv@}} variant.
  @end{short}
  @see-class{gtk:page-setup}
  @see-type{g:variant}"
  (setup (g:object page-setup)))

(export 'page-setup-to-gvariant)

;;; ---- End of file gtk4.page-setup.lisp --------------------------------------
