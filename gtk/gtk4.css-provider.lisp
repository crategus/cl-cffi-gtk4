;;; ----------------------------------------------------------------------------
;;; gtk4.css-provider.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;; GtkCssProvider
;;;
;;;     CSS-like styling for widgets
;;;
;;; Types and Values
;;;
;;;     GtkCssProvider
;;;     GtkCssParserError                                  not implemented
;;;     GtkCssParserWarning                                not implemented
;;;     GtkCssLocation
;;;     GtkCssSection
;;;
;;;     GTK_CSS_PARSER_ERROR                               not implemented
;;;
;;; Functions
;;;
;;;     gtk_css_provider_load_named
;;;     gtk_css_provider_load_from_data                    Deprecated 4.12
;;;     gtk_css_provider_load_from_file
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_load_from_resource
;;;     gtk_css_provider_load_from_bytes                   Since 4.12
;;;     gtk_css_provider_load_from_string                  Since 4.12
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;
;;;     gtk_css_section_new
;;;     gtk_css_section_ref                                not needed
;;;     gtk_css_section_unref                              not needed
;;;     gtk_css_section_print                              not needed
;;;     gtk_css_section_to_string
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_start_location
;;;     gtk_css_section_get_end_location
;;;
;;; Signals
;;;
;;;     parsing-error
;;;
;;; Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkCssSection
;;;
;;;     GObject
;;;     ╰── GtkCssProvider
;;;
;;; Implemented Interfaces
;;;
;;;     GtkStyleProvider
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCssLocation
;;; ----------------------------------------------------------------------------

(cffi:defcstruct css-location
  (bytes :size)
  (chars :size)
  (lines :size)
  (line-bytes :size)
  (line-chars :size))

#+liber-documentation
(setf (liber:alias-for-symbol 'css-location)
      "CStruct"
      (liber:symbol-documentation 'css-location)
 "@version{#2022-8-20}
  @begin{short}
    The @class{gtk:css-location} structure is used to present a location in a
    file or other source of data parsed by the CSS engine.
  @end{short}
  The @code{bytes} and @code{line-bytes} offsets are meant to be used to
  programmatically match data. The @code{lines} and @code{line-chars} offsets
  can be used for printing the location in a file.

  Note that the lines parameter starts from 0 and is increased whenever a CSS
  line break is encountered. CSS defines the C character sequences \"\\r\\n\",
  \"\\r\", \"\\n\" and \"\\f\" as newlines. If your document uses different
  rules for line breaking, you might want run into problems here.
  @begin{pre}
(cffi:defcstruct css-location
  (bytes :size)
  (chars :size)
  (lines :size)
  (line-bytes :size)
  (line-chars :size))
  @end{pre}
  @begin[code]{table}
    @entry[bytes]{Number of bytes parsed since the beginning.}
    @entry[chars]{Number of characters parsed since the beginning.}
    @entry[lines]{Number of full lines that have been parsed. If you want to
      display this as a line number, you need to add 1 to this.}
    @entry[line-bytes]{Number of bytes parsed since the last line break.}
    @entry[line-chars]{Number of characters parsed since the last line break.}
  @end{table}
  @see-function{gtk:css-location-bytes}
  @see-function{gtk:css-location-chars}
  @see-function{gtk:css-location-lines}
  @see-function{gtk:css-location-line-bytes}
  @see-function{gtk:css-location-line-chars}
  @see-class{gtk:css-section}")

(export 'css-location)

;;; ----------------------------------------------------------------------------
;;; Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:css-location-bytes -------------------------------------------------
#+liber-documentation
(setf (liber:alias-for-function 'css-location-bytes) "Accessor")

(defun css-location-bytes (location)
 #+liber-documentation
 "@version{#2022-8-20}
  @syntax[]{(gtk:css-location-bytes location) => bytes}
  @argument[location]{a @symbol{gtk:css-location} instance}
  @argument[bytes]{a @code{:size} value}
  @short{Returns the number of bytes parsed since the beginning.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'bytes))

(export 'css-location-bytes)

;;; --- gtk:css-location-chars -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-chars) "Accessor")

(defun css-location-chars (location)
 #+liber-documentation
 "@version{#2022-8-20}
  @syntax[]{(gtk:css-location-bytes location) => chars}
  @argument[location]{a @symbol{gtk:css-location} instance}
  @argument[bytes]{a @code{:size} value}
  @short{Returns the number of characters parsed since the beginning.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'chars))

(export 'css-location-chars)

;;; --- gtk:css-location-lines -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-lines) "Accessor")

(defun css-location-lines (location)
 #+liber-documentation
 "@version{2022-12-4}
  @syntax[]{(gtk:css-location-bytes location) => lines}
  @argument[location]{a @symbol{gtk:css-location} instance}
  @argument[lines]{a @code{:size} value}
  @short{Returns the number of full lines that have been parsed.}
  If you want to display this as a line number, you need to add 1 to this.
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'lines))

(export 'css-location-lines)

;;; --- gtk:css-location-line-bytes --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-line-bytes) "Accessor")

(defun css-location-line-bytes (location)
 "@version{2022-12-4}
  @syntax[]{(gtk:css-location-bytes location) => line-bytes}
  @argument[location]{a @symbol{gtk:css-location} instance}
  @argument[line-bytes]{a @code{:size} value}
  @short{Returns the number of bytes parsed since the last line break.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'line-bytes))

(export 'css-location-line-bytes)

;;; --- gtk:css-location-line-chars --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-line-chars) "Accessor")

(defun css-location-line-chars (location)
 "@version{#2022-8-20}
  @syntax[]{(gtk:css-location-bytes location) => lines}
  @argument[location]{a @symbol{gtk:css-location} instance}
  @argument[lines]{a @code{:size} value}
  @short{Returns the number of characters parsed since the last line break.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'line-chars))

(export 'css-location-line-chars)

;;; ----------------------------------------------------------------------------
;;; GtkCssSection
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque css-section "GtkCssSection"
  :type-initializer "gtk_css_section_get_type"
  :alloc (error "GtkCssSection cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'css-section)
      "GBoxed"
      (documentation 'css-section 'type)
 "@version{#2023-1-24}
  @begin{short}
    Defines a part of a CSS document.
  @end{short}
  Because sections are nested into one another, you can use the
  @fun{gtk:css-section-parent} function to get the containing region.
  @see-constructor{gtk:css-section-new}
  @see-class{gtk:css-provider}
  @see-function{gtk:css-section-parent}")

(export 'css-section)

;;; ----------------------------------------------------------------------------
;;; GTK_CSS_PARSER_ERROR
;;;
;;; #define GTK_CSS_PARSER_ERROR (gtk_css_parser_error_quark ())
;;;
;;; Domain for GtkCssParser errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkCssParserError
;;;
;;; Errors that can occur while parsing CSS.
;;;
;;; These errors are unexpected and will cause parts of the given CSS to be
;;; ignored.
;;;
;;; GTK_CSS_PARSER_ERROR_FAILED :
;;;     Unknown failure.
;;;
;;; GTK_CSS_PARSER_ERROR_SYNTAX :
;;;     The given text does not form valid syntax
;;;
;;; GTK_CSS_PARSER_ERROR_IMPORT :
;;;     Failed to import a resource
;;;
;;; GTK_CSS_PARSER_ERROR_NAME :
;;;     The given name has not been defined
;;;
;;; GTK_CSS_PARSER_ERROR_UNKNOWN_VALUE :
;;;     The given value is not correct
;;;-----------------------------------------------------------------------------

;;;-----------------------------------------------------------------------------
;;; enum GtkCssParserWarning
;;;
;;; Warnings that can occur while parsing CSS.
;;;
;;; Unlike GtkCssParserErrors, warnings do not cause the parser to skip any
;;; input, but they indicate issues that should be fixed.
;;;
;;; GTK_CSS_PARSER_WARNING_DEPRECATED :
;;;     The given construct is deprecated and will be removed in a future
;;;     version
;;;
;;; GTK_CSS_PARSER_WARNING_SYNTAX :
;;;     A syntax construct was used that should be avoided
;;;
;;; GTK_CSS_PARSER_WARNING_UNIMPLEMENTED :
;;;     A feature is not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkCssProvider
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCssProvider" css-provider
  (:superclass g:object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_css_provider_get_type")
  nil)

;; TODO: Add an example for a "parse-error" signal handler

#+liber-documentation
(setf (documentation 'css-provider 'type)
 "@version{2022-11-25}
  @begin{short}
    The @class{gtk:css-provider} object is an object implementing the
    @class{gtk:style-provider} interface.
  @end{short}
  It is able to parse CSS-like input in order to style widgets.

  An application can make GTK parse a specific CSS style sheet by calling
  the @fun{gtk:css-provider-load-from-file} or
  @fun{gtk:css-provider-load-from-resource} functions and adding the provider
  with the @fun{gtk:style-context-add-provider} or
  @fun{gtk:style-context-add-provider-for-display} functions.

  In addition, certain files will be read when GTK is initialized. First, the
  @file{$XDG_CONFIG_HOME/gtk-4.0/gtk.css} file is loaded if it exists. Then,
  GTK loads the first existing file among
  @file{XDG_DATA_HOME/themes/THEME/gtk-VERSION/gtk-VARIANT.css},
  @file{$HOME/.themes/THEME/gtk-VERSION/gtk:VARIANT.css},
  @file{$XDG_DATA_DIRS/themes/THEME/gtk-VERSION/gtk-VARIANT.css} and
  @file{DATADIR/share/themes/THEME/gtk-VERSION/gtk-VARIANT.css}, where
  @code{THEME} is  the name of the current theme, see the
  @slot[gtk:settings]{gtk-icon-theme-name setting} setting,
  @code{VARIANT} is the variant to load, see the
  @slot[gtk:settings]{gtk-application-prefer-dark-theme} setting,
  @code{DATADIR} is the prefix configured when GTK was compiled (unless
  overridden by the @code{GTK_DATA_PREFIX} environment variable), and
  @code{VERSION} is the GTK version number. If no file is found for the current
  version, GTK tries older versions all the way back to 4.0.

  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"parsing-error\" signal}
      @begin{pre}
lambda (provider section error)    :run-last
      @end{pre}
      Signals that a parsing error occured. The @arg{section} argument describes
      the actual location of the error as accurately as possible. Parsing errors
      are never fatal, so the parsing will resume after the error. Errors may
      however cause parts of the given data or even all of it to not be parsed
      at all. So it is a useful idea to check that the parsing succeeds by
      connecting to this signal.

      Note that this signal may be emitted at any time as the CSS provider may
      opt to defer parsing parts or all of the input to a later time than when
      a loading function was called.
      @begin[code]{table}
        @entry[provider]{The @class{gtk:css-provider} object that had a parsing
          error.}
        @entry[section]{The @class{gtk:css-section} instance the error happened
          in.}
        @entry[error]{The parsing error of type @code{GError}.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:css-provider-new}
  @see-class{gtk:style-context}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-section}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new
;;; ----------------------------------------------------------------------------

(declaim (inline css-provider-new))

(defun css-provider-new ()
 #+liber-documentation
 "@version{2023-4-15}
  @return{A new @class{gtk:css-provider} object.}
  @short{Returns a newly created CSS provider object.}
  @see-class{gtk:css-provider}"
  (make-instance 'css-provider))

(export 'css-provider-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_named
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_named" %css-provider-load-named) :void
  (provider (g:object css-provider))
  (name :string)
  (variant :string))

(defun css-provider-load-named (provider name &optional variant)
 #+liber-documentation
 "@version{2023-4-15}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[name]{a string with the theme name}
  @argument[variant]{an optional string with a variant to load, e.g. \"dark\"}
  @begin{short}
    Loads a theme from the usual theme paths.
  @end{short}
  The actual process of finding the theme might change between releases, but it
  is guaranteed that this function uses the same mechanism to load the theme
  that GTK uses for loading its own theme.
  @see-class{gtk:css-provider}"
  (%css-provider-load-named provider
                            name
                            (if variant variant (cffi:null-pointer))))

(export 'css-provider-load-named)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_data
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_data" %css-provider-load-from-data)
    :void
  (provider (g:object css-provider))
  (data :string)
  (len :ssize))

(defun css-provider-load-from-data (provider data)
 #+liber-documentation
 "@version{2023-10-4}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[data]{a string with the CSS data}
  @begin{short}
    Loads data into the CSS provider, making it clear any previously loaded
    information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:css-provider-load-from-string} or
    @fun{gtk:css-provider-load-from-bytes} functions instead.
  @end{dictionary}
  @see-class{gtk:css-provider}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:CSS-PROVIDER-LOAD-FROM-DATA is deprecated since 4.12."))
  (%css-provider-load-from-data provider data -1))

(export 'css-provider-load-from-data)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_file" css-provider-load-from-file)
    :void
 #+liber-documentation
 "@version{2023-8-30}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[file]{a @class{g:file} object pointing to a file to load}
  @begin{short}
    Loads the data contained in @arg{file} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}
  @see-class{g:file}"
  (provider (g:object css-provider))
  (file g:object))
;; TODO: Introduce the FILE-AS-NAMESTRING type
;  (file g:file-as-namestring))

(export 'css-provider-load-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_path" %css-provider-load-from-path)
    :void
  (provider (g:object css-provider))
  (path :string))

(defun css-provider-load-from-path (provider path)
 #+liber-documentation
 "@version{2023-1-29}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a pathname or namestring with the path of a file to load, in
    the GLib filename encoding}
  @begin{short}
    Loads the data contained in @arg{path} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}"
  (%css-provider-load-from-path provider (namestring path)))

(export 'css-provider-load-from-path)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_resource"
               css-provider-load-from-resource) :void
 #+liber-documentation
 "@version{2023-8-30}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a string with the resource path}
  @begin{short}
    Loads the data contained in the resource at @arg{path} into the CSS
    provider, clearing any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the @code{\"parsing-error\"}
  signal of the @class{gtk:css-provider} object.
  @see-class{gtk:css-provider}
  @see-class{g:resource}"
  (provider (g:object css-provider))
  (path :string))

(export 'css-provider-load-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_bytes
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_css_provider_load_from_bytes" css-provider-load-from-bytes)
    :void
 #+liber-documentation
 "@version{2023-12-16}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[data]{a @class{g:bytes} instance containing the CSS data to load}
  @begin{short}
    Loads data into the CSS provider.
  @end{short}
  This clears any previously loaded information.

  Since 4.12
  @see-class{gtk:css-provider}"
  (provider (g:object css-provider))
  (data (g:boxed g:bytes)))

#+gtk-4-12
(export 'css-provider-load-from-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_string
;;; ----------------------------------------------------------------------------

#+gtk-4-12
(cffi:defcfun ("gtk_css_provider_load_from_string"
               css-provider-load-from-string) :void
 #+liber-documentation
 "@version{2023-12-16}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[str]{a string with the CSS data to load}
  @begin{short}
    Loads data into the CSS provider.
  @end{short}
  This clears any previously loaded information.

  Since 4.12
  @see-class{gtk:css-provider}"
  (provider (g:object css-provider))
  (str :string))

#+gtk-4-12
(export 'css-provider-load-from-string)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_to_string" css-provider-to-string) :string
 #+liber-documentation
 "@version{2023-4-15}
  @argument[provider]{a @class{gtk:css-provider} object to write to a string}
  @return{A string representing the CSS provider.}
  @begin{short}
    Convertes the CSS provider into a string representation in CSS format.
  @end{short}
  Using the @fun{gtk:css-provider-load-from-data} function with the return
  value from this function on a new CSS provider created with the
  @fun{gtk:css-provider-new} function will basically create a duplicate of this
  provider.
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-new}
  @see-function{gtk:css-provider-load-from-data}"
  (provider (g:object css-provider)))

(export 'css-provider-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk:css-section-new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_new" css-section-new)
    (g:boxed css-section :return)
 #+liber-documentation
 "@version{#2023-8-30}
  @argument[file]{a @class{g:file} object this section refers to}
  @argument[start]{a @symbol{gtk:css-location} instance with the start location}
  @argument[end]{a @symbol{gtk:css-location} instance with the end location}
  @begin{short}
    Creates a new @class{gtk:css-section} instance referring to the section in
    the given file from the @arg{start} location to the @arg{end} location.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (file g:object)
  (start (:pointer (:struct css-location)))
  (end (:pointer (:struct css-location))))

(export 'css-section-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_ref ()
;;;
;;; GtkCssSection *
;;; gtk_css_section_ref (GtkCssSection *section);
;;;
;;; Increments the reference count on section .
;;;
;;; section :
;;;     a GtkCssSection
;;;
;;; Returns :
;;;     section itself.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_unref ()
;;;
;;; void
;;; gtk_css_section_unref (GtkCssSection *section);
;;;
;;; Decrements the reference count on section , freeing the structure if the
;;; reference count reaches 0.
;;;
;;; section :
;;;     a GtkCssSection
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_print ()
;;;
;;; void
;;; gtk_css_section_print (const GtkCssSection *section,
;;;                        GString *string);
;;;
;;; Prints the section into string in a human-readable form. This is a form like
;;; gtk.css:32:1-23 to denote line 32, characters 1 to 23 in the file gtk.css.
;;;
;;; section :
;;;     a section
;;;
;;; string :
;;;     a GString to print to
;;; ----------------------------------------------------------------------------

;; not needed, see gtk_css_section_to_string

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_to_string" css-section-to-string) :string
 #+liber-documentation
 "@version{#2022-8-20}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{A string with the human-readable text form.}
  @begin{short}
    Prints the section into a human-readable text form.
  @end{short}
  This is a form like @code{gtk.css:32:1-23} to denote line 32, characters 1
  to 23 in the @file{gtk.css} file.
  @see-class{gtk:css-section}"
  (section (g:boxed css-section)))

(export 'css-section-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file  -> css-section-file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_get_file" css-section-file) (g:object g:file)
 #+liber-documentation
 "@version{#2022-8-20}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The @class{g:file} object that @arg{section} was parsed from or
  @code{nil} if @arg{section} was parsed from other data.}
  @begin{short}
    Gets the file that the section was parsed from.
  @end{short}
  If no such file exists, for example because the CSS was loaded via the
  @fun{gtk:css-provider-load-from-data} function, then @code{nil} is returned.
  @see-class{gtk:css-section}
  @see-class{g:file}
  @see-function{gtk:css-provider-load-from-data}"
  (section (g:boxed css-section)))

(export 'css-section-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent  -> css-section-parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_parent" css-section-parent)
    (g:boxed css-section)
 #+liber-documentation
 "@version{#2022-8-20}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The @class{gtk:css-section} parent section, or @code{nil} if none.}
  @begin{short}
    Gets the parent section for the given section.
  @end{short}
  The parent section is the section that contains this section.
  @see-class{gtk:css-section}"
  (section (g:boxed css-section)))

(export 'css-section-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_location  -> css-section-start-location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_start_location" css-section-start-location)
    (:pointer (:struct css-location))
 #+liber-documentation
 "@version{2022-12-4}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{A @symbol{gtk:css-location} instance with the start location of the
    section.}
  @begin{short}
    Returns the location in the CSS document where this section starts.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (section (g:boxed css-section)))

(export 'css-section-start-location)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_location  -> css-section-end-location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_end_location" css-section-end-location)
    (:pointer (:struct css-location))
 #+liber-documentation
 "@version{2022-12-4}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{A @symbol{gtk:css-location} instance with the end location of the
    section.}
  @begin{short}
    Returns the location in the CSS document where this section ends.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (section (g:boxed css-section)))

(export 'css-section-end-location)

;;; --- End of file gtk4.css-provider.lisp -------------------------------------
