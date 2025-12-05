;;; ----------------------------------------------------------------------------
;;; gtk4.css-provider.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;;     GtkCssLocation
;;;     GtkCssSection
;;;
;;; Functions
;;;
;;;     gtk_css_section_new
;;;     gtk_css_section_new_with_bytes                      Since 4.16
;;;     gtk_css_section_ref                                 not needed
;;;     gtk_css_section_unref                               not needed
;;;     gtk_css_section_print                               not needed
;;;     gtk_css_section_to_string
;;;     gtk_css_section_get_bytes                           Since 4.16
;;;     gtk_css_section_get_file
;;;     gtk_css_section_get_parent
;;;     gtk_css_section_get_start_location
;;;     gtk_css_section_get_end_location
;;;
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkCssProvider
;;;     GtkInferfaceColorScheme                             Since 4.20
;;;     GtkInterfaceContrast                                Since 4.20
;;;
;;;     GtkCssParserError                                   not implemented
;;;     GtkCssParserWarning                                 not implemented
;;;
;;; Functions
;;;
;;;     gtk_css_provider_new
;;;     gtk_css_provider_to_string
;;;     gtk_css_provider_load_named                         Deprecated 4.20
;;;     gtk_css_provider_load_from_data                     Deprecated 4.12
;;;     gtk_css_provider_load_from_file
;;;     gtk_css_provider_load_from_path
;;;     gtk_css_provider_load_from_resource
;;;     gtk_css_provider_load_from_bytes                    Since 4.12
;;;     gtk_css_provider_load_from_string                   Since 4.12
;;;
;;; Properties
;;;
;;;     prefers-color-scheme                                Since 4.20
;;;     prefers-contrast                                    Since 4.20
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
;;; GtkInterfaceColorScheme
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(gobject:define-genum "GtkInterfaceColorScheme" interface-color-scheme
  (:export t
   :type-initializer "gtk_interface_color_scheme_get_type")
  (:unsupported 0)
  (:default 1)
  (:dark 2)
  (:light 3))

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-symbol 'interface-color-scheme)
      "GEnum"
      (liber:symbol-documentation 'interface-color-scheme)
 "@version{2025-11-08}
  @begin{declaration}
(gobject:define-genum \"GtkInterfaceColorScheme\" interface-color-scheme
  (:export t
   :type-initializer \"gtk_interface_color_scheme_get_type\")
  (:unsupported 0)
  (:default 1)
  (:dark 2)
  (:light 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unsupported]{The system does not support color schemes.}
      @entry[:default]{The default color scheme is used.}
      @entry[:dark]{A dark color scheme is used.}
      @entry[:light]{A light color scheme is used.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Values for the @slot[gtk:settings]{gtk-interface-color-scheme} and
    @slot[gtk:css-provider]{prefers-color-scheme} properties that indicates what
    color scheme is used.
  @end{short}
  This information can be used inside CSS via media queries.

  More values may be added to this enumeration. Unknown values should be treated
  the same as the @val[gtk:interface-color-scheme]{:default} value.

  Since 4.20
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-prefers-color-scheme}
  @see-function{gtk:settings-gtk-interface-color-scheme}")

;;; ----------------------------------------------------------------------------
;;; GtkInterfaceContrast
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(gobject:define-genum "GtkInterfaceContrast" interface-contrast
  (:export t
   :type-initializer "gtk_interface_contrast_get_type")
  (:unsupported 0)
  (:no-preference 1)
  (:more 2)
  (:less 3))

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-symbol 'interface-contrast)
      "GEnum"
      (liber:symbol-documentation 'interface-contrast)
 "@version{2025-11-08}
  @begin{declaration}
(gobject:define-genum \"GtkInterfaceContrast\" interface-contrast
  (:export t
   :type-initializer \"gtk_interface_contrast_get_type\")
  (:unsupported 0)
  (:no-preference 1)
  (:more 2)
  (:less 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unsupported]{The system does not support contrast levels.}
      @entry[:no-preference]{No particular preference for contrast.}
      @entry[:more]{More contrast is preferred.}
      @entry[:less]{Less contrast is preferred.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Values for the @slot[gtk:settings]{gtk-interface-contrast} and
    @slot[gtk:css-provider]{prefers-contrast} properties that indicates the
    preferred level of contrast.
  @end{short}
  This information can be used inside CSS via media queries.

  More values may be added to this enumeration. Unknown values should be treated
  the same as the @val[gtk:interface-contrast]{:no-preference} value.

  Since 4.20
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-prefers-contrast}
  @see-function{gtk:settings-gtk-interface-contrast}")

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
 "@version{2025-09-22}
  @begin{declaration}
(cffi:defcstruct css-location
  (bytes :size)
  (chars :size)
  (lines :size)
  (line-bytes :size)
  (line-chars :size))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[bytes]{Number of bytes parsed since the beginning.}
      @entry[chars]{Number of characters parsed since the beginning.}
      @entry[lines]{Number of full lines that have been parsed. If you want to
        display this as a line number, you need to add 1 to this.}
      @entry[line-bytes]{Number of bytes parsed since the last line break.}
      @entry[line-chars]{Number of characters parsed since the last line break.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{gtk:css-location} structure is used to present a location in a
    file or other source of data parsed by the CSS engine.
  @end{short}
  The @code{bytes} and @code{line-bytes} offsets are meant to be used to
  programmatically match data. The @code{lines} and @code{line-chars} offsets
  can be used for printing the location in a file.

  Note that the lines parameter starts from 0 and is increased whenever a CSS
  line break is encountered. CSS defines the C character sequences
  @code{\"\\r\\n\"}, @code{\"\\r\"}, @code{\"\\n\"} and @code{\"\\f\"} as
  newlines. If your document uses different rules for line breaking, you might
  want run into problems here. See the @class{gtk:css-section} documentation
  for an example.
  @see-slot{gtk:css-location-bytes}
  @see-slot{gtk:css-location-chars}
  @see-slot{gtk:css-location-lines}
  @see-slot{gtk:css-location-line-bytes}
  @see-slot{gtk:css-location-line-chars}
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
 "@version{2025-09-22}
  @syntax{(gtk:css-location-bytes location) => bytes}
  @argument[location]{a @sym{gtk:css-location} instance}
  @argument[bytes]{a number with the foreign type @code{:size}}
  @short{Returns the number of bytes parsed since the beginning.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'bytes))

(export 'css-location-bytes)

;;; --- gtk:css-location-chars -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-chars) "Accessor")

(defun css-location-chars (location)
 #+liber-documentation
 "@version{2025-09-22}
  @syntax{(gtk:css-location-chars location) => chars}
  @argument[location]{a @sym{gtk:css-location} instance}
  @argument[bytes]{a number with the foreign type @code{:size}}
  @short{Returns the number of characters parsed since the beginning.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'chars))

(export 'css-location-chars)

;;; --- gtk:css-location-lines -------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-lines) "Accessor")

(defun css-location-lines (location)
 #+liber-documentation
 "@version{2025-09-22}
  @syntax{(gtk:css-location-lines location) => lines}
  @argument[location]{a @sym{gtk:css-location} instance}
  @argument[lines]{a number with the foreign type @code{:size}}
  @short{Returns the number of full lines that have been parsed.}
  If you want to display this as a line number, you need to add 1 to this.
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'lines))

(export 'css-location-lines)

;;; --- gtk:css-location-line-bytes --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-line-bytes) "Accessor")

(defun css-location-line-bytes (location)
 "@version{2025-09-22}
  @syntax{(gtk:css-location-line-bytes location) => line-bytes}
  @argument[location]{a @sym{gtk:css-location} instance}
  @argument[line-bytes]{a number with the foreign type @code{:size}}
  @short{Returns the number of bytes parsed since the last line break.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'line-bytes))

(export 'css-location-line-bytes)

;;; --- gtk:css-location-line-chars --------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'css-location-line-chars) "Accessor")

(defun css-location-line-chars (location)
 "@version{2025-09-22}
  @syntax{(gtk:css-location-line-chars location) => line-chars}
  @argument[location]{a @sym{gtk:css-location} instance}
  @argument[line-chars]{a number with the foreign type @code{:size}}
  @short{Returns the number of characters parsed since the last line break.}
  @see-symbol{gtk:css-location}"
  (cffi:foreign-slot-value location '(:struct css-location) 'line-chars))

(export 'css-location-line-chars)

;;; ----------------------------------------------------------------------------
;;; GtkCssSection
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque css-section "GtkCssSection"
  :export t
  :type-initializer "gtk_css_section_get_type"
  :alloc (error "GtkCssSection cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'css-section)
      "GBoxed"
      (documentation 'css-section 'type)
 "@version{2025-09-22}
  @begin{declaration}
(glib:define-gboxed-opaque css-section \"GtkCssSection\"
  :export t
  :type-initializer \"gtk_css_section_get_type\"
  :alloc (error \"GtkCssSection cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    Defines a part of a CSS document.
  @end{short}
  Because sections are nested into one another, you can use the
  @fun{gtk:css-section-parent} function to get the containing region.
  @begin[Examples]{dictionary}
    Use the @sym{gtk:css-location} and @sym{gtk:css-section} structures in a
    @sig[gtk:css-provider]{parsing-error} handler to mark an error in
    the parsed CSS file, that is loaded into the text buffer.
    @begin{pre}
(g:signal-connect provider \"parsing-error\"
        (lambda (provider section err)
          (declare (ignore provider err))
          (let* ((startloc (gtk:css-section-start-location section))
                 (start (gtk:text-buffer-iter-at-line-index
                                text
                                (gtk:css-location-lines startloc)
                                (gtk:css-location-line-bytes startloc)))
                 (endloc (gtk:css-section-end-location section))
                 (end (gtk:text-buffer-iter-at-line-index
                              text
                              (gtk:css-location-lines endloc)
                              (gtk:css-location-line-bytes endloc))))
            (gtk:text-buffer-apply-tag text \"error\" start end)
            gdk:+event-stop+)))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:css-section-new}
  @see-constructor{gtk:css-section-new-with-bytes}
  @see-class{gtk:css-provider}
  @see-class{gtk:location}")

(export 'css-section)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_new" css-section-new)
    (g:boxed css-section :return)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[path]{a pathname or namestring for the file this section refers to}
  @argument[start]{a @sym{gtk:css-location} instance for the start location}
  @argument[end]{a @sym{gtk:css-location} instance for the end location}
  @return{The new @class{gtk:css-section} instance.}
  @begin{short}
    Creates a new @class{gtk:css-section} instance referring to the section in
    the given @arg{path} from the @arg{start} location to the @arg{end}
    location.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (path g:file-as-namestring)
  (start (:pointer (:struct css-location)))
  (end (:pointer (:struct css-location))))

(export 'css-section-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_new_with_bytes                          Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_css_section_new_with_bytes" css-section-new-with-bytes)
    (g:boxed css-section :return)
 #+liber-documentation
 "@version{2025-09-22}
  @argument[path]{a pathname or namestring for the file this section refers to}
  @argument[bytes]{a @class{g:bytes} instance for the bytes this section
    refers to}
  @argument[start]{a @sym{gtk:css-location} instance for the start location}
  @argument[end]{a @sym{gtk:css-location} instance for the end location}
  @return{The new @class{gtk:css-section} instance.}
  @begin{short}
    Creates a new @class{gtk:css-section} instance referring to the section in
    the given @arg{path} or the given @arg{bytes} from the @arg{start} location
    to the @arg{end} location.
  @end{short}

  Since 4.16
  @see-class{gtk:css-section}
  @see-class{gtk:css-location}
  @see-class{g:bytes}"
  (path g:file-as-namestring)
  (bytes (g:boxed g:bytes))
  (start (:pointer (:struct css-location)))
  (end (:pointer (:struct css-location))))

#+gtk-4-16
(export 'css-section-new-with-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_ref                                     not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_unref                                   not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_print                                   not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_to_string" css-section-to-string) :string
 #+liber-documentation
 "@version{2025-07-25}
  @argument[section]{a @class{gtk:css-section} instance}
  @return{The string for the human-readable text form.}
  @begin{short}
    Prints the section into a human-readable text form.
  @end{short}
  This is a form like @code{gtk.css:32:1-23} to denote line 32, characters 1
  to 23 in the @file{gtk.css} file.
  @see-class{gtk:css-section}"
  (section (g:boxed css-section)))

(export 'css-section-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_bytes                               Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_css_section_get_bytes" css-section-bytes) (g:boxed g:bytes)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[section]{a @class{gtk:css-section} instance}
  @begin{return}
    The @class{g:bytes} instance for the bytes from which @arg{section} was
    parsed.
  @end{return}
  @begin{short}
    Gets the bytes that the section was parsed from.
  @end{short}

  Since 4.16
  @see-class{gtk:css-section}
  @see-class{g:bytes}"
  (section (g:boxed css-section)))

#+gtk-4-16
(export 'css-section-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_file" css-section-file)
    g:file-as-namestring
 #+liber-documentation
 "@version{2025-05-10}
  @argument[section]{a @class{gtk:css-section} instance}
  @begin{return}
    The namestring for the file that @arg{section} was parsed from or
    @code{nil} if @arg{section} was parsed from other data.
  @end{return}
  @begin{short}
    Gets the file that the section was parsed from.
  @end{short}
  If no such file exists, for example because the CSS was loaded via the
  @fun{gtk:css-provider-load-from-string} function, then @code{nil} is returned.
  @see-class{gtk:css-section}
  @see-function{gtk:css-provider-load-from-string}"
  (section (g:boxed css-section)))

(export 'css-section-file)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_parent
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_parent" css-section-parent)
    (g:boxed css-section)
 #+liber-documentation
 "@version{2025-09-22}
  @argument[section]{a @class{gtk:css-section} instance}
  @begin{return}
    The @class{gtk:css-section} instance for the parent section, or @code{nil}
    if none.
  @end{return}
  @begin{short}
    Gets the parent section for the given section.
  @end{short}
  The parent section is the section that contains this section.
  @see-class{gtk:css-section}"
  (section (g:boxed css-section)))

(export 'css-section-parent)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_start_location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_start_location" css-section-start-location)
    (:pointer (:struct css-location))
 #+liber-documentation
 "@version{2025-07-26}
  @argument[section]{a @class{gtk:css-section} instance}
  @begin{return}
    The @sym{gtk:css-location} instance for the start location of the section.
  @end{return}
  @begin{short}
    Returns the location in the CSS document where this section starts.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (section (g:boxed css-section)))

(export 'css-section-start-location)

;;; ----------------------------------------------------------------------------
;;; gtk_css_section_get_end_location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_section_get_end_location" css-section-end-location)
    (:pointer (:struct css-location))
 #+liber-documentation
 "@version{2025-07-26}
  @argument[section]{a @class{gtk:css-section} instance}
  @begin{return}
    The @sym{gtk:css-location} instance for the end location of the section.
  @end{return}
  @begin{short}
    Returns the location in the CSS document where this section ends.
  @end{short}
  @see-class{gtk:css-section}
  @see-symbol{gtk:css-location}"
  (section (g:boxed css-section)))

(export 'css-section-end-location)

;;; ----------------------------------------------------------------------------
;;; GtkCssProvider
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCssProvider" css-provider
  (:superclass g:object
   :export t
   :interfaces ("GtkStyleProvider")
   :type-initializer "gtk_css_provider_get_type")
  (#+gtk-4-20
   (prefers-color-scheme
    css-provider-prefers-color-scheme
    "prefers-color-scheme" "GtkInterfaceColorScheme" t t)
   #+gtk-4-20
   (prefers-contrast
    css-provider-prefers-contrast
    "prefers-contrast" "GtkInterfaceContrast" t t)))

#+liber-documentation
(setf (documentation 'css-provider 'type)
 "@version{2025-11-08}
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
  @slot[gtk:settings]{gtk-icon-theme-name} setting, @code{VARIANT} is the
  variant to load, see the
  @slot[gtk:settings]{gtk-application-prefer-dark-theme} setting,
  @code{DATADIR} is the prefix configured when GTK was compiled (unless
  overridden by the @code{GTK_DATA_PREFIX} environment variable), and
  @code{VERSION} is the GTK version number. If no file is found for the current
  version, GTK tries older versions all the way back to 4.0.

  To track errors while loading CSS, connect to the
  @sig[gtk:css-provider]{parsing-error} signal.
  @begin[Signal Details]{dictionary}
    @begin[css-provider::parsing-error]{signal}
      @begin{pre}
lambda (provider section error)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[provider]{The @class{gtk:css-provider} object that had a parsing
          error.}
        @entry[section]{The @class{gtk:css-section} instance the error happened
          in.}
        @entry[error]{The @class{glib:error} instance for the parsing error.}
      @end{simple-table}
      Signals that a parsing error occured. The @arg{section} argument describes
      the actual location of the error as accurately as possible. Parsing errors
      are never fatal, so the parsing will resume after the error. Errors may
      however cause parts of the given data or even all of it to not be parsed
      at all. So it is a useful idea to check that the parsing succeeds by
      connecting to this signal.

      Note that this signal may be emitted at any time as the CSS provider may
      opt to defer parsing parts or all of the input to a later time than when
      a loading function was called.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:css-provider-new}
  @see-slot{gtk:css-provider-prefers-color-scheme}
  @see-slot{gtk:css-provider-prefers-contrast}
  @see-class{gtk:style-provider}
  @see-class{gtk:css-section}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:css-provider-prefers-color-scheme ----------------------------------

#+(and gtk-4-20 liber-documentation)
(setf (documentation (liber:slot-documentation "prefers-color-scheme"
                                               'css-provider) t)
 "The @code{prefers-color-scheme} property of type
  @sym{gtk:interface-color-scheme} (Read / Write) @br{}
  Defines the color scheme used for rendering the user interface. @br{}
  Default value: @val[gtk:interface-color-scheme]{:default}")

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-function 'css-provider-prefers-color-scheme)
      "Accessor"
      (documentation 'css-provider-prefers-color-scheme 'function)
 "@version{2025-11-08}
  @syntax{(gtk:css-provider-prefers-color-scheme object) => mode}
  @syntax{(setf (gtk:css-provider-prefers-color-scheme object) mode)}
  @argument[object]{a @class{gtk:css-provider} object}
  @argument[mode]{a @sym{gtk:instance-color-scheme} value}
  @begin{short}
    The accessor for the @slot[gtk:css-provider]{prefers-color-scheme} slot
    of the @class{gtk:css-provider} class gets or sets the color scheme used for
    rendering the user interface.
  @end{short}
  The UI can be set to either @val[gtk:interface-color-scheme]{:light} or
  @val[gtk:interface-color-scheme]{:dark} mode. Other values will be interpreted
  the same as the @val[gtk:interface-color-scheme]{:light} value.

  This setting is be available for media queries in CSS:
  @begin{pre}
@@media (prefers-color-scheme: dark) {
  // some dark mode styling
@}
  @end{pre}
  Changing this setting will reload the style sheet.

  Since 4.20
  @see-class{gtk:css-provider}
  @see-symbol{gtk:interface-color-scheme}")

;;; --- gtk:css-provider-prefers-contrast --------------------------------------

#+(and gtk-4-20 liber-documentation)
(setf (documentation (liber:slot-documentation "prefers-contrast"
                                               'css-provider) t)
 "The @code{prefers-contrast} property of type @sym{gtk:interface-contrast}
  (Read / Write) @br{}
  Defines the contrast mode to use for the user interface. @br{}
  Default value: @val[gtk:interface-contrast]{:no-preferences}")

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-function 'css-provider-prefers-contrast)
      "Accessor"
      (documentation 'css-provider-prefers-contrast 'function)
 "@version{2025-11-08}
  @syntax{(gtk:css-provider-prefers-color-scheme object) => mode}
  @syntax{(setf (gtk:css-provider-prefers-color-scheme object) mode)}
  @argument[object]{a @class{gtk:css-provider} object}
  @argument[mode]{a @sym{gtk:instance-contrast} value}
  @begin{short}
    The accessor for the @slot[gtk:css-provider]{prefers-contrast} slot
    of the @class{gtk:css-provider} class gets or sets the contrast mode to use
    for the user interface.
  @end{short}

  When set to @val[gtk:interface-contrast]{:more} or
  @val[gtk:interface-contrast]{:less}, the UI is rendered in high or low
  contrast. When set to @val[gtk:interface-contrast]{:no-preference} (the
  default), the user interface will be rendered in default mode.

  This setting is be available for media queries in CSS:
  @begin{pre}
@@media (prefers-contrast: more) {
  // some style with high contrast
@}
  @end{pre}
  Changing this setting will reload the style sheet.

  Since 4.20
  @see-class{gtk:css-provider}
  @see-symbol{gtk:interface-contrast}")

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_new
;;; ----------------------------------------------------------------------------

(declaim (inline css-provider-new))

(defun css-provider-new ()
 #+liber-documentation
 "@version{2025-01-11}
  @return{The new @class{gtk:css-provider} object.}
  @short{Returns a newly created CSS provider object.}
  @see-class{gtk:css-provider}"
  (make-instance 'css-provider))

(export 'css-provider-new)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_to_string" css-provider-to-string) :string
 #+liber-documentation
 "@version{2025-02-25}
  @argument[provider]{a @class{gtk:css-provider} object to write to a string}
  @return{The string representing the CSS provider.}
  @begin{short}
    Convertes the CSS provider into a string representation in CSS format.
  @end{short}
  Using the @fun{gtk:css-provider-load-from-string} function with the return
  value from this function on a new CSS provider created with the
  @fun{gtk:css-provider-new} function will basically create a duplicate of this
  provider.
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-new}
  @see-function{gtk:css-provider-load-from-string}"
  (provider (g:object css-provider)))

(export 'css-provider-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_named
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_named" %css-provider-load-named) :void
  (provider (g:object css-provider))
  (name :string)
  (variant :string))

(defun css-provider-load-named (provider name &optional variant)
 #+liber-documentation
 "@version{2025-11-08}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[name]{a string for the theme name}
  @argument[variant]{an optional string for a variant to load, for example,
    a \"dark\" variant}
  @begin{short}
    Loads a theme from the usual theme paths.
  @end{short}
  The actual process of finding the theme might change between releases, but it
  is guaranteed that this function uses the same mechanism to load the theme
  that GTK uses for loading its own theme.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20. Using any of the other theme
    loaders, combine with media queries.
  @end{dictionary}
  @see-class{gtk:css-provider}"
  (%css-provider-load-named provider
                            name
                            (or variant (cffi:null-pointer))))

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
 "@version{2025-09-22}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[data]{a string for the CSS data}
  @begin{short}
    Loads data into the CSS provider, making it clear any previously loaded
    information.
  @end{short}
  To track errors while loading CSS, connect to the
  @sig[gtk:css-provider]{parsing-error} signal of the @class{gtk:css-provider}
  object.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:css-provider-load-from-string} or
    @fun{gtk:css-provider-load-from-bytes} functions instead.
  @end{dictionary}
  @see-class{gtk:css-provider}
  @see-function{gtk:css-provider-load-from-string}
  @see-function{gtk:css-provider-load-from-bytes}"
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
 "@version{2025-07-05}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[file]{a @class{g:file} object pointing to a file to load}
  @begin{short}
    Loads the data contained in @arg{file} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the
  @sig[gtk:css-provider]{parsing-error} signal of the @class{gtk:css-provider}
  object.
  @see-class{gtk:css-provider}
  @see-class{g:file}"
  (provider (g:object css-provider))
  (file g:object))

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
 "@version{2025-07-05}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a pathname or namestring for the path of a file to load, in
    the GLib filename encoding}
  @begin{short}
    Loads the data contained in @arg{path} into the CSS provider, making it
    clear any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the
  @sig[gtk:css-provider]{parsing-error} signal of the @class{gtk:css-provider}
  object.
  @see-class{gtk:css-provider}"
  (%css-provider-load-from-path provider (namestring path)))

(export 'css-provider-load-from-path)

;;; ----------------------------------------------------------------------------
;;; gtk_css_provider_load_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_css_provider_load_from_resource"
               css-provider-load-from-resource) :void
 #+liber-documentation
 "@version{2025-07-05}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[path]{a string for the resource path}
  @begin{short}
    Loads the data contained in the resource at @arg{path} into the CSS
    provider, clearing any previously loaded information.
  @end{short}
  To track errors while loading CSS, connect to the
  @sig[gtk:css-provider]{parsing-error} signal of the @class{gtk:css-provider}
  object.
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
 "@version{2025-01-11}
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
 "@version{2025-01-11}
  @argument[provider]{a @class{gtk:css-provider} object}
  @argument[str]{a string for the CSS data to load}
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

;;; --- End of file gtk4.css-provider.lisp -------------------------------------
