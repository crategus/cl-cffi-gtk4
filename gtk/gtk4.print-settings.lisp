;;; ----------------------------------------------------------------------------
;;; gtk4.print-settings.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkPrintSettings
;;;
;;;     Stores print settings
;;;
;;; Types and Values
;;;
;;;     GtkPrintSettings
;;;
;;;     GTK_PRINT_SETTINGS_PRINTER
;;;
;;;     GtkPageOrientation
;;;     GTK_PRINT_SETTINGS_ORIENTATION
;;;     GTK_PRINT_SETTINGS_PAPER_FORMAT
;;;     GTK_PRINT_SETTINGS_PAPER_WIDTH
;;;     GTK_PRINT_SETTINGS_PAPER_HEIGHT
;;;     GTK_PRINT_SETTINGS_USE_COLOR
;;;     GTK_PRINT_SETTINGS_COLLATE
;;;     GTK_PRINT_SETTINGS_REVERSE
;;;
;;;     GtkPrintDuplex
;;;     GTK_PRINT_SETTINGS_DUPLEX
;;;
;;;     GtkPrintQuality
;;;     GTK_PRINT_SETTINGS_QUALITY
;;;     GTK_PRINT_SETTINGS_N_COPIES
;;;     GTK_PRINT_SETTINGS_NUMBER_UP
;;;
;;;     GtkNumberUpLayout
;;;     GTK_PRINT_SETTINGS_NUMBER_UP_LAYOUT
;;;     GTK_PRINT_SETTINGS_RESOLUTION
;;;     GTK_PRINT_SETTINGS_RESOLUTION_X
;;;     GTK_PRINT_SETTINGS_RESOLUTION_Y
;;;     GTK_PRINT_SETTINGS_PRINTER_LPI
;;;     GTK_PRINT_SETTINGS_SCALE
;;;
;;;     GtkPrintPages
;;;     GTK_PRINT_SETTINGS_PRINT_PAGES
;;;
;;;     GtkPageRange
;;;     GTK_PRINT_SETTINGS_PAGE_RANGES
;;;
;;;     GtkPageSet
;;;     GTK_PRINT_SETTINGS_PAGE_SET
;;;     GTK_PRINT_SETTINGS_DEFAULT_SOURCE
;;;     GTK_PRINT_SETTINGS_MEDIA_TYPE
;;;     GTK_PRINT_SETTINGS_DITHER
;;;     GTK_PRINT_SETTINGS_FINISHINGS
;;;     GTK_PRINT_SETTINGS_OUTPUT_BIN
;;;     GTK_PRINT_SETTINGS_OUTPUT_DIR
;;;     GTK_PRINT_SETTINGS_OUTPUT_BASENAME
;;;     GTK_PRINT_SETTINGS_OUTPUT_FILE_FORMAT
;;;     GTK_PRINT_SETTINGS_OUTPUT_URI
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_EXTRA
;;;     GTK_PRINT_SETTINGS_WIN32_DRIVER_VERSION
;;;
;;; Functions
;;;
;;;     GtkPrintSettingsFunc
;;;
;;;     gtk_print_settings_new
;;;     gtk_print_settings_copy
;;;     gtk_print_settings_has_key
;;;     gtk_print_settings_get
;;;     gtk_print_settings_set
;;;     gtk_print_settings_unset
;;;     gtk_print_settings_foreach
;;;     gtk_print_settings_get_bool
;;;     gtk_print_settings_set_bool
;;;     gtk_print_settings_get_double
;;;     gtk_print_settings_get_double_with_default
;;;     gtk_print_settings_set_double
;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length
;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int
;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer
;;;     gtk_print_settings_get_orientation
;;;     gtk_print_settings_set_orientation
;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size
;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width
;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height
;;;     gtk_print_settings_get_use_color
;;;     gtk_print_settings_set_use_color
;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate
;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse
;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex
;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality
;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies
;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up
;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout
;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy
;;;     gtk_print_settings_get_resolution_x
;;;     gtk_print_settings_get_resolution_y
;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages
;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges
;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin
;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_new_from_gvariant
;;;     gtk_print_settings_load_file
;;;     gtk_print_settings_load_key_file
;;;     gtk_print_settings_to_file
;;;     gtk_print_settings_to_key_file
;;;     gtk_print_settings_to_gvariant
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintSettings
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettings
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPrintSettings" print-settings
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_print_settings_get_type")
  nil)

#+liber-documentation
(setf (documentation 'print-settings 'type)
 "@version{#2020-4-5}
  @begin{short}
    A @sym{gtk:print-settings} object represents the settings of a print dialog
    in a system independent way.
  @end{short}
  The main use for this object is that once you have printed you can get a
  settings object that represents the settings the user chose, and the next time
  you print you can pass that object in so that the user does not have to
  re-set all his settings.

  Its also possible to enumerate the settings so that you can easily save the
  settings for the next time your app runs, or even store them in a document.
  The predefined keys try to use shared values as much as possible so that
  moving such a document between systems still works.

  The list of keys for a print setting:
  @begin{pre}
\"printer\"                \"orientation\"              \"paper-format\"
\"paper-width\"            \"paper-height\"             \"use-color\"
\"collate\"                \"reverse\"                  \"duplex\"
\"quality\"                \"n-copies\"                 \"number-up\"
\"number-up-layout\"       \"resolution\"               \"resolution-x\"
\"resolution-y\"           \"printer-lpi\"              \"scale\"
\"print-pages\"            \"page-ranges\"              \"page-set\"
\"default-source\"         \"media-type\"               \"dither\"
\"finishings\"             \"output-bin\"               \"output-dir\"
\"output-basename\"        \"output-file-format\"       \"output-uri\"
\"win32-driver-extra\"     \"win32-driver-version\"
  @end{pre}
  @see-class{gtk:print-operation}
  @see-class{gtk:print-unix-dialog}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPageOrientation
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPageOrientation" page-orientation
  (:export t
   :type-initializer "gtk_page_orientation_get_type")
  :portrait
  :landscape
  :reverse-portrait
  :reverse-landscape)

#+liber-documentation
(setf (liber:alias-for-symbol 'page-orientation)
      "GEnum"
      (liber:symbol-documentation 'page-orientation)
 "@version{#2020-4-5}
  @short{See the function @fun{gtk:print-settings-orientation}.}
  @begin{pre}
(define-g-enum \"GtkPageOrienation\" page-orientation
  (:export t
   :type-initializer \"gtk_page_orientation_get_type\")
  :portrait
  :landscape
  :reverse-portrait
  :reverse-landscape)
  @end{pre}
  @begin[code]{table}
    @entry[:portrait]{Portrait mode.}
    @entry[:landscape]{Landscape mode.}
    @entry[:reverse-portrait]{Reverse portrait mode.}
    @entry[:reverse-landscape]{Reverse landscape mode.}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-orientation}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintDuplex
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintDuplex" print-duplex
  (:export t
   :type-initializer "gtk_print_duplex_get_type")
  :simplex
  :horizontal
  :vertical)

#+liber-documentation
(setf (liber:alias-for-symbol 'print-duplex)
      "GEnum"
      (liber:symbol-documentation 'print-duplex)
 "@version{#2020-4-5}
  @short{See the function @fun{gtk:print-settings-duplex}.}
  @begin{pre}
(define-g-enum \"GtkPrintDuplex\" print-duplex
  (:export t
   :type-initializer \"gtk_print_duplex_get_type\")
  :simplex
  :horizontal
  :vertical)
  @end{pre}
  @begin[code]{table}
    @entry[:simplex]{No duplex.}
    @entry[:horizontal]{Horizontal duplex.}
    @entry[:vertical]{Vertical duplex.}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-duplex}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintQuality
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintQuality" print-quality
  (:export t
   :type-initializer "gtk_print_quality_get_type")
  :low
  :normal
  :high
  :draft)

#+liber-documentation
(setf (liber:alias-for-symbol 'print-quality)
      "GEnum"
      (liber:symbol-documentation 'print-quality)
 "@version{#2020-4-5}
  @short{See the function @fun{gtk:print-settings-quality}.}
  @begin{pre}
(define-g-enum \"GtkPrintQuality\" print-quality
  (:export t
   :type-initializer \"gtk_print_quality_get_type\")
  :low
  :normal
  :high
  :draft)
  @end{pre}
  @begin[code]{table}
    @entry[:low]{Low quality.}
    @entry[:normal]{Normal quality.}
    @entry[:high]{High quality.}
    @entry[:draft]{Draft quality.}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-quality}")

;;; ----------------------------------------------------------------------------
;;; enum GtkNumberUpLayout
;;; ----------------------------------------------------------------------------

;; TODO: Change the nick names to the short form

(define-g-enum "GtkNumberUpLayout" number-up-layout
  (:export t
   :type-initializer "gtk_number_up_layout_get_type")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'number-up-layout)
      "GEnum"
      (liber:symbol-documentation 'number-up-layout)
 "@version{#2020-4-5}
  @begin{short}
    Used to determine the layout of pages on a sheet when printing multiple
    pages per sheet.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkNubmerUpLayout\" number-up-layout
  (:export t
   :type-initializer \"gtk_number_up_layout_get_type\")
  (:left-to-right-top-to-bottom 0)
  (:left-to-right-bottom-to-top 1)
  (:right-to-left-bottom-to-top 2)
  (:right-to-left-top-to-bottom 3)
  (:top-to-bottom-left-to-right 4)
  (:top-to-bottom-right-to-left 5)
  (:bottom-to-top-left-to-right 6)
  (:bottom-to-top-right-to-left 7))
  @end{pre}
  @begin[code]{table}
    @entry[:left-to-right-top-to-bottom]{@image[layout-lrtb]{}}
    @entry[:left-to-right-bottom-to-top]{@image[layout-lrbt]{}}
    @entry[:right-to-left-bottom-to-top]{@image[layout-rlbt]{}}
    @entry[:right-to-left-top-to-bottom]{@image[layout-rltb]{}}
    @entry[:top-to-bottom-left-to-right]{@image[layout-tblr]{}}
    @entry[:top-to-bottom-right-to-left]{@image[layout-tbrl]{}}
    @entry[:bottom-to-top-left-to-right]{@image[layout-btlr]{}}
    @entry[:bottom-to-top-right-to-left]{@image[layout-btrl]{}}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-number-up-layout}")

;;; ----------------------------------------------------------------------------
;;; enum GtkPrintPages
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPrintPages" print-pages
  (:export t
   :type-initializer "gtk_print_pages_get_type")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-pages)
      "GEnum"
      (liber:symbol-documentation 'print-pages)
 "@version{#2020-4-5}
  @begin{short}
    See the functions @fun{gtk:print-job-pages} and
    @fun{gtk:print-settings-print-pages}.
  @end{short}
  @begin{pre}
(define-g-enum \"GtkPrintPages\" print-pages
  (:export t
   :type-initializer \"gtk_print_pages_get_type\")
  (:all 0)
  (:current 1)
  (:ranges 2)
  (:selection 3))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All pages.}
    @entry[:current]{Current page.}
    @entry[:ranges]{Range of pages.}
    @entry[:selection]{Selected pages.}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-print-pages}
  @see-function{gtk:print-job-pages}")

;;; ----------------------------------------------------------------------------
;;; struct GtkPageRange
;;;
;;; struct GtkPageRange {
;;;   gint start;
;;;   gint end;
;;; };
;;;
;;; See also gtk_print_settings_set_page_ranges().
;;;
;;; gint start;
;;;     start of page range.
;;;
;;; gint end;
;;;     end of page range.
;;; ----------------------------------------------------------------------------

;; Not implemented.
;; We handle page ranges as a list of lists, e.g. '((1) (15 20) 25).
;; The string representation of this is "pages-ranges=1, 15-20, 25"

;;; ----------------------------------------------------------------------------
;;; enum GtkPageSet
;;; ----------------------------------------------------------------------------

(define-g-enum "GtkPageSet" page-set
  (:export t
   :type-initializer "gtk_page_set_get_type")
  (:all 0)
  (:even 1)
  (:odd 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'page-set)
      "GEnum"
      (liber:symbol-documentation 'page-set)
 "@version{#2020-4-5}
  @short{See the function @fun{gtk:print-job-page-set}.}
  @begin{pre}
(define-g-enum \"GtkPageSet\" page-set
  (:export t
   :type-initializer \"gtk_page_set_get_type\")
  (:all 0)
  (:even 1)
  (:odd 2))
  @end{pre}
  @begin[code]{table}
    @entry[:all]{All pages.}
    @entry[:even]{Even pages.}
    @entry[:odd]{Odd pages.}
  @end{table}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-job-page-set}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline print-settings-new))

(defun print-settings-new ()
 #+liber-documentation
 "@version{#2020-4-5}
  @return{A new @class{gtk:print-settings} object.}
  @short{Creates a new @class{gtk:print-settings} object.}
  @see-class{gtk:print-settings}"
  (make-instance 'print-settings))

(export 'print-settings-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_copy" print-settings-copy)
    (g:object print-settings)
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[other]{a @class{gtk:print-settings} object}
  @return{A newly allocated copy of @arg{other}.}
  @short{Copies a @class{gtk:print-settings} object.}
  @see-class{gtk:print-settings}"
  (other (g:object print-settings)))

(export 'print-settings-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_has_key ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_has_key" print-settings-has-key) :boolean
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @return{@em{True}, if @arg{key} has a value.}
  @short{Returns @em{true}, if a value is associated with @arg{key}.}
  @see-class{gtk:print-settings}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-has-key)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get" print-settings-get) :string
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @return{The string value for @arg{key}.}
  @short{Looks up the string value associated with @arg{key}.}
  @see-class{gtk:print-settings}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-get)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_set" %print-settings-set) :void
  (settings (g:object print-settings))
  (key :string)
  (value :string))

(defun print-settings-set (settings key value)
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[value]{a string with a value, or @code{nil}}
  @short{Associates @arg{value} with @arg{key}.}

  If @arg{value} is @code{nil} removes any value associated with @arg{key}. This
  has the same effect as using the function @func{gtk:print-settings-unset}.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-unset}"
  (%print-settings-set settings key (if value value (null-pointer))))

(export 'print-settings-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_unset ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_unset" print-settings-unset) :void
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @begin{short}
    Removes any value associated with @arg{key}.
  @end{short}
  This has the same effect as setting the value to @code{nil}.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-unset)

;;; ----------------------------------------------------------------------------
;;; GtkPrintSettingsFunc ()
;;; ----------------------------------------------------------------------------

(defcallback print-settings-func :void
    ((key :string)
     (value :string)
     (data :pointer))
  (restart-case
      (funcall (glib:get-stable-pointer-value data) key value)
    (return () nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-settings-func)
      "Callback"
      (liber:symbol-documentation 'print-settings-func)
 "@version{#2022-7-24}
  @begin{short}
    No description available.
  @end{short}
  @begin{pre}
lambda (key value)
  @end{pre}
  @begin[code]{table}
    @entry[key]{A string.}
    @entry[value]{A string.}
  @end{table}
  @see-class{gtk:print-settings}")

(export 'print-settings-func)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_foreach" %print-settings-foreach) :void
  (settings (g:object print-settings))
  (callback :pointer)
  (data :pointer))

(defun print-settings-foreach (settings func)
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[func]{the function to call}
  @begin{short}
    Calls @arg{func} for each key value pair of the print settings.
  @end{short}
  @see-class{gtk:print-settings}"
  (with-stable-pointer (ptr func)
    (%print-settings-foreach settings
                                 (callback print-settings-func)
                                 ptr)))

(export 'print-settings-foreach)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_bool ()
;;; gtk_print_settings_set_bool ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-bool) (value settings key)
  (foreign-funcall "gtk_print_settings_set_bool"
                   (g:object print-settings) settings
                   :string key
                   :boolean value
                   :void)
  value)

(defcfun ("gtk_print_settings_get_bool" print-settings-bool) :boolean
 #+liber-documentation
 "@version{#2020-4-12}
  @syntax[]{(gtk:print-settings-bool settings key) => value}
  @syntax[]{(setf (gtk:print-settings-bool settings key) value)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[value]{a boolean}
  @begin{short}
    Accessor of the boolean value of a key in a print setting.
  @end{short}

  The function @sym{gtk:print-settings-bool} returns the boolean represented
  by the value that is associated with @arg{key}. The function
  @sym{(setf gtk:print-settings-bool)} sets @arg{key} to a boolean value.

  The string \"true\" represents @em{true}, any other string @em{false}.
  @see-class{gtk:print-settings}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-bool)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double ()
;;; gtk_print_settings_set_double ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-double) (value settings key)
  (foreign-funcall "gtk_print_settings_set_double"
                   (g:object print-settings) settings
                   :string key
                   :double (coerce value 'double-float)
                   :void)
  value)

(defcfun ("gtk_print_settings_get_double" print-settings-double) :double
 #+liber-documentation
 "@version{#2020-11-25}
  @syntax[]{(gtk:print-settings-double settings key) => value}
  @syntax[]{(setf (gtk:print-settings-double settings key) value)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[value]{a double float value}
  @begin{short}
    Accessor of the double float value of a key in a print setting.
  @end{short}

  The function @sym{gtk:print-settings-double} gets the double float value of
  @arg{key}, or 0. The function @sym{(setf gtk:print-settings-double)}
  sets @arg{key} to a double float value.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-double-with-default}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-double)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_double_with_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_double_with_default"
          %print-settings-double-with-default) :double
  (settings (g:object print-settings))
  (key :string)
  (default :double))

(defun print-settings-double-with-default (settings key default)
 #+liber-documentation
 "@version{#2020-4-13}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[default]{a number with the default value}
  @return{The floating point number associated with @arg{key}.}
  @begin{short}
    Returns the floating point number represented by the value that is
    associated with @arg{key}, or @arg{default} if the value does not represent
    a floating point number.
  @end{short}

  Floating point numbers are parsed with @code{g_ascii_strtod()}.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-double}"
  (%print-settings-double-with-default settings
                                           key
                                           (coerce default 'double-float)))

(export 'print-settings-double-with-default)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_length ()
;;; gtk_print_settings_set_length ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-length) (value settings key unit)
  (foreign-funcall "gtk_print_settings_set_length"
                   (g:object print-settings) settings
                   :string key
                   :double (coerce value 'double-float)
                   unit unit
                   :void)
  value)

(defcfun ("gtk_print_settings_get_length" print-settings-length) :double
 #+liber-documentation
 "@version{#2020-4-13}
  @syntax[]{(gtk:print-settings-length settings key unit) => value}
  @syntax[]{(setf (gtk:print-settings-length settings key unit) value)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[unit]{the unit of type @symbol{gtk:unit} of the return value}
  @argument[value]{a number with the length}
  @begin{short}
    Accessor of the length value of a key in a print setting.
  @end{short}

  The function @sym{gtk:print-settings-length} returns the length value of
  @arg{key}, converted to @arg{unit}. The function
  @sym{(setf gtk:print-settings-length)} associates a length in units of
  @arg{unit} with @arg{key}.
  @begin[Example]{dictionary}
    @begin{pre}
 (setq settings (make-instance 'gtk:print-settings))
=> #<gtk:print-settings {1004A34623@}>
 (setf (gtk:print-settings-length settings \"paper-width\" :mm) 100.0d0)
 (gtk:print-settings-length settings \"paper-width\" :mm)
=> 100.0d0
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-settings}
  @see-symbol{gtk:unit}"
  (settings (g:object print-settings))
  (key :string)
  (unit unit))

(export 'print-settings-length)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int ()
;;; gtk_print_settings_set_int ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-int) (value settings key)
  (foreign-funcall "gtk_print_settings_set_int"
                   (g:object print-settings) settings
                   :string key
                   :int value)
  value)

(defcfun ("gtk_print_settings_get_int" print-settings-int) :int
 #+liber-documentation
 "@version{#2020-4-13}
  @syntax[]{(gtk:print-settings-int settings key) => value}
  @syntax[]{(setf (gtk:print-settings-int settings key) value)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[value]{an integer}
  @begin{short}
    Accessor of the integer value of a key in a print setting.
  @end{short}

  The function @sym{gtk:print-settings-int} returns the integer value of
  @arg{key}, or 0. The function @sym{(setf gtk:print-settings-int)} sets
  @arg{key} to an integer value.
  @see-class{gtk:print-settings}
  @see-class{gtk:print-settings-int-with-default}"
  (settings (g:object print-settings))
  (key :string))

(export 'print-settings-int)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_int_with_default ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_int_with_default"
           print-settings-int-with-default) :int
 #+liber-documentation
 "@version{#2020-4-5}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[key]{a string with a key}
  @argument[default]{an integer with the default value}
  @return{The integer value of @arg{key}.}
  @begin{short}
    Returns the value of @arg{key}, interpreted as an integer, or the default
    value.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-int}"
  (settings (g:object print-settings))
  (key :string)
  (default :int))

(export 'print-settings-int-with-default)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer ()
;;; gtk_print_settings_set_printer ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-printer) (printer settings)
  (foreign-funcall "gtk_print_settings_set_printer"
                   (g:object print-settings) settings
                   :string printer
                   :void)
  printer)

(defcfun ("gtk_print_settings_get_printer" print-settings-printer) :string
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-printer settings) => printer}
  @syntax[]{(setf (gtk:print-settings-printer settings) printer)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[printer]{a string with the printer name}
  @begin{short}
    Accessor of the printer name of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-printer} obtains the value of
  \"printer\". The function @sym{(setf gtk:print-settings-printer)} sets
  \"printer\" to @arg{printer}.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-printer)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_orientation ()
;;; gtk_print_settings_set_orientation ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-orientation) (orientation settings)
  (foreign-funcall "gtk_print_settings_set_orientation"
                   (g:object print-settings) settings
                   page-orientation orientation
                   :void)
  orientation)

(defcfun ("gtk_print_settings_get_orientation" print-settings-orientation)
    page-orientation
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-orientation settings) => orientation}
  @syntax[]{(setf (gtk:print-settings-orientation settings) orientation)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[orientation]{a page orientation of type
    @symbol{gtk:page-orientation}}
  @begin{short}
    Accessor of the \"orientation\" value of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-orientation} gets the value of
  \"orientation\", converted to a @symbol{gtk:page-orientation}. The function
  @sym{(setf gtk:print-settings-orientation)} sets the value of \"orientation\".
  @see-class{gtk:print-settings}
  @see-symbol{gtk:page-orientation}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-orientation)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_size ()
;;; gtk_print_settings_set_paper_size ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-paper-size) (paper-size settings)
  (foreign-funcall "gtk_print_settings_set_paper_size"
                   (g:object print-settings) settings
                   (g:boxed paper-size) paper-size
                   :void)
  paper-size)

(defcfun ("gtk_print_settings_get_paper_size" print-settings-paper-size)
    (g:boxed paper-size)
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-paper-size settings) => paper-size}
  @syntax[]{(setf (gtk:print-settings-paper-size settings) paper-size)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[paper-size]{a paper size of type @class{gtk:paper-size}}
  @begin{short}
    Accessor of the \"paper-format\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-paper-size} gets the value of
  \"paper-format\", converted to a @class{gtk:paper-size}. The function
  @sym{(setf gtk:print-settings-paper-size)} sets the value of \"paper-format\",
  \"paper-width\", and \"paper-height\".
  @begin[Example]{dictionary}
    @begin{pre}
 (setq settings (make-instance 'gtk:print-settings))
=> #<gtk:print-settings {1001A0F643@}>
 (setf (gtk:print-settings-paper-size settings) (gtk:paper-size-new \"iso_a4\"))
=> #<GTK-PAPER-SIZE {1001A244C3@}>
 (gtk:print-settings-paper-size settings)
=> #<GTK-PAPER-SIZE {1001A24B63@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-settings}
  @see-class{gtk:paper-size}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-paper-size)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_width ()
;;; gtk_print_settings_set_paper_width ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-paper-width) (width settings unit)
  (foreign-funcall "gtk_print_settings_set_paper_width"
                   (g:object print-settings) settings
                   :double (coerce width 'double-float)
                   unit unit
                   :void)
  width)

(defcfun ("gtk_print_settings_get_paper_width"
           print-settings-paper-width) :double
 #+liber-documentation
 "@version{#2020-4-13}
  @syntax[]{(gtk:print-settings-paper-width settings) => width}
  @syntax[]{(setf (gtk:print-settings-paper-width settings) width)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[unit]{the unit of type @symbol{gtk:unit} for the return value}
  @argument[width]{a @code{:double} with the paper width}
  @begin{short}
    Accessor of the paper width of a print setting, in units of @arg{unit}.
  @end{short}

  The function @sym{gtk:print-settings-paper-width} gets the value of
  \"paper-width\" converted to @arg{unit}. The function
  @sym{(setf gtk:print-settings-paper-width)} sets the value of \"paper-width\".
  @see-class{gtk:print-settings}
  @see-symbol{gtk:unit}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings))
  (unit unit))

(export 'print-settings-paper-width)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_paper_height ()
;;; gtk_print_settings_set_paper_height ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-paper-height) (height settings unit)
  (foreign-funcall "gtk_print_settings_set_paper_height"
                   (g:object print-settings) settings
                   :double (coerce height 'double-float)
                   unit unit
                   :void)
  height)

(defcfun ("gtk_print_settings_get_paper_height"
           print-settings-paper-height) :double
 #+liber-documentation
 "@version{#2020-4-13}
  @syntax[]{(gtk:print-settings-paper-height settings) => height}
  @syntax[]{(setf (gtk:print-settings-paper-height settings) height)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[unit]{the unit of type @symbol{gtk:unit} for the return value}
  @argument[height]{a @code{:double} with the paper height}
  @begin{short}
    Accessor of the paper height of a print setting, in units of @arg{unit}.
  @end{short}

  The function @sym{gtk:print-settings-paper-height} gets the value of
  \"paper-height\", converted to @arg{unit}. The function
  @sym{(setf gtk:print-settings-paper-height)} sets the value of
  \"paper-height\".
  @see-class{gtk:print-settings}
  @see-symbol{gtk:unit}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings))
  (unit unit))

(export 'print-settings-paper-height)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_use_color ()
;;; gtk_print_settings_set_use_color ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-use-color) (use-color settings)
  (foreign-funcall "gtk_print_settings_set_use_color"
                   (g:object print-settings) settings
                   :boolean use-color
                   :void)
  use-color)

(defcfun ("gtk_print_settings_get_use_color" print-settings-use-color)
    :boolean
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-use-color settings) => use-color}
  @syntax[]{(setf (gtk:print-settings-use-color settings) use-color)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[use-color]{a boolean whether to use color}
  @begin{short}
    Accessor of \"use-color\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-use-color} gets the value of
  \"use-color\". The function @sym{(setf gtk:print-settings-use-color)} sets
  the value of \"use-color\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-use-color)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_collate ()
;;; gtk_print_settings_set_collate ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-collate) (collate settings)
  (foreign-funcall "gtk_print_settings_set_collate"
                   (g:object print-settings) settings
                   :boolean collate
                   :void)
  collate)

(defcfun ("gtk_print_settings_get_collate" print-settings-collate) :boolean
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-collate settings) => collate}
  @syntax[]{(setf (gtk:print-settings-collate settings) collate)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[collate]{a boolean whether to collate the output}
  @begin{short}
    Accessor of \"collate\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-collate} gets the value of \"collate\".
  The function @sym{(setf gtk:print-settings-collate)} sets the value of
  \"collate\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_reverse ()
;;; gtk_print_settings_set_reverse ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-reverse) (reverse settings)
  (foreign-funcall "gtk_print_settings_set_reverse"
                   (g:object print-settings) settings
                   :boolean reverse
                   :void)
  reverse)

(defcfun ("gtk_print_settings_get_reverse" print-settings-reverse) :boolean
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-reverse settings) => reverse}
  @syntax[]{(setf (gtk:print-settings-reverse settings) reverse)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[reverse]{a boolean whether to reverse the output}
  @return{Whether to reverse the order of the printed pages.}
  @begin{short}
    Accessor of \"reverse\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-reverse} gets the value of \"reverse\".
  The function @sym{(setf gtk:print-settings-reverse)} sets the value of
  \"reserve\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-reverse)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_duplex ()
;;; gtk_print_settings_set_duplex ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-duplex) (duplex settings)
  (foreign-funcall "gtk_print_settings_set_duplex"
                   (g:object print-settings) settings
                   print-duplex duplex
                   :void)
  duplex)

(defcfun ("gtk_print_settings_get_duplex" print-settings-duplex) print-duplex
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-duplex settings) => duplex}
  @syntax[]{(setf (gtk:print-settings-duplex settings) duplex)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[duplex]{a @symbol{gtk:print-duplex} value}
  @begin{short}
    Accessor of \"duplex\" of a print setting.
  @end{short}
  Whether to print the output in duplex.

  The function @sym{gtk:print-settings-duplex} gets the value of \"duplex\".
  The function @sym{(setf gtk:print-settings-duplex)} sets the value of
  \"duplex\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-duplex)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_quality ()
;;; gtk_print_settings_set_quality ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-quality) (quality settings)
  (foreign-funcall "gtk_print_settings_set_quality"
                   (g:object print-settings) settings
                   print-quality quality
                   :void)
  quality)

(defcfun ("gtk_print_settings_get_quality" print-settings-quality)
    print-quality
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-quality settings) => quality}
  @syntax[]{(setf (gtk:print-settings-quality settings) quality)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[quality]{a @class{gtk:print-quality} value}
  @begin{short}
    Accessor of \"quality\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-quality} gets the value of \"quality\".
  The function @sym{(setf gtk:print-settings-quality)} sets the value of
  \"quality\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-quality)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_n_copies ()
;;; gtk_print_settings_set_n_copies ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-n-copies) (n-copies settings)
  (foreign-funcall "gtk_print_settings_set_n_copies"
                   (g:object print-settings) settings
                   :int n-copies
                   :void)
  n-copies)

(defcfun ("gtk_print_settings_get_n_copies" print-settings-n-copies) :int
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-n-copies settings) => n-copies}
  @syntax[]{(setf (gtk:print-settings-n-copies settings) n-copies)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[n-copies]{the number of copies}
  @begin{short}
    Accessor of \"n-copies\" of a print setting.
  @end{short}
  The number of type @code{:int} of copies to print.

  The function @sym{gtk:print-settings-n-copies} gets the value of \"n-copies\".
  The function @sym{(setf gtk:print-settings-n-copies)} sets the value of
  \"n-copies\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-n-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up ()
;;; gtk_print_settings_set_number_up ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-number-up) (number-up settings)
  (foreign-funcall "gtk_print_settings_set_number_up"
                   (g:object print-settings) settings
                   :int number-up
                   :void)
  number-up)

(defcfun ("gtk_print_settings_get_number_up" print-settings-number-up) :int
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-number-up settings) => number-up}
  @syntax[]{(setf (gtk:print-settings-number-up settings) number-up)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[number-up]{the number of pages per sheet}
  @begin{short}
    Accessor of \"number-up\" of a print setting.
  @end{short}
  The number of pages per sheet.

  The function @sym{gtk:print-settings-number-up} gets the value of
  \"number-up\". The function @sym{(setf gtk:print-settings-number-up)} sets
  the value of \"number-up\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-number-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_number_up_layout ()
;;; gtk_print_settings_set_number_up_layout ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-number-up-layout) (number-up-layout settings)
  (foreign-funcall "gtk_print_settings_set_number_up_layout"
                   (g:object print-settings) settings
                   number-up-layout number-up-layout
                   :void)
  number-up-layout)

(defcfun ("gtk_print_settings_get_number_up_layout"
           print-settings-number-up-layout) number-up-layout
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-number-up-layout settings) => number-up-layout}
  @syntax[]{(setf (gtk:print-settings-number-up-layout settings) number-up-layout)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[number-up-layout]{a @symbol{gtk:number-up-layout} value}
  @begin{short}
    Accessor of \"number-up-layout\" of a print setting.
  @end{short}
  Layout of page in number-up mode.

  The function @sym{gtk:print-settings-number-up-layout} gets the value of
  \"number-up-layout\". The function
  @sym{(setf gtk:print-settings-number-up-layout)} sets the value of
  \"number-up-layout\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set}
  @see-function{gtk:print-settings-get}"
  (settings (g:object print-settings)))

(export 'print-settings-number-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution ()
;;; gtk_print_settings_set_resolution ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-resolution) (resolution settings)
  (foreign-funcall "gtk_print_settings_set_resolution"
                   (g:object print-settings) settings
                   :int resolution
                   :void)
  resolution)

(defcfun ("gtk_print_settings_get_resolution" print-settings-resolution)
    :int
 #+liber-documentation
 "@version{#2020-4-5}
  @syntax[]{(gtk:print-settings-resolution settings) => resolution}
  @syntax[]{(setf (gtk:print-settings-resolution settings) resolution)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[resolution]{a @code{:int} with the resolution in dpi}
  @begin{short}
    Accessor of \"resolution\" of a print setting.
  @end{short}
  The resolution in dpi.

  The function @sym{gtk:print-settings-resolution} gets the value of
  \"resolution\". The function @sym{(setf gtk:print-settings-resolution)} sets
  the values of \"resolution\", \"resolution-x\" and \"resolution-y\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-set-resolution_xy}"
  (settings (g:object print-settings)))

(export 'print-settings-resolution)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_set_resolution_xy ()
;;; ----------------------------------------------------------------------------

;; We do not export this function. Consider to remove it.

(defcfun ("gtk_print_settings_set_resolution_xy"
           print-settings-set-resolution-xy) :void
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[resolution-x]{an integer with the horizontal resolution in dpi}
  @argument[resolution-y]{an integer with the vertical resolution in dpi}
  @short{Sets the values of \"resolution\", \"resolution-x\" and
    \"resolution-y\".}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-resolution}"
  (settings (g:object print-settings))
  (resolution-x :int)
  (resolution-y :int))

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_x ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_resolution_x" print-settings-resolution-x)
    :int
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[settings]{a @class{gtk:print-settings} object}
  @return{An integer with the horizontal resolution in dpi.}
  @short{Gets the value of \"resolution-x\".}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-resolution}
  @see-function{gtk:print-settings-resolution-y}"
  (settings (g:object print-settings)))

(export 'print-settings-resolution-x)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_resolution_y ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_get_resolution_y" print-settings-resolution-y)
    :int
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[settings]{a @class{gtk:print-settings} object}
  @return{An integer with the vertical resolution in dpi.}
  @short{Gets the value of \"resolution-y\".}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-resolution}
  @see-function{gtk:print-settings-resolution-x}"
  (settings (g:object print-settings)))

(export 'print-settings-resolution-y)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_printer_lpi ()
;;; gtk_print_settings_set_printer_lpi ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-printer-lpi) (printer-lpi settings)
  (foreign-funcall "gtk_print_settings_set_printer_lpi"
                   (g:object print-settings) settings
                   :double (coerce printer-lpi 'double-float)
                   :void)
  printer-lpi)

(defcfun ("gtk_print_settings_get_printer_lpi" print-settings-printer-lpi)
    :double
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-printer-lpi settings) => printer-lpi}
  @syntax[]{(setf (gtk:print-settings-printer-lip settings) printer-lpi)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[lpi]{an integer with the resolution in lpi (lines per inch)}
  @begin{short}
    Accessor of \"printer-lip\" of a print setting.
  @end{short}
  The resolution in lpi (lines per inch).

  The function @sym{gtk:print-settings} gets the value of \"print-lpi\". The
  function @sym{(setf gtk:print-settings-printer-lip)} sets the values of
  \"printer-lpi\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-printer-lpi)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_scale ()
;;; gtk_print_settings_set_scale ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-scale) (scale settings)
  (foreign-funcall "gtk_print_settings_set_scale"
                   (g:object print-settings) settings
                   :double (coerce scale 'double-float)
                   :void)
  scale)

(defcfun ("gtk_print_settings_get_scale" print-settings-scale) :double
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-scale settings) => scale}
  @syntax[]{(setf (gtk:print-settings-scale settings) scale)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[scale]{a @code{:double} with the scale in percent}
  @begin{short}
    Accessor of \"scale\" of a print setting.
  @end{short}
  The scale in percent.

  The function @sym{gtk:print-settings-scale} gets the value of \"scale\". The
  function @sym{(setf gtk:print-settings-scale)} sets the values of \"scale\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_print_pages ()
;;; gtk_print_settings_set_print_pages ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-print-pages) (print-pages settings)
  (foreign-funcall "gtk_print_settings_set_print_pages"
                   (g:object print-settings) settings
                   print-pages print-pages
                   :void)
  print-pages)

(defcfun ("gtk_print_settings_get_print_pages" print-settings-print-pages)
    print-pages
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-print-pages settings) => print-pages}
  @syntax[]{(setf (gtk:print-settings-print-pages settings) print-pages)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[pages]{a @symbol{gtk:print-pages} value}
  @begin{short}
    Accessor of \"print-pages\" of a print setting.
  @end{short}
  Which pages to print.

  The function @sym{gtk:print-settings-print-pages} gets the value of
  \"print-pages\". The function @sym{(setf gtk:print-settings-print-pages)}
  sets the values of \"print-pages\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-print-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_ranges ()
;;; gtk_print_settings_set_page_ranges ()
;;; ----------------------------------------------------------------------------

(defun print-settings-page-ranges (settings)
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-page-ranges settings) => pages-ranges}
  @syntax[]{(setf (gtk:print-settings-page-ranges settings) page-ranges)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[page-ranges]{a list of pages ranges}
  @begin{short}
    Accessor of \"page-ranges\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-page-ranges} gets the value of
  \"page-ranges\". The function @sym{(setf gtk:print-settings-page-ranges)}
  sets the value of \"page-ranges\".
  @begin[Example]{dictionary}
    @begin{pre}
 (setq settings (gtk:print-settings-new))
=> #<gtk:print-settings {1001929323@}>
 (setf (gtk:print-settings-page-ranges settings) '((1) (15 20) (25)))
=> ((1) (15 20) (25))
 (gtk:print-settings-page-ranges settings)
=> ((1) (15 20) (25))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (let ((result nil)
        (value (print-settings-get settings "page-ranges")))
    (setf value (split-sequence:split-sequence #\, value))
    (dolist (range value)
      (setf range (split-sequence:split-sequence #\- range))
      (let ((start (first range)) (end (second range)))
        (if (not end)
            (push (list (parse-integer start)) result)
            (push (list (parse-integer start) (parse-integer end)) result))))
    (nreverse result)))

(defun (setf print-settings-page-ranges) (page-ranges settings)
  (let ((value nil))
    (dolist (range page-ranges)
      (let ((start (first range)) (end (second range)))
        (if (or (not end) (= start end))
            (setf value (concatenate 'string value (format nil "~d" start)))
            (setf value
                  (concatenate 'string value (format nil "~d-~d" start end))))
        (setf value (concatenate 'string value ","))))
    (setf value (string-right-trim "," value))
    (print-settings-set settings "page-ranges" value)
    page-ranges))

(export 'print-settings-page-ranges)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_page_set ()
;;; gtk_print_settings_set_page_set ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-page-set) (page-set settings)
  (foreign-funcall "gtk_print_settings_set_page_set"
                   (g:object print-settings) settings
                   page-set page-set
                   :void)
  page-set)

(defcfun ("gtk_print_settings_get_page_set" print-settings-page-set)
    page-set
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-page-set settings) => pages-set}
  @syntax[]{(setf (gtk:print-settings-page-set settings) page-set)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[page-set]{a @symbol{gtk:page-set} value}
  @begin{short}
    Accessor of \"page-set\" of a print setting.
  @end{short}
  The set of pages to print.

  The function @sym{gtk:print-settings-page-set} gets the value of \"page-set\".
  The function @sym{(setf gtk:print-settings-page-set)} sets the values of
  \"page-set\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_default_source ()
;;; gtk_print_settings_set_default_source ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-default-source) (default-source settings)
  (foreign-funcall "gtk_print_settings_set_default_source"
                   (g:object print-settings) settings
                   :string default-source
                   :void)
  default-source)

(defcfun ("gtk_print_settings_get_default_source"
           print-settings-default-source) :string
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-default-source settings) => default-source}
  @syntax[]{(setf (gtk:print-settings-default-source settings) default-source)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[default-source]{a string with the default source}
  @begin{short}
    Accessor of \"default-source\" of a print setting.
  @end{short}
  The default source.

  The function @sym{gtk:print-settings-default-source} gets the value of
  \"default-source\". The function
  @sym{(setf gtk:print-settings-default-source)} sets the value of
  \"default-source\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-default-source)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_media_type ()
;;; gtk_print_settings_set_media_type ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-media-type) (media-type settings)
  (foreign-funcall "gtk_print_settings_set_media_type"
                   (g:object print-settings) settings
                   :string media-type
                   :void)
  media-type)

(defcfun ("gtk_print_settings_get_media_type" print-settings-media-type)
    :string
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-media-type settings) => media-type}
  @syntax[]{(setf (gtk:print-settings-media-type settings) media-type)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[media-type]{a string with the media type}
  @begin{short}
    Accessor of \"media-type\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-media-type} gets the value of
  \"media-type\". The function @sym{(setf gtk:print-settings-media-type)} sets
  the value of \"media-type\".

  The set of media types is defined in PWG 5101.1-2002 PWG.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-media-type)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_dither ()
;;; gtk_print_settings_set_dither ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-dither) (dither settings)
  (foreign-funcall "gtk_print_settings_set_dither"
                   (g:object print-settings) settings
                   :string dither
                   :void)
  dither)

(defcfun ("gtk_print_settings_get_dither" print-settings-dither) :string
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-dither settings) => dither}
  @syntax[]{(setf (gtk:print-settings-dither settings) dither)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[dither]{a string with the dithering that is used}
  @begin{short}
    Accessor of \"dither\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-dither} gets the value of \"dither\".
  The function @sym{(setf gtk:print-settings-dither)} sets the value of
  \"dither\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-dither)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_finishings ()
;;; gtk_print_settings_set_finishings ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-finishings) (finishings settings)
  (foreign-funcall "gtk_print_settings_set_finishings"
                   (g:object print-settings) settings
                   :string finishings
                   :void)
  finishings)

(defcfun ("gtk_print_settings_get_finishings" print-settings-finishings)
    :string
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-finishings settings) => finishings}
  @syntax[]{(setf (gtk:print-settings-finishings settings) finishings)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[finishings]{a string with the finishings}
  @begin{short}
    Accessor of \"finishings\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-finishings} gets the value of
  \"finishings\". The function @sym{(setf gtk:print-settings-finishings)} sets
  the value of \"finishing\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-finishings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_get_output_bin ()
;;; gtk_print_settings_set_output_bin ()
;;; ----------------------------------------------------------------------------

(defun (setf print-settings-output-bin) (output-bin settings)
  (foreign-funcall "gtk_print_settings_set_output_bin"
                   (g:object print-settings) settings
                   :string output-bin
                   :void)
  output-bin)

(defcfun ("gtk_print_settings_get_output_bin" print-settings-output-bin)
    :string
 #+liber-documentation
 "@version{#2020-4-6}
  @syntax[]{(gtk:print-settings-output-bin settings) => output-bin}
  @syntax[]{(setf (gtk:print-settings-output-bin settings) output-bin)}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[output-bin]{a string with the output bin}
  @begin{short}
    Accessor of \"output-bin\" of a print setting.
  @end{short}

  The function @sym{gtk:print-settings-outpu-bin} gets the value of
  \"output-bin\". The function @sym{(setf gtk:print-settings-output-bin)}
  sets the value of \"output-bin\".
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-get}
  @see-function{gtk:print-settings-set}"
  (settings (g:object print-settings)))

(export 'print-settings-output-bin)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_new_from_file" %print-settings-new-from-file)
    (g:object print-settings)
  (filename :string)
  (err :pointer))

(defun print-settings-new-from-file (path)
 #+liber-documentation
 "@version{#2023-1-29}
  @argument[path]{a pathname or namestring with the file to read the settings
    from}
  @return{The restored @class{gtk:print-settings} object.}
  @begin{short}
    Reads the print settings from @arg{path}.
  @end{short}
  Returns a new @class{gtk:print-settings} object with the restored settings,
  or @code{nil} if an error occurred.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-file}
  @see-function{gtk:print-settings-load-file}"
  (with-ignore-g-error (err)
    (%print-settings-new-from-file (namestring path) err)))

(export 'print-settings-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_new_from_key_file"
          %print-settings-new-from-key-file) (g:object print-settings)
  (key-file (:pointer (:struct glib:variant)))
  (group-name :string)
  (error :pointer))

(defun print-settings-new-from-key-file (key-file group-name)
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[key-file]{a @type{glib:key-file} instance to retrieve the settings
    from}
  @argument[group-name]{a string with the name of the group to use, or
    @code{nil} to use the default \"Print Settings\"}
  @return{The restored @class{gtk:print-settings} object.}
  @begin{short}
    Reads the print settings from the group @arg{group-name} in the key file.
  @end{short}
  Returns a new @class{gtk:print-settings} object with the restored settings,
  or @code{nil} if an error occurred.
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-file}
  @see-function{gtk:print-settings-load-file}"
  (with-g-error (err)
    (%print-settings-new-from-key-file key-file group-name err)))

(export 'print-settings-new-from-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_new_from_gvariant ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_new_from_gvariant"
           print-settings-new-from-gvariant) (g:object print-settings)
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[variant]{an @code{a{sv@}} @type{glib:variant} structure}
  @return{The restored @class{gtk:print-settings} object.}
  @begin{short}
    Deserialize print settings from an @code{a{sv@}} variant in the format
    produced by the function @fun{gtk:print-settings-to-gvariant}.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(let* ((variant (g:variant-parse \"a{sv@}\"
                                 \"{'scale': <'100'>,
                                   'number-up': <'1'>,
                                   'n-copies': <'1'>,
                                   'page-ranges': <'0-11'>,
                                   'page-set': <'all'>,
                                   'printer': <'In Datei drucken'>,
                                   'print-pages': <'ranges'>,
                                   'reverse': <'false'>,
                                   'collate': <'false'>,
                                   'output-file-format': <'pdf'>@}\"))
       (settings (gtk:print-settings-new-from-gvariant variant)))
  (g:variant-print (gtk:print-settings-to-gvariant settings) nil))
=> \"{'scale': <'100'>, 'number-up': <'1'>, 'n-copies': <'1'>,
      'page-ranges': <'0-11'>, 'page-set': <'all'>,
      'printer': <'In Datei drucken'>, 'print-pages': <'ranges'>,
      'reverse': <'false'>, 'collate': <'false'>,
      'output-file-format': <'pdf'>@}\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-gvariant}"
  (variant (:pointer (:struct glib:variant))))

(export 'print-settings-new-from-gvariant)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_load_file" %print-settings-load-file) :boolean
  (settings (g:object print-settings))
  (filename :string)
  (err :pointer))

(defun print-settings-load-file (settings path)
 #+liber-documentation
 "@version{2023-1-29}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[path]{a pathname or namestring with the filename to read the
    settings from}
  @return{@em{True} on success.}
  @begin{short}
    Reads the print settings from @arg{path}.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-file}
  @see-function{gtk:print-settings-new-from-file}"
  (with-g-error (err)
    (%print-settings-load-file settings (namestring path) err)))

(export 'print-settings-load-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_load_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_load_key_file" %print-settings-load-key-file)
    :boolean
  (settings (g:object print-settings))
  (keyfile (:pointer (:struct glib:key-file)))
  (group :string)
  (err :pointer))

(defun print-settings-load-key-file (settings keyfile group)
 #+liber-documentation
 "@version{#2022-10-21}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[keyfile]{a @type{glib:key-file} instance to retrieve the settings
    from}
  @argument[group]{a string with the name of the group to use, or @code{nil} to
    use the default \"Print Settings\"}
  @return{@em{True} on success.}
  @begin{short}
    Reads the print settings from the group @arg{group} in the key file.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-key-file}
  @see-function{gtk:print-settings-new-from-key-file}"
  (with-g-error (err)
    (%print-settings-load-key-file settings keyfile group err)))

(export 'print-settings-load-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_to_file" %print-settings-to-file) :boolean
  (settings (g:object print-settings))
  (filename :string)
  (err :pointer))

(defun print-settings-to-file (settings path)
 #+liber-documentation
 "@version{2023-1-29}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[path]{a pathname or namestring with the filename to save to}
  @return{@em{True} on success.}
  @begin{short}
    This function saves the print settings from settings to @arg{file-name}.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-load-file}
  @see-function{gtk:print-settings-new-from-file}"
  (with-g-error (err)
    (%print-settings-to-file settings (namestring path) err)))

(export 'print-settings-to-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_key_file ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_to_key_file" print-settings-to-key-file) :void
 #+liber-documentation
 "@version{#2022-10-21}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[keyfile]{the @type{glib:key-file} structure to save the print
    settings to}
  @argument[group]{a string with the group to add the settings to in the key
    file, or @code{nil} to use the default \"Print Settings\"}
  @begin{short}
    This function adds the print settings from the print settings to the key
    file.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-to-key-file}
  @see-function{gtk:print-settings-new-from-key-file}"
  (settings (g:object print-settings))
  (keyfile (:pointer (:struct glib:key-file)))
  (group :string))

(export 'print-settings-to-key-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_settings_to_gvariant ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_print_settings_to_gvariant" print-settings-to-gvariant)
    (:pointer (:struct glib:variant))
 #+liber-documentation
 "@version{#2020-4-6}
  @argument[settings]{a @class{gtk:print-settings} object}
  @return{A new @type{glib:variant} structure.}
  @begin{short}
    Serialize print settings to an @code{a{sv@}} variant.
  @end{short}
  @see-class{gtk:print-settings}
  @see-function{gtk:print-settings-new-from-gvariant}"
  (settings (g:object print-settings)))

(export 'print-settings-to-gvariant)

;;; --- End of file gtk4.print-settings.lisp -----------------------------------
