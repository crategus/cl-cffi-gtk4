;;; ----------------------------------------------------------------------------
;;; gtk4.about-dialog.lisp
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
;;; GtkAboutDialog
;;;
;;;     Display information about an application
;;;
;;; Types and Values
;;;
;;;     GtkAboutDialog
;;;     GtkLicense
;;;
;;; Accessors
;;;
;;;     gtk_about_dialog_get_artists
;;;     gtk_about_dialog_set_artists
;;;     gtk_about_dialog_get_authors
;;;     gtk_about_dialog_set_authors
;;;     gtk_about_dialog_get_comments
;;;     gtk_about_dialog_set_comments
;;;     gtk_about_dialog_get_copyright
;;;     gtk_about_dialog_set_copyright
;;;     gtk_about_dialog_get_documenters
;;;     gtk_about_dialog_set_documenters
;;;     gtk_about_dialog_get_license
;;;     gtk_about_dialog_set_license
;;;     gtk_about_dialog_get_license_type
;;;     gtk_about_dialog_set_license_type
;;;     gtk_about_dialog_get_logo
;;;     gtk_about_dialog_set_logo
;;;     gtk_about_dialog_get_logo_icon_name
;;;     gtk_about_dialog_set_logo_icon_name
;;;     gtk_about_dialog_get_program_name
;;;     gtk_about_dialog_set_program_name
;;;     gtk_about_dialog_get_system_information
;;;     gtk_about_dialog_set_system_information
;;;     gtk_about_dialog_get_translator_credits
;;;     gtk_about_dialog_set_translator_credits
;;;     gtk_about_dialog_get_version
;;;     gtk_about_dialog_set_version
;;;     gtk_about_dialog_get_website
;;;     gtk_about_dialog_set_website
;;;     gtk_about_dialog_get_website_label
;;;     gtk_about_dialog_set_website_label
;;;     gtk_about_dialog_get_wrap_license
;;;     gtk_about_dialog_set_wrap_license
;;;
;;; Functions
;;;
;;;     gtk_about_dialog_new
;;;     gtk_about_dialog_add_credit_section
;;;     gtk_show_about_dialog
;;;
;;; Properties
;;;
;;;     artists
;;;     authors
;;;     comments
;;;     copyright
;;;     documenters
;;;     license
;;;     license-type
;;;     logo
;;;     logo-icon-name
;;;     program-name
;;;     system-information
;;;     translator-credits
;;;     version
;;;     website
;;;     website-label
;;;     wrap-license
;;;
;;; Signals
;;;
;;;     activate-link
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkAboutDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessbile
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLicense
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkLicense" license
  (:export t
   :type-initializer "gtk_license_get_type")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8)
  (:gpl-2-0-only 9)
  (:gpl-3-0-only 10)
  (:lgpl-2-1-only 11)
  (:lgpl-3-0-only 12)
  (:agpl-3-0 13)
  (:agpl-3-0-only 14)
  (:bsd-3 15)
  (:apache-2-0 16)
  (:mpl-2-0 17)
  #+gtk-4-14
  (:0bsd 18))

#+liber-documentation
(setf (liber:alias-for-symbol 'license)
      "GEnum"
      (liber:symbol-documentation 'license)
 "@version{2024-5-25}
  @begin{declaration}
(gobject:define-genum \"GtkLicense\" license
  (:export t
   :type-initializer \"gtk_license_get_type\")
  (:unknown 0)
  (:custom 1)
  (:gpl-2-0 2)
  (:gpl-3-0 3)
  (:lgpl-2-1 4)
  (:lgpl-3-0 5)
  (:bsd 6)
  (:mit-x11 7)
  (:artistic 8)
  (:gpl-2-0-only 9)
  (:gpl-3-0-only 10)
  (:lgpl-2-1-only 11)
  (:lgpl-3-0-only 12)
  (:agpl-3-0 13)
  (:agpl-3-0-only 14)
  (:bsd-3 15)
  (:apache-2-0 16)
  (:mpl-2-0 17)
  #+gtk-4-14
  (:0bsd 18))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:unknown]{No license specified.}
      @entry[:custom]{A license text is going to be specified by the developer.}
      @entry[:gpl-2-0]{The GNU General Public License, version 2.0.}
      @entry[:gpl-3-0]{The GNU General Public License, version 3.0.}
      @entry[:lgpl-2-1]{The GNU Lesser General Public License, version 2.1.}
      @entry[:lgpl-3-0]{The GNU Lesser General Public License, version 3.0.}
      @entry[:bsd]{The BSD standard license.}
      @entry[:mit-x11]{The MIT/X11 standard license.}
      @entry[:artistic]{The Artistic License, version 2.0.}
      @entry[:gpl-2-0-only]{The GNU General Public License, version 2.0 only.}
      @entry[:gpl-3-0-only]{The GNU General Public License, version 3.0 only.}
      @entry[:lgpl-2-1-only]{The GNU Lesser General Public License, version 2.1
        only.}
      @entry[:lgpl-3-0-only]{The GNU Lesser General Public License, version 3.0
        only.}
      @entry[:agpl-3-0]{The GNU Affero General Public License, version 3.0 or
        later.}
      @entry[:agpl-3-0-only]{The GNU Affero General Public License, version 3.0
        only.}
      @entry[:bsd-3]{The 3-clause BSD license.}
      @entry[:apache-2-0]{The Apache License, version 2.0.}
      @entry[:mpl-2-0]{The Mozilla Public License, version 2.0.}
      @entry[:0bsd]{Zero-clause BSD license. Since 4.14}
    @end{table}
  @end{values}
  @begin{short}
    The type of license for an application.
  @end{short}
  @see-class{gtk:about-dialog}")

;;; ----------------------------------------------------------------------------
;;; GtkAboutDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAboutDialog" about-dialog
  (:superclass window
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_about_dialog_get_type")
  ((artists
    about-dialog-artists
    "artists" "GStrv" t t)
   (authors
    about-dialog-authors
    "authors" "GStrv" t t)
   (comments
    about-dialog-comments
    "comments" "gchararray" t t)
   (copyright
    about-dialog-copyright
    "copyright" "gchararray" t t)
   (documenters
    about-dialog-documenters
    "documenters" "GStrv" t t)
   (license
    about-dialog-license
    "license" "gchararray" t t)
   (license-type
    about-dialog-license-type
    "license-type" "GtkLicense" t t)
   (logo
    about-dialog-logo
    "logo" "GdkPaintable" t t)
   (logo-icon-name
    about-dialog-logo-icon-name
    "logo-icon-name" "gchararray" t t)
   (program-name
    about-dialog-program-name
    "program-name" "gchararray" t t)
   (system-information
    about-dialog-system-information
    "system-information" "gchararray" t t)
   (translator-credits
    about-dialog-translator-credits
    "translator-credits" "gchararray" t t)
   (version
    about-dialog-version
    "version" "gchararray" t t)
   (website
    about-dialog-website
    "website" "gchararray" t t)
   (website-label
    about-dialog-website-label
    "website-label" "gchararray" t t)
   (wrap-license
    about-dialog-wrap-license
    "wrap-license" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'about-dialog 'type)
 "@version{2024-4-11}
  @begin{short}
    The @class{gtk:about-dialog} widget offers a simple way to display
    information about a program like its logo, name, copyright, website and
    license.
  @end{short}
  It is also possible to give credits to the authors, documenters, translators
  and artists who have worked on the program. The about dialog is typically
  opened when the user selects the About option from the Help menu. All parts
  of the about dialog are optional.

  @image[about-dialog]{Figure: GtkAboutDialog}

  The about dialog often contain links and email addresses. The about dialog
  displays these as clickable links. By default, it calls the @fun{gtk:show-uri}
  function when a user clicks one. The behaviour can be overridden with the
  @code{\"activate-link\"} signal.

  To specify a person with an email address, use a string like
  @code{\"Edgar Allan Poe <edgar@@poe.com>\"}. To specify a website with a
  title, use a string like @code{\"GTK team https://www.gtk.org\"}.

  To make constructing an about dialog as convenient as possible, you can use
  the @fun{gtk:show-about-dialog} function which constructs and shows an about
  dialog and keeps it around so that it can be shown again.

  Note that GTK sets a default title of @code{\"About @code{%s}\"} on the about
  dialog window where @code{%s} is replaced by the name of the application, but
  in order to ensure proper translation of the title, applications should set
  the title property explicitly when constructing a @class{gtk:about-dialog}
  widget, as shown in the following example:
  @begin{pre}
(gtk:show-about-dialog nil
                       :program-name \"ExampleCode\"
                       :logo example-logo
                       :title \"About ExampleCode\")
  @end{pre}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:about-dialog} implementation has a single CSS node with the
    name @code{window} and the @code{.aboutdialog} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
lambda (dialog uri)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[dialog]{The @class{gtk:about-dialog} widget on which the signal
          was emitted.}
        @entry[uri]{The string with the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{table}
      Emitted when a URL is activated. Applications may connect to it to
      override the default behaviour, which is to call the @fun{gtk:show-uri}
      function.
  @end{dictionary}
  @see-constructor{gtk:about-dialog-new}
  @see-slot{gtk:about-dialog-artists}
  @see-slot{gtk:about-dialog-authors}
  @see-slot{gtk:about-dialog-comments}
  @see-slot{gtk:about-dialog-copyright}
  @see-slot{gtk:about-dialog-documenters}
  @see-slot{gtk:about-dialog-license}
  @see-slot{gtk:about-dialog-license-type}
  @see-slot{gtk:about-dialog-logo}
  @see-slot{gtk:about-dialog-logo-icon-name}
  @see-slot{gtk:about-dialog-program-name}
  @see-slot{gtk:about-dialog-system-information}
  @see-slot{gtk:about-dialog-translator-credits}
  @see-slot{gtk:about-dialog-version}
  @see-slot{gtk:about-dialog-website}
  @see-slot{gtk:about-dialog-website-label}
  @see-slot{gtk:about-dialog-wrap-license}
  @see-class{gtk:dialog}
  @see-symbol{gtk:license}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:about-dialog-artists -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "artists" 'about-dialog) t)
 "The @code{artists} property of type @type{glib:strv-t} (Read / Write) @br{}
  The people who contributed artwork to the program, as a list of strings. Each
  string may contain email addresses and URLs, which will be displayed as
  links.")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-artists)
      "Accessor"
      (documentation 'about-dialog-artists 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-artists object) => artists}
  @syntax{(setf (gtk:about-dialog-artists object) artists)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[artists]{a list of strings with the people who contributed artwork
    to the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{artists} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-artists} function returns the strings which are
  displayed in the credits page. The @setf{gtk:about-dialog-artists} function
  sets the strings.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-authors -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "authors" 'about-dialog) t)
 "The @code{authors} property of type @type{glib:strv-t} (Read / Write) @br{}
  The authors of the program, as a list of strings. Each string may contain
  email addresses and URLs, which will be displayed as links.")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-authors)
      "Accessor"
      (documentation 'about-dialog-authors 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-authors object) => authors}
  @syntax{(setf (gtk:about-dialog-authors object) authors)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[authors]{a list of string with the authors of the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{authors} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-authors} function returns the strings which are
  displayed in the credits page. The @setf{gtk:about-dialog-authors} function
  sets the strings. Each string may contain email addresses and URLs, which will
  be displayed as links.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq about (make-instance 'gtk:about-dialog))
=> #<GTK:ABOUT-DIALOG {1009A9FF83@}>
(setf (gtk:about-dialog-artists about)
      (list \"first author\" \"second author\"))
=> (\"first author\" \"second author\")
(gtk:about-dialog-artists about)
=> (\"first author\" \"second author\")
    @end{pre}
  @end{dictionary}
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-comments ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "comments" 'about-dialog) t)
 "The @code{comments} property of type @code{:string} (Read / Write) @br{}
  Comments about the program. This string is displayed in a label in the main
  dialog, thus it should be a short explanation of the main purpose of the
  program, not a detailed list of features. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-comments)
      "Accessor"
      (documentation 'about-dialog-comments 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-comments object) => comments}
  @syntax{(setf (gtk:about-dialog-comments object) comments)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[comments]{a string with comments about the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{comments} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-comments} function returns the comments string. The
  @setf{gtk:about-dialog-comments} function sets the comments string to display
  in the about dialog. The string is displayed in a label in the main dialog,
  thus it should be a short explanation of the main purpose of the program, not
  a detailed list of features.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-copyright ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "copyright" 'about-dialog) t)
 "The @code{copyright} property of type @code{:string} (Read / Write) @br{}
  Copyright information for the program. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-copyright)
      "Accessor"
      (documentation 'about-dialog-copyright 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-copyright object) => copyright}
  @syntax{(setf (gtk:about-dialog-copyright object) copyright)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[copyright]{a string with copyright information}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{copyright} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-copyright} function returns the copyright
  information for the program. The @setf{gtk:about-dialog-copyright} function
  sets the copyright information. This should be a short string of one or two
  lines.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-documenters -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "documenters" 'about-dialog) t)
 "The @code{documenters} property of type @type{glib:strv-t} (Read / Write)
  @br{}
  The people documenting the program, as a list of strings. Each string may
  contain email addresses and URLs, which will be displayed as links.")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-documenters)
      "Accessor"
      (documentation 'about-dialog-documenters 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-documenters object) => documenters}
  @syntax{(setf (gtk:about-dialog-documenters object) documenters)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[documenters]{a string with the people documenting the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{documenters} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-documenters} function returns the strings which are
  displayed in the documenters credits page. The
  @setf{gtk:about-dialog-documenters} function sets the strings. Each string may
  contain email addresses and URLs, which will be displayed as links.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-license -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "license" 'about-dialog) t)
 "The @code{license} property of type @code{:string} (Read / Write) @br{}
  The license of the program. The string is displayed in a text view in a
  secondary dialog, therefore it is fine to use a long multi-paragraph text.
  Note that the text is only wrapped in the text view if the @code{wrap-license}
  property is set to @em{true}. Otherwise the text itself must contain the
  intended linebreaks. When setting this property to a non-@code{nil} value,
  the @code{license-type} property is set to the @code{:custom} value of the
  @symbol{gtk:license} enumeration as a side effect. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-license)
      "Accessor"
      (documentation 'about-dialog-license 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-license object) => license}
  @syntax{(setf (gtk:about-dialog-license object) license)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[license]{a string with the license of the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{license} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-license} function returns the license information.
  The @setf{gtk:about-dialog-license} function sets the license information to
  be displayed in the secondary license dialog. If the @arg{license} argument is
  @code{nil}, the license page is hidden.

  The text may contain links in this format @code{\"<http://www.some.place/>\"}
  and email references in the form @code{\"<mail-to@@some.body>\"}, and these
  will be converted into clickable links.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-license-type ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "license-type" 'about-dialog) t)
 "The @code{license-type} property of type @symbol{gtk:license} (Read / Write)
  @br{}
  The license of the program, as a value of the @symbol{gtk:license}
  enumeration. The about dialog will automatically fill out a standard
  disclaimer and link the user to the appropriate online resource for the
  license text. If the @code{:unknown} value is used, the link used will be the
  same specified in the @code{website} property. If the @code{:custom} value is
  used, the current contents of the @code{license} property are used. For any
  other @symbol{gtk:license} value, the contents of the @code{license} property
  are also set by this property as a side effect. @br{}
  Default value: @code{:unkown}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-license-type)
      "Accessor"
      (documentation 'about-dialog-license-type 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-license-type object) => license-type}
  @syntax{(setf (gtk:about-dialog-license-type object) license-type)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[license-type]{a value of the @symbol{gtk:license} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{license-type} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-license-type} function retrieves the license type
  of type @symbol{gtk:license}. The @setf{gtk:about-dialog-license-type}
  function sets the license of of the application showing the about dialog from
  a list of known licenses. This function overrides the license set using the
  @fun{gtk:about-dialog-license} function.
  @see-class{gtk:about-dialog}
  @see-symbol{gtk:license}
  @see-function{gtk:about-dialog-license}")

;;; --- gtk:about-dialog-logo --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "logo" 'about-dialog) t)
 "The @code{logo} property of type @class{gdk:paintable} (Read / Write) @br{}
  A logo for the about box. If this is not set, the default window icon set
  with the @fun{gtk:window-default-icon-name} function will be used.")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-logo)
      "Accessor"
      (documentation 'about-dialog-logo 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-logo object) => logo}
  @syntax{(setf (gtk:about-dialog-logo object) logo)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[logo]{a @class{gdk:paintable} logo for the about box}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{logo} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-logo} function returns the paintable displayed as
  logo in the about dialog. The @setf{gtk:about-dialog-logo} function sets the
  paintable. If it is @code{nil}, the default window icon set with the
  @fun{gtk:window-default-icon-name} function will be used.
  @see-class{gtk:about-dialog}
  @see-class{gdk:paintable}
  @see-function{gtk:window-default-icon-name}")

;;; --- gtk:about-dialog-logo-icon-name ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "logo-icon-name"
                                               'about-dialog) t)
 "The @code{logo-icon-name} property of type @code{:string} (Read / Write) @br{}
  A named icon to use as the logo for the about box. This property overrides
  the @code{logo} property. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-logo-icon-name)
      "Accessor"
      (documentation 'about-dialog-logo-icon-name 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-logo-icon-name object) => name}
  @syntax{(setf (gtk:about-dialog-logo-icon-name object) name)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[name]{a string with a namend icon to use as the logo}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{logo-icon-name} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-logo-icon-name} function returns the icon name
  displayed as logo in the about dialog. The
  @setf{gtk:about-dialog-logo-icon-name} function sets the icon name.
  @see-class{gtk:about-dialog}
  @see-function{gtk:window-default-icon-name}")

;;; --- gtk:about-dialog-program-name ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "program-name" 'about-dialog) t)
 "The @code{program-name} property of type @code{:string} (Read / Write) @br{}
  The name of the program. If this is not set, it defaults to the return value
  of the @fun{g:application-name} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-program-name)
      "Accessor"
      (documentation 'about-dialog-program-name 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-program-name object) => name}
  @syntax{(setf (gtk:about-dialog-program-name object) name)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[name]{a string with the name of the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{program-name} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-program-name} function returns the program name
  displayed in the about dialog. The @setf{gtk:about-dialog-program-name}
  function sets the program name. If this is not set, it defaults to the return
  value of the @fun{g:application-name} function.
  @see-class{gtk:about-dialog}
  @see-function{g:application-name}")

;;; --- gtk:about-dialog-system-information ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "system-information"
                                               'about-dialog) t)
 "The @code{system-information} property of type @code{:string} (Read / Write)
  @br{}
  Information about the system on which the program is running. This information
  is displayed in a separate page, therefore it is fine to use a long
  multi-paragraph text. Note that the text should contain the intended
  linebreaks. The text may contain links in this format
  @code{\"<http://www.some.place/>\"} and email references in the form
  @code{\"<mail-to@@some.body>\"}, and these will be converted into clickable
  links. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-system-information)
      "Accessor"
      (documentation 'about-dialog-system-information 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-system-information object) => information}
  @syntax{(setf (gtk:about-dialog-system-information object) information)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[information]{a string with the system information}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{system-information} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-system-information} function returns the system
  information which is shown in the about dialog. The
  @setf{gtk:about-dialog-program-name} function sets the system information.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-translator-credits ------------------------------------

;; TODO: Implement translation support!?

#+liber-documentation
(setf (documentation (liber:slot-documentation "translator-credits"
                                               'about-dialog) t)
 "The @code{translator-credits} property of type @code{:string} (Read / Write)
  @br{}
  Credits to the translators. This string should be marked as translatable.
  The string may contain email addresses and URLs, which will be displayed as
  links. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-translator-credits)
      "Accessor"
      (documentation 'about-dialog-translator-credits 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-translator-credits object) => credits}
  @syntax{(setf (gtk:about-dialog-translator-credits object) credits)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[credits]{a string with the credits to the translators}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{translator-credits} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-translator-credits} function returns the translator
  credits string which is displayed in the credits page. The
  @setf{gtk:about-dialog-translator-credits} function sets the translator
  credits.

  The intended use for this string is to display the translator of the language
  which is currently used in the user interface. Using GNU gettext, a simple way
  to achieve that is to mark the string for translation:
  @begin{pre}
gtk_about_dialog_set_translator_credits (about, _(\"translator-credits\"));
  @end{pre}
  It is a good idea to use the customary \"translator-credits\" msgid for this
  purpose, since translators will already know the purpose of that msgid, and
  since the @class{gtk:about-dialog} widget will detect if the
  \"translator-credits\" property is untranslated and hide the tab.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-version -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "version" 'about-dialog) t)
 "The @code{version} property of type @code{:string} (Read / Write) @br{}
  The version of the program. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-version)
      "Accessor"
      (documentation 'about-dialog-version 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-version object) => version}
  @syntax{(setf (gtk:about-dialog-version object) version)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[version]{a string with the version of the program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{version} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-version} function returns the version string. The
  @setf{gtk:about-dialog-version} function sets the version string to display
  in the about dialog.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-website -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "website" 'about-dialog) t)
 "The @code{website} property of type @code{:string} (Read / Write) @br{}
  The URL for the link to the website of the program. This should be a string
  starting with @code{\"http://\"}. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-website)
      "Accessor"
      (documentation 'about-dialog-website 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-website object) => website}
  @syntax{(setf (gtk:about-dialog-website object) website)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[website]{a string with URL for the link to the website of the
    program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{website} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @setf{gtk:about-dialog-website} function returns the website URL. The
  @setf{gtk:about-dialog-website} function sets the URL string starting with
  @code{\"http://\"} to use for the website link.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-website-label -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "website-label" 'about-dialog) t)
 "The @code{website-label} property of type @code{:string} (Read / Write) @br{}
  The label for the link to the website of the program. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-website-label)
      "Accessor"
      (documentation 'about-dialog-website-label 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-website-label object) => label}
  @syntax{(setf (gtk:about-dialog-website-label object) label)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[label]{a string with the label for the link to the website of the
    program}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{website-label} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-website-label} function returns the URL to use for
  the website. The @setf{gtk:about-dialog-website-label} function sets the URL.
  @see-class{gtk:about-dialog}")

;;; --- gtk:about-dialog-wrap-license ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-license" 'about-dialog) t)
 "The @code{wrap-license} property of type @code{:boolean} (Read / Write) @br{}
  Whether to wrap the text in the license dialog. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'about-dialog-wrap-license)
      "Accessor"
      (documentation 'about-dialog-wrap-license 'function)
 "@version{2024-4-11}
  @syntax{(gtk:about-dialog-wrap-license object) => setting}
  @syntax{(setf (gtk:about-dialog-wrap-license object) setting)}
  @argument[object]{a @class{gtk:about-dialog} widget}
  @argument[setting]{a boolean whether to wrap the text in the license dialog}
  @begin{short}
    Accessor of the @slot[gtk:about-dialog]{wrap-license} slot of the
    @class{gtk:about-dialog} class.
  @end{short}
  The @fun{gtk:about-dialog-wrap-license} function returns whether the license
  text in the about dialog is automatically wrapped. The
  @setf{gtk:about-dialog-wrap-license} function sets the property.
  @see-class{gtk:about-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline about-dialog-new))

(defun about-dialog-new ()
 #+liber-documentation
 "@version{2024-4-11}
  @return{The newly created @class{gtk:about-dialog} widget.}
  @short{Creates a new about dialog.}
  @see-class{gtk:about-dialog}"
  (make-instance 'about-dialog))

(export 'about-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_about_dialog_add_credit_section
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_about_dialog_add_credit_section"
               about-dialog-add-credit-section) :void
 #+liber-documentation
 "@version{#2024-4-11}
  @argument[about]{a @class{gtk:about-dialog} widget}
  @argument[section]{a string with the name of the section}
  @argument[people]{a list of strings of the people who belong to that section}
  @short{Creates a new section in the credits page.}
  @see-class{gtk:about-dialog}"
  (about (g:object about-dialog))
  (section :string)
  (people glib:strv-t))

(export 'about-dialog-add-credit-section)

;;; ----------------------------------------------------------------------------
;;; gtk_show_about_dialog
;;; ----------------------------------------------------------------------------

(let ((aboutdialog nil))
  (defun show-about-dialog (parent &rest args)
   #+liber-documentation
   "@version{2024-8-16}
    @argument[parent]{a @class{gtk:window} transient parent, or @code{nil}
      for none}
    @argument[args]{pairs of property name and property value}
    @begin{short}
      This is a convenience function for showing the about dialog of an
      application.
    @end{short}
    The constructed about dialog is associated with the parent window and reused
    for future invocations of this function.
    @see-class{gtk:about-dialog}"
    (let ((dialog (if parent
                      (g:object-data parent "gtk:about-dialog")
                      aboutdialog)))
      (unless dialog
        (setf dialog (apply #'make-instance 'about-dialog args))
        (setf (window-hide-on-close dialog) t)
        (g:signal-connect dialog "close-request"
                          (lambda (widget)
                            ;; TODO: The C implementation calls in addition the
                            ;; function gtk_stack_set_visible_child_name for
                            ;; the field about->stack
                            (setf (widget-visible widget) nil)
                            t))
        (if parent
            (setf (window-modal dialog) t
                  (window-transient-for dialog) parent
                  (window-destroy-with-parent dialog) t
                  (g:object-data parent "gtk:about-dialog") dialog)
            (setf aboutdialog dialog)))
      (window-present dialog))))

(export 'show-about-dialog)

;;; --- End of file gtk4.about-dialog.lisp -------------------------------------
