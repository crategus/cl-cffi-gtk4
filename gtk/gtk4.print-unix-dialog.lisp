;;; ----------------------------------------------------------------------------
;;; gtk4.print-unix-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkPrintUnixDialog
;;;
;;;     A print dialog
;;;
;;; Types and Values
;;;
;;;     GtkPrintUnixDialog
;;;     GtkPrintCapabilities
;;;
;;; Accessors
;;;
;;;     gtk_print_unix_dialog_set_current_page
;;;     gtk_print_unix_dialog_get_current_page
;;;     gtk_print_unix_dialog_set_embed_page_setup
;;;     gtk_print_unix_dialog_get_embed_page_setup
;;;     gtk_print_unix_dialog_set_has_selection
;;;     gtk_print_unix_dialog_get_has_selection
;;;     gtk_print_unix_dialog_set_manual_capabilities
;;;     gtk_print_unix_dialog_get_manual_capabilities
;;;     gtk_print_unix_dialog_set_page_setup
;;;     gtk_print_unix_dialog_get_page_setup
;;;     gtk_print_unix_dialog_get_selected_printer
;;;     gtk_print_unix_dialog_set_support_selection
;;;     gtk_print_unix_dialog_get_support_selection
;;;
;;; Functions
;;;
;;;     gtk_print_unix_dialog_new
;;;     gtk_print_unix_dialog_set_settings
;;;     gtk_print_unix_dialog_get_settings
;;;     gtk_print_unix_dialog_add_custom_tab
;;;     gtk_print_unix_dialog_get_page_setup_set
;;;
;;; Properties
;;;
;;;     current-page
;;;     embed-page-setup
;;;     has-selection
;;;     manual-capabilities
;;;     page-setup
;;;     print-settings
;;;     selected-printer
;;;     support-selection
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkPrintUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintCapabilities
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkPrintCapabilities" print-capabilities
  (:export t
   :type-initializer "gtk_print_capabilities_get_type")
  (:page-set         #.(ash 1 0))
  (:copies           #.(ash 1 1))
  (:collate          #.(ash 1 2))
  (:reverse          #.(ash 1 3))
  (:scale            #.(ash 1 4))
  (:generate-pdf     #.(ash 1 5))
  (:generate-ps      #.(ash 1 6))
  (:preview          #.(ash 1 7))
  (:number-up        #.(ash 1 8))
  (:number-up-layout #.(ash 1 9)))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-capabilities)
      "GFlags"
      (liber:symbol-documentation 'print-capabilities)
 "@version{2025-07-27}
  @begin{declaration}
(gobject:define-gflags \"GtkPrintCapabilities\" print-capabilities
  (:export t
   :type-initializer \"gtk_print_capabilities_get_type\")
  (:page-set         #.(ash 1 0))
  (:copies           #.(ash 1 1))
  (:collate          #.(ash 1 2))
  (:reverse          #.(ash 1 3))
  (:scale            #.(ash 1 4))
  (:generate-pdf     #.(ash 1 5))
  (:generate-ps      #.(ash 1 6))
  (:preview          #.(ash 1 7))
  (:number-up        #.(ash 1 8))
  (:number-up-layout #.(ash 1 9)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:page-set]{Print dialog will offer printing even/odd pages.}
      @entry[:copies]{Print dialog will allow to print multiple copies.}
      @entry[:collate]{Print dialog will allow to collate multiple copies.}
      @entry[:reverse]{Print dialog will allow to print pages in reverse order.}
      @entry[:scale]{Print dialog will allow to scale the output.}
      @entry[:generate-pdf]{The program will send the document to the printer
        in PDF format.}
      @entry[:generate-ps]{The program will send the document to the printer in
        Postscript format.}
      @entry[:preview]{Print dialog will offer a preview.}
      @entry[:number-up]{Print dialog will offer printing multiple pages per
        sheet.}
      @entry[:up-layout]{Print dialog will allow to rearrange pages when
        printing multiple pages per sheet.}
    @end{simple-table}
  @end{values}
  @begin{short}
    A flags for specifying which features the print dialog should offer.
  @end{short}
  If neither the @val[gtk:print-capabilities]{:generate-pdf} nor the
  @val[gtk:print-capabilities]{:generate-ps} value is specified, GTK assumes
  that all formats are supported.
  @see-class{gtk:print-unix-dialog}")

;;; ----------------------------------------------------------------------------
;;; GtkPrintUnixDialog
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintUnixDialog" print-unix-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_print_unix_dialog_get_type")
  ((current-page
    print-unix-dialog-current-page
    "current-page" "gint" t t)
   (embed-page-setup
    print-unix-dialog-embed-page-setup
    "embed-page-setup" "gboolean" t t)
   (has-selection
    print-unix-dialog-has-selection
    "has-selection" "gboolean" t t)
   (manual-capabilities
    print-unix-dialog-manual-capabilities
    "manual-capabilities" "GtkPrintCapabilities" t t)
   (page-setup
    print-unix-dialog-page-setup
    "page-setup" "GtkPageSetup" t t)
   (print-settings
    print-unix-dialog-print-settings
    "print-settings" "GtkPrintSettings" t t)
   (selected-printer
    print-unix-dialog-selected-printer
    "selected-printer" "GtkPrinter" t nil)
   (support-selection
    print-unix-dialog-support-selection
    "support-selection" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'print-unix-dialog 'type)
 "@version{2025-07-27}
  @begin{short}
    The @class{gtk:print-unix-dialog} widget implements a print dialog for
    platforms which do not provide a native print dialog, like Unix.
  @end{short}
  It can be used very much like any other GTK dialog, at the cost of the
  portability offered by the high-level printing API.

  @image[print-dialog]{Figure: GtkPrintUnixDialog}

  In order to print something with the @class{gtk:print-unix-dialog} widget,
  you need to use the @fun{gtk:print-unix-dialog-selected-printer} function to
  obtain a @class{gtk:printer} object and use it to construct a
  @class{gtk:print-job} object using the @fun{gtk:print-job-new} function.

  The @class{gtk:print-unix-dialog} widget uses the following response values:
  @begin[code]{table}
    @entry[:ok]{For the \"Print\" button.}
    @entry[:apply]{For the \"Preview\" button.}
    @entry[:cancel]{For the \"Cancel\" button.}
  @end{table}
  @begin[GtkPrintUnixDialog as GtkBuildable]{dictionary}
    The @class{gtk:print-unix-dialog} implementation of the
    @class{gtk:buildable} interface exposes its notebook internal children with
    the name @code{\"notebook\"}.

    @b{Example:} A @class{gtk:print-unix-dialog} UI definition fragment.
    @begin{pre}
<object class=\"GtkPrintUnixDialog\" id=\"dialog1\">
  <child internal-child=\"notebook\">
    <object class=\"GtkNotebook\" id=\"notebook\">
      <child>
        <object class=\"GtkLabel\" id=\"tabcontent\">
        <property name=\"label\">Content on notebook tab</property>
        </object>
      </child>
      <child type=\"tab\">
        <object class=\"GtkLabel\" id=\"tablabel\">
          <property name=\"label\">Tab label</property>
        </object>
        <packing>
          <property name=\"tab_expand\">False</property>
          <property name=\"tab_fill\">False</property>
        </packing>
      </child>
    </object>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:print-unix-dialog-new}
  @see-slot{gtk:print-unix-dialog-current-page}
  @see-slot{gtk:print-unix-dialog-embed-page-setup}
  @see-slot{gtk:print-unix-dialog-has-selection}
  @see-slot{gtk:print-unix-dialog-manual-capabilities}
  @see-slot{gtk:print-unix-dialog-page-setup}
  @see-slot{gtk:print-unix-dialog-print-settings}
  @see-slot{gtk:print-unix-dialog-selected-printer}
  @see-slot{gtk:print-unix-dialog-support-selection}
  @see-class{gtk:printer}
  @see-class{gtk:print-job}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:print-unix-dialog-current-page -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "current-page"
                                               'print-unix-dialog) t)
 "The @code{current-page} property of type @code{:int} (Read / Write) @br{}
  The current page in the document. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-current-page)
      "Accessor"
      (documentation 'print-unix-dialog-current-page 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-current-page object) => current-page}
  @syntax{(setf (gtk:print-unix-dialog-current-page object) current-page)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[current-page]{an integer for the current page number}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{current-page} slot of the
    @class{gtk:print-unix-dialog} class gets or sets the current page number of
    the print dialog.
  @end{short}
  If @arg{current-page} is not -1, this enables the current page choice for the
  range of pages to print.
  @see-class{gtk:print-unix-dialog}")

;;; --- gtk:print-unix-dialog-embed-page-setup ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "embed-page-setup"
                                               'print-unix-dialog) t)
 "The @code{embed-page-setup} property of type @code{:boolean}
 (Read / Write) @br{}
  @em{True} if page setup combos are embedded in the print dialog. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-embed-page-setup)
      "Accessor"
      (documentation 'print-unix-dialog-embed-page-setup 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-embed-page-setup object) => embed}
  @syntax{(setf (gtk:print-unix-dialog-embed-page-setup object) embed)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[embed]{a boolean whether embed page setup selection}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{embed-page-setup} slot of
    the @class{gtk:print-unix-dialog} class gets or sets whether page setup
    combos are embedded in the print dialog.
  @end{short}
  @see-class{gtk:print-unix-dialog}")

;;; --- gtk:print-unix-dialog-has-selection ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-selection"
                                               'print-unix-dialog) t)
 "The @code{has-selection} property of type @code{:boolean} (Read / Write) @br{}
  Whether the application has a selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-has-selection)
      "Accessor"
      (documentation 'print-unix-dialog-has-selection 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-has-selection object) => has-selection}
  @syntax{(setf (gtk:print-unix-dialog-has-selection object) has-selection)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[has-selection]{@em{true} indicates that a selection exists}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{has-selection} slot of the
    @class{gtk:print-unix-dialog} class gets or sets whether the application has
    a selection.
  @end{short}
  @see-class{gtk:print-unix-dialog}")

;;; --- gtk:print-unix-dialog-manual-capabilities ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "manual-capabilities"
                                               'print-unix-dialog) t)
 "The @code{manual-capabilities} property of type @sym{gtk:print-capabilities}
  (Read / Write) @br{}
  The capabilities the application can handle.")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-manual-capabilities)
      "Accessor"
      (documentation 'print-unix-dialog-manual-capabilities 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-manual-capabilities) => capabilities}
  @syntax{(setf (gtk:print-unix-dialog-manual-capabilities object) capabilities)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[capabilities]{a @sym{gtk:print-capabilities} value for the printing
    capabilities of the application}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{manual-capabilities} slot
    of the @class{gtk:print-unix-dialog} class gets or sets the capabilities the
    application can handle.
  @end{short}
  This lets you specify the printing capabilities your application supports.
  For instance, if you can handle scaling the output then you pass the
  @val[gtk:print-capabilities]{:scale} value. If you do not pass that, then the
  dialog will only let you select the scale if the printing system automatically
  handles scaling.
  @see-class{gtk:print-unix-dialog}
  @see-symbol{gtk:print-capabilities}")

;;; --- gtk:print-unix-dialog-page-setup ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-setup"
                                               'print-unix-dialog) t)
 "The @code{page-setup} property of type @class{gtk:page-setup}
  (Read / Write) @br{}
  The page setup to use.")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-page-setup)
      "Accessor"
      (documentation 'print-unix-dialog-page-setup 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-page-setup) => page-setup}
  @syntax{(setf (gtk:print-unix-dialog-page-setup object) page-setup)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[page-setup]{a @class{gtk:page-setup} object}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{page-setup} slot of the
    @class{gtk:print-unix-dialog} class gets or sets the page setup that is used
    by the print dialog.
  @end{short}
  @see-class{gtk:print-unix-dialog}
  @see-class{gtk:page-setup}")

;;; --- gtk:print-unix-dialog-print-settings -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "print-settings"
                                               'print-unix-dialog) t)
 "The @code{print-settings} property of type @class{gtk:print-settings}
  (Read / Write) @br{}
  The print settings used for initializing the dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-print-settings)
      "Accessor"
      (documentation 'print-unix-dialog-print-settings 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-print-settings object) => settings}
  @syntax{(setf (gtk:print-unix-dialog-print-settings object) settings)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{print-settings} slot of
    the @class{gtk:print-unix-dialog} class gets or sets the print settings that
    represents the current values in the print dialog.
  @end{short}

  Typically, this is used to restore saved print settings from a previous print
  operation before the print dialog is shown.
  @see-class{gtk:print-unix-dialog}
  @see-class{gtk:print-settings}")

;;; --- gtk:print-unix-dialog-selected-printer ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected-printer"
                                               'print-unix-dialog) t)
 "The @code{selected-printer} property of type @class{gtk:printer}
  (Read) @br{}
  The printer which is selected.")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-selected-printer)
      "Accessor"
      (documentation 'print-unix-dialog-selected-printer 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-selected-printer object) => printer}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[printer]{a @class{gtk:printer} object}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{selected-printer} slot of
    the @class{gtk:print-unix-dialog} class returns the currently selected
    printer.
  @end{short}
  @see-class{gtk:print-unix-dialog}
  @see-class{gtk:printer}")

;;; --- gtk:print-unix-dialog-support-selection --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "support-selection"
                                               'print-unix-dialog) t)
 "The @code{support-selection} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the dialog supports selection. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-unix-dialog-support-selection)
      "Accessor"
      (documentation 'print-unix-dialog-support-selection 'function)
 "@version{2025-09-28}
  @syntax{(gtk:print-unix-dialog-support-selection) => selection}
  @syntax{(setf (gtk:print-unix-dialog-support-selection object) selection)}
  @argument[object]{a @class{gtk:print-unix-dialog} widget}
  @argument[selection]{@em{true} to allow print selection}
  @begin{short}
    The accessor for the @slot[gtk:print-unix-dialog]{support-selection} slot
    of the @class{gtk:print-unix-dialog} class gets or sets whether the print
    dialog allows user to print a selection.
  @end{short}
  @see-class{gtk:print-unix-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_new
;;; ----------------------------------------------------------------------------

(defun print-unix-dialog-new (title parent)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} widget for the transient parent of
    the dialog, or @code{nil}}
  @return{The new @class{gtk:print-unix-dialog} widget.}
  @short{Creates a new print dialog.}
  @see-class{gtk:print-unix-dialog}
  @see-class{gtk:window}"
  (let ((dialog (make-instance 'print-unix-dialog)))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    dialog))

(export 'print-unix-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_settings
;;; gtk_print_unix_dialog_set_settings
;;; ----------------------------------------------------------------------------

(defun (setf print-unix-dialog-settings) (settings dialog)
  (setf (print-unix-dialog-print-settings dialog) settings))

(defun print-unix-dialog-settings (dialog)
 #+liber-documentation
 "@version{2024-02-18}
  @syntax{(gtk:print-unix-dialog-settings object) => settings}
  @syntax{(setf (gtk:print-unix-dialog-settings object) settings)}
  @argument[dialog]{a @class{gtk:print-unix-dialog} widget}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    The @fun{gtk:print-unix-dialog-settings} function gets the print settings
    that represents the current values in the print dialog.
  @end{short}
  The @setf{gtk:print-unix-dialog-settings} function sets the print settings
  for the print dialog.

  Typically, this is used to restore saved print settings from a previous print
  operation before the print dialog is shown.
  @begin[Notes]{dictionary}
    The @fun{gtk:print-unix-dialog-settings} function corresponds to the
    @fun{gtk:print-unix-dialog-print-settings} function.
  @end{dictionary}
  @see-class{gtk:print-unix-dialog}
  @see-function{gtk:print-unix-dialog-print-settings}"
  (print-unix-dialog-print-settings dialog))

(export 'print-unix-dialog-settings)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_add_custom_tab
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_unix_dialog_add_custom_tab"
               print-unix-dialog-add-custom-tab) :void
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[dialog]{a @class{gtk:print-unix-dialog} widget}
  @argument[child]{a @class{gtk:widget} object for the widget to put in the
    custom tab}
  @argument[tab-label]{a @class{gtk:widget} object to use as tab label}
  @begin{short}
    Adds a custom tab to the print dialog.
  @end{short}
  @see-class{gtk:print-unix-dialog}
  @see-class{gtk:widget}"
  (dialog (g:object print-unix-dialog))
  (child (g:object widget))
  (tab-label (g:object widget)))

(export 'print-unix-dialog-add-custom-tab)

;;; ----------------------------------------------------------------------------
;;; gtk_print_unix_dialog_get_page_setup_set
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_unix_dialog_get_page_setup_set"
               print-unix-dialog-page-setup-set) :boolean
 #+liber-documentation
 "@version{#2024-02-18}
  @argument[dialog]{a @class{gtk:print-unix-dialog} widget}
  @return{The boolean whether a page setup was set by the user.}
  @begin{short}
    Gets whether a page setup was set by the user.
  @end{short}
  @see-class{gtk:print-unix-dialog}"
  (dialog (g:object print-unix-dialog)))

(export 'print-unix-dialog-page-setup-set)

;;; --- End of file gtk4.print-unix-dialog.lisp --------------------------------
