;;; ----------------------------------------------------------------------------
;;; gtk4.print-job.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GtkPrintJob
;;;
;;;     Represents a print job
;;;
;;; Types and Values
;;;
;;;     GtkPrintJob
;;;
;;; Accessors
;;;
;;;     gtk_print_job_get_printer
;;;     gtk_print_job_get_settings
;;;     gtk_print_job_get_title
;;;     gtk_print_job_set_track_print_status
;;;     gtk_print_job_get_track_print_status
;;;
;;; Functions
;;;
;;;     GtkPrintJobCompleteFunc
;;;
;;;     gtk_print_job_new
;;;     gtk_print_job_get_status
;;;     gtk_print_job_set_source_file
;;;     gtk_print_job_set_source_fd
;;;     gtk_print_job_get_surface
;;;     gtk_print_job_send
;;;     gtk_print_job_get_pages
;;;     gtk_print_job_set_pages
;;;     gtk_print_job_get_page_ranges
;;;     gtk_print_job_set_page_ranges
;;;     gtk_print_job_get_page_set
;;;     gtk_print_job_set_page_set
;;;     gtk_print_job_get_num_copies
;;;     gtk_print_job_set_num_copies
;;;     gtk_print_job_get_scale
;;;     gtk_print_job_set_scale
;;;     gtk_print_job_get_n_up
;;;     gtk_print_job_set_n_up
;;;     gtk_print_job_get_n_up_layout
;;;     gtk_print_job_set_n_up_layout
;;;     gtk_print_job_get_rotate
;;;     gtk_print_job_set_rotate
;;;     gtk_print_job_get_collate
;;;     gtk_print_job_set_collate
;;;     gtk_print_job_get_reverse
;;;     gtk_print_job_set_reverse
;;;
;;; Properties
;;;
;;;     page-setup
;;;     printer
;;;     settings
;;;     title
;;;     track-print-status
;;;
;;; Signals
;;;
;;;     status-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkPrintJob
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPrintJob
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPrintJob" print-job
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_print_job_get_type")
  ((page-setup
    print-job-page-setup
    "page-setup" "GtkPageSetup" t t)
   (printer
    print-job-printer
    "printer" "GtkPrinter" t t)
   (settings
    print-job-settings
    "settings" "GtkPrintSettings" t t)
   (title
    print-job-title
    "title" "gchararray" t t)
   (track-print-status
    print-job-track-print-status
    "track-print-status" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'print-job 'type)
 "@version{2024-7-7}
  @begin{short}
    The @class{gtk:print-job} object represents a job that is sent to a printer.
  @end{short}
  You only need to deal directly with print jobs if you use the non-portable
  @class{gtk:print-unix-dialog} API.

  Use the @fun{gtk:print-job-surface} function to obtain the Cairo surface onto
  which the pages must be drawn. Use the @fun{gtk:print-job-send} function to
  send the finished job to the printer. If you do not use Cairo the
  @class{gtk:print-job} object also supports printing of manually generated
  PostScript, via the @fun{gtk:print-job-set-source-file} function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"status-changed\" signal}
      @begin{pre}
lambda (job)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[job]{The @class{gtk:print-job} object on which the signal was
        emitted.}
      @end{table}
      Gets emitted when the status of a job changes. The signal handler can use
      the @fun{gtk:print-job-status} function to obtain the new status.
  @end{dictionary}
  @see-constructor{gtk:print-job-new}
  @see-slot{gtk:print-job-page-setup}
  @see-slot{gtk:print-job-printer}
  @see-slot{gtk:print-job-settings}
  @see-slot{gtk:print-job-title}
  @see-slot{gtk:print-job-track-print-status}
  @see-class{gtk:print-unix-dialog}
  @see-function{gtk:print-job-surface}
  @see-function{gtk:print-job-send}
  @see-function{gtk:print-job-set-source-file}
  @see-function{gtk:print-job-status}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:print-job-page-setup -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-setup" 'print-job) t)
 "The @code{page-setup} property of type @class{gtk:page-setup}
  (Read / Write / Construct Only) @br{}
  The page setup.")

#+liber-documentation
(setf (liber:alias-for-function 'print-job-page-setup)
      "Accessor"
      (documentation 'print-job-page-setup 'function)
 "@version{2024-7-7}
  @syntax{(gtk:print-job-page-setup object) => setup}
  @argument[object]{a @class{gtk:print-job} object}
  @argument[setup]{a @class{gtk:page-setup} object}
  @begin{short}
    Accessor of the @slot[gtk:print-job]{page-setup} slot of the
    @class{gtk:print-job} class.
  @end{short}
  @see-class{gtk:print-job}
  @see-class{gtk:page-setup}")

;;; --- gtk:print-job-printer --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "printer" 'print-job) t)
 "The @code{printer} property of type @class{gtk:printer}
  (Read / Write / Construct Only) @br{}
  The printer to print the job to.")

#+liber-documentation
(setf (liber:alias-for-function 'print-job-printer)
      "Accessor"
      (documentation 'print-job-printer 'function)
 "@version{2024-7-7}
  @syntax{(gtk:print-job-printer object) => printer}
  @argument[object]{a @class{gtk:print-job} object}
  @argument[printer]{a @class{gtk:printer} object}
  @begin{short}
    Accessor of the @slot[gtk:print-job]{printer} of the @class{gtk:print-job}
    class.
  @end{short}
  Gets the printer of the print job.
  @see-class{gtk:print-job}
  @see-class{gtk:printer}")

;;; --- gtk:print-job-settings -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "settings" 'print-job) t)
 "The @code{settings} property of type @class{gtk:print-settings}
  (Read / Write / Construct Only) @br{}
  The printer settings.")

#+liber-documentation
(setf (liber:alias-for-function 'print-job-settings)
      "Accessor"
      (documentation 'print-job-settings 'function)
 "@version{2024-7-7}
  @syntax{(gtk:print-job-settings object) => settings}
  @argument[object]{a @class{gtk:print-job} object}
  @argument[settings]{a @class{gtk:print-settings} object}
  @begin{short}
    Accessor of the @slot[gtk:print-job]{settings} slot of the
    @class{gtk:print-job} class.
  @end{short}
  Gets the print settings of the print job.
  @see-class{gtk:print-job}
  @see-class{gtk:print-settings}")

;;; --- gtk:print-job-title ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'print-job) t)
 "The @code{title} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The title of the print job. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'print-job-title)
      "Accessor"
      (documentation 'print-job-title 'function)
 "@version{2024-7-7}
  @syntax{(gtk:print-job-title object) => title}
  @argument[object]{a @class{gtk:print-job} object}
  @argument[title]{a string with the job title.}
  @begin{short}
    Accessor of the @slot[gtk:print-job]{title} slot of the
    @class{gtk:print-job} class.
  @end{short}
  Gets the job title.
  @see-class{gtk:print-job}")

;;; --- gtk:print-job-track-print-status ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "track-print-status"
                                               'print-job) t)
 "The @code{track-print-status} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the print job will continue to emit @code{\"status-changed\"}
  signals after the print data has been sent to the printer or print server.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'print-job-track-print-status)
      "Accessor"
      (documentation 'print-job-track-print-status 'function)
 "@version{2024-7-7}
  @syntax{(gtk:print-job-track-print-status object) => status}
  @syntax{(setf (gtk:print-job-track-print-status object) status)}
  @argument[object]{a @class{gtk:print-job} object}
  @argument[status]{@em{true} to track status after printing}
  @begin{short}
    Accessor of the @slot[gtk:print-job]{track-print-status} slot of the
    @class{gtk:print-job} class.
  @end{short}
  The @fun{gtk:print-job-track-print-status} function returns whether jobs will
  be tracked after printing.

  If the @arg{status} argument is @em{true}, the print job will try to continue
  report on the status of the print job in the printer queues and printer.

  This can allow your application to show things like \"out of paper\" issues,
  and when the print job actually reaches the printer. This function is often
  implemented using some form of polling, so it should not be enabled unless
  needed.
  @see-class{gtk:print-job}")

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_new
;;; ----------------------------------------------------------------------------

(declaim (inline print-job-new))

(defun print-job-new (title printer settings page-setup)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[title]{a string with the job title}
  @argument[printer]{a @class{gtk:printer} object}
  @argument[settings]{a @class{gtk:print-settings} object}
  @argument[setup]{a @class{gtk:page-setup} object}
  @return{The new @class{gtk:print-job} object.}
  @short{Creates a new @class{gtk:print-job} object.}
  @see-class{gtk:print-job}
  @see-class{gtk:printer}
  @see-class{gtk:print-settings}
  @see-class{gtk:page-setup}"
  (make-instance 'print-job
                 :title title
                 :printer printer
                 :settings settings
                 :page-setup page-setup))

(export 'print-job-new)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_status
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_job_get_status" print-job-status) print-status
 #+liber-documentation
 "@version{2024-7-7}
  @argument[job]{a @class{gtk:print-job} object}
  @return{The @symbol{gtk:print-status} value with the status of @arg{job}.}
  @short{Gets the status of the print job.}
  @see-class{gtk:print-job}
  @see-symbol{gtk:print-status}"
  (job (g:object print-job)))

(export 'print-job-status)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_source_fd ()
;;;
;;; gboolean
;;; gtk_print_job_set_source_fd (GtkPrintJob *job,
;;;                              int fd,
;;;                              GError **error);
;;;
;;; Make the GtkPrintJob send an existing document to the printing system. The
;;; file can be in any format understood by the platforms printing system
;;; (typically PostScript, but on many platforms PDF may work too). See
;;; gtk_printer_accepts_pdf() and gtk_printer_accepts_ps().
;;;
;;; This is similar to gtk_print_job_set_source_file(), but takes expects an
;;; open file descriptor for the file, instead of a filename.
;;;
;;; job :
;;;     a GtkPrintJob
;;;
;;; fd :
;;;     a file descriptor
;;;
;;; error :
;;;     return location for errors
;;;
;;; Returns :
;;;     FALSE if an error occurred
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_set_source_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_job_set_source_file" %print-job-set-source-file)
    :boolean
  (job (g:object print-job))
  (filename :string)
  (err :pointer))

(defun print-job-set-source-file (job filename)
 #+liber-documentation
 "@version{2024-11-21}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[filename]{a pathname or namestring with the file to be printed}
  @return{@em{False} if an error occurred.}
  @begin{short}
    Make the print job send an existing document to the printing system.
  @end{short}
  The file can be in any format understood by the platforms printing system,
  typically PostScript, but on many platforms PDF may work too. See the
  @fun{gtk:printer-accepts-pdf} and @fun{gtk:printer-accepts-ps} functions.
  @see-class{gtk:print-job}
  @see-function{gtk:printer-accepts-pdf}
  @see-function{gtk:printer-accepts-ps}"
  (glib:with-error (err)
    (%print-job-set-source-file job (namestring filename) err)))

(export 'print-job-set-source-file)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_job_get_surface" %print-job-get-surface)
    (:pointer (:struct cairo:surface-t))
  (job (g:object print-job))
  (err :pointer))

(defun print-job-surface (job)
 #+liber-documentation
 "@version{2024-11-21}
  @argument[job]{a @class{gtk:print-job} object}
  @return{The @symbol{cairo:surface-t} instance for the Cairo surface of
    @arg{job}.}
  @begin{short}
    Gets a Cairo surface onto which the pages of the print job should be
    rendered.
  @end{short}
  @see-class{gtk:print-job}
  @see-symbol{cairo:surface-t}"
  (glib:with-error (err)
    (%print-job-get-surface job err)))

(export 'print-job-surface)

;;; ----------------------------------------------------------------------------
;;; GtkPrintJobCompleteFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback print-job-complete-func :void
    ((job (g:object print-job))
     (data :pointer)
     (err :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func job err)))

#+liber-documentation
(setf (liber:alias-for-symbol 'print-job-complete-func)
      "Callback"
      (liber:symbol-documentation 'print-job-complete-func)
 "@version{2024-7-7}
  @syntax{lambda (job)}
  @argument[job]{a @class{gtk:print-job} object}
  @begin{short}
    The type of callback that is passed to the @fun{gtk:print-job-send}
    function.
  @end{short}
  It is called when the print job has been completely sent.
  @see-class{gtk:print-job}
  @see-function{gtk:print-job-send}")

(export 'print-job-complete-func)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_send
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_print_job_send" %print-job-send) :void
  (job (g:object print-job))
  (func :pointer)
  (data :pointer)
  (destroy :pointer))

(defun print-job-send (job func)
 #+liber-documentation
 "@version{2024-7-7}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[func]{a @symbol{gtk:print-job-complete-func} callback function to
    call when the job completes or an error occurs}
  @short{Sends the print job off to the printer.}
  @see-class{gtk:print-job}
  @see-symbol{gtk:print-job-complete-func}"
  (%print-job-send job (cffi:callback print-job-complete-func)
                       (glib:allocate-stable-pointer func)
                       (cffi:callback glib:stable-pointer-destroy-notify)))

(export 'print-job-send)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_pages
;;; gtk_print_job_set_pages
;;; ----------------------------------------------------------------------------

(defun (setf print-job-pages) (pages job)
  (cffi:foreign-funcall "gtk_print_job_set_pages"
                        (g:object print-job) job
                        print-pages pages
                        :void)
  pages)

(cffi:defcfun ("gtk_print_job_get_pages" print-job-pages) print-pages
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-pages job) => pages}
  @syntax{(setf (gtk:print-job-pages job) pages)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[pages]{a @symbol{gtk:print-pages} setting}
  @begin{short}
    The @fun{gtk:print-job-pages} function gets the pages setting for the print
    job.
  @end{short}
  The @setf{gtk:print-job-pages} function sets the pages setting for the print
  job.
  @see-class{gtk:print-job}
  @see-symbol{gtk:print-pages}"
  (job (g:object print-job)))

(export 'print-job-pages)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_ranges
;;; gtk_print_job_set_page_ranges
;;; ----------------------------------------------------------------------------

(defun (setf print-job-page-ranges) (ranges job)
  (setf (print-settings-page-ranges (print-job-settings job)) ranges))

(defun print-job-page-ranges (job)
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-page-ranges job) => ranges}
  @syntax{(setf (gtk:print-job-page-ranges job) ranges)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[ranges]{a list of integers with the page ranges}
  @begin{short}
    Accessor of the page ranges for the print job.
  @end{short}
  The @fun{gtk:print-job-page-ranges} function gets the page ranges for the
  print job. The @setf{gtk:print-job-page-ranges} function sets the page ranges.
  See the @fun{gtk:print-settings-page-ranges} function.
  @see-class{gtk:print-job}
  @see-function{gtk:print-settings-page-ranges}"
  (print-settings-page-ranges (print-job-settings job)))

(export 'print-job-page-ranges)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_page_set
;;; gtk_print_job_set_page_set
;;; ----------------------------------------------------------------------------

(defun (setf print-job-page-set) (page-set job)
  (cffi:foreign-funcall "gtk_print_job_set_page_set"
                        (g:object print-job) job
                        page-set page-set
                        :void)
  page-set)

(cffi:defcfun ("gtk_print_job_get_page_set" print-job-page-set) page-set
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-page-set job) => page-set}
  @syntax{(setf (gtk:print-job-page-set job) page-set)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[page-set]{a @symbol{gtk:page-set} setting}
  @begin{short}
    The @fun{gtk:print-job-page-set} function gets the setting for the print
    job.
  @end{short}
  The @setf{gtk:print-job-page-set} function sets the setting for the print job.
  @see-class{gtk:print-job}
  @see-symbol{gtk:page-set}"
  (job (g:object print-job)))

(export 'print-job-page-set)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_num_copies
;;; gtk_print_job_set_num_copies
;;; ----------------------------------------------------------------------------

(defun (setf print-job-num-copies) (num-copies job)
  (cffi:foreign-funcall "gtk_print_job_set_num_copies"
                        (g:object print-job) job
                        :int num-copies
                        :void)
  num-copies)

(cffi:defcfun ("gtk_print_job_get_num_copies" print-job-num-copies) :int
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-num-copies job) => num-copies}
  @syntax{(setf (gtk:print-job-num-copies job) num-copies)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[num-copies]{an integer with the number of copies}
  @begin{short}
    Accessor of the number of copies for the print job.
  @end{short}
  The @fun{gtk:print-job-num-copies} function gets the number of copies of the
  print job. The @setf{gtk:print-job-num-copies} function sets the number of
  copies for the print job.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-num-copies)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_scale
;;; gtk_print_job_set_scale
;;; ----------------------------------------------------------------------------

(defun (setf print-job-scale) (scale job)
  (let ((scale (coerce scale 'double-float)))
    (cffi:foreign-funcall "gtk_print_job_set_scale"
                          (g:object print-job) job
                          :double scale
                          :void)
    scale))

(cffi:defcfun ("gtk_print_job_get_scale" print-job-scale) :double
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-scale job) => scale}
  @syntax{(setf (gtk:print-job-scale job) scale)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[scale]{a number coerce to a double float with the scale}
  @begin{short}
    The @fun{gtk:print-job-scale} function gets the scale for the print job,
    where 1.0 means unscaled.
  @end{short}
  The @setf{gtk:print-job-scale} function sets the scale for the print job.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up
;;; gtk_print_job_set_n_up
;;; ----------------------------------------------------------------------------

(defun (setf print-job-n-up) (n-up job)
  (cffi:foreign-funcall "gtk_print_job_set_n_up"
                        (g:object print-job) job
                        :uint n-up
                        :void)
  n-up)

(cffi:defcfun ("gtk_print_job_get_n_up" print-job-n-up) :uint
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-n-up job) => n-up}
  @syntax{(setf (gtk:print-job-n-up job) n-up)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[n-up]{an unsigned integer with the n-up value}
  @begin{short}
    The @fun{gtk:print-job-n-up} function gets the n-up setting for the print
    job.
  @end{short}
  The @setf{gtk:print-job-n-up} function sets the n-up setting for the print
  job.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-n-up)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_n_up_layout
;;; gtk_print_job_set_n_up_layout
;;; ----------------------------------------------------------------------------

(defun (setf print-job-n-up-layout) (layout job)
  (cffi:foreign-funcall "gtk_print_job_set_n_up_layout"
                        (g:object print-job) job
                        number-up-layout layout
                        :void)
  layout)

(cffi:defcfun ("gtk_print_job_get_n_up_layout" print-job-n-up-layout)
    number-up-layout
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-n-up-layout job) => layout}
  @syntax{(setf (gtk:print-job-n-up-layout job) layout)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[layout]{a @symbol{gtk:number-up-layout} value with the layout
    setting}
  @begin{short}
    The @fun{gtk:print-job-n-up-layout} function gets the layout setting for
    the print job.
  @end{short}
  The @setf{gtk:print-job-n-up-layout} function sets the layout setting for the
  print job.
  @see-class{gtk:print-job}
  @see-symbol{gtk:number-up-layout}"
  (job (g:object print-job)))

(export 'print-job-n-up-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_rotate
;;; gtk_print_job_set_rotate
;;; ----------------------------------------------------------------------------

(defun (setf print-job-rotate) (rotate job)
  (cffi:foreign-funcall "gtk_print_job_set_rotate"
                        (g:object print-job) job
                        :boolean rotate
                        :void)
  rotate)

(cffi:defcfun ("gtk_print_job_get_rotate" print-job-rotate) :boolean
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-rotate job) => rotate}
  @syntax{(setf (gtk:print-job-rotate job) rotate)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[rotate]{a boolean whether to print rotated}
  @begin{short}
    The @fun{gtk:print-job-rotate} function gets whether the job is printed
    rotated.
  @end{short}
  The @setf{gtk:print-job-rotate} function sets whether the job is printed
  rotated.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_collate
;;; gtk_print_job_set_collate
;;; ----------------------------------------------------------------------------

(defun (setf print-job-collate) (collate job)
  (cffi:foreign-funcall "gtk_print_job_set_collate"
                        (g:object print-job) job
                        :boolean collate
                        :void)
  collate)

(cffi:defcfun ("gtk_print_job_get_collate" print-job-collate) :boolean
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-collate job) => collate}
  @syntax{(setf (gtk:print-job-collate job) collate)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[collate]{a boolean whether the job is printed collated}
  @begin{short}
    The @fun{gtk:print-job-collate} function gets whether the job is printed
    collated.
  @end{short}
  The @setf{gtk:print-job-collate} function sets whether the job is printed
  collated.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-collate)

;;; ----------------------------------------------------------------------------
;;; gtk_print_job_get_reverse
;;; gtk_print_job_set_reverse
;;; ----------------------------------------------------------------------------

(defun (setf print-job-reverse) (reverse job)
  (cffi:foreign-funcall "gtk_print_job_set_reverse"
                        (g:object print-job) job
                        :boolean reverse
                        :void)
  reverse)

(cffi:defcfun ("gtk_print_job_get_reverse" print-job-reverse) :boolean
 #+liber-documentation
 "@version{2024-7-7}
  @syntax{(gtk:print-job-reverse job) => reverse}
  @syntax{(setf (gtk:print-job-reverse job) reverse)}
  @argument[job]{a @class{gtk:print-job} object}
  @argument[reverse]{a boolean whether the job is printed reversed}
  @begin{short}
    The @fun{gtk:print-job-reverse} function gets whether the job is printed
    reversed.
  @end{short}
  The @setf{gtk:print-job-reverse} function sets whether the job is printed
  reversed.
  @see-class{gtk:print-job}"
  (job (g:object print-job)))

(export 'print-job-reverse)

;;; --- End of file gtk4.print-job.lisp ----------------------------------------
