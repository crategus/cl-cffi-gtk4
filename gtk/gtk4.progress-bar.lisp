;;; ----------------------------------------------------------------------------
;;; gtk4.progress-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; cersion 4.20 and modified to document the Lisp binding to the GTK library,
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
;;; GtkProgressBar
;;;
;;;     A widget that indicates progress visually
;;;
;;; Types and Values
;;;
;;;     GtkProgressBar
;;;
;;; Accessors
;;;
;;;     gtk_progress_bar_set_ellipsize
;;;     gtk_progress_bar_get_ellipsize
;;;     gtk_progress_bar_set_fraction
;;;     gtk_progress_bar_get_fraction
;;;     gtk_progress_bar_set_inverted
;;;     gtk_progress_bar_get_inverted
;;;     gtk_progress_bar_set_pulse_step
;;;     gtk_progress_bar_get_pulse_step
;;;     gtk_progress_bar_set_show_text
;;;     gtk_progress_bar_get_show_text
;;;     gtk_progress_bar_set_text
;;;     gtk_progress_bar_get_text
;;;
;;; Functions
;;;
;;;     gtk_progress_bar_new
;;;     gtk_progress_bar_pulse
;;;
;;; Properties
;;;
;;;     ellipsize
;;;     fraction
;;;     inverted
;;;     pulse-step
;;;     show-text
;;;     text
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkProgressBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkAcessibleRange                                  Since 4.10
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkProgressBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkProgressBar" progress-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessibleRange"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_progress_bar_get_type")
  ((ellipsize
    progress-bar-ellipsize
    "ellipsize" "PangoEllipsize" t t)
   (fraction
    progress-bar-fraction
    "fraction" "gdouble" t t)
   (inverted
    progress-bar-inverted
    "inverted" "gboolean" t t)
   (pulse-step
    progress-bar-pulse-step
    "pulse-step" "gdouble" t t)
   (show-text
    progress-bar-show-text
    "show-text" "gboolean" t t)
   (text
    progress-bar-text
    "text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'progress-bar 'type)
 "@version{2025-12-09}
  @begin{short}
    The @class{gtk:progress-bar} widget is typically used to display the
    progress of a long running operation.
  @end{short}
  It provides a visual clue that processing is underway. The
  @class{gtk:progress-bar} widget can be used in two different modes: percentage
  mode and activity mode.

  @image[progress-bar]{Figure: GtkProgressBar}

  When an application can determine how much work needs to take place, for
  example, read a fixed number of bytes from a file, and can monitor its
  progress, it can use the progress bar in percentage mode and the user sees a
  growing bar indicating the percentage of the work that has been completed. In
  this mode, the application is required to call the
  @fun{gtk:progress-bar-fraction} function periodically to update the progress
  bar.

  When an application has no accurate way of knowing the amount of work to do,
  it can use the progress bar in activity mode, which shows activity by a block
  moving back and forth within the progress area. In this mode, the application
  is required to call the @fun{gtk:progress-bar-pulse} function periodically to
  update the progress bar.

  There is quite a bit of flexibility provided to control the appearance of
  the progress bar. Functions are provided to control the orientation of the
  progress bar, optional text can be displayed along with the progress bar, and
  the step size used in activity mode can be set.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
progressbar[.osd]
├── [text]
╰── trough[.empty][.full]
      ╰── progress[.pulse]
    @end{pre}
    The @class{gtk:progress-bar} implementation has a main CSS node with name
    @code{progressbar} and subnodes with names @code{text} and @code{trough},
    of which the latter has a subnode named @code{progress}. The @code{text}
    subnode is only present if text is shown. The @code{progress} subnode has
    the @code{.pulse} style class when in activity mode. It gets the
    @code{.left}, @code{.right}, @code{.top} or @code{.bottom} style classes
    added when the progress 'touches' the corresponding end of the progress bar.
    The @code{.osd} style class on the progress bar node is for use in overlays
    like the one Epiphany has for page loading progress.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:progress-bar} implementation uses the
    @val[gtk:accessible-role]{:progress-bar} role.
  @end{dictionary}
  @see-constructor{gtk:progress-bar-new}
  @see-slot{gtk:progress-bar-ellipsize}
  @see-slot{gtk:progress-bar-fraction}
  @see-slot{gtk:progress-bar-inverted}
  @see-slot{gtk:progress-bar-pulse-step}
  @see-slot{gtk:progress-bar-show-text}
  @see-slot{gtk:progress-bar-text}
  @see-class{gtk:spinner}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:progress-bar-ellipsize ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize" 'progress-bar) t)
 "The @code{ellipsize} property of type @sym{pango:ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the progress bar does not
  have enough room to display the entire string, specified as a value of the
  @sym{pango:ellipsize-mode} enumeration. Note that setting this property to
  a value other than @val[pango:ellipsize-mode]{:none} has the side-effect that
  the progress bar requests only enough space to display the ellipsis (\"...\").
  Another means to set the width of the progress bar is the
  @fun{gtk:widget-size-request} function. @br{}
  Default value: @val[pango:ellipsize-mode]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-ellipsize)
      "Accessor"
      (documentation 'progress-bar-ellipsize 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-ellipsize object) => mode}
  @syntax{(setf (gtk:progress-bar-ellipsize object) mode)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[mode]{a value of the @sym{pango:ellipsize-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{ellipsize} slot of the
    @class{gtk:progress-bar} class gets or sets the mode used to ellipsize the
    text if there is not enough space to render the entire string.
  @end{short}
  @see-class{gtk:progress-bar}
  @see-symbol{pango:ellipsize-mode}")

;;; --- gtk:progress-bar-fraction ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fraction" 'progress-bar) t)
 "The @code{fraction} property of type @code{:double} (Read / Write) @br{}
  The fraction of total work that has been completed. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-fraction)
      "Accessor"
      (documentation 'progress-bar-fraction 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-fraction object) => fraction}
  @syntax{(setf (gtk:progress-bar-fraction object) fraction)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[fraction]{a number coerced to a double float for the fraction of
    the task that is been completed}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{fraction} slot of the
    @class{gtk:progress-bar} class gets or sets the fraction of total work that
    has been completed.
  @end{short}
  The fraction should be between 0.0 and 1.0, inclusive.
  @see-class{gtk:progress-bar}")

;;; --- gtk:progress-bar-inverted ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inverted" 'progress-bar) t)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Invert the direction in which the progress bar grows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-inverted)
      "Accessor"
      (documentation 'progress-bar-inverted 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-inverted object) => inverted}
  @syntax{(setf (gtk:progress-bar-inverted object) inverted)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[inverted]{@em{true} to invert the progress bar}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{inverted} slot of the
    @class{gtk:progress-bar} class gets or sets the invert direction in which
    the progess bar grows.
  @end{short}
  Progress bars normally grow from top to bottom or left to right. Inverted
  progress bars grow in the opposite direction.
  @see-class{gtk:progress-bar}")

;;; --- gtk:progress-bar-pulse-step --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pulse-step" 'progress-bar) t)
 "The @code{pulse-step} property of type @code{:double} (Read / Write) @br{}
  The fraction of total progress to move the bouncing block when pulsed. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.1")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-pulse-step)
      "Accessor"
      (documentation 'progress-bar-pulse-step 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-pulse-step object) => step}
  @syntax{(setf (gtk:progress-bar-pulse-step object) step)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[step]{a number coerced to a double float for the fraction between
    0.0 and 1.0}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{pulse-step} slot of the
    @class{gtk:progress-bar} class gets or sets the fraction of total progress
    bar length to move the bouncing block for each call to the
    @fun{gtk:progress-bar-pulse} function.
  @end{short}
  The pulse step is a fraction from 0.0 to 1.0.
  @see-class{gtk:progress-bar}
  @see-function{gtk:progress-bar-pulse}")

;;; --- gtk:progress-bar-show-text ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-text" 'progress-bar) t)
 "The @code{show-text} property of type @code{:boolean} (Read / Write) @br{}
  Sets whether the progress bar will show text superimposed over the progress
  bar. The shown text is either the value of the @slot[gtk:progress-bar]{text}
  property or, if that is @code{nil}, the @slot[gtk:progress-bar]{fraction}
  value, as a percentage. To make a progress bar that is styled and sized
  suitably for containing text, even if the actual text is blank, set the
  @slot[gtk:progress-bar]{show-text} property to @em{true} and the
  @slot[gtk:progres-bar]{text} property to the empty string, not @code{nil}.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-show-text)
      "Accessor"
      (documentation 'progress-bar-show-text 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-show-text object) => setting}
  @syntax{(setf (gtk:progress-bar-show-text object) setting)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[setting]{a boolean whether to show superimposed text}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{show-text} slot of the
    @class{gtk:progress-bar} class gets or sets whether the progress bar will
    show text superimposed over the progress bar.
  @end{short}
  The shown text is either the value of the @slot[gtk:progress-bar]{text}
  property or, if that is @code{nil}, the @slot[gtk:progress-bar]{fraction}
  value, as a percentage.

  To make a progress bar that is styled and sized suitably for containing text,
  even if the actual text is blank, set the @slot[gtk:progress-bar]{show-text}
  property to @em{true} and the @slot[gtk:progress-bar]{text} property to the
  empty string, not @code{nil}.
  @see-class{gtk:progress-bar}
  @see-function{gtk:progress-bar-text}
  @see-function{gtk:progress-bar-fraction}")

;;; --- gtk:progress-bar-text --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'progress-bar) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The text to be displayed in the progress bar. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'progress-bar-text)
      "Accessor"
      (documentation 'progress-bar-text 'function)
 "@version{2025-08-04}
  @syntax{(gtk:progress-bar-text object) => text}
  @syntax{(setf (gtk:progress-bar-text object) text)}
  @argument[object]{a @class{gtk:progress-bar} widget}
  @argument[text]{a UTF-8 string}
  @begin{short}
    The accessor for the @slot[gtk:progress-bar]{text} slot of the
    @class{gtk:progress-bar} class gets or sets the text displayed superimposed
    on the progress bar.
  @end{short}
  If the @arg{text} argument is @code{nil} and the
  @slot[gtk:progress-bar]{show-text} property is @em{true}, the current value
  of the @slot[gtk:progress-bar]{fraction} property will be displayed as a
  percentage.

  If the @arg{text} argument is non-@code{nil} and the
  @slot[gtk:progress-bar]{show-text} property is @em{true}, the text will be
  displayed. In this case, it will not display the progress percentage. If text
  is the empty string, the progress bar will still be styled and sized suitably
  for containing text, as long as the @slot[gtk:progress-bar]{show-text}
  property is @em{true}.
  @see-class{gtk:progress-bar}
  @see-function{gtk:progress-bar-fraction}
  @see-function{gtk:progress-bar-show-text}")

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline progress-bar-new))

(defun progress-bar-new ()
 #+liber-documentation
 "@version{2025-05-30}
  @return{The new @class{gtk:progress-bar} widget.}
  @short{Creates a new progress bar.}
  @see-class{gtk:progress-bar}"
  (make-instance 'progress-bar))

(export 'progress-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_progress_bar_pulse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_progress_bar_pulse" progress-bar-pulse) :void
 #+liber-documentation
 "@version{2025-05-30}
  @argument[bar]{a @class{gtk:progress-bar} widget}
  @begin{short}
    Indicates that some progress has been made, but you do not know how much.
  @end{short}
  Causes the progress bar to enter \"activity mode\", where a block bounces
  back and forth.

  Each call to the @fun{gtk:progress-bar-pulse} function causes the block to
  move by a little bit, the amount of movement per pulse is determined by the
  @slot[gtk:progress-bar]{pulse-step} property.
  @see-class{gtk:progress-bar}
  @see-function{gtk:progress-bar-pulse-step}"
  (bar (g:object progress-bar)))

(export 'progress-bar-pulse)

;;; --- End of file gtk4.progress-bar.lisp -------------------------------------
