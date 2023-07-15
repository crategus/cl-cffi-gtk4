;;; ----------------------------------------------------------------------------
;;; gtk4.adjustment.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkAdjustment
;;;
;;;     A representation of an adjustable bounded value
;;;
;;; Types and Values
;;;
;;;     GtkAdjustment
;;;
;;; Accessors
;;;
;;;     gtk_adjustment_get_lower
;;;     gtk_adjustment_set_lower
;;;     gtk_adjustment_get_page_increment
;;;     gtk_adjustment_set_page_increment
;;;     gtk_adjustment_get_page_size
;;;     gtk_adjustment_set_page_size
;;;     gtk_adjustment_get_step_increment
;;;     gtk_adjustment_set_step_increment
;;;     gtk_adjustment_get_upper
;;;     gtk_adjustment_set_upper
;;;     gtk_adjustment_get_value
;;;     gtk_adjustment_set_value
;;;
;;; Functions
;;;
;;;     gtk_adjustment_new
;;;     gtk_adjustment_clamp_page
;;;     gtk_adjustment_configure
;;;     gtk_adjustment_get_minimum_increment
;;;
;;; Properties
;;;
;;;     lower
;;;     page-increment
;;;     page-size
;;;     step-increment
;;;     upper
;;;     value
;;;
;;; Signals
;;;
;;;     changed
;;;     value-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkAdjustment
;;; ----------------------------------------------------------------------------

(in-package :gtk)

(gobject:define-g-object-class "GtkAdjustment" adjustment
  (:superclass g:initially-unowned
   :export t
   :interfaces nil
   :type-initializer "gtk_adjustment_get_type")
  ((lower
    adjustment-lower
    "lower" "gdouble" t t)
   (page-increment
    adjustment-page-increment
    "page-increment" "gdouble" t t)
   (page-size
    adjustment-page-size
    "page-size" "gdouble" t t)
   (step-increment
    adjustment-step-increment
    "step-increment" "gdouble" t t)
   (upper
    adjustment-upper
    "upper" "gdouble" t t)
   (value
    adjustment-value
    "value" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'adjustment 'type)
 "@version{#2022-11-13}
  @begin{short}
    The @sym{gtk:adjustment} object represents a value which has an associated
    lower and upper bound, together with step and page increments, and a page
    size.
  @end{short}
  It is used within several widgets, including the @class{gtk:spin-button},
  @class{gtk:viewport}, and @class{gtk:range} widget, which is a base class for
  the @class{gtk:scale} widget.

  The @sym{gtk:adjustment} object does not update the value itself. Instead it
  is left up to the owner of the @sym{gtk:adjustment} object to control the
  value.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (adjustment)    :no-recurse
      @end{pre}
      Emitted when one or more of the adjustment properties have been changed,
      other than the @code{value} property.
      @begin[code]{table}
        @entry[adjustment]{The @sym{gtk:adjustment} object which received the
          signal.}
      @end{table}
    @subheading{The \"value-changed\" signal}
      @begin{pre}
lambda (adjustment)    :no-recurse
      @end{pre}
      Emitted when the @code{value} property of the adjustment has been changed.
      @begin[code]{table}
        @entry[adjustment]{The @sym{gtk:adjustment} object which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:adjustment-lower}
  @see-slot{gtk:adjustment-page-increment}
  @see-slot{gtk:adjustment-page-size}
  @see-slot{gtk:adjustment-step-increment}
  @see-slot{gtk:adjustment-upper}
  @see-slot{gtk:adjustment-value}
  @see-constructor{gtk:adjustment-new}
  @see-class{gtk:range}
  @see-class{gtk:viewport}
  @see-class{gtk:spin-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- adjustment-lower ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "lower" 'adjustment) t)
 "The @code{lower} property of type @code{:double} (Read / Write) @br{}
  The minimum value of the adjustment. @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-lower)
      "Accessor"
      (documentation 'adjustment-lower 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-lower object) => lower}
  @syntax[]{(setf (gtk:adjustment-lower object) lower)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[lower]{a double float with the minimum value}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{lower} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-lower} function retrieves the minimum value of the
  adjustment. The @sym{(setf gtk:adjustment-lower)} function sets the minimum
  value of the adjustment.

  When setting multiple adjustment properties via their individual setters,
  multiple \"changed\" signals will be emitted. However, since the emission of
  the \"changed\" signal is tied to the emission of the \"notify\" signals of
  the changed properties, it is possible to compress the \"changed\"
  signals into one by calling the @fun{g:object-freeze-notify} and
  @fun{g:object-thaw-notify} functions around the calls to the individual
  setters.

  Alternatively, using the @fun{gtk:adjustment-configure} function has the same
  effect of compressing \"changed\" emissions.
  @see-class{gtk:adjustment}
  @see-function{g:object-freeze-notify}
  @see-function{g:object-thaw-notify}
  @see-function{gtk:adjustment-configure}")

;;; --- adjustment-page-increment ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-increment" 'adjustment) t)
 "The @code{page-increment} property of type @code{:double} (Read / Write) @br{}
  The page increment of the adjustment. @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-page-increment)
      "Accessor"
      (documentation 'adjustment-page-increment 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-page-increment object) => page-increment}
  @syntax[]{(setf (gtk:adjustment-page-increment object) page-increment)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[page-increment]{a double float with the page increment}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{page-increment} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-page-increment} function retrieves the page increment
  of the adjustment. The @sym{(setf gtk:adjustment-page-increment)} function
  sets the page increment of the adjustment.

  See the @fun{gtk:adjustment-lower} function about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}")

;;; --- adjustment-page-size ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "page-size" 'adjustment) t)
 "The @code{page-size} property of type @code{:double} (Read / Write) @br{}
  The page size of the adjustment. Note that the page size is irrelevant and
  should be set to zero if the adjustment is used for a simple scalar value.
  @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-page-size)
      "Accessor"
      (documentation 'adjustment-page-size 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-page-size object) => page-size}
  @syntax[]{(setf (gtk:adjustment-page-size object) page-size)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[page-size]{a double float with the page size}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{page-size} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-page-size} function retrieves the page size of the
  adjustment. The @sym{(setf gtk:adjustment-page-size)} function sets the page
  size of the adjustment.

  See the @fun{gtk:adjustment-lower} function about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}")

;;; --- adjustment-step-increment ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "step-increment" 'adjustment) t)
 "The @code{step-increment} property of type @code{:double} (Read / Write) @br{}
  The step increment of the adjustment. @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-step-increment)
      "Accessor"
      (documentation 'adjustment-step-increment 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-step-increment object) => step-increment}
  @syntax[]{(setf (gtk:adjustment-step-increment object) step-increment)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[step-increment]{a double float with the step increment}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{step-increment} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-step-increment} function retrieves the step increment
  of the adjustment. The @sym{(setf gtk:adjustment-step-increment)} function
  sets the step increment of the adjustment.

  See the @fun{gtk:adjustment-lower} function about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}")

;;; --- adjustment-upper -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "upper" 'adjustment) t)
 "The @code{upper} property of type @code{:double} (Read / Write) @br{}
  The maximum value of the adjustment. Note that values will be restricted by
  @code{upper} - @code{page-size} if the @code{page-size} property is nonzero.
  @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-upper)
      "Accessor"
      (documentation 'adjustment-upper 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-upper object) => upper}
  @syntax[]{(setf (gtk:adjustment-upper object) upper)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[upper]{a double float with the maximum value}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{upper} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-upper} function retrieves the maximum value of the
  adjustment. The @sym{(setf gtk:adjustment-upper)} function sets the maximum
  value of the adjustment.

  Note that values will be restricted by the @code{(@slot[gtk:adjustment]{upper}
  - @slot[gtk:adjustment]{page-size})} value if the
  @slot[gtk:adjustment]{page-size} value is nonzero.

  See the @fun{gtk:adjustment-lower} function about how to compress multiple
  emissions of the \"changed\" signal when setting multiple adjustment
  properties.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}
  @see-function{gtk:adjustment-upper}
  @see-function{gtk:adjustment-page-size}")

;;; --- adjustment-value -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'adjustment) t)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The value of the adjustment. @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'adjustment-value)
      "Accessor"
      (documentation 'adjustment-value 'function)
 "@version{#2022-11-13}
  @syntax[]{(gtk:adjustment-value object) => value}
  @syntax[]{(setf (gtk:adjustment-value object) value)}
  @argument[object]{a @class{gtk:adjustment} object}
  @argument[value]{a double float with the value}
  @begin{short}
    Accessor of the @slot[gtk:adjustment]{value} slot of the
    @class{gtk:adjustment} class.
  @end{short}

  The @sym{gtk:adjustment-value} function gets the current value of the
  adjustment. The @sym{(setf gtk:adjustment-value)} function sets the adjustment
  value. The value is clamped to lie between the @slot[gtk:adjustment]{lower}
  and @slot[gtk:adjustment]{upper} values.

  Note that for adjustments which are used in a @class{gtk:scrollbar} widget,
  the effective range of allowed values goes from the
  @slot[gtk:adjustment]{lower} to @code{(@slot[gtk:adjustment]{upper} -
  @slot[gtk:adjustemnt]{page-size})} values.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}
  @see-function{gtk:adjustment-upper}
  @see-function{gtk:adjustment-page-size}")

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_new ()
;;; ----------------------------------------------------------------------------

(defun adjustment-new (value lower upper
                       step-increment page-increment page-size)
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[value]{a double float with the initial value}
  @argument[lower]{a double float with the minimum value}
  @argument[upper]{a double with the maximum value}
  @argument[step-increment]{a double float with the step increment}
  @argument[page-increment]{a double float with the page increment}
  @argument[page-size]{a double float with the page size}
  @return{A new @class{gtk:adjustment} object.}
  @short{Creates a new adjustment.}
  @see-class{gtk:adjustment}"
  (let ((adjustment (make-instance 'adjustment
                                   :lower lower
                                   :upper upper
                                   :step-increment step-increment
                                   :page-increment page-increment
                                   :page-size page-size)))
    ;; At last set VALUE
    (setf (adjustment-value adjustment) value)
    adjustment))

(export 'adjustment-new)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_clamp_page ()
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_adjustment_clamp_page" adjustment-clamp-page) :void
  (adjustment (g:object adjustment))
  (lower :double)
  (upper :double))

(defun adjustment-clamp-page (adjustment lower upper)
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @argument[lower]{a double float with the lower value}
  @argument[upper]{a double float with the upper value}
  @begin{short}
    Updates the adjustment value to ensure that the range between @code{lower}
    and @code{upper} is in the current page.
  @end{short}
  If the range is larger than the page size, then only the start of it will be
  in the current page. A \"changed\" signal will be emitted if the value is
  changed.
  @see-class{gtk:adjustment}"
  (cffi:foreign-funcall "gtk_adjustment_clamp_page"
                        (g:object adjustment) adjustment
                        :double (coerce lower 'double-float)
                        :double (coerce upper 'double-float)
                        :void))

(export 'adjustment-clamp-page)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_configure ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_adjustment_configure" adjustment-configure) :void
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @argument[value]{a double float with the new value}
  @argument[lower]{a double float with the new minimum value}
  @argument[upper]{a double float with the new maximum value}
  @argument[step-increment]{a double float with the new step increment}
  @argument[page-increment]{a double float with the new page increment}
  @argument[page-size]{a double float with the new page size}
  @begin{short}
    Sets all properties of the adjustment at once.
  @end{short}
  Use this function to avoid multiple emissions of the \"changed\" signal. See
  the @fun{gtk:adjustment-lower} function for an alternative way of compressing
  multiple emissions of \"changed\" signals into one.
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-lower}"
  (adjustment (g:object adjustment))
  (value :double)
  (lower :double)
  (upper :double)
  (step-increment :double)
  (page-increment :double)
  (page-size :double))

(export 'adjustment-configure)

;;; ----------------------------------------------------------------------------
;;; gtk_adjustment_get_minimum_increment () -> adjustment-minimum-increment
;;; ----------------------------------------------------------------------------

(declaim (inline adjustment-minimum-increment))

(defun adjustment-minimum-increment (adjustment)
 #+liber-documentation
 "@version{#2022-11-13}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @return{A double float with the minimum increment of the adjustment.}
  @short{Gets the smaller of step increment and page increment.}
  @see-class{gtk:adjustment}
  @see-function{gtk:adjustment-step-increment}
  @see-function{gtk:adjustment-page-increment}"
  (min (adjustment-step-increment adjustment)
       (adjustment-page-increment adjustment)))

(export 'adjustment-minimum-increment)

;;; --- End of file gtk4.adjustment.lisp ---------------------------------------
