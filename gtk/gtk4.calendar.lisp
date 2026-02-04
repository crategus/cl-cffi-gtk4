;;; ----------------------------------------------------------------------------
;;; gtk4.calendar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2026 Dieter Kaiser
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
;;; GtkCalendar
;;;
;;;     Displays a calendar and allows the user to select a date
;;;
;;; Types and Values
;;;
;;;     GtkCalendar
;;;
;;; Accessors
;;;
;;;     gtk_calendar_get_date                               Since 4.20
;;;     gtk_calendar_set_date                               Since 4.20
;;;     gtk_calendar_get_show_day_names
;;;     gtk_calendar_set_show_day_names
;;;     gtk_calendar_get_show_heading
;;;     gtk_calendar_set_show_heading
;;;     gtk_calendar_get_show_week_numbers
;;;     gtk_calendar_set_show_week_numbers
;;;
;;; Functions
;;;
;;;     gtk_calendar_new
;;;     gtk_calendar_select_day                             Deprecated 4.20
;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked
;;;     gtk_calendar_clear_marks
;;;
;;; Properties
;;;
;;;     date                                                Since 4.20
;;;     day                                                 Deprecated 4.20
;;;     month                                               Deprecated 4.20
;;;     show-day-names
;;;     show-heading
;;;     show-week-numbers
;;;     year                                                Deprecated 4.20
;;;
;;; Signals
;;;
;;;     day-selected
;;;     next-month
;;;     next-year
;;;     prev-month
;;;     prev-year
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkCalendar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCalendar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCalendar" calendar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_calendar_get_type")
  (#+gtk-4-20
   (date
    %calendar-date ; We do not export this accessor
    "date" "GDateTime" t t)
   (day
    calendar-day
    "day" "gint" t t)
   (month
    calendar-month "month" "gint" t t)
   (show-day-names
    calendar-show-day-names
    "show-day-names" "gboolean" t t)
   (show-heading
    calendar-show-heading
    "show-heading" "gboolean" t t)
   (show-week-numbers
    calendar-show-week-numbers
    "show-week-numbers" "gboolean" t t)
   (year
    calendar-year
    "year" "gint" t t)))

(unexport '%calendar-date)

#+liber-documentation
(setf (documentation 'calendar 'type)
 "@version{2025-12-05}
  @begin{short}
    The @class{gtk:calendar} widget displays a Gregorian calendar, one month at
    a time.
  @end{short}
  It can be created with the @fun{gtk:calendar-new} function.

  @image[calendar]{Figure: GtkCalendar}

  The selected date can be retrieved and altered from a @class{gtk:calendar}
  widget using the @fun{gtk:calendar-date} accessor function.

  To place a visual marker on a particular day, use the
  @fun{gtk:calendar-mark-day} function and to remove the marker, the
  @fun{gtk:calendar-unmark-day} function. Alternative, all marks can be cleared
  with the @fun{gtk:calendar-clear-marks} function.

  Users should be aware that, although the Gregorian calendar is the legal
  calendar in most countries, it was adopted progressively between 1582 and
  1929. Display before these dates is likely to be historically incorrect.
  @begin[Shortcuts and Gestures]{dictionary}
    @begin{itemize}
      @item{Scrolling up or down will switch to the previous or next month.}
      @item{Date strings can be dropped for setting the current day.}
    @end{itemize}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
calendar.view
├── header
│   ├── button
│   ├── stack.month
│   ├── button
│   ├── button
│   ├── label.year
│   ╰── button
╰── grid
    ╰── label[.day-name][.week-number][.day-number][.other-month][.today]
    @end{pre}
    The @class{gtk:calendar} implementation has a main node with name
    @code{calendar}. It contains a subnode called @code{header} containing the
    widgets for switching between years and months. The @code{grid} subnode
    contains all day labels, including week numbers on the left. marked with
    the @code{.week-number} style class, and day names on top, marked with the
    @code{.day-name} style class. Day labels that belong to the previous or next
    month get the @code{.other-month} style class. The label of the current day
    get the @code{.today} style class. Marked day labels get the
    @code{:selected} state assigned.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[calendar::day-selection]{signal}
      @begin{pre}
lambda (calendar)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[calendar]{The @class{gtk:calendar} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the user selects a day.
    @end{signal}
    @begin[calendar::next-month]{signal}
      @begin{pre}
lambda (calendar)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[calendar]{The @class{gtk:calendar} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the user switched to the next month.
    @end{signal}
    @begin[calendar::next-year]{signal}
      @begin{pre}
lambda (calendar)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[calendar]{The @class{gtk:calendar} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when user switched to the next year.
    @end{signal}
    @begin[calendar::prev-month]{signal}
      @begin{pre}
lambda (calendar)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[calendar]{The @class{gtk:calendar} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the user switched to the previous month.
    @end{signal}
    @begin[calendar::prev-year]{signal}
      @begin{pre}
lambda (calendar)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[calendar]{The @class{gtk:calendar} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted when user switched to the previous year.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:calendar-new}
  @see-slot{gtk:calendar-date}
  @see-slot{gtk:calendar-day}
  @see-slot{gtk:calendar-month}
  @see-slot{gtk:calendar-show-day-names}
  @see-slot{gtk:calendar-show-heading}
  @see-slot{gtk:calendar-show-week-numbers}
  @see-slot{gtk:calendar-year}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+(and gtk-4-20 liber-documentation)
(setf (documentation (liber:slot-documentation "date" 'calendar) t)
 "The @code{date} property of type @class{g:date-time} (Read / Write) @br{}
  The selected date. This property gets initially set to the current date. The
  accessor translates the internal @class{g:date-time} instance into the
  Lisp universal time. Since 4.20")

#+gtk-4-20
(defun (setf calendar-date) (value object)
  (setf (%calendar-date object)
        (cffi:convert-to-foreign value 'g:date-time))
  value)

#+gtk-4-20
(defun calendar-date (object)
 #+liber-documentation
 "@version{2025-12-05}
  @syntax{(gtk:calendar-date object) => date}
  @syntax{(setf (gtk:calendar-date object) date)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[date]{an unsigned integer for the Lisp universal time}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{date} slot of the
    @class{gtk:calendar} class gets or sets the date of the calendar.
  @end{short}
  The date represents the shown year, month and the selected day.

  Since 4.20
  @see-class{gtk:calendar}"
  (cffi:convert-from-foreign (%calendar-date object) 'g:date-time))

#+gtk-4-20
(export 'calendar-date)

;;; --- gtk:calendar-day -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "day" 'calendar) t)
 "The @code{day} property of type @code{:int} (Read / Write) @br{}
  The selected day as a number between 1 and 31, or 0 to unselect the currently
  selected day. This property gets initially set to the current day. Deprecated
  4.20 @br{}
  Allowed values: [0, 31] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-day)
      "Accessor"
      (documentation 'calendar-day 'function)
 "@version{2025-12-05}
  @syntax{(gtk:calender-day object) => day}
  @syntax{(setf (gtk:calendar-day object) day)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[day]{an integer for the selected day}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{day} slot of the
    @class{gtk:calendar} class gets or sets the selected day as a number between
    1 and 31, or 0 to unselect the currently selected day.
  @end{short}
  This property gets initially set to the current day.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20. Use the @fun{gtk:calendar-date}
    function instead.
  @end{dictionary}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-date}
  @see-function{gtk:calendar-select-day}")

;;; --- gtk:calendar-month -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "month" 'calendar) t)
 "The @code{month} property of type @code{:int} (Read / Write) @br{}
  The selected month as a number between 0 and 11. This property gets initially
  set to the current month. Deprecated 4.20 @br{}
  Allowed values: [0, 11] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-month)
      "Accessor"
      (documentation 'calendar-month 'function)
 "@version{2025-12-05}
  @syntax{(gtk:calendar-month object) => month}
  @syntax{(setf (gtk:calendar-month object) month)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[month]{an integer for the selected month}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{month} slot of the
    @class{gtk:calendar} class gets or sets the selected month as a number
    between 0 and 11.
  @end{short}
  This property gets initially set to the current month.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20. Use the @fun{gtk:calendar-date}
    function instead.
  @end{dictionary}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-date}")

;;; --- gtk:calendar-show-day-names --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-day-names" 'calendar) t)
 "The @code{show-day-names} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether day names are displayed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-show-day-names)
      "Accessor"
      (documentation 'calendar-show-day-names 'function)
 "@version{2025-08-04}
  @syntax{(gtk:calendar-show-day-names object) => setting}
  @syntax{(setf (gtk:calendar-show-day-names object) setting)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[setting]{a boolean whether day names are displayed}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{show-day-names} slot of the
    @class{gtk:calendar} class gets or sets whether day names are displayed.
  @end{short}
  @see-class{gtk:calendar}")

;;; --- gtk:calendar-show-heading ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-heading" 'calendar) t)
 "The @code{show-heading} property of type @code{:boolean} (Read / Write) @br{}
  Determines whether a heading is displayed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-show-heading)
      "Accessor"
      (documentation 'calendar-show-heading 'function)
 "@version{2025-08-04}
  @syntax{(gtk:calendar-show-heading object) => setting}
  @syntax{(setf (gtk:calendar-show-heading object) setting)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[setting]{a boolean whether a heading is displayed}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{show-heading} slot of the
    @class{gtk:calendar} class gets or sets whether a heading is displayed.
  @end{short}
  @see-class{gtk:calendar}")

;;; --- gtk:calendar-show-week-numbers -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-week-numbers" 'calendar) t)
 "The @code{show-week-numbers} property of type @code{:boolean} (Read / Write)
  @br{}
  Determines whether week numbers are displayed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-show-week-numbers)
      "Accessor"
      (documentation 'calendar-show-week-numbers 'function)
 "@version{2025-08-04}
  @syntax{(gtk:calendar-show-week-numbers object) => setting}
  @syntax{(setf (gtk:calendar-show-week-numbers object) setting)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[setting]{a boolean whether week numbers are displayed}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{show-week-numbers} slot of the
    @class{gtk:calendar} class gets or sets whether week numbers are displayed.
  @end{short}
  @see-class{gtk:calendar}")

;;; --- gtk:calendar-year ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "year" 'calendar) t)
 "The @code{year} property of type @code{:int} (Read / Write) @br{}
  The selected year. This property gets initially set to the current year.
  Deprecated 4.20 @br{}
  Allowed values: [0, 4194303] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'calendar-year)
      "Accessor"
      (documentation 'calendar-year 'function)
 "@version{2025-12-05}
  @syntax{(gtk:calendar-year object) => year}
  @syntax{(setf (gtk:calendar-year object) year)}
  @argument[object]{a @class{gtk:calendar} widget}
  @argument[year]{an integer for the selected year}
  @begin{short}
    The accessor for the @slot[gtk:calendar]{year} slot of the
    @class{gtk:calendar} class gets or sets the selected year.
  @end{short}
  This property gets initially set to the current year.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20. Use the @fun{gtk:calendar-date}
    function instead.
  @end{dictionary}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-date}")

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_new
;;; ----------------------------------------------------------------------------

(declaim (inline calendar-new))

(defun calendar-new ()
 #+liber-documentation
 "@version{2025-05-31}
  @return{The new @class{gtk:calendar} widget.}
  @begin{short}
    Creates a new calendar, with the current date being selected.
  @end{short}
  @see-class{gtk:calendar}"
  (make-instance 'calendar))

(export 'calendar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_select_day                                 Deprecated 4.20
;;; ----------------------------------------------------------------------------

;; TODO: The C implementation introduces the GDateTime type. We pass in the
;; day, month and year argument.

(declaim (inline calendar-select-day))

(defun calendar-select-day (calendar year month day)
 #+liber-documentation
 "@version{2025-12-05}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @argument[year]{an integer for the year}
  @argument[month]{an integer for the month}
  @argument[day]{an integer for the day between 1 and 31}
  @begin{short}
    Will switch to the year and month of the date and select its day.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.20. Use the @fun{gtk:calendar-date}
    function instead.
  @end{dictionary}
  @see-class{gtk:calendar}"
  #+(and gtk-4-20 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:CALENDAR-SELECT-DAY is deprecated since 4.20."))
  (setf (calendar-year calendar) year
        (calendar-month calendar) month
        (calendar-day calendar) day)
  nil)

(export 'calendar-select-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_mark_day
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_calendar_mark_day" calendar-mark-day) :void
 #+liber-documentation
 "@version{2025-05-31}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @argument[day]{an unsigned integer for the day to mark between 1 and 31}
  @begin{short}
    Places a visual marker on a particular day.
  @end{short}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-unmark-day}
  @see-function{gtk:calendar-day-is-marked}
  @see-function{gtk:calendar-clear-marks}"
  (calendar (g:object calendar))
  (day :uint))

(export 'calendar-mark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_unmark_day
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_calendar_unmark_day" calendar-unmark-day) :void
 #+liber-documentation
 "@version{2025-05-31}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @argument[day]{an unsigned integer for the day to unmark between 1 and 31}
  @begin{short}
    Removes the visual marker from a particular day.
  @end{short}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-mark-day}
  @see-function{gtk:calendar-day-is-marked}
  @see-function{gtk:calendar-clear-marks}"
  (calendar (g:object calendar))
  (day :uint))

(export 'calendar-unmark-day)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_day_is_marked
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_calendar_get_day_is_marked" calendar-day-is-marked) :boolean
 #+liber-documentation
 "@version{2025-05-31}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @argument[day]{an unsigned integer for the day between 1 and 31}
  @return{The boolean whether the day is marked.}
  @begin{short}
    Returns if the day of the calendar is already marked.
  @end{short}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-mark-day}
  @see-function{gtk:calendar-unmark-day}"
  (calendar (g:object calendar))
  (day :uint))

(export 'calendar-day-is-marked)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_clear_marks
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_calendar_clear_marks" calendar-clear-marks) :void
 #+liber-documentation
 "@version{2025-05-31}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @begin{short}
    Remove all visual markers.
  @end{short}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-mark-day}
  @see-function{gtk:calendar-unmark-day}"
  (calendar (g:object calendar)))

(export 'calendar-clear-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_calendar_get_date
;;; ----------------------------------------------------------------------------

;; TODO: The C implementation introduces the GDateTime type. We return
;; the values from the calendar widget.

#-gtk-4-20
(defun calendar-date (calendar)
 #+liber-documentation
 "@version{2025-05-31}
  @syntax{(gtk:calendar-data calendar) => year, month, day}
  @argument[calendar]{a @class{gtk:calendar} widget}
  @argument[year]{an integer for the year as a decimal number}
  @argument[month]{an integer for the month number, between 0 and 11}
  @argument[day]{an integer for the day number, between 1 and 31}
  @begin{short}
    Obtains the selected date from the calendar.
  @end{short}
  @see-class{gtk:calendar}
  @see-function{gtk:calendar-select-day}"
  (values (calendar-year calendar)
          (calendar-month calendar)
          (calendar-day calendar)))

(export 'calendar-date)

;;; --- End of file gtk4.calendar.lisp -----------------------------------------
