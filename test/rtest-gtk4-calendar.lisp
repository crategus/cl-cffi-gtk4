(in-package :gtk-test)

(def-suite gtk-calendar :in gtk-suite)
(in-suite gtk-calendar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCalendar

(test gtk-calendar-class
  ;; Check type
  (is (g:type-is-object "GtkCalendar"))
  ;; Check registered name
  (is (eq 'gtk:calendar
          (glib:symbol-for-gtype "GtkCalendar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCalendar")
          (g:gtype (cffi:foreign-funcall "gtk_calendar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkCalendar")))
  ;; Check children
  (is (equal '()
             (list-children "GtkCalendar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkCalendar")))
  ;; Check properties
  (is (equal '("day" "month" "show-day-names" "show-heading" "show-week-numbers"
               "year")
             (list-properties "GtkCalendar")))
  ;; Check signals
  (is (equal '("day-selected" "next-month" "next-year" "prev-month" "prev-year")
             (list-signals "GtkCalendar")))
  ;; Check CSS name
  (is (string= "calendar"
               (gtk:widget-class-css-name "GtkCalendar")))
  ;; Check CSS classes
  (is (equal '("view")
             (gtk:widget-css-classes (make-instance 'gtk:calendar))))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkCalendar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkCalendar" GTK-CALENDAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_calendar_get_type")
                               ((DAY GTK-CALENDAR-DAY "day" "gint" T T)
                                (MONTH GTK-CALENDAR-MONTH "month" "gint" T T)
                                (SHOW-DAY-NAMES GTK-CALENDAR-SHOW-DAY-NAMES
                                 "show-day-names" "gboolean" T T)
                                (SHOW-HEADING GTK-CALENDAR-SHOW-HEADING
                                 "show-heading" "gboolean" T T)
                                (SHOW-WEEK-NUMBERS
                                 GTK-CALENDAR-SHOW-WEEK-NUMBERS
                                 "show-week-numbers" "gboolean" T T)
                                (YEAR GTK-CALENDAR-YEAR "year" "gint" T T)))
             (gobject:get-g-type-definition "GtkCalendar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-calendar-properties
  (let* ((time (multiple-value-list (get-decoded-time)))
         (day (fourth time))
         (month (fifth time))
         (year (sixth time))
         (calendar (make-instance 'gtk:calendar)))
    (is (= day (gtk:calendar-day calendar)))
    (is (= (1- month) (gtk:calendar-month calendar)))
    (is-true (gtk:calendar-show-day-names calendar))
    (is-true (gtk:calendar-show-heading calendar))
    (is-false (gtk:calendar-show-week-numbers calendar))
    (is (= year (gtk:calendar-year calendar)))))

;;; --- Signals ----------------------------------------------------------------

;;;     day-selected
;;;     next-month
;;;     next-year
;;;     prev-month
;;;     prev-year

;;; --- Functions --------------------------------------------------------------

;;;     gtk_calendar_new

(test gtk-calendar-new
  (is (typep (gtk:calendar-new) 'gtk:calendar)))

;;;     gtk_calendar_select_day
;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked
;;;     gtk_calendar_clear_marks
;;;     gtk_calendar_get_date

;;; 2024-4-25
