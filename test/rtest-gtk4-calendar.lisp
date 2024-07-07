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
             (gtk-test:list-children "GtkCalendar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkCalendar")))
  ;; Check properties
  (is (equal '("day" "month" "show-day-names" "show-heading" "show-week-numbers"
               "year")
             (gtk-test:list-properties "GtkCalendar")))
  ;; Check signals
  (is (equal '("day-selected" "next-month" "next-year" "prev-month" "prev-year")
             (gtk-test:list-signals "GtkCalendar")))
  ;; Check CSS name
  (is (string= "calendar"
               (gtk:widget-class-css-name "GtkCalendar")))
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

(test gtk-calendar-day-selected-signal
  (let* ((name "day-selected")
         (gtype (g:gtype "GtkCalendar"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     next-month

(test gtk-calendar-next-month-signal
  (let* ((name "next-month")
         (gtype (g:gtype "GtkCalendar"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     next-year

(test gtk-calendar-next-year-signal
  (let* ((name "next-year")
         (gtype (g:gtype "GtkCalendar"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     prev-month

(test gtk-calendar-prev-month-signal
  (let* ((name "prev-month")
         (gtype (g:gtype "GtkCalendar"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     prev-year

(test gtk-calendar-prev-year-signal
  (let* ((name "prev-year")
         (gtype (g:gtype "GtkCalendar"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_calendar_new

(test gtk-calendar-new
  (is (typep (gtk:calendar-new) 'gtk:calendar)))

;;;     gtk_calendar_select_day
;;;     gtk_calendar_get_date

(test gtk-calendar-select-day
  (let ((calendar (gtk:calendar-new)))
    (is-false (gtk:calendar-select-day calendar 2024 7 5))
    (is (equal '(2024 7 5) (multiple-value-list (gtk:calendar-date calendar))))))

;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked

(test gtk-calendar-mark/unmark-day
  (let ((calendar (gtk:calendar-new)))
    (is-false (gtk:calendar-mark-day calendar 5))
    (is-true (gtk:calendar-day-is-marked calendar 5))
    (is-false (gtk:calendar-unmark-day calendar 5))
    (is-false (gtk:calendar-day-is-marked calendar 5))))

;;;     gtk_calendar_clear_marks

(test gtk-calendar-clear-marks
  (let ((calendar (gtk:calendar-new)))
    (is-false (gtk:calendar-mark-day calendar 5))
    (is-false (gtk:calendar-mark-day calendar 6))
    (is-true (gtk:calendar-day-is-marked calendar 5))
    (is-true (gtk:calendar-day-is-marked calendar 6))
    (is-false (gtk:calendar-clear-marks calendar))
    (is-false (gtk:calendar-day-is-marked calendar 5))
    (is-false (gtk:calendar-day-is-marked calendar 6))))

;;; 2024-7-5
