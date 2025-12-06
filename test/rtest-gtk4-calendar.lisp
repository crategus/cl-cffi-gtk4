(in-package :gtk-test)

(def-suite gtk-calendar :in gtk-display-widgets)
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
             (glib-test:list-children "GtkCalendar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkCalendar")))
  ;; Check properties
  (is (equal '("date" "day" "month" "show-day-names" "show-heading"
               "show-week-numbers" "year")
             (glib-test:list-properties "GtkCalendar")))
  ;; Check signals
  (is (equal '("day-selected" "next-month" "next-year" "prev-month" "prev-year")
             (glib-test:list-signals "GtkCalendar")))
  ;; Check CSS name
  (is (string= "calendar"
               (gtk:widget-class-css-name "GtkCalendar")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkCalendar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCalendar" GTK:CALENDAR
                      (:SUPERCLASS GTK:WIDGET :EXPORT T :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_calendar_get_type")
                      ((DATE CALENDAR-DATE "date" "GDateTime" T T)
                       (DAY CALENDAR-DAY "day" "gint" T T)
                       (MONTH CALENDAR-MONTH "month" "gint" T T)
                       (SHOW-DAY-NAMES CALENDAR-SHOW-DAY-NAMES
                        "show-day-names" "gboolean" T T)
                       (SHOW-HEADING CALENDAR-SHOW-HEADING "show-heading"
                        "gboolean" T T)
                       (SHOW-WEEK-NUMBERS CALENDAR-SHOW-WEEK-NUMBERS
                        "show-week-numbers" "gboolean" T T)
                       (YEAR CALENDAR-YEAR "year" "gint" T T)))
             (gobject:get-gtype-definition "GtkCalendar"))))

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

;;; --- Properties -------------------------------------------------------------

(test gtk-calendar-properties
  (glib-test:with-check-memory (calendar)
    (let* ((time1 (multiple-value-list (get-decoded-time)))
           (day (fourth time1))
           (month (fifth time1))
           (year (sixth time1)))
      (is (typep (setf calendar (make-instance 'gtk:calendar)) 'gtk:calendar))
      (is (= day (gtk:calendar-day calendar)))
      (is (= (1- month) (gtk:calendar-month calendar)))
      (is-true (gtk:calendar-show-day-names calendar))
      (is-true (gtk:calendar-show-heading calendar))
      (is-false (gtk:calendar-show-week-numbers calendar))
      (is (= year (gtk:calendar-year calendar))))))

(test gtk-calendar-date
  (let* ((time1 (encode-universal-time 12 36 20 12 5 2025 -1))
         (calendar (make-instance 'gtk:calendar))
         time2)
    (is (= time1 (setf (gtk:calendar-date calendar) time1)))
    (is (= time1 (setf time2 (gtk:calendar-date calendar))))
    (is (= 0 (- time1 time2)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_calendar_new

(test gtk-calendar-new
  (glib-test:with-check-memory (calendar)
    (is (typep (setf calendar (gtk:calendar-new)) 'gtk:calendar))))

;;;     gtk_calendar_select_day
;;;     gtk_calendar_get_date

(test gtk-calendar-select-day
  (let ((gtk-init:*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (calendar)
      (is (typep (setf calendar (gtk:calendar-new)) 'gtk:calendar))
      (is-false (gtk:calendar-select-day calendar 2024 7 5))
      ;; FIXME: Correct the time implementation, there is an offset
      (is (equal '(0 0 23 4 8 2024 6 T -1)
                 (multiple-value-list
                   (decode-universal-time (gtk:calendar-date calendar))))))))

;;;     gtk_calendar_mark_day
;;;     gtk_calendar_unmark_day
;;;     gtk_calendar_get_day_is_marked

(test gtk-calendar-mark/unmark-day
  (glib-test:with-check-memory (calendar)
    (is (typep (setf calendar (gtk:calendar-new)) 'gtk:calendar))
    (is-false (gtk:calendar-mark-day calendar 5))
    (is-true (gtk:calendar-day-is-marked calendar 5))
    (is-false (gtk:calendar-unmark-day calendar 5))
    (is-false (gtk:calendar-day-is-marked calendar 5))))

;;;     gtk_calendar_clear_marks

(test gtk-calendar-clear-marks
  (glib-test:with-check-memory (calendar)
    (is (typep (setf calendar (gtk:calendar-new)) 'gtk:calendar))
    (is-false (gtk:calendar-mark-day calendar 5))
    (is-false (gtk:calendar-mark-day calendar 6))
    (is-true (gtk:calendar-day-is-marked calendar 5))
    (is-true (gtk:calendar-day-is-marked calendar 6))
    (is-false (gtk:calendar-clear-marks calendar))
    (is-false (gtk:calendar-day-is-marked calendar 5))
    (is-false (gtk:calendar-day-is-marked calendar 6))))

;;; 2025-12-05
