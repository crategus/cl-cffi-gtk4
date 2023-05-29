(in-package :gtk-test)

(def-suite gtk-progress-bar :in gtk-suite)
(in-suite gtk-progress-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkProgressBar

(test progress-bar-class
  ;; Type check
  (is (g:type-is-object "GtkProgressBar"))
  ;; Check the registered name
  (is (eq 'gtk:progress-bar
          (glib:symbol-for-gtype "GtkProgressBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkProgressBar")
          (g:gtype (cffi:foreign-funcall "gtk_progress_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkProgressBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkProgressBar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (list-interfaces "GtkProgressBar")))
  ;; Check the properties
  (is (equal '("ellipsize" "fraction" "inverted" "orientation" "pulse-step"
               "show-text" "text")
             (list-properties "GtkProgressBar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkProgressBar")))
  ;; CSS information
  (is (string= "progressbar"
               (gtk:widget-class-css-name "GtkProgressBar")))
  (is (string=
"progressbar.horizontal:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:progress-bar))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkProgressBar" GTK-PROGRESS-BAR
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                       ((ELLIPSIZE GTK-PROGRESS-BAR-ELLIPSIZE "ellipsize"
                         "PangoEllipsizeMode" T T)
                        (FRACTION GTK-PROGRESS-BAR-FRACTION "fraction"
                         "gdouble" T T)
                        (INVERTED GTK-PROGRESS-BAR-INVERTED "inverted"
                         "gboolean" T T)
                        (PULSE-STEP GTK-PROGRESS-BAR-PULSE-STEP "pulse-step"
                         "gdouble" T T)
                        (SHOW-TEXT GTK-PROGRESS-BAR-SHOW-TEXT "show-text"
                         "gboolean" T T)
                        (TEXT GTK-PROGRESS-BAR-TEXT "text" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkProgressBar"))))

;;; --- Properties -------------------------------------------------------------

;;;     ellipsize
;;;     fraction
;;;     inverted
;;;     pulse-step
;;;     show-text
;;;     text

;;; --- Functions --------------------------------------------------------------

;;;     gtk_progress_bar_new
;;;     gtk_progress_bar_pulse

;;; --- 2023-5-29 --------------------------------------------------------------
