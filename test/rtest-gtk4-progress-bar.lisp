(in-package :gtk-test)

(def-suite gtk-progress-bar :in gtk-suite)
(in-suite gtk-progress-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkProgressBar

(test gtk-progress-bar-class
  ;; Check type
  (is (g:type-is-object "GtkProgressBar"))
  ;; Check registered name
  (is (eq 'gtk:progress-bar
          (glib:symbol-for-gtype "GtkProgressBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkProgressBar")
          (g:gtype (cffi:foreign-funcall "gtk_progress_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkProgressBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkProgressBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (glib-test:list-interfaces "GtkProgressBar")))
  ;; Check properties
  (is (equal '("ellipsize" "fraction" "inverted" "orientation" "pulse-step"
               "show-text" "text")
             (glib-test:list-properties "GtkProgressBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkProgressBar")))
  ;; Check CSS information
  (is (string= "progressbar"
               (gtk:widget-class-css-name "GtkProgressBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkProgressBar" GTK:PROGRESS-BAR
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                         "GtkConstraintTarget" "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_progress_bar_get_type")
                       ((ELLIPSIZE PROGRESS-BAR-ELLIPSIZE
                         "ellipsize" "PangoEllipsizeMode" T T)
                        (FRACTION PROGRESS-BAR-FRACTION "fraction" "gdouble" T T)
                        (INVERTED PROGRESS-BAR-INVERTED "inverted" "gboolean" T T)
                        (PULSE-STEP PROGRESS-BAR-PULSE-STEP
                         "pulse-step" "gdouble" T T)
                        (SHOW-TEXT PROGRESS-BAR-SHOW-TEXT
                         "show-text" "gboolean" T T)
                        (TEXT PROGRESS-BAR-TEXT "text" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkProgressBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-progress-bar-properties
  (let ((bar (make-instance 'gtk:progress-bar)))
    (is (eq :none (gtk:progress-bar-ellipsize bar)))
    (is (= 0.0d0 (gtk:progress-bar-fraction bar)))
    (is-false (gtk:progress-bar-inverted bar))
    (is (= 0.1d0 (gtk:progress-bar-pulse-step bar)))
    (is-false (gtk:progress-bar-show-text bar))
    (is-false (gtk:progress-bar-text bar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_progress_bar_new

(test gtk-progress-bar-new
  (is (typep (gtk:progress-bar-new) 'gtk:progress-bar)))

;;;     gtk_progress_bar_pulse

;;; 2024-9-20
