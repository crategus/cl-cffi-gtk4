(in-package :gtk-test)

(def-suite gtk-level-bar :in gtk-suite)
(in-suite gtk-level-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GTK_LEVEL_BAR_OFFSET_LOW
;;;     GTK_LEVEL_BAR_OFFSET_HIGH
;;;     GTK_LEVEL_BAR_OFFSET_FULL

;;;     GtkLevelBarMode

(test gtk-level-bar-mode
  ;; Check type
  (is (g:type-is-enum "GtkLevelBarMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLevelBarMode")
          (g:gtype (cffi:foreign-funcall "gtk_level_bar_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:level-bar-mode
          (glib:symbol-for-gtype "GtkLevelBarMode")))
  ;; Check names
  (is (equal '("GTK_LEVEL_BAR_MODE_CONTINUOUS" "GTK_LEVEL_BAR_MODE_DISCRETE")
             (list-enum-item-name "GtkLevelBarMode")))
  ;; Check values
  (is (equal '(0 1)
             (list-enum-item-value "GtkLevelBarMode")))
  ;; Check nick names
  (is (equal '("continuous" "discrete")
             (list-enum-item-nick "GtkLevelBarMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkLevelBarMode"
                                     GTK-LEVEL-BAR-MODE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_level_bar_mode_get_type")
                                     (:CONTINUOUS 0)
                                     (:DISCRETE 1))
             (gobject:get-g-type-definition "GtkLevelBarMode"))))

;;;     GtkLevelBar

(test gtk-level-bar-class
  ;; Check type
  (is (g:type-is-object "GtkLevelBar"))
  ;; Check registered name
  (is (eq 'gtk:level-bar
          (glib:symbol-for-gtype "GtkLevelBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLevelBar")
          (g:gtype (cffi:foreign-funcall "gtk_level_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkLevelBar")))
  ;; Check children
  (is (equal '()
             (list-children "GtkLevelBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (list-interfaces "GtkLevelBar")))
  ;; Check properties
  (is (equal '("inverted" "max-value" "min-value" "mode" "orientation" "value")
             (list-properties "GtkLevelBar")))
  ;; Check signals
  (is (equal '("offset-changed")
             (list-signals "GtkLevelBar")))
  ;; Check CSS name
  (is (string= "levelbar"
               (gtk:widget-class-css-name "GtkLevelBar")))
  ;; Check CSS classes
  (is (equal '("horizontal" "continuous")
             (gtk:widget-css-classes (make-instance 'gtk:level-bar))))
  ;; Check accessible role
  (is (eq :meter (gtk:widget-class-accessible-role "GtkLevelBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLevelBar" GTK-LEVEL-BAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkAccessibleRange"
                                 "GtkBuildable" "GtkConstraintTarget"
                                 "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_level_bar_get_type")
                               ((INVERTED GTK-LEVEL-BAR-INVERTED "inverted"
                                 "gboolean" T T)
                                (MAX-VALUE GTK-LEVEL-BAR-MAX-VALUE "max-value"
                                 "gdouble" T T)
                                (MIN-VALUE GTK-LEVEL-BAR-MIN-VALUE "min-value"
                                 "gdouble" T T)
                                (MODE GTK-LEVEL-BAR-MODE "mode"
                                 "GtkLevelBarMode" T T)
                                (VALUE GTK-LEVEL-BAR-VALUE "value" "gdouble" T
                                 T)))
             (gobject:get-g-type-definition "GtkLevelBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-level-bar-properties
  (let ((levelbar (make-instance 'gtk:level-bar)))
    (is-false (gtk:level-bar-inverted levelbar))
    (is (= 1.0 (gtk:level-bar-max-value levelbar)))
    (is (= 0.0 (gtk:level-bar-min-value levelbar)))
    (is (eq :continuous (gtk:level-bar-mode levelbar)))
    (is (= 0.0 (gtk:level-bar-value levelbar)))))

;;; --- Signals ----------------------------------------------------------------

;;;     offset-changed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_level_bar_new

(test gtk-level-bar-new
  (is (typep (gtk:level-bar-new) 'gtk:level-bar)))

;;;     gtk_level_bar_new_for_interval

(test gtk-level-bar-new-for-interval
  (is (typep (gtk:level-bar-new-for-interval 1/2 10) 'gtk:level-bar)))

;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value

;;; 2024-4-25
