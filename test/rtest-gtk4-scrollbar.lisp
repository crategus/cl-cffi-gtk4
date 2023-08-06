(in-package :gtk-test)

(def-suite gtk-scrollbar :in gtk-suite)
(in-suite gtk-scrollbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollbar

(test gtk-scrollbar-class
  ;; Type check
  (is (g:type-is-object "GtkScrollbar"))
  ;; Check the registered name
  (is (eq 'gtk:scrollbar
          (glib:symbol-for-gtype "GtkScrollbar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkScrollbar")
          (g:gtype (cffi:foreign-funcall "gtk_scrollbar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScrollbar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkScrollbar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkOrientable")
             (list-interfaces "GtkScrollbar")))
  ;; Check the properties
  (is (equal '("adjustment" "orientation")
             (list-properties "GtkScrollbar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkScrollbar")))
  ;; CSS name
  (is (string= "scrollbar"
               (gtk:widget-class-css-name "GtkScrollbar")))
  ;; CSS style context
  (is (string=
"scrollbar.horizontal:dir(ltr)
  range.horizontal:dir(ltr)
    trough:dir(ltr)
      slider:dir(ltr)
"
               (print-style-context "GtkScrollbar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkScrollbar" GTK-SCROLLBAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                               ((ADJUSTMENT GTK-SCROLLBAR-ADJUSTMENT
                                 "adjustment" "GtkAdjustment" T T)))
             (gobject:get-g-type-definition "GtkScrollbar"))))

;;; --- Properties -------------------------------------------------------------

;;;     adjustment

(test gtk-scrollbar-properties
  (let ((scrollbar (make-instance 'gtk:scrollbar)))
    (is (typep (gtk:scrollbar-adjustment scrollbar) 'gtk:adjustment))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollbar_new

(test gtk-scrollbar-new
  (is (typep (gtk:scrollbar-new :horizontal) 'gtk:scrollbar))
  (is (typep (gtk:scrollbar-new :vertical (make-instance 'gtk:adjustment))
             'gtk:scrollbar)))

;;; --- 2023-8-6 ---------------------------------------------------------------
