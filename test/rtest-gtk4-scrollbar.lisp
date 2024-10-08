(in-package :gtk-test)

(def-suite gtk-scrollbar :in gtk-suite)
(in-suite gtk-scrollbar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkScrollbar

(test gtk-scrollbar-class
  ;; Check type
  (is (g:type-is-object "GtkScrollbar"))
  ;; Check registered name
  (is (eq 'gtk:scrollbar
          (glib:symbol-for-gtype "GtkScrollbar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkScrollbar")
          (g:gtype (cffi:foreign-funcall "gtk_scrollbar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkScrollbar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkScrollbar")))
  ;; Check interfaces
  ;; The GtkAccessibleRange interface is missing on Ubuntu. Why?!
  #-windows
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkScrollbar")))
  #+windows
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable" "GtkAccessibleRange")
             (glib-test:list-interfaces "GtkScrollbar")))
  ;; Check properties
  (is (equal '("adjustment" "orientation")
             (glib-test:list-properties "GtkScrollbar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkScrollbar")))
  ;; Check CSS name
  (is (string= "scrollbar"
               (gtk:widget-class-css-name "GtkScrollbar")))
  ;; Check accessible role
  (is (eq :scrollbar (gtk:widget-class-accessible-role "GtkScrollbar")))
  ;; Check class definition
  #-windows
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScrollbar" GTK:SCROLLBAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                      ((ADJUSTMENT SCROLLBAR-ADJUSTMENT
                        "adjustment" "GtkAdjustment" T T)))
             (gobject:get-gtype-definition "GtkScrollbar")))
  #+windows
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkScrollbar" GTK:SCROLLBAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkAccessibleRange" "GtkBuildable"
                        "GtkConstraintTarget" "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_scrollbar_get_type")
                      ((ADJUSTMENT SCROLLBAR-ADJUSTMENT
                        "adjustment" "GtkAdjustment" T T)))
             (gobject:get-gtype-definition "GtkScrollbar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-scrollbar-properties
  (let ((scrollbar (make-instance 'gtk:scrollbar)))
    (is (typep (gtk:scrollbar-adjustment scrollbar) 'gtk:adjustment))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_scrollbar_new

(test gtk-scrollbar-new
  (is (typep (gtk:scrollbar-new :horizontal) 'gtk:scrollbar))
  (is (typep (gtk:scrollbar-new :vertical (make-instance 'gtk:adjustment))
             'gtk:scrollbar)))

;;; 2024-7-3
