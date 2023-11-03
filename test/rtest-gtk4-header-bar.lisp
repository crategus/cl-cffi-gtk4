(in-package :gtk-test)

(def-suite gtk-header-bar :in gtk-suite)
(in-suite gtk-header-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkHeaderBar

(test gtk-header-bar-class
  ;; Type check
  (is (g:type-is-object "GtkHeaderBar"))
  ;; Check the registered name
  (is (eq 'gtk:header-bar
          (glib:symbol-for-gtype "GtkHeaderBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkHeaderBar")
          (g:gtype (cffi:foreign-funcall "gtk_header_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkHeaderBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkHeaderBar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkHeaderBar")))
  ;; Check the properties
  (is (equal '("decoration-layout" "show-title-buttons" "title-widget")
             (list-properties "GtkHeaderBar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkHeaderBar")))
  ;; CSS name
  (is (string= "headerbar"
               (gtk:widget-class-css-name "GtkHeaderBar")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:header-bar))))
  ;; Accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkHeaderBar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkHeaderBar" GTK-HEADER-BAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_header_bar_get_type")
                               ((DECORATION-LAYOUT
                                 GTK-HEADER-BAR-DECORATION-LAYOUT
                                 "decoration-layout" "gchararray" T T)
                                (SHOW-TITLE-BUTTONS
                                 GTK-HEADER-BAR-SHOW-TITLE-BUTTONS
                                 "show-title-buttons" "gboolean" T T)
                                (TITLE-WIDGET GTK-HEADER-BAR-TITLE-WIDGET
                                 "title-widget" "GtkWidget" T T)))
             (gobject:get-g-type-definition "GtkHeaderBar"))))

;;; --- Properties -------------------------------------------------------------

;;;     decoration-layout
;;;     show-title-buttons
;;;     title-widget

(test gtk-header-bar-properties
  (let ((headerbar (make-instance 'gtk:header-bar)))
    (is-false (gtk:header-bar-decoration-layout headerbar))
    (is-true (gtk:header-bar-show-title-buttons headerbar))
    (is-false (gtk:header-bar-title-widget headerbar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_header_bar_new

(test gtk-header-bar-new
  (is (typep (gtk:header-bar-new) 'gtk:header-bar)))

;;;     gtk_header_bar_pack_start
;;;     gtk_header_bar_pack_end
;;;     gtk_header_bar_remove

;;; --- 2023-8-9 ---------------------------------------------------------------
