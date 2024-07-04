(in-package :gtk-test)

(def-suite gtk-header-bar :in gtk-suite)
(in-suite gtk-header-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkHeaderBar

(test gtk-header-bar-class
  ;; Check type
  (is (g:type-is-object "GtkHeaderBar"))
  ;; Check the registered name
  (is (eq 'gtk:header-bar
          (glib:symbol-for-gtype "GtkHeaderBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkHeaderBar")
          (g:gtype (cffi:foreign-funcall "gtk_header_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkHeaderBar")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkHeaderBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkHeaderBar")))
  ;; Check properties
  (is (equal '("decoration-layout" "show-title-buttons" "title-widget")
             (gtk-test:list-properties "GtkHeaderBar")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkHeaderBar")))
  ;; Check CSS name
  (is (string= "headerbar"
               (gtk:widget-class-css-name "GtkHeaderBar")))
  ;; Check accessible role
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

;;; 2024-4-16
