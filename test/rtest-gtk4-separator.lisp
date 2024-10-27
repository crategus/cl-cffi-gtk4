(in-package :gtk-test)

(def-suite gtk-separator :in gtk-suite)
(in-suite gtk-separator)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkSeparator

(test gtk-separator-class
  ;; Check type
  (is (g:type-is-object "GtkSeparator"))
  ;; Check registered name
  (is (eq 'gtk:separator
          (glib:symbol-for-gtype "GtkSeparator")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkSeparator")
          (g:gtype (cffi:foreign-funcall "gtk_separator_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkSeparator")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkSeparator")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkSeparator")))
  ;; Check properties
  (is (equal '("orientation")
             (glib-test:list-properties "GtkSeparator")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkSeparator")))
  ;; Check CSS name
  (is (string= "separator"
               (gtk:widget-class-css-name "GtkSeparator")))
  ;; Check accessible role
  (is (eq :separator (gtk:widget-class-accessible-role "GtkSeparator")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkSeparator" GTK:SEPARATOR
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkOrientable")
                        :TYPE-INITIALIZER "gtk_separator_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkSeparator"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_separator_new

(test gtk-separator-new
  (let (separator)
    (is (typep (setf separator
                     (gtk:separator-new :vertical)) 'gtk:separator))
    (is (eq :vertical (gtk:orientable-orientation separator)))
    (is (= 1 (g:object-ref-count separator)))))

;;; 2024-10-27
