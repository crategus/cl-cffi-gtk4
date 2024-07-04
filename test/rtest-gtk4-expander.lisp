(in-package :gtk-test)

(def-suite gtk-expander :in gtk-suite)
(in-suite gtk-expander)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpander

(test gtk-expander-class
  ;; Check type
  (is (g:type-is-object "GtkExpander"))
  ;; Check registered name
  (is (eq 'gtk:expander
          (glib:symbol-for-gtype "GtkExpander")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkExpander")
          (g:gtype (cffi:foreign-funcall "gtk_expander_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkExpander")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkExpander")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (gtk-test:list-interfaces "GtkExpander")))
  ;; Check properties
  (is (equal '("child" "expanded" "label" "label-widget" "resize-toplevel"
               "use-markup" "use-underline")
             (gtk-test:list-properties "GtkExpander")))
  ;; Check signals
  (is (equal '("activate")
             (gtk-test:list-signals "GtkExpander")))
  ;; Check CSS name
  (is (string= "expander-widget"
               (gtk:widget-class-css-name "GtkExpander")))
  ;; Check accessible role
  (is (eq :button (gtk:widget-class-accessible-role "GtkExpander")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkExpander" GTK-EXPANDER
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_expander_get_type")
                               ((CHILD GTK-EXPANDER-CHILD "child" "GtkWidget" T
                                 T)
                                (EXPANDED GTK-EXPANDER-EXPANDED "expanded"
                                 "gboolean" T T)
                                (LABEL GTK-EXPANDER-LABEL "label" "gchararray"
                                 T T)
                                (LABEL-WIDGET GTK-EXPANDER-LABEL-WIDGET
                                 "label-widget" "GtkWidget" T T)
                                (RESIZE-TOPLEVEL GTK-EXPANDER-RESIZE-TOPLEVEL
                                 "resize-toplevel" "gboolean" T T)
                                (USE-MARKUP GTK-EXPANDER-USE-MARKUP
                                 "use-markup" "gboolean" T T)
                                (USE-UNDERLINE GTK-EXPANDER-USE-UNDERLINE
                                 "use-underline" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkExpander"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-expander-properties
  (let ((expander (make-instance 'gtk:expander)))
    (is-false (gtk:expander-child expander))
    (is-false (gtk:expander-expanded expander))
    (is-false (gtk:expander-label expander))
    (is-false (gtk:expander-label-widget expander))
    (is-false (gtk:expander-resize-toplevel expander))
    (is-false (gtk:expander-use-markup expander))
    (is-false (gtk:expander-use-underline expander))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_expander_new

(test gtk-expander-new
  (let ((expander (gtk:expander-new "label")))
    (is-false (gtk:expander-child expander))
    (is-false (gtk:expander-expanded expander))
    (is (string= "label" (gtk:expander-label expander)))
    (is (typep (gtk:expander-label-widget expander) 'gtk:label))
    (is-false (gtk:expander-resize-toplevel expander))
    (is-false (gtk:expander-use-markup expander))
    (is-false (gtk:expander-use-underline expander))))

;;;     gtk_expander_new_with_mnemonic

(test gtk-expander-new-with-mnemonic
  (let ((expander (gtk:expander-new-with-mnemonic "_label")))
    (is-false (gtk:expander-child expander))
    (is-false (gtk:expander-expanded expander))
    (is (string= "_label" (gtk:expander-label expander)))
    (is (typep (gtk:expander-label-widget expander) 'gtk:label))
    (is-false (gtk:expander-resize-toplevel expander))
    (is-false (gtk:expander-use-markup expander))
    (is-true (gtk:expander-use-underline expander))))

;;; 2024-4-17
