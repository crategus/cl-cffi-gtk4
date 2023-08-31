(in-package :gtk-test)

(def-suite gtk-expander :in gtk-suite)
(in-suite gtk-expander)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkExpander

(test gtk-expander-class
  ;; Type check
  (is (g:type-is-object "GtkExpander"))
  ;; Check the registered name
  (is (eq 'gtk:expander
          (glib:symbol-for-gtype "GtkExpander")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkExpander")
          (g:gtype (cffi:foreign-funcall "gtk_expander_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkExpander")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkExpander")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkExpander")))
  ;; Check the properties
  (is (equal '("child" "expanded" "label" "label-widget" "resize-toplevel"
               "use-markup" "use-underline")
             (list-properties "GtkExpander")))
  ;; Check the signals
  (is (equal '("activate")
             (list-signals "GtkExpander")))
  ;; CSS name
  (is (string= "expander-widget"
               (gtk:widget-class-css-name "GtkExpander")))
  ;; CSS style context
  (is (string=
"expander-widget:dir(ltr)
  box.vertical:dir(ltr)
    title.horizontal:dir(ltr)
      expander.horizontal:dir(ltr)
"
               (print-style-context "GtkExpander")))
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

;;;     child
;;;     expanded
;;;     label
;;;     label-widget
;;;     resize-toplevel
;;;     use-markup
;;;     use-underline

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

;;; --- 2023-8-23 --------------------------------------------------------------
