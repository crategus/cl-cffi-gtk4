(in-package :gtk-test)

(def-suite gtk-action-bar :in gtk-suite)
(in-suite gtk-action-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActionBar

(test gtk-action-bar-class
  ;; Type check
  (is (g:type-is-object "GtkActionBar"))
  ;; Check the registered name
  (is (eq 'gtk:action-bar
          (glib:symbol-for-gtype "GtkActionBar")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkActionBar")
          (g:gtype (cffi:foreign-funcall "gtk_action_bar_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkActionBar")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkActionBar")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkActionBar")))
  ;; Check the properties
  (is (equal '("revealed")
             (list-properties "GtkActionBar")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkActionBar")))
  ;; CSS name
  (is (string= "actionbar"
               (gtk:widget-class-css-name "GtkActionBar")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:action-bar))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkActionBar")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkActionBar" GTK-ACTION-BAR
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_action_bar_get_type")
                               ((REVEALED GTK-ACTION-BAR-REVEALED "revealed"
                                 "gboolean" T T)))
             (gobject:get-g-type-definition "GtkActionBar"))))

;;; --- Properties -------------------------------------------------------------

;;;     revealed

(test gtk-action-bar-revealed
  (let ((actionbar (make-instance 'gtk:action-bar)))
    (is-true (gtk:action-bar-revealed actionbar))
))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_bar_new

(test gtk-action-bar-new
  (is (typep (gtk:action-bar-new) 'gtk:action-bar)))

;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_remove
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget

;;; --- 2023-8-9 ---------------------------------------------------------------
