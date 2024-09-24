(in-package :gtk-test)

(def-suite gtk-action-bar :in gtk-suite)
(in-suite gtk-action-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkActionBar

(test gtk-action-bar-class
  ;; Check Type
  (is (g:type-is-object "GtkActionBar"))
  ;; Check registered name
  (is (eq 'gtk:action-bar
          (glib:symbol-for-gtype "GtkActionBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkActionBar")
          (g:gtype (cffi:foreign-funcall "gtk_action_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkActionBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkActionBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkActionBar")))
  ;; Check properties
  (is (equal '("revealed")
             (glib-test:list-properties "GtkActionBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkActionBar")))
  ;; Check CSS name
  (is (string= "actionbar"
               (gtk:widget-class-css-name "GtkActionBar")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkActionBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkActionBar" GTK:ACTION-BAR
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_action_bar_get_type")
                      ((REVEALED ACTION-BAR-REVEALED "revealed" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkActionBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-action-bar-revealed
  (let ((actionbar (make-instance 'gtk:action-bar)))
    (is-true (gtk:action-bar-revealed actionbar))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_action_bar_new

(test gtk-action-bar-new
  (is (typep (gtk:action-bar-new) 'gtk:action-bar)))

;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_remove
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget

;;; 2024-9-19
