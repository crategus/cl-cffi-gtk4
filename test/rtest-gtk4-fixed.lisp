(in-package :gtk-test)

(def-suite gtk-fixed :in gtk-layout-widgets)
(in-suite gtk-fixed)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFixed

(test gtk-fixed-class
  ;; Check type
  (is (g:type-is-object "GtkFixed"))
  ;; Check registered name
  (is (eq 'gtk:fixed
          (glib:symbol-for-gtype "GtkFixed")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFixed")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFixed")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFixed")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkFixed")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkFixed")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFixed")))
  ;; Check CSS name
  (is (string= "widget"
               (gtk:widget-class-css-name "GtkFixed")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkFixed")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFixed" GTK:FIXED
                      (:SUPERCLASS GTK:WIDGET
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_fixed_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkFixed"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_new

(test gtk-fixed-new
  (glib-test:with-check-memory (fixed)
    (is (typep (setf fixed (gtk:fixed-new)) 'gtk:fixed))))

;;;     gtk_fixed_put

(test gtk-fixed-put
  (glib-test:with-check-memory (fixed button1 button2)
    (setf fixed (gtk:fixed-new))
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (is-false (gtk:fixed-put fixed button1 10 10))
    (is-false (gtk:fixed-put fixed button2 100 100))
    (is (eq button1 (gtk:widget-first-child fixed)))
    (is (eq button2 (gtk:widget-last-child fixed)))
    ;; Remove references
    (is-false (gtk:fixed-remove fixed button1))
    (is-false (gtk:fixed-remove fixed button2))))

;;;     gtk_fixed_remove

(test gtk-fixed-remove
  (glib-test:with-check-memory (fixed button1 button2)
    (setf fixed (gtk:fixed-new))
    (setf button1 (make-instance 'gtk:button))
    (setf button2 (make-instance 'gtk:button))
    (is-false (gtk:fixed-put fixed button1 10 10))
    (is-false (gtk:fixed-put fixed button2 100 100))
    (is (eq button1 (gtk:widget-first-child fixed)))
    (is (eq button2 (gtk:widget-last-child fixed)))
    (is-false (gtk:fixed-remove fixed button2))
    (is (eq button1 (gtk:widget-last-child fixed)))
    ;; Remove references
    (is-false (gtk:fixed-remove fixed button1))))

;;;     gtk_fixed_move

(test gtk-fixed-move
  (glib-test:with-check-memory (fixed button)
    (setf fixed (gtk:fixed-new))
    (setf button (make-instance 'gtk:button))
    (is-false (gtk:fixed-put fixed button 10 10))
    (is-false (gtk:fixed-move fixed button 20 20))
    ;; TODO: Position is not (10, 10).
    (is (equal '(0.0d0 0.0d0)
               (multiple-value-list (gtk:fixed-child-position fixed button))))
    ;; Remove references
    (is-false (gtk:fixed-remove fixed button))))

;;;     gtk_fixed_get_child_position

(test gtk-fixed-child-position
  (glib-test:with-check-memory (fixed button)
    (setf fixed (gtk:fixed-new))
    (setf button (make-instance 'gtk:button))
    (is-false (gtk:fixed-put fixed button 10 10))
    ;; Position is not (10, 10), but (0, 0)
    (is (equal '(0.0d0 0.0d0)
               (multiple-value-list (gtk:fixed-child-position fixed button))))
    ;; Remove references
    (is-false (gtk:fixed-remove fixed button))))

;;;     gtk_fixed_get_child_transform
;;;     gtk_fixed_set_child_transform

(test gtk-fixed-child-transform
  (glib-test:with-check-memory (fixed button)
    (let (transform)
      (setf fixed (gtk:fixed-new))
      (setf button (make-instance 'gtk:button))
      (is-false (gtk:fixed-put fixed button 10 10))
      (is (typep (setf transform
                       (gtk:fixed-child-transform fixed button)) 'gsk:transform))
      (is (string= "translate(10, 10)" (gsk:transform-to-string transform)))
      (is-false (gtk:fixed-move fixed button 25 25))
      (is (typep (setf transform
                       (gtk:fixed-child-transform fixed button)) 'gsk:transform))
      (is (string= "translate(25, 25)" (gsk:transform-to-string transform)))
      ;; Remove references
      (is-false (gtk:fixed-remove fixed button)))))

;;; 2025-3-25
