(in-package :gtk-test)

(def-suite gtk-layout-child :in gtk-layout-managers)
(in-suite gtk-layout-child)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLayoutChild

(test gtk-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:layout-child
          (glib:symbol-for-gtype "GtkLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_layout_child_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkLayoutChild")))
  ;; Check children
  (is (equal '("GtkConstraintLayoutChild" "GtkFixedLayoutChild"
               "GtkGridLayoutChild" "GtkOverlayLayoutChild")
             (glib-test:list-children "GtkLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkLayoutChild")))
  ;; Check properties
  (is (equal '("child-widget" "layout-manager")
             (glib-test:list-properties "GtkLayoutChild")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLayoutChild" GTK:LAYOUT-CHILD
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_layout_child_get_type")
                       ((CHILD-WIDGET LAYOUT-CHILD-CHILD-WIDGET
                         "child-widget" "GtkWidget" T NIL)
                        (LAYOUT-MANAGER LAYOUT-CHILD-LAYOUT-MANAGER
                         "layout-manager" "GtkLayoutManager" T NIL)))
             (gobject:get-gtype-definition "GtkLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-layout-child-properties
  (let ((fixed (make-instance 'gtk:fixed))
        (button (make-instance 'gtk:button))
        manager childmanager)
    ;; Put button in the fixed widget
    (is-false (gtk:fixed-put fixed button 0 0))
    ;; Get Layout Manager and Layout Manager for child widget
    (is (typep (setf manager (gtk:widget-layout-manager fixed))
               'gtk:layout-manager))
    (is (typep (setf childmanager
                    (gtk:layout-manager-layout-child manager button))
               'gtk:layout-child))
    (is (eq button (gtk:layout-child-child-widget childmanager)))
    (is (eq manager (gtk:layout-child-layout-manager childmanager)))

    (is-false (gtk:fixed-remove fixed button))
    (is-false (setf (gtk:widget-layout-manager fixed) nil))

    (is (= 1 (g:object-ref-count fixed)))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count childmanager)))
    (is (= 1 (g:object-ref-count manager)))))

;;; 2024-10-19
