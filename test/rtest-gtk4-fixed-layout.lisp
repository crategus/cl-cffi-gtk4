(in-package :gtk-test)

(def-suite gtk-fixed-layout :in gtk-layout-managers)
(in-suite gtk-fixed-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFixedLayoutChild

(test gtk-fixed-layout-child-class
  ;; Check type
  (is (g:type-is-object "GtkFixedLayoutChild"))
  ;; Check registered name
  (is (eq 'gtk:fixed-layout-child
          (glib:symbol-for-gtype "GtkFixedLayoutChild")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFixedLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_layout_child_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutChild")
          (g:type-parent "GtkFixedLayoutChild")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFixedLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkFixedLayoutChild")))
  ;; Check properties
  (is (equal '("transform")
             (glib-test:list-properties "GtkFixedLayoutChild")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFixedLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFixedLayoutChild"
                                      GTK:FIXED-LAYOUT-CHILD
                      (:SUPERCLASS GTK:LAYOUT-CHILD
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_fixed_layout_child_get_type")
                      ((TRANSFORM FIXED-LAYOUT-CHILD-TRANSFORM
                        "transform" "GskTransform" T T)))
             (gobject:get-gtype-definition "GtkFixedLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-fixed-layout-child-transform
  (let ((fixed (make-instance 'gtk:fixed))
        (button (make-instance 'gtk:button))
        layout childlayout)
    ;; Put button in the fixed widget
    (is-false (gtk:fixed-put fixed button 10 20))
    ;; Get Layout Manager and Layout Manager for child widget
    (is (typep (setf layout (gtk:widget-layout-manager fixed))
               'gtk:layout-manager))
    (is (typep (setf childlayout
                    (gtk:layout-manager-layout-child layout button))
               'gtk:fixed-layout-child))
    (is (eq button (gtk:layout-child-child-widget childlayout)))
    (is (eq layout (gtk:layout-child-layout-manager childlayout)))

    (is (string= "translate(10, 20)"
                 (gsk:transform-to-string
                     (gtk:fixed-child-transform fixed button))))
    ;; TODO: Should return the transform, but gives an critical error!?
;    (is-false (gtk:fixed-layout-child-transform childlayout))

    (is-false (gtk:fixed-remove fixed button))
    (is-false (setf (gtk:widget-layout-manager button) nil))
    (is-false (setf (gtk:widget-layout-manager fixed) nil))

    (is (= 1 (g:object-ref-count fixed)))
    (is (= 1 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count layout)))
    (is (= 1 (g:object-ref-count childlayout)))))

;;;     GtkFixedLayout

(test gtk-fixed-layout-class
  ;; Check type
  (is (g:type-is-object "GtkFixedLayout"))
  ;; Check registered name
  (is (eq 'gtk:fixed-layout
          (glib:symbol-for-gtype "GtkFixedLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFixedLayout")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkFixedLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFixedLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkFixedLayout")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkFixedLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFixedLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFixedLayout" GTK:FIXED-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_fixed_layout_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkFixedLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_layout_new

(test gtk-fixed-layout-new
  (is (typep (gtk:fixed-layout-new) 'gtk:fixed-layout)))

;;; 2024-9-19
