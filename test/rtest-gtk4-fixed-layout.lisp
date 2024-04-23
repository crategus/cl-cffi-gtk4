(in-package :gtk-test)

(def-suite gtk-fixed-layout :in gtk-suite)
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
             (list-children "GtkFixedLayoutChild")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkFixedLayoutChild")))
  ;; Check properties
  (is (equal '("transform")
             (list-properties "GtkFixedLayoutChild")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkFixedLayoutChild")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFixedLayoutChild" GTK-FIXED-LAYOUT-CHILD
                       (:SUPERCLASS GTK-LAYOUT-CHILD :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_fixed_layout_child_get_type")
                       ((TRANSFORM GTK-FIXED-LAYOUT-CHILD-TRANSFORM "transform"
                         "GskTransform" T T)))
             (gobject:get-g-type-definition "GtkFixedLayoutChild"))))

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
))

;;;     GtkFixedLayout

(test gtk-fixed-layout-class
  ;; Type check
  (is (g:type-is-object "GtkFixedLayout"))
  ;; Check the registered name
  (is (eq 'gtk:fixed-layout
          (glib:symbol-for-gtype "GtkFixedLayout")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFixedLayout")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_layout_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkFixedLayout")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFixedLayout")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkFixedLayout")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkFixedLayout")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFixedLayout")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFixedLayout" GTK-FIXED-LAYOUT
                       (:SUPERCLASS GTK-LAYOUT-MANAGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_fixed_layout_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFixedLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_layout_new

(test gtk-fixed-layout-new
  (is (typep (gtk:fixed-layout-new) 'gtk:fixed-layout)))

;;; --- 2023-5-29 --------------------------------------------------------------
