(in-package :gtk-test)

(def-suite gtk-fixed-layout :in gtk-suite)
(in-suite gtk-fixed-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFixedLayoutChild

(test gtk-fixed-layout-child-class
  ;; Type check
  (is (g:type-is-object "GtkFixedLayoutChild"))
  ;; Check the registered name
  (is (eq 'gtk:fixed-layout-child
          (glib:symbol-for-gtype "GtkFixedLayoutChild")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFixedLayoutChild")
          (g:gtype (cffi:foreign-funcall "gtk_fixed_layout_child_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkLayoutChild")
          (g:type-parent "GtkFixedLayoutChild")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFixedLayoutChild")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkFixedLayoutChild")))
  ;; Check the properties
  (is (equal '("transform")
             (list-properties "GtkFixedLayoutChild")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFixedLayoutChild")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFixedLayoutChild" GTK-FIXED-LAYOUT-CHILD
                       (:SUPERCLASS GTK-LAYOUT-CHILD :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_fixed_layout_child_get_type")
                       ((TRANSFORM GTK-FIXED-LAYOUT-CHILD-TRANSFORM "transform"
                         "GskTransform" T T)))
             (gobject:get-g-type-definition "GtkFixedLayoutChild"))))

;;; --- Properties -------------------------------------------------------------

;;;     transform

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
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkFixedLayout" GTK-FIXED-LAYOUT
                       (:SUPERCLASS GTK-LAYOUT-MANAGER :EXPORT T :INTERFACES
                        NIL :TYPE-INITIALIZER "gtk_fixed_layout_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkFixedLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_fixed_layout_new

;;; --- 2023-5-29 --------------------------------------------------------------
