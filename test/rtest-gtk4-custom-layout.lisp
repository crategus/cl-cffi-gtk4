(in-package :gtk-test)

(def-suite gtk-custom-layout :in gtk-suite)
(in-suite gtk-custom-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkCustomLayout

(test gtk-custom-layout-class
  ;; Check type
  (is (g:type-is-object "GtkCustomLayout"))
  ;; Check registered name
  (is (eq 'gtk:custom-layout
          (glib:symbol-for-gtype "GtkCustomLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkCustomLayout")
          (g:gtype (cffi:foreign-funcall "gtk_custom_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkLayoutManager")
          (g:type-parent "GtkCustomLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkCustomLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkCustomLayout")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkCustomLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkCustomLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkCustomLayout" GTK:CUSTOM-LAYOUT
                      (:SUPERCLASS GTK:LAYOUT-MANAGER
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_custom_layout_get_type")
                      NIL)
             (gobject:get-gtype-definition "GtkCustomLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkCustomRequestModeFunc
;;;     GtkCustomMeasureFunc
;;;     GtkCustomAllocateFunc

;;;     gtk_custom_layout_new

;;; 2024-9-19
