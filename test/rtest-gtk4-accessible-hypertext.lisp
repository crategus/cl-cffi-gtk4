(in-package :gtk-test)

(def-suite gtk-accessible-hypertext :in gtk-accessibility)
(in-suite gtk-accessible-hypertext)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleHypertext

(test gtk-accessible-hypertext-interface
  ;; Check type
  (is (g:type-is-interface "GtkAccessibleHypertext"))
  ;; Check registered name
  (is (eq 'gtk:accessible-hypertext
          (glib:symbol-for-gtype "GtkAccessibleHypertext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleHypertext")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_hypertext_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkAccessible" "GObject")
             (glib-test:list-interface-prerequisites "GtkAccessibleHypertext")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkAccessibleHypertext")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAccessibleHypertext")))
  ;; Get interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAccessibleHypertext"
                                         GTK:ACCESSIBLE-HYPERTEXT
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_accessible_hypertext_get_type"))
             (gobject:get-gtype-definition "GtkAccessibleHypertext"))))

;;; 2026-07-11
