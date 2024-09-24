(in-package :gtk-test)

(def-suite gtk-accessible-text :in gtk-suite)
(in-suite gtk-accessible-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleText

(test gtk-accessible-text-interface
  ;; Check type
  (is (g:type-is-interface "GtkAccessibleText"))
  ;; Check registered name
  (is (eq 'gtk:accessible-text
          (glib:symbol-for-gtype "GtkAccessibleText")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleText")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_text_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkAccessible" "GObject")
             (glib-test:list-interface-prerequisites "GtkAccessibleText")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkAccessibleText")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAccessibleText")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAccessibleText" GTK:ACCESSIBLE-TEXT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_accessible_text_get_type"))
             (gobject:get-gtype-definition "GtkAccessibleText"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_text_get_attributes
;;;     gtk_accessible_text_get_caret_position
;;;     gtk_accessible_text_get_contents
;;;     gtk_accessible_text_get_contents_at
;;;     gtk_accessible_text_get_default_attributes
;;;     gtk_accessible_text_get_extents                     Since 4.16
;;;     gtk_accessible_text_get_offset                      Since 4.16
;;;     gtk_accessible_text_get_selection

;;;     gtk_accessible_text_update_caret_position
;;;     gtk_accessible_text_update_contents
;;;     gtk_accessible_text_update_selection_bound

;;; 2024-9-19
