(in-package :gtk-test)

(def-suite gtk-print-context :in gtk-printing)
(in-suite gtk-print-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintContext

(test gtk-print-context-class
  ;; Check type
  (is (g:type-is-object "GtkPrintContext"))
  ;; Check registered name
  (is (eq 'gtk:print-context
          (glib:symbol-for-gtype "GtkPrintContext")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintContext")
          (g:gtype (cffi:foreign-funcall "gtk_print_context_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintContext")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPrintContext")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrintContext")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkPrintContext")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPrintContext")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintContext" GTK:PRINT-CONTEXT
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_context_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkPrintContext"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_print_context_get_cairo_context
;;;     gtk_print_context_set_cairo_context
;;;     gtk_print_context_get_page_setup
;;;     gtk_print_context_get_width
;;;     gtk_print_context_get_height
;;;     gtk_print_context_get_dpi_x
;;;     gtk_print_context_get_dpi_y
;;;     gtk_print_context_get_pango_fontmap
;;;     gtk_print_context_create_pango_context
;;;     gtk_print_context_create_pango_layout
;;;     gtk_print_context_get_hard_margins

;;; 2024-9-20
