(in-package :gtk-test)

(def-suite gtk-print-context :in gtk-suite)
(in-suite gtk-print-context)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPrintContext

(test gtk-print-context-class
  ;; Type check
  (is (g:type-is-object "GtkPrintContext"))
  ;; Check the registered name
  (is (eq 'gtk:print-context
          (glib:symbol-for-gtype "GtkPrintContext")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPrintContext")
          (g:gtype (cffi:foreign-funcall "gtk_print_context_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintContext")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPrintContext")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkPrintContext")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkPrintContext")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPrintContext")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPrintContext" GTK-PRINT-CONTEXT
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_print_context_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkPrintContext"))))

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

;;; 2024-2-16
