(in-package :gtk-test)

(def-suite gtk-page-setup :in gtk-suite)
(in-suite gtk-page-setup)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetup

(test gtk-page-setup-class
  ;; Type check
  (is (g:type-is-object "GtkPageSetup"))
  ;; Check the registered name
  (is (eq 'gtk:page-setup
          (glib:symbol-for-gtype "GtkPageSetup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPageSetup")
          (g:gtype (cffi:foreign-funcall "gtk_page_setup_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPageSetup")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPageSetup")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkPageSetup")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkPageSetup")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPageSetup")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPageSetup" GTK-PAGE-SETUP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_page_setup_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkPageSetup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_new

(test gtk-page-setup-new
  (is (typep (gtk:page-setup-new) 'gtk:page-setup)))

;;;     gtk_page_setup_new_from_file
;;;     gtk_page_setup_new_from_key_file
;;;     gtk_page_setup_new_from_gvariant

;;;     gtk_page_setup_copy
;;;     gtk_page_setup_get_orientation
;;;     gtk_page_setup_set_orientation
;;;     gtk_page_setup_get_paper_size
;;;     gtk_page_setup_set_paper_size
;;;     gtk_page_setup_get_top_margin
;;;     gtk_page_setup_set_top_margin
;;;     gtk_page_setup_get_bottom_margin
;;;     gtk_page_setup_set_bottom_margin
;;;     gtk_page_setup_get_left_margin
;;;     gtk_page_setup_set_left_margin
;;;     gtk_page_setup_get_right_margin
;;;     gtk_page_setup_set_right_margin
;;;     gtk_page_setup_set_paper_size_and_default_margins
;;;     gtk_page_setup_get_paper_width
;;;     gtk_page_setup_get_paper_height
;;;     gtk_page_setup_get_page_width
;;;     gtk_page_setup_get_page_height
;;;     gtk_page_setup_load_file
;;;     gtk_page_setup_load_key_file
;;;     gtk_page_setup_to_file
;;;     gtk_page_setup_to_key_file
;;;     gtk_page_setup_to_gvariant

;;; --- 2023-8-28 --------------------------------------------------------------
