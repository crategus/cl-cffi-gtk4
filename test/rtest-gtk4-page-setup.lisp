(in-package :gtk-test)

(def-suite gtk-page-setup :in gtk-suite)
(in-suite gtk-page-setup)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetup

(test gtk-page-setup-class
  ;; Check type
  (is (g:type-is-object "GtkPageSetup"))
  ;; Check registered name
  (is (eq 'gtk:page-setup
          (glib:symbol-for-gtype "GtkPageSetup")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPageSetup")
          (g:gtype (cffi:foreign-funcall "gtk_page_setup_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPageSetup")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPageSetup")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPageSetup")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkPageSetup")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPageSetup")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPageSetup" GTK:PAGE-SETUP
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_page_setup_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkPageSetup"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_new

(test gtk-page-setup-new
  (is (typep (gtk:page-setup-new) 'gtk:page-setup)))

;;;     gtk_page_setup_new_from_file

(test gtk-page-setup-new-from-file
  (let ((path (glib-sys:sys-path "test/resource/page-setup.ini")))
    (is (typep (gtk:page-setup-new-from-file path) 'gtk:page-setup))
    (is (typep (gtk:page-setup-new-from-file (namestring path))
               'gtk:page-setup))))

;;;     gtk_page_setup_new_from_key_file

(test gtk-page-setup-new-from-key-file
  (glib:with-key-file (keyfile)
    (let ((path (glib-sys:sys-path "test/resource/page-setup.ini")))
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is (typep (gtk:page-setup-new-from-key-file keyfile nil)
                 'gtk:page-setup)))))

;;;     gtk_page_setup_new_from_gvariant

(test gtk-page-setup-new-from-gvariant
  (let ((variant (gtk:page-setup-to-gvariant (gtk:page-setup-new))))
    (is (typep (gtk:page-setup-new-from-gvariant variant) 'gtk:page-setup))))

;;;     gtk_page_setup_copy

(test gtk-page-setup-copy
  (is (typep (gtk:page-setup-copy (gtk:page-setup-new)) 'gtk:page-setup)))

;;;     gtk_page_setup_get_orientation
;;;     gtk_page_setup_set_orientation

(test gtk-page-setup-orientation
  (let ((setup (gtk:page-setup-new)))
    (is (eq :portrait (gtk:page-setup-orientation setup)))
    (is (eq :landscape (setf (gtk:page-setup-orientation setup) :landscape)))
    (is (eq :landscape (gtk:page-setup-orientation setup)))))

;;;     gtk_page_setup_get_paper_size
;;;     gtk_page_setup_set_paper_size

(test gtk-page-setup-paper-size
  (let ((setup (gtk:page-setup-new)))
    (is (string= "iso_a4"
                 (gtk:paper-size-name (gtk:page-setup-paper-size setup))))
    (is (string= "iso_a3"
                 (gtk:paper-size-name (setf (gtk:page-setup-paper-size setup)
                                            (gtk:paper-size-new "iso_a3")))))
    (is (string= "iso_a3"
                 (gtk:paper-size-name (gtk:page-setup-paper-size setup))))))

;;;     gtk_page_setup_get_top_margin
;;;     gtk_page_setup_set_top_margin

(test gtk-page-setup-top-margin
  (let ((setup (gtk:page-setup-new)))
    (is (= 18.0d0 (gtk:page-setup-top-margin setup :points)))
    (is (= 0.25d0 (gtk:page-setup-top-margin setup :inch)))
    (is (= 6.35d0 (gtk:page-setup-top-margin setup :mm)))
    (is (= 25.0d0 (setf (gtk:page-setup-top-margin setup :mm) 25)))
    (is (= 25.0d0 (gtk:page-setup-top-margin setup :mm)))))

;;;     gtk_page_setup_get_bottom_margin
;;;     gtk_page_setup_set_bottom_margin

(test gtk-page-setup-bottom-margin
  (let ((setup (gtk:page-setup-new)))
    (is (= 14.224d0 (gtk:page-setup-bottom-margin setup :mm)))
    (is (= 25.0d0 (setf (gtk:page-setup-bottom-margin setup :mm) 25)))
    (is (= 25.0d0 (gtk:page-setup-bottom-margin setup :mm)))))

;;;     gtk_page_setup_get_left_margin
;;;     gtk_page_setup_set_left_margin

(test gtk-page-setup-left-margin
  (let ((setup (gtk:page-setup-new)))
    (is (= 6.35d0 (gtk:page-setup-left-margin setup :mm)))
    (is (= 25.0d0 (setf (gtk:page-setup-left-margin setup :mm) 25)))
    (is (= 25.0d0 (gtk:page-setup-left-margin setup :mm)))))

;;;     gtk_page_setup_get_right_margin
;;;     gtk_page_setup_set_right_margin

(test gtk-page-setup-right-margin
  (let ((setup (gtk:page-setup-new)))
    (is (= 6.35d0 (gtk:page-setup-right-margin setup :mm)))
    (is (= 25.0d0 (setf (gtk:page-setup-right-margin setup :mm) 25)))
    (is (= 25.0d0 (gtk:page-setup-right-margin setup :mm)))))

;;;     gtk_page_setup_set_paper_size_and_default_margins
(test gtk-page-setp-set-paper-size-and-default-margins
  (let ((setup (gtk:page-setup-new))
        (paper (gtk:paper-size-new "iso_a4")))
    (is-false (gtk:page-setup-set-paper-size-and-default-margins setup paper))
    (is (string= "iso_a4"
                 (gtk:paper-size-name (gtk:page-setup-paper-size setup))))
    (is (= 6.35d0 (gtk:page-setup-top-margin setup :mm)))
    (is (= 14.224d0 (gtk:page-setup-bottom-margin setup :mm)))
    (is (= 6.35d0 (gtk:page-setup-left-margin setup :mm)))
    (is (= 6.35d0 (gtk:page-setup-right-margin setup :mm)))))

;;;     gtk_page_setup_get_paper_width
;;;     gtk_page_setup_get_paper_height

(test gtk-paper-setup-paper-width/height
  (let ((setup (gtk:page-setup-new)))
    (is (= 210.0d0 (gtk:page-setup-paper-width setup :mm)))
    (is (= 297.0d0 (gtk:page-setup-paper-height setup :mm)))))

;;;     gtk_page_setup_get_page_width
;;;     gtk_page_setup_get_page_height

(test gtk-paper-setup-page-width/height
  (let ((setup (gtk:page-setup-new)))
    (is (= 197.3d0 (gtk:page-setup-page-width setup :mm)))
    (is (= 276.426d0 (gtk:page-setup-page-height setup :mm)))))

;;;     gtk_page_setup_load_file

(test gtk-page-setup-load-file
  (let ((setup (gtk:page-setup-new))
        (path (glib-sys:sys-path "test/resource/page-setup.ini")))
    (is-true (gtk:page-setup-load-file setup path))
    (is-true (gtk:page-setup-load-file setup (namestring path)))))

;;;     gtk_page_setup_load_key_file

(test gtk-page-setup-load-key-file
  (glib:with-key-file (keyfile)
    (let ((setup (gtk:page-setup-new))
          (path (glib-sys:sys-path "test/resource/page-setup.ini")))
      (is-true (g:key-file-load-from-file keyfile path :none))
      (is-true (gtk:page-setup-load-key-file setup keyfile nil)))))

;;;     gtk_page_setup_to_file

(test gtk-page-setup-to-file
  (let ((setup (gtk:page-setup-new))
        (path (glib-sys:sys-path "test/out/page-setup.txt")))
    (is-true (gtk:page-setup-to-file setup path))
    (is-true (gtk:page-setup-to-file setup (namestring path)))))

;;;     gtk_page_setup_to_key_file

(test gtk-page-setup-to-key-file
  (glib:with-key-file (keyfile)
    (let ((setup (gtk:page-setup-new))
          (path (glib-sys:sys-path "test/out/page-setup.ini")))
      (is-false (gtk:page-setup-to-key-file setup keyfile nil))
      (is-true (g:key-file-save-to-file keyfile path)))))

;;;     gtk_page_setup_to_gvariant

(test gtk-page-setup-to-gvariant
  (let ((setup (gtk:page-setup-new))
        (variant nil))
    (is (cffi:pointerp (setf variant
                             (gtk:page-setup-to-gvariant setup))))
    (is (string= "{'PPDName': <'A4'>, 'DisplayName': <'A4'>, 'Width': <210.0>, 'Height': <297.0>, 'MarginTop': <6.3499999999999996>, 'MarginBottom': <14.224>, 'MarginLeft': <6.3499999999999996>, 'MarginRight': <6.3499999999999996>, 'Orientation': <'portrait'>}"
                 (g:variant-print variant)))))

;;; 2024-11-25
