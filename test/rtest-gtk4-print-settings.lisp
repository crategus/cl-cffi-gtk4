(in-package :gtk-test)

(def-suite gtk-print-settings :in gtk-suite)
(in-suite gtk-print-settings)

;; Ensure a file with print settings for use in the following tests
(test ensure-print-settings
  (let* ((path (glib-sys:sys-path "test/out/print-settings.ini"))
         (variant (g:variant-parse "a{sv}"
                                   "{'scale': <'100'>,
                                     'number-up': <'1'>,
                                     'n-copies': <'1'>,
                                     'page-ranges': <'0-11'>,
                                     'page-set': <'all'>,
                                     'printer': <'In Datei drucken'>,
                                     'print-pages': <'ranges'>,
                                     'reverse': <'false'>,
                                     'collate': <'false'>,
                                     'output-file-format': <'pdf'>}"))
         (settings (gtk:print-settings-new-from-gvariant variant)))
    (unless (probe-file path)
      (gtk:print-settings-to-file settings path))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageOrientation

(test gtk-page-orientation
  ;; Check type
  (is (g:type-is-enum "GtkPageOrientation"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPageOrientation")
          (g:gtype (cffi:foreign-funcall "gtk_page_orientation_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:page-orientation
          (glib:symbol-for-gtype "GtkPageOrientation")))
  ;; Check names
  (is (equal '("GTK_PAGE_ORIENTATION_PORTRAIT" "GTK_PAGE_ORIENTATION_LANDSCAPE"
               "GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT"
               "GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE")
             (glib-test:list-enum-item-names "GtkPageOrientation")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPageOrientation")))
  ;; Check nick names
  (is (equal '("portrait" "landscape" "reverse-portrait" "reverse-landscape")
             (glib-test:list-enum-item-nicks "GtkPageOrientation")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPageOrientation" GTK:PAGE-ORIENTATION
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_page_orientation_get_type")
                       (:PORTRAIT 0)
                       (:LANDSCAPE 1)
                       (:REVERSE-PORTRAIT 2)
                       (:REVERSE-LANDSCAPE 3))
             (gobject:get-gtype-definition "GtkPageOrientation"))))

;;;     GtkPrintDuplex

(test gtk-print-duplex
  ;; Check type
  (is (g:type-is-enum "GtkPrintDuplex"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintDuplex")
          (g:gtype (cffi:foreign-funcall "gtk_print_duplex_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:print-duplex
          (glib:symbol-for-gtype "GtkPrintDuplex")))
  ;; Check names
  (is (equal '("GTK_PRINT_DUPLEX_SIMPLEX" "GTK_PRINT_DUPLEX_HORIZONTAL"
               "GTK_PRINT_DUPLEX_VERTICAL")
             (glib-test:list-enum-item-names "GtkPrintDuplex")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkPrintDuplex")))
  ;; Check nick names
  (is (equal '("simplex" "horizontal" "vertical")
             (glib-test:list-enum-item-nicks "GtkPrintDuplex")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPrintDuplex" GTK:PRINT-DUPLEX
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_print_duplex_get_type")
                       (:SIMPLEX 0)
                       (:HORIZONTAL 1)
                       (:VERTICAL 2))
             (gobject:get-gtype-definition "GtkPrintDuplex"))))

;;;     GtkPrintQuality

(test gtk-print-quality
  ;; Check type
  (is (g:type-is-enum "GtkPrintQuality"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintQuality")
          (g:gtype (cffi:foreign-funcall "gtk_print_quality_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:print-quality
          (glib:symbol-for-gtype "GtkPrintQuality")))
  ;; Check names
  (is (equal '("GTK_PRINT_QUALITY_LOW" "GTK_PRINT_QUALITY_NORMAL"
               "GTK_PRINT_QUALITY_HIGH" "GTK_PRINT_QUALITY_DRAFT")
             (glib-test:list-enum-item-names "GtkPrintQuality")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPrintQuality")))
  ;; Check nick names
  (is (equal '("low" "normal" "high" "draft")
             (glib-test:list-enum-item-nicks "GtkPrintQuality")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPrintQuality" GTK:PRINT-QUALITY
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_print_quality_get_type")
                       (:LOW 0)
                       (:NORMAL 1)
                       (:HIGH 2)
                       (:DRAFT 3))
             (gobject:get-gtype-definition "GtkPrintQuality"))))

;;;     GtkNumberUpLayout

(test gtk-number-up-layout
  ;; Check type
  (is (g:type-is-enum "GtkNumberUpLayout"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNumberUpLayout")
          (g:gtype (cffi:foreign-funcall "gtk_number_up_layout_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:number-up-layout
          (glib:symbol-for-gtype "GtkNumberUpLayout")))
  ;; Check names
  (is (equal '("GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_TOP_TO_BOTTOM"
               "GTK_NUMBER_UP_LAYOUT_LEFT_TO_RIGHT_BOTTOM_TO_TOP"
               "GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_TOP_TO_BOTTOM"
               "GTK_NUMBER_UP_LAYOUT_RIGHT_TO_LEFT_BOTTOM_TO_TOP"
               "GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_LEFT_TO_RIGHT"
               "GTK_NUMBER_UP_LAYOUT_TOP_TO_BOTTOM_RIGHT_TO_LEFT"
               "GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_LEFT_TO_RIGHT"
               "GTK_NUMBER_UP_LAYOUT_BOTTOM_TO_TOP_RIGHT_TO_LEFT")
             (glib-test:list-enum-item-names "GtkNumberUpLayout")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GtkNumberUpLayout")))
  ;; Check nick names
  (is (equal '("lrtb" "lrbt" "rltb" "rlbt" "tblr" "tbrl" "btlr" "btrl")
             (glib-test:list-enum-item-nicks "GtkNumberUpLayout")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkNumberUpLayout" GTK:NUMBER-UP-LAYOUT
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_number_up_layout_get_type")
                       (:LRTB 0)
                       (:LRBT 1)
                       (:RLTB 2)
                       (:RLBT 3)
                       (:TBLR 4)
                       (:TBRL 5)
                       (:BTLR 6)
                       (:BTRL 7))
             (gobject:get-gtype-definition "GtkNumberUpLayout"))))

;;;     GtkPrintPages

(test gtk-print-pages
  ;; Check type
  (is (g:type-is-enum "GtkPrintPages"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintPages")
          (g:gtype (cffi:foreign-funcall "gtk_print_pages_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:print-pages
          (glib:symbol-for-gtype "GtkPrintPages")))
  ;; Check names
  (is (equal '("GTK_PRINT_PAGES_ALL" "GTK_PRINT_PAGES_CURRENT"
               "GTK_PRINT_PAGES_RANGES" "GTK_PRINT_PAGES_SELECTION")
             (glib-test:list-enum-item-names "GtkPrintPages")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkPrintPages")))
  ;; Check nick names
  (is (equal '("all" "current" "ranges" "selection")
             (glib-test:list-enum-item-nicks "GtkPrintPages")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPrintPages" GTK:PRINT-PAGES
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_print_pages_get_type")
                       (:ALL 0)
                       (:CURRENT 1)
                       (:RANGES 2)
                       (:SELECTION 3))
             (gobject:get-gtype-definition "GtkPrintPages"))))

;;;     GtkPageRange

;;;     GtkPageSet

(test gtk-page-set
  ;; Check type
  (is (g:type-is-enum "GtkPageSet"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPageSet")
          (g:gtype (cffi:foreign-funcall "gtk_page_set_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:page-set
          (glib:symbol-for-gtype "GtkPageSet")))
  ;; Check names
  (is (equal '("GTK_PAGE_SET_ALL" "GTK_PAGE_SET_EVEN" "GTK_PAGE_SET_ODD")
             (glib-test:list-enum-item-names "GtkPageSet")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkPageSet")))
  ;; Check nick names
  (is (equal '("all" "even" "odd")
             (glib-test:list-enum-item-nicks "GtkPageSet")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkPageSet" GTK:PAGE-SET
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_page_set_get_type")
                       (:ALL 0)
                       (:EVEN 1)
                       (:ODD 2))
             (gobject:get-gtype-definition "GtkPageSet"))))

;;;     GtkPrintSettings

(test gtk-print-settings-class
  ;; Check type
  (is (g:type-is-object "GtkPrintSettings"))
  ;; Check registered name
  (is (eq 'gtk:print-settings
          (glib:symbol-for-gtype "GtkPrintSettings")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPrintSettings")
          (g:gtype (cffi:foreign-funcall "gtk_print_settings_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkPrintSettings")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPrintSettings")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkPrintSettings")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkPrintSettings")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPrintSettings")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPrintSettings" GTK:PRINT-SETTINGS
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_print_settings_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkPrintSettings"))))

;;; --- Functions --------------------------------------------------------------

;;;     GtkPrintSettingsFunc

;;;     gtk_print_settings_new

(test gtk-print-settings-new
  (is (typep (gtk:print-settings-new) 'gtk:print-settings)))

;;;     gtk_print_settings_copy

(test gtk-print-settings-copy
  (let ((settings (gtk:print-settings-new)))
    (is (typep (gtk:print-settings-copy settings) 'gtk:print-settings))))

;;;     gtk_print_settings_has_key
;;;     gtk_print_settings_get
;;;     gtk_print_settings_set
;;;     gtk_print_settings_unset

(test gtk-print-settings-get/set
  (let* ((path (glib-sys:sys-path "test/out/print-settings.ini"))
         (settings (gtk:print-settings-new-from-file path)))

  (is-true (gtk:print-settings-has-key settings "scale"))
  (is (string= "100" (gtk:print-settings-get settings "scale")))
  (is-false (gtk:print-settings-set settings "scale" "200"))
  (is (string= "200" (gtk:print-settings-get settings "scale")))

  (is-true (gtk:print-settings-has-key settings "page-set"))
  (is (string= "all" (gtk:print-settings-get settings "page-set")))
  (is-false (gtk:print-settings-unset settings "page-set"))
  (is-false (gtk:print-settings-get settings "page-set"))

  (is-true (gtk:print-settings-has-key settings "printer"))
  (is (string= "In Datei drucken" (gtk:print-settings-get settings "printer")))
  (is-false (gtk:print-settings-set settings "printer" nil))
  (is-false (gtk:print-settings-get settings "printer"))))


;;;     gtk_print_settings_foreach

(test gtk-print-settings-foreach
  (let* ((path (glib-sys:sys-path "test/out/print-settings.ini"))
         (settings (gtk:print-settings-new-from-file path))
         values)
    (is-false (gtk:print-settings-foreach settings
        (lambda (key value)
          (push (list key value) values))))
    (is (equal '(("scale" "100")
                 ("number-up" "1")
                 ("n-copies" "1")
                 ("page-ranges" "0-11")
                 ("page-set" "all")
                 ("printer" "In Datei drucken")
                 ("print-pages" "ranges")
                 ("reverse" "false")
                 ("collate" "false")
                 ("output-file-format" "pdf"))
               (reverse values)))))

;;;     gtk_print_settings_get_bool
;;;     gtk_print_settings_set_bool

(test gtk-print-settings-bool
  (let* ((path (glib-sys:sys-path "test/out/print-settings.ini"))
         (settings (gtk:print-settings-new-from-file path)))
    (is-false (gtk:print-settings-bool settings "reverse"))
    (is-true (setf (gtk:print-settings-bool settings "reverse") t))
    (is-true (gtk:print-settings-bool settings "reverse"))))

;;;     gtk_print_settings_get_double
;;;     gtk_print_settings_get_double_with_default
;;;     gtk_print_settings_set_double

;;;     gtk_print_settings_get_length
;;;     gtk_print_settings_set_length

(test gtk-print-settings-length
  (let ((settings (gtk:print-settings-new)))
    (is (= 100.0d0 (setf (gtk:print-settings-length settings "paper-width" :mm)
                         100.0d0)))
    (is (= 100.0d0 (gtk:print-settings-length settings "paper-width" :mm)))))

;;;     gtk_print_settings_get_int
;;;     gtk_print_settings_get_int_with_default
;;;     gtk_print_settings_set_int

;;;     gtk_print_settings_get_printer
;;;     gtk_print_settings_set_printer
;;;     gtk_print_settings_get_orientation
;;;     gtk_print_settings_set_orientation

;;;     gtk_print_settings_get_paper_size
;;;     gtk_print_settings_set_paper_size

(test gtk-print-settings-paper-size
  (let ((settings (gtk:print-settings-new)))
    (is (typep (setf (gtk:print-settings-paper-size settings)
                     (gtk:paper-size-new "iso_a4"))
               'gtk:paper-size))
    (is (typep (gtk:print-settings-paper-size settings) 'gtk:paper-size))
    (is (= 210.0d0
           (gtk:paper-size-width (gtk:print-settings-paper-size settings) :mm)))
    (is (= 297.0d0
           (gtk:paper-size-height (gtk:print-settings-paper-size settings) :mm)))))

;;;     gtk_print_settings_get_paper_width
;;;     gtk_print_settings_set_paper_width
;;;     gtk_print_settings_get_paper_height
;;;     gtk_print_settings_set_paper_height
;;;     gtk_print_settings_get_use_color
;;;     gtk_print_settings_set_use_color
;;;     gtk_print_settings_get_collate
;;;     gtk_print_settings_set_collate
;;;     gtk_print_settings_get_reverse
;;;     gtk_print_settings_set_reverse
;;;     gtk_print_settings_get_duplex
;;;     gtk_print_settings_set_duplex
;;;     gtk_print_settings_get_quality
;;;     gtk_print_settings_set_quality
;;;     gtk_print_settings_get_n_copies
;;;     gtk_print_settings_set_n_copies
;;;     gtk_print_settings_get_number_up
;;;     gtk_print_settings_set_number_up
;;;     gtk_print_settings_get_number_up_layout
;;;     gtk_print_settings_set_number_up_layout
;;;     gtk_print_settings_get_resolution
;;;     gtk_print_settings_set_resolution
;;;     gtk_print_settings_set_resolution_xy
;;;     gtk_print_settings_get_resolution_x
;;;     gtk_print_settings_get_resolution_y
;;;     gtk_print_settings_get_printer_lpi
;;;     gtk_print_settings_set_printer_lpi
;;;     gtk_print_settings_get_scale
;;;     gtk_print_settings_set_scale
;;;     gtk_print_settings_get_print_pages
;;;     gtk_print_settings_set_print_pages

;;;     gtk_print_settings_get_page_ranges
;;;     gtk_print_settings_set_page_ranges

(test gtk-print-settings-page-ranges
  (let ((settings (gtk:print-settings-new)))
    (is (equal '((1) (15 20) (25))
               (setf (gtk:print-settings-page-ranges settings)
                     '((1) (15 20) (25)))))
    (is (equal '((1) (15 20) (25))
               (gtk:print-settings-page-ranges settings)))))

;;;     gtk_print_settings_get_page_set
;;;     gtk_print_settings_set_page_set
;;;     gtk_print_settings_get_default_source
;;;     gtk_print_settings_set_default_source
;;;     gtk_print_settings_get_media_type
;;;     gtk_print_settings_set_media_type
;;;     gtk_print_settings_get_dither
;;;     gtk_print_settings_set_dither
;;;     gtk_print_settings_get_finishings
;;;     gtk_print_settings_set_finishings
;;;     gtk_print_settings_get_output_bin
;;;     gtk_print_settings_set_output_bin

;;;     gtk_print_settings_new_from_file
;;;     gtk_print_settings_to_file

(test gtk-print-settings-new-from-file
  (let ((path (glib-sys:sys-path "test/out/print-settings.ini"))
        settings)
    (is (typep (setf settings
                     (gtk:print-settings-new-from-file path))
               'gtk:print-settings))
    (is-true (gtk:print-settings-to-file settings path))))

;;;     gtk_print_settings_new_from_key_file
;;;     gtk_print_settings_to_key_file

(test gtk-print-settings-new-from-key-file
  (let ((path (glib-sys:sys-path "test/out/print-settings.ini"))
        settings)
    (glib:with-g-key-file (keyfile)
      (is-true (glib:key-file-load-from-file keyfile path :none))
      (is (typep (setf settings
                       (gtk:print-settings-new-from-key-file keyfile nil))
                 'gtk:print-settings))
      (is-false (gtk:print-settings-to-key-file settings keyfile nil))
      (is-true (g:key-file-save-to-file keyfile path)))))

;;;     gtk_print_settings_new_from_gvariant
;;;     gtk_print_settings_to_gvariant

(test gtk-print-settings-new-from-gvariant
  (let* ((variant (g:variant-parse "a{sv}"
                                   "{'scale': <'100'>,
                                     'number-up': <'1'>,
                                     'n-copies': <'1'>,
                                     'page-ranges': <'0-11'>,
                                     'page-set': <'all'>,
                                     'printer': <'In Datei drucken'>,
                                     'print-pages': <'ranges'>,
                                     'reverse': <'false'>,
                                     'collate': <'false'>,
                                     'output-file-format': <'pdf'>}"))
         settings)
    (is (typep (setf settings
                     (gtk:print-settings-new-from-gvariant variant))
               'gtk:print-settings))
    (is (g:variant-equal variant
                         (gtk:print-settings-to-gvariant settings)))))

;;;     gtk_print_settings_load_file

(test gtk-print-settings-load-file
  (let ((path (glib-sys:sys-path "test/out/print-settings.ini"))
        (settings (gtk:print-settings-new)))
    (is-true (gtk:print-settings-load-file settings path))))

;;;     gtk_print_settings_load_key_file

(test gtk-print-settings-load-key-file
  (let ((path (glib-sys:sys-path "test/out/print-settings.ini"))
        (settings (gtk:print-settings-new)))
    (glib:with-g-key-file (keyfile)
      (is-true (glib:key-file-load-from-file keyfile path :none))
      (is-true (gtk:print-settings-load-key-file settings keyfile nil)))))

;;; 2024-10-13
