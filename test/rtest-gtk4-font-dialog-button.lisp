(in-package :gtk-test)

(def-suite gtk-font-dialog-button :in gtk-selector-widgets)
(in-suite gtk-font-dialog-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontLevel

(test gtk-font-level
  ;; Check type
  (is (g:type-is-enum "GtkFontLevel"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontLevel")
          (g:gtype (cffi:foreign-funcall "gtk_font_level_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:font-level
          (glib:symbol-for-gtype "GtkFontLevel")))
  ;; Check names
  (is (equal '("GTK_FONT_LEVEL_FAMILY" "GTK_FONT_LEVEL_FACE"
               "GTK_FONT_LEVEL_FONT" "GTK_FONT_LEVEL_FEATURES")
             (glib-test:list-enum-item-names "GtkFontLevel")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GtkFontLevel")))
  ;; Check nick names
  (is (equal '("family" "face" "font" "features")
             (glib-test:list-enum-item-nicks "GtkFontLevel")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkFontLevel" GTK:FONT-LEVEL
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gtk_font_level_get_type")
                                    (:FAMILY 0)
                                    (:FACE 1)
                                    (:FONT 2)
                                    (:FEATURES 3))
             (gobject:get-gtype-definition "GtkFontLevel"))))

;;;     GtkFontDialogButton

(test gtk-font-dialog-button-class
  ;; Check type
  (is (g:type-is-object "GtkFontDialogButton"))
  ;; Check registered name
  (is (eq 'gtk:font-dialog-button
          (glib:symbol-for-gtype "GtkFontDialogButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontDialogButton")
          (g:gtype (cffi:foreign-funcall "gtk_font_dialog_button_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkFontDialogButton")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFontDialogButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkFontDialogButton")))
  ;; Check properties
  (is (equal '("dialog" "font-desc" "font-features" "language" "level"
               "use-font" "use-size")
             (glib-test:list-properties "GtkFontDialogButton")))
  ;; Check signals
  (is (equal '("activate")
             (glib-test:list-signals "GtkFontDialogButton")))
  ;; Check CSS name
  (is (string= "fontbutton"
               (gtk:widget-class-css-name "GtkFontDialogButton")))
  ;; Check accessible role
  (is (eq :group (gtk:widget-class-accessible-role "GtkFontDialogButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontDialogButton"
                                      GTK:FONT-DIALOG-BUTTON
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_font_dialog_button_get_type")
                       ((DIALOG FONT-DIALOG-BUTTON-DIALOG
                         "dialog" "GtkFontDialog" T T)
                        (FONT-DESC FONT-DIALOG-BUTTON-FONT-DESC
                         "font-desc" "PangoFontDescription" T T)
                        (FONT-FEATURES FONT-DIALOG-BUTTON-FONT-FEATURES
                         "font-features" "gchararray" T T)
                        (LANGUAGE FONT-DIALOG-BUTTON-LANGUAGE
                         "language" "PangoLanguage" T T)
                        (LEVEL FONT-DIALOG-BUTTON-LEVEL
                         "level" "GtkFontLevel" T T)
                        (USE-FONT FONT-DIALOG-BUTTON-USE-FONT
                         "use-font" "gboolean" T T)
                        (USE-SIZE FONT-DIALOG-BUTTON-USE-SIZE
                         "use-size" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkFontDialogButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     dialog
;;;     font-desc
;;;     font-features
;;;     language
;;;     level
;;;     use-font
;;;     use-size

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-font-dialog-button-activate-signal
  (let ((query (g:signal-query (g:signal-lookup "activate"
                                                "GtkFontDialogButton"))))
    (is (string= "activate" (g:signal-query-signal-name query)))
    (is (string= "GtkFontDialogButton"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_dialog_button_new

;;; 2024-9-20
