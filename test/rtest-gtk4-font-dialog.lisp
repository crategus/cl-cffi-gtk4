(in-package :gtk-test)

(def-suite gtk-font-dialog :in gtk-selector-widgets)
(in-suite gtk-font-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontDialog

(test gtk-font-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkFontDialog"))
  ;; Check registered name
  (is (eq 'gtk:font-dialog
          (glib:symbol-for-gtype "GtkFontDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkFontDialog")
          (g:gtype (cffi:foreign-funcall "gtk_font_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFontDialog")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkFontDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkFontDialog")))
  ;; Check properties
  (is (equal '("filter" "font-map" "language" "modal" "title")
             (glib-test:list-properties "GtkFontDialog")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkFontDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkFontDialog" GTK:FONT-DIALOG
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_font_dialog_get_type")
                       ((FILTER FONT-DIALOG-FILTER "filter" "GtkFilter" T T)
                        (FONT-MAP FONT-DIALOG-FONT-MAP
                         "font-map" "PangoFontMap" T T)
                        (LANGUAGE FONT-DIALOG-LANGUAGE
                         "language" "PangoLanguage" T T)
                        (MODAL FONT-DIALOG-MODAL "modal" "gboolean" T T)
                        (TITLE FONT-DIALOG-TITLE "title" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkFontDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     filter
;;;     font-map
;;;     language
;;;     modal
;;;     title

;;; --- Functions --------------------------------------------------------------

;;;     gtk_font_dialog_new

;;;     gtk_font_dialog_choose_face
;;;     gtk_font_dialog_choose_face_finish
;;;     gtk_font_dialog_choose_family
;;;     gtk_font_dialog_choose_family_finish
;;;     gtk_font_dialog_choose_font
;;;     gtk_font_dialog_choose_font_and_features
;;;     gtk_font_dialog_choose_font_and_features_finish
;;;     gtk_font_dialog_choose_font_finish

;;; 2024-9-20
