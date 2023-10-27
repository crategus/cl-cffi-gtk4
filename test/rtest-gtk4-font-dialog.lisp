(in-package :gtk-test)

(def-suite gtk-font-dialog :in gtk-suite)
(in-suite gtk-font-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkFontDialog

(test gtk-font-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkFontDialog"))
  ;; Check the registered name
  (is (eq 'gtk:font-dialog
          (glib:symbol-for-gtype "GtkFontDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkFontDialog")
          (g:gtype (cffi:foreign-funcall "gtk_font_dialog_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkFontDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkFontDialog")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkFontDialog")))
  ;; Check the properties
  (is (equal '("filter" "font-map" "language" "modal" "title")
             (list-properties "GtkFontDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkFontDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkFontDialog" GTK-FONT-DIALOG
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_font_dialog_get_type")
                               ((FILTER GTK-FONT-DIALOG-FILTER "filter"
                                 "GtkFilter" T T)
                                (FONT-MAP GTK-FONT-DIALOG-FONT-MAP "font-map"
                                 "PangoFontMap" T T)
                                (LANGUAGE GTK-FONT-DIALOG-LANGUAGE "language"
                                 "PangoLanguage" T T)
                                (MODAL GTK-FONT-DIALOG-MODAL "modal" "gboolean"
                                 T T)
                                (TITLE GTK-FONT-DIALOG-TITLE "title"
                                 "gchararray" T T)))
             (gobject:get-g-type-definition "GtkFontDialog"))))

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

;;; --- 2023-8-27 --------------------------------------------------------------
