(in-package :gtk-test)

(def-suite gtk-color-dialog :in gtk-suite)
(in-suite gtk-color-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkColorDialog

(test gtk-color-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkColorDialog"))
  ;; Check registered name
  (is (eq 'gtk:color-dialog
          (glib:symbol-for-gtype "GtkColorDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkColorDialog")
          (g:gtype (cffi:foreign-funcall "gtk_color_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkColorDialog")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkColorDialog")))
  ;; Check interfaces
  (is (equal '()
             (gtk-test:list-interfaces "GtkColorDialog")))
  ;; Check properties
  (is (equal '("modal" "title" "with-alpha")
             (gtk-test:list-properties "GtkColorDialog")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GtkColorDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkColorDialog" GTK-COLOR-DIALOG
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gtk_color_dialog_get_type")
                               ((MODAL GTK-COLOR-DIALOG-MODAL "modal"
                                 "gboolean" T T)
                                (TITLE GTK-COLOR-DIALOG-TITLE "title"
                                 "gchararray" T T)
                                (WITH-ALPHA GTK-COLOR-DIALOG-WITH-ALPHA
                                 "with-alpha" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkColorDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     modal
;;;     title
;;;     with-alpha

(test gtk-color-dialog-properties
  (let ((dialog (make-instance 'gtk:color-dialog)))
    (is-true (gtk:color-dialog-modal dialog))
    (is-false (gtk:color-dialog-title dialog))
    (is-true (gtk:color-dialog-with-alpha dialog))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_color_dialog_new

(test gtk-color-dialog-new
  (is (typep (gtk:color-dialog-new) 'gtk:color-dialog)))

;;;     gtk_color_dialog_choose_rgba
;;;     gtk_color_dialog_choose_rgba_finish

;;; 2024-7-4
