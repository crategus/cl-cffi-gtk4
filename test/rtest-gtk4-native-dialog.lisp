(in-package :gtk-test)

(def-suite gtk-native-dialog :in gtk-suite)
(in-suite gtk-native-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNativeDialog

(test gtk-native-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkNativeDialog"))
  ;; Check registered name
  ;; FIXME: We have no symbol for GtkNativeDialog. Why!?
  #+nil
  (is (eq 'gtk:native-dialog
          (glib:symbol-for-gtype "GtkNativeDialog")))
  (is-false (glib:symbol-for-gtype "GtkNativeDialog"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNativeDialog")
          (g:gtype (cffi:foreign-funcall "gtk_native_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNativeDialog")))
  ;; Check children
  (is (equal '("GtkFileChooserNative")
             (list-children "GtkNativeDialog")))
  ;; Check interfaces
  (is (equal '()
             (list-interfaces "GtkNativeDialog")))
  ;; Check properties
  (is (equal '("modal" "title" "transient-for" "visible")
             (list-properties "GtkNativeDialog")))
  ;; Check signals
  (is (equal '("response")
             (list-signals "GtkNativeDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkNativeDialog" GTK-NATIVE-DIALOG
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_native_dialog_get_type")
                       ((MODAL GTK-NATIVE-DIALOG-MODAL "modal" "gboolean" T T)
                        (TITLE GTK-NATIVE-DIALOG-TITLE "title" "gchararray" T
                         T)
                        (TRANSIENT-FOR GTK-NATIVE-DIALOG-TRANSIENT-FOR
                         "transient-for" "GtkWindow" T T)
                        (VISIBLE GTK-NATIVE-DIALOG-VISIBLE "visible" "gboolean"
                         T T)))
             (gobject:get-g-type-definition "GtkNativeDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     modal
;;;     title
;;;     transient-for
;;;     visible

;;; --- Signals ----------------------------------------------------------------

;;;     response

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy

;;; 2024-4-11
