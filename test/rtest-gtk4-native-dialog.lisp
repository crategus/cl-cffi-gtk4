(in-package :gtk-test)

(def-suite gtk-native-dialog :in gtk-windows)
(in-suite gtk-native-dialog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: After loading GTK the symbol for "GtkNativeDialog" is not
  ;; registered. Why?
  (g:type-ensure "GtkNativeDialog")
  (setf (glib:symbol-for-gtype "GtkNativeDialog") 'gtk:native-dialog))

(gobject:define-gobject-subclass "NativeDialog" native-dialog
  (:superclass gtk:native-dialog
   :export t
   :interfaces ())
  nil)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNativeDialog

(test gtk-native-dialog-class
  ;; Check type
  (is (g:type-is-object "GtkNativeDialog"))
  ;; Check registered name
  ;; FIXME: We have no symbol for GtkNativeDialog. Why!?
  (is (eq 'gtk:native-dialog
          (glib:symbol-for-gtype "GtkNativeDialog")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNativeDialog")
          (g:gtype (cffi:foreign-funcall "gtk_native_dialog_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkNativeDialog")))
  ;; Check children
  (is (equal '("GtkFileChooserNative" "NativeDialog")
             (glib-test:list-children "GtkNativeDialog")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GtkNativeDialog")))
  ;; Check properties
  (is (equal '("modal" "title" "transient-for" "visible")
             (glib-test:list-properties "GtkNativeDialog")))
  ;; Check signals
  (is (equal '("response")
             (glib-test:list-signals "GtkNativeDialog")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkNativeDialog" GTK:NATIVE-DIALOG
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gtk_native_dialog_get_type")
                      ((MODAL NATIVE-DIALOG-MODAL "modal" "gboolean" T T)
                       (TITLE NATIVE-DIALOG-TITLE "title" "gchararray" T T)
                       (TRANSIENT-FOR NATIVE-DIALOG-TRANSIENT-FOR
                        "transient-for" "GtkWindow" T T)
                       (VISIBLE NATIVE-DIALOG-VISIBLE
                        "visible" "gboolean" T T)))
             (gobject:get-gtype-definition "GtkNativeDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     modal
;;;     title
;;;     transient-for
;;;     visible

(test gtk-native-dialog-properties
  (let ((dialog (make-instance 'native-dialog
                               :modal t
                               :title "title")))
    (is-true (gtk:native-dialog-modal dialog))
    (is (string= "title" (gtk:native-dialog-title dialog)))
    (is-false (gtk:native-dialog-transient-for dialog))
    (is-false (gtk:native-dialog-visible dialog))))

;;; --- Signals ----------------------------------------------------------------

;;;     response

(test gtk-native-dialog-response-signal
  (let* ((name "response")
         (gtype (g:gtype "GtkNativeDialog"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_dialog_show
;;;     gtk_native_dialog_hide
;;;     gtk_native_dialog_destroy

;;; 2025-3-24
