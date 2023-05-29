(in-package :gtk-test)

(def-suite gtk-native :in gtk-suite)
(in-suite gtk-native)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNative

(test native-interface
  ;; Type check
  (is (g:type-is-interface "GtkNative"))
  ;; Check the registered name
  (is (eq 'gtk:native
          (glib:symbol-for-gtype "GtkNative")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNative")
          (g:gtype (cffi:foreign-funcall "gtk_native_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkNative")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkNative"
                                  GTK-NATIVE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_native_get_type"))
             (gobject:get-g-type-definition "GtkNative"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_get_for_surface

;;;     gtk_native_get_surface

(test native-surface
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:native-surface window))))

;;;     gtk_native_get_renderer

(test native-renderer
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:native-renderer window))))

;;;     gtk_native_get_surface_transform

(test native-surface-transform
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(0d0 0d0)
               (multiple-value-list (gtk:native-surface-transform window))))))

;;;     gtk_native_realize
;;;     gtk_native_unrealize

;;; --- 2023-5-29 --------------------------------------------------------------
