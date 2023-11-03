(in-package :gtk-test)

(def-suite gtk-native :in gtk-suite)
(in-suite gtk-native)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNative

(test gtk-native-interface
  ;; Type check
  (is (g:type-is-interface "GtkNative"))
  ;; Check the registered name
  (is (eq 'gtk:native
          (glib:symbol-for-gtype "GtkNative")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkNative")
          (g:gtype (cffi:foreign-funcall "gtk_native_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GtkWidget")
             (list-interface-prerequisites "GtkNative")))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkNative")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkNative")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkNative"
                                  GTK-NATIVE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_native_get_type"))
             (gobject:get-g-type-definition "GtkNative"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_get_for_surface
;;;     gtk_native_get_surface

;; TODO: Look more carefully at the warning:
;; Gsk-Message: 19:23:58.086: Failed to realize renderer of type 'GskGLRenderer'
;; for surface 'GdkWaylandToplevel': Es wird versucht EGL zu verwenden, aber X11
;; GLX ist bereits in Verwendung

#+nil
(test gtk-native-surface/for-surface
  (let ((window (make-instance 'gtk:window))
        (surface nil))
    (is-false (gtk:native-surface window))
    (is-false (gtk:window-present window))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_get_renderer

#+nil
(test gtk-native-renderer
  (let ((window (make-instance 'gtk:window))
        (renderer nil))
    (is-false (gtk:native-renderer window))
    (is-false (gtk:window-present window))
    (is (typep (setf renderer (gtk:native-renderer window)) 'gsk:renderer))
    (is (typep (gsk:renderer-surface renderer) 'gdk:surface))
    (is (eq (gtk:native-surface window)
            (gsk:renderer-surface renderer)))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_get_surface_transform

#+nil
(test gtk-native-surface-transform
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(0d0 0d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:window-present window))
    (is (equal '(14d0 12d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_realize
;;;     gtk_native_unrealize

;; Does not work in the testsuite. We get a warning from GDK. The surface is
;; not created.

#+nil
(test gtk-native-realize/unrealize
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:native-surface window))
;    (is-false (gtk:native-realize window))
))

;;; --- 2023-11-3 --------------------------------------------------------------
