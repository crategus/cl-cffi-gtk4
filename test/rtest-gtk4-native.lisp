(in-package :gtk-test)

(def-suite gtk-native :in gtk-suite)
(in-suite gtk-native)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkNative

(test gtk-native-interface
  ;; Check type
  (is (g:type-is-interface "GtkNative"))
  ;; Check registered name
  (is (eq 'gtk:native
          (glib:symbol-for-gtype "GtkNative")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkNative")
          (g:gtype (cffi:foreign-funcall "gtk_native_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GtkWidget")
             (gtk-test:list-interface-prerequisites "GtkNative")))
  ;; Check interface properties
  (is (equal '()
             (gtk-test:list-interface-properties "GtkNative")))
  ;; Check interface signals
  (is (equal '()
             (gtk-test:list-signals "GtkNative")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkNative"
                                  GTK-NATIVE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_native_get_type"))
             (gobject:get-g-type-definition "GtkNative"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_get_for_surface
;;;     gtk_native_get_surface

;; TODO: We get a warning, but the test works
;; Gsk-Message: Failed to realize renderer of type 'GskGLRenderer' for surface
;; 'GdkWaylandToplevel': Es wird versucht EGL zu verwenden, aber X11 GLX ist
;; bereits in Verwendung

#+nil
(test gtk-native-surface/for-surface
  (let ((window (make-instance 'gtk:window))
        (surface nil))
    ;; Realize WINDOW to create GDK resources
    (is-false (gtk:widget-realize window))
    ;; Native widget for WINDOW is WINDOW itself
    (is (eq window (gtk:widget-native window)))
    ;; Get GDK surface
    (is (typep (setf surface (gtk:native-surface window)) 'gdk:surface))
    ;; Get WINDOW from GDK surface
    (is (eq window (gtk:native-for-surface surface)))
    ;; Unrealize WINDOW
    (is-false (gtk:widget-unrealize window))))

;;;     gtk_native_get_renderer

#+nil
(test gtk-native-renderer
  (let ((window (make-instance 'gtk:window))
        (renderer nil))
    ;; Realize WINDOW to create GDK resources
    (is-false (gtk:widget-realize window))
    ;; Get GSK renderer
    (is (typep (setf renderer (gtk:native-renderer window)) 'gsk:renderer))
    ;; RENDERER has surface eq to surface of WINDOW
    (is-true (gsk:renderer-realized renderer))
    (is (eq (gtk:native-surface window) (gsk:renderer-surface renderer)))))

;;;     gtk_native_get_surface_transform

#+nil
(test gtk-native-surface-transform
  (let ((window (make-instance 'gtk:window)))
    (is (equal '(0d0 0d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:widget-realize window))
    (is (equal '(14d0 12d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:widget-unrealize window))))

;;;     gtk_native_realize
;;;     gtk_native_unrealize

;; TODO: Does not work in the testsuite. We get a warning from GDK. The surface
;; is not created. But GTK:WIDGET-REALIZE and GTK:WIDGET-UNREALIZE is working.
;;
;;  (gtk-test:9391): Gdk-CRITICAL **: gdk_surface_get_frame_clock: assertion
;;  'GDK_IS_SURFACE (surface)' failed
;;
;;  (gtk-test:9391): Gtk-CRITICAL **: gtk_native_realize: assertion 'clock !=
;;   NULL' failed

#+nil
(test gtk-native-realize/unrealize
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:native-realize window))
))

;;; 2024-7-3
