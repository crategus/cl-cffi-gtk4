(in-package :gtk-test)

(def-suite gtk-native :in gtk-windows)
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
             (glib-test:list-interface-prerequisites "GtkNative")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkNative")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkNative")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkNative" GTK:NATIVE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_native_get_type"))
             (gobject:get-gtype-definition "GtkNative"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_native_get_for_surface
;;;     gtk_native_get_surface

;; This tests has 2 references on SURFACE

#-windows
(test gtk-native-surface/for-surface
  (glib-test:with-check-memory (button window (surface 2) :strong 1)
    (setf button (make-instance 'gtk:button))
    (setf window (make-instance 'gtk:window :child button))
    ;; Realize WINDOW to create GDK resources
    (is-false (gtk:widget-realize window))
    ;; Native widget for WINDOW is WINDOW itself
    (is (eq window (gtk:widget-native window)))
    ;; Native widget for BUTTON is WINDOW
    (is (eq window (gtk:widget-native button)))
    ;; Get GDK surface
    (is (typep (setf surface (gtk:native-surface window)) 'gdk:surface))
    ;; Get WINDOW from GDK surface
    (is (eq window (gtk:native-for-surface surface)))
    ;; Remove button from window and destroy window
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))))

;; This tests has 1 references on SURFACE. Why the difference?!

#+windows
(test gtk-native-surface/for-surface
  (glib-test:with-check-memory (button window (surface 1) :strong 1)
    (setf button (make-instance 'gtk:button))
    (setf window (make-instance 'gtk:window :child button))
    ;; Realize WINDOW to create GDK resources
    (is-false (gtk:widget-realize window))
    ;; Native widget for WINDOW is WINDOW itself
    (is (eq window (gtk:widget-native window)))
    ;; Native widget for BUTTON is WINDOW
    (is (eq window (gtk:widget-native button)))
    ;; Get GDK surface
    (is (typep (setf surface (gtk:native-surface window)) 'gdk:surface))
    ;; Get WINDOW from GDK surface
    (is (eq window (gtk:native-for-surface surface)))
    ;; Remove button from window and destroy window
    (is-false (setf (gtk:window-child window) nil))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_get_renderer

(test gtk-native-renderer
  (glib-test:with-check-memory (window renderer)
    (setf window (make-instance 'gtk:window))
    ;; Realize WINDOW to create GDK resources
    (is-false (gtk:widget-realize window))
    ;; Get GSK renderer
    (is (typep (setf renderer (gtk:native-renderer window)) 'gsk:renderer))
    ;; RENDERER has surface eq to surface of WINDOW
    (is-true (gsk:renderer-realized renderer))
    (is (eq (gtk:native-surface window) (gsk:renderer-surface renderer)))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_get_surface_transform

(test gtk-native-surface-transform
  (glib-test:with-check-memory (window)
    (setf window (make-instance 'gtk:window))
    (is (equal '(0d0 0d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:widget-realize window))
    (is (equal '(14d0 12d0)
               (multiple-value-list (gtk:native-surface-transform window))))
    (is-false (gtk:window-destroy window))))

;;;     gtk_native_realize
;;;     gtk_native_unrealize

;; TODO: Does not work in the testsuite. We get a warning from GDK. The surface
;; is not created. But GTK:WIDGET-REALIZE and GTK:WIDGET-UNREALIZE is working.
;;
;;  (glib-test:9391): Gdk-CRITICAL **: gdk_surface_get_frame_clock: assertion
;;  'GDK_IS_SURFACE (surface)' failed
;;
;;  (glib-test:9391): Gtk-CRITICAL **: gtk_native_realize: assertion 'clock !=
;;   NULL' failed

#+nil
(test gtk-native-realize/unrealize
  (let ((window (make-instance 'gtk:window)))
    (is-false (gtk:native-realize window))))

;;; 2025-05-30
