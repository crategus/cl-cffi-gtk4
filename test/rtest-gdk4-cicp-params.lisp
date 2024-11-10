(in-package :gtk-test)

(def-suite gdk-cicp-params :in gdk-suite)
(in-suite gdk-cicp-params)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkCicpParams

(test gdk-cicp-params-class
  ;; Check type
  (is (g:type-is-object "GdkCicpParams"))
  ;; Check registered name
  (is (eq 'gdk:cicp-params
          (glib:symbol-for-gtype "GdkCicpParams")))
  ;; Check type initializer
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "gdk_cicp_params_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkCicpParams")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkCicpParams")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkCicpParams")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GdkCicpParams")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkCicpParams")))
  ;; Check class definition
  (is (equal '()
             (gobject:get-gtype-definition "GdkCicpParams"))))

;;; --- Properties -------------------------------------------------------------

;;;     color-primaries
;;;     matrix-coefficients
;;;     range
;;;     transfer-function

;;; Functions

;;;     gdk_cicp_params_new
;;;     gdk_cicp_params_build_color_state


;;; 2024-11-6
