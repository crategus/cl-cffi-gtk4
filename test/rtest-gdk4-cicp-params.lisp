(in-package :gtk-test)

(def-suite gdk-cicp-params :in gdk-suite)
(in-suite gdk-cicp-params)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkCicpRange

(test gdk-cicp-range
  ;; Check type
  (is (g:type-is-enum "GdkCicpRange"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCicpRange")
          (g:gtype (cffi:foreign-funcall "gdk_cicp_range_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:cicp-range
          (glib:symbol-for-gtype "GdkCicpRange")))
  ;; Check names
  (is (equal '("GDK_CICP_RANGE_NARROW" "GDK_CICP_RANGE_FULL")
             (glib-test:list-enum-item-names "GdkCicpRange")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GdkCicpRange")))
  ;; Check nick names
  (is (equal '("narrow" "full")
             (glib-test:list-enum-item-nicks "GdkCicpRange")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkCicpRange" GDK:CICP-RANGE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gdk_cicp_range_get_type")
                       (:NARROW 0)
                       (:FULL 1))
             (gobject:get-gtype-definition "GdkCicpRange"))))

;;;     GdkCicpParams

(test gdk-cicp-params-class
  ;; Check type
  (is (g:type-is-object "GdkCicpParams"))
  ;; Check registered name
  (is (eq 'gdk:cicp-params
          (glib:symbol-for-gtype "GdkCicpParams")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkCicpParams")
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
  (is (equal '("color-primaries" "matrix-coefficients" "range"
               "transfer-function")
             (glib-test:list-properties "GdkCicpParams")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkCicpParams")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkCicpParams" GDK:CICP-PARAMS
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_cicp_params_get_type")
                       ((COLOR-PRIMARIES CICP-PARAMS-COLOR-PRIMARIES
                         "color-primaries" "guint" T T)
                        (MATRIX-COEFFICIENTS CICP-PARAMS-MATRIX-COEFFICIENTS
                         "matrix-coefficients" "guint" T T)
                        (RANGE CICP-PARAMS-RANGE "range" "GdkCicpRange" T T)
                        (TRANSFER-FUNCTION CICP-PARAMS-TRANSFER-FUNCTION
                         "transfer-function" "guint" T T)))
             (gobject:get-gtype-definition "GdkCicpParams"))))

;;; --- Properties -------------------------------------------------------------

;;;     color-primaries
;;;     matrix-coefficients
;;;     range
;;;     transfer-function

;;; --- Functions --------------------------------------------------------------

;;;     gdk_cicp_params_new
;;;     gdk_cicp_params_build_color_state

;;; 2024-11-10
