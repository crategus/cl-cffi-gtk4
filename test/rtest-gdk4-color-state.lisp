(in-package :gtk-test)

(def-suite gdk-color-state :in gdk-suite)
(in-suite gdk-color-state)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkColorState

(test gdk-color-state-boxed
  ;; Check type
  (is (g:type-is-boxed "GdkColorState"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkColorState")
          (g:gtype (cffi:foreign-funcall "gdk_color_state_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:color-state
          (glib:symbol-for-gtype "GdkColorState"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_color_state_get_oklab                           Since 4.18

(test gdk-color-state-oklab
  (is (typep (gdk:color-state-oklab) 'gdk:color-state)))

;;;     gdk_color_state_get_oklch                           Since 4.18

(test gdk-color-state-oklch
  (is (typep (gdk:color-state-oklch) 'gdk:color-state)))

;;;     gdk_color_state_get_rec2100_linear

(test gdk-color-state-rec2100-linear
  (is (typep (gdk:color-state-rec2100-linear) 'gdk:color-state)))

;;;     gdk_color_state_get_rec2100_pq

(test gdk-color-state-rec2100-pq
  (is (typep (gdk:color-state-rec2100-pq) 'gdk:color-state)))

;;;     gdk_color_state_get_srgb

(test gdk-color-state-srgb
  (is (typep (gdk:color-state-srgb) 'gdk:color-state)))

;;;     gdk_color_state_get_srgb_linear

(test gdk-color-state-srgb-linear
  (is (typep (gdk:color-state-srgb-linear) 'gdk:color-state)))

;;;     gdk_color_state_create_cicp_params

(test gdk-color-state-create-cicp-params.1
  (let ((rec2100-linear (gdk:color-state-rec2100-linear))
        (rec2100-pq (gdk:color-state-rec2100-pq))
        (srgb (gdk:color-state-srgb))
        (srgb-linear (gdk:color-state-srgb-linear)))
  (is (typep (gdk:color-state-create-cicp-params rec2100-linear) 'gdk:cicp-params))
  (is (typep (gdk:color-state-create-cicp-params rec2100-pq) 'gdk:cicp-params))
  (is (typep (gdk:color-state-create-cicp-params srgb) 'gdk:cicp-params))
  (is (typep (gdk:color-state-create-cicp-params srgb-linear) 'gdk:cicp-params))))

(test gdk-color-state-create-cicp-params.2
  (glib-test:with-check-memory (params)
    (let ((colorstate (gdk:color-state-srgb)))
      (setf params (gdk:color-state-create-cicp-params colorstate))
      (is (= 1 (gdk:cicp-params-color-primaries params)))
      (is (= 0 (gdk:cicp-params-matrix-coefficients params)))
      (is (eq :full (gdk:cicp-params-range params)))
      (is (= 13 (gdk:cicp-params-transfer-function params))))))

;;;     gdk_color_state_equal

(test gdk-color-state-equal
  (is (gdk:color-state-equal (gdk:color-state-srgb) (gdk:color-state-srgb)))
  (is-false (gdk:color-state-equal (gdk:color-state-srgb)
                                   (gdk:color-state-srgb-linear))))

;;;     gdk_color_state_equivalent

(test gdk-color-state-equivalent
  (is-true  (gdk:color-state-equivalent (gdk:color-state-srgb)
                                        (gdk:color-state-srgb)))
  (is-false (gdk:color-state-equivalent (gdk:color-state-srgb)
                                        (gdk:color-state-srgb-linear))))

;;; 2026-08-01
