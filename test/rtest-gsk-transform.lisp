(in-package :gtk-test)

(def-suite gsk-transform :in gsk-suite)
(in-suite gsk-transform)

;;; --- Types and Values -------------------------------------------------------

;;;     GskTransform

(test gsk-transform-class
  ;; Type check
  (is (g:type-is-a (gtype "GskTransform") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GskTransform")
          (g:gtype (cffi:foreign-funcall "gsk_transform_get_type" :size)))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_transform_new

(test gsk-transform-new
  (is (typep (gsk-transform-new) 'gsk-transform)))

;;;     gsk_transform_parse

(test gsk-transform-parse

  (is-false (gsk-transform-parse ""))
  (is (typep (gsk-transform-parse "translate(10,20)") 'gsk-transform))
  (is (typep (gsk-transform-parse "rotate(10)") 'gsk-transform))

)


;;;     gsk_transform_equal

(test gsk-transform-equal
  (let ((transform (gsk-transform-parse "translate(10,20)")))
    (is-true (gsk-transform-equal transform transform))
    (is-true (gsk-transform-equal (gsk-transform-new)
                                  (gsk-transform-new)))
    ;; FIXME: Why is this FALSE?
    (is-false (gsk-transform-equal (gsk-transform-parse "translate(10,20)")
                                   (gsk-transform-parse "translate(10,20)")))

))

;;;     gsk_transform_get_category


(test transform-category

  (is (eq :identity (gsk:transform-category (gsk:transform-new))))

  (is (eq :identity
          (gsk:transform-category (gsk:transform-parse "translate(10,20)"))))

  (is (eq :identity
          (gsk:transform-category (gsk:transform-parse "rotate(10deg)"))))
)


;;;     gsk_transform_invert
;;;     gsk_transform_matrix
;;;     gsk_transform_perspective
;;;     gsk_transform_print

;;;     gsk_transform_ref

(test transform-ref
  (is (typep (gsk:transform-ref (gsk:transform-new)) 'gsk:transform)))

;;;     gsk_transform_rotate
;;;     gsk_transform_rotate_3d
;;;     gsk_transform_scale
;;;     gsk_transform_scale_3d
;;;     gsk_transform_skew
;;;     gsk_transform_to_2d
;;;     gsk_transform_to_2d_components
;;;     gsk_transform_to_affine
;;;     gsk_transform_to_matrix

;;;     gsk_transform_to_string

;;;     gsk_transform_to_translate
;;;     gsk_transform_transform
;;;     gsk_transform_transform_bounds
;;;     gsk_transform_transform_point
;;;     gsk_transform_translate
;;;     gsk_transform_translate_3d
;;;     gsk_transform_unref

;;; --- 2023-3-18 --------------------------------------------------------------
