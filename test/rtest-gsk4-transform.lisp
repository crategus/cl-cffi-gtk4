(in-package :gtk-test)

(def-suite gsk-transform :in gsk-suite)
(in-suite gsk-transform)

;;; --- Types and Values -------------------------------------------------------

;;;     GskTransformCategory

(test gsk-transform-category-enumeration
  ;; Check the type
  (is (g:type-is-enum "GskTransformCategory"))
  ;; Check the type initializer
  (is (eq (g:gtype "GskTransformCategory")
          (g:gtype (cffi:foreign-funcall "gsk_transform_category_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gsk:transform-category
          (glib:symbol-for-gtype "GskTransformCategory")))
  ;; Check the names
  (is (equal '("GSK_TRANSFORM_CATEGORY_UNKNOWN" "GSK_TRANSFORM_CATEGORY_ANY"
               "GSK_TRANSFORM_CATEGORY_3D" "GSK_TRANSFORM_CATEGORY_2D"
               "GSK_TRANSFORM_CATEGORY_2D_AFFINE"
               "GSK_TRANSFORM_CATEGORY_2D_TRANSLATE"
               "GSK_TRANSFORM_CATEGORY_IDENTITY")
             (list-enum-item-name "GskTransformCategory")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GskTransformCategory")))
  ;; Check the nick names
  (is (equal '("unknown" "any" "3d" "2d" "2d-affine" "2d-translate" "identity")
             (list-enum-item-nick "GskTransformCategory")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GskTransformCategory"
                                     GSK-TRANSFORM-CATEGORY
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gsk_transform_category_get_type")
                                     (:UNKNOWN 0)
                                     (:ANY 1)
                                     (:|3D| 2)
                                     (:|2D| 3)
                                     (:2D-AFFINE 4)
                                     (:2D-TRANSLATE 5)
                                     (:IDENTITY 6))
             (gobject:get-g-type-definition "GskTransformCategory"))))

;;;     GskTransform

#+nil
(test gsk-transform-class
  ;; Type check
  (is (g:type-is-a (gtype "GskTransform") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "GskTransform")
          (g:gtype (cffi:foreign-funcall "gsk_transform_get_type" :size)))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_transform_new

(test gsk-transform-new
  (is (typep (gsk:transform-new) 'gsk:transform)))

;;;     gsk_transform_parse

#+nil
(test gsk-transform-parse

  (is-false (gsk:transform-parse ""))
  (is (typep (gsk:transform-parse "translate(10,20)") 'gsk:transform))
  (is (typep (gsk:transform-parse "rotate(10)") 'gsk:transform))

)


;;;     gsk_transform_equal

#+nil
(test gsk-transform-equal
  (let ((transform (gsk:transform-parse "translate(10,20)")))
    (is-true (gsk:transform-equal transform transform))
    (is-true (gsk:transform-equal (gsk:transform-new)
                                  (gsk:transform-new)))
    ;; FIXME: Why is this FALSE?
    (is-false (gsk:transform-equal (gsk:transform-parse "translate(10,20)")
                                   (gsk:transform-parse "translate(10,20)")))

))

;;;     gsk_transform_get_category

#+nil
(test gsk-transform-category

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

#+nil
(test gsk-transform-ref
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

;;; --- 2023-9-22 --------------------------------------------------------------
