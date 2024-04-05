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

(test gsk-transform-boxed
  ;; Check type
  (is (g:type-is-boxed "GskTransform"))
  ;; Check type initializer
  (is (eq (g:gtype "GskTransform")
          (g:gtype (cffi:foreign-funcall "gsk_transform_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:transform
          (glib:symbol-for-gtype "GskTransform"))))

;;; --- Functions --------------------------------------------------------------

;;;     gsk_transform_new

(test gsk-transform-new
  (is (typep (gsk:transform-new) 'gsk:transform))
  (is (string= "none" (gsk:transform-to-string (gsk:transform-new)))))

;;;     gsk_transform_ref
;;;     gsk_transform_unref

;;;     gsk_transform_get_category

(test gsk-transform-category.1
  (graphene:with-point (point 10 20)
    (let* ((transform1 (gsk:transform-new))
           (transform2 (gsk:transform-translate transform1 point))
           (transform3 (gsk:transform-rotate transform1 10)))
      (is (eq :identity (gsk:transform-category transform1)))
      (is (eq :2d-translate (gsk:transform-category transform2)))
      (is (eq :2d (gsk:transform-category transform3)))
)))

(test gsk-transform-category.2
  (graphene:with-point (point 10 20)
    (let* ((transform (gsk:transform-new)))
      (is (eq :identity (gsk:transform-category transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (eq :2d-translate (gsk:transform-category transform)))
      (setf transform (gsk:transform-rotate transform 10))
      (is (eq :2d (gsk:transform-category transform)))
)))

;;;     gsk_transform_print
;;;     gsk_transform_to_string

(test gsk-transform-to-string
  (graphene:with-point (point 10 20)
    (let ((transform (gsk:transform-new)))
      (is (string= "none" (gsk:transform-to-string transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (string= "translate(10, 20)" (gsk:transform-to-string transform)))
      (setf transform (gsk:transform-rotate transform 10))
      (is (string= "translate(10, 20) rotate(10)"
                   (gsk:transform-to-string transform))))))

;;;     gsk_transform_parse

;; FIXME: The implementation of gsk:transform-parse does not work!?

#+nil
(test gsk-transform-parse

  (trace cffi:translate-to-foreign)
  (trace cffi:translate-from-foreign)
  (trace glib::make-boxed-type)
  (trace gsk:transform-parse)
  (trace gsk::%transform-parse)
  (trace gsk:transform-to-string)

  (let ((transform nil))
    (is (typep (setf transform (gsk:transform-parse "none")) 'gsk:transform))
    (is-false transform)
    ;; This works
    (is-false (gsk:transform-category transform))
    ;; This crashes with: Unhandled memory fault at #x38..
    (is-false (gsk:transform-to-string transform))
  )

  (untrace cffi:translate-to-foreign)
  (untrace cffi:translate-from-foreign)
  (untrace glib::make-boxed-info)
  (untrace gsk:transform-parse)
  (untrace gsk::%transform-parse)
  (untrace gsk:transform-to-string)

)

;;;     gsk_transform_to_matrix

(test gsk-transform-to-matrix
  (graphene:with-point (point 3 4)
    (graphene:with-matrix (matrix)
      (let ((transform (gsk:transform-new)))
        (is (cffi:pointer-eq matrix
                             (gsk:transform-to-matrix transform matrix)))
        (is (equal '(1.0 0.0 0.0 0.0
                     0.0 1.0 0.0 0.0
                     0.0 0.0 1.0 0.0
                     0.0 0.0 0.0 1.0)
                   (graphene:matrix-to-float matrix)))
        (setf transform (gsk:transform-translate transform point))
        (is (cffi:pointer-eq matrix
                             (gsk:transform-to-matrix transform matrix)))
        (is (equal '(1.0 0.0 0.0 0.0
                     0.0 1.0 0.0 0.0
                     0.0 0.0 1.0 0.0
                     3.0 4.0 0.0 1.0)
                   (graphene:matrix-to-float matrix)))))))

;;;     gsk_transform_to_2d

(test gsk-transform-to-2d
  (graphene:with-point (point 3 4)
    (let ((transform (gsk:transform-new)))
      (is (equal '(1.0 0.0 0.0 1.0 0.0 0.0)
                 (gsk:transform-to-2d transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (equal '(1.0 0.0 0.0 1.0 3.0 4.0)
                 (gsk:transform-to-2d transform))))))

;;;     gsk_transform_to_2d_components                     Since 4.6

(test gsk-transform-to-2-components
  (graphene:with-point (point 5 6)
    (let ((transform (gsk:transform-new)))
      (is (equal '(0.0 0.0 1.0 1.0 0.0 0.0 0.0)
                 (gsk:transform-to-2d-components transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (equal '(0.0 0.0 1.0 1.0 0.0 5.0 6.0)
                 (gsk:transform-to-2d-components transform)))
      (setf transform (gsk:transform-rotate transform 90))
      (is (equal '(0.0 0.0 1.0 1.0 90.0 5.0 6.0)
                 (gsk:transform-to-2d-components transform))))))

;;;     gsk_transform_to_affine

(test gsk-transform-to-affine
  (graphene:with-point (point 5 6)
    (let ((transform (gsk:transform-new)))
      (is (equal '(1.0 1.0 0.0 0.0)
                 (gsk:transform-to-affine transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (equal '(1.0 1.0 5.0 6.0)
                 (gsk:transform-to-affine transform))))))

;;;     gsk_transform_to_translate

(test gsk-transform-to-translate
  (graphene:with-point (point 5 6)
    (let ((transform (gsk:transform-new)))
      (is (equal '(0.0 0.0)
                 (gsk:transform-to-translate transform)))
      (setf transform (gsk:transform-translate transform point))
      (is (equal '(5.0 6.0)
                 (gsk:transform-to-translate transform))))))

;;;     gsk_transform_transform

(test gsk-transform-transform
  (graphene:with-point (point 2 3)
    (let ((transform (gsk:transform-new))
          (other (gsk:transform-new))
          (result nil))
    (setf other (gsk:transform-translate other point))
    (setf result (gsk:transform-transform transform other))
    (is (equal '(2.0 3.0)
               (gsk:transform-to-translate result))))))

;;;     gsk_transform_invert

(test gsk-transform-invert
  (graphene:with-point (point 2 3)
    (let ((transform (gsk:transform-new))
          (other (gsk:transform-new))
          (result nil))
    (setf other (gsk:transform-translate other point))
    (setf result (gsk:transform-transform transform other))
    (is (equal '(2.0 3.0)
               (gsk:transform-to-translate result)))
    (is (equal '(-2.0 -3.0)
               (gsk:transform-to-translate (gsk:transform-invert result)))))))

;;;     gsk_transform_matrix

(test gsk-transform-matrix
  (graphene:with-matrix (matrix)
    (let ((transform (gsk:transform-new)))
      (graphene:matrix-init-scale matrix 1 2 3)
      (setf transform (gsk:transform-matrix transform matrix))
      (is (equal '(1.0 0.0 0.0 0.0
                   0.0 2.0 0.0 0.0
                   0.0 0.0 3.0 0.0
                   0.0 0.0 0.0 1.0)
                 (graphene:matrix-to-float
                     (gsk:transform-to-matrix transform matrix)))))))

;;;     gsk_transform_translate
;;;     gsk_transform_translate_3d

;;;     gsk_transform_rotate

(test gsk-transform-rotate
  (let ((transform (gsk:transform-new)))
    (is (typep (setf transform (gsk:transform-rotate transform 10))
               'gsk:transform))
    (is (string= "rotate(10)" (gsk:transform-to-string transform)))
    (is (typep (setf transform (gsk:transform-rotate transform 10))
               'gsk:transform))
    (is (string= "rotate(20)" (gsk:transform-to-string transform)))
    (is (typep (setf transform (gsk:transform-rotate transform 0.5))
               'gsk:transform))
    (is (string= "rotate(20.5)" (gsk:transform-to-string transform)))
    (is (typep (setf transform (gsk:transform-rotate transform -1/2))
               'gsk:transform))
    (is (string= "rotate(20)" (gsk:transform-to-string transform)))))

;;;     gsk_transform_rotate_3d

(test gsk-transform-rotate-3d.z
  (graphene:with-vec3 (vec 0 0 1)
    (let ((transform (gsk:transform-new)))
      (is (string= "none" (gsk:transform-to-string transform)))
      (is (string= "rotate(10)"
                   (gsk:transform-to-string
                     (gsk:transform-rotate-3d transform 10 vec))))
      (is (string= "none" (gsk:transform-to-string transform))))))

(test gsk-transform-rotate-3d.x
  (graphene:with-vec3 (vec 1 0 0)
    (let ((transform (gsk:transform-new)))
      (is (string= "rotate3d(1, 0, 0, 10)"
                   (gsk:transform-to-string
                     (gsk:transform-rotate-3d transform 10 vec)))))))

(test gsk-transform-rotate-3d.y
  (graphene:with-vec3 (vec 0 1 0)
    (let ((transform (gsk:transform-new)))
      (is (string= "rotate3d(0, 1, 0, 10)"
                   (gsk:transform-to-string
                     (gsk:transform-rotate-3d transform 10 vec)))))))

;;;     gsk_transform_scale

(test gsk-transform-scale
  (let ((transform (gsk:transform-new)))
    (is (string= "scale(2.5, 0.4)"
                 (gsk:transform-to-string
                   (setf transform (gsk:transform-scale transform 5/2 2/5)))))
    (is (string= "none"
                 (gsk:transform-to-string
                   (setf transform (gsk:transform-scale transform 2/5 5/2)))))))

;;;     gsk_transform_scale_3d

(test gsk-transform-scale-3d
  (let ((transform (gsk:transform-new)))
    (is (string= "scale3d(2.5, 0.4, 0.5)"
                 (gsk:transform-to-string
                   (setf transform
                         (gsk:transform-scale-3d transform 5/2 2/5 1/2)))))
    (is (string= "none"
                 (gsk:transform-to-string
                   (setf transform
                         (gsk:transform-scale-3d transform 2/5 5/2 2)))))))

;;;     gsk_transform_skew                                 Since 4.6

;;;     gsk_transform_perspective

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

;;;     gsk_transform_transform_bounds
;;;     gsk_transform_transform_point

;;; --- 2023-10-31 -------------------------------------------------------------
