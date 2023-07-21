(in-package :gtk-test)

(def-suite gsk-enumerations :in gsk-suite)
(in-suite gsk-enumerations)

;;; --- Types and Values -------------------------------------------------------

;;;     GskRenderNodeType

(test render-node-type
  ;; Check the type
  (is (g:type-is-enum "GskRenderNodeType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GskRenderNodeType")
          (g:gtype (cffi:foreign-funcall "gsk_render_node_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gsk:render-node-type
          (glib:symbol-for-gype "GskRenderNodeType")))
  ;; Check the names
  (is (equal '("GSK_NOT_A_RENDER_NODE" "GSK_CONTAINER_NODE" "GSK_CAIRO_NODE"
               "GSK_COLOR_NODE" "GSK_LINEAR_GRADIENT_NODE"
               "GSK_REPEATING_LINEAR_GRADIENT_NODE" "GSK_RADIAL_GRADIENT_NODE"
               "GSK_REPEATING_RADIAL_GRADIENT_NODE" "GSK_CONIC_GRADIENT_NODE"
               "GSK_BORDER_NODE" "GSK_TEXTURE_NODE" "GSK_INSET_SHADOW_NODE"
               "GSK_OUTSET_SHADOW_NODE" "GSK_TRANSFORM_NODE" "GSK_OPACITY_NODE"
               "GSK_COLOR_MATRIX_NODE" "GSK_REPEAT_NODE" "GSK_CLIP_NODE"
               "GSK_ROUNDED_CLIP_NODE" "GSK_SHADOW_NODE" "GSK_BLEND_NODE"
               "GSK_CROSS_FADE_NODE" "GSK_TEXT_NODE" "GSK_BLUR_NODE"
               "GSK_DEBUG_NODE" "GSK_GL_SHADER_NODE")
             (listenum-item-name "GskRenderNodeType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
               24 25)
             (list-enum-item-value "GskRenderNodeType")))
  ;; Check the nick names
  (is (equal '("not-a-render-node" "container-node" "cairo-node" "color-node"
               "linear-gradient-node" "repeating-linear-gradient-node"
               "radial-gradient-node" "repeating-radial-gradient-node"
               "conic-gradient-node" "border-node" "texture-node"
               "inset-shadow-node" "outset-shadow-node" "transform-node"
               "opacity-node" "color-matrix-node" "repeat-node" "clip-node"
               "rounded-clip-node" "shadow-node" "blend-node" "cross-fade-node"
               "text-node" "blur-node" "debug-node" "gl-shader-node")
             (list-enum-item-nick "GskRenderNodeType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GskRenderNodeType" RENDER-NODE-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gsk_render_node_type_get_type")
                             (:NOT-A-RENDER-NODE 0)
                             (:CONTAINER-NODE 1)
                             (:CAIRO-NODE 2)
                             (:COLOR-NODE 3)
                             (:LINEAR-GRADIENT-NODE 4)
                             (:REPEATING-LINEAR-GRADIENT-NODE 5)
                             (:RADIAL-GRADIENT-NODE 6)
                             (:REPEATING-RADIAL-GRADIENT-NODE 7)
                             (:CONIC-GRADIENT-NODE 8)
                             (:BORDER-NODE 9)
                             (:TEXTURE-NODE 10)
                             (:INSET-SHADOW-NODE 11)
                             (:OUTSET-SHADOW-NODE 12)
                             (:TRANSFORM-NODE 13)
                             (:OPACITY-NODE 14)
                             (:COLOR-MATRIX-NODE 15)
                             (:REPEAT-NODE 16)
                             (:CLIP-NODE 17)
                             (:ROUNDED-CLIP-NODE 18)
                             (:SHADOW-NODE 19)
                             (:BLEND-NODE 20)
                             (:CROSS-FADE-NODE 21)
                             (:TEXT-NODE 22)
                             (:BLUR-NODE 23)
                             (:DEBUG-NODE 24)
                             (:GL-SHADER-NODE 25))
             (gobject:get-g-type-definition "GskRenderNodeType"))))

;;;     GskScalingFilter
;;;     GskBlendMode
;;;     GskCorner
;;;     GskSerializationError

;;;     GskTransformCategory

(test transform-category-enumeration
  ;; Check the type
  (is (g-type-is-enum "GskTransformCategory"))
  ;; Check the type initializer
  (is (eq (gtype "GskTransformCategory")
          (gtype (cffi:foreign-funcall "gsk_transform_category_get_type" :size))))
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
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GskTransformCategory" TRANSFORM-CATEGORY
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

;;;     GskGLUniformType

;;; --- 2023-5-29 --------------------------------------------------------------
