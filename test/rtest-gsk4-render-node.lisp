(in-package :gtk-test)

(def-suite gsk-render-node :in gsk-suite)
(in-suite gsk-render-node)

;;; --- Types and Values -------------------------------------------------------

;;;     GskRenderNodeType

(test gsk-render-node-type
  ;; Check type
  (is (g:type-is-enum "GskRenderNodeType"))
  ;; Check type initializer
  (is (eq (g:gtype "GskRenderNodeType")
          (g:gtype (cffi:foreign-funcall "gsk_render_node_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:render-node-type
          (glib:symbol-for-gtype "GskRenderNodeType")))
  ;; Check names
  (is (equal '("GSK_NOT_A_RENDER_NODE" "GSK_CONTAINER_NODE" "GSK_CAIRO_NODE"
               "GSK_COLOR_NODE" "GSK_LINEAR_GRADIENT_NODE"
               "GSK_REPEATING_LINEAR_GRADIENT_NODE" "GSK_RADIAL_GRADIENT_NODE"
               "GSK_REPEATING_RADIAL_GRADIENT_NODE" "GSK_CONIC_GRADIENT_NODE"
               "GSK_BORDER_NODE" "GSK_TEXTURE_NODE" "GSK_INSET_SHADOW_NODE"
               "GSK_OUTSET_SHADOW_NODE" "GSK_TRANSFORM_NODE" "GSK_OPACITY_NODE"
               "GSK_COLOR_MATRIX_NODE" "GSK_REPEAT_NODE" "GSK_CLIP_NODE"
               "GSK_ROUNDED_CLIP_NODE" "GSK_SHADOW_NODE" "GSK_BLEND_NODE"
               "GSK_CROSS_FADE_NODE" "GSK_TEXT_NODE" "GSK_BLUR_NODE"
               "GSK_DEBUG_NODE" "GSK_GL_SHADER_NODE" "GSK_TEXTURE_SCALE_NODE"
               "GSK_MASK_NODE" "GSK_FILL_NODE" "GSK_STROKE_NODE"
               "GSK_SUBSURFACE_NODE" "GSK_COMPONENT_TRANSFER_NODE")
             (glib-test:list-enum-item-names "GskRenderNodeType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31)
             (glib-test:list-enum-item-values "GskRenderNodeType")))
  ;; Check nick names
  (is (equal '("not-a-render-node" "container-node" "cairo-node" "color-node"
               "linear-gradient-node" "repeating-linear-gradient-node"
               "radial-gradient-node" "repeating-radial-gradient-node"
               "conic-gradient-node" "border-node" "texture-node"
               "inset-shadow-node" "outset-shadow-node" "transform-node"
               "opacity-node" "color-matrix-node" "repeat-node" "clip-node"
               "rounded-clip-node" "shadow-node" "blend-node" "cross-fade-node"
               "text-node" "blur-node" "debug-node" "gl-shader-node"
               "texture-scale-node" "mask-node" "fill-node" "stroke-node"
               "subsurface-node" "component-transfer-node")
             (glib-test:list-enum-item-nicks "GskRenderNodeType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskRenderNodeType" GSK:RENDER-NODE-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gsk_render_node_type_get_type")
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
                                    (:GL-SHADER-NODE 25)
                                    (:TEXTURE-SCALE-NODE 26)
                                    (:MASK-NODE 27)
                                    (:FILL-NODE 28)
                                    (:STROKE-NODE 29)
                                    (:SUBSURFACE-NODE 30)
                                    (:COMPONENT-TRANSFER-NODE 31))
             (gobject:get-gtype-definition "GskRenderNodeType"))))

;;;     GskScalingFilter

(test gsk-scaling-filter
  ;; Check type
  (is (g:type-is-enum "GskScalingFilter"))
  ;; Check type initializer
  (is (eq (g:gtype "GskScalingFilter")
          (g:gtype (cffi:foreign-funcall "gsk_scaling_filter_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:scaling-filter
          (glib:symbol-for-gtype "GskScalingFilter")))
  ;; Check names
  (is (equal '("GSK_SCALING_FILTER_LINEAR" "GSK_SCALING_FILTER_NEAREST"
               "GSK_SCALING_FILTER_TRILINEAR")
             (glib-test:list-enum-item-names "GskScalingFilter")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GskScalingFilter")))
  ;; Check nick names
  (is (equal '("linear" "nearest" "trilinear")
             (glib-test:list-enum-item-nicks "GskScalingFilter")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskScalingFilter" GSK:SCALING-FILTER
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_scaling_filter_get_type")
                       (:LINEAR 0)
                       (:NEAREST 1)
                       (:TRILINEAR 2))
             (gobject:get-gtype-definition "GskScalingFilter"))))

;;;     GskColorStop
;;;     GskShadow

;;;     GskBlendMode

(test gsk-blend-mode
  ;; Check type
  (is (g:type-is-enum "GskBlendMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GskBlendMode")
          (g:gtype (cffi:foreign-funcall "gsk_blend_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:blend-mode
          (glib:symbol-for-gtype "GskBlendMode")))
  ;; Check names
  (is (equal '("GSK_BLEND_MODE_DEFAULT" "GSK_BLEND_MODE_MULTIPLY"
               "GSK_BLEND_MODE_SCREEN" "GSK_BLEND_MODE_OVERLAY"
               "GSK_BLEND_MODE_DARKEN" "GSK_BLEND_MODE_LIGHTEN"
               "GSK_BLEND_MODE_COLOR_DODGE" "GSK_BLEND_MODE_COLOR_BURN"
               "GSK_BLEND_MODE_HARD_LIGHT" "GSK_BLEND_MODE_SOFT_LIGHT"
               "GSK_BLEND_MODE_DIFFERENCE" "GSK_BLEND_MODE_EXCLUSION"
               "GSK_BLEND_MODE_COLOR" "GSK_BLEND_MODE_HUE"
               "GSK_BLEND_MODE_SATURATION" "GSK_BLEND_MODE_LUMINOSITY")
             (glib-test:list-enum-item-names "GskBlendMode")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
             (glib-test:list-enum-item-values "GskBlendMode")))
  ;; Check nick names
  (is (equal '("default" "multiply" "screen" "overlay" "darken" "lighten"
               "color-dodge" "color-burn" "hard-light" "soft-light" "difference"
               "exclusion" "color" "hue" "saturation" "luminosity")
             (glib-test:list-enum-item-nicks "GskBlendMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskBlendMode" GSK:BLEND-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_blend_mode_get_type")
                       (:DEFAULT 0)
                       (:MULTIPLY 1)
                       (:SCREEN 2)
                       (:OVERLAY 3)
                       (:DARKEN 4)
                       (:LIGHTEN 5)
                       (:COLOR-DODGE 6)
                       (:COLOR-BURN 7)
                       (:HARD-LIGHT 8)
                       (:SOFT-LIGHT 9)
                       (:DIFFERENCE 10)
                       (:EXCLUSION 11)
                       (:COLOR 12)
                       (:HUE 13)
                       (:SATURATION 14)
                       (:LUMINOSITY 15))
             (gobject:get-gtype-definition "GskBlendMode"))))

;;;     GskMaskMode                                         Since 4.10

(test gsk-mask-mode
  ;; Check type
  (is (g:type-is-enum "GskMaskMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GskMaskMode")
          (g:gtype (cffi:foreign-funcall "gsk_mask_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:mask-mode
          (glib:symbol-for-gtype "GskMaskMode")))
  ;; Check names
  (is (equal '("GSK_MASK_MODE_ALPHA" "GSK_MASK_MODE_INVERTED_ALPHA"
               "GSK_MASK_MODE_LUMINANCE" "GSK_MASK_MODE_INVERTED_LUMINANCE")
             (glib-test:list-enum-item-names "GskMaskMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "GskMaskMode")))
  ;; Check nick names
  (is (equal '("alpha" "inverted-alpha" "luminance" "inverted-luminance")
             (glib-test:list-enum-item-nicks "GskMaskMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskMaskMode" GSK:MASK-MODE
                       (:EXPORT T
                        :TYPE-INITIALIZER "gsk_mask_mode_get_type")
                       (:ALPHA 0)
                       (:INVERTED-ALPHA 1)
                       (:LUMINANCE 2)
                       (:INVERTED-LUMINANCE 3))
             (gobject:get-gtype-definition "GskMaskMode"))))

;;; --- GskRenderNode ----------------------------------------------------------

;;;     gsk_render_node_ref
;;;     gsk_render_node_unref
;;;     gsk_render_node_get_node_type

(test gsk-render-node-ref/unref
  (graphene:with-rect (rect 0 0 10 20)
    (let* ((color (gdk:rgba-parse "red"))
           (node (gsk:color-node-new color rect)))
      (is (eq :color-node (gsk:render-node-node-type node)))
      (is (eq :color-node
              (gsk:render-node-node-type (gsk:render-node-ref node))))
      (is-false (gsk:render-node-unref node))
      (is (eq :color-node (gsk:render-node-node-type node)))
      (is-false (gsk:render-node-unref node)))))

;;;     gsk_render_node_draw

;; Function for drawing a node on a PDF surface
(defun render-node-draw-to-pdf (node width height &optional (filename nil))
  (let* ((path (if filename
                   (glib-sys:sys-path filename)
                   (glib-sys:sys-path "test/out/render-node.pdf")))
         (surface (cairo:pdf-surface-create path width height))
         (context (cairo:create surface)))
    (gsk:render-node-ref node)
    (cairo:save context)
    ;; Clear surface
    (cairo:set-source-rgb context 1.0 1.0 1.0)
    (cairo:paint context)
    ;; Draw the render node
    (gsk:render-node-draw node context)
    (cairo:stroke context)
    (cairo:restore context)
    (cairo:show-page context)
    ;; Clean the resources
    (cairo:surface-destroy surface)
    (cairo:destroy context)
    (gsk:render-node-unref node)))

;; Check the function
(test gsk-render-node-draw.1
  (graphene:with-rect (rect 20 20 60 60)
    (let* ((color (gdk:rgba-parse "red"))
           (node (gsk:color-node-new color rect))
           (width 100) (height 100))
      (render-node-draw-to-pdf node width height)
      (gsk:render-node-unref node))))

;;;     gsk_render_node_serialize
;;;     gsk_render_node_deserialize

(test gsk-render-node-serialize/deserialize
  (graphene:with-rect (rect 0 0 10 20)
    (let* ((color (gdk:rgba-parse "red"))
           (node (gsk:color-node-new color rect))
           (bytes nil))
      (is (eq :color-node (gsk:render-node-node-type node)))
      (is (typep (setf bytes (gsk:render-node-serialize node)) 'g:bytes))
      (setf node (gsk:render-node-deserialize bytes))
      (is (eq :color-node (gsk:render-node-node-type node))))))

;;;     gsk_render_node_write_to_file

(test gsk-render-node-write-to-file
  (graphene:with-rect (rect 0 0 10 20)
    (let* ((color (gdk:rgba-parse "red"))
           (node (gsk:color-node-new color rect))
           (filename  (glib-sys:sys-path "test/out/render-node.txt")))
      (gsk:render-node-write-to-file node filename))))

;;;     gsk_render_node_get_bounds

(test gsk-render-node-bounds
  (graphene:with-rects ((rect1 0 0 10 20) (rect2 0 0 0 0))
    (let* ((color (gdk:rgba-parse "red"))
           (node (gsk:color-node-new color rect1)))
      (is (eq :color-node (gsk:render-node-node-type node)))
      (is (graphene:rect-equal rect1
                               (gsk:render-node-bounds node rect2))))))

;;; --- GskContainerNode -------------------------------------------------------

;;;     gsk_container_node_new
;;;     gsk_container_node_get_n_children
;;;     gsk_container_node_get_child

(test gsk-container-node-new
  (graphene:with-rect (rect 0 0 10 20)
    (let* ((color1 (gsk:color-node-new (gdk:rgba-new :red 1 :alpha 1) rect))
           (color2 (gsk:color-node-new (gdk:rgba-new :green 1 :alpha 1) rect))
           (color3 (gsk:color-node-new (gdk:rgba-new :blue 1 :alpha 1) rect))
           (node (gsk:container-node-new (list color1 color2 color3))))
      (is (eq :container-node
              (gsk:render-node-node-type node)))
      (is (= 3 (gsk:container-node-n-children node)))
      (is (typep (gsk:color-node-color (gsk:container-node-child node 0))
                 'gdk:rgba))
      (is (typep (gsk:color-node-color (gsk:container-node-child node 1))
                 'gdk:rgba))
      (is (typep (gsk:color-node-color (gsk:container-node-child node 2))
                 'gdk:rgba))
      (is-false (gsk:render-node-unref node)))))

(test gsk-container-node-draw
  (graphene:with-rects ((rect1 10 10 80 80)
                        (rect2 20 20 60 60)
                        (rect3 30 30 40 40))
    (let* ((filename  (glib-sys:sys-path "test/out/container-node.txt"))
           (pdffile (glib-sys:sys-path "test/out/container-node.pdf"))
           (color1 (gsk:color-node-new (gdk:rgba-new :red 1 :alpha 1) rect1))
           (color2 (gsk:color-node-new (gdk:rgba-new :green 1 :alpha 1) rect2))
           (color3 (gsk:color-node-new (gdk:rgba-new :blue 1 :alpha 1) rect3))
           (node (gsk:container-node-new (list color1 color2 color3))))
      (is-false (render-node-draw-to-pdf node 100 100 pdffile))
      (is-true (gsk:render-node-write-to-file node filename))
      (is-false (gsk:render-node-unref node)))))

;;; --- GskCairoNode -----------------------------------------------------------

;;;     gsk_cairo_node_new
;;;     gsk_cairo_node_get_draw_context
;;;     gsk_cairo_node_get_surface

(test gsk-cairo-node-new
  (let (node context)
    (graphene:with-rect (bounds 0 0 200 100)
      (is (eq :cairo-node
              (gsk:render-node-node-type (setf node
                                               (gsk:cairo-node-new bounds)))))
      (is (cffi:pointerp (setf context (gsk:cairo-node-draw-context node))))
      (is (cffi:pointer-eq (cairo:target context)
                           (gsk:cairo-node-surface node)))
      ;; Free memory
      (is-false (cairo:destroy context))
      (is-false (gsk:render-node-unref node)))))

(test gsk-cairo-node-draw
  (let ((filename (glib-sys:sys-path "test/out/cairo-node.txt"))
        (pdffile (glib-sys:sys-path "test/out/cairo-node.pdf"))
        (width 100) (height 100)
        node context)
    (graphene:with-rect (bounds 0 0 width height)
      ;; Create GskCairoNode
      (is (eq :cairo-node
              (gsk:render-node-node-type (setf node
                                               (gsk:cairo-node-new bounds)))))
      ;; Get Cairo context
      (setf context (gsk:cairo-node-draw-context node))
      ;; Draw into Cairo context
      (cairo:save context)
      ;; Clear surface
      (cairo:set-source-rgb context 1.0 1.0 1.0)
      (cairo:paint context)
      ;; Example is in 1.0 x 1.0 coordinate space
      (cairo:scale context width height)
      ;; Drawing code goes here
      (setf (cairo:line-width context) 0.1)
      (cairo:set-source-rgb context 1.0 0.0 0.0)
      (cairo:rectangle context 0.25 0.25 0.5 0.5)
      (cairo:stroke context)
      (cairo:restore context)
      ;; Draw GskCairoNode into PDF and text file
      (is-false (render-node-draw-to-pdf node 100 100 pdffile))
      (is-true (gsk:render-node-write-to-file node filename))
      ;; Free memory
      (is-false (cairo:destroy context))
      (is-false (gsk:render-node-unref node)))))

;;; --- GskColorNode -----------------------------------------------------------

;;;     gsk_color_node_new
;;;     gsk_color_node_get_color

(test gsk-color-node-new
  (let ((color (gdk:rgba-parse "red"))
        (node nil))
    (graphene:with-rect (rect 0 0 1 10)
      (is (eq (g:gtype "GskColorNode")
              (g:type-from-instance (setf node
                                          (gsk:color-node-new color rect)))))
    (is (gdk:rgba-equal (gdk:rgba-new :red 1 :alpha 1)
                        (gsk:color-node-color node))))))

;;; --- GskLinearGradientNode --------------------------------------------------

;;;     gsk_linear_gradient_node_new
;;;     gsk_linear_gradient_node_get_start
;;;     gsk_linear_gradient_node_get_end
;;;     gsk_linear_gradient_node_get_n_color_stops
;;;     gsk_linear_gradient_node_get_color_stops

(test gsk-linear-gradient-node-new
  (let ((color-stops (list (list 0 (gdk:rgba-parse "red"))
                           (list 1 (gdk:rgba-parse "blue"))))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-points ((start 10 10) (end 90 90))
        (is (cffi:pointerp (setf node
                                 (gsk:linear-gradient-node-new bounds
                                                               start end
                                                               color-stops))))
        (is (graphene:point-equal start
                                  (gsk:linear-gradient-node-start node)))
        (is (graphene:point-equal end
                                  (gsk:linear-gradient-node-end node)))
        (is (= 2 (gsk:linear-gradient-node-n-color-stops node)))
        (cffi:with-foreign-object (n-stops :size)
          (is (cffi:pointerp (gsk::%linear-gradient-node-color-stops node
                                                                     n-stops)))
          (is (= 2 (cffi:mem-ref n-stops :size))))
        (is (listp (gsk:linear-gradient-node-color-stops node)))))))

(test gsk-linear-gradient-node-draw
  (let ((color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                           (list 1.0 (gdk:rgba-parse "blue"))))
        (filename (glib-sys:sys-path "test/out/linear-gradient-node.txt"))
        (pdffile (glib-sys:sys-path "test/out/linear-gradient-node.pdf"))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-points ((start 0 0) (end 0 100))

        (setf node
              (gsk:linear-gradient-node-new bounds start end color-stops))
        (is-false (render-node-draw-to-pdf node 100 100 pdffile))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;; --- GskRepeatingLinearGradientNode -----------------------------------------

;;;     gsk_repeating_linear_gradient_node_new

(test gsk-repeating-linear-gradient-node-draw
  (let ((color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                           (list 1.0 (gdk:rgba-parse "blue"))))
        (filename (glib-sys:sys-path "test/out/repeating-linear-gradient-node.txt"))
        (pdffile (glib-sys:sys-path "test/out/repeating-linear-gradient-node.pdf"))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-points ((start 0 0) (end 30 30))
        (setf node
              (gsk:repeating-linear-gradient-node-new bounds
                                                      start end
                                                      color-stops))
        (is-false (render-node-draw-to-pdf node 100 100 pdffile))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;;     GskRadialGradientNode
;;;     gsk_radial_gradient_node_new
;;;     gsk_radial_gradient_node_get_n_color_stops
;;;     gsk_radial_gradient_node_get_color_stops
;;;     gsk_radial_gradient_node_get_start
;;;     gsk_radial_gradient_node_get_end
;;;     gsk_radial_gradient_node_get_hradius
;;;     gsk_radial_gradient_node_get_vradius
;;;     gsk_radial_gradient_node_get_center


(test gsk-radial-gradient-node-draw
  (let ((color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                           (list 1.0 (gdk:rgba-parse "blue"))))
        (filename (glib-sys:sys-path "test/out/render-node-to-file.txt"))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-point (center 50 50)

        (setf node
              (gsk:radial-gradient-node-new bounds
                                            center
                                            0.5 0.5
                                            0 100
                                            color-stops))
        (is-false (render-node-draw-to-pdf node 100 100))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;;     GskRepeatingRadialGradientNode
;;;     gsk_repeating_radial_gradient_node_new

(test gsk-repeating-radial-gradient-node-draw
  (let ((color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                           (list 1.0 (gdk:rgba-parse "blue"))))
        (filename (glib-sys:sys-path "test/out/render-node-to-file.txt"))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-point (center 50 50)

        (setf node
              (gsk:repeating-radial-gradient-node-new bounds
                                                      center
                                                      0.5 0.5
                                                      0 30
                                                      color-stops))
        (is-false (render-node-draw-to-pdf node 100 100))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;;     GskConicGradientNode
;;;     gsk_conic_gradient_node_new
;;;     gsk_conic_gradient_node_get_n_color_stops
;;;     gsk_conic_gradient_node_get_color_stops
;;;     gsk_conic_gradient_node_get_center
;;;     gsk_conic_gradient_node_get_rotation

(test gsk-conic-gradient-node-draw
  (let ((color-stops (list (list 0.0 (gdk:rgba-parse "red"))
                           (list 1.0 (gdk:rgba-parse "blue"))))
        (filename (glib-sys:sys-path "test/out/render-node-to-file.txt"))
        node)
    (graphene:with-rect (bounds 0 0 100 100)
      (graphene:with-point (center 50 50)

        (setf node
              (gsk:conic-gradient-node-new bounds center 360 color-stops))
        (is-false (render-node-draw-to-pdf node 100 100))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;;     GskBorderNode
;;;     gsk_border_node_new
;;;     gsk_border_node_get_outline
;;;     gsk_border_node_get_widths
;;;     gsk_border_node_get_colors

(test gsk-border-node-new
  ;; TODO: Simplify the usage with the implementation of macros
  (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
    (graphene:with-rects ((rect -50 -50 100 100) bounds)
      (gsk:rounded-rect-init-from-rect outline rect 50)
      (let* ((widths (list 2.0 2 2.0d0 1/2))
             (black (gdk:rgba-parse "black"))
             (red (gdk:rgba-parse "red"))
             (green (gdk:rgba-parse "green"))
             (blue (gdk:rgba-parse "blue"))
             (colors (list red green blue black))
             (node (gsk:border-node-new outline widths colors))
             rounded bounds corner)
        (is (cffi:pointerp (setf rounded (gsk:border-node-outline node))))
        (is (cffi:pointerp (setf bounds (gsk:rounded-rect-bounds rounded))))
        (is (= -50 (graphene:rect-x bounds)))
        (is (= -50 (graphene:rect-y bounds)))
        (is (= 100 (graphene:rect-width bounds)))
        (is (= 100 (graphene:rect-height bounds)))

        (is (cffi:pointerp (setf corner (gsk:rounded-rect-corner rounded 0))))
        (is (= 50.0 (graphene:size-width corner)))
        (is (= 50.0 (graphene:size-height corner)))

        (is (cffi:pointerp (setf corner (gsk:rounded-rect-corner rounded 1))))
        (is (= 50.0 (graphene:size-width corner)))
        (is (= 50.0 (graphene:size-height corner)))

        (is (cffi:pointerp (setf corner (gsk:rounded-rect-corner rounded 2))))
        (is (= 50.0 (graphene:size-width corner)))
        (is (= 50.0 (graphene:size-height corner)))

        (is (cffi:pointerp (setf corner (gsk:rounded-rect-corner rounded 3))))
        (is (= 50.0 (graphene:size-width corner)))
        (is (= 50.0 (graphene:size-height corner)))

        (is (equal '(2.0 2.0 2.0 0.5)
                   (gsk:border-node-widths node)))
        (is (mapcar #'gdk:rgba-equal
                    (list red green blue black)
                    (gsk:border-node-colors node)))
        (is-false (gsk:render-node-unref node ))))))

(test gsk-border-node-draw
  ;; TODO: Simplify the usage with the implementation of macros
  (cffi:with-foreign-object (outline '(:struct gsk:rounded-rect))
    (graphene:with-rects ((rect 10 10 80 80) bounds)
      (gsk:rounded-rect-init-from-rect outline rect 10)
      (let* ((filename (glib-sys:sys-path "test/out/render-node-to-file.txt"))
             (widths (list 6.0 4.0 2.0 1/2))
             (black (gdk:rgba-parse "black"))
             (red (gdk:rgba-parse "red"))
             (green (gdk:rgba-parse "green"))
             (blue (gdk:rgba-parse "blue"))
             (colors (list red green blue black))
             (node (gsk:border-node-new outline widths colors)))
        (is-false (render-node-draw-to-pdf node 100 100))
        (is-true (gsk:render-node-write-to-file node filename))))))

;;;     GskTextureNode
;;;     gsk_texture_node_new
;;;     gsk_texture_node_get_texture

;;;     GskInsetShadowNode
;;;     GskOutsetShadowNode
;;;     GskTransformNode
;;;     GskOpacityNode
;;;     GskColorMatrixNode
;;;     GskRepeatNode
;;;     GskClipNode
;;;     GskRoundedClipNode

;;;     GskShadowNode

;;;     gsk_shadow_node_new
;;;     gsk_shadow_node_get_shadow
;;;     gsk_shadow_node_get_n_shadows
;;;     gsk_shadow_node_get_child

(test gsk-shadow-node-new
  (graphene:with-rect (bounds 0 0 100 100)
    (let* ((blue (gdk:rgba-parse "blue"))
           (green (gdk:rgba-parse "green"))
           (black (gdk:rgba-parse "black"))
           (child (gsk:color-node-new (gdk:rgba-parse "red") bounds))
           (shadows (list (list blue 10 10 5)
                          (list green 20 20 6)
                          (list black 30 30 7)))
           node)

      (is (cffi:pointerp (setf node
                               (gsk:shadow-node-new child shadows))))

      (is (= 3 (gsk:shadow-node-n-shadows node)))
      (is (cffi:pointerp (gsk:shadow-node-child node)))

      (is (gdk:rgba-equal blue (first (gsk:shadow-node-shadow node 0))))
      (is (gdk:rgba-equal green (first (gsk:shadow-node-shadow node 1))))
      (is (gdk:rgba-equal black (first (gsk:shadow-node-shadow node 2)))))))

;;;     GskBlendNode
;;;     GskCrossFadeNode
;;;     GskTextNode
;;;     GskBlurNode
;;;     GskDebugNode
;;;     GskGLShaderNode
;;;     GskTextureScaleNode                                Since 4.10
;;;     GskMaskNode                                        Since 4.10


;;; --- Functions --------------------------------------------------------------

;;;     gsk_inset_shadow_node_new
;;;     gsk_inset_shadow_node_get_outline
;;;     gsk_inset_shadow_node_get_color
;;;     gsk_inset_shadow_node_get_dx
;;;     gsk_inset_shadow_node_get_dy
;;;     gsk_inset_shadow_node_get_spread
;;;     gsk_inset_shadow_node_get_blur_radius
;;;     gsk_outset_shadow_node_new
;;;     gsk_outset_shadow_node_get_outline
;;;     gsk_outset_shadow_node_get_color
;;;     gsk_outset_shadow_node_get_dx
;;;     gsk_outset_shadow_node_get_dy
;;;     gsk_outset_shadow_node_get_spread
;;;     gsk_outset_shadow_node_get_blur_radius
;;;     gsk_cairo_node_new
;;;     gsk_cairo_node_get_draw_context
;;;     gsk_cairo_node_get_surface
;;;     gsk_transform_node_new
;;;     gsk_transform_node_get_child
;;;     gsk_transform_node_get_transform
;;;     gsk_opacity_node_new
;;;     gsk_opacity_node_get_child
;;;     gsk_opacity_node_get_opacity
;;;     gsk_color_matrix_node_new
;;;     gsk_color_matrix_node_get_child
;;;     gsk_color_matrix_node_get_color_matrix
;;;     gsk_color_matrix_node_get_color_offset
;;;     gsk_repeat_node_new
;;;     gsk_repeat_node_get_child
;;;     gsk_repeat_node_get_child_bounds
;;;     gsk_clip_node_new
;;;     gsk_clip_node_get_child
;;;     gsk_clip_node_get_clip
;;;     gsk_rounded_clip_node_new
;;;     gsk_rounded_clip_node_get_child
;;;     gsk_rounded_clip_node_get_clip
;;;     gsk_blend_node_new
;;;     gsk_blend_node_get_bottom_child
;;;     gsk_blend_node_get_top_child
;;;     gsk_blend_node_get_blend_mode
;;;     gsk_cross_fade_node_new
;;;     gsk_cross_fade_node_get_start_child
;;;     gsk_cross_fade_node_get_end_child
;;;     gsk_cross_fade_node_get_progress
;;;     gsk_text_node_new
;;;     gsk_text_node_get_font
;;;     gsk_text_node_get_glyphs
;;;     gsk_text_node_get_color
;;;     gsk_text_node_has_color_glyphs
;;;     gsk_text_node_get_num_glyphs
;;;     gsk_text_node_get_offset
;;;     gsk_blur_node_new
;;;     gsk_blur_node_get_child
;;;     gsk_blur_node_get_radius
;;;     gsk_debug_node_new
;;;     gsk_debug_node_get_child
;;;     gsk_debug_node_get_message
;;;     gsk_gl_shader_node_new
;;;     gsk_gl_shader_node_get_n_children
;;;     gsk_gl_shader_node_get_child
;;;     gsk_gl_shader_node_get_args
;;;     gsk_gl_shader_node_get_shader

;;; 2026-02-05
