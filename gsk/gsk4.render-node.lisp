;;; ----------------------------------------------------------------------------
;;; gsk.render-node.lisp
;;;
;;; The documentation in this file is taken from the GSK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2026 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GskRenderNode
;;;
;;;     Simple scene graph element
;;;
;;; Types and Values
;;;
;;;     GskRenderNodeType
;;;     GskSerializationError
;;;     GskParseLocation
;;;     GskScalingFilter
;;;     GskColorStop
;;;     GskShadow
;;;     GskBlendMode
;;;     GskMaskMode                                         Since 4.10
;;;     GskComponentTransfer                                Since 4.20
;;;
;;;     GskRenderNode
;;;     GskContainerNode
;;;     GskCairoNode
;;;     GskColorNode
;;;     GskLinearGradientNode
;;;     GskRepeatingLinearGradientNode
;;;     GskRadialGradientNode
;;;     GskRepeatingRadialGradientNode
;;;     GskConicGradientNode
;;;     GskBorderNode
;;;     GskTextureNode
;;;     GskInsetShadowNode
;;;     GskOutsetShadowNode
;;;     GskTransformNode
;;;     GskOpacityNode
;;;     GskColorMatrixNode
;;;     GskRepeatNode
;;;     GskClipNode
;;;     GskRoundedClipNode
;;;     GskShadowNode
;;;     GskBlendNode
;;;     GskCrossFadeNode
;;;     GskTextNode
;;;     GskBlurNode
;;;     GskDebugNode
;;;     GskGLShaderNode                                     Deprecated 4.16
;;;     GskTextureScaleNode                                 Since 4.10
;;;     GskMaskNode                                         Since 4.10
;;;     GskFillNode                                         Since 4.14
;;;     GskStrokeNode                                       Since 4.14
;;;     GskSubsurfaceNode                                   Since 4.14
;;;     GskComponentTransferNode                            Since 4.20
;;;
;;; Functions
;;;
;;;     GskParseErrorFunc
;;;
;;;     gsk_render_node_ref
;;;     gsk_render_node_unref
;;;     gsk_render_node_get_node_type
;;;     gsk_render_node_draw
;;;     gsk_render_node_serialize
;;;     gsk_render_node_deserialize
;;;     gsk_render_node_write_to_file
;;;     gsk_render_node_get_bounds
;;;     gsk_render_node_get_opaque_rect                     Since 4.16
;;;
;;;     gsk_value_dup_render_node                           Since 4.6
;;;     gsk_value_get_render_node                           Since 4.6
;;;     gsk_value_set_render_node                           Since 4.6
;;;     gsk_value_take_render_node                          Since 4.6
;;;
;;;     gsk_container_node_new
;;;     gsk_container_node_get_n_children
;;;     gsk_container_node_get_child
;;;     gsk_cairo_node_new
;;;     gsk_cairo_node_get_draw_context
;;;     gsk_cairo_node_get_surface
;;;     gsk_color_node_new
;;;     gsk_color_node_get_color
;;;     gsk_linear_gradient_node_new
;;;     gsk_linear_gradient_node_get_start
;;;     gsk_linear_gradient_node_get_end
;;;     gsk_linear_gradient_node_get_n_color_stops
;;;     gsk_linear_gradient_node_get_color_stops
;;;     gsk_repeating_linear_gradient_node_new
;;;     gsk_radial_gradient_node_new
;;;     gsk_radial_gradient_node_get_n_color_stops
;;;     gsk_radial_gradient_node_get_color_stops
;;;     gsk_radial_gradient_node_get_start
;;;     gsk_radial_gradient_node_get_end
;;;     gsk_radial_gradient_node_get_hradius
;;;     gsk_radial_gradient_node_get_vradius
;;;     gsk_radial_gradient_node_get_center
;;;     gsk_repeating_radial_gradient_node_new
;;;     gsk_conic_gradient_node_new
;;;     gsk_conic_gradient_node_get_n_color_stops
;;;     gsk_conic_gradient_node_get_color_stops
;;;     gsk_conic_gradient_node_get_center
;;;     gsk_conic_gradient_node_get_rotation
;;;     gsk_border_node_new
;;;     gsk_border_node_get_outline
;;;     gsk_border_node_get_widths
;;;     gsk_border_node_get_colors
;;;     gsk_texture_node_new
;;;     gsk_texture_node_get_texture
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
;;;     gsk_shadow_node_new
;;;     gsk_shadow_node_get_shadow
;;;     gsk_shadow_node_get_n_shadows
;;;     gsk_shadow_node_get_child
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
;;;     gsk_gl_shader_node_new                              Deprecated 4.16
;;;     gsk_gl_shader_node_get_n_children                   Deprecated 4.16
;;;     gsk_gl_shader_node_get_child                        Deprecated 4.16
;;;     gsk_gl_shader_node_get_args                         Deprecated 4.16
;;;     gsk_gl_shader_node_get_shader                       Deprecated 4.16
;;;     gsk_texture_scale_node_new                          Since 4.10
;;;     gsk_texture_scale_node_get_filter                   Since 4.10
;;;     gsk_texture_scale_node_get_texture                  Since 4.10
;;;     gsk_mask_node_new                                   Since 4.10
;;;     gsk_mask_node_get_mask                              Since 4.10
;;;     gsk_mask_node_get_mask_mode                         Since 4.10
;;;     gsk_mask_node_get_source                            Since 4.10
;;;     gsk_fill_node_new                                   Since 4.14
;;;     gsk_fill_node_get_child                             Since 4.14
;;;     gsk_fill_node_get_fill_rule                         Since 4.14
;;;     gsk_fill_node_get_path                              Since 4.14
;;;     gsk_stroke_node_new                                 Since 4.14
;;;     gsk_stroke_node_get_child                           Since 4.14
;;;     gsk_stroke_node_get_path                            Since 4.14
;;;     gsk_stroke_node_get_stroke                          Since 4.14
;;;     gsk_subsurface_node_new                             Since 4.14
;;;     gsk_subsurface_node_subsurface                      Since 4.14
;;;     gsk_subsurface_node_child                           Since 4.14
;;;
;;;     gsk_component_transfer_new_discrete                 Since 4.20
;;;     gsk_component_transfer_new_gamma                    Since 4.20
;;;     gsk_component_transfer_new_identity                 Since 4.20
;;;     gsk_component_transfer_new_levels                   Since 4.20
;;;     gsk_component_transfer_new_linear                   Since 4.20
;;;     gsk_component_transfer_new_table                    Since 4.20
;;;     gsk_component_transfer_copy                         Since 4.20
;;;     gsk_component_transfer_free                         Since 4.20
;;;     gsk_component_transfer_equal                        Since 4.20
;;;
;;;     gsk_component_transfer_node_new                     Since 4.20
;;;     gsk_component_transfer_node_get_child               Since 4.20
;;;     gsk_component_transfer_node_get_transfer            Since 4.20
;;;
;;; Object Hierarchy
;;;
;;;     GskRenderNode
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskRenderNodeType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskRenderNodeType" render-node-type
  (:export t
   :type-initializer "gsk_render_node_type_get_type")
  :not-a-render-node
  :container-node
  :cairo-node
  :color-node
  :linear-gradient-node
  :repeating-linear-gradient-node
  :radial-gradient-node
  :repeating-radial-gradient-node
  :conic-gradient-node
  :border-node
  :texture-node
  :inset-shadow-node
  :outset-shadow-node
  :transform-node
  :opacity-node
  :color-matrix-node
  :reapeat-node
  :clip-node
  :rounded-clip-node
  :shadow-node
  :blend-node
  :cross-fade-node
  :text-node
  :blur-node
  :debug-node
  :gl-shader-node
  #+gtk-4-10
  :texture-scale-node
  #+gtk-4-10
  :mask-node
  #+gtk-4-14
  :fill-node
  #+gtk-4-14
  :stroke-node
  #+gtk-4-14
  :subsurface-node
  #+gtk-4-20
  :component-transfer-node)

#+liber-documentation
(setf (liber:alias-for-symbol 'render-node-type)
      "GEnum"
      (liber:symbol-documentation 'render-node-type)
 "@version{2025-11-02}
  @begin{declaration}
(gobject:define-genum \"GskRenderNodeType\" render-node-type
  (:export t
   :type-initializer \"gsk_render_node_type_get_type\")
  :not-a-render-node
  :container-node
  :cairo-node
  :color-node
  :linear-gradient-node
  :repeating-linear-gradient-node
  :radial-gradient-node
  :repeating-radial-gradient-node
  :conic-gradient-node
  :border-node
  :texture-node
  :inset-shadow-node
  :outset-shadow-node
  :transform-node
  :opacity-node
  :color-matrix-node
  :reapeat-node
  :clip-node
  :rounded-clip-node
  :shadow-node
  :blend-node
  :cross-fade-node
  :text-node
  :blur-node
  :debug-node
  :gl-shader-node
  #+gtk-4-10
  :texture-scale-node
  #+gtk-4-10
  :mask-node
  #+gtk-4-14
  :fill-node
  #+gtk-4-14
  :stroke-node
  #+gtk-4-14
  :subsurface-node
  #+gtk-4-20
  :component-transfer-node)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:not-a-render-node]{Error type. No node will ever have this type.}
      @entry[:container-node]{A node containing a stack of children.}
      @entry[:cairo-node]{A node drawing a `cairo_surface_t`}
      @entry[:color-node]{A node drawing a single color rectangle.}
      @entry[:linear-gradient-node]{A node drawing a linear gradient.}
      @entry[:repeating-linear-gradient-node]{A node drawing a repeating linear
        gradient.}
      @entry[:radial-gradient-node]{A node drawing a radial gradient.}
      @entry[:repeating-radial-gradient-node]{A node drawing a repeating radial
        gradient.}
      @entry[:conic-gradient-node]{A node drawing a conic gradient.}
      @entry[:border-node]{A node stroking a border around an area.}
      @entry[:texture-node]{A node drawing a `GdkTexture`.}
      @entry[:inset-shadow-node]{A node drawing an inset shadow.}
      @entry[:outset-shadow-node]{A node drawing an outset shadow.}
      @entry[:transform-node]{A node that renders its child after applying a
        matrix transform.}
      @entry[:opacity-node]{A node that changes the opacity of its child.}
      @entry[:color-matrix-node]{A node that applies a color matrix to every
        pixel.}
      @entry[:repeat-node]{A node that repeats the child's contents.}
      @entry[:clip-node]{A node that clips its child to a rectangular area.}
      @entry[:rounded-clip-node]{A node that clips its child to a rounded
        rectangle.}
      @entry[:shadow-node]{A node that draws a shadow below its child.}
      @entry[:blend-node]{A node that blends two children together.}
      @entry[:cross-fade-node]{A node that cross-fades between two children.}
      @entry[:text-node]{A node containing a glyph string.}
      @entry[:blur-node]{A node that applies a blur.}
      @entry[:debug-node]{Debug information that does not affect the rendering.}
      @entry[:gl-shader-node]{A node that uses OpenGL fragment shaders to render.}
      @entry[:texture-scale-node]{A node drawing a @class{gdk:texture} object
        scaled and filtered. Since 4.10}
      @entry[:mask-node]{A node that masks one child with another. Since 4.10}
      @entry[:fill-node]{A node that fills a path. Since 4.14}
      @entry[:stroke-node]{A node that strokes a path. Since 4.14}
      @entry[:subsurface-node]{A node that possibly redirects part of the scene
        graph to a subsurface. Since 4.14}
      @entry[:component-transfer-node]{A node that applies some function to each
        color component. Since 4.20}
    @end{simple-table}
  @end{values}
  @begin{short}
    The type of a node determines what the node is rendering.
  @end{short}
  @see-class{gsk:render-node}")

;;; ----------------------------------------------------------------------------
;;; enum GskSerializationError
;;;
;;; Errors that can happen during (de)serialization.
;;;
;;; GSK_SERIALIZATION_UNSUPPORTED_FORMAT :
;;;     The format can not be identified
;;;
;;; GSK_SERIALIZATION_UNSUPPORTED_VERSION :
;;;     The version of the data is not understood
;;;
;;; GSK_SERIALIZATION_INVALID_DATA :
;;;     The given data may not exist in a proper serialization
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GskParseLocation
;;;
;;; struct GskParseLocation {
;;;   gsize bytes;
;;;   gsize chars;
;;;   gsize lines;
;;;   gsize line_bytes;
;;;   gsize line_chars;
;;; };
;;;
;;; A location in a parse buffer.
;;;
;;; gsize bytes;
;;;     the offset of the location in the parse buffer, as bytes
;;;
;;; gsize chars;
;;;     the offset of the location in the parse buffer, as characters
;;;
;;; gsize lines;
;;;     the line of the location in the parse buffer
;;;
;;; gsize line_bytes;
;;;     the position in the line, as bytes
;;;
;;; gsize line_chars;
;;;     the position in the line, as characters
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskScalingFilter
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskScalingFilter" scaling-filter
  (:export t
   :type-initializer "gsk_scaling_filter_get_type")
  (:linear 0)
  (:nearest 1)
  (:trilinear 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'scaling-filter)
      "GEnum"
      (liber:symbol-documentation 'scaling-filter)
 "@version{2025-08-23}
  @begin{declaration}
(gobject:define-genum \"GskScalingFilter\" scaling-filter
  (:export t
   :type-initializer \"gsk_scaling_filter_get_type\")
  (:linear 0)
  (:nearest 1)
  (:trilinear 2))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:linear]{Linear interpolation filter.}
      @entry[:nearest]{Nearest neighbor interpolation filter.}
      @entry[:trilinear]{Linear interpolation along each axis, plus mipmap
        generation, with linear interpolation along the mipmap levels.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The filters used when scaling texture data.
  @end{short}
  The actual implementation of each filter is deferred to the rendering
  pipeline.
  @see-class{gsk:render-node}")

;;; ----------------------------------------------------------------------------
;;; struct GskColorStop
;;;
;;; struct GskColorStop {
;;;   float offset;
;;;   GdkRGBA color;
;;; };
;;;
;;; A color stop in a gradient node.
;;;
;;; float offset;
;;;     the offset of the color stop
;;;
;;; GdkRGBA color;
;;;     the color at the given offset
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %color-stop
  (offset :float)
  (color (:struct gdk::rgba-cstruct)))

;;; ----------------------------------------------------------------------------
;;; struct GskShadow
;;;
;;; struct GskShadow {
;;;   GdkRGBA color;
;;;   float dx;
;;;   float dy;
;;;   float radius;
;;; };
;;;
;;; The shadow parameters in a shadow node.
;;;
;;; GdkRGBA color;
;;;     the color of the shadow
;;;
;;; float dx;
;;;     the horizontal offset of the shadow
;;;
;;; float dy;
;;;     the vertical offset of the shadow
;;;
;;; float radius;
;;;     the radius of the shadow
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %shadow
  (color (:struct gdk::rgba-cstruct))
  (dx :float)
  (dy :float)
  (radius :float))

;;; ----------------------------------------------------------------------------
;;; GskBlendMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GskBlendMode" blend-mode
  (:export t
   :type-initializer "gsk_blend_mode_get_type")
  (:default 0)
  (:multiply 1)
  (:screen 2)
  (:overlay 3)
  (:darken 4)
  (:ligthen 5)
  (:color-dodge 6)
  (:color-burn 7)
  (:hard-light 8)
  (:soft-ligth 9)
  (:difference 10)
  (:exclusion 11)
  (:color 12)
  (:hue 13)
  (:saturation 14)
  (:luminosity 15))

#+liber-documentation
(setf (liber:alias-for-symbol 'blend-mode)
      "GEnum"
      (liber:symbol-documentation 'blend-mode)
 "@version{2025-08-23}
  @begin{declaration}
(gobject:define-genum \"GskBlendMode\" blend-mode
  (:export t
   :type-initializer \"gsk_blend_mode_get_type\")
  (:default 0)
  (:multiply 1)
  (:screen 2)
  (:overlay 3)
  (:darken 4)
  (:ligthen 5)
  (:color-dodge 6)
  (:color-burn 7)
  (:hard-light 8)
  (:soft-ligth 9)
  (:difference 10)
  (:exclusion 11)
  (:color 12)
  (:hue 13)
  (:saturation 14)
  (:luminosity 15))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:default]{The default blend mode, which specifies no blending.}
      @entry[:multiply]{The source color is multiplied by the destination and
        replaces the destination.}
      @entry[:screen]{Multiplies the complements of the destination and source
        color values, then complements the result.}
      @entry[:overlay]{Multiplies or screens the colors, depending on the
        destination color value. This is the inverse of hard-list.}
      @entry[:darken]{Selects the darker of the destination and source colors.}
      @entry[:ligthen]{Selects the lighter of the destination and source
        colors.}
      @entry[:color-dodge]{Brightens the destination color to reflect the
        source color.}
      @entry{:color-burn]{Darkens the destination color to reflect the source
        color.}
      @entry[:hard-ligth]{Multiplies or screens the colors, depending on the
        source color value.}
      @entry[:soft-ligth]{Darkens or lightens the colors, depending on the
        source color value.}
      @entry[:difference]{Subtracts the darker of the two constituent colors
        from the lighter color.}
      @entry[:exclusion]{Produces an effect similar to that of the difference
        mode but lower in contrast.}
      @entry[:color]{Creates a color with the hue and saturation of the source
        color and the luminosity of the destination color.}
      @entry[:hue]{Creates a color with the hue of the source color and the
        saturation and luminosity of the destination color.}
      @entry[:saturation]{Creates a color with the saturation of the source
        color and the hue and luminosity of the destination color.}
      @entry[:luminosity]{Creates a color with the luminosity of the source
        color and the hue and saturation of the destination color.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The blend modes available for render nodes.
  @end{short}
  The implementation of each blend mode is deferred to the rendering pipeline.
  See @url[https://www.w3.org/TR/compositing-1/]{Composting and Blending} for
  more information on blending and blend modes.
  @see-class{gsk:render-node}")

;;; ----------------------------------------------------------------------------
;;; GskMaskMode
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(gobject:define-genum "GskMaskMode" mask-mode
  (:export t
   :type-initializer "gsk_mask_mode_get_type")
  (:alpha 0)
  (:inverted-alpha 1)
  (:luminance 2)
  (:inverted-luminace 3))

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-symbol 'mask-mode)
      "GEnum"
      (liber:symbol-documentation 'mask-mode)
 "@version{2025-08-23}
  @begin{declaration}
(gobject:define-genum \"GskMaskMode\" mask-mode
  (:export t
   :type-initializer \"gsk_mask_mode_get_type\")
  (:alpha 0)
  (:inverted-alpha 1)
  (:luminance 2)
  (:inverted-luminace 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:alpha]{Use the alpha channel of the mask.}
      @entry[inverted-alpha]{Use the inverted alpha channel of the mask.}
      @entry[:luminance]{Use the luminance of the mask, multiplied by mask
        alpha.}
      @entry[:inverted-luminance]{Use the inverted luminance of the mask,
        multiplied by mask alpha.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The mask modes available for mask nodes.
  @end{short}
  Since 4.10
  @see-class{gsk:render-node}")

;;; ----------------------------------------------------------------------------
;;; GskRenderNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type render-node ()
  ()
  (:actual-type :pointer)
  (:simple-parser render-node))

(defmethod cffi:translate-to-foreign (proxy (type render-node))
  proxy)

(defmethod cffi:translate-from-foreign (native (type render-node))
  native)

#+liber-documentation
(setf (liber:alias-for-class 'render-node)
      "GskRenderNode"
      (documentation 'render-node 'type)
 "@version{2026-02-11}
  @begin{short}
    The @class{gsk:render-node} instance is the basic block in a scene graph to
    be rendered using the @class{gsk:renderer} object.
  @end{short}
  Each node has a parent, except the top-level node. Each node may have child
  nodes.

  Each node has an associated drawing surface, which has the size of the
  rectangle set when creating it.

  Render nodes are meant to be transient. Once they have been associated to a
  @class{gsk:renderer} object it is safe to release any reference you have on
  them. All @class{gsk:render-node} instances are immutable, you can only
  specify their properties during construction.
  @see-class{gsk:render-node}")

(export 'render-node)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_ref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_ref" render-node-ref) render-node
 #+liber-documentation
 "@version{2026-02-11}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:render-node} instance for an additional reference.}
  @short{Acquires a reference on the given render node.}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'render-node-ref)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_unref
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_unref" render-node-unref) :void
 #+liber-documentation
 "@version{2026-02-11}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{short}
    Releases a reference on the given render node.
  @end{short}
  If the reference was the last, the resources associated to the node are freed.
  @see-class{gsk:render-node}"
  (node render-node))

(export 'render-node-unref)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_node_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_get_node_type" render-node-node-type)
    render-node-type
 #+liber-documentation
 "@version{2025-08-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @sym{gsk:render-node-type} value for the type of @arg{node}.}
  @short{Returns the type of the render node.}
  @see-class{gsk:render-node}
  @see-symbol{gsk:render-node-type}"
  (node render-node))

(export 'render-node-node-type)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_draw
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_draw" render-node-draw) :void
 #+liber-documentation
 "@version{2025-08-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[cr]{a @sym{cairo:context-t} instance to draw to}
  @begin{short}
    Draw the contents of @arg{node} to the given Cairo context.
  @end{short}
  Typically, you will use this function to implement fallback rendering of
  render nodes on an intermediate Cairo context, instead of using the drawing
  context associated to the rendering buffer of a @class{gdk:surface} object.

  For advanced nodes that cannot be supported using Cairo, in particular for
  nodes doing 3D operations, this function may fail.
  @see-class{gsk:render-node}
  @see-class{gdk:surface}
  @see-symbol{cairo:context-t}"
  (node render-node)
  (cr (:pointer (:struct cairo:context-t))))

(export 'render-node-draw)

;;; ----------------------------------------------------------------------------
;;; GskParseErrorFunc ()
;;;
;;; void
;;; (*GskParseErrorFunc) (const GskParseLocation *start,
;;;                       const GskParseLocation *end,
;;;                       const GError *error,
;;;                       gpointer user_data);
;;;
;;; The type of callback that is called when a parse error occurs during
;;; deserialization of node data.
;;;
;;; start :
;;;     start of the error location
;;;
;;; end :
;;;     end of the error location
;;;
;;; error :
;;;     the error
;;;
;;; user_data :
;;;     user data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_serialize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_serialize" render-node-serialize)
    (g:boxed g:bytes)
 #+liber-documentation
 "@version{2026-02-11}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{g:bytes} instance representing the node.}
  @begin{short}
    Serializes the node for later deserialization via the
    @fun{gsk:render-node-deserialize} function.
  @end{short}
  No guarantees are made about the format used other than that the same version
  of GTK will be able to deserialize the result of a call to the
  @fun{gsk:render-node-serialize} and @fun{gsk:render-node-deserialize}
  functions will correctly reject files it cannot open that were created with
  previous versions of GTK.

  The intended use of this functions is testing, benchmarking and debugging.
  The format is not meant as a permanent storage format.
  @see-class{gsk:render-node}
  @see-function{gsk:render-node-deserialize}"
  (node render-node))

(export 'render-node-serialize)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_deserialize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_deserialize" %render-node-deserialize)
    render-node
  (bytes (g:boxed g:bytes))
  (errfunc :pointer)
  (data :pointer))

(defun render-node-deserialize (bytes)
 #+liber-documentation
 "@version{2026-02-11}
  @argument[bytes]{a @class{g:bytes} instance containing the data}
  @return{The new @class{gsk:render-node} instance, or @code{nil} on error.}
  @begin{short}
    Loads data previously created via the @fun{gsk:render-node-serialize}
    function.
  @end{short}
  For a discussion of the supported format, see that function.
  @see-class{gsk:render-node}
  @see-function{gsk:render-node-serialize}"
  (%render-node-deserialize bytes (cffi:null-pointer) (cffi:null-pointer)))

(export 'render-node-deserialize)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_write_to_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_write_to_file" %render-node-write-to-file)
    :boolean
  (node render-node)
  (path :string)
  (err :pointer))

(defun render-node-write-to-file (node path)
 #+liber-documentation
 "@version{2025-02-08}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[path]{a pathname or namestring for the file to save it to}
  @return{@em{True} if saving was successful.}
  @begin{short}
    This function is mostly intended for use inside a debugger to quickly dump
    a render node to a file for later inspection.
  @end{short}
  @begin[Examples]{dictionary}
    Example output for a @class{gsk:color-node} instance.
    @begin{pre}
color {
  bounds: 0 0 10 20;
  color: rgb(255,0,0);
@}
    @end{pre}
  @end{dictionary}
  @see-class{gsk:render-node}"
  (glib:with-error (err)
    (%render-node-write-to-file node (namestring path) err)))

(export 'render-node-write-to-file)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_get_bounds" %render-node-bounds) :void
  (node render-node)
  (bounds (:pointer (:struct graphene:rect-t))))

(defun render-node-bounds (node bounds)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[bounds]{a @sym{graphene:rect-t} instance}
  @return{The @arg{bounds} value for the boundaries.}
  @begin{short}
    Retrieves the boundaries of the render node.
  @end{short}
  The render node will not draw outside of its boundaries.
  @see-class{gsk:render-node}"
  (%render-node-bounds node bounds)
  bounds)

(export 'render-node-bounds)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_opaque_rect                         Since 4.16
;;; ----------------------------------------------------------------------------

;; TODO: Implement the function

;;; ----------------------------------------------------------------------------
;;; gsk_value_dup_render_node                               Since 4.6
;;;
;;; Retrieves the `GskRenderNode` stored inside the given `value`, and acquires
;;; a reference to it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_value_get_render_node function                      Since 4.6
;;;
;;; Retrieves the `GskRenderNode` stored inside the given `value`.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_value_set_render_node                               Since 4.6
;;;
;;; Stores the given `GskRenderNode` inside `value`. The [struct@GObject.Value]
;;; will acquire a reference to the `node`.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_value_take_render_node function                     Since 4.6
;;;
;;; Stores the given `GskRenderNode` inside `value`. This function transfers
;;; the ownership of the `node` to the `GValue`.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskContainerNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type container-node (render-node)
  ()
  (:simple-parser container-node))

#+liber-documentation
(setf (liber:alias-for-class 'container-node)
      "GskRenderNode"
      (documentation 'container-node 'type)
 "@version{2026-02-05}
  @begin{short}
    The render node that can contain other render nodes.
  @end{short}
  @see-constructor{gsk:container-node-new}
  @see-class{gsk:render-node}")

(export 'container-node)

;;; ----------------------------------------------------------------------------
;;; gsk_container_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_new" %container-node-new) render-node
  (children :pointer)
  (n-children :uint))

(defun container-node-new (children)
 #+liber-documentation
 "@version{2026-02-05}
  @argument[children]{a list of @class{gsk:render-node} instances with the
    children of the render node}
  @return{The new @class{gsk:container-node} instance.}
  @begin{short}
    Creates a new container node for holding the given @arg{children}.
  @end{short}
  The new render node will acquire a reference to each of the children.
  @see-class{gsk:container-node}"
  (let ((n-children (length children)))
    (cffi:with-foreign-object (children-ptr :pointer n-children)
      (iter (for i from 0 below n-children)
            (for child in children)
            (setf (cffi:mem-aref children-ptr :pointer i) child))
      (%container-node-new children-ptr n-children))))

(export 'container-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_container_node_get_n_children
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_get_n_children" container-node-n-children)
    :uint
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @class{gsk:container-node} instance}
  @return{The unsigned integer for the number of children of @arg{node}.}
  @begin{short}
    Retrieves the number of direct children of @arg{node}.
  @end{short}
  @see-class{gsk:container-node}"
  (node render-node))

(export 'container-node-n-children)

;;; ----------------------------------------------------------------------------
;;; gsk_container_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_get_child" container-node-child) render-node
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @class{gsk:container-node} instance}
  @argument[index]{an unsigned integer for the position of the child to get}
  @return{The @arg{index}'th child of @arg{node}.}
  @short{Gets one of the children of the container node.}
  @see-class{gsk:container-node}"
  (node render-node)
  (index :uint))

(export 'container-node-child)

;;; ----------------------------------------------------------------------------
;;; GskCairoNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type cairo-node (render-node)
  ()
  (:simple-parser cairo-node))

#+liber-documentation
(setf (liber:alias-for-class 'cairo-node)
      "GskRenderNode"
      (documentation 'cairo-node 'type)
 "@version{2026-02-05}
  @begin{short}
    The render node for a Cairo surface.
  @end{short}
  @see-constructor{gsk:cairo-node-new}
  @see-class{gsk:render-node}")

(export 'cairo-node)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_new" cairo-node-new) render-node
 #+liber-documentation
 "@version{2026-02-05}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render to}
  @return{The new @class{gsk:cairo-node} instance.}
  @begin{short}
    Creates a render node that will render a Cairo surface into the area given
    by @arg{bounds}.
  @end{short}
  You can draw to the Cairo surface using the @fun{gsk:cairo-node-draw-context}
  function.
  @see-class{gsk:cairo-node}
  @see-symbol{graphene:rect-t}
  @see-function{gsk:cairo-node-draw-context}"
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'cairo-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_get_draw_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_get_draw_context" cairo-node-draw-context)
    (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @class{gsk:cairo-node} instance for a Cairo surface}
  @begin{return}
    The @symbol{cairo:context-t} instance used for drawing, use the
    @fun{cairo:destroy} function when done drawing.
  @end{return}
  @begin{short}
    Creates a Cairo context for drawing using the surface associated to the
    render node.
  @end{short}
  If no surface exists yet, a surface will be created optimized for rendering.
  @see-class{gsk:cairo-node}
  @see-symbol{cairo:context-t}
  @see-function{cairo:destroy}"
  (node cairo-node))

(export 'cairo-node-draw-context)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_get_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_get_surface" cairo-node-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @class{gsk:cairo-node} instance for a Cairo surface}
  @return{The @sym{cairo:surface-t} instance for a Cairo surface.}
  @begin{short}
    Retrieves the Cairo surface used by the render node.
  @end{short}
  @see-class{gsk:cairo-node}
  @see-symbol{cairo:surface-t}"
  (node cairo-node))

(export 'cairo-node-surface)

;;; ----------------------------------------------------------------------------
;;; GskColorNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type color-node (render-node)
  ()
  (:simple-parser color-node))

#+liber-documentation
(setf (liber:alias-for-class 'color-node)
      "GskRenderNode"
      (documentation 'color-node 'type)
 "@version{2026-02-05}
  @begin{short}
    The render node for a solid color.
  @end{short}
  @see-constructor{gsk:color-node-new}
  @see-class{gsk:render-node}")

(export 'color-node)

;;; ----------------------------------------------------------------------------
;;; gsk_color_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_node_new" color-node-new) render-node
 #+liber-documentation
 "@version{2026-02-05}
  @argument[rgba]{a @class{gdk:rgba} instance specifying a color}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the color into}
  @return{The new @class{gsk:color-node} instance.}
  @begin{short}
    Creates a render node that will render the color specified by @arg{rgba}
    into the area given by @arg{bounds}.
  @end{short}
  @see-class{gsk:color-node}
  @see-class{gdk:rgba}
  @see-symbol{graphene:rect-t}"
  (rgba (g:boxed gdk:rgba))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'color-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_color_node_get_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_node_get_color" color-node-color) (g:boxed gdk:rgba)
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @class{gsk:color-node} instance}
  @return{The @class{gdk:rgba} instance for the color of the render node.}
  @short{Retrieves the color of the given render node.}
  @see-class{gsk:color-node}
  @see-class{gdk:rgba}"
  (node render-node))

(export 'color-node-color)

;;; ----------------------------------------------------------------------------
;;; GskLinearGradientNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type linear-gradient-node (render-node)
  ()
  (:simple-parser linear-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'linear-gradient-node)
      "GskRenderNode"
      (documentation 'linear-gradient-node 'type)
 "@version{2026-02-05}
  @begin{short}
    The render node for a linear gradient.
  @end{short}
  @see-constructor{gsk:linear-gradient-node-new}
  @see-class{gsk:render-node}")

(export 'linear-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_new" %linear-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (stops :pointer)
  (nstops :size))

(defun linear-gradient-node-new (bounds start end stops)
 #+liber-documentation
 "@version{2026-02-05}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[start]{a @sym{graphene:point-t} instance for the point at which
    the linear gradient will begin}
  @argument[end]{a @sym{graphene:point-t} instance for the point at which
    the linear gradient will finish}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @return{The new @class{gsk:linear-gradient-node} instance.}
  @begin{short}
    Creates a render node that will create a linear gradient from the given
    points and color stops, and render that into the area given by @arg{bounds}.
  @end{short}

  The offsets of all color stops must be increasing. The first stop's offset
  must be >= 0 and the last stop's offset must be <= 1.
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((nstops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct %color-stop) nstops)
      (iter (for i from 0 below nstops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%linear-gradient-node-new bounds start end stops-ptr nstops))))

(export 'linear-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_start" linear-gradient-node-start)
    (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @sym{gsk:linear-gradient-node} instance}
  @return{The @sym{graphene:point-t} instance for the initial point.}
  @short{Retrieves the initial point of the linear gradient.}
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:point-t}"
  (node render-node))

(export 'linear-gradient-node-start)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_end" linear-gradient-node-end)
    (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @sym{gsk:linear-gradient-node} instance}
  @return{The @sym{graphene:point-t} instance for the final point.}
  @short{Retrieves the final point of the linear gradient.}
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:point-t}"
  (node render-node))

(export 'linear-gradient-node-end)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_n_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_n_color_stops"
               linear-gradient-node-n-color-stops) :size
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @sym{gsk:linear-gradient-node} instance}
  @return{The unsigned integer for the number of color stops.}
  @short{Retrieves the number of color stops in the linear gradient.}
  @see-class{gsk:linear-gradient-node}"
  (node render-node))

(export 'linear-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_color_stops"
               %linear-gradient-node-color-stops) :pointer
  (node render-node)
  (nstops (:pointer :size)))

(defun linear-gradient-node-color-stops (node)
 #+liber-documentation
 "@version{2026-02-05}
  @argument[node]{a @sym{gsk:linear-gradient-node} instance}
  @begin{return}
    The list of the form @code{'((offset1 color1) (offset2 color2) ...)} with
    the offsets and colors of the linear gradient.
  @end{return}
  @short{Retrieves the color stops in the linear gradient.}
  @see-class{gsk:linear-gradient-node}"
  (cffi:with-foreign-object (nstops :size)
    (let ((stops-ptr (%linear-gradient-node-color-stops node nstops)))
      (iter (for i from 0 below (cffi:mem-ref nstops :size))
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'linear-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; GskRepeatingLinearGradientNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type repeating-linear-gradient-node (render-node)
  ()
  (:simple-parser repeating-linear-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'repeating-linear-gradient-node)
      "GskRenderNode"
      (documentation 'repeating-linear-gradient-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a repeating linear gradient.
  @end{short}
  @see-constructor{gsk:repeating-linear-gradient-node-new}
  @see-class{gsk:render-node}")

(export 'repeating-linear-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_repeating_linear_gradient_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeating_linear_gradient_node_new"
               %repeating-linear-gradient-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (stops :pointer)
  (nstops :size))

(defun repeating-linear-gradient-node-new (bounds start end stops)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[start]{a @sym{graphene:point-t} instance for the point at which
    the linear gradient will begin}
  @argument[end]{a @sym{graphene:point-t} instance for the point at which
    the linear gradient will finish}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @return{The new @class{gsk:linear-gradient-node} instance.}
  @begin{short}
    Creates a render node that will create a repeating linear gradient from the
    given points and color stops, and render that into the area given by
    @arg{bounds}.
  @end{short}

  The offsets of all color stops must be increasing. The first stop's offset
  must be >= 0 and the last stop's offset must be <= 1.
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((nstops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct %color-stop) nstops)
      (iter (for i from 0 below nstops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%repeating-linear-gradient-node-new bounds
                                           start end stops-ptr nstops))))

(export 'repeating-linear-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; GskRadialGradientNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type radial-gradient-node (render-node)
  ()
  (:simple-parser radial-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'radial-gradient-node)
      "GskRenderNode"
      (documentation 'radial-gradient-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a radial gradient.
  @end{short}
  @see-constructor{gsk:radial-gradient-node-new}
  @see-class{gsk:render-node}")

(export 'radial-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_new" %radial-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (stops :pointer)
  (nstops :size))

(defun radial-gradient-node-new (bounds center hradius vradius start end stops)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[center]{a @sym{graphene:point-t} instance for the center of
    the radial gradient}
  @argument[hradius]{a number coerced to a single float for the horizontal
    radius}
  @argument[vradius]{a number coerced to a single float for the vertical radius}
  @argument[start]{a number coerced to a single float for the percentage >= 0
    that defines the start of the radial gradient around center}
  @argument[end]{a number coerced to a single float for the percentage >= 0 that
    defines the end of the radial gradient around center}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} for the offsets and colors defining the gradient}
  @return{The new @class{gsk:radial-gradient-node} instance.}
  @begin{short}
    Creates a render node that will create a radial gradient.
  @end{short}
  The radial gradient starts around @arg{center}. The size of the gradient is
  dictated by @arg{hradius} in horizontal orientation and by @arg{vradius} in
  vertial orientation.

  The offsets of all color stops must be increasing. The first stop's offset
  must be >= 0 and the last stop's offset must be <= 1.
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((nstops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct %color-stop) nstops)
      (iter (for i from 0 below nstops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%radial-gradient-node-new bounds
                                 center
                                 (coerce hradius 'single-float)
                                 (coerce vradius 'single-float)
                                 (coerce start 'single-float)
                                 (coerce end 'single-float)
                                 stops-ptr
                                 nstops))))

(export 'radial-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_n_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_n_color_stops"
               radial-gradient-node-n-color-stops) :size
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @sym{gsk:radial-gradient-node} instance}
  @return{The unsigned integer for the number of color stops.}
  @short{Retrieves the number of color stops in the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (node render-node))

(export 'radial-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_color_stops"
               %radial-gradient-node-color-stops) :pointer
  (node render-node)
  (nstops (:pointer :size)))

(defun radial-gradient-node-color-stops (node)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @sym{gsk:radial-gradient-node} instance}
  @begin{return}
    The list of the form @code{'((offset1 color1) (offset2 color2) ...)} for
    the offsets and colors of the radial gradient.
  @end{return}
  @short{Retrieves the color stops in the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (cffi:with-foreign-object (nstops :size)
    (let ((stops-ptr (%radial-gradient-node-color-stops node nstops)))
      (iter (for i from 0 below (cffi:mem-ref nstops :size))
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'radial-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_start" radial-gradient-node-start)
    :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:radial-gradient-node} instance}
  @return{The single float for the start value for the radial gradient.}
  @short{Retrieves the start value for the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (node render-node))

(export 'radial-gradient-node-start)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_end" radial-gradient-node-end)
    :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:radial-gradient-node} instance}
  @return{The single float for the end value for the radial gradient.}
  @short{Retrieves the end value for the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (node render-node))

(export 'radial-gradient-node-end)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_hradius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_hradius"
               radial-gradient-node-hradius) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:radial-gradient-node} instance}
  @return{The single float for the horizontal radius for the radial gradient.}
  @short{Retrieves the horizontal radius for the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (node render-node))

(export 'radial-gradient-node-hradius)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_vradius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_vradius"
               radial-gradient-node-vradius) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:radial-gradient-node} instance}
  @return{The single float for the vertical radius for the radial gradient.}
  @short{Retrieves the vertical radius for the radial gradient.}
  @see-class{gsk:radial-gradient-node}"
  (node render-node))

(export 'radial-gradient-node-vradius)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_center
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_center"
               radial-gradient-node-center)
    (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:radial-gradient-node} instance}
  @begin{return}
    The @sym{graphene:point-t} instance for the center point for the radial
    gradient.
  @end{return}
  @short{Retrieves the center point for the radial gradient.}
  @see-class{gsk:radial-gradient-node}
  @see-symbol{graphene:point-t}"
  (node render-node))

(export 'radial-gradient-node-center)

;;; ----------------------------------------------------------------------------
;;; GskRepeatingRadialGradientNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type repeating-radial-gradient-node (render-node)
  ()
  (:simple-parser repeating-radial-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'repeating-radial-gradient-node)
      "GskRenderNode"
      (documentation 'repeating-radial-gradient-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a repeating radial gradient.
  @end{short}
  @see-constructor{gsk:repeating-radial-gradient-node-new}
  @see-class{gsk:render-node}")

(export 'repeating-radial-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_repeating_radial_gradient_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeating_radial_gradient_node_new"
               %repeating-radial-gradient-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (stops :pointer)
  (nstops :size))

(defun repeating-radial-gradient-node-new (bounds
                                           center
                                           hradius vradius
                                           start end
                                           stops)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[center]{a @sym{graphene:point-t} instance for the center of
    the radial gradient}
  @argument[hradius]{a number coerced to a single float for the horizontal
    radius}
  @argument[vradius]{a number coerced to a single float for the vertical radius}
  @argument[start]{a number coerced to a single float for the percentage >= 0
    that defines the start of the radial gradient around center}
  @argument[end]{a number coerced to a single float for the percentage >= 0 that
    defines the end of the radial gradient around center}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} for the offsets and colors defining the gradient}
  @return{The new @class{gsk:repeating-radial-gradient-node} instance.}
  @begin{short}
    Creates a render node that will create a repeating radial gradient.
  @end{short}
  The repeating radial gradient starts around @arg{center}. The size of the
  gradient is dictated by @arg{hradius} in horizontal orientation and by
  @arg{vradius} in vertial orientation.

  The offsets of all color stops must be increasing. The first stop's offset
  must be >= 0 and the last stop's offset must be <= 1.
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((nstops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct %color-stop) nstops)
      (iter (for i from 0 below nstops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%repeating-radial-gradient-node-new bounds
                                           center
                                           (coerce hradius 'single-float)
                                           (coerce vradius 'single-float)
                                           (coerce start 'single-float)
                                           (coerce end 'single-float)
                                           stops-ptr
                                           nstops))))

(export 'repeating-radial-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; GskConicGradientNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type conic-gradient-node (render-node)
  ()
  (:simple-parser conic-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'conic-gradient-node)
      "GskRenderNode"
      (documentation 'conic-gradient-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a conic gradient.
  @end{short}
  @see-constructor{gsk:conic-gradient-node-new}
  @see-class{gsk:render-node}")

(export 'conic-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_new" %conic-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (rotation :float)
  (stops :pointer)
  (nstops :size))

(defun conic-gradient-node-new (bounds center rotation stops)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the bounds of the
    render node}
  @argument[center]{a @sym{graphene:point-t} instance for the center of the
    gradient}
  @argument[rotation]{a single float for the rotation of the gradient in
    degrees}
  @argument[stops]{a list for the color stops defining the gradient, the offset
    of all color steps must be increasing, the first stop's offset must
    be >= 0 and the last stop's offset must be <= 1}
  @return{The new @class{gsk:conic-gradient-node} instance.}
  @begin{short}
    Creates a render node that draws a conic gradient.
  @end{short}
  The conic gradient starts around @arg{center} in the direction of
  @arg{rotation}. A rotation of 0 means that the gradient points up. Color stops
  are then added clockwise.
  @see-class{gsk:conic-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((nstops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct %color-stop) nstops)
      (iter (for i from 0 below nstops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%conic-gradient-node-new bounds
                                center
                                (coerce rotation 'single-float)
                                stops-ptr
                                nstops))))

(export 'conic-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_n_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_n_color_stops"
               conic-gradient-node-n-color-stops) :size
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:conic-gradient-node} instance for a conic
    gradient}
  @return{The unsigned integer for the number of color stops.}
  @short{Retrieves the number of color stops in the conic gradient.}
  @see-class{gsk:conic-gradient-node}"
  (node render-node))

(export 'conic-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_color_stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_color_stops"
               %conic-gradient-node-color-stops) :pointer
  (node render-node)
  (nstops (:pointer :size)))

(defun conic-gradient-node-color-stops (node)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @sym{gsk:conic-gradient-node} instance}
  @begin{return}
    The list of the form @code{'((offset1 color1) (offset2 color2) ...)} for
    the offsets and colors of the conic gradient.
  @end{return}
  @short{Retrieves the color stops in the conic gradient.}
  @see-class{gsk:conic-gradient-node}"
  (cffi:with-foreign-object (nstops :size)
    (let ((stops-ptr (%conic-gradient-node-color-stops node nstops)))
      (iter (for i from 0 below (cffi:mem-ref nstops :size))
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'conic-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_center
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_center"
               conic-gradient-node-center) (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:conic-gradient-node} instance for a conic
    gradient}
  @begin{return}
    The @sym{graphene:point-t} instance for the center point for the gradient.
  @end{return}
  @begin{short}
    Retrieves the center pointer for the gradient.
  @end{short}
  @see-class{gsk:conic-gradient-node}
  @see-symbol{graphene:point-t}"
  (node render-node))

(export 'conic-gradient-node-center)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_rotation
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_rotation"
               conic-gradient-node-rotation) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @sym{gsk:conic-gradient-node} instance}
  @return{The single float for the rotation for the conic gradient.}
  @short{Retrieves the rotation for the conic gradient in degrees.}
  @see-class{gsk:conic-gradient-node}"
  (node render-node))

(export 'conic-gradient-node-rotation)

;;; ----------------------------------------------------------------------------
;;; GskBorderNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type border-node (render-node)
  ()
  (:simple-parser border-node))

#+liber-documentation
(setf (liber:alias-for-class 'border-node)
      "GskRenderNode"
      (documentation 'border-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a border.
  @end{short}
  @see-constructor{gsk:border-node-new}
  @see-class{gsk:render-node}")

(export 'border-node)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_new" %border-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (width (:pointer :float))
  (color :pointer))

(defun border-node-new (outline widths colors)
 #+liber-documentation
 "@version{2025-08-02}
  @argument[outline]{a @sym{gsk:rounded-rect} instance describing the outline
    of the border}
  @argument[widths]{a list with 4 single floats for the stroke width of the
    border on the top, right, bottom, and left side respectively}
  @argument[colors]{a list with 4 @class{gdk:rgba} instances for the color
    used on the top, right, bottom, and left side}
  @return{The @class{gsk:border-node} instance.}
  @begin{short}
    Creates a border render node that will stroke a border rectangle inside the
    given @arg{outline}.
  @end{short}
  The 4 sides of the border can have different widths and colors.
  @see-class{gsk:border-node}
  @see-class{gdk:rgba}
  @see-symbol{gsk:rounded-rect}"
  (cffi:with-foreign-object (widths-ptr :float 4)
    (iter (for i from 0 below 4)
          (for width in widths)
          (setf (cffi:mem-aref widths-ptr :float i)
                (coerce width 'single-float)))
    (glib:with-gboxed-array (n-colors colors-ptr gdk:rgba colors)
      (%border-node-new outline widths-ptr colors-ptr))))

(export 'border-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_outline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_outline" border-node-outline)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2025-08-02}
  @argument[node]{a @class{gsk:border-node} instance}
  @return{The @sym{gsk:rounded-rect} instance for the outline of the border.}
  @begin{short}
    Retrieves the outline of the border.
  @end{short}
  @see-class{gsk:border-node}
  @see-symbol{gsk:rounded-rect}"
  (node border-node))

(export 'border-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_widths
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_widths" %border-node-widths)
    (:pointer :float)
  (node border-node))

(defun border-node-widths (node)
 #+liber-documentation
 "@version{2025-08-03}
  @argument[node]{a @class{gsk:border-node} instance for a border}
  @begin{return}
    The list with 4 single floats for the top, right, bottom, and left stroke
    width of the border, respectively.
  @end{return}
  @begin{short}
    Retrieves the stroke widths of the border.
  @end{short}
  @see-class{gsk:border-node}"
  (let ((widths-ptr (%border-node-widths node)))
    (iter (for i from 0 below 4)
          (collect (cffi:mem-aref widths-ptr :float i)))))

(export 'border-node-widths)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_colors
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_colors" %border-node-colors) :pointer
  (node border-node))

(defun border-node-colors (node)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:border-node} instance for a border}
  @begin{return}
    The list with 4 @class{gdk:rgba} instances for the top, right, bottom, and
    left color of the border.
  @end{return}
  @begin{short}
    Retrieves the colors of the border.
  @end{short}
  @see-class{gsk:border-node}
  @see-class{gdk:rgba}"
  (let ((colors-ptr (%border-node-colors node)))
    (iter (for i from 0 below 4)
          (for ptr = (cffi:mem-aptr colors-ptr '(:struct gdk::rgba-cstruct) i))
          (collect (cffi:convert-from-foreign ptr
                                              '(g:boxed gdk:rgba))))))

(export 'border-node-colors)

;;; ----------------------------------------------------------------------------
;;; GskTextureNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type texture-node (render-node)
  ()
  (:simple-parser texture-node))

#+liber-documentation
(setf (liber:alias-for-class 'texture-node)
      "GskRenderNode"
      (documentation 'texture-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for a @class{gdk:texture} object.
  @end{short}
  @see-constructor{gsk:texture-node-new}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}")

(export 'texture-node)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_texture_node_new" texture-node-new) render-node
 #+liber-documentation
 "@version{2026-02-07}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the rectangle to
    render the texture into}
  @return{The new @sym{gsk:texture-node} instance.}
  @begin{short}
    Creates a render node that will render the given texture into the area
    given by @arg{bounds}.
  @end{short}
  @see-class{gsk:texture-node}
  @see-class{gdk:texture}
  @see-symbol{graphene:rect-t}"
  (texture (g:object gdk:texture))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'texture-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_node_get_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_texture_node_get_texture" texture-node-texture)
    (g:object gdk:texture)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:texture-node} instance}
  @return{The @class{gdk:texture} object.}
  @short{Retrieves the texture used when creating this render node.}
  @see-class{gsk:texture-node}
  @see-class{gdk:texture}"
  (node render-node))

(export 'texture-node-texture)

;;; ----------------------------------------------------------------------------
;;; GskInsetShadowNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type inset-shadow-node (render-node)
  ()
  (:simple-parser inset-shadow-node))

#+liber-documentation
(setf (liber:alias-for-class 'inset-shadow-node)
      "GskRenderNode"
      (documentation 'inset-shadow-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for an inset shadow.
  @end{short}
  @see-constructor{gsk:inset-shadow-node-new}
  @see-class{gsk:render-node}")

(export 'inset-shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_new" %inset-shadow-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(defun inset-shadow-node-new (outline color dx dy spread radius)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[outline]{a @sym{gsk:rounded-rect} instance for the outline of the
    region containing the shadow}
  @argument[color]{a @class{gdk:rgba} instance for the color of the shadow}
  @argument[dx]{a number coerced to a single float for the horizontal offset
    of the shadow}
  @argument[dy]{a number coerced to a single float for the vertical offset
    of the shadow}
  @argument[spread]{a number coerced to a single float how far the shadow
    spreads towards the inside}
  @argument[radius]{a number coerced to a single float how much blur to apply
    to the shadow}
  @return{The @class{gsk:inset-shadow-node} instance.}
  @begin{short}
    Creates a render node that will render an inset shadow into the box given
    by @arg{outline}.
  @end{short}
  @see-class{gsk:inset-shadow-node}
  @see-class{gdk:rgba}
  @see-symbol{gsk:rounded-rect}"
  (%inset-shadow-node-new outline color
                          (coerce dx 'single-float)
                          (coerce dy 'single-float)
                          (coerce spread 'single-float)
                          (coerce radius 'single-float)))

(export 'inset-shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_outline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_outline" inset-shadow-node-outline)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The @sym{gsk:rounded-rect} instance for the rounded rectangle.}
  @short{Retrieves the outline rectangle of the inset shadow.}
  @see-class{gsk:inset-shadow-node}
  @see-symbol{gsk:rounded-rect}"
  (node render-node))

(export 'inset-shadow-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_color" inset-shadow-node-color)
    (g:boxed gdk:rgba)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The @class{gdk:rgba} instance for the color of the shadow.}
  @short{Retrieves the color of the inset shadow.}
  @see-class{gsk:inset-shadow-node}
  @see-class{gdk:rgba}"
  (node render-node))

(export 'inset-shadow-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_dx
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_dx" inset-shadow-node-dx) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The single float for the offset, in pixels.}
  @short{Retrieves the horizontal offset of the inset shadow.}
  @see-class{gsk:inset-shadow-node}"
  (node render-node))

(export 'inset-shadow-node-dx)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_dy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_dy" inset-shadow-node-dy) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The single float for the offset, in pixels.}
  @short{Retrieves the vertical offset of the inset shadow.}
  @see-class{gsk:inset-shadow-node}"
  (node render-node))

(export 'inset-shadow-node-dy)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_spread
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_spread" inset-shadow-node-spread)
    :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The single float for the size of the shadow, in pixels.}
  @short{Retrieves how much the shadow spreads inwards.}
  @see-class{gsk:inset-shadow-node}"
  (node render-node))

(export 'inset-shadow-node-spread)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_blur_radius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_blur_radius"
               inset-shadow-node-blur-radius) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:inset-shadow-node} instance}
  @return{The single float for the blur radius, in pixels.}
  @short{Retrieves the blur radius to apply to the shadow.}
  @see-class{gsk:inset-shadow-node}"
  (node render-node))

(export 'inset-shadow-node-blur-radius)

;;; ----------------------------------------------------------------------------
;;; GskOutsetShadowNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type outset-shadow-node (render-node)
  ()
  (:simple-parser outset-shadow-node))

#+liber-documentation
(setf (liber:alias-for-class 'outset-shadow-node)
      "GskRenderNode"
      (documentation 'outset-shadow-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node for an outset shadow.
  @end{short}
  @see-constructor{gsk:outset-shadow-node-new}
  @see-class{gsk:render-node}")

(export 'outset-shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_new" %outset-shadow-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(defun outset-shadow-node-new (outline color dx dy spread radius)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[outline]{a @sym{gsk:rounded-rect} instance for the outline of
    the region containing the shadow}
  @argument[color]{a @class{gdk:rgba} instance for the color of the shadow}
  @argument[dx]{a number coerced to a single float for the horizontal offset
    of the shadow}
  @argument[dy]{a number coerced to a single float for the vertical offset
    of the shadow}
  @argument[spread]{a number coerced to a single float how far the shadow
    spreads towards the inside}
  @argument[radius]{a number coerced to a single float how much blur to apply
    to the shadow}
  @return{The @class{gsk:outset-shadow-node} instance.}
  @begin{short}
    Creates a render node that will render an outset shadow into the box given
    by @arg{outline}.
  @end{short}
  @see-class{gsk:outset-shadow-node}
  @see-class{gdk:rgba}
  @see-symbol{gsk:rounded-rect}"
  (%outset-shadow-node-new outline
                           color
                           (coerce dx 'single-float)
                           (coerce dy 'single-float)
                           (coerce spread 'single-float)
                           (coerce radius 'single-float)))

(export 'outset-shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_outline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_outline" outset-shadow-node-outline)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The @sym{gsk:rounded-rect} instance for the rounded rectangle.}
  @short{Retrieves the outline rectangle of the outset shadow.}
  @see-class{gsk:outset-shadow-node}
  @see-symbol{gsk:rounded-rect}"
  (node render-node))

(export 'outset-shadow-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_color" outset-shadow-node-color)
    (g:boxed gdk:rgba)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The @class{gdk:rgba} instance for the color of the shadow.}
  @short{Retrieves the color of the outset shadow.}
  @see-class{gsk:outset-shadow-node}
  @see-class{gdk:rgba}"
  (node render-node))

(export 'outset-shadow-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_dx
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_dx" outset-shadow-node-dx) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The single float for an offset, in pixels.}
  @short{Retrieves the horizontal offset of the outset shadow.}
  @see-class{gsk:outset-shadow-node}"
  (node render-node))

(export 'outset-shadow-node-dx)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_dy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_dy" outset-shadow-node-dy) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The single float for an offset, in pixels.}
  @short{Retrieves the vertical offset of the outset shadow.}
  @see-class{gsk:outset-shadow-node}"
  (node render-node))

(export 'outset-shadow-node-dy)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_spread
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_spread" outset-shadow-node-spread)
    :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The single float for the size of the shadow, in pixels.}
  @short{Retrieves how much the shadow spreads inwards.}
  @see-class{gsk:outset-shadow-node}"
  (node render-node))

(export 'outset-shadow-node-spread)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_blur_radius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_blur_radius"
               outset-shadow-node-blur-radius) :float
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:outset-shadow-node} instance}
  @return{The float for the blur radius, in pixels.}
  @short{Retrieves the blur radius to apply to the shadow.}
  @see-class{gsk:outset-shadow-node}"
  (node render-node))

(export 'outset-shadow-node-blur-radius)

;;; ----------------------------------------------------------------------------
;;; GskTransformNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type transform-node (render-node)
  ()
  (:simple-parser transform-node))

#+liber-documentation
(setf (liber:alias-for-class 'transform-node)
      "GskRenderNode"
      (documentation 'transform-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node applying a @sym{gsk:transform} instance to its single child
    render node.
  @end{short}
  @see-constructor{gsk:transform-node-new}
  @see-class{gsk:render-node}
  @see-symbol{gsk:transform}")

(export 'transform-node)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_new" transform-node-new) render-node
 #+liber-documentation
 "@version{2026-02-07}
  @argument[child]{a @class{gsk:render-node} instance for the render node
    to transform}
  @argument[transform]{a @class{gsk:transform} instance for the transform
    to apply}
  @return{The new @class{gsk:transform-node} instance.}
  @begin{short}
    Creates a render node that will transform the given @arg{child} with the
    given @arg{transform}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-class{gsk:render-node}
  @see-class{gsk:transform}"
  (child render-node)
  (transform (g:boxed transform)))

(export 'transform-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_get_child" transform-node-child) render-node
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:transform-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child render node that is
    getting transformed.
  @end{return}
  @begin{short}
    Gets the child render node that is getting transformed by the given
    @arg{node}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-symbol{gsk:render-node}"
  (node render-node))

(export 'transform-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_get_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_get_transform" transform-node-transform)
    (g:boxed transform)
 #+liber-documentation
 "@version{2026-02-07}
  @argument[node]{a @class{gsk:transform-node} instance}
  @return{The @sym{gsk:transform} instance.}
  @begin{short}
    Retrieves the @sym{gsk:transform} instance used by @arg{node}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-symbol{gsk:transform}"
  (node render-node))

(export 'transform-node-transform)

;;; ----------------------------------------------------------------------------
;;; GskOpacityNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type opacity-node (render-node)
  ()
  (:simple-parser opacity-node))

#+liber-documentation
(setf (liber:alias-for-class 'opacity-node)
      "GskRenderNode"
      (documentation 'opacity-node 'type)
 "@version{2026-02-07}
  @begin{short}
    The render node controlling the opacity of its single child render node.
  @end{short}
  @see-constructor{gsk:opacity-node-new}
  @see-class{gsk:render-node}")

(export 'opacity-node)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_new" %opacity-node-new) render-node
  (child render-node)
  (opacity :float))

(defun opacity-node-new (child opacity)
 #+liber-documentation
 "@version{2026-02-09}
  @argument[child]{a @class{gsk:render-node} instance}
  @argument[opacity]{a number coerced to a single float for the opacity to
    apply}
  @return{The new @class{gsk:opacity-node} instance.}
  @begin{short}
    Creates a render node that will draw the child with reduced @arg{opacity}.
  @end{short}
  @see-class{gsk:opacity-node}"
  (%opacity-node-new child (coerce opacity 'single-float)))

(export 'opacity-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_get_child" opacity-node-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:opacity-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child that is getting
    opacityed.
  @end{return}
  @begin{short}
    Gets the child node that is getting opacityed by the given @arg{node}.
  @end{short}
  @see-class{gsk:opacity-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'opacity-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_get_opacity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_get_opacity" opacity-node-opacity) :float
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:opacity-node} instance}
  @return{The single float for the opacity factor.}
  @short{Gets the transparency factor for an opacity node.}
  @see-class{gsk:opacity-node}"
  (node render-node))

(export 'opacity-node-opacity)

;;; ----------------------------------------------------------------------------
;;; GskColorMatrixNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type color-matrix-node (render-node)
  ()
  (:simple-parser color-matrix-node))

#+liber-documentation
(setf (liber:alias-for-class 'color-matrix-node)
      "GskRenderNode"
      (documentation 'color-matrix-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node controlling the color matrix of its single child render
    node.
  @end{short}
  @see-constructor{gsk:color-matrix-node-new}
  @see-class{gsk:render-node}")

(export 'color-matrix-node)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_new" color-matrix-node-new) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[child]{a @class{gsk:render-node} instance for the child render node
    to draw}
  @argument[matrix]{a @sym{graphene:matrix-t} instance for the matrix to apply}
  @argument[offset]{a @sym{graphene:vec4-t} instance for the values to add to
    the color}
  @return{The new @class{gsk:color-matrix-node} instance.}
  @begin{short}
    Creates a render node that will draw the child render node with
    @arg{matrix}.
  @end{short}
  In particular, the node will transform colors by applying
  @begin{pre}
pixel = transpose(matrix) * pixel + offset
  @end{pre}
  for every pixel. The transformation operates on unpremultiplied colors, with
  color components ordered R, G, B, A.
  @see-class{gsk:color-matrix-node}
  @see-class{gsk:render-node}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec4-t}"
  (child render-node)
  (matrix (:pointer (:struct graphene:matrix-t)))
  (offset (:pointer (:struct graphene:vec4-t))))

(export 'color-matrix-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_child" color-matrix-node-child)
    render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child render node that is
    getting its colors modified.
  @end{return}
  @begin{short}
    Gets the child render node that is getting its colors modified by the given
    @arg{node}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'color-matrix-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_color_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_color_matrix"
               color-matrix-node-color-matrix)
    (:pointer (:struct graphene:matrix-t))
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @return{The @sym{graphene:matrix-t} instance for a 4x4 color matrix.}
  @begin{short}
    Retrieves the color matrix used by @arg{node}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-symbol{graphene:matrix-t}"
  (node render-node))

(export 'color-matrix-node-color-matrix)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_color_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_color_offset"
               color-matrix-node-color-offset)
    (:pointer (:struct graphene:vec4-t))
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @return{The @sym{graphene:vec4-t} instance for a color vector.}
  @begin{short}
    Retrieves the color offset used by @arg{node}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-symbol{graphene:vec4-t}"
  (node render-node))

(export 'color-matrix-node-color-offset)

;;; ----------------------------------------------------------------------------
;;; GskRepeatNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type repeat-node (render-node)
  ()
  (:simple-parser repeat-node))

#+liber-documentation
(setf (liber:alias-for-class 'repeat-node)
      "GskRenderNode"
      (documentation 'repeat-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node repeating its single child render node.
  @end{short}
  @see-constructor{gsk:repeat-node-new}
  @see-class{gsk:render-node}")

(export 'repeat-node)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_new" %repeat-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (child render-node)
  (child-bounds (:pointer (:struct graphene:rect-t))))

(defun repeat-node-new (bounds child &optional child-bounds)
 #+liber-documentation
 "@version{2026-02-09}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the bounds of the
    area to be painted}
  @argument[child]{a @class{gsk:render-node} instance for the child to repeat}
  @argument[child-bounds]{a @sym{graphene:rect-t} instance for the area of the
    child to repeat or @code{nil} to use the child's bounds}
  @return{The new @class{gsk:repeat-node} instance.}
  @begin{short}
    Creates a render node that will repeat the drawing of child across the
    given @arg{bounds}.
  @end{short}
  @see-class{gsk:repeat-node}
  @see-class{gsk:render-node}
  @see-symbol{graphene:rect-t}"
  (%repeat-node-new bounds child (or child-bounds (cffi:null-pointer))))

(export 'repeat-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_get_child" repeat-node-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @sym{gsk:repeat-node} instance}
  @return{The @class{gsk:render-node} instance for the child.}
  @short{Retrieves the child of @arg{node}.}
  @see-class{gsk:repeat-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'repeat-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_get_child_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_get_child_bounds" repeat-node-child-bounds)
    (:pointer (:struct graphene:rect-t))
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:repeat-node} instance}
  @return{The @sym{graphene:rect-t} instance for the bounding rectangle.}
  @short{Retrieves the bounding rectangle of the child of @arg{node}.}
  @see-class{gsk:repeat-node}
  @see-symbol{graphene:rect-t}"
  (node render-node))

(export 'repeat-node-child-bounds)

;;; ----------------------------------------------------------------------------
;;; GskClipNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type clip-node (render-node)
  ()
  (:simple-parser clip-node))

#+liber-documentation
(setf (liber:alias-for-class 'clip-node)
      "GskRenderNode"
      (documentation 'clip-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node applying a rectangular clip to its single child render node.
  @end{short}
  @see-constructor{gsk:clip-node-new}
  @see-class{gsk:render-node}")

(export 'clip-node)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_new" clip-node-new) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[child]{a @class{gsk:render-node} instance for the render node to
    draw}
  @argument[clip]{a @sym{graphene:rect-t} instance for the clip to apply}
  @return{The new @class{gsk:render-node} instance.}
  @begin{short}
    Creates a render node that will clip the child render node to the area
    given by @arg{clip}.
  @end{short}
  @see-class{gsk:clip-node}
  @see-class{gsk:render-node}
  @see-symbol{graphene:rect-t}"
  (child render-node)
  (clip (:pointer (:struct graphene:rect-t))))

(export 'clip-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_get_child" clip-node-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:clip-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child render node that is
    getting clipped.
  @end{return}
  @begin{short}
    Gets the child render node that is getting clipped by the given @arg{node}.
  @end{short}
  @see-class{gsk:clip-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'clip-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_get_clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_get_clip" clip-node-clip)
    (:pointer (:struct graphene:rect-t))
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:clip-node} instance}
  @return{The @sym{graphene:rect-t} instance for a clip rectangle.}
  @begin{short}
    Retrieves the clip rectangle for @arg{node}.
  @end{short}
  @see-class{gsk:clip-node}
  @see-symbol{graphene:rect-t}"
  (node render-node))

(export 'clip-node-clip)

;;; ----------------------------------------------------------------------------
;;; GskRoundedClipNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type rounded-clip-node (render-node)
  ()
  (:simple-parser rounded-clip-node))

#+liber-documentation
(setf (liber:alias-for-class 'rounded-clip-node)
      "GskRenderNode"
      (documentation 'rounded-clip-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node applying a rounded rectangle clip to its single child
    render node.
  @end{short}
  @see-constructor{gsk:rounded-clip-node-new}
  @see-class{gsk:render-node}")

(export 'rounded-clip-node)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_new" rounded-clip-node-new) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[child]{a @class{gsk:render-node} instance for the node to draw}
  @argument[clip]{a @sym{gsk:rounded-rect} instance for the clip to apply}
  @return{The new @class{gsk:rounded-clip-node} instance.}
  @begin{short}
    Creates a render node that will clip the child node to the area given by
    @arg{clip}.
  @end{short}
  @see-class{gsk:rounded-clip-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:rounded-rect}"
  (child render-node)
  (clip (:pointer (:struct rounded-rect))))

(export 'rounded-clip-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_get_child" rounded-clip-node-child)
    render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:rounded-clip-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child that is getting clipped.
  @end{return}
  @begin{short}
    Gets the child node that is getting clipped by the given @arg{node}.
  @end{short}
  @see-class{gsk:rounded-clip-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'rounded-clip-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_get_clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_get_clip" rounded-clip-node-clip)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:rounded-clip-node} instance}
  @return{The @sym{gsk:rounded-rect} instance for the rounded rectangle.}
  @begin{short}
    Retrievs the rounded rectangle used to clip the contents of @arg{node}.
  @end{short}
  @see-class{gsk:rounded-clip-node}
  @see-symbol{gsk:rounded-rect}"
  (node render-node))

(export 'rounded-clip-node-clip)

;;; ----------------------------------------------------------------------------
;;; GskShadowNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type shadow-node (render-node)
  ()
  (:simple-parser shadow-node))

#+liber-documentation
(setf (liber:alias-for-class 'shadow-node)
      "GskRenderNode"
      (documentation 'shadow-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node drawing one or more shadows behind its single child render
    node.
  @end{short}
  @see-constructor{gsk:shadow-node-new}
  @see-class{gsk:render-node}")

(export 'shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_new" %shadow-node-new) render-node
  (child render-node)
  (shadows :pointer)
  (n-shadows :size))

(defun shadow-node-new (child shadows)
 #+liber-documentation
 "@version{2026-02-09}
  @argument[child]{a @class{gsk:render-node} instance for the node to draw}
  @argument[shadows]{a list of the form @code{'((color1 dx1 dy1 radius1)
    (color2 dx2 dy2 radius2) ...)} with the shadows to apply}
  @return{The new @class{gsk:shadow-node} instance.}
  @begin{short}
    Creates a render node that will draw a child with the given shadows below
    it.
  @end{short}
  @see-class{gsk:shadow-node}
  @see-class{gsk:render-node}"
  (let ((n-shadows (length shadows))
        (cstruct-size (cffi:foreign-type-size '(:struct gdk::rgba-cstruct))))
    (cffi:with-foreign-object (shadows-ptr '(:struct %shadow) n-shadows)
      (iter (for i from 0 below n-shadows)
            (for (color dx dy radius) in shadows)
            (for ptr = (cffi:mem-aptr shadows-ptr '(:struct %shadow) i))
            (glib::copy-boxed-slots-to-foreign color ptr 'gdk:rgba)
            (cffi:incf-pointer ptr cstruct-size)
            (setf (cffi:mem-ref ptr :float) (coerce dx 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce dy 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce radius 'single-float)))
      (%shadow-node-new child shadows-ptr n-shadows))))

(export 'shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_shadow
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_shadow" %shadow-node-shadow) :pointer
  (node render-node)
  (index :size))

(defun shadow-node-shadow (node index)
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:shadow-node} instance}
  @argument[index]{an unsigned integer for the given index}
  @begin{return}
    The list of the form @code{'(color dx dy radius)} with the shadow data.
  @end{return}
  @short{Retrieves the shadow data at the given index.}
  @see-class{gsk:shadow-node}"
  (let ((ptr (%shadow-node-shadow node index))
        (cstruct-size (cffi:foreign-type-size '(:struct gdk::rgba-cstruct)))
        (float-size (cffi:foreign-type-size :float)))
    (list (cffi:convert-from-foreign ptr '(g:boxed gdk:rgba))
          (cffi:mem-ref (cffi:incf-pointer ptr cstruct-size) :float)
          (cffi:mem-ref (cffi:incf-pointer ptr float-size) :float)
          (cffi:mem-ref (cffi:incf-pointer ptr float-size) :float))))

(export 'shadow-node-shadow)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_n_shadows
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_n_shadows" shadow-node-n-shadows) :size
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:shadow-node} instance}
  @return{The unsigned integer for the number of shadows.}
  @short{Retrieves the number of shadows in @arg{node}.}
  @see-class{gsk:shadow-node}"
  (node render-node))

(export 'shadow-node-n-shadows)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_child" shadow-node-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:shadow-node} instance}
  @return{The @class{gsk:render-node} instance for the child render node.}
  @short{Retrieves the child render node of the shadow node.}
  @see-class{gsk:shadow-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'shadow-node-child)

;;; ----------------------------------------------------------------------------
;;; GskBlendNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type blend-node (render-node)
  ()
  (:simple-parser blend-node))

#+liber-documentation
(setf (liber:alias-for-class 'blend-node)
      "GskRenderNode"
      (documentation 'blend-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node applying a blending function between its two child nodes.
  @end{short}
  @see-constructor{gsk:blend-node-new}
  @see-class{gsk:render-node}")

(export 'blend-node)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_new" blend-node-new) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[bottom]{a @class{gsk:render-node} instance for the bottom node to
    be drawn}
  @argument[top]{a @class{gsk:render-node} instance for the top node to
    be blended onto the bottom node}
  @argument[mode]{a @sym{gsk:blend-mode} value for the blend mode to use}
  @return{The new @class{gsk:blend-node} instance.}
  @begin{short}
    Creates a render node that will use @arg{mode} to blend the top node onto
    the bottom node.
  @end{short}
  @see-class{gsk:blend-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:blend-mode}"
  (bottom render-node)
  (top render-node)
  (mode blend-mode))

(export 'blend-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_bottom_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_bottom_child" blend-node-bottom-child)
    render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{The @class{gsk:render-node} instance for the bottom child node.}
  @begin{short}
    Retrieves the bottom render node child of @arg{node}.
  @end{short}
  @see-class{gsk:blend-mode}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'blend-node-bottom-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_top_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_top_child" blend-node-top-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{The @class{gsk:render-node} instance for the top child node.}
  @begin{short}
    Retrieves the top render node child of @arg{node}.
  @end{short}
  @see-class{gsk:blend-mode}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'blend-node-top-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_blend_mode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_blend_mode" blend-node-blend-mode) blend-mode
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{The @sym{gsk:blend-mode} value for the blend mode.}
  @begin{short}
    Retrieves the blend mode used by @arg{node}.
  @end{short}
  @see-class{gsk:blend-node}
  @see-symbol{gsk:blend-mode}"
  (node render-node))

(export 'blend-node-blend-mode)

;;; ----------------------------------------------------------------------------
;;; GskCrossFadeNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type cross-fade-node (render-node)
  ()
  (:simple-parser cross-fade-node))

#+liber-documentation
(setf (liber:alias-for-class 'cross-fade-node)
      "GskRenderNode"
      (documentation 'cross-fade-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node cross fading between two child render nodes.
  @end{short}
  @see-constructor{gsk:cross-fade-node-new}
  @see-class{gsk:render-node}")

(export 'cross-fade-node)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_new" %cross-fade-node-new) render-node
  (start render-node)
  (end render-node)
  (progress :float))

(defun cross-fade-node-new (start end progress)
 #+liber-documentation
 "@version{2026-02-09}
  @argument[start]{a @class{gsk:render-node} instance for the start render
    node to be drawn}
  @argument[end]{a @class{gsk:render-node} instance for the render node to be
    cross-fadeed onto the start node}
  @argument[progress]{a number coerced to a single float how far the fade has
    progressed from start to end, the value will be clamped to the range
    [0 ... 1]}
  @return{The new @class{gsk:cross-fade-node} instance.}
  @begin{short}
    Creates a render node that will do a cross-fade between @arg{start} and
    @arg{end}.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (%cross-fade-node-new start end (coerce progress 'single-float)))

(export 'cross-fade-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_start_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_start_child"
               cross-fade-node-start-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{The @class{gsk:render-node} instance.}
  @begin{short}
    Retrieves the child render node at the beginning of the cross-fade.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'cross-fade-node-start-child)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_end_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_end_child"
               cross-fade-node-end-child) render-node
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{The @class{gsk:render-node} instance.}
  @begin{short}
    Retrieves the child render node at the end of the cross-fade.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'cross-fade-node-end-child)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_progress
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_progress" cross-fade-node-progress)
    :float
 #+liber-documentation
 "@version{2026-02-09}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{The single float for the progress value, between 0 and 1.}
  @short{Retrieves the progress value of the cross fade node.}
  @see-class{gsk:cross-fade-node}"
  (node render-node))

(export 'cross-fade-node-progress)

;;; ----------------------------------------------------------------------------
;;; GskTextNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type text-node (render-node)
  ()
  (:simple-parser text-node))

#+liber-documentation
(setf (liber:alias-for-class 'text-node)
      "GskRenderNode"
      (documentation 'text-node 'type)
 "@version{2026-02-09}
  @begin{short}
    The render node drawing a set of glyphs.
  @end{short}
  @see-constructor{gsk:text-node-new}
  @see-class{gsk:render-node}")

(export 'text-node)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_new" text-node-new) render-node
 #+liber-documentation
 "@version{2026-02-10}
  @argument[font]{a @class{pango:font} object containing the glyphs}
  @argument[glyphs]{a @class{pango:glyph-string} instance to render}
  @argument[color]{a @class{gdk:rgba} instance for the foreground color to
    render with}
  @argument[offset]{a @sym{graphene:point-t} instance for the offset of the
    baseline}
  @return{The new @class{gsk:text-node} instance.}
  @begin{short}
    Creates a render node that renders the given glyphs.
  @end{short}
  Note that @arg{color} may not be used if the font contains color glyphs.
  @see-class{gsk:text-node}
  @see-class{pango:font}
  @see-class{pango:glyph-string}
  @see-class{gdk:rgba}
  @see-symbol{graphene:point-t}"
  (font (g:object pango:font))
  (glyphs (g:boxed pango:glyph-string))
  (color (g:boxed gdk:rgba))
  (offset (:pointer (:struct graphene:point-t))))

(export 'text-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_font" text-node-font) (g:object pango:font)
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:text-node} instance}
  @return{The @class{pango:font} object for the font.}
  @short{Returns the font used by the text node.}
  @see-class{gsk:text-node}
  @see-class{pango:font}"
  (node render-node))

(export 'text-node-font)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_glyphs ()
;;;
;;; const PangoGlyphInfo *
;;; gsk_text_node_get_glyphs (GskRenderNode *node,
;;;                           guint *n_glyphs);
;;;
;;; Retrieves the glyph information in the node .
;;;
;;; node
;;;     a text GskRenderNode.
;;;
;;; n_glyphs
;;;     the number of glyphs returned.
;;;
;;; Returns
;;;     the glyph information.
;;; ----------------------------------------------------------------------------

;; TODO: The PangoGlyphInfo structure is not implemented. We return the pointer
;; to the instance. Improve the implementation.

#+nil
(cffi:defcfun ("gsk_text_node_get_glyphs" text-node-glyphs) :pointer
  (node render-node)
  (n-glyphs :uint))

#+nil
(export 'text-node-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_color" text-node-color) (g:boxed gdk:rgba)
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:text-node} instance}
  @return{The @class{gdk:rgba} instance for the text color.}
  @short{Retrieves the color used by the text node.}
  The value returned by this function will not be correct if the render node
  was created for a non-sRGB color.
  @see-class{gsk:text-node}
  @see-class{gdk:rgba}"
  (node render-node))

(export 'text-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_has_color_glyphs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_has_color_glyphs" text-node-has-color-glyphs)
    :boolean
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:text-node} instance}
  @return{@em{True} if the text node has color glyphs.}
  @short{Checks whether the text node has color glyphs.}
  @see-class{gsk:text-node}"
  (node render-node))

(export 'text-node-has-color-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_num_glyphs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_num_glyphs" text-node-num-glyphs) :uint
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:text-node} instance}
  @return{The unsigned integer for the number of glyphs.}
  @short{Retrieves the number of glyphs in the text node.}
  @see-class{gsk:text-node}"
  (node render-node))

(export 'text-node-num-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_offset" text-node-offset)
    (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[node]{a @class{gsk:text-node} instance}
  @begin{return}
    The @sym{graphene:point-t} instance for the horizontal and vertical offsets.
  @end{return}
  @short{Retrieves the offset applied to the text node.}
  @see-class{gsk:text-node}
  @see-symbol{grapene:point-t}"
  (node render-node))

(export 'text-node-offset)

;;; ----------------------------------------------------------------------------
;;; GskBlurNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type blur-node (render-node)
  ()
  (:simple-parser blur-node))

#+liber-documentation
(setf (liber:alias-for-class 'blur-node)
      "GskRenderNode"
      (documentation 'blur-node 'type)
 "@version{2026-02-10}
  @begin{short}
    The render node applying a blur effect between its single child node.
  @end{short}
  @see-constructor{gsk:blur-node-new}
  @see-class{gsk:render-node}")

(export 'blur-node)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_new" %blur-node-new) render-node
  (child render-node)
  (radius :float))

(defun blur-node-new (child radius)
 #+liber-documentation
 "@version{2026-02-10}
  @argument[child]{a @class{gsk:render-node} instance for the child to blur}
  @argument[radius]{a number coerced to a single float for the blur radius}
  @return{The new @class{gsk:blur-node} instance.}
  @begin{short}
    Creates a render node that blurs the child render node.
  @end{short}
  @see-class{gsk:blur-node}
  @see-class{gsk:render-node}"
  (%blur-node-new child (coerce radius 'single-float)))

(export 'blur-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_get_child" blur-node-child) render-node
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:blur-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the blurred child render node.
  @end{return}
  @begin{short}
    Retrieves the child render node of the blur render node.
  @end{short}
  @see-class{gsk:blur-node}
  @see-class{gsk:render-node}"
  (node blur-node))

(export 'blur-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_get_radius
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_get_radius" blur-node-radius) :float
 #+liber-documentation
 "@version{2026-02-10}
  @argument[node]{a @class{gsk:blur-node} instance}
  @return{The single float for the blur radius.}
  @begin{short}
    Retrieves the blur radius of the blur render node.
  @end{short}
  @see-class{gsk:blur-node}"
  (node blur-node))

(export 'blur-node-radius)

;;; ----------------------------------------------------------------------------
;;; GskDebugNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type debug-node (render-node)
  ()
  (:simple-parser debug-node))

#+liber-documentation
(setf (liber:alias-for-class 'debug-node)
      "GskRenderNode"
      (documentation 'debug-node 'type)
 "@version{2026-02-10}
  @begin{short}
    The render node that emits a debugging message when drawing its child
    render node.
  @end{short}
  @see-constructor{gsk:debug-node-new}
  @see-class{gsk:render-node}")

(export 'debug-node)

;;; ----------------------------------------------------------------------------
;;; gsk_debug_node_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_new" debug-node-new) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[child]{a @class{gsk:render-node} instance for the child to add
    debug info for}
  @argument[message]{a string for the debug message}
  @return{The new @class{gsk:debug-node} instance.}
  @begin{short}
    Creates a render node that will add debug information about the given
    @arg{child}.
  @end{short}
  Adding this node has no visual effect.
  @see-class{gsk:debug-node}
  @see-class{gsk:render-node}"
  (child render-node)
  (message :string))

(export 'debug-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_debug_node_get_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_get_child" debug-node-child) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a debug @class{gsk:debug-node} instance}
  @return{The child @class{gsk:render-node} instance.}
  @begin{short}
    Gets the child node that is getting drawn by the given @arg{node}.
  @end{short}
  @see-class{gsk:debug-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'debug-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_debug_node_get_message
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_get_message" debug-node-message) :string
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:debug-node} instance}
  @return{The string with the debug message.}
  @begin{short}
    Gets the debug message that was set on this render node.
  @end{short}
  @see-class{gsk:debug-node}"
  (node render-node))

(export 'debug-node-message)

;;; ----------------------------------------------------------------------------
;;; GskGLShaderNode                                         Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_new                                  Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_n_children                       Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_child                            Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_args                             Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_shader                           Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskTextureScaleNode                                     Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:define-foreign-type texture-scale-node (render-node)
  ()
  (:simple-parser texture-scale-node))

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-class 'texture-scale-node)
      "GskRenderNode"
      (documentation 'texture-scale-node 'type)
 "@version{2026-02-11}
  @begin{short}
    The render node for a @class{gdk:texture} object.
  @end{short}

  Since 4.10
  @see-constructor{gsk:texture-scale-node-new}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}")

#+gtk-4-10
(export 'texture-scale-node)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_new                              Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_texture_scale_node_new" texture-scale-node-new) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[texture]{a @class{gdk:texture} object to scale}
  @argument[bounds]{a @sym{graphene:rect-t} instance for the size of the
    texture to scale to}
  @argument[filter]{a @sym{gsk:scaling-filter} value how to scale the texture}
  @return{The new @class{gsk:texture-scale-node} instance.}
  @begin{short}
    Creates a node that scales the texture to the size given by the bounds
    using the filter and then places it at the position of the bounds.
  @end{short}
  Note that further scaling and other transformations which are applied to the
  node will apply linear filtering to the resulting texture, as usual.

  This node is intended for tight control over scaling applied to a texture,
  such as in image editors and requires the application to be aware of the
  whole render tree as further transforms may be applied that conflict with the
  desired effect of this node.

  Since 4.10
  @see-class{gsk:texture-scale-node}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}
  @see-symbol{graphene:rect-t}
  @see-symbol{gsk:scaling-filter}"
  (texture (g:object gdk:texture))
  (bounds (:pointer (:struct graphene:rect-t)))
  (filter scaling-filter))

#+gtk-4-10
(export 'texture-scale-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_get_filter                       Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_texture_scale_node_get_filter" texture-scale-node-filter)
    scaling-filter
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:texture-scale-node} instance}
  @return{The @sym{gsk:scaling-filter} value.}
  @begin{short}
    Retrieves the @sym{gsk:scaling-filter} value used when creating the render
    node.
  @end{short}

  Since 4.10
  @see-class{gsk:texture-scale-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:scaling-filter}"
  (node render-node))

#+gtk-4-10
(export 'texture-scale-node-filter)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_get_texture                      Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_texture_scale_node_get_texture" texture-scale-node-texture)
    render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:texture-scale-node} instance}
  @return{The @class{gdk:texture} object.}
  @begin{short}
    Retrieves the texture used when creating the render node.
  @end{short}

  Since 4.10
  @see-class{gsk:texture-scale-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:scaling-filter}"
  (node render-node))

#+gtk-4-10
(export 'texture-scale-node-texture)

;;; ----------------------------------------------------------------------------
;;; GskMaskNode
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:define-foreign-type mask-node (render-node)
  ()
  (:simple-parser mask-node))

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-class 'mask-node)
      "GskRenderNode"
      (documentation 'mask-node 'type)
 "@version{#2026-02-11}
  @begin{short}
    The render node masking one child node with another.
  @end{short}

  Since 4.10
  @see-constructor{gsk:mask-node-new}
  @see-class{gsk:render-node}")

#+gtk-4-10
(export 'mask-node)

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_new
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_mask_node_new" mask-node-new) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[source]{a @class{gsk:render-node} instance for the source node
    to be drawn}
  @argument[mask]{a @class{gsk:render-node} instance for the node to be used
    as mask}
  @argument[mode]{a @sym{gsk:mask-mode} value for the mask mode to use}
  @begin{short}
    Creates a render node that will mask a given node by another.
  @end{short}
  The @arg{mode} value determines how the mask values are derived from the
  colors of the mask. Applying the mask consists of multiplying the mask value
  with the alpha of the source.

  Since 4.10
  @see-class{gsk:mask-node}"
  (source render-node)
  (mask render-node)
  (mode mask-mode))

#+gtk-4-10
(export 'mask-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_mask
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_mask_node_get_mask" mask-node-mask) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:mask-node} instance}
  @return{The @class{gsk:render-node} instance for the mask child node.}
  @begin{short}
    Retrieves the @class{gsk:render-node} instance for the mask child of the
    node.
  @end{short}

  Since 4.10
  @see-class{gsk:mask-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-10
(export 'mask-node-mask)

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_mask_mode
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_mask_node_get_mask_mode" mask-node-mask-mode) mask-mode
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:mask-node} instance}
  @return{The @sym{gsk:mask-mode} value for the mask mode.}
  @begin{short}
    Retrieves the mask mode used by @arg{node}.
  @end{short}

  Since 4.10
  @see-class{gsk:mask-node}
  @see-symbol{gsk:mask-mode}"
  (node render-node))

#+gtk-4-10
(export 'mask-node-mask-mode)

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_source
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gsk_mask_node_get_source" mask-node-source) render-node
 #+liber-documentation
 "@version{#2026-02-10}
  @argument[node]{a @class{gsk:mask-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the source child of the node.
  @end{return}
  @begin{short}
    Retrieves the render node for the source child of the node.
  @end{short}

  Since 4.10
  @see-class{gsk:mask-node}"
  (node render-node))

#+gtk-4-10
(export 'mask-node-source)

;;; ----------------------------------------------------------------------------
;;; GskFillNode                                             Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:define-foreign-type fill-node (render-node)
  ()
  (:simple-parser fill-node))

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-class 'fill-node)
      "GskRenderNode"
      (documentation 'fill-node 'type)
 "@version{#2026-02-10}
  @begin{short}
    The render node filling the area given by a @class{gsk:path} instance and
    a @sym{gsk:fill-rule} value for the child node.
  @end{short}

  Since 4.14
  @see-constructor{gsk:fill-node-new}
  @see-class{gsk:render-node}
  @see-class{gsk:path}
  @see-symbol{gsk:fill-rule}")

#+gtk-4-14
(export 'fill-node)

;;; ----------------------------------------------------------------------------
;;; gsk_fill_node_new
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_fill_node_new" fill-node-new) render-node
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[child]{a @class{gsk:render-node} instance for the node to fill the
    area with}
  @argument[path]{a @class{gsk:path} instance for the path describing the area
    to fill}
  @argument[rule]{a @sym{gsk:fill-rule} value for the fill rule to use}
  @begin{short}
    Creates a @class{gsk:render-node} instance that will fill the child in the
    area given by @arg{path} and @arg{rule}.
  @end{short}

  Since 4.14
  @see-class{gsk:fill-node}
  @see-class{gsk:render-node}
  @see-class{gsk:path}
  @see-symbol{gsk:fill-rule}"
  (child render-node)
  (path (g:boxed path))
  (rule fill-rule))

#+gtk-4-14
(export 'fill-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_fill_node_get_child
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_fill_node_get_child" fill-node-child) render-node
 #+liber-documentation
 "@version{#2025-08-03}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for child that is getting drawn.
  @end{return}
  @begin{short}
    Gets the child node that is getting drawn by the given @arg{node}.
  @end{short}

  Since 4.14
  @see-class{gsk:fill-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-14
(export 'fill-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_fill_node_get_fill_rule
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_fill_node_get_fill_rule" fill-node-fill-rule) fill-rule
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @sym{gsk:fill-rule} value.}
  @begin{short}
    Retrieves the fill rule used to determine how the path is filled.
  @end{short}

  Since 4.14
  @see-class{gsk:fill-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-14
(export 'fill-node-fill-rule)

;;; ----------------------------------------------------------------------------
;;; gsk_fill_node_get_path
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_fill_node_get_path" fill-node-path) (g:boxed path)
 #+liber-documentation
 "@version{#2024-12-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:path} instance.}
  @begin{short}
    Retrieves the path used to describe the area filled with the contents of
    the node.
  @end{short}

  Since 4.14
  @see-class{gsk:fill-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-14
(export 'fill-node-path)

;;; ----------------------------------------------------------------------------
;;; GskStrokeNode                                           Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:define-foreign-type stroke-node (render-node)
  ()
  (:simple-parser stroke-node))

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-class 'stroke-node)
      "GskRenderNode"
      (documentation 'stroke-node 'type)
 "@version{2026-02-11}
  @begin{short}
    The render node that will fill the area determined by stroking the the given
    @class{gsk:path} instance using the @class{gsk:stroke} attributes.
  @end{short}

  Since 4.14
  @see-constructor{gsk:stroke-node-new}
  @see-class{gsk:render-node}
  @see-class{gsk:path}
  @see-class{gsk:stroke}")

#+gtk-4-14
(export 'stroke-node)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_node_new
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_stroke_node_new" stroke-node-new) render-node
 #+liber-documentation
 "@version{#2025-08-02}
  @argument[child]{a @class{gsk:render-node} instance for the node to stroke
    the area with}
  @argument[path]{a @class{gsk:path} instance for the path describing the area
    to stroke}
  @argument[stroke]{a @sym{gsk:stroke} value for the stroke attributes to use}
  @begin{short}
    Creates a @class{gsk:render-node} instance that will fill the outline
    generated by stroking the given @arg{path} using the attributes defined in
    @arg{stroke}.
  @end{short}
  The area is filled with child.

  Since 4.14
  @see-class{gsk:stroke-node}
  @see-class{gsk:render-node}
  @see-class{gsk:path}
  @see-symbol{gsk:stroke}"
  (child render-node)
  (path (g:boxed path))
  (stroke (g:boxed stroke)))

#+gtk-4-14
(export 'stroke-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_node_get_child
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_stroke_node_get_child" stroke-node-child) render-node
 #+liber-documentation
 "@version{#2025-08-03}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{return}
    The @class{gsk:render-node} instance for the child that is getting drawn.
  @end{return}
  @begin{short}
    Gets the child node that is getting drawn by the given @arg{node}.
  @end{short}

  Since 4.14
  @see-class{gsk:stroke-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-14
(export 'stroke-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_node_get_path
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_stroke_node_get_path" stroke-node-path) (g:boxed path)
 #+liber-documentation
 "@version{#2024-12-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:path} instance.}
  @begin{short}
    Retrieves the path that will be stroked with the contents of @arg{node}.
  @end{short}

  Since 4.14
  @see-class{gsk:stroke-node}
  @see-class{gsk:render-node}
  @see-class{gsk:path}"
  (node render-node))

#+gtk-4-14
(export 'stroke-node-path)

;;; ----------------------------------------------------------------------------
;;; gsk_stroke_node_get_stroke
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_stroke_node_get_stroke" stroke-node-stroke) (g:boxed stroke)
 #+liber-documentation
 "@version{#2024-12-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:stroke} value.}
  @begin{short}
    Retrieves the stroke attributes used in @arg{node}.
  @end{short}

  Since 4.14
  @see-class{gsk:stroke-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:stroke}"
  (node render-node))

#+gtk-4-14
(export 'stroke-node-stroke)

;;; ----------------------------------------------------------------------------
;;; GskSubsurfaceNode                                       Since 4.14
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:define-foreign-type subsurface-node (render-node)
  ()
  (:simple-parser subsurface-node))

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-class 'subsurface-node)
      "GskRenderNode"
      (documentation 'subsurface-node 'type)
 "@version{2026-02-11}
  @begin{short}
    The render node that potentially diverts a part of the scene graph to a
    subsurface.
  @end{short}

  Since 4.14
  @see-constructor{gsk:subsurface-node-new}
  @see-class{gsk:render-node}")

#+gtk-4-14
(export 'subsurface-node)

;;; ----------------------------------------------------------------------------
;;; gsk_subsurface_node_new
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_subsurface_node_new" subsurface-node-new) render-node
 #+liber-documentation
 "@version{#2024-12-02}
  @argument[child]{a @class{gsk:render-node} instance}
  @argument[subsurface]{a pointer with the subsurface to use}
  @return{The new @class{gsk:render-node} instance.}
  @begin{short}
    Creates a @class{gsk:render-node} instance that will possibly divert the
    child node to a subsurface.
  @end{short}
  @begin[Notes]{dictionary}
    Since subsurfaces are currently private, these nodes cannot currently be
    created outside of GTK. See the @class{gtk:graphics-offload} documentation.
  @end{dictionary}
  This constructor is not directly available to language bindings.

  Since 4.14
  @see-class{gsk:subsurface-node}
  @see-class{gsk:render-node}"
  (child render-node)
  (subsurface :pointer))

#+gtk-4-14
(export 'subsurface-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_subsurface_node_get_subsurface
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_subsurface_node_get_subsurface" subsurface-node-subsurface)
    :pointer
 #+liber-documentation
 "@version{#2024-12-02}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The pointer with the subsurface.}
  @begin{short}
    Gets the subsurface that was set on this node.
  @end{short}
 This function is not directly available to language bindings.

  Since 4.14
  @see-class{gsk:subsurface-node}"
  (node render-node))

#+gtk-4-14
(export 'subsurface-node-subsurface)

;;; ----------------------------------------------------------------------------
;;; gsk_subsurface_node_get_child
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gsk_subsurface_node_get_child" subsurface-node-child)
    render-node
 #+liber-documentation
 "@version{#2025-08-03}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:render-node} instance for the child.}
  @begin{short}
    Gets the child node that is getting drawn by the given @arg{node}.
  @end{short}

  Since 4.14
  @see-class{gsk:subsurface-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-14
(export 'subsurface-node-child)

;;; ----------------------------------------------------------------------------
;;; GskComponentTransfer                                    Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(glib:define-gboxed-opaque component-transfer "GskComponentTransfer"
  :export t
  :type-initializer "gsk_component_transfer_get_type"
  :alloc (error "GskComponentTransfer: Use constructor function"))

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-class 'component-transfer)
      "GBoxed"
      (documentation 'component-transfer 'type)
 "@version{2026-02-11}
  @begin{declaration}
(glib:define-gboxed-opaque component-transfer \"GskComponentTransfer\"
  :export t
  :type-initializer \"gsk_component_transfer_get_type\"
  :alloc (error \"GskComponentTransfer: Use constructor function\"))
  @end{declaration}
  @begin{short}
    Specifies a transfer function for a color component to be applied while
    rendering.
  @end{short}
  The available functions include linear, piecewise-linear, gamma and step
  functions.

  Note that the transfer function is applied to un-premultiplied values, and
  all results are clamped to the [0, 1] range.

  Since 4.20
  @see-constructor{gsk:component-transfer-new-discrete}
  @see-constructor{gsk:component-transfer-new-gamma}
  @see-constructor{gsk:component-transfer-new-identiy}
  @see-constructor{gsk:component-transfer-new-levels}
  @see-constructor{gsk:component-transfer-new-linear}
  @see-constructor{gsk:component-transfer-new-table}
  @see-class{gsk:component-transfer-node}")

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_discrete                     Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_discrete"
               %component-transfer-new-discrete)
    (g:boxed component-transfer :return)
  (n :uint)
  (values :pointer))

#+gtk-4-20
(defun component-transfer-new-discrete (values)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[values]{a list of numbers coerced to single floats}
  @begin{short}
    Creates a new component transfer that applies a step function.
  @end{short}
  The new value is computed as
  @begin{pre}
C' = values[k]
  @end{pre}
  where @code{k} is the smallest value such that
  @begin{pre}
k / n <= C < (k + 1) / n
  @end{pre}
  @image[discrete-light]{Figure: gsk:component-transfer-new-discrete}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (let ((n (length values)))
    (cffi:with-foreign-object (ptr :float n)
      (iter (for i from 0 below n)
            (for value in values)
            (setf (cffi:mem-aref ptr :float i) value))
      (%component-transfer-new-discrete n ptr))))

#+gtk-4-20
(export 'component-transfer-new-discrete)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_gamma                        Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_gamma"
               %component-transfer-new-gamma)
    (g:boxed component-transfer :return)
  (amp :float)
  (exp :float)
  (ofs :float))

#+gtk-4-20
(defun component-transfer-new-gamma (amp exp ofs)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[amp]{a number coerced to a single float for the amplitude}
  @argument[exp]{a number coerced to a single float for the exponent}
  @argument[ofs]{a number coerced to a single float for the offset}
  @begin{short}
    Creates a new component transfer that applies a gamma transform.
  @end{short}
  The new value is computed as
  @begin{pre}
C' = amp * pow (C, exp) + ofs
  @end{pre}
  @image[gamma-light]{Figure: gsk:component-transfer-new-gamma}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (%component-transfer-new-gamma (coerce amp 'single-float)
                                 (coerce exp 'single-float)
                                 (coerce ofs 'single-float)))

#+gtk-4-20
(export 'component-transfer-new-gamma)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_identity                     Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_identity"
               component-transfer-new-identity)
    (g:boxed component-transfer :return)
 #+liber-documentation
 "@version{#2026-02-12}
  @begin{short}
    Creates a new component transfer that does not change the component value.
  @end{short}

  @image[identity-light]{Figure: gsk:component-transfer-new-identity}

  Since 4.20
  @see-class{gsk:component-transfer}")

#+gtk-4-20
(export 'component-transfer-new-identity)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_levels                       Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_levels"
               %component-transfer-new-levels)
    (g:boxed component-transfer :return)
  (n :float))

#+gtk-4-20
(defun component-transfer-new-levels (n)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[n]{a number coerced to a single float for the number of levels}
  @begin{short}
    Creates a new component transfer that limits the values of the component
    to @code{n} levels.
  @end{short}
  The new value is computed as
  @begin{pre}
C' = (floor (C * n) + 0.5) / n
  @end{pre}
  @image[levels-light]{Figure: gsk:component-transfer-new-levels}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (%component-transfer-new-levels (coerce n 'single-float)))

#+gtk-4-20
(export 'component-transfer-new-levels)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_linear                       Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_linear"
               %component-transfer-new-linear)
    (g:boxed component-transfer :return)
  (m :float)
  (b :float))

#+gtk-4-20
(defun component-transfer-new-linear (m b)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[m]{a number coerced to a single float for the slop}
  @argument[b]{a number coerced to a single float for the offset}
  @begin{short}
    Creates a new component transfer that applies a linear transform.
  @end{short}
  The new value is computed as
  @begin{pre}
C' = C * m + b
  @end{pre}
  @image[linear-light]{Figure: gsk:component-transfer-new-levels}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (%component-transfer-new-linear (coerce m 'single-float)
                                  (coerce b 'single-float)))

#+gtk-4-20
(export 'component-transfer-new-linear)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_new_table                        Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_new_table"
               %component-transfer-new-table)
    (g:boxed component-transfer :return)
  (n :uint)
  (values :pointer))

#+gtk-4-20
(defun component-transfer-new-table (values)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[values]{a list of numbers coerced to single floats}
  @begin{short}
    Creates a new component transfer that applies a linear transform.
  @end{short}
  The new value is computed as
  @begin{pre}
C' = values[k] + (C - k / (n - 1)) * n * (values[k + 1] - values[k])
  @end{pre}
  where @code{k} is the smallest value such that
  @begin{pre}
k / (n - 1) <= C < (k + 1) / (n - 1)
  @end{pre}
  @image[table-light]{Figure: gsk:component-transfer-new-levels}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (let ((n (length values)))
    (cffi:with-foreign-object (ptr :float n)
      (iter (for i from 0 below n)
            (for value in values)
            (setf (cffi:mem-aref ptr :float i) value))
      (%component-transfer-new-table n ptr))))

#+gtk-4-20
(export 'component-transfer-new-table)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_copy                             Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_copy" component-transfer-copy)
    (g:boxed component-transfer :return)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[other]{a @class{gsk:component-transfer} instance}
  @begin{short}
    Creates a copy of @arg{other}.
  @end{short}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (other (g:boxed component-transfer)))

#+gtk-4-20
(export 'component-transfer-copy)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_free                             Since 4.20
;;; ----------------------------------------------------------------------------

;; not needed

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_equal                            Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_equal" component-transfer-equal) :boolean
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[transfer]{a @class{gsk:component-transfer} instance}
  @argument[other]{another @class{gsk:component-transfer} instance}
  @begin{short}
    Compares two component transfers for equality.
  @end{short}

  Since 4.20
  @see-class{gsk:component-transfer}"
  (transfer (g:boxed component-transfer))
  (other (g:boxed component-transfer)))

#+gtk-4-20
(export 'component-transfer-equal)

;;; ----------------------------------------------------------------------------
;;; GskComponentTransferNode                                Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:define-foreign-type component-transfer-node (render-node)
  ()
  (:simple-parser component-transfer-node))

#+(and gtk-4-20 liber-documentation)
(setf (liber:alias-for-class 'component-transfer-node)
      "GskRenderNode"
      (documentation 'component-transfer-node 'type)
 "@version{2026-02-11}
  @begin{short}
    The render node for applying a @class{gsk:component-transfer} instance for
    each color component of the child node.
  @end{short}

  Since 4.20
  @see-constructor{gsk:component-transfer-node-new}
  @see-class{gsk:component-transfer}
  @see-class{gsk:render-node}")

#+gtk-4-20
(export 'component-transfer-node)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_node_new                         Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_node_new" component-transfer-node-new)
    render-node
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[child]{a @class{gsk:render-node} instance for the child to apply
    component transfers to}
  @argument[r]{a @class{gsk:component-transfer} instance for the red component}
  @argument[g]{a @class{gsk:component-transfer} instance for the green
    component}
  @argument[b]{a @class{gsk:component-transfer} instance for the blue component}
  @argument[a]{a @class{gsk:component-transfer} instance for the alpha
    component}
  @return{The new @class{gsk:component-transfer-node} instance.}
  @begin{short}
    Creates a render node that will apply component transfers to a child node.
  @end{short}

  Since 4.20
  @see-class{gsk:component-transfer-node}
  @see-class{gsk:component-transfer}"
  (child render-node)
  (r (g:boxed component-transfer))
  (g (g:boxed component-transfer))
  (b (g:boxed component-transfer))
  (a (g:boxed component-transfer)))

#+gtk-4-20
(export 'component-transfer-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_node_get_child                   Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_node_get_child"
               component-transfer-node-child) render-node
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[node]{a @class{gsk:component-transfer-node} instance}
  @return{The child @class{gsk:render-node} instance.}
  @begin{short}
    Gets the child node that is getting drawn by the given @arg{node}.
  @end{short}

  Since 4.20
  @see-class{gsk:component-transfer-node}
  @see-class{gsk:render-node}"
  (node render-node))

#+gtk-4-20
(export 'component-transfer-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_component_transfer_node_get_transfer                Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gsk_component_transfer_node_get_transfer"
               component-transfer-node-transfer) (g:boxed component-transfer)
 #+liber-documentation
 "@version{#2026-02-12}
  @argument[node]{a @class{gsk:component-transfer-node} instance}
  @argument[component]{an unsigned integer for the component}
  @return{The @class{gsk:component-transfer} instance.}
  @begin{short}
    Gets the component transfer for one of the components.
  @end{short}

  Since 4.20
  @see-class{gsk:component-transfer-node}
  @see-class{gsk:render-node}"
  (node render-node)
  (component :uint))

#+gtk-4-20
(export 'component-transfer-node-transfer)

;;; --- End of file gsk4.render-node.lis ---------------------------------------
