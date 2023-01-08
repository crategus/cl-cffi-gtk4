;;; ----------------------------------------------------------------------------
;;; gsk.render-node.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GskRenderNode
;;;
;;;     Simple scene graph element
;;;
;;; Types and Values
;;;
;;;     GskRenderNode
;;;     GskBlendNode
;;;     GskBlurNode
;;;     GskBorderNode
;;;     GskCairoNode
;;;     GskClipNode
;;;     GskColorMatrixNode
;;;     GskColorNode
;;;     GskConicGradientNode
;;;     GskContainerNode
;;;     GskCrossFadeNode
;;;     GskDebugNode
;;;     GskInsetShadowNode
;;;     GskLinearGradientNode
;;;     GskRadialGradientNode
;;;     GskOpacityNode
;;;     GskOutsetShadowNode
;;;     GskRepeatingLinearGradientNode
;;;     GskRepeatingRadialGradientNode
;;;     GskRepeatNode
;;;     GskRoundedClipNode
;;;     GskShadowNode
;;;     GskTextNode
;;;     GskTextureNode
;;;     GskTransformNode
;;;     GskGLShaderNode
;;;
;;;     GskRenderNodeType
;;;     GskSerializationError
;;;     GskParseLocation
;;;     GskScalingFilter
;;;     GskColorStop
;;;     GskShadow
;;;     GskBlendMode
;;;
;;; Functions
;;;
;;;     gsk_render_node_ref
;;;     gsk_render_node_unref
;;;     gsk_render_node_get_node_type
;;;     gsk_render_node_draw

;;;     GskParseErrorFunc

;;;     gsk_render_node_serialize
;;;     gsk_render_node_deserialize
;;;     gsk_render_node_write_to_file
;;;     gsk_render_node_get_bounds
;;;     gsk_color_node_new
;;;     gsk_color_node_get_color
;;;     gsk_texture_node_new
;;;     gsk_texture_node_get_texture
;;;     gsk_linear_gradient_node_new
;;;     gsk_repeating_linear_gradient_node_new
;;;     gsk_linear_gradient_node_get_start
;;;     gsk_linear_gradient_node_get_end
;;;     gsk_linear_gradient_node_get_n_color_stops
;;;     gsk_linear_gradient_node_get_color_stops
;;;     gsk_radial_gradient_node_new
;;;     gsk_repeating_radial_gradient_node_new
;;;     gsk_radial_gradient_node_get_n_color_stops
;;;     gsk_radial_gradient_node_get_color_stops
;;;     gsk_radial_gradient_node_get_start
;;;     gsk_radial_gradient_node_get_end
;;;     gsk_radial_gradient_node_get_hradius
;;;     gsk_radial_gradient_node_get_vradius
;;;     gsk_radial_gradient_node_get_center
;;;     gsk_conic_gradient_node_new
;;;     gsk_conic_gradient_node_get_n_color_stops
;;;     gsk_conic_gradient_node_get_color_stops
;;;     gsk_conic_gradient_node_get_center
;;;     gsk_conic_gradient_node_get_rotation
;;;     gsk_border_node_new
;;;     gsk_border_node_get_outline
;;;     gsk_border_node_get_widths
;;;     gsk_border_node_get_colors
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
;;;     gsk_container_node_new
;;;     gsk_container_node_get_n_children
;;;     gsk_container_node_get_child
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
;;;     gsk_gl_shader_node_new
;;;     gsk_gl_shader_node_get_n_children
;;;     gsk_gl_shader_node_get_child
;;;     gsk_gl_shader_node_get_args
;;;     gsk_gl_shader_node_get_shader
;;;
;;; Object Hierarchy
;;;
;;;     GskRenderNode
;;;
;;; Description
;;;
;;; GskRenderNode is the basic block in a scene graph to be rendered using
;;; GskRenderer.
;;;
;;; Each node has a parent, except the top-level node; each node may have
;;; children nodes.
;;;
;;; Each node has an associated drawing surface, which has the size of the
;;; rectangle set using gsk_render_node_set_bounds().
;;;
;;; Render nodes are meant to be transient; once they have been associated to a
;;; GskRenderer it's safe to release any reference you have on them. All
;;; GskRenderNodes are immutable, you can only specify their properties during
;;; construction.
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; enum GskRenderNodeType
;;; ----------------------------------------------------------------------------

(define-g-enum "GskRenderNodeType" render-node-type
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
  :gl-shader-node)

#+liber-documentation
(setf (liber:alias-for-symbol 'render-node-type)
      "GEnum"
      (liber:symbol-documentation 'render-node-type)
 "@version{#2022-9-13}
  @begin{short}
    The type of a node determines what the node is rendering.
  @end{short}
  @begin{pre}
(define-g-enum \"GskRenderNodeType\" render-node-type
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
  :gl-shader-node)
  @end{pre}
  @begin[code]{table}
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
  @end{table}")

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
;;; enum GskScalingFilter
;;;
;;; The filters used when scaling texture data.
;;
;;; The actual implementation of each filter is deferred to the rendering
;;; pipeline.
;;;
;;; GSK_SCALING_FILTER_LINEAR
;;;     linear interpolation filter
;;;
;;; GSK_SCALING_FILTER_NEAREST
;;;     nearest neighbor interpolation filter
;;;
;;; GSK_SCALING_FILTER_TRILINEAR
;;;     linear interpolation along each axis, plus mipmap generation, with
;;;     linear interpolation along the mipmap levels
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; enum GskBlendMode
;;;
;;; The blend modes available for render nodes.
;;;
;;; The implementation of each blend mode is deferred to the rendering pipeline.
;;;
;;; See https://www.w3.org/TR/compositing-1/blending for more information on
;;; blending and blend modes.
;;;
;;; GSK_BLEND_MODE_DEFAULT
;;;     The default blend mode, which specifies no blending
;;;
;;; GSK_BLEND_MODE_MULTIPLY
;;;     The source color is multiplied by the destination and replaces the
;;;     destination
;;;
;;; GSK_BLEND_MODE_SCREEN
;;;     Multiplies the complements of the destination and source color values,
;;;     then complements the result.
;;;
;;; GSK_BLEND_MODE_OVERLAY
;;;     Multiplies or screens the colors, depending on the destination color
;;;     value. This is the inverse of hard-list
;;;
;;; GSK_BLEND_MODE_DARKEN
;;;     Selects the darker of the destination and source colors
;;;
;;; GSK_BLEND_MODE_LIGHTEN
;;;     Selects the lighter of the destination and source colors
;;;
;;; GSK_BLEND_MODE_COLOR_DODGE
;;;     Brightens the destination color to reflect the source color
;;;
;;; GSK_BLEND_MODE_COLOR_BURN
;;;     Darkens the destination color to reflect the source color
;;;
;;; GSK_BLEND_MODE_HARD_LIGHT
;;;     Multiplies or screens the colors, depending on the source color value
;;;
;;; GSK_BLEND_MODE_SOFT_LIGHT
;;;     Darkens or lightens the colors, depending on the source color value
;;;
;;; GSK_BLEND_MODE_DIFFERENCE
;;;     Subtracts the darker of the two constituent colors from the lighter
;;;     color
;;;
;;; GSK_BLEND_MODE_EXCLUSION
;;;     Produces an effect similar to that of the difference mode but lower in
;;;     contrast
;;;
;;; GSK_BLEND_MODE_COLOR
;;;     Creates a color with the hue and saturation of the source color and the
;;;     luminosity of the destination color
;;;
;;; GSK_BLEND_MODE_HUE
;;;     Creates a color with the hue of the source color and the saturation and
;;;     luminosity of the destination color
;;;
;;; GSK_BLEND_MODE_SATURATION
;;;     Creates a color with the saturation of the source color and the hue and
;;;     luminosity of the destination color
;;;
;;; GSK_BLEND_MODE_LUMINOSITY
;;;     Creates a color with the luminosity of the source color and the hue and
;;;     saturation of the destination color
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRenderNode
;;;
;;; typedef struct _GskRenderNode GskRenderNode;
;;;
;;; A node in the render tree.
;;; ----------------------------------------------------------------------------


(defcstruct render-node)

;;; ----------------------------------------------------------------------------
;;; GskBlendNode
;;;
;;; typedef struct _GskBlendNode GskBlendNode;
;;;
;;; A render node applying a blending function between its two child nodes.
;;; ----------------------------------------------------------------------------

(defcstruct blend-node)

;;; ----------------------------------------------------------------------------
;;; GskBlurNode
;;;
;;; typedef struct _GskBlurNode GskBlurNode;
;;;
;;; A render node applying a blur effect to its single child.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskBorderNode
;;;
;;; typedef struct _GskBorderNode GskBorderNode;
;;;
;;; A render node for a border.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskCairoNode
;;;
;;; typedef struct _GskCairoNode GskCairoNode;
;;;
;;; A render node for a Cairo surface.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskClipNode
;;;
;;; typedef struct _GskClipNode GskClipNode;
;;;
;;; A render node applying a rectangular clip to its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskColorMatrixNode
;;;
;;; typedef struct _GskColorMatrixNode GskColorMatrixNode;
;;;
;;; A render node controlling the color matrix of its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskColorNode
;;;
;;; typedef struct _GskColorNode GskColorNode;
;;;
;;; A render node for a solid color.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskConicGradientNode
;;;
;;; typedef struct _GskConicGradientNode GskConicGradientNode;
;;;
;;; A render node for a conic gradient.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskContainerNode
;;;
;;; typedef struct _GskContainerNode GskContainerNode;
;;;
;;; A render node that can contain other render nodes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskCrossFadeNode
;;;
;;; typedef struct _GskCrossFadeNode GskCrossFadeNode;
;;;
;;; A render node cross fading between two child nodes.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskDebugNode
;;;
;;; typedef struct _GskDebugNode GskDebugNode;
;;;
;;; A render node that emits a debugging message when drawing its child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskInsetShadowNode
;;;
;;; typedef struct _GskInsetShadowNode GskInsetShadowNode;
;;;
;;; A render node for an inset shadow.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskLinearGradientNode
;;;
;;; typedef struct _GskLinearGradientNode GskLinearGradientNode;
;;;
;;; A render node for a linear gradient.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRadialGradientNode
;;;
;;; typedef struct _GskRadialGradientNode GskRadialGradientNode;
;;;
;;; A render node for a radial gradient.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskOpacityNode
;;;
;;; typedef struct _GskOpacityNode GskOpacityNode;
;;;
;;; A render node controlling the opacity of its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskOutsetShadowNode
;;;
;;; typedef struct _GskOutsetShadowNode GskOutsetShadowNode;
;;;
;;; A render node for an outset shadow.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRepeatingLinearGradientNode
;;;
;;; typedef struct _GskRepeatingLinearGradientNode
;;;                 GskRepeatingLinearGradientNode;
;;;
;;; A render node for a repeating linear gradient.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRepeatingRadialGradientNode
;;;
;;; typedef struct _GskRepeatingRadialGradientNode
;;;                 GskRepeatingRadialGradientNode;
;;;
;;; A render node for a repeating radial gradient.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRepeatNode
;;;
;;; typedef struct _GskRepeatNode GskRepeatNode;
;;;
;;; A render node repeating its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskRoundedClipNode
;;;
;;; typedef struct _GskRoundedClipNode GskRoundedClipNode;
;;;
;;; A render node applying a rounded rectangle clip to its single child.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskShadowNode
;;;
;;; typedef struct _GskShadowNode GskShadowNode;
;;;
;;; A render node drawing one or more shadows behind its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskTextNode
;;;
;;; typedef struct _GskTextNode GskTextNode;
;;;
;;; A render node drawing a set of glyphs.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskTextureNode
;;;
;;; typedef struct _GskTextureNode GskTextureNode;
;;;
;;; A render node for a GdkTexture.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskTransformNode
;;;
;;; typedef struct _GskTransformNode GskTransformNode;
;;;
;;; A render node applying a GskTransform to its single child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskGLShaderNode
;;;
;;; typedef struct _GskGLShaderNode GskGLShaderNode;
;;;
;;; A render node using a GL shader when drawing its children nodes.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gsk_render_node_ref ()
;;;
;;; GskRenderNode *
;;; gsk_render_node_ref (GskRenderNode *node);
;;;
;;; Acquires a reference on the given GskRenderNode.
;;;
;;; node :
;;;     a GskRenderNode
;;;
;;; Returns :
;;;     the GskRenderNode with an additional reference.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_unref ()
;;;
;;; void
;;; gsk_render_node_unref (GskRenderNode *node);
;;;
;;; Releases a reference on the given GskRenderNode.
;;;
;;; If the reference was the last, the resources associated to the node are
;;; freed.
;;;
;;; node :
;;;     a GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_node_type ()
;;;
;;; GskRenderNodeType
;;; gsk_render_node_get_node_type (GskRenderNode *node);
;;;
;;; Returns the type of the node .
;;;
;;; node :
;;;     a GskRenderNode
;;;
;;; Returns :
;;;     the type of the GskRenderNode
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_draw ()
;;;
;;; void
;;; gsk_render_node_draw (GskRenderNode *node,
;;;                       cairo_t *cr);
;;;
;;; Draw the contents of node to the given cairo context.
;;;
;;; Typically, you'll use this function to implement fallback rendering of
;;; GskRenderNodes on an intermediate Cairo context, instead of using the
;;; drawing context associated to a GdkSurface's rendering buffer.
;;;
;;; For advanced nodes that cannot be supported using Cairo, in particular for
;;; nodes doing 3D operations, this function may fail.
;;;
;;; node :
;;;     a GskRenderNode
;;;
;;; cr :
;;;     cairo context to draw to
;;; ----------------------------------------------------------------------------

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
;;; gsk_render_node_serialize ()
;;;
;;; GBytes *
;;; gsk_render_node_serialize (GskRenderNode *node);
;;;
;;; Serializes the node for later deserialization via
;;; gsk_render_node_deserialize(). No guarantees are made about the format used
;;; other than that the same version of GTK will be able to deserialize the
;;; result of a call to gsk_render_node_serialize() and
;;; gsk_render_node_deserialize() will correctly reject files it cannot open
;;; that were created with previous versions of GTK.
;;;
;;; The intended use of this functions is testing, benchmarking and debugging.
;;; The format is not meant as a permanent storage format.
;;;
;;; node :
;;;     a GskRenderNode
;;;
;;; Returns :
;;;     a GBytes representing the node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_deserialize ()
;;;
;;; GskRenderNode *
;;; gsk_render_node_deserialize (GBytes *bytes,
;;;                              GskParseErrorFunc error_func,
;;;                              gpointer user_data);
;;;
;;; Loads data previously created via gsk_render_node_serialize(). For a
;;; discussion of the supported format, see that function.
;;;
;;; bytes :
;;;     the bytes containing the data
;;;
;;; error_func :
;;;     Callback on parsing errors or NULL.
;;;
;;; user_data :
;;;     user_data for error_func .
;;;
;;; Returns :
;;;     a new GskRenderNode or NULL on error.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_write_to_file ()
;;;
;;; gboolean
;;; gsk_render_node_write_to_file (GskRenderNode *node,
;;;                                const char *filename,
;;;                                GError **error);
;;;
;;; This function is equivalent to calling gsk_render_node_serialize() followed
;;; by g_file_set_contents(). See those two functions for details on the
;;; arguments.
;;;
;;; It is mostly intended for use inside a debugger to quickly dump a render
;;; node to a file for later inspection.
;;;
;;; node :
;;;     a GskRenderNode
;;;
;;; filename :
;;;     the file to save it to.
;;;
;;; error :
;;;     Return location for a potential error
;;;
;;; Returns :
;;;     TRUE if saving was successful
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_bounds ()
;;;
;;; void
;;; gsk_render_node_get_bounds (GskRenderNode *node,
;;;                             graphene_rect_t *bounds);
;;;
;;; Retrieves the boundaries of the node . The node will not draw outside of its
;;; boundaries.
;;
;;; node :
;;;     a GskRenderNode
;;;
;;; bounds :
;;;     return location for the boundaries.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_color_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_color_node_new (const GdkRGBA *rgba,
;;;                     const graphene_rect_t *bounds);
;;;
;;; Creates a GskRenderNode that will render the color specified by rgba into
;;; the area given by bounds .
;;;
;;; rgba :
;;;     a GdkRGBA specifying a color
;;;
;;; bounds :
;;;     the rectangle to render the color into
;;;
;;; Returns :
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_color_node_get_color ()
;;;
;;; const GdkRGBA *
;;; gsk_color_node_get_color (GskRenderNode *node);
;;;
;;; Retrieves the color of the given node .
;;;
;;; node :
;;;     a GskColorNode.
;;;
;;; Returns :
;;;     the color of the node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_texture_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_texture_node_new (GdkTexture *texture,
;;;                       const graphene_rect_t *bounds);
;;;
;;; Creates a GskRenderNode that will render the given texture into the area
;;; given by bounds .
;;;
;;; texture :
;;;     the GdkTexture
;;;
;;; bounds :
;;;     the rectangle to render the texture into
;;;
;;; Returns :
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_texture_node_get_texture ()
;;;
;;; GdkTexture *
;;; gsk_texture_node_get_texture (GskRenderNode *node);
;;;
;;; Retrieves the GdkTexture used when creating this GskRenderNode.
;;;
;;; node :
;;;     a GskRenderNode of type GSK_TEXTURE_NODE.
;;;
;;; Returns :
;;;     the GdkTexture.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_linear_gradient_node_new (const graphene_rect_t *bounds,
;;;                               const graphene_point_t *start,
;;;                               const graphene_point_t *end,
;;;                               const GskColorStop *color_stops,
;;;                               gsize n_color_stops);
;;;
;;; Creates a GskRenderNode that will create a linear gradient from the given
;;; points and color stops, and render that into the area given by bounds .
;;;
;;; bounds :
;;;     the rectangle to render the linear gradient into
;;;
;;; start :
;;;     the point at which the linear gradient will begin
;;;
;;; end :
;;;     the point at which the linear gradient will finish
;;;
;;; color_stops :
;;;     a pointer to an array of GskColorStop defining the gradient The offsets
;;;     of all color steps must be increasing. The first stop's offset must be
;;;     >= 0 and the last stop's offset must be <= 1.
;;;
;;; n_color_stops :
;;;     the number of elements in color_stops
;;;
;;; Returns :
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_repeating_linear_gradient_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_repeating_linear_gradient_node_new
;;;                                (const graphene_rect_t *bounds,
;;;                                 const graphene_point_t *start,
;;;                                 const graphene_point_t *end,
;;;                                 const GskColorStop *color_stops,
;;;                                 gsize n_color_stops);
;;;
;;; Creates a GskRenderNode that will create a repeating linear gradient from
;;; the given points and color stops, and render that into the area given by
;;; bounds .
;;;
;;; bounds :
;;;     the rectangle to render the linear gradient into
;;;
;;; start :
;;;     the point at which the linear gradient will begin
;;;
;;; end :
;;;     the point at which the linear gradient will finish
;;;
;;; color_stops :
;;;     a pointer to an array of GskColorStop defining the gradient The offsets
;;;     of all color steps must be increasing. The first stop's offset must be
;;;     >= 0 and the last stop's offset must be <= 1.
;;;
;;; n_color_stops :
;;;     the number of elements in color_stops
;;;
;;; Returns :
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_start ()
;;;
;;; const graphene_point_t *
;;; gsk_linear_gradient_node_get_start (GskRenderNode *node);
;;;
;;; Retrieves the initial point of the linear gradient.
;;;
;;; node :
;;;     a GskRenderNode for a linear gradient.
;;;
;;; Returns :
;;;     the initial point.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_end ()
;;;
;;; const graphene_point_t *
;;; gsk_linear_gradient_node_get_end (GskRenderNode *node);
;;;
;;; Retrieves the final point of the linear gradient.
;;;
;;; node :
;;;     a GskRenderNode for a linear gradient.
;;;
;;; Returns :
;;;     the final point.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;gsk_linear_gradient_node_get_n_color_stops ()
;;;gsize
;;;gsk_linear_gradient_node_get_n_color_stops
;;;                               (GskRenderNode *node);
;;;Retrieves the number of color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a linear gradient.

;;;[type GskLinearGradientNode]
;;;Returns
;;;the number of color stops

;;;gsk_linear_gradient_node_get_color_stops ()
;;;const GskColorStop *
;;;gsk_linear_gradient_node_get_color_stops
;;;                               (GskRenderNode *node,
;;;                                gsize *n_stops);
;;;Retrieves the color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a linear gradient.

;;;[type GskLinearGradientNode]
;;;n_stops

;;;the number of color stops in the returned array.

;;;[out][optional]
;;;Returns
;;;the color stops in the gradient.

;;;[array length=n_stops]

;;;gsk_radial_gradient_node_new ()
;;;GskRenderNode *
;;;gsk_radial_gradient_node_new (const graphene_rect_t *bounds,
;;;                              const graphene_point_t *center,
;;;                              float hradius,
;;;                              float vradius,
;;;                              float start,
;;;                              float end,
;;;                              const GskColorStop *color_stops,
;;;                              gsize n_color_stops);
;;;Creates a GskRenderNode that draws a radial gradient. The radial gradient starts around center . The size of the gradient is dictated by hradius in horizontal orientation and by vradius in vertial orientation.

;;;Parameters
;;;bounds

;;;the bounds of the node

;;;
;;;center

;;;the center of the gradient

;;;
;;;hradius

;;;the horizontal radius

;;;
;;;vradius

;;;the vertical radius

;;;
;;;start

;;;a percentage >= 0 that defines the start of the gradient around center

;;;
;;;end

;;;a percentage >= 0 that defines the end of the gradient around center

;;;
;;;color_stops

;;;a pointer to an array of GskColorStop defining the gradient The offsets of all color steps must be increasing. The first stop's offset must be >= 0 and the last stop's offset must be <= 1.

;;;[array length=n_color_stops]
;;;n_color_stops

;;;the number of elements in color_stops

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskRadialGradientNode]

;;;gsk_repeating_radial_gradient_node_new ()
;;;GskRenderNode *
;;;gsk_repeating_radial_gradient_node_new
;;;                               (const graphene_rect_t *bounds,
;;;                                const graphene_point_t *center,
;;;                                float hradius,
;;;                                float vradius,
;;;                                float start,
;;;                                float end,
;;;                                const GskColorStop *color_stops,
;;;                                gsize n_color_stops);
;;;Creates a GskRenderNode that draws a repeating radial gradient. The radial gradient starts around center . The size of the gradient is dictated by hradius in horizontal orientation and by vradius in vertial orientation.

;;;Parameters
;;;bounds

;;;the bounds of the node

;;;
;;;center

;;;the center of the gradient

;;;
;;;hradius

;;;the horizontal radius

;;;
;;;vradius

;;;the vertical radius

;;;
;;;start

;;;a percentage >= 0 that defines the start of the gradient around center

;;;
;;;end

;;;a percentage >= 0 that defines the end of the gradient around center

;;;
;;;color_stops

;;;a pointer to an array of GskColorStop defining the gradient The offsets of all color steps must be increasing. The first stop's offset must be >= 0 and the last stop's offset must be <= 1.

;;;[array length=n_color_stops]
;;;n_color_stops

;;;the number of elements in color_stops

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskRepeatingRadialGradientNode]

;;;gsk_radial_gradient_node_get_n_color_stops ()
;;;gsize
;;;gsk_radial_gradient_node_get_n_color_stops
;;;                               (GskRenderNode *node);
;;;Retrieves the number of color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the number of color stops

;;;gsk_radial_gradient_node_get_color_stops ()
;;;const GskColorStop *
;;;gsk_radial_gradient_node_get_color_stops
;;;                               (GskRenderNode *node,
;;;                                gsize *n_stops);
;;;Retrieves the color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;n_stops

;;;the number of color stops in the returned array.

;;;[out][optional]
;;;Returns
;;;the color stops in the gradient.

;;;[array length=n_stops]

;;;gsk_radial_gradient_node_get_start ()
;;;float
;;;gsk_radial_gradient_node_get_start (GskRenderNode *node);
;;;Retrieves the start value for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the start value for the gradient

;;;gsk_radial_gradient_node_get_end ()
;;;float
;;;gsk_radial_gradient_node_get_end (GskRenderNode *node);
;;;Retrieves the end value for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the end value for the gradient

;;;gsk_radial_gradient_node_get_hradius ()
;;;float
;;;gsk_radial_gradient_node_get_hradius (GskRenderNode *node);
;;;Retrieves the horizonal radius for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the horizontal radius for the gradient

;;;gsk_radial_gradient_node_get_vradius ()
;;;float
;;;gsk_radial_gradient_node_get_vradius (GskRenderNode *node);
;;;Retrieves the vertical radius for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the vertical radius for the gradient

;;;gsk_radial_gradient_node_get_center ()
;;;const graphene_point_t *
;;;gsk_radial_gradient_node_get_center (GskRenderNode *node);
;;;Retrieves the center pointer for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a radial gradient.

;;;[type GskRadialGradientNode]
;;;Returns
;;;the center point for the gradient

;;;gsk_conic_gradient_node_new ()
;;;GskRenderNode *
;;;gsk_conic_gradient_node_new (const graphene_rect_t *bounds,
;;;                             const graphene_point_t *center,
;;;                             float rotation,
;;;                             const GskColorStop *color_stops,
;;;                             gsize n_color_stops);
;;;Creates a GskRenderNode that draws a conic gradient. The conic gradient starts around center in the direction of rotation . A rotation of 0 means that the gradient points up. Color stops are then added clockwise.

;;;Parameters
;;;bounds

;;;the bounds of the node

;;;
;;;center

;;;the center of the gradient

;;;
;;;rotation

;;;the rotation of the gradient in degrees

;;;
;;;color_stops

;;;a pointer to an array of GskColorStop defining the gradient The offsets of all color steps must be increasing. The first stop's offset must be >= 0 and the last stop's offset must be <= 1.

;;;[array length=n_color_stops]
;;;n_color_stops

;;;the number of elements in color_stops

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskConicGradientNode]

;;;gsk_conic_gradient_node_get_n_color_stops ()
;;;gsize
;;;gsk_conic_gradient_node_get_n_color_stops
;;;                               (GskRenderNode *node);
;;;Retrieves the number of color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a conic gradient.

;;;[type GskConicGradientNode]
;;;Returns
;;;the number of color stops

;;;gsk_conic_gradient_node_get_color_stops ()
;;;const GskColorStop *
;;;gsk_conic_gradient_node_get_color_stops
;;;                               (GskRenderNode *node,
;;;                                gsize *n_stops);
;;;Retrieves the color stops in the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a conic gradient.

;;;[type GskConicGradientNode]
;;;n_stops

;;;the number of color stops in the returned array.

;;;[out][optional]
;;;Returns
;;;the color stops in the gradient.

;;;[array length=n_stops]

;;;gsk_conic_gradient_node_get_center ()
;;;const graphene_point_t *
;;;gsk_conic_gradient_node_get_center (GskRenderNode *node);
;;;Retrieves the center pointer for the gradient.

;;;Parameters
;;;node

;;;a GskRenderNode for a conic gradient.

;;;[type GskConicGradientNode]
;;;Returns
;;;the center point for the gradient

;;;gsk_conic_gradient_node_get_rotation ()
;;;float
;;;gsk_conic_gradient_node_get_rotation (GskRenderNode *node);
;;;Retrieves the rotation for the gradient in degrees.

;;;Parameters
;;;node

;;;a GskRenderNode for a conic gradient.

;;;[type GskConicGradientNode]
;;;Returns
;;;the rotation for the gradient

;;;gsk_border_node_new ()
;;;GskRenderNode *
;;;gsk_border_node_new (const GskRoundedRect *outline,
;;;                     const float border_width[4],
;;;                     const GdkRGBA border_color[4]);
;;;Creates a GskRenderNode that will stroke a border rectangle inside the given outline . The 4 sides of the border can have different widths and colors.

;;;Parameters
;;;outline

;;;a GskRoundedRect describing the outline of the border

;;;
;;;border_width

;;;the stroke width of the border on the top, right, bottom and left side respectively.

;;;[array fixed-size=4]
;;;border_color

;;;the color used on the top, right, bottom and left side.

;;;[array fixed-size=4]
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskBorderNode]

;;;gsk_border_node_get_outline ()
;;;const GskRoundedRect *
;;;gsk_border_node_get_outline (GskRenderNode *node);
;;;Retrieves the outline of the border.

;;;Parameters
;;;node

;;;a GskRenderNode for a border.

;;;[type GskBorderNode]
;;;Returns
;;;the outline of the border

;;;gsk_border_node_get_widths ()
;;;const float *
;;;gsk_border_node_get_widths (GskRenderNode *node);
;;;Retrieves the stroke widths of the border.

;;;Parameters
;;;node

;;;a GskRenderNode for a border.

;;;[type GskBorderNode]
;;;Returns
;;;an array of 4 floats for the top, right, bottom and left stroke width of the border, respectively.

;;;[transfer none][array fixed-size=4]

;;;gsk_border_node_get_colors ()
;;;const GdkRGBA *
;;;gsk_border_node_get_colors (GskRenderNode *node);
;;;Retrieves the colors of the border.

;;;Parameters
;;;node

;;;a GskRenderNode for a border.

;;;[type GskBorderNode]
;;;Returns
;;;an array of 4 GdkRGBA structs for the top, right, bottom and left color of the border.

;;;[transfer none]

;;;gsk_inset_shadow_node_new ()
;;;GskRenderNode *
;;;gsk_inset_shadow_node_new (const GskRoundedRect *outline,
;;;                           const GdkRGBA *color,
;;;                           float dx,
;;;                           float dy,
;;;                           float spread,
;;;                           float blur_radius);
;;;Creates a GskRenderNode that will render an inset shadow into the box given by outline .

;;;Parameters
;;;outline

;;;outline of the region containing the shadow

;;;
;;;color

;;;color of the shadow

;;;
;;;dx

;;;horizontal offset of shadow

;;;
;;;dy

;;;vertical offset of shadow

;;;
;;;spread

;;;how far the shadow spreads towards the inside

;;;
;;;blur_radius

;;;how much blur to apply to the shadow

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskInsetShadowNode]

;;;gsk_inset_shadow_node_get_outline ()
;;;const GskRoundedRect *
;;;gsk_inset_shadow_node_get_outline (GskRenderNode *node);
;;;Retrieves the outline rectangle of the inset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;a rounded rectangle.

;;;[transfer none]

;;;gsk_inset_shadow_node_get_color ()
;;;const GdkRGBA *
;;;gsk_inset_shadow_node_get_color (GskRenderNode *node);
;;;Retrieves the color of the inset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;the color of the shadow.

;;;[transfer none]

;;;gsk_inset_shadow_node_get_dx ()
;;;float
;;;gsk_inset_shadow_node_get_dx (GskRenderNode *node);
;;;Retrieves the horizontal offset of the inset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;an offset, in pixels

;;;gsk_inset_shadow_node_get_dy ()
;;;float
;;;gsk_inset_shadow_node_get_dy (GskRenderNode *node);
;;;Retrieves the vertical offset of the inset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;an offset, in pixels

;;;gsk_inset_shadow_node_get_spread ()
;;;float
;;;gsk_inset_shadow_node_get_spread (GskRenderNode *node);
;;;Retrieves how much the shadow spreads inwards.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;the size of the shadow, in pixels

;;;gsk_inset_shadow_node_get_blur_radius ()
;;;float
;;;gsk_inset_shadow_node_get_blur_radius (GskRenderNode *node);
;;;Retrieves the blur radius to apply to the shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an inset shadow.

;;;[type GskInsetShadowNode]
;;;Returns
;;;the blur radius, in pixels

;;;gsk_outset_shadow_node_new ()
;;;GskRenderNode *
;;;gsk_outset_shadow_node_new (const GskRoundedRect *outline,
;;;                            const GdkRGBA *color,
;;;                            float dx,
;;;                            float dy,
;;;                            float spread,
;;;                            float blur_radius);
;;;Creates a GskRenderNode that will render an outset shadow around the box given by outline .

;;;Parameters
;;;outline

;;;outline of the region surrounded by shadow

;;;
;;;color

;;;color of the shadow

;;;
;;;dx

;;;horizontal offset of shadow

;;;
;;;dy

;;;vertical offset of shadow

;;;
;;;spread

;;;how far the shadow spreads towards the inside

;;;
;;;blur_radius

;;;how much blur to apply to the shadow

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskOutsetShadowNode]

;;;gsk_outset_shadow_node_get_outline ()
;;;const GskRoundedRect *
;;;gsk_outset_shadow_node_get_outline (GskRenderNode *node);
;;;Retrieves the outline rectangle of the outset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;a rounded rectangle.

;;;[transfer none]

;;;gsk_outset_shadow_node_get_color ()
;;;const GdkRGBA *
;;;gsk_outset_shadow_node_get_color (GskRenderNode *node);
;;;Retrieves the color of the outset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;a color.

;;;[transfer none]

;;;gsk_outset_shadow_node_get_dx ()
;;;float
;;;gsk_outset_shadow_node_get_dx (GskRenderNode *node);
;;;Retrieves the horizontal offset of the outset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;an offset, in pixels

;;;gsk_outset_shadow_node_get_dy ()
;;;float
;;;gsk_outset_shadow_node_get_dy (GskRenderNode *node);
;;;Retrieves the vertical offset of the outset shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;an offset, in pixels

;;;gsk_outset_shadow_node_get_spread ()
;;;float
;;;gsk_outset_shadow_node_get_spread (GskRenderNode *node);
;;;Retrieves how much the shadow spreads outwards.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;the size of the shadow, in pixels

;;;gsk_outset_shadow_node_get_blur_radius ()
;;;float
;;;gsk_outset_shadow_node_get_blur_radius
;;;                               (GskRenderNode *node);
;;;Retrieves the blur radius of the shadow.

;;;Parameters
;;;node

;;;a GskRenderNode for an outset shadow.

;;;[type GskOutsetShadowNode]
;;;Returns
;;;the blur radius, in pixels

;;;gsk_cairo_node_new ()
;;;GskRenderNode *
;;;gsk_cairo_node_new (const graphene_rect_t *bounds);
;;;Creates a GskRenderNode that will render a cairo surface into the area given by bounds . You can draw to the cairo surface using gsk_cairo_node_get_draw_context()

;;;Parameters
;;;bounds

;;;the rectangle to render to

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskCairoNode]

;;;gsk_cairo_node_get_draw_context ()
;;;cairo_t *
;;;gsk_cairo_node_get_draw_context (GskRenderNode *node);
;;;Creates a Cairo context for drawing using the surface associated to the render node.

;;;If no surface exists yet, a surface will be created optimized for rendering to renderer .

;;;Parameters
;;;node

;;;a GskRenderNode for a Cairo surface.

;;;[type GskCairoNode]
;;;Returns
;;;a Cairo context used for drawing; use cairo_destroy() when done drawing.

;;;[transfer full]

;;;gsk_cairo_node_get_surface ()
;;;cairo_surface_t *
;;;gsk_cairo_node_get_surface (GskRenderNode *node);
;;;Retrieves the Cairo surface used by the render node.

;;;Parameters
;;;node

;;;a GskRenderNode for a Cairo surface.

;;;[type GskCairoNode]
;;;Returns
;;;a Cairo surface.

;;;[transfer none]

;;;gsk_container_node_new ()
;;;GskRenderNode *
;;;gsk_container_node_new (GskRenderNode **children,
;;;                        guint n_children);
;;;Creates a new GskRenderNode instance for holding the given children . The new node will acquire a reference to each of the children.

;;;Parameters
;;;children

;;;The children of the node.

;;;[array length=n_children][transfer none]
;;;n_children

;;;Number of children in the children array

;;;
;;;Returns
;;;the new GskRenderNode.

;;;[transfer full][type GskContainerNode]

;;;gsk_container_node_get_n_children ()
;;;guint
;;;gsk_container_node_get_n_children (GskRenderNode *node);
;;;Retrieves the number of direct children of node .

;;;Parameters
;;;node

;;;a container GskRenderNode.

;;;[type GskContainerNode]
;;;Returns
;;;the number of children of the GskRenderNode

;;;gsk_container_node_get_child ()
;;;GskRenderNode *
;;;gsk_container_node_get_child (GskRenderNode *node,
;;;                              guint idx);
;;;Gets one of the children of container .

;;;Parameters
;;;node

;;;a container GskRenderNode.

;;;[type GskContainerNode]
;;;idx

;;;the position of the child to get

;;;
;;;Returns
;;;the idx 'th child of container .

;;;[transfer none]

;;;gsk_transform_node_new ()
;;;GskRenderNode *
;;;gsk_transform_node_new (GskRenderNode *child,
;;;                        GskTransform *transform);
;;;Creates a GskRenderNode that will transform the given child with the given transform .

;;;Parameters
;;;child

;;;The node to transform

;;;
;;;transform

;;;The transform to apply.

;;;[transfer none]
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskTransformNode]

;;;gsk_transform_node_get_child ()
;;;GskRenderNode *
;;;gsk_transform_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting transformed by the given node .

;;;Parameters
;;;node

;;;a GskRenderNode for a transform.

;;;[type GskTransformNode]
;;;Returns
;;;The child that is getting transformed.

;;;[transfer none]

;;;gsk_transform_node_get_transform ()
;;;GskTransform *
;;;gsk_transform_node_get_transform (GskRenderNode *node);
;;;Retrieves the GskTransform used by the node .

;;;Parameters
;;;node

;;;a GskRenderNode for a transform.

;;;[type GskTransformNode]
;;;Returns
;;;a GskTransform.

;;;[transfer none]

;;;gsk_opacity_node_new ()
;;;GskRenderNode *
;;;gsk_opacity_node_new (GskRenderNode *child,
;;;                      float opacity);
;;;Creates a GskRenderNode that will drawn the child with reduced opacity .

;;;Parameters
;;;child

;;;The node to draw

;;;
;;;opacity

;;;The opacity to apply

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskOpacityNode]

;;;gsk_opacity_node_get_child ()
;;;GskRenderNode *
;;;gsk_opacity_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting opacityed by the given node .

;;;Parameters
;;;node

;;;a GskRenderNode for an opacity.

;;;[type GskOpacityNode]
;;;Returns
;;;The child that is getting opacityed.

;;;[transfer none]

;;;gsk_opacity_node_get_opacity ()
;;;float
;;;gsk_opacity_node_get_opacity (GskRenderNode *node);
;;;Gets the transparency factor for an opacity node.

;;;Parameters
;;;node

;;;a GskRenderNode for an opacity.

;;;[type GskOpacityNode]
;;;Returns
;;;the opacity factor

;;;gsk_color_matrix_node_new ()
;;;GskRenderNode *
;;;gsk_color_matrix_node_new (GskRenderNode *child,
;;;                           const graphene_matrix_t *color_matrix,
;;;                           const graphene_vec4_t *color_offset);
;;;Creates a GskRenderNode that will drawn the child with reduced color_matrix .

;;;In particular, the node will transform the operation

;;;pixel = color_matrix * pixel + color_offset
;;;for every pixel.

;;;Parameters
;;;child

;;;The node to draw

;;;
;;;color_matrix

;;;The matrix to apply

;;;
;;;color_offset

;;;Values to add to the color

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskColorMatrixNode]

;;;gsk_color_matrix_node_get_child ()
;;;GskRenderNode *
;;;gsk_color_matrix_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting its colors modified by the given node .

;;;Parameters
;;;node

;;;a color matrix GskRenderNode.

;;;[type GskColorMatrixNode]
;;;Returns
;;;The child that is getting its colors modified.

;;;[transfer none]

;;;gsk_color_matrix_node_get_color_matrix ()
;;;const graphene_matrix_t *
;;;gsk_color_matrix_node_get_color_matrix
;;;                               (GskRenderNode *node);
;;;Retrieves the color matrix used by the node .

;;;Parameters
;;;node

;;;a color matrix GskRenderNode.

;;;[type GskColorMatrixNode]
;;;Returns
;;;a 4x4 color matrix

;;;gsk_color_matrix_node_get_color_offset ()
;;;const graphene_vec4_t *
;;;gsk_color_matrix_node_get_color_offset
;;;                               (GskRenderNode *node);
;;;Retrieves the color offset used by the node .

;;;Parameters
;;;node

;;;a color matrix GskRenderNode.

;;;[type GskColorMatrixNode]
;;;Returns
;;;a color vector

;;;gsk_repeat_node_new ()
;;;GskRenderNode *
;;;gsk_repeat_node_new (const graphene_rect_t *bounds,
;;;                     GskRenderNode *child,
;;;                     const graphene_rect_t *child_bounds);
;;;Creates a GskRenderNode that will repeat the drawing of child across the given bounds .

;;;Parameters
;;;bounds

;;;The bounds of the area to be painted

;;;
;;;child

;;;The child to repeat

;;;
;;;child_bounds

;;;The area of the child to repeat or NULL to use the child's bounds.

;;;[allow-none]
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskRepeatNode]

;;;gsk_repeat_node_get_child ()
;;;GskRenderNode *
;;;gsk_repeat_node_get_child (GskRenderNode *node);
;;;Retrieves the child of node .

;;;Parameters
;;;node

;;;a repeat GskRenderNode.

;;;[type GskRepeatNode]
;;;Returns
;;;a GskRenderNode.

;;;[transfer none]

;;;gsk_repeat_node_get_child_bounds ()
;;;const graphene_rect_t *
;;;gsk_repeat_node_get_child_bounds (GskRenderNode *node);
;;;Retrieves the bounding rectangle of the child of node .

;;;Parameters
;;;node

;;;a repeat GskRenderNode.

;;;[type GskRepeatNode]
;;;Returns
;;;a bounding rectangle.

;;;[transfer none]

;;;gsk_clip_node_new ()
;;;GskRenderNode *
;;;gsk_clip_node_new (GskRenderNode *child,
;;;                   const graphene_rect_t *clip);
;;;Creates a GskRenderNode that will clip the child to the area given by clip .

;;;Parameters
;;;child

;;;The node to draw

;;;
;;;clip

;;;The clip to apply

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskClipNode]

;;;gsk_clip_node_get_child ()
;;;GskRenderNode *
;;;gsk_clip_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting clipped by the given node .

;;;Parameters
;;;node

;;;a clip GskRenderNode .

;;;[type GskClipNode]
;;;Returns
;;;The child that is getting clipped.

;;;[transfer none]

;;;gsk_clip_node_get_clip ()
;;;const graphene_rect_t *
;;;gsk_clip_node_get_clip (GskRenderNode *node);
;;;Retrieves the clip rectangle for node .

;;;Parameters
;;;node

;;;a GskClipNode.

;;;[type GskClipNode]
;;;Returns
;;;a clip rectangle

;;;gsk_rounded_clip_node_new ()
;;;GskRenderNode *
;;;gsk_rounded_clip_node_new (GskRenderNode *child,
;;;                           const GskRoundedRect *clip);
;;;Creates a GskRenderNode that will clip the child to the area given by clip .

;;;Parameters
;;;child

;;;The node to draw

;;;
;;;clip

;;;The clip to apply

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer none][type GskRoundedClipNode]

;;;gsk_rounded_clip_node_get_child ()
;;;GskRenderNode *
;;;gsk_rounded_clip_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting clipped by the given node .

;;;Parameters
;;;node

;;;a rounded clip GskRenderNode.

;;;[type GskRoundedClipNode]
;;;Returns
;;;The child that is getting clipped.

;;;[transfer none]

;;;gsk_rounded_clip_node_get_clip ()
;;;const GskRoundedRect *
;;;gsk_rounded_clip_node_get_clip (GskRenderNode *node);
;;;Retrievs the rounded rectangle used to clip the contents of the node .

;;;Parameters
;;;node

;;;a rounded clip GskRenderNode.

;;;[type GskRoundedClipNode]
;;;Returns
;;;a rounded rectangle.

;;;[transfer none]

;;;gsk_shadow_node_new ()
;;;GskRenderNode *
;;;gsk_shadow_node_new (GskRenderNode *child,
;;;                     const GskShadow *shadows,
;;;                     gsize n_shadows);
;;;Creates a GskRenderNode that will draw a child with the given shadows below it.

;;;Parameters
;;;child

;;;The node to draw

;;;
;;;shadows

;;;The shadows to apply.

;;;[array length=n_shadows]
;;;n_shadows

;;;number of entries in the shadows array

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskShadowNode]

;;;gsk_shadow_node_get_shadow ()
;;;const GskShadow *
;;;gsk_shadow_node_get_shadow (GskRenderNode *node,
;;;                            gsize i);
;;;Retrieves the shadow data at the given index i .

;;;Parameters
;;;node

;;;a shadow GskRenderNode.

;;;[type GskShadowNode]
;;;i

;;;the given index

;;;
;;;Returns
;;;the shadow data.

;;;[transfer none]

;;;gsk_shadow_node_get_n_shadows ()
;;;gsize
;;;gsk_shadow_node_get_n_shadows (GskRenderNode *node);
;;;Retrieves the number of shadows in the node .

;;;Parameters
;;;node

;;;a shadow GskRenderNode.

;;;[type GskShadowNode]
;;;Returns
;;;the number of shadows.

;;;gsk_shadow_node_get_child ()
;;;GskRenderNode *
;;;gsk_shadow_node_get_child (GskRenderNode *node);
;;;Retrieves the child GskRenderNode of the shadow node .

;;;Parameters
;;;node

;;;a shadow GskRenderNode.

;;;[type GskShadowNode]
;;;Returns
;;;the child render node.

;;;[transfer none]

;;;gsk_blend_node_new ()
;;;GskRenderNode *
;;;gsk_blend_node_new (GskRenderNode *bottom,
;;;                    GskRenderNode *top,
;;;                    GskBlendMode blend_mode);
;;;Creates a GskRenderNode that will use blend_mode to blend the top node onto the bottom node.

;;;Parameters
;;;bottom

;;;The bottom node to be drawn

;;;
;;;top

;;;The node to be blended onto the bottom node

;;;
;;;blend_mode

;;;The blend mode to use

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskBlendNode]

;;;gsk_blend_node_get_bottom_child ()
;;;GskRenderNode *
;;;gsk_blend_node_get_bottom_child (GskRenderNode *node);
;;;Retrieves the bottom GskRenderNode child of the node .

;;;Parameters
;;;node

;;;a blending GskRenderNode.

;;;[type GskBlendNode]
;;;Returns
;;;the bottom child node.

;;;[transfer none]

;;;gsk_blend_node_get_top_child ()
;;;GskRenderNode *
;;;gsk_blend_node_get_top_child (GskRenderNode *node);
;;;Retrieves the top GskRenderNode child of the node .

;;;Parameters
;;;node

;;;a blending GskRenderNode.

;;;[type GskBlendNode]
;;;Returns
;;;the top child node.

;;;[transfer none]

;;;gsk_blend_node_get_blend_mode ()
;;;GskBlendMode
;;;gsk_blend_node_get_blend_mode (GskRenderNode *node);
;;;Retrieves the blend mode used by node .

;;;Parameters
;;;node

;;;a blending GskRenderNode.

;;;[type GskBlendNode]
;;;Returns
;;;the blend mode

;;;gsk_cross_fade_node_new ()
;;;GskRenderNode *
;;;gsk_cross_fade_node_new (GskRenderNode *start,
;;;                         GskRenderNode *end,
;;;                         float progress);
;;;Creates a GskRenderNode that will do a cross-fade between start and end .

;;;Parameters
;;;start

;;;The start node to be drawn

;;;
;;;end

;;;The node to be cross_fadeed onto the start node

;;;
;;;progress

;;;How far the fade has progressed from start to end. The value will be clamped to the range [0 ... 1]

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskCrossFadeNode]

;;;gsk_cross_fade_node_get_start_child ()
;;;GskRenderNode *
;;;gsk_cross_fade_node_get_start_child (GskRenderNode *node);
;;;Retrieves the child GskRenderNode at the beginning of the cross-fade.

;;;Parameters
;;;node

;;;a cross-fading GskRenderNode.

;;;[type GskCrossFadeNode]
;;;Returns
;;;a GskRenderNode.

;;;[transfer none]

;;;gsk_cross_fade_node_get_end_child ()
;;;GskRenderNode *
;;;gsk_cross_fade_node_get_end_child (GskRenderNode *node);
;;;Retrieves the child GskRenderNode at the end of the cross-fade.

;;;Parameters
;;;node

;;;a cross-fading GskRenderNode.

;;;[type GskCrossFadeNode]
;;;Returns
;;;a GskRenderNode.

;;;[transfer none]

;;;gsk_cross_fade_node_get_progress ()
;;;float
;;;gsk_cross_fade_node_get_progress (GskRenderNode *node);
;;;Retrieves the progress value of the cross fade.

;;;Parameters
;;;node

;;;a cross-fading GskRenderNode.

;;;[type GskCrossFadeNode]
;;;Returns
;;;the progress value, between 0 and 1

;;;gsk_text_node_new ()
;;;GskRenderNode *
;;;gsk_text_node_new (PangoFont *font,
;;;                   PangoGlyphString *glyphs,
;;;                   const GdkRGBA *color,
;;;                   const graphene_point_t *offset);
;;;Creates a render node that renders the given glyphs, Note that color may not be used if the font contains color glyphs.

;;;Parameters
;;;font

;;;the PangoFont containing the glyphs

;;;
;;;glyphs

;;;the PangoGlyphString to render

;;;
;;;color

;;;the foreground color to render with

;;;
;;;offset

;;;offset of the baseline

;;;
;;;Returns
;;;a new GskRenderNode.

;;;[nullable][transfer full][type GskTextNode]

;;;gsk_text_node_get_font ()
;;;PangoFont *
;;;gsk_text_node_get_font (GskRenderNode *node);
;;;Returns the font used by the text node .

;;;Parameters
;;;node

;;;The GskRenderNode.

;;;[type GskTextNode]
;;;Returns
;;;the font.

;;;[transfer none]

;;;gsk_text_node_get_glyphs ()
;;;const PangoGlyphInfo *
;;;gsk_text_node_get_glyphs (GskRenderNode *node,
;;;                          guint *n_glyphs);
;;;Retrieves the glyph information in the node .

;;;Parameters
;;;node

;;;a text GskRenderNode.

;;;[type GskTextNode]
;;;n_glyphs

;;;the number of glyphs returned.

;;;[out][optional]
;;;Returns
;;;the glyph information.

;;;[transfer none][array length=n_glyphs]

;;;gsk_text_node_get_color ()
;;;const GdkRGBA *
;;;gsk_text_node_get_color (GskRenderNode *node);
;;;Retrieves the color used by the text node .

;;;Parameters
;;;node

;;;a text GskRenderNode.

;;;[type GskTextNode]
;;;Returns
;;;the text color.

;;;[transfer none]

;;;gsk_text_node_has_color_glyphs ()
;;;gboolean
;;;gsk_text_node_has_color_glyphs (GskRenderNode *node);
;;;Checks whether the text node has color glyphs.

;;;Parameters
;;;node

;;;a text GskRenderNode.

;;;[type GskTextNode]
;;;Returns
;;;TRUE if the text node has color glyphs

;;;gsk_text_node_get_num_glyphs ()
;;;guint
;;;gsk_text_node_get_num_glyphs (GskRenderNode *node);
;;;Retrieves the number of glyphs in the text node.

;;;Parameters
;;;node

;;;a text GskRenderNode.

;;;[type GskTextNode]
;;;Returns
;;;the number of glyphs

;;;gsk_text_node_get_offset ()
;;;const graphene_point_t *
;;;gsk_text_node_get_offset (GskRenderNode *node);
;;;Retrieves the offset applied to the text.

;;;Parameters
;;;node

;;;a text GskRenderNode.

;;;[type GskTextNode]
;;;Returns
;;;a point with the horizontal and vertical offsets.

;;;[transfer none]

;;;gsk_blur_node_new ()
;;;GskRenderNode *
;;;gsk_blur_node_new (GskRenderNode *child,
;;;                   float radius);
;;;Creates a render node that blurs the child.

;;;Parameters
;;;child

;;;the child node to blur

;;;
;;;radius

;;;the blur radius

;;;
;;;Returns
;;;a new GskRenderNode.

;;;[transfer full][type GskBlurNode]

;;;gsk_blur_node_get_child ()
;;;GskRenderNode *
;;;gsk_blur_node_get_child (GskRenderNode *node);
;;;Retrieves the child GskRenderNode of the blur node .

;;;Parameters
;;;node

;;;a blur GskRenderNode.

;;;[type GskBlurNode]
;;;Returns
;;;the blurred child node.

;;;[transfer none]

;;;gsk_blur_node_get_radius ()
;;;float
;;;gsk_blur_node_get_radius (GskRenderNode *node);
;;;Retrieves the blur radius of the node .

;;;Parameters
;;;node

;;;a blur GskRenderNode.

;;;[type GskBlurNode]
;;;Returns
;;;the blur radius

;;;gsk_debug_node_new ()
;;;GskRenderNode *
;;;gsk_debug_node_new (GskRenderNode *child,
;;;                    char *message);
;;;Creates a GskRenderNode that will add debug information about the given child .

;;;Adding this node has no visual effect.

;;;Parameters
;;;child

;;;The child to add debug info for

;;;
;;;message

;;;The debug message.

;;;[transfer full]
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskDebugNode]

;;;gsk_debug_node_get_child ()
;;;GskRenderNode *
;;;gsk_debug_node_get_child (GskRenderNode *node);
;;;Gets the child node that is getting drawn by the given node .

;;;Parameters
;;;node

;;;a debug GskRenderNode.

;;;[type GskDebugNode]
;;;Returns
;;;the child GskRenderNode.

;;;[transfer none]

;;;gsk_debug_node_get_message ()
;;;const char *
;;;gsk_debug_node_get_message (GskRenderNode *node);
;;;Gets the debug message that was set on this node

;;;Parameters
;;;node

;;;a debug GskRenderNode.

;;;[type GskDebugNode]
;;;Returns
;;;The debug message.

;;;[transfer none]

;;;gsk_gl_shader_node_new ()
;;;GskRenderNode *
;;;gsk_gl_shader_node_new (GskGLShader *shader,
;;;                        const graphene_rect_t *bounds,
;;;                        GBytes *args,
;;;                        GskRenderNode **children,
;;;                        guint n_children);
;;;Creates a GskRenderNode that will render the given shader into the area given by bounds . The args is a block of data to use for uniform input, as per types and offsets defined by the shader . Normally this is generated by gsk_gl_shader_format_args() or GskGLShaderArgBuilder.

;;;See GskGLShader for details about how the shader should be written.

;;;All the children will be rendered into textures (if they aren't already GskTextureNodes, which will be used directly). These textures will be sent as input to the shader.

;;;If the renderer doesn't support GL shaders, or if there is any problem when compiling the shader, then the node will draw pink. You should use gsk_gl_shader_compile() to ensure the shader will work for the renderer before using it.

;;;Parameters
;;;shader

;;;the GskGLShader

;;;
;;;bounds

;;;the rectangle to render the shader into

;;;
;;;args

;;;Arguments for the uniforms

;;;
;;;children

;;;array of child nodes, these will be rendered to textures and used as input.

;;;[array length=n_children]
;;;n_children

;;;Length of children (currenly the GL backend supports up to 4 children)

;;;
;;;Returns
;;;A new GskRenderNode.

;;;[transfer full][type GskGLShaderNode]

;;;gsk_gl_shader_node_get_n_children ()
;;;guint
;;;gsk_gl_shader_node_get_n_children (GskRenderNode *node);
;;;Returns the number of children

;;;Parameters
;;;node

;;;a GskRenderNode for a gl shader.

;;;[type GskGLShaderNode]
;;;Returns
;;;The number of children

;;;gsk_gl_shader_node_get_child ()
;;;GskRenderNode *
;;;gsk_gl_shader_node_get_child (GskRenderNode *node,
;;;                              guint idx);
;;;Gets one of the children.

;;;Parameters
;;;node

;;;a GskRenderNode for a gl shader.

;;;[type GskGLShaderNode]
;;;idx

;;;the position of the child to get

;;;
;;;Returns
;;;the idx 'th child of node .

;;;[transfer none]

;;;gsk_gl_shader_node_get_args ()
;;;GBytes *
;;;gsk_gl_shader_node_get_args (GskRenderNode *node);
;;;Gets args for the node.

;;;Parameters
;;;node

;;;a GskRenderNode for a gl shader.

;;;[type GskGLShaderNode]
;;;Returns
;;;A GBytes with the uniform arguments.

;;;[transfer none]

;;;gsk_gl_shader_node_get_shader ()
;;;GskGLShader *
;;;gsk_gl_shader_node_get_shader (GskRenderNode *node);
;;;Gets shader code for the node.

;;;Parameters
;;;node

;;;a GskRenderNode for a gl shader.

;;;[type GskGLShaderNode]
;;;Returns
;;;the GskGLShader shader.

;;;[transfer none]


;;; --- End of file gsk.render-node.lisp ---------------------------------------
