;;; ----------------------------------------------------------------------------
;;; gsk.render-node.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;;     GskMaskMode                                        Since 4.10
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
;;;     GskGLShaderNode
;;;     GskTextureScaleNode                                Since 4.10
;;;     GskMaskNode                                        Since 4.10
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
;;;     gsk_gl_shader_node_new
;;;     gsk_gl_shader_node_get_n_children
;;;     gsk_gl_shader_node_get_child
;;;     gsk_gl_shader_node_get_args
;;;     gsk_gl_shader_node_get_shader
;;;     gsk_texture_scale_node_new
;;;     gsk_texture_scale_node_get_filter
;;;     gsk_texture_scale_node_get_texture
;;;     gsk_mask_node_new
;;;     gsk_mask_node_get_mask
;;;     gsk_mask_node_get_mask_mode
;;;     gsk_mask_node_get_source
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

(gobject:define-g-enum "GskRenderNodeType" render-node-type
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
  :mask-node)

#+liber-documentation
(setf (liber:alias-for-symbol 'render-node-type)
      "GEnum"
      (liber:symbol-documentation 'render-node-type)
 "@version{2023-9-22}
  @begin{short}
    The type of a node determines what the node is rendering.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GskRenderNodeType\" render-node-type
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
  :mask-node)
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
    @entry[:texture-scale-node]{A node drawing a @class{gdk:texture} object
      scaled and filtered. Since 4.10}
    @entry[:mask-node]{A node that masks one child with another. Since 4.10}
  @end{table}
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
;;; enum GskScalingFilter
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GskScalingFilter" scaling-filter
  (:export t
   :type-initializer "gsk_scaling_filter_get_type")
  (:linear 0)
  (:nearest 1)
  (:trilinear 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'scaling-filter)
      "GEnum"
      (liber:symbol-documentation 'scaling-filter)
 "@version{2023-9-22}
  @begin{short}
    The filters used when scaling texture data.
  @end{short}
  The actual implementation of each filter is deferred to the rendering
  pipeline.
  @begin{pre}
(gobject:define-g-enum \"GskScalingFilter\" scaling-filter
  (:export t
   :type-initializer \"gsk_scaling_filter_get_type\")
  (:linear 0)
  (:nearest 1)
  (:trilinear 2))
  @end{pre}
  @begin[code]{table}
    @entry[:linear]{Linear interpolation filter.}
    @entry[:nearest]{Nearest neighbor interpolation filter.}
    @entry[:trilinear]{Linear interpolation along each axis, plus mipmap
      generation, with linear interpolation along the mipmap levels.}
  @end{table}
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
;;; enum GskBlendMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GskBlendMode" blend-mode
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
 "@version{2023-10-26}
  @begin{short}
    The blend modes available for render nodes.
  @end{short}
  The implementation of each blend mode is deferred to the rendering pipeline.
  See @url[https://www.w3.org/TR/compositing-1/]{Composting and Blending} for
  more information on blending and blend modes.
  @begin{pre}
(gobject:define-g-enum \"GskBlendMode\" blend-mode
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
  @end{pre}
  @begin[code]{table}
    @entry[:default]{The default blend mode, which specifies no blending.}
    @entry[:multiply]{The source color is multiplied by the destination and
      replaces the destination.}
    @entry[:screen]{Multiplies the complements of the destination and source
      color values, then complements the result.}
    @entry[:overlay]{Multiplies or screens the colors, depending on the
      destination color value. This is the inverse of hard-list.}
    @entry[:darken]{Selects the darker of the destination and source colors.}
    @entry[:ligthen]{Selects the lighter of the destination and source colors.}
    @entry[:color-dodge]{Brightens the destination color to reflect the source
      color.}
    @entry{:color-burn]{Darkens the destination color to reflect the source
      color.}
    @entry[:hard-ligth]{Multiplies or screens the colors, depending on the
      source color value.}
    @entry[:soft-ligth]{Darkens or lightens the colors, depending on the source
      color value.}
    @entry[:difference]{Subtracts the darker of the two constituent colors from
      the lighter color.}
    @entry[:exclusion]{Produces an effect similar to that of the difference mode
      but lower in contrast.}
    @entry[:color]{Creates a color with the hue and saturation of the source
      color and the luminosity of the destination color.}
    @entry[:hue]{Creates a color with the hue of the source color and the
      saturation and luminosity of the destination color.}
    @entry[:saturation]{Creates a color with the saturation of the source color
      and the hue and luminosity of the destination color.}
    @entry[:luminosity]{Creates a color with the luminosity of the source color
      and the hue and saturation of the destination color.}
  @end{table}
  @see-class{gsk:render-node}")

;;; ----------------------------------------------------------------------------
;;; enum GskMaskMode
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(gobject:define-g-enum "GskMaskMode" mask-mode
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
 "@version{2023-10-26}
  @begin{short}
    The mask modes available for mask nodes.
  @end{short}

  @begin{pre}
(gobject:define-g-enum \"GskMaskMode\" mask-mode
  (:export t
   :type-initializer \"gsk_mask_mode_get_type\")
  (:alpha 0)
  (:inverted-alpha 1)
  (:luminance 2)
  (:inverted-luminace 3))
  @end{pre}
  @begin[code]{table}
    @entry[:alpha]{Use the alpha channel of the mask.}
    @entry[inverted-alpha]{Use the inverted alpha channel of the mask.}
    @entry[:luminance]{Use the luminance of the mask, multiplied by mask alpha.}
    @entry[:inverted-luminance]{Use the inverted luminance of the mask,
      multiplied by mask alpha.}
  @end{table}
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
 "@version{2023-10-25}
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
;;; gsk_render_node_ref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_ref" render-node-ref) render-node
 #+liber-documentation
 "@version{2023-10-25}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{The @class{gsk:render-node} instance with an additional reference}
  @short{Acquires a reference on the given render node.}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'render-node-ref)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_unref ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_unref" render-node-unref) :void
 #+liber-documentation
 "@version{2023-10-25}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{short}
    Releases a reference on the given render node.
  @end{short}
  If the reference was the last, the resources associated to the node are freed.
  @see-class{gsk:render-node}"
  (node render-node))

(export 'render-node-unref)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_node_type ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_get_node_type" render-node-node-type)
    render-node-type
 #+liber-documentation
 "@version{2023-10-26}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{A @symbol{gsk:render-node-type} value with the type of @arg{node}.}
  @short{Returns the type of the render node.}
  @see-class{gsk:render-node}
  @see-symbol{gsk:render-node-type}"
  (node render-node))

(export 'render-node-node-type)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_draw ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_draw" render-node-draw) :void
 #+liber-documentation
 "@version{2023-10-26}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[cr]{a @symbol{cairo:context-t} instance to draw to}
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
;;; gsk_render_node_serialize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_serialize" render-node-serialize)
    (g:boxed g:bytes)
 #+liber-documentation
 "@version{2023-10-26}
  @argument[node]{a @class{gsk:render-node} instance}
  @return{A @class{g:bytes} instance representing the node.}
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
;;; gsk_render_node_deserialize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_deserialize" %render-node-deserialize)
    render-node
  (bytes (g:boxed g:bytes))
  (errfunc :pointer)
  (data :pointer))

(defun render-node-deserialize (bytes)
 #+liber-documentation
 "@version{2023-10-26}
  @argument[bytes]{a @class{g:bytes} instance containing the data}
  @return{A new @class{gsk:render-node} instance, or @code{nil} on error.}
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
;;; gsk_render_node_write_to_file ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_write_to_file" %render-node-write-to-file)
    :boolean
  (node render-node)
  (filename :string)
  (err :pointer))

(defun render-node-write-to-file (node filename)
 #+liber-documentation
 "@version{2023-10-26}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[filename]{a namestring or path with the file to save it to}
  @begin{short}
    This function is mostly intended for use inside a debugger to quickly dump
    a render node to a file for later inspection.
  @end{short}
  @begin[Example]{dictionary}
    Example output for a @class{gsk:color-node} instance.
    @begin{pre}
color {
  bounds: 0 0 10 20;
  color: rgb(255,0,0);
@}
    @end{pre}
  @end{dictionary}
  @see-class{gsk:render-node}"
  (glib:with-g-error (err)
    (%render-node-write-to-file node (namestring filename) err)))

(export 'render-node-write-to-file)

;;; ----------------------------------------------------------------------------
;;; gsk_render_node_get_bounds ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_render_node_get_bounds" %render-node-bounds) :void
  (node render-node)
  (bounds (:pointer (:struct graphene:rect-t))))

(defun render-node-bounds (node bounds)
 #+liber-documentation
 "@version{2023-10-26}
  @argument[node]{a @class{gsk:render-node} instance}
  @argument[bounds]{a @symbol{graphene:rect-t} instance}
  @return{The @arg{bounds} value with the boundaries.}
  @begin{short}
    Retrieves the boundaries of the render node.
  @end{short}
  The render node will not draw outside of its boundaries.
  @see-class{gsk:render-node}"
  (%render-node-bounds node bounds)
  bounds)

(export 'render-node-bounds)

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
 "@version{#2023-10-27}
  @begin{short}
    A render node that can contain other render nodes.
  @end{short}
  @see-class{gsk:render-node}")

(export 'container-node)

;;; ----------------------------------------------------------------------------
;;; gsk_container_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_new" %container-node-new) render-node
  (children :pointer)
  (n-children :uint))

(defun container-node-new (children)
 #+liber-documentation
 "@version{#2023-11-6}
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
;;; gsk_container_node_get_n_children ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_get_n_children" container-node-n-children)
    :uint
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[node]{a @class{gsk:container-node} instance}
  @return{An unsigned integer with the number of children of @arg{node}.}
  @begin{short}
    Retrieves the number of direct children of @arg{node}.
  @end{short}
  @see-class{gsk:container-node}"
  (node render-node))

(export 'container-node-n-children)

;;; ----------------------------------------------------------------------------
;;; gsk_container_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_container_node_get_child" container-node-child) render-node
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[node]{a @class{gsk:container-node} instance}
  @argument[index]{an unsigned integer with the position of the child to get}
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
 "@version{2023-10-27}
  @begin{short}
    A render node for a Cairo surface.
  @end{short}
  @see-class{gsk:render-node}")

(export 'cairo-node)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_new" cairo-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the rectangle to
    render to}
  @return{A new @class{gsk:render-node} instance.}
  @begin{short}
    Creates a render node that will render a Cairo surface into the area given
    by @arg{bounds}.
  @end{short}
  You can draw to the Cairo surface using the @fun{gsk:cairo-node-draw-context}
  function.
  @see-class{gsk:cairo-node}
  @see-class{gsk:render-node}
  @see-function{gsk:cairo-node-draw-context}"
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'cairo-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_get_draw_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_get_draw_context" cairo-node-draw-context)
    (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:cairo-node} instance for a Cairo surface}
  @return{A Cairo context used for drawing, use the @fun{cairo:destroy} function
    when done drawing.}
  @begin{short}
    Creates a Cairo context for drawing using the surface associated to the
    render node.
  @end{short}
  If no surface exists yet, a surface will be created optimized for rendering.
  @see-class{gsk:cairo-node}
  @see-symbol{cairo:context-t}"
  (node cairo-node))

(export 'cairo-node-draw-context)

;;; ----------------------------------------------------------------------------
;;; gsk_cairo_node_get_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cairo_node_get_surface" cairo-node-surface)
    (:pointer (:struct cairo:surface-t))
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:cairo-node} instance for a Cairo surface}
  @return{A @symbol{cairo:surface-t} instance with a Cairo surface.}
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
 "@version{#2023-10-25}
  @begin{short}
    A render node for a solid color.
  @end{short}
  @see-class{gsk:render-node}")

(export 'color-node)

;;; ----------------------------------------------------------------------------
;;; gsk_color_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_node_new" color-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-25}
  @argument[rgba]{a @class{gdk:rgba} instance specifying a color}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the rectangle to
    render the color into}
  @return{A new @class{gsk:color-node} instance.}
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
;;; gsk_color_node_get_color ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_node_get_color" color-node-color) (g:boxed gdk:rgba)
 #+liber-documentation
 "@version{#2023-10-25}
  @argument[node]{a @class{gsk:color-node} instance}
  @return{A @class{gdk:rgba} instance with the color of the render node}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node for a linear gradient.
  @end{short}
  @see-class{gsk:render-node}")

(export 'linear-gradient-node)

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

(cffi:defcfun ("gsk_linear_gradient_node_new" %linear-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (color-stops :pointer)
  (n-stops :size))

(defun linear-gradient-node-new (bounds start end color-stops)
  (let ((n-stops (length color-stops)))
    (cffi:with-foreign-object (color-stops-ptr '(:struct %color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in color-stops)
            (for ptr = (cffi:mem-aptr color-stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%linear-gradient-node-new bounds start end color-stops-ptr n-stops))))

(export 'linear-gradient-node-new)

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

(cffi:defcfun ("gsk_linear_gradient_node_get_start" linear-gradient-node-start)
    (:pointer (:struct graphene:point-t))
  (node render-node))

(export 'linear-gradient-node-start)

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

(cffi:defcfun ("gsk_linear_gradient_node_get_end" linear-gradient-node-end)
    (:pointer (:struct graphene:point-t))
  (node render-node))

(export 'linear-gradient-node-end)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_n_color_stops ()
;;;
;;; gsize
;;; gsk_linear_gradient_node_get_n_color_stops
;;;                                (GskRenderNode *node);
;;;
;;; Retrieves the number of color stops in the gradient.
;;;
;;; node
;;;     a GskRenderNode for a linear gradient.
;;;
;;; Returns
;;;     the number of color stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_n_color_stops"
               linear-gradient-node-n-color-stops) :size
  (node render-node))

(export 'linear-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_linear_gradient_node_get_color_stops ()
;;;
;;; const GskColorStop *
;;; gsk_linear_gradient_node_get_color_stops (GskRenderNode *node,
;;;                                           gsize *n_stops);
;;;
;;; Retrieves the color stops in the gradient.
;;;
;;; node
;;;     a GskRenderNode for a linear gradient.
;;;
;;; n_stops
;;;     the number of color stops in the returned array.
;;;
;;; Returns
;;;     the color stops in the gradient.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_linear_gradient_node_get_color_stops"
               %linear-gradient-node-color-stops) :pointer
  (node render-node)
  (n-stops (:pointer :size)))

(defun linear-gradient-node-color-stops (node)
  (cffi:with-foreign-object (n-stops :size)
    (let ((color-stops-ptr (%linear-gradient-node-color-stops node n-stops)))
      (iter (for i from 0 below (cffi:mem-ref n-stops :size))
            (for ptr = (cffi:mem-aptr color-stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'linear-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; GskRepeatingLinearGradientNode
;;;
;;; typedef struct _GskRepeatingLinearGradientNode
;;;                 GskRepeatingLinearGradientNode;
;;;
;;; A render node for a repeating linear gradient.
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type repeating-linear-gradient-node (render-node)
  ()
  (:simple-parser repeating-linear-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'repeating-linear-gradient-node)
      "GskRenderNode"
      (documentation 'repeating-linear-gradient-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node for a repeating linear gradient.
  @end{short}
  @see-class{gsk:render-node}")

(export 'repeating-linear-gradient-node)

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

(cffi:defcfun ("gsk_repeating_linear_gradient_node_new"
               %repeating-linear-gradient-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (color-stops :pointer)
  (n-stops :size))

(defun repeating-linear-gradient-node-new (bounds start end color-stops)
  (let ((n-stops (length color-stops)))
    (cffi:with-foreign-object (color-stops-ptr '(:struct %color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in color-stops)
            (for ptr = (cffi:mem-aptr color-stops-ptr
                                      '(:struct %color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%repeating-linear-gradient-node-new bounds
                                           start end color-stops-ptr n-stops))))

(export 'repeating-linear-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; GskRadialGradientNode
;;;
;;; typedef struct _GskRadialGradientNode GskRadialGradientNode;
;;;
;;; A render node for a radial gradient.
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type radial-gradient-node (render-node)
  ()
  (:simple-parser radial-gradient-node))

#+liber-documentation
(setf (liber:alias-for-class 'radial-gradient-node)
      "GskRenderNode"
      (documentation 'radial-gradient-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node for a radial gradient.
  @end{short}
  @see-class{gsk:render-node}")

(export 'radial-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_radial_gradient_node_new (const graphene_rect_t *bounds,
;;;                               const graphene_point_t *center,
;;;                               float hradius,
;;;                               float vradius,
;;;                               float start,
;;;                               float end,
;;;                               const GskColorStop *color_stops,
;;;                               gsize n_color_stops);
;;;
;;; Creates a GskRenderNode that draws a radial gradient. The radial gradient
;;; starts around center . The size of the gradient is dictated by hradius in
;;; horizontal orientation and by vradius in vertial orientation.
;;;
;;; bounds
;;;     the bounds of the node
;;;
;;; center
;;;     the center of the gradient
;;;
;;; hradius
;;;     the horizontal radius
;;;
;;; vradius
;;;     the vertical radius
;;;
;;; start
;;;     a percentage >= 0 that defines the start of the gradient around center
;;;
;;; end
;;;     a percentage >= 0 that defines the end of the gradient around center
;;;
;;; color_stops
;;;     a pointer to an array of GskColorStop defining the gradient The offsets
;;;     of all color steps must be increasing. The first stop's offset must be
;;;     >= 0 and the last stop's offset must be <= 1.
;;;
;;; n_color_stops
;;;     the number of elements in color_stops
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_new" %radial-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (color-stops :pointer)
  (n-stops :size))

(defun radial-gradient-node-new (bounds
                                 center
                                 hradius vradius
                                 start end
                                 color-stops)
  (let ((n-stops (length color-stops)))
    (cffi:with-foreign-object (color-stops-ptr '(:struct %color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in color-stops)
            (for ptr = (cffi:mem-aptr color-stops-ptr
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
                                 color-stops-ptr
                                 n-stops))))

(export 'radial-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_n_color_stops ()
;;;
;;; gsize
;;; gsk_radial_gradient_node_get_n_color_stops (GskRenderNode *node);
;;;
;;; Retrieves the number of color stops in the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the number of color stops
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_n_color_stops"
               radial-gradient-node-n-color-stops) :size
  (node render-node))

(export 'radial-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_color_stops ()
;;;
;;; const GskColorStop *
;;; gsk_radial_gradient_node_get_color_stops
;;;                                (GskRenderNode *node,
;;;                                 gsize *n_stops);
;;;
;;; Retrieves the color stops in the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; n_stops
;;;     the number of color stops in the returned array.
;;;
;;; Returns
;;;     the color stops in the gradient.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_color_stops"
               %radial-gradient-node-color-stops) :pointer
  (node render-node)
  (n-stops (:pointer :size)))

(defun radial-gradient-node-color-stops (node)
  (cffi:with-foreign-object (n-stops :size)
    (let ((color-stops-ptr (%radial-gradient-node-color-stops node n-stops)))
      (iter (for i from 0 below (cffi:mem-ref n-stops :size))
            (for ptr = (cffi:mem-aptr color-stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'radial-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_start ()
;;;
;;; float
;;; gsk_radial_gradient_node_get_start (GskRenderNode *node);
;;;
;;; Retrieves the start value for the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the start value for the gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_start" radial-gradient-node-start)
    :float
  (node render-node))

(export 'radial-gradient-node-start)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_end ()
;;;
;;; float
;;; gsk_radial_gradient_node_get_end (GskRenderNode *node);
;;;
;;; Retrieves the end value for the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the end value for the gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_end" radial-gradient-node-end)
    :float
  (node render-node))

(export 'radial-gradient-node-end)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_hradius ()
;;;
;;; float
;;; gsk_radial_gradient_node_get_hradius (GskRenderNode *node);
;;;
;;; Retrieves the horizonal radius for the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the horizontal radius for the gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_hradius"
               radial-gradient-node-hradius) :float
  (node render-node))

(export 'radial-gradient-node-hradius)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_vradius ()
;;;
;;; float
;;; gsk_radial_gradient_node_get_vradius (GskRenderNode *node);
;;;
;;; Retrieves the vertical radius for the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the vertical radius for the gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_vradius"
               radial-gradient-node-vradius) :float
  (node render-node))

(export 'radial-gradient-node-vradius)

;;; ----------------------------------------------------------------------------
;;; gsk_radial_gradient_node_get_center ()
;;;
;;; const graphene_point_t *
;;; gsk_radial_gradient_node_get_center (GskRenderNode *node);
;;;
;;; Retrieves the center pointer for the gradient.
;;;
;;; node
;;;     a GskRenderNode for a radial gradient.
;;;
;;; Returns
;;;     the center point for the gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_radial_gradient_node_get_center"
               radial-gradient-node-center)
    (:pointer (:struct graphene:point-t))
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
 "@version{#2023-10-27}
  @begin{short}
    A render node for a repeating radial gradient.
  @end{short}
  @see-class{gsk:render-node}")

(export 'repeating-radial-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_repeating_radial_gradient_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_repeating_radial_gradient_node_new
;;;                                (const graphene_rect_t *bounds,
;;;                                 const graphene_point_t *center,
;;;                                 float hradius,
;;;                                 float vradius,
;;;                                 float start,
;;;                                 float end,
;;;                                 const GskColorStop *color_stops,
;;;                                 gsize n_color_stops);
;;;
;;; Creates a GskRenderNode that draws a repeating radial gradient. The radial
;;; gradient starts around center . The size of the gradient is dictated by
;;; hradius in horizontal orientation and by vradius in vertial orientation.
;;;
;;; bounds
;;;     the bounds of the node
;;;
;;; center
;;;     the center of the gradient
;;;
;;; hradius
;;;     the horizontal radius
;;;
;;; vradius
;;;     the vertical radius
;;;
;;; start
;;;     a percentage >= 0 that defines the start of the gradient around center
;;;
;;; end
;;;     a percentage >= 0 that defines the end of the gradient around center
;;;
;;; color_stops
;;;     a pointer to an array of GskColorStop defining the gradient The offsets
;;;     of all color steps must be increasing. The first stop's offset must be
;;;     >= 0 and the last stop's offset must be <= 1.
;;;
;;; n_color_stops
;;;     the number of elements in color_stops
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeating_radial_gradient_node_new"
               %repeating-radial-gradient-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (color-stops :pointer)
  (n-stops :size))

(defun repeating-radial-gradient-node-new (bounds
                                           center
                                           hradius vradius
                                           start end
                                           color-stops)
  (let ((n-stops (length color-stops)))
    (cffi:with-foreign-object (color-stops-ptr '(:struct %color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in color-stops)
            (for ptr = (cffi:mem-aptr color-stops-ptr
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
                                           color-stops-ptr
                                           n-stops))))

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
 "@version{#2023-10-27}
  @begin{short}
    A render node for a conic gradient.
  @end{short}
  @see-class{gsk:render-node}")

(export 'conic-gradient-node)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_new" %conic-gradient-node-new)
    render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (rotation :float)
  (stops :pointer)
  (n-stops :size))

(defun conic-gradient-node-new (bounds center rotation color-stops)
 #+liber-documentation
 "@version{#2023-11-24}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds of the
    render node}
  @argument[center]{a @symbol{graphene:point-t} instance with the center of the
    gradient}
  @argument[rotation]{a single float with the rotation of the gradient in
    degrees}
  @argument[stops]{a list with the color stops defining the gradient, the
    offset of all color steps must be increasing, the first stop's offset must
    be >= 0 and the last stop's offset must be <= 1}
  @argument[n-stops]{an integer with the number of elements in @arg{stops}}
  @return{A new @class{gsk:conic-gradient-node} instance.}
  @begin{short}
    Creates a render node that draws a conic gradient.
  @end{short}
  The conic gradient starts around @arg{center} in the direction of
  @arg{rotation}. A rotation of 0 means that the gradient points up. Color stops
  are then added clockwise.
  @see-class{gsk:conic-gradient-node}
  @see-class{gsk:render-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length color-stops)))
    (cffi:with-foreign-object (color-stops-ptr '(:struct %color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in color-stops)
            (for ptr = (cffi:mem-aptr color-stops-ptr
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
                                color-stops-ptr
                                n-stops))))

(export 'conic-gradient-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_n_color_stops ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_n_color_stops"
               conic-gradient-node-n-color-stops) :size
 #+liber-documentation
 "@version{#2023-11-23}
  @argument[node]{a @class{gsk:conic-gradient-node} instance for a conic
    gradient}
  @return{The unsigned integer with the number of color stops.}
  @short{Retrieves the number of color stops in the conic gradient.}
  @see-class{gsk:conic-gradient-node}"
  (node render-node))

(export 'conic-gradient-node-n-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_color_stops ()
;;;
;;; const GskColorStop *
;;; gsk_conic_gradient_node_get_color_stops
;;;                                (GskRenderNode *node,
;;;                                 gsize *n_stops);
;;;
;;; Retrieves the color stops in the gradient.
;;;
;;; node
;;;     a GskRenderNode for a conic gradient.
;;;
;;; n_stops
;;;     the number of color stops in the returned array.
;;;
;;; Returns
;;;     the color stops in the gradient.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_color_stops"
               %conic-gradient-node-color-stops) :pointer
  (node render-node)
  (n-stops (:pointer :size)))

(defun conic-gradient-node-color-stops (node)
  (cffi:with-foreign-object (n-stops :size)
    (let ((color-stops-ptr (%conic-gradient-node-color-stops node n-stops)))
      (iter (for i from 0 below (cffi:mem-ref n-stops :size))
            (for ptr = (cffi:mem-aptr color-stops-ptr
                                      '(:struct %color-stop) i))
            (collect (list (cffi:mem-ref ptr :float)
                           (cffi:convert-from-foreign
                             (cffi:inc-pointer ptr
                                               (cffi:foreign-type-size :float))
                             '(g:boxed gdk:rgba))))))))

(export 'conic-gradient-node-color-stops)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_center ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_center"
               conic-gradient-node-center) (:pointer (:struct graphene:point-t))
 #+liber-documentation
 "@version{#2023-11-23}
  @argument[node]{a @class{gsk:conic-gradient-node} instance for a conic
    gradient}
  @return{The @symbol{graphene:point-t} instance with the center point for the
    gradient.}
  @begin{short}
    Retrieves the center pointer for the gradient.
  @end{short}
  @see-class{gsk:conic-gradient-node}
  @see-symbol{graphene:point-t}"
  (node render-node))

(export 'conic-gradient-node-center)

;;; ----------------------------------------------------------------------------
;;; gsk_conic_gradient_node_get_rotation ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_conic_gradient_node_get_rotation"
               conic-gradient-node-rotation) :float
 #+liber-documentation
 "@version{#2023-11-23}
  @argument[node]{a @symbol{gsk:conic-gradient-node} instance}
  @return{The single float with the rotation for the conic gradient.}
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
 "@version{2023-10-26}
  @begin{short}
    A render node for a border.
  @end{short}
  @see-class{gsk:render-node}")

(export 'border-node)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_new" %border-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (width (:pointer :float))
  (color :pointer))

(defun border-node-new (outline widths colors)
 #+liber-documentation
 "@version{2023-11-19}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance describing the
    outline of the border}
  @argument[widths]{a list with 4 single values with the stroke width of the
    border on the top, right, bottom, and left side respectively}
  @argument[colors]{a list with 4 @class{gdk:rgba} instance with the color
    used on the top, right, bottom, and left side}
  @return{A @class{gsk:border-node} instance.}
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
    (glib:with-g-boxed-array (n-colors colors-ptr gdk:rgba colors)
      (%border-node-new outline widths-ptr colors-ptr))))

(export 'border-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_outline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_outline" border-node-outline)
    (:pointer (:struct rounded-rect))
 #+liber-documentation
 "@version{2023-11-19}
  @argument[node]{a @class{gsk:border-node} instance}
  @return{A @symbol{gsk:rounded-rect} instance with the outline of the border}
  @begin{short}
    Retrieves the outline of the border.
  @end{short}
  @see-class{gsk:border-node}
  @see-symbol{gsk:rounded-rect}"
  (node border-node))

(export 'border-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_widths ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_widths" %border-node-widths)
    (:pointer :float)
  (node border-node))

(defun border-node-widths (node)
 #+liber-documentation
 "@version{2023-11-19}
  @argument[node]{a @class{gsk:border-node} instance for a border}
  @return{A list with 4 single float values for the top, right, bottom, and
    left stroke width of the border, respectively.}
  @begin{short}
    Retrieves the stroke widths of the border.
  @end{short}
  @see-class{gsk:border-node}"
  (let ((widths-ptr (%border-node-widths node)))
    (iter (for i from 0 below 4)
          (collect (cffi:mem-aref widths-ptr :float i)))))

(export 'border-node-widths)

;;; ----------------------------------------------------------------------------
;;; gsk_border_node_get_colors ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_border_node_get_colors" %border-node-colors) :pointer
  (node border-node))

(defun border-node-colors (node)
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:border-node} instance for a border}
  @return{A list with 4 @class{gdk:rgba} instances for the top, right, bottom,
    and left color of the border.}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node for a @class{gdk:texture} object.
  @end{short}
  @see-class{gsk:render-node}
  @see-class{gdk:texture}")

(export 'texture-node)

;;; ----------------------------------------------------------------------------
;;; gsk_texture_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_texture_node_new" texture-node-new) render-node
 #+liber-documentation
 "@version{#2023-11-23}
  @argument[texture]{a @class{gdk:texture} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the rectangle
    to render the texture into}
  @return{The new @symbol{gsk:texure-node} instance.}
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
;;; gsk_texture_node_get_texture ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_texture_node_get_texture" texture-node-texture)
    (g:object gdk:texture)
 #+liber-documentation
 "@version{#2023-11-23}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node for an inset shadow.
  @end{short}
  @see-class{gsk:render-node}")

(export 'inset-shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_inset_shadow_node_new (const GskRoundedRect *outline,
;;;                            const GdkRGBA *color,
;;;                            float dx,
;;;                            float dy,
;;;                            float spread,
;;;                            float blur_radius);
;;;
;;; Creates a GskRenderNode that will render an inset shadow into the box given
;;; by outline .
;;;
;;; outline
;;;     outline of the region containing the shadow
;;;
;;; color
;;;     color of the shadow
;;;
;;; dx
;;;     horizontal offset of shadow
;;;
;;; dy
;;;     vertical offset of shadow
;;;
;;; spread
;;;     how far the shadow spreads towards the inside
;;;
;;; blur_radius
;;;     how much blur to apply to the shadow
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_new" inset-shadow-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(export 'inset-shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_outline ()
;;;
;;; const GskRoundedRect *
;;; gsk_inset_shadow_node_get_outline (GskRenderNode *node);
;;;
;;; Retrieves the outline rectangle of the inset shadow.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     a rounded rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_outline" inset-shadow-node-outline)
    (:pointer (:struct rounded-rect))
  (node render-node))

(export 'inset-shadow-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_color ()
;;;
;;; const GdkRGBA *
;;; gsk_inset_shadow_node_get_color (GskRenderNode *node);
;;;
;;; Retrieves the color of the inset shadow.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     the color of the shadow.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_color" inset-shadow-node-color)
    (g:boxed gdk:rgba)
  (node render-node))

(export 'inset-shadow-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_dx ()
;;;
;;; float
;;; gsk_inset_shadow_node_get_dx (GskRenderNode *node);
;;;
;;; Retrieves the horizontal offset of the inset shadow.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     an offset, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_dx" inset-shadow-node-dx) :float
  (node render-node))

(export 'inset-shadow-node-dx)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_dy ()
;;;
;;; float
;;; gsk_inset_shadow_node_get_dy (GskRenderNode *node);
;;;
;;; Retrieves the vertical offset of the inset shadow.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     an offset, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_dy" inset-shadow-node-dy) :float
  (node render-node))

(export 'inset-shadow-node-dy)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_spread ()
;;;
;;; float
;;; gsk_inset_shadow_node_get_spread (GskRenderNode *node);
;;;
;;; Retrieves how much the shadow spreads inwards.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     the size of the shadow, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_spread" inset-shadow-node-spread)
    :float
  (node render-node))

(export 'inset-shadow-node-spread)

;;; ----------------------------------------------------------------------------
;;; gsk_inset_shadow_node_get_blur_radius ()
;;;
;;; float
;;; gsk_inset_shadow_node_get_blur_radius (GskRenderNode *node);
;;;
;;; Retrieves the blur radius to apply to the shadow.
;;;
;;; node
;;;     a GskRenderNode for an inset shadow.
;;;
;;; Returns
;;;     the blur radius, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_inset_shadow_node_get_blur_radius"
               inset-shadow-node-blur-radius) :float
  (node render-node))

(export 'inset-shadow-node-blur-radius)

;;; ----------------------------------------------------------------------------
;;; GskOutsetShadowNode
;;;
;;; typedef struct _GskOutsetShadowNode GskOutsetShadowNode;
;;;
;;; A render node for an outset shadow.
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type outset-shadow-node (render-node)
  ()
  (:simple-parser outset-shadow-node))

#+liber-documentation
(setf (liber:alias-for-class 'outset-shadow-node)
      "GskRenderNode"
      (documentation 'outset-shadow-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node for an outset shadow.
  @end{short}
  @see-class{gsk:render-node}")

(export 'outset-shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_outset_shadow_node_new (const GskRoundedRect *outline,
;;;                             const GdkRGBA *color,
;;;                             float dx,
;;;                             float dy,
;;;                             float spread,
;;;                             float blur_radius);
;;;
;;; Creates a GskRenderNode that will render an outset shadow around the box
;;; given by outline .
;;;
;;; outline
;;;     outline of the region surrounded by shadow
;;;
;;; color
;;;     color of the shadow
;;;
;;; dx
;;;     horizontal offset of shadow
;;;
;;; dy
;;;     vertical offset of shadow
;;;
;;; spread
;;;     how far the shadow spreads towards the inside
;;;
;;; blur_radius
;;;     how much blur to apply to the shadow
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_new" outset-shadow-node-new) render-node
  (outline (:pointer (:struct rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(export 'outset-shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_outline ()
;;;
;;; const GskRoundedRect *
;;; gsk_outset_shadow_node_get_outline (GskRenderNode *node);
;;;
;;; Retrieves the outline rectangle of the outset shadow.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     a rounded rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_outline" outset-shadow-node-outline)
    (:pointer (:struct rounded-rect))
  (node render-node))

(export 'outset-shadow-node-outline)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_color ()
;;;
;;; const GdkRGBA *
;;; gsk_outset_shadow_node_get_color (GskRenderNode *node);
;;;
;;; Retrieves the color of the outset shadow.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     a color.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_color" outset-shadow-node-color)
    (g:boxed gdk:rgba)
  (node render-node))

(export 'outset-shadow-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_dx ()
;;;
;;; float
;;; gsk_outset_shadow_node_get_dx (GskRenderNode *node);
;;;
;;; Retrieves the horizontal offset of the outset shadow.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     an offset, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_dx" outset-shadow-node-dx) :float
  (node render-node))

(export 'outset-shadow-node-dx)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_dy ()
;;;
;;; float
;;; gsk_outset_shadow_node_get_dy (GskRenderNode *node);
;;;
;;; Retrieves the vertical offset of the outset shadow.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     an offset, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_dy" outset-shadow-node-dy) :float
  (node render-node))

(export 'outset-shadow-node-dy)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_spread ()
;;;
;;; float
;;; gsk_outset_shadow_node_get_spread (GskRenderNode *node);
;;;
;;; Retrieves how much the shadow spreads outwards.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     the size of the shadow, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_spread" outset-shadow-node-spread)
    :float
  (node render-node))

(export 'outset-shadow-node-spread)

;;; ----------------------------------------------------------------------------
;;; gsk_outset_shadow_node_get_blur_radius ()
;;;
;;; float
;;; gsk_outset_shadow_node_get_blur_radius (GskRenderNode *node);
;;;
;;; Retrieves the blur radius of the shadow.
;;;
;;; node
;;;     a GskRenderNode for an outset shadow.
;;;
;;; Returns
;;;     the blur radius, in pixels
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_outset_shadow_node_get_blur_radius"
               outset-shadow-node-blur-radius) :float
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
 "@version{#2023-10-27}
  @begin{short}
    A render node applying a @symbol{gsk:transform} instance to its single
    child render node.
  @end{short}
  @see-class{gsk:render-node}
  @see-symbol{gsk:transform}")

(export 'transform-node)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_new" transform-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[child]{a @class{gsk:render-node} instance with the render node
    to transform}
  @argument[transform]{a @class{gsk:transform} instance with the transform
    to apply}
  @return{A new @class{gsk:transform-node} instance.}
  @begin{short}
    Creates a render node that will transform the given @arg{child} with the
    given @arg{transform}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-class{gsk:render-node}
  @see-symbol{gsk:transform}"
  (child render-node)
  (transform (g:boxed transform)))

(export 'transform-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_get_child" transform-node-child) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:transform-node} instance}
  @return{A @class{gsk:render-node} instance with the child render node that
    is getting transformed.}
  @begin{short}
    Gets the child render node that is getting transformed by the given
    @arg{node}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-symbol{gsk:render-node}"
  (node render-node))

(export 'transform-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_node_get_transform ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_node_get_transform" transform-node-transform)
    (g:boxed transform)
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:transform-node} instance}
  @return{A @symbol{gsk:transform} instance.}
  @begin{short}
    Retrieves the @symbol{gsk:transform} instance used by @arg{node}.
  @end{short}
  @see-class{gsk:transform-node}
  @see-symbol{gsk:transform}"
  (node render-node))

(export 'transform-node-transform)

;;; ----------------------------------------------------------------------------
;;; GskOpacityNode
;;;
;;; typedef struct _GskOpacityNode GskOpacityNode;
;;;
;;; A render node controlling the opacity of its single child node.
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type opacity-node (render-node)
  ()
  (:simple-parser opacity-node))

#+liber-documentation
(setf (liber:alias-for-class 'opacity-node)
      "GskRenderNode"
      (documentation 'opacity-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node controlling the opacity of its single child render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'opacity-node)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_opacity_node_new (GskRenderNode *child,
;;;                       float opacity);
;;;
;;; Creates a GskRenderNode that will drawn the child with reduced opacity .
;;;
;;; child
;;;     The node to draw
;;;
;;; opacity
;;;     The opacity to apply
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_new" opacity-node-new) render-node
  (child render-node)
  (opacity :float))

(export 'opacity-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_get_child ()
;;;
;;; GskRenderNode *
;;; gsk_opacity_node_get_child (GskRenderNode *node);
;;;
;;; Gets the child node that is getting opacityed by the given node .
;;;
;;; node
;;;     a GskRenderNode for an opacity.
;;;
;;; Returns
;;;     The child that is getting opacityed.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_get_child" opacity-node-child) render-node
  (node render-node))

(export 'opacity-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_opacity_node_get_opacity ()
;;;
;;; float
;;; gsk_opacity_node_get_opacity (GskRenderNode *node);
;;;
;;; Gets the transparency factor for an opacity node.
;;;
;;; node
;;;     a GskRenderNode for an opacity.
;;;
;;; Returns
;;;     the opacity factor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_opacity_node_get_opacity" opacity-node-opacity) :float
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
 "@version{2023-10-27}
  @begin{short}
    A render node controlling the color matrix of its single child render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'color-matrix-node)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_new" color-matrix-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[child]{a @class{gsk:render-node} instance with the child render
    node to draw}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the matrix to
    apply}
  @argument[offset]{a @symbol{graphene:vec4-t} instance with the values to add
    to the color}
  @return{A new @class{gsk:rende-node} instance.}
  @begin{short}
    Creates a render node that will draw the child render node with reduced
    @arg{matrix}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-class{gsk:render-node}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec4-t}"
  (child render-node)
  (matrix (:pointer (:struct graphene:matrix-t)))
  (offset (:pointer (:struct graphene:vec4-t))))

(export 'color-matrix-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_child" color-matrix-node-child)
    render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @return{A @class{gsk:render-node} instance with the child render node that
    is getting its colors modified.}
  @begin{short}
    Gets the child render node that is getting its colors modified by the given
    @arg{node}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'color-matrix-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_color_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_color-matrix"
               color-matrix-node-color-matrix)
    (:pointer (:struct graphene:matrix-t))
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @return{A @symbol{graphene:matrix-t} instance with a 4x4 color matrix.}
  @begin{short}
    Retrieves the color matrix used by @arg{node}.
  @end{short}
  @see-class{gsk:colr-matrix-node}
  @see-symbol{graphene:matrix-t}"
  (node render-node))

(export 'color-matrix-node-color-matrix)

;;; ----------------------------------------------------------------------------
;;; gsk_color_matrix_node_get_color_offset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_color_matrix_node_get_color_offset"
               color-matrix-node-color-offset)
    (:pointer (:struct graphene:vec4-t))
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:color-matrix-node} instance}
  @return{A @symbol{graphene:vec4-t} instance with a color vector}
  @begin{short}
    Retrieves the color offset used by @arg{node}.
  @end{short}
  @see-class{gsk:color-matrix-node}
  @see-symbol{graphene:vec4-t}"
  (node render-node))

(export 'color-matrix-node-color-offset)

;;; ----------------------------------------------------------------------------
;;; GskRepeatNode
;;;
;;; typedef struct _GskRepeatNode GskRepeatNode;
;;;
;;; A render node repeating its single child node.
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type repeat-node (render-node)
  ()
  (:simple-parser repeat-node))

#+liber-documentation
(setf (liber:alias-for-class 'repeat-node)
      "GskRenderNode"
      (documentation 'repeat-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node repeating its single child render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'repeat-node)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_repeat_node_new (const graphene_rect_t *bounds,
;;;                      GskRenderNode *child,
;;;                      const graphene_rect_t *child_bounds);
;;;
;;; Creates a GskRenderNode that will repeat the drawing of child across the
;;; given bounds .
;;;
;;; bounds
;;;     The bounds of the area to be painted
;;;
;;; child
;;;     The child to repeat
;;;
;;; child_bounds
;;;     The area of the child to repeat or NULL to use the child's bounds.
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_new" repeat-node-new) render-node
  (bounds (:pointer (:struct graphene:rect-t)))
  (child render-node)
  (child-bounds (:pointer (:struct graphene:rect-t))))

(export 'repeat-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_get_child ()
;;;
;;; GskRenderNode *
;;; gsk_repeat_node_get_child (GskRenderNode *node);
;;;
;;; Retrieves the child of node .
;;;
;;; node
;;;     a repeat GskRenderNode.
;;;
;;; Returns
;;;     a GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_get_child" repeat-node-child) render-node
  (node render-node))

(export 'repeat-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_repeat_node_get_child_bounds ()
;;;
;;; const graphene_rect_t *
;;; gsk_repeat_node_get_child_bounds (GskRenderNode *node);
;;;
;;; Retrieves the bounding rectangle of the child of node .
;;;
;;; node
;;;     a repeat GskRenderNode.
;;;
;;; Returns
;;;     a bounding rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_repeat_node_get_child_bounds" repeat-node-child-bounds)
    (:pointer (:struct graphene:rect-t))
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
 "@version{2023-10-27}
  @begin{short}
    A render node applying a rectangular clip to its single child render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'clip-node)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_new" clip-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[child]{a @class{gsk:render-node} instance with the render node to
    draw}
  @argument[clip]{a @symbol{graphene:rect-t} instance with the clip to apply}
  @return{A new @class{gsk:render-node} instance.}
  @begin{short}
    Creates a render node that will clip the child render node to the area
    given by @arg{clip}.
  @end{short}
  @see-class{gsk:clip-node}
  @see-class{gsk:render-node}"
  (child render-node)
  (clip (:pointer (:struct graphene:rect-t))))

(export 'clip-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_get_child" clip-node-child) render-node
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:clip-node} instance}
  @return{A @class{gsk:render-node} instance with the child render node that
    is getting clipped.}
  @begin{short}
    Gets the child render node that is getting clipped by the given @arg{node}.
  @end{short}
  @see-class{gsk:clip-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'clip-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_clip_node_get_clip ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_clip_node_get_clip" clip-node-clip)
    (:pointer (:struct graphene:rect-t))
 #+liber-documentation
 "@version{#2023-10-27}
  @argument[node]{a @class{gsk:clip-node} instance}
  @return{A @symbol{graphene:rect-t} instance with a clip rectangle.}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node applying a rounded rectangle clip to its single child
    render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'rounded-clip-node)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_rounded_clip_node_new (GskRenderNode *child,
;;;                            const GskRoundedRect *clip);
;;;
;;; Creates a GskRenderNode that will clip the child to the area given by clip .
;;;
;;; child
;;;     The node to draw
;;;
;;; clip
;;;     The clip to apply
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_new" rounded-clip-node-new) render-node
  (child render-node)
  (clip (:pointer (:struct rounded-rect))))

(export 'rounded-clip-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_get_child ()
;;;
;;; GskRenderNode *
;;; gsk_rounded_clip_node_get_child (GskRenderNode *node);
;;;
;;; Gets the child node that is getting clipped by the given node .
;;;
;;; node
;;;     a rounded clip GskRenderNode.
;;;
;;; Returns
;;;     The child that is getting clipped.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_get_child" rounded-clip-node-child)
    render-node
  (node render-node))

(export 'rounded-clip-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_rounded_clip_node_get_clip ()
;;;
;;; const GskRoundedRect *
;;; gsk_rounded_clip_node_get_clip (GskRenderNode *node);
;;;
;;; Retrievs the rounded rectangle used to clip the contents of the node .
;;;
;;; node
;;;     a rounded clip GskRenderNode.
;;;
;;; Returns
;;;     a rounded rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_rounded_clip_node_get_clip" rounded-clip-node-clip)
    (:pointer (:struct rounded-rect))
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
 "@version{#2023-10-27}
  @begin{short}
    A render node drawing one or more shadows behind its single child render
    node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'shadow-node)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_shadow_node_new (GskRenderNode *child,
;;;                      const GskShadow *shadows,
;;;                      gsize n_shadows);
;;;
;;; Creates a GskRenderNode that will draw a child with the given shadows below
;;; it.
;;;
;;; child
;;;     The node to draw
;;;
;;; shadows
;;;     The shadows to apply.
;;;
;;; n_shadows
;;;     number of entries in the shadows array
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_new" %shadow-node-new) render-node
  (child render-node)
  (shadows :pointer)
  (n-shadows :size))

(defun shadow-node-new (child shadows)
  (let ((n-shadows (length shadows))
        (cstruct-size (cffi:foreign-type-size '(:struct gdk::rgba-cstruct))))
    (cffi:with-foreign-object (shadows-ptr '(:struct %shadow) n-shadows)
      (iter (for i from 0 below n-shadows)
            (for (color dx dy radius) in shadows)
            (for ptr = (cffi:mem-aptr shadows-ptr
                                      '(:struct %shadow) i))
            (glib::copy-boxed-slots-to-foreign
                color
                ptr
                'gdk:rgba)
            (cffi:incf-pointer ptr cstruct-size)
            (setf (cffi:mem-ref ptr :float) (coerce dx 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce dy 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce radius 'single-float)))
      (%shadow-node-new child shadows-ptr n-shadows))))

(export 'shadow-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_shadow ()
;;;
;;; const GskShadow *
;;; gsk_shadow_node_get_shadow (GskRenderNode *node,
;;;                             gsize i);
;;;
;;; Retrieves the shadow data at the given index i .
;;;
;;; node
;;;     a shadow GskRenderNode.
;;;
;;; i
;;;     the given index
;;;
;;; Returns
;;;     the shadow data.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_shadow" %shadow-node-shadow) :pointer
  (node render-node)
  (index :size))

(defun shadow-node-shadow (node index)
  (let ((ptr (%shadow-node-shadow node index))
        (cstruct-size (cffi:foreign-type-size '(:struct gdk::rgba-cstruct)))
        (float-size (cffi:foreign-type-size :float)))
    (list (cffi:convert-from-foreign ptr '(g:boxed gdk:rgba))
          (cffi:mem-ref (cffi:incf-pointer ptr cstruct-size) :float)
          (cffi:mem-ref (cffi:incf-pointer ptr float-size) :float)
          (cffi:mem-ref (cffi:incf-pointer ptr float-size) :float))))

(export 'shadow-node-shadow)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_n_shadows ()
;;;
;;; gsize
;;; gsk_shadow_node_get_n_shadows (GskRenderNode *node);
;;;
;;; Retrieves the number of shadows in the node .
;;;
;;; node
;;;     a shadow GskRenderNode.
;;;
;;; Returns
;;;     the number of shadows.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_n_shadows" shadow-node-n-shadows) :size
  (node render-node))

(export 'shadow-node-n-shadows)

;;; ----------------------------------------------------------------------------
;;; gsk_shadow_node_get_child ()
;;;
;;; GskRenderNode *
;;; gsk_shadow_node_get_child (GskRenderNode *node);
;;;
;;; Retrieves the child GskRenderNode of the shadow node .
;;;
;;; node
;;;     a shadow GskRenderNode.
;;;
;;; Returns
;;;     the child render node.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_shadow_node_get_child" shadow-node-child) render-node
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
 "@version{2023-10-25}
  @begin{short}
    A render node applying a blending function between its two child nodes.
  @end{short}
  @see-class{gsk:render-node}")

(export 'blend-node)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_new" blend-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[bottom]{a @class{gsk:render-node} instance with the bottom node to
    be drawn}
  @argument[top]{a @class{gsk:render-node} instance with the top node to
    be blended onto the bottom node}
  @argument[mode]{a @symbol{gsk:blend-mode} value with the blend mode to use}
  @return{A new @class{gsk:blend-node} instance.}
  @begin{short}
    Creates a render node that will use @arg{mode} to blend the top node onto
    the bottom node.
  @end{short}
  @see-class{gsk:blend-node}
  @see-class{gsk:render-node}"
  (bottom render-node)
  (top render-node)
  (mode blend-mode))

(export 'blend-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_bottom_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_bottom_child" blend-node-bottom-child)
    render-node
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{A @class{gsk:render-node} instance with the bottom child node}
  @begin{short}
    Retrieves the bottom render node child of @arg{node}.
  @end{short}
  @see-class{gsk:blend-mode}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'blend-node-bottom-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_top_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_top_child" blend-node-top-child) render-node
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{A @class{gsk:render-node} instance with the top child node}
  @begin{short}
    Retrieves the top render node child of @arg{node}.
  @end{short}
  @see-class{gsk:blend-mode}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'blend-node-top-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blend_node_get_blend_mode ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blend_node_get_blend_mode" blend-node-blend-mode) blend-mode
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[node]{a @class{gsk:blend-node} instance}
  @return{A @symbol{gsk:blend-mode} value with the blend mode.}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node cross fading between two child render nodes.
  @end{short}
  @see-class{gsk:render-node}")

(export 'cross-fade-node)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_new" cross-fade-node-new) render-node
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[start]{a @class{gsk:render-node} instance with the start render
    node to be drawn}
  @argument[end]{a @class{gsk:render-node} instance with the render node to be
    cross-fadeed onto the start node}
  @argument[progress]{a single float how far the fade has progressed from start
    to end. The value will be clamped to the range [0 ... 1]}
  @return{A new @class{gsk:cross-fade-node} instance.}
  @begin{short}
    Creates a render node that will do a cross-fade between @arg{start} and
    @arg{end}.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (start render-node)
  (end render-node)
  (progress :float))

(export 'cross-fade-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_start_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_start_child"
               cross-fade-node-start-child) render-node
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{A @class{gsk:render-node} instance.}
  @begin{short}
    Retrieves the child render node at the beginning of the cross-fade.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'cross-fade-node-start-child)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_end_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_end_child"
               cross-fade-node-end-child) render-node
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{A @class{gsk:render-node} instance.}
  @begin{short}
    Retrieves the child render node at the end of the cross-fade.
  @end{short}
  @see-class{gsk:cross-fade-node}
  @see-class{gsk:render-node}"
  (node render-node))

(export 'cross-fade-node-end-child)

;;; ----------------------------------------------------------------------------
;;; gsk_cross_fade_node_get_progress ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_cross_fade_node_get_progress" cross-fade-node-progress)
    :float
 #+liber-documentation
 "@version{#2023-11-6}
  @argument[node]{a @class{gsk:cross-fade-node} instance}
  @return{A single float with the progress value, between 0 and 1.}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node drawing a set of glyphs.
  @end{short}
  @see-class{gsk:render-node}")

(export 'text-node)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_text_node_new (PangoFont *font,
;;;                    PangoGlyphString *glyphs,
;;;                    const GdkRGBA *color,
;;;                    const graphene_point_t *offset);
;;;
;;; Creates a render node that renders the given glyphs, Note that color may
;;; not be used if the font contains color glyphs.
;;;
;;; font
;;;     the PangoFont containing the glyphs
;;;
;;; glyphs
;;;     the PangoGlyphString to render
;;;
;;; color
;;;     the foreground color to render with
;;;
;;; offset
;;;     offset of the baseline
;;;
;;; Returns
;;;     a new GskRenderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_new" text-node-new) render-node
  (font (g:object pango:font))
  (glyphs (g:boxed pango:glyph-string))
  (color (g:boxed gdk:rgba))
  (offset (:pointer (:struct graphene:point-t))))

(export 'text-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_font ()
;;;
;;; PangoFont *
;;; gsk_text_node_get_font (GskRenderNode *node);
;;;
;;; Returns the font used by the text node .
;;;
;;; node
;;;     The GskRenderNode.
;;;
;;; Returns
;;;     the font.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_font" text-node-font) (g:object pango:font)
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
;; to the instance.

(cffi:defcfun ("gsk_text_node_get_glyphs" text-node-glyphs) :pointer
  (node render-node)
  (n-glyphs :uint))

(export 'text-node-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_color ()
;;;
;;; const GdkRGBA *
;;; gsk_text_node_get_color (GskRenderNode *node);
;;;
;;; Retrieves the color used by the text node .
;;;
;;; node
;;;     a text GskRenderNode.
;;;
;;; Returns
;;;     the text color.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_color" text-node-color) (g:boxed gdk:rgba)
  (node render-node))

(export 'text-node-color)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_has_color_glyphs ()
;;;
;;; gboolean
;;; gsk_text_node_has_color_glyphs (GskRenderNode *node);
;;;
;;; Checks whether the text node has color glyphs.
;;;
;;; node
;;;     a text GskRenderNode.
;;;
;;; Returns
;;;     TRUE if the text node has color glyphs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_has_color_glyphs" text-node-has-color-glyphs)
    :boolean
  (node render-node))

(export 'text-node-has-color-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_num_glyphs ()
;;;
;;; guint
;;; gsk_text_node_get_num_glyphs (GskRenderNode *node);
;;;
;;; Retrieves the number of glyphs in the text node.
;;;
;;; node
;;;     a text GskRenderNode.
;;;
;;; Returns
;;;     the number of glyphs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_num_glyphs" text-node-num-glyphs) :uint
  (node render-node))

(export 'text-node-num-glyphs)

;;; ----------------------------------------------------------------------------
;;; gsk_text_node_get_offset ()
;;;
;;; const graphene_point_t *
;;; gsk_text_node_get_offset (GskRenderNode *node);
;;;
;;; Retrieves the offset applied to the text.
;;;
;;; node
;;;     a text GskRenderNode.
;;;
;;; Returns
;;;     a point with the horizontal and vertical offsets.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_text_node_get_offset" text-node-offset)
    (:pointer (:struct graphene:point-t))
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
 "@version{2023-10-26}
  @begin{short}
    A render node applying a blur effect between its single child node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'blur-node)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_new" blur-node-new) render-node
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[child]{a @class{gsk:render-node} instance with the child to blur}
  @argument[radius]{a single float with the blur radius}
  @return{A new @class{gsk:blur-node} instance.}
  @begin{short}
    Creates a render node that blurs the child render node.
  @end{short}
  @see-class{gsk:blur-node}
  @see-class{gsk:render-node}"
  (child render-node)
  (radius :float))

(export 'blur-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_get_child" blur-node-child) render-node
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[node]{a @class{gsk:blur-node} instance}
  @return{A @class{gsk:render-node} instance with the blurred child render
    node.}
  @begin{short}
    Retrieves the child render node of the blur render node.
  @end{short}
  @see-class{gsk:blur-node}
  @see-class{gsk:render-node}"
  (node blur-node))

(export 'blur-node-child)

;;; ----------------------------------------------------------------------------
;;; gsk_blur_node_get_radius ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_blur_node_get_radius" blur-node-radius) :float
 #+liber-documentation
 "@version{#2023-10-26}
  @argument[node]{a @class{gsk:blur-node} instance}
  @return{A single float with the blur radius.}
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
 "@version{#2023-10-27}
  @begin{short}
    A render node that emits a debugging message when drawing its child
    render node.
  @end{short}
  @see-class{gsk:render-node}")

(export 'debug-node)

;;; ----------------------------------------------------------------------------
;;; gsk_debug_node_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_new" debug-node-new) render-node
 #+liber-documentation
 "@version{#2023-11-16}
  @argument[child]{a @class{gsk:render-node} instance with the child to add
    debug info for}
  @argument[message]{a string with the debug message}
  @return{The new @class{gsk:debug-node} instance.}
  @begin{short}
    Creates a render node that will add debug information about the given
    @arg{child}.
  @end{short}
  Adding this node has no visual effect.
  @see-class{gsk:debug-node}
  @see-class{gsk:rendernode}"
  (child render-node)
  (message :string))

(export 'debug-node-new)

;;; ----------------------------------------------------------------------------
;;; gsk_debug_node_get_child ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_get_child" debug-node-child) render-node
 #+liber-documentation
 "@version{#2023-11-16}
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
;;; gsk_debug_node_get_message ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_debug_node_get_message" debug-node-message) :string
 #+liber-documentation
 "@version{#2023-11-16}
  @argument[node]{a @class{gsk:debug-node} instance}
  @return{A string with the debug message.}
  @begin{short}
    Gets the debug message that was set on this render node.
  @end{short}
  @see-class{gsk:debug-node}"
  (node render-node))

(export 'debug-node-message)

;;; ----------------------------------------------------------------------------
;;; GskGLShaderNode
;;; ----------------------------------------------------------------------------

(cffi:define-foreign-type gl-shader-node (render-node)
  ()
  (:simple-parser gl-shader-node))

#+liber-documentation
(setf (liber:alias-for-class 'gl-shader-node)
      "GskRenderNode"
      (documentation 'gl-shader-node 'type)
 "@version{#2023-10-27}
  @begin{short}
    A render node using a GL shader when drawing its child render nodes.
  @end{short}
  @see-class{gsk:render-node}")

(export 'gl-shader-node)

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_new ()
;;;
;;; GskRenderNode *
;;; gsk_gl_shader_node_new (GskGLShader *shader,
;;;                         const graphene_rect_t *bounds,
;;;                         GBytes *args,
;;;                         GskRenderNode **children,
;;;                         guint n_children);
;;;
;;; Creates a GskRenderNode that will render the given shader into the area
;;; given by bounds . The args is a block of data to use for uniform input, as
;;; per types and offsets defined by the shader . Normally this is generated by
;;; gsk_gl_shader_format_args() or GskGLShaderArgBuilder.
;;;
;;; See GskGLShader for details about how the shader should be written.
;;;
;;; All the children will be rendered into textures (if they aren't already
;;; GskTextureNodes, which will be used directly). These textures will be sent
;;; as input to the shader.
;;;
;;; If the renderer doesn't support GL shaders, or if there is any problem when
;;; compiling the shader, then the node will draw pink. You should use
;;; gsk_gl_shader_compile() to ensure the shader will work for the renderer
;;; before using it.
;;;
;;; shader
;;;     the GskGLShader
;;;
;;; bounds
;;;     the rectangle to render the shader into
;;;
;;; args
;;;     Arguments for the uniforms
;;;
;;; children
;;;     array of child nodes, these will be rendered to textures and used as
;;;     input.
;;;
;;; n_children
;;;     Length of children (currenly the GL backend supports up to 4 children)
;;;
;;; Returns
;;;     A new GskRenderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_n_children ()
;;;
;;; guint
;;; gsk_gl_shader_node_get_n_children (GskRenderNode *node);
;;;
;;; Returns the number of children
;;;
;;; node
;;;     a GskRenderNode for a gl shader.
;;;
;;; Returns
;;;     The number of children
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_child ()
;;;
;;; GskRenderNode *
;;; gsk_gl_shader_node_get_child (GskRenderNode *node,
;;;                               guint idx);
;;;
;;; Gets one of the children.
;;;
;;; node
;;;     a GskRenderNode for a gl shader.
;;;
;;; idx
;;;     the position of the child to get
;;;
;;; Returns
;;;     the idx 'th child of node .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_args ()
;;;
;;; GBytes *
;;; gsk_gl_shader_node_get_args (GskRenderNode *node);
;;;
;;; Gets args for the node.
;;;
;;; node
;;;     a GskRenderNode for a gl shader.
;;;
;;; Returns
;;;     A GBytes with the uniform arguments.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_gl_shader_node_get_shader ()
;;;
;;; GskGLShader *
;;; gsk_gl_shader_node_get_shader (GskRenderNode *node);
;;;
;;; Gets shader code for the node.
;;;
;;; node
;;;     a GskRenderNode for a gl shader.
;;;
;;; Returns
;;;     the GskGLShader shader.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskTextureScaleNode
;;;
;;; final class Gsk.TextureScaleNode : GObject.TypeInstance
;;; {
;;;   /* No available fields */
;;; }
;;;
;;; A render node for a GdkTexture.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_new
;;;
;;; Creates a node that scales the texture to the size given by the bounds
;;; using the filter and then places it at the bounds’ position.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_get_filter
;;;
;;; Retrieves the GskScalingFilter used when creating this GskRenderNode.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_texture_scale_node_get_texture
;;;
;;; Retrieves the GdkTexture used when creating this GskRenderNode.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GskMaskNode
;;;
;;; final class Gsk.MaskNode : GObject.TypeInstance
;;; {
;;;   /* No available fields */
;;; }
;;;
;;; A render node masking one child node with another.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_new
;;;
;;; Creates a GskRenderNode that will mask a given node by another.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_mask
;;;
;;; Retrieves the mask GskRenderNode child of the node.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_mask_mode
;;;
;;; Retrieves the mask mode used by node.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_mask_node_get_source
;;;
;;; Retrieves the source GskRenderNode child of the node.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gsk4.render-node.lis ---------------------------------------
