;;; ----------------------------------------------------------------------------
;;; gtk4.snapshot.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkSnapshot
;;;
;;;     Auxiliary object for snapshots
;;;
;;; Types and Values
;;;
;;;     GtkSnapshot
;;;
;;; Functions
;;;
;;;     gtk_snapshot_new
;;;     gtk_snapshot_to_node
;;;     gtk_snapshot_to_paintable
;;;     gtk_snapshot_free_to_node
;;;     gtk_snapshot_free_to_paintable
;;;     gtk_snapshot_push_opacity
;;;     gtk_snapshot_push_color_matrix
;;;     gtk_snapshot_push_repeat
;;;     gtk_snapshot_push_clip
;;;     gtk_snapshot_push_rounded_clip
;;;     gtk_snapshot_push_cross_fade
;;;     gtk_snapshot_push_blend
;;;     gtk_snapshot_push_blur
;;;     gtk_snapshot_push_shadow
;;;     gtk_snapshot_push_debug
;;;     gtk_snapshot_push_gl_shader
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture
;;;     gtk_snapshot_save
;;;     gtk_snapshot_restore
;;;     gtk_snapshot_transform
;;;     gtk_snapshot_transform_matrix
;;;     gtk_snapshot_translate
;;;     gtk_snapshot_translate_3d
;;;     gtk_snapshot_rotate
;;;     gtk_snapshot_rotate_3d
;;;     gtk_snapshot_scale
;;;     gtk_snapshot_scale_3d
;;;     gtk_snapshot_perspective
;;;     gtk_snapshot_append_node
;;;     gtk_snapshot_append_cairo
;;;     gtk_snapshot_append_texture
;;;     gtk_snapshot_append_color
;;;     gtk_snapshot_append_layout
;;;     gtk_snapshot_append_linear_gradient
;;;     gtk_snapshot_append_repeating_linear_gradient
;;;     gtk_snapshot_append_conic_gradient
;;;     gtk_snapshot_append_border
;;;     gtk_snapshot_append_inset_shadow
;;;     gtk_snapshot_append_outset_shadow
;;;     gtk_snapshot_append_radial_gradient
;;;     gtk_snapshot_append_repeating_radial_gradient
;;;     gtk_snapshot_render_insertion_cursor
;;;     gtk_snapshot_render_background
;;;     gtk_snapshot_render_frame
;;;     gtk_snapshot_render_focus
;;;     gtk_snapshot_render_layout
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkSnapshot
;;;         ╰── GtkSnapshot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSnapshot
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSnapshot" snapshot
  (:superclass gdk:snapshot
   :export t
   :interfaces ()
   :type-initializer "gtk_snapshot_get_type")
  nil)

#+liber-documentation
(setf (documentation 'snapshot 'type)
 "@version{#2022-7-10}
  @begin{short}
    The @sym{gtk:snapshot} object is an auxiliary object that assists in
    creating @class{render-nodes} objects in the
    @code{GdkPaintableInterface.snapshot()} virtual function.
  @end{short}
  It functions in a similar way to a Cairo context, and maintains a stack of
  render nodes and their associated transformations.

  The node at the top of the stack is the the one that the snapshot functions
  operate on. Use the @sym{gtk:snapshot-push-...} functions and
  @sym{gtk:snapshot-pop} function to change the current node.

  The typical way to obtain a @class{gtk:snapshot} object is as an argument to
  the @code{GtkWidgetClass.snapshot()} virtual function. If you need to create
  your own @sym{gtk:snapshot} object, use the @fun{gtk:snapshot-new} function.
  @see-class{gdk:snapshot}")


;;;gtk_snapshot_new ()
;;;GtkSnapshot *
;;;gtk_snapshot_new (void);
;;;Creates a new GtkSnapshot.

;;;Returns
;;;a newly-allocated GtkSnapshot

;;;gtk_snapshot_to_node ()
;;;GskRenderNode *
;;;gtk_snapshot_to_node (GtkSnapshot *snapshot);
;;;Returns the render node that was constructed by snapshot . After calling this function, it is no longer possible to add more nodes to snapshot . The only function that should be called after this is g_object_unref().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;Returns
;;;the constructed GskRenderNode.

;;;[transfer full]

;;;gtk_snapshot_to_paintable ()
;;;GdkPaintable *
;;;gtk_snapshot_to_paintable (GtkSnapshot *snapshot,
;;;                           const graphene_size_t *size);
;;;Returns a paintable encapsulating the render node that was constructed by snapshot . After calling this function, it is no longer possible to add more nodes to snapshot . The only function that should be called after this is g_object_unref().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;size

;;;The size of the resulting paintable or NULL to use the bounds of the snapshot.

;;;[allow-none]
;;;Returns
;;;a new GdkPaintable.

;;;[transfer full]

;;;gtk_snapshot_free_to_node ()
;;;GskRenderNode *
;;;gtk_snapshot_free_to_node (GtkSnapshot *snapshot);
;;;Returns the node that was constructed by snapshot and frees snapshot .

;;;[skip]

;;;Parameters
;;;snapshot

;;;a GtkSnapshot.

;;;[transfer full]
;;;Returns
;;;a newly-created GskRenderNode.

;;;[transfer full]

;;;gtk_snapshot_free_to_paintable ()
;;;GdkPaintable *
;;;gtk_snapshot_free_to_paintable (GtkSnapshot *snapshot,
;;;                                const graphene_size_t *size);
;;;Returns a paintable for the node that was constructed by snapshot and frees snapshot .

;;;[skip]

;;;Parameters
;;;snapshot

;;;a GtkSnapshot.

;;;[transfer full]
;;;size

;;;The size of the resulting paintable or NULL to use the bounds of the snapshot.

;;;[allow-none]
;;;Returns
;;;a newly-created GdkPaintable.

;;;[transfer full]

;;;gtk_snapshot_push_opacity ()
;;;void
;;;gtk_snapshot_push_opacity (GtkSnapshot *snapshot,
;;;                           double opacity);
;;;Modifies the opacity of an image.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;opacity

;;;the opacity to use

;;;gtk_snapshot_push_color_matrix ()
;;;void
;;;gtk_snapshot_push_color_matrix (GtkSnapshot *snapshot,
;;;                                const graphene_matrix_t *color_matrix,
;;;                                const graphene_vec4_t *color_offset);
;;;Modifies the colors of an image by applying an affine transformation in RGB space.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;color_matrix

;;;the color matrix to use

;;;color_offset

;;;the color offset to use

;;;gtk_snapshot_push_repeat ()
;;;void
;;;gtk_snapshot_push_repeat (GtkSnapshot *snapshot,
;;;                          const graphene_rect_t *bounds,
;;;                          const graphene_rect_t *child_bounds);
;;;Creates a node that repeats the child node.

;;;The child is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the bounds within which to repeat

;;;child_bounds

;;;the bounds of the child or NULL to use the full size of the collected child node.

;;;[nullable]
;;;gtk_snapshot_push_clip ()
;;;void
;;;gtk_snapshot_push_clip (GtkSnapshot *snapshot,
;;;                        const graphene_rect_t *bounds);
;;;Clips an image to a rectangle.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to clip to

;;;gtk_snapshot_push_rounded_clip ()
;;;void
;;;gtk_snapshot_push_rounded_clip (GtkSnapshot *snapshot,
;;;                                const GskRoundedRect *bounds);
;;;Clips an image to a rounded rectangle.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rounded rectangle to clip to

;;;gtk_snapshot_push_cross_fade ()
;;;void
;;;gtk_snapshot_push_cross_fade (GtkSnapshot *snapshot,
;;;                              double progress);
;;;Snapshots a cross-fade operation between two images with the given progress .

;;;Until the first call to gtk_snapshot_pop(), the start image will be snapshot. After that call, the end image will be recorded until the second call to gtk_snapshot_pop().

;;;Calling this function requires 2 calls to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;progress

;;;progress between 0.0 and 1.0

;;;gtk_snapshot_push_blend ()
;;;void
;;;gtk_snapshot_push_blend (GtkSnapshot *snapshot,
;;;                         GskBlendMode blend_mode);
;;;Blends together 2 images with the given blend mode.

;;;Until the first call to gtk_snapshot_pop(), the bottom image for the blend operation will be recorded. After that call, the top image to be blended will be recorded until the second call to gtk_snapshot_pop().

;;;Calling this function requires 2 subsequent calls to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;blend_mode

;;;blend mode to use

;;;gtk_snapshot_push_blur ()
;;;void
;;;gtk_snapshot_push_blur (GtkSnapshot *snapshot,
;;;                        double radius);
;;;Blurs an image.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;radius

;;;the blur radius to use

;;;gtk_snapshot_push_shadow ()
;;;void
;;;gtk_snapshot_push_shadow (GtkSnapshot *snapshot,
;;;                          const GskShadow *shadow,
;;;                          gsize n_shadows);
;;;Applies a shadow to an image.

;;;The image is recorded until the next call to gtk_snapshot_pop().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;shadow

;;;the first shadow specification

;;;n_shadows

;;;number of shadow specifications

;;;gtk_snapshot_push_debug ()
;;;void
;;;gtk_snapshot_push_debug (GtkSnapshot *snapshot,
;;;                         const char *message,
;;;                         ...);
;;;Inserts a debug node with a message. Debug nodes don't affect the rendering at all, but can be helpful in identifying parts of a render node tree dump, for example in the GTK inspector.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;message

;;;a printf-style format string

;;;...

;;;arguments for message

;;;gtk_snapshot_push_gl_shader ()
;;;void
;;;gtk_snapshot_push_gl_shader (GtkSnapshot *snapshot,
;;;                             GskGLShader *shader,
;;;                             const graphene_rect_t *bounds,
;;;                             GBytes *take_args);
;;;Push a GskGLShaderNode with a specific GskGLShader and a set of uniform values to use while rendering. Additionally this takes a list of n_children other nodes which will be passed to the GskGLShaderNode.

;;;The take_args argument is a block of data to use for uniform arguments, as per types and offsets defined by the shader . Normally this is generated by gsk_gl_shader_format_args() or GskGLShaderArgBuilder. The snapshotter takes ownership of take_args , so the caller should not free it after this.

;;;If the renderer doesn't support GL shaders, or if there is any problem when compiling the shader, then the node will draw pink. You should use gsk_gl_shader_compile() to ensure the shader will work for the renderer before using it.

;;;If the shader requires textures (see gsk_gl_shader_get_n_textures()), then it is expected that you call gtk_snapshot_gl_shader_pop_texture() the number of times that are required. Each of these calls will generate a node that is added as a child to the gl shader node, which in turn will render these offscreen and pass as a texture to the shader.

;;;Once all textures (if any) are pop:ed, you must call the regular gtk_snapshot_pop().

;;;If you want to use pre-existing textures as input to the shader rather than rendering new ones, use gtk_snapshot_append_texture() to push a texture node. These will be used directly rather than being re-rendered.

;;;For details on how to write shaders, see GskGLShader.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;shader

;;;The code to run

;;;bounds

;;;the rectangle to render into

;;;take_args

;;;Data block with arguments for the shader.

;;;[transfer full]
;;;gtk_snapshot_pop ()
;;;void
;;;gtk_snapshot_pop (GtkSnapshot *snapshot);
;;;Removes the top element from the stack of render nodes, and appends it to the node underneath it.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;gtk_snapshot_gl_shader_pop_texture ()
;;;void
;;;gtk_snapshot_gl_shader_pop_texture (GtkSnapshot *snapshot);
;;;Removes the top element from the stack of render nodes and adds it to the nearest GskGLShaderNode below it. This must be called the same number of times as the number of textures is needed for the shader in gtk_snapshot_push_gl_shader().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;gtk_snapshot_save ()
;;;void
;;;gtk_snapshot_save (GtkSnapshot *snapshot);
;;;Makes a copy of the current state of snapshot and saves it on an internal stack of saved states for snapshot . When gtk_snapshot_restore() is called, snapshot will be restored to the saved state. Multiple calls to gtk_snapshot_save() and gtk_snapshot_restore() can be nested; each call to gtk_snapshot_restore() restores the state from the matching paired gtk_snapshot_save().

;;;It is necessary to clear all saved states with corresponding calls to gtk_snapshot_restore().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;gtk_snapshot_restore ()
;;;void
;;;gtk_snapshot_restore (GtkSnapshot *snapshot);
;;;Restores snapshot to the state saved by a preceding call to gtk_snapshot_save() and removes that state from the stack of saved states.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;gtk_snapshot_transform ()
;;;void
;;;gtk_snapshot_transform (GtkSnapshot *snapshot,
;;;                        GskTransform *transform);
;;;Transforms snapshot 's coordinate system with the given transform .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;transform

;;;the transform to apply.

;;;[allow-none]
;;;gtk_snapshot_transform_matrix ()
;;;void
;;;gtk_snapshot_transform_matrix (GtkSnapshot *snapshot,
;;;                               const graphene_matrix_t *matrix);
;;;Transforms snapshot 's coordinate system with the given matrix .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;matrix

;;;the matrix to multiply the transform with

;;;gtk_snapshot_translate ()
;;;void
;;;gtk_snapshot_translate (GtkSnapshot *snapshot,
;;;                        const graphene_point_t *point);
;;;Translates snapshot 's coordinate system by point in 2-dimensional space.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;point

;;;the point to translate the snapshot by

;;;gtk_snapshot_translate_3d ()
;;;void
;;;gtk_snapshot_translate_3d (GtkSnapshot *snapshot,
;;;                           const graphene_point3d_t *point);
;;;Translates snapshot 's coordinate system by point .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;point

;;;the point to translate the snapshot by

;;;gtk_snapshot_rotate ()
;;;void
;;;gtk_snapshot_rotate (GtkSnapshot *snapshot,
;;;                     float angle);
;;;Rotates @snapshot 's coordinate system by angle degrees in 2D space - or in 3D speak, rotates around the z axis.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;angle

;;;the rotation angle, in degrees (clockwise)

;;;gtk_snapshot_rotate_3d ()
;;;void
;;;gtk_snapshot_rotate_3d (GtkSnapshot *snapshot,
;;;                        float angle,
;;;                        const graphene_vec3_t *axis);
;;;Rotates snapshot 's coordinate system by angle degrees around axis .

;;;For a rotation in 2D space, use gsk_transform_rotate().

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;angle

;;;the rotation angle, in degrees (clockwise)

;;;axis

;;;The rotation axis

;;;gtk_snapshot_scale ()
;;;void
;;;gtk_snapshot_scale (GtkSnapshot *snapshot,
;;;                    float factor_x,
;;;                    float factor_y);
;;;Scales snapshot 's coordinate system in 2-dimensional space by the given factors.

;;;Use gtk_snapshot_scale_3d() to scale in all 3 dimensions.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;factor_x

;;;scaling factor on the X axis

;;;factor_y

;;;scaling factor on the Y axis

;;;gtk_snapshot_scale_3d ()
;;;void
;;;gtk_snapshot_scale_3d (GtkSnapshot *snapshot,
;;;                       float factor_x,
;;;                       float factor_y,
;;;                       float factor_z);
;;;Scales snapshot 's coordinate system by the given factors.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;factor_x

;;;scaling factor on the X axis

;;;factor_y

;;;scaling factor on the Y axis

;;;factor_z

;;;scaling factor on the Z axis

;;;gtk_snapshot_perspective ()
;;;void
;;;gtk_snapshot_perspective (GtkSnapshot *snapshot,
;;;                          float depth);
;;;Applies a perspective projection transform.

;;;See gsk_transform_perspective() for a discussion on the details.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;depth

;;;distance of the z=0 plane

;;;gtk_snapshot_append_node ()
;;;void
;;;gtk_snapshot_append_node (GtkSnapshot *snapshot,
;;;                          GskRenderNode *node);
;;;Appends node to the current render node of snapshot , without changing the current node. If snapshot does not have a current node yet, node will become the initial node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;node

;;;a GskRenderNode

;;;gtk_snapshot_append_cairo ()
;;;cairo_t *
;;;gtk_snapshot_append_cairo (GtkSnapshot *snapshot,
;;;                           const graphene_rect_t *bounds);
;;;Creates a new render node and appends it to the current render node of snapshot , without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the bounds for the new node

;;;Returns
;;;a cairo_t suitable for drawing the contents of the newly created render node

;;;gtk_snapshot_append_texture ()
;;;void
;;;gtk_snapshot_append_texture (GtkSnapshot *snapshot,
;;;                             GdkTexture *texture,
;;;                             const graphene_rect_t *bounds);
;;;Creates a new render node drawing the texture into the given bounds and appends it to the current render node of snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;texture

;;;the GdkTexture to render

;;;bounds

;;;the bounds for the new node

;;;gtk_snapshot_append_color ()
;;;void
;;;gtk_snapshot_append_color (GtkSnapshot *snapshot,
;;;                           const GdkRGBA *color,
;;;                           const graphene_rect_t *bounds);
;;;Creates a new render node drawing the color into the given bounds and appends it to the current render node of snapshot .

;;;You should try to avoid calling this function if color is transparent.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;color

;;;the GdkRGBA to draw

;;;bounds

;;;the bounds for the new node

;;;gtk_snapshot_append_layout ()
;;;void
;;;gtk_snapshot_append_layout (GtkSnapshot *snapshot,
;;;                            PangoLayout *layout,
;;;                            const GdkRGBA *color);
;;;Creates render nodes for rendering layout in the given foregound color and appends them to the current node of snapshot without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;layout

;;;the PangoLayout to render

;;;color

;;;the foreground color to render the layout in

;;;gtk_snapshot_append_linear_gradient ()
;;;void
;;;gtk_snapshot_append_linear_gradient (GtkSnapshot *snapshot,
;;;                                     const graphene_rect_t *bounds,
;;;                                     const graphene_point_t *start_point,
;;;                                     const graphene_point_t *end_point,
;;;                                     const GskColorStop *stops,
;;;                                     gsize n_stops);
;;;Appends a linear gradient node with the given stops to snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to render the linear gradient into

;;;start_point

;;;the point at which the linear gradient will begin

;;;end_point

;;;the point at which the linear gradient will finish

;;;stops

;;;a pointer to an array of GskColorStop defining the gradient.

;;;[array length=n_stops]
;;;n_stops

;;;the number of elements in stops

;;;gtk_snapshot_append_repeating_linear_gradient ()
;;;void
;;;gtk_snapshot_append_repeating_linear_gradient
;;;                               (GtkSnapshot *snapshot,
;;;                                const graphene_rect_t *bounds,
;;;                                const graphene_point_t *start_point,
;;;                                const graphene_point_t *end_point,
;;;                                const GskColorStop *stops,
;;;                                gsize n_stops);
;;;Appends a repeating linear gradient node with the given stops to snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to render the linear gradient into

;;;start_point

;;;the point at which the linear gradient will begin

;;;end_point

;;;the point at which the linear gradient will finish

;;;stops

;;;a pointer to an array of GskColorStop defining the gradient.

;;;[array length=n_stops]
;;;n_stops

;;;the number of elements in stops

;;;gtk_snapshot_append_conic_gradient ()
;;;void
;;;gtk_snapshot_append_conic_gradient (GtkSnapshot *snapshot,
;;;                                    const graphene_rect_t *bounds,
;;;                                    const graphene_point_t *center,
;;;                                    float rotation,
;;;                                    const GskColorStop *stops,
;;;                                    gsize n_stops);
;;;Appends a conic gradient node with the given stops to snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to render the gradient into

;;;center

;;;the center point of the conic gradient

;;;rotation

;;;the clockwise rotation in degrees of the starting angle. 0 means the starting angle is the top.

;;;stops

;;;a pointer to an array of GskColorStop defining the gradient.

;;;[array length=n_stops]
;;;n_stops

;;;the number of elements in stops

;;;gtk_snapshot_append_border ()
;;;void
;;;gtk_snapshot_append_border (GtkSnapshot *snapshot,
;;;                            const GskRoundedRect *outline,
;;;                            const float border_width[4],
;;;                            const GdkRGBA border_color[4]);
;;;Appends a stroked border rectangle inside the given outline . The 4 sides of the border can have different widths and colors.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;outline

;;;a GskRoundedRect describing the outline of the border

;;;border_width

;;;the stroke width of the border on the top, right, bottom and left side respectively.

;;;[array fixed-size=4]
;;;border_color

;;;the color used on the top, right, bottom and left side.

;;;[array fixed-size=4]
;;;gtk_snapshot_append_inset_shadow ()
;;;void
;;;gtk_snapshot_append_inset_shadow (GtkSnapshot *snapshot,
;;;                                  const GskRoundedRect *outline,
;;;                                  const GdkRGBA *color,
;;;                                  float dx,
;;;                                  float dy,
;;;                                  float spread,
;;;                                  float blur_radius);
;;;Appends an inset shadow into the box given by outline .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;outline

;;;outline of the region surrounded by shadow

;;;color

;;;color of the shadow

;;;dx

;;;horizontal offset of shadow

;;;dy

;;;vertical offset of shadow

;;;spread

;;;how far the shadow spreads towards the inside

;;;blur_radius

;;;how much blur to apply to the shadow

;;;gtk_snapshot_append_outset_shadow ()
;;;void
;;;gtk_snapshot_append_outset_shadow (GtkSnapshot *snapshot,
;;;                                   const GskRoundedRect *outline,
;;;                                   const GdkRGBA *color,
;;;                                   float dx,
;;;                                   float dy,
;;;                                   float spread,
;;;                                   float blur_radius);
;;;Appends an outset shadow node around the box given by outline .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;outline

;;;outline of the region surrounded by shadow

;;;color

;;;color of the shadow

;;;dx

;;;horizontal offset of shadow

;;;dy

;;;vertical offset of shadow

;;;spread

;;;how far the shadow spreads towards the outside

;;;blur_radius

;;;how much blur to apply to the shadow

;;;gtk_snapshot_append_radial_gradient ()
;;;void
;;;gtk_snapshot_append_radial_gradient (GtkSnapshot *snapshot,
;;;                                     const graphene_rect_t *bounds,
;;;                                     const graphene_point_t *center,
;;;                                     float hradius,
;;;                                     float vradius,
;;;                                     float start,
;;;                                     float end,
;;;                                     const GskColorStop *stops,
;;;                                     gsize n_stops);
;;;Appends a radial gradient node with the given stops to snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to render the readial gradient into

;;;center

;;;the center point for the radial gradient

;;;hradius

;;;the horizontal radius

;;;vradius

;;;the vertical radius

;;;start

;;;the start position (on the horizontal axis)

;;;end

;;;the end position (on the horizontal axis)

;;;stops

;;;a pointer to an array of GskColorStop defining the gradient.

;;;[array length=n_stops]
;;;n_stops

;;;the number of elements in stops

;;;gtk_snapshot_append_repeating_radial_gradient ()
;;;void
;;;gtk_snapshot_append_repeating_radial_gradient
;;;                               (GtkSnapshot *snapshot,
;;;                                const graphene_rect_t *bounds,
;;;                                const graphene_point_t *center,
;;;                                float hradius,
;;;                                float vradius,
;;;                                float start,
;;;                                float end,
;;;                                const GskColorStop *stops,
;;;                                gsize n_stops);
;;;Appends a repeating radial gradient node with the given stops to snapshot .

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;bounds

;;;the rectangle to render the readial gradient into

;;;center

;;;the center point for the radial gradient

;;;hradius

;;;the horizontal radius

;;;vradius

;;;the vertical radius

;;;start

;;;the start position (on the horizontal axis)

;;;end

;;;the end position (on the horizontal axis)

;;;stops

;;;a pointer to an array of GskColorStop defining the gradient.

;;;[array length=n_stops]
;;;n_stops

;;;the number of elements in stops

;;;gtk_snapshot_render_insertion_cursor ()
;;;void
;;;gtk_snapshot_render_insertion_cursor (GtkSnapshot *snapshot,
;;;                                      GtkStyleContext *context,
;;;                                      double x,
;;;                                      double y,
;;;                                      PangoLayout *layout,
;;;                                      int index,
;;;                                      PangoDirection direction);
;;;Draws a text caret using snapshot at the specified index of layout .

;;;Parameters
;;;snapshot

;;;snapshot to render to

;;;context

;;;a GtkStyleContext

;;;x

;;;X origin

;;;y

;;;Y origin

;;;layout

;;;the PangoLayout of the text

;;;index

;;;the index in the PangoLayout

;;;direction

;;;the PangoDirection of the text

;;;gtk_snapshot_render_background ()
;;;void
;;;gtk_snapshot_render_background (GtkSnapshot *snapshot,
;;;                                GtkStyleContext *context,
;;;                                double x,
;;;                                double y,
;;;                                double width,
;;;                                double height);
;;;Creates a render node for the CSS background according to context , and appends it to the current node of snapshot , without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;context

;;;the GtkStyleContext to use

;;;x

;;;X origin of the rectangle

;;;y

;;;Y origin of the rectangle

;;;width

;;;rectangle width

;;;height

;;;rectangle height

;;;gtk_snapshot_render_frame ()
;;;void
;;;gtk_snapshot_render_frame (GtkSnapshot *snapshot,
;;;                           GtkStyleContext *context,
;;;                           double x,
;;;                           double y,
;;;                           double width,
;;;                           double height);
;;;Creates a render node for the CSS border according to context , and appends it to the current node of snapshot , without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;context

;;;the GtkStyleContext to use

;;;x

;;;X origin of the rectangle

;;;y

;;;Y origin of the rectangle

;;;width

;;;rectangle width

;;;height

;;;rectangle height

;;;gtk_snapshot_render_focus ()
;;;void
;;;gtk_snapshot_render_focus (GtkSnapshot *snapshot,
;;;                           GtkStyleContext *context,
;;;                           double x,
;;;                           double y,
;;;                           double width,
;;;                           double height);
;;;Creates a render node for the focus outline according to context , and appends it to the current node of snapshot , without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;context

;;;the GtkStyleContext to use

;;;x

;;;X origin of the rectangle

;;;y

;;;Y origin of the rectangle

;;;width

;;;rectangle width

;;;height

;;;rectangle height

;;;gtk_snapshot_render_layout ()
;;;void
;;;gtk_snapshot_render_layout (GtkSnapshot *snapshot,
;;;                            GtkStyleContext *context,
;;;                            double x,
;;;                            double y,
;;;                            PangoLayout *layout);
;;;Creates a render node for rendering layout according to the style information in context , and appends it to the current node of snapshot , without changing the current node.

;;;Parameters
;;;snapshot

;;;a GtkSnapshot

;;;context

;;;the GtkStyleContext to use

;;;x

;;;X origin of the rectangle

;;;y

;;;Y origin of the rectangle

;;;layout

;;;the PangoLayout to render


;;; --- End of file gtk4.snapshot.lisp -----------------------------------------
