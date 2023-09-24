;;; ----------------------------------------------------------------------------
;;; gtk4.snapshot.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
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
;;;
;;;     gtk_snapshot_to_node
;;;     gtk_snapshot_to_paintable
;;;     gtk_snapshot_free_to_node
;;;     gtk_snapshot_free_to_paintable
;;;
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
;;;     gtk_snapshot_push_mask                             Since 4.10
;;;     gtk_snapshot_push_fill                             Since 4.14 unstable
;;;     gtk_snapshot_push_stroke                           Since 4.14 unstable
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture
;;;
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
;;;
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
;;;     gtk_snapshot_append_scaled_texture                 Since 4.10
;;;     gtk_snapshot_append_fill                           Since 4.14 unstable
;;;     gtk_snapshot_append_stroke                         Since 4.14 unstable
;;;
;;;     gtk_snapshot_render_insertion_cursor               Deprecated 4.10
;;;     gtk_snapshot_render_background                     Deprecated 4.10
;;;     gtk_snapshot_render_frame                          Deprecated 4.10
;;;     gtk_snapshot_render_focus                          Deprecated 4.10
;;;     gtk_snapshot_render_layout                         Deprecated 4.10
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
    The @class{gtk:snapshot} object is an auxiliary object that assists in
    creating @class{gsk:render-node} objects in the
    @code{GdkPaintableInterface.snapshot()} virtual function.
  @end{short}
  It functions in a similar way to a Cairo context, and maintains a stack of
  render nodes and their associated transformations.

  The node at the top of the stack is the the one that the snapshot functions
  operate on. Use the @sym{gtk:snapshot-push-...} functions and
  @fun{gtk:snapshot-pop} function to change the current node.

  The typical way to obtain a @class{gtk:snapshot} object is as an argument to
  the @code{GtkWidgetClass.snapshot()} virtual function. If you need to create
  your own @class{gtk:snapshot} object, use the @fun{gtk:snapshot-new} function.
  @see-class{gdk:snapshot}")

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_new ()
;;;
;;; GtkSnapshot *
;;; gtk_snapshot_new (void);
;;;
;;; Creates a new GtkSnapshot.
;;;
;;; Returns :
;;;     a newly-allocated GtkSnapshot
;;; ----------------------------------------------------------------------------

(declaim (inline snapshot-new))

(defun snapshot-new ()
  (make-instance 'snapshot))

(export 'snapshot-new)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_node
;;;
;;; Returns the render node that was constructed by snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_node" snapshot-to-node) gsk:render-node
  (snapshot (g:object snapshot)))

(export 'snapshot-to-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_paintable
;;;
;;; Returns a paintable encapsulating the render node that was constructed by
;;; snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_paintable" snapshot-to-paintable)
    (g:object gdk:paintable)
  (snapshot (g:object snapshot))
  (size (:pointer (:struct graphene:size-t))))

(export 'snapshot-to-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_node
;;;
;;; Returns the node that was constructed by snapshot and frees snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_free_to_node" snapshot-free-to-node) 
    gsk:render-node
  (snapshot (g:object snapshot)))    

(export 'snapshot-free-to-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_paintable
;;;
;;; Returns a paintable for the node that was constructed by snapshot and frees
;;; snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_free_to_paintable" snapshot-free-to-paintable)
    (g:object gdk:paintable)
  (snapshot (g:object snapshot))
  (size (:pointer (:struct graphene:size-t))))

(export 'snapshot-free-to-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_opacity
;;;
;;; Modifies the opacity of an image.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_opacity" snapshot-push-opacity) :void
  (snapshot (g:object snapshot))
  (opacity :double))

(export 'snapshot-push-opacity)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_color_matrix
;;;
;;; Modifies the colors of an image by applying an affine transformation in RGB
;;; space.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_color_matrix" snapshot-push-color-matrix) 
    :void
  (snapshot (g:object snapshot))
  (color-matrix (:pointer (:struct graphene:matrix-t)))
  (color-offset (:pointer (:struct graphene:vec4-t))))

(export 'snapshot-push-color-matrix)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_repeat
;;;
;;; Creates a node that repeats the child node.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_repeat" snapshot-push-repeat) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (child-bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-push-repeat)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_clip
;;;
;;; Clips an image to a rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_clip" snapshot-push-clip) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-push-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_rounded_clip
;;;
;;; Clips an image to a rounded rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_rounded_clip" snapshot-push-rounded-clip) 
    :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct gsk:rounded-rect))))

(export 'snapshot-push-rounded-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_cross_fade
;;;
;;; Snapshots a cross-fade operation between two images with the given progress.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_cross_fade" snapshot-push-cross-fade) :void
  (snapshot (g:object snapshot))
  (progress :double))

(export 'snapshot-push-cross-fade)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blend
;;;
;;; Blends together two images with the given blend mode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blend" snapshot-push-blend) :void
  (snapshot (g:object snapshot))
  (blend-mode gsk:blend-mode))

(export 'snapshot-push-blend)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blur
;;;
;;; Blurs an image.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blur" snapshot-push-blur) :void
  (snapshot (g:object snapshot))
  (radius :double))

(export 'snapshot-push-blur)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_shadow
;;;
;;; Applies a shadow to an image.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_debug
;;;
;;; Inserts a debug node with a message.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_gl_shader
;;;
;;; Push a GskGLShaderNode.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_gl_shader" snapshot-push-gl-shader) :void
  (snapshot (g:object snapshot))
  (shader (g:object gsk:gl-shader))
  (bounds (:pointer (:struct graphene:rect-t)))
  (take-args (g:boxed g:bytes)))

(export 'snapshot-push-gl-shader)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_mask
;;;
;;; Until the first call to gtk_snapshot_pop(), the mask image for the mask
;;; operation will be recorded.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_mask" snapshot-push-mask) :void
  (snapshot (g:object snapshot))
  (mask-mode gsk:mask-mode))

(export 'snapshot-push-mask)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_fill
;;;
;;; Fills the area given by path and fill_rule with an image and discards
;;; everything outside of it.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_stroke
;;;
;;; Strokes the given path with the attributes given by stroke and an image.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_pop
;;;
;;; Removes the top element from the stack of render nodes, and appends it to
;;; the node underneath it.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_pop" snapshot-pop) :void
  (snapshot (g:object snapshot)))

(export 'snapshot-pop)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_gl_shader_pop_texture
;;;
;;; Removes the top element from the stack of render nodes and adds it to the
;;; nearest GskGLShaderNode below it.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_gl_shader_pop_texture"
               snapshot-gl-shader-pop-texture) :void
  (snapshot (g:object snapshot)))

(export 'snapshot-gl-shader-pop-texture)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_save
;;;
;;; Makes a copy of the current state of snapshot and saves it on an internal
;;; stack.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_save" snapshot-save) :void
  (snapshot (g:object snapshot)))

(export 'snapshot-save)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_restore
;;;
;;; Restores snapshot to the state saved by a preceding call to
;;; gtk_snapshot_save() and removes that state from the stack of saved states.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_restore" snapshot-restore) :void
  (snapshot (g:object snapshot)))

(export 'snapshot-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform
;;;
;;; Transforms snapshot‘s coordinate system with the given transform.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_transform" snapshot-transform) :void
  (snapshot (g:object snapshot))
  (transform (g:boxed transform)))

(export 'snapshot-transform)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform_matrix
;;;
;;; Transforms snapshot‘s coordinate system with the given matrix.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_transform_matrix" snapshot-transform-matrix) :void
  (snapshot (g:object snapshot))
  (matrix (:pointer (:struct graphene:matrix-t))))

(export 'snapshot-transform-matrix)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate
;;;
;;; Translates snapshot‘s coordinate system by point in 2-dimensional space.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_translate" snapshot-translate) :void
  (snapshot (g:object snapshot))
  (point (:pointer (:struct graphene:point-t))))

(export 'snapshot-translate)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate_3d
;;;
;;; Translates snapshot‘s coordinate system by point.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_translate_3d" snapshot-translate-3d) :void
  (snapshot (g:object snapshot))
  (point (:pointer (:struct graphene:point3d-t))))

(export 'snapshot-translate-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate
;;;
;;; Rotates @snapshot‘s coordinate system by angle degrees in 2D space - or in
;;; 3D speak, rotates around the Z axis. The rotation happens around the origin
;;; point of (0, 0) in the snapshot‘s current coordinate system.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_rotate" %snapshot-rotate) :void
  (snapshot (g:object snapshot))
  (angle :float))

(defun snapshot-rotate (snapshot angle)
  (%snapshot-rotate snapshot (coerce angle 'single-float)))

(export 'snapshot-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate_3d
;;;
;;; Rotates snapshot‘s coordinate system by angle degrees around axis.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_rotate_3d" %snapshot-rotate-3d) :void
  (snapshot (g:object snapshot))
  (angle :float)
  (axis (:pointer (:struct graphene:vec3-t))))

(defun snapshot-rotate-3d (snapshot angle axis)
  (%snapshot-rotate snapshot (coerce angle 'single-float) axis))

(export 'snapshot-rotate-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale
;;;
;;; Scales snapshot‘s coordinate system in 2-dimensional space by the given
;;; factors.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_scale" snapshot-scale) :void
  (snapshot (g:object snapshot))
  (xfactor :float)
  (yfactor :float))

(defun snapshot-scale (snapshot xfactor yfactor)
  (%snapshot-scale snapshot (coerce xfactor 'single-float)
                            (coerce yfactor 'single-float)))

(export 'snapshot-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale_3d
;;;
;;; Scales snapshot‘s coordinate system by the given factors.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_scale_3d" snapshot-scale-3d) :void
  (snapshot (g:object snapshot))
  (xfactor :float)
  (yfactor :float)
  (zfactor :float))

(defun snapshot-scale (snapshot xfactor yfactor zfactor)
  (%snapshot-scale snapshot (coerce xfactor 'single-float)
                            (coerce yfactor 'single-float)
                            (coerce zfactor 'single-float)))

(export 'snapshot-scale-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_perspective
;;;
;;; Applies a perspective projection transform.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_perspective" snapshot-perspective) :void
  (snapshot (g:object snapshot))
  (depth :float))

(export 'snapshot-perspective)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_node
;;;
;;; Appends node to the current render node of snapshot, without changing the
;;; current node.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_node" snapshot-append-node) :void
  (snapshot (g:object snapshot))
  (node gsk:render-node))

(export 'snapshot-append-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_cairo
;;;
;;; Creates a new GskCairoNode and appends it to the current render node of
;;; snapshot, without changing the current node.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_cairo" snapshot-append-cairo)
    (:pointer (:struct cairo:context-t))
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t))))
  
(export 'snapshot-append-cairo)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_texture
;;;
;;; Creates a new render node drawing the texture into the given bounds and
;;; appends it to the current render node of snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_texture" snapshot-append-texture) :void
  (snapshot (g:object snapshot))
  (texture (g:object gdk:texture))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-texture)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_color
;;;
;;; Creates a new render node drawing the color into the given bounds and
;;; appends it to the current render node of snapshot.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_color" snapshot-append-color) :void
  (snapshot (g:object snapshot))
  (color (g:boxed gdk:rgba))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-color)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_layout
;;;
;;; No description available.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_layout" snapshot-append-layout) :void
  (snapshot (g:object snapshot))
  (layout (g:object pango:layout))
  (color (g:boxed gdk:rgba)))

(export 'snapshot-append-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_linear_gradient
;;;
;;; Appends a linear gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_repeating_linear_gradient
;;;
;;; Appends a repeating linear gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_conic_gradient
;;;
;;; Appends a conic gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_border
;;;
;;; Appends a stroked border rectangle inside the given outline.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_border" %snapshot-append-border) :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (border-width (:pointer :float))
  (border-color :pointer))

(defun snapshot-append-border (snapshot outline border-width border-color)
  (cffi:with-foreign-object (width-ptr :float 4)
    (iter (for i from 0 below 4)
          (for width in border-width)
          (setf (cffi:mem-aref width-ptr :float i) width))   
    (glib:with-foreign-boxed-array (n-colors colors-ptr gdk:rgba border-color)
      (%snapshot-append-border snapshot
                               outline
                               width-ptr
                               colors-ptr))))

(export 'snapshot-append-border)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_inset_shadow
;;;
;;; Appends an inset shadow into the box given by outline.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_inset_shadow" snapshot-append-inset-shadow)
    :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (blur-radius :float))

(export 'snapshot-append-inset-shadow)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_outset_shadow
;;;
;;; Appends an outset shadow node around the box given by outline.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_outset_shadow" 
               snapshot-append-outset-shadow) :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (blur-radius :float))

(export 'snapshot-append-outset-shadow)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_radial_gradient
;;;
;;; Appends a radial gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_repeating_radial_gradient
;;;
;;; Appends a repeating radial gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_scaled_texture
;;;
;;; Creates a new render node drawing the texture into the given bounds and
;;; appends it to the current render node of snapshot.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_snapshot_append_scaled_texture"
               snapshot-append-scaled-texture) :void
  (snapshot (g:object snapshot))
  (texture (g:object gdk:texture))
  (filter gsk:scaling-filter)
  (bounds (:pointer (:struct graphene:rect-t))))

#+gtk-4-10
(export 'snapshot-append-scaled-texture)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_fill
;;;
;;; A convenience method to fill a path with a color.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_stroke
;;;
;;; A convenience method to stroke a path with a color.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_render_insertion_cursor
;;;
;;; Draws a text caret using snapshot at the specified index of layout.
;;;
;;; Deprecated 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_render_background
;;;
;;; Creates a render node for the CSS background according to context, and
;;; appends it to the current node of snapshot, without changing the current
;;; node.
;;;
;;; Deprecated 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_render_frame
;;;
;;; Creates a render node for the CSS border according to context, and appends
;;; it to the current node of snapshot, without changing the current node.
;;;
;;; Deprecated 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_render_focus
;;;
;;; Creates a render node for the focus outline according to context, and
;;; appends it to the current node of snapshot, without changing the current
;;; node.
;;;
;;; Deprecated 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_render_layout
;;;
;;; Creates a render node for rendering layout according to the style
;;; information in context, and appends it to the current node of snapshot,
;;; without changing the current node.
;;;
;;; Deprecated 4.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.snapshot.lisp -----------------------------------------
