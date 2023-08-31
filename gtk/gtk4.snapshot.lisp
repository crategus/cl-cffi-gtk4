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
;;;     gtk_snapshot_append_border
;;;     gtk_snapshot_append_cairo
;;;     gtk_snapshot_append_color
;;;     gtk_snapshot_append_conic_gradient
;;;     gtk_snapshot_append_fill                           Since 4.14 unstable
;;;     gtk_snapshot_append_inset_shadow
;;;     gtk_snapshot_append_layout
;;;     gtk_snapshot_append_linear_gradient
;;;     gtk_snapshot_append_node
;;;     gtk_snapshot_append_outset_shadow
;;;     gtk_snapshot_append_radial_gradient
;;;     gtk_snapshot_append_repeating_linear_gradient
;;;     gtk_snapshot_append_repeating_radial_gradient
;;;     gtk_snapshot_append_scaled_texture                 Since 4.10
;;;     gtk_snapshot_append_stroke                         Since 4.14 unstable
;;;     gtk_snapshot_append_texture
;;;     gtk_snapshot_free_to_node
;;;     gtk_snapshot_free_to_paintable
;;;     gtk_snapshot_gl_shader_pop_texture
;;;     gtk_snapshot_perspective
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_push_blend
;;;     gtk_snapshot_push_blur
;;;     gtk_snapshot_push_clip
;;;     gtk_snapshot_push_color_matrix
;;;     gtk_snapshot_push_cross_fade
;;;     gtk_snapshot_push_debug
;;;     gtk_snapshot_push_fill                             Since 4.14 unstable
;;;     gtk_snapshot_push_gl_shader
;;;     gtk_snapshot_push_mask                             Since 4.10
;;;     gtk_snapshot_push_opacity
;;;     gtk_snapshot_push_repeat
;;;     gtk_snapshot_push_rounded_clip
;;;     gtk_snapshot_push_shadow
;;;     gtk_snapshot_push_stroke                           Since 4.14 unstable
;;;     gtk_snapshot_render_background                     Deprecated 4.10
;;;     gtk_snapshot_render_focus                          Deprecated 4.10
;;;     gtk_snapshot_render_frame                          Deprecated 4.10
;;;     gtk_snapshot_render_insertion_cursor               Deprecated 4.10
;;;     gtk_snapshot_render_layout                         Deprecated 4.10
;;;     gtk_snapshot_restore
;;;     gtk_snapshot_rotate
;;;     gtk_snapshot_rotate_3d
;;;     gtk_snapshot_save
;;;     gtk_snapshot_scale
;;;     gtk_snapshot_scale_3d
;;;     gtk_snapshot_to_node
;;;     gtk_snapshot_to_paintable
;;;     gtk_snapshot_transform
;;;     gtk_snapshot_transform_matrix
;;;     gtk_snapshot_translate
;;;     gtk_snapshot_translate_3d
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
;;; gtk_snapshot_append_border
;;;
;;; Appends a stroked border rectangle inside the given outline.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_cairo
;;;
;;; Creates a new GskCairoNode and appends it to the current render node of
;;; snapshot, without changing the current node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_color
;;;
;;; Creates a new render node drawing the color into the given bounds and
;;; appends it to the current render node of snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_conic_gradient
;;;
;;; Appends a conic gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_fill
;;;
;;; A convenience method to fill a path with a color.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_inset_shadow
;;;
;;; Appends an inset shadow into the box given by outline.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_layout
;;;
;;; No description available.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_linear_gradient
;;;
;;; Appends a linear gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_node
;;;
;;; Appends node to the current render node of snapshot, without changing the
;;; current node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_outset_shadow
;;;
;;; Appends an outset shadow node around the box given by outline.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_radial_gradient
;;;
;;; Appends a radial gradient node with the given stops to snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_repeating_linear_gradient
;;;
;;; Appends a repeating linear gradient node with the given stops to snapshot.
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

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_stroke
;;;
;;; A convenience method to stroke a path with a color.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_texture
;;;
;;; Creates a new render node drawing the texture into the given bounds and
;;; appends it to the current render node of snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_node
;;;
;;; Returns the node that was constructed by snapshot and frees snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_paintable
;;;
;;; Returns a paintable for the node that was constructed by snapshot and frees
;;; snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_gl_shader_pop_texture
;;;
;;; Removes the top element from the stack of render nodes and adds it to the
;;; nearest GskGLShaderNode below it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_perspective
;;;
;;; Applies a perspective projection transform.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_pop
;;;
;;; Removes the top element from the stack of render nodes, and appends it to
;;; the node underneath it.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blend
;;;
;;; Blends together two images with the given blend mode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blur
;;;
;;; Blurs an image.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_clip
;;;
;;; Clips an image to a rectangle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_color_matrix
;;;
;;; Modifies the colors of an image by applying an affine transformation in RGB
;;; space.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_cross_fade
;;;
;;; Snapshots a cross-fade operation between two images with the given progress.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_debug
;;;
;;; Inserts a debug node with a message.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_fill
;;;
;;; Fills the area given by path and fill_rule with an image and discards
;;; everything outside of it.
;;;
;;; unstable Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_gl_shader
;;;
;;; Push a GskGLShaderNode.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_mask
;;;
;;; Until the first call to gtk_snapshot_pop(), the mask image for the mask
;;; operation will be recorded.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_opacity
;;;
;;; Modifies the opacity of an image.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_repeat
;;;
;;; Creates a node that repeats the child node.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_rounded_clip
;;;
;;; Clips an image to a rounded rectangle.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_shadow
;;;
;;; Applies a shadow to an image.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_stroke
;;;
;;; Strokes the given path with the attributes given by stroke and an image.
;;;
;;; unstable Since 4.14
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
;;; gtk_snapshot_render_focus
;;;
;;; Creates a render node for the focus outline according to context, and
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
;;; gtk_snapshot_render_insertion_cursor
;;;
;;; Draws a text caret using snapshot at the specified index of layout.
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

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_restore
;;;
;;; Restores snapshot to the state saved by a preceding call to
;;; gtk_snapshot_save() and removes that state from the stack of saved states.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate
;;;
;;; Rotates @snapshot‘s coordinate system by angle degrees in 2D space - or in
;;; 3D speak, rotates around the Z axis. The rotation happens around the origin
;;; point of (0, 0) in the snapshot‘s current coordinate system.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate_3d
;;;
;;; Rotates snapshot‘s coordinate system by angle degrees around axis.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_save
;;;
;;; Makes a copy of the current state of snapshot and saves it on an internal
;;; stack.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale
;;;
;;; Scales snapshot‘s coordinate system in 2-dimensional space by the given
;;; factors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale_3d
;;;
;;; Scales snapshot‘s coordinate system by the given factors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_node
;;;
;;; Returns the render node that was constructed by snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_paintable
;;;
;;; Returns a paintable encapsulating the render node that was constructed by
;;; snapshot.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform
;;;
;;; Transforms snapshot‘s coordinate system with the given transform.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform_matrix
;;;
;;; Transforms snapshot‘s coordinate system with the given matrix.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate
;;;
;;; Translates snapshot‘s coordinate system by point in 2-dimensional space.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate_3d
;;;
;;; Translates snapshot‘s coordinate system by point.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.snapshot.lisp -----------------------------------------
