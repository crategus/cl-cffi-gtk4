;;; ----------------------------------------------------------------------------
;;; gtk4.snapshot.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;;     gtk_snapshot_push_gl_shader                        not implemented
;;;     gtk_snapshot_push_mask                             Since 4.10
;;;     gtk_snapshot_push_fill                             Since 4.14 unstable
;;;     gtk_snapshot_push_stroke                           Since 4.14 unstable
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture                 not implemented
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
;;; gtk_snapshot_new
;;; ----------------------------------------------------------------------------

(declaim (inline snapshot-new))

(defun snapshot-new ()
 #+liber-documentation
 "@version{#2023-10-21}
  @return{The newly allocated @class{gtk:snapshot} object.}
  @short{Creates a new @class{gtk:snapshot} object.}
  @see-class{gtk:snapshot}"
  (make-instance 'snapshot))

(export 'snapshot-new)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_node
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_node" snapshot-to-node) gsk:render-node
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @return{The constructed @class{gsk:render-node} instance or @code{nil} if
    there are no nodes to render.}
  @begin{short}
    Returns the render node that was constructed by @arg{snapshot}.
  @end{short}
  Note that this function may return @code{nil} if nothing has been added to the
  snapshot or if its content does not produce pixels to be rendered.

  After calling this function, it is no longer possible to add more nodes to
  snapshot.
  @see-class{gtk:snapshot}
  @see-class{gsk:render-node}"
  (snapshot (g:object snapshot)))

(export 'snapshot-to-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_paintable" snapshot-to-paintable)
    (g:object gdk:paintable)
 #+liber-documentation
 "@version{#2023-10-21}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[size]{a @symbol{graphene:size-t} instance with the size of the
    resulting paintable or @code{nil} to use the bounds of the snapshot}
  @return{The new @class{gdk:paintable} object.}
  @begin{short}
    Returns a paintable encapsulating the render node that was constructed by
    @arg{snapshot}.
  @end{short}
  After calling this function, it is no longer possible to add more nodes to the
  snapshot.
  @see-class{gtk:snapshot}
  @see-class{gdk:paintable}
  @see-symbol{graphene:size-t}"
  (snapshot (g:object snapshot))
  (size (:pointer (:struct graphene:size-t))))

(export 'snapshot-to-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_node
;;;
;;; Returns the node that was constructed by snapshot and frees snapshot.
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_snapshot_free_to_node" snapshot-free-to-node)
    gsk:render-node
  (snapshot (g:object snapshot)))

#+nil
(export 'snapshot-free-to-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_free_to_paintable
;;;
;;; Returns a paintable for the node that was constructed by snapshot and frees
;;; snapshot.
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("gtk_snapshot_free_to_paintable" snapshot-free-to-paintable)
    (g:object gdk:paintable)
  (snapshot (g:object snapshot))
  (size (:pointer (:struct graphene:size-t))))

#+nil
(export 'snapshot-free-to-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_opacity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_opacity" snapshot-push-opacity) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[opacity]{a double float with the opacity to use}
  @begin{short}
    Modifies the opacity of an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (opacity :double))

(export 'snapshot-push-opacity)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_color_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_color_matrix" snapshot-push-color-matrix)
    :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the color matrix
    to use}
  @argument[offset]{a @symbol{graphene:vec4-t} instance with the color offset
    to use}
  @begin{short}
    Modifies the colors of an image by applying an affine transformation in RGB
    space.
  @end{short}
  In particular, the colors will be transformed by applying
  @begin{pre}
pixel = transpose(matrix) * pixel + offset
  @end{pre}
  for every pixel. The transformation operates on unpremultiplied colors, with
  color components ordered R, G, B, A.

  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-symbol{graphene:matrix-t}
  @see-symbol{graphene:vec4-t}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (matrix (:pointer (:struct graphene:matrix-t)))
  (offset (:pointer (:struct graphene:vec4-t))))

(export 'snapshot-push-color-matrix)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_repeat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_repeat" snapshot-push-repeat) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds within
    which to repeat}
  @argument[child-bounds]{a @symbol{graphene:rect-t} instance with the bounds
    of the child or @code{nil} to use the full size of the collected child node}
  @begin{short}
    Creates a node that repeats the child node.
  @end{short}
  The child is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (child-bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-push-repeat)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_clip" snapshot-push-clip) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the rectangle to
    clip to}
  @begin{short}
    Clips an image to a rectangle.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-push-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_rounded_clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_rounded_clip" snapshot-push-rounded-clip)
    :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{gsk:rounded-rect} instance with the rounded
    rectangle to clip to}
  @begin{short}
    Clips an image to a rounded rectangle.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-symbol{gsk:rounded-rect}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct gsk:rounded-rect))))

(export 'snapshot-push-rounded-clip)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_cross_fade
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_cross_fade" snapshot-push-cross-fade) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[progress]{a double float with the progress between to 0.0 and 1.0}
  @begin{short}
    Snapshots a cross-fade operation between two images with the given progress.
  @end{short}
  Until the first call to the @fun{gtk:snapshot-pop} function, the start image
  will be @arg{snapshot}. After that call, the end image will be recorded until
  the second call to the @fun{gtk:snapshot-pop} function.

  Calling this function requires two subsequent calls to the
  @fun{gtk:snapshot-pop} function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (progress :double))

(export 'snapshot-push-cross-fade)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blend" snapshot-push-blend) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[blend-mode]{a @symbol{gsk:blend-mode} value to use}
  @begin{short}
    Blends together two images with the given blend mode.
  @end{short}
  Until the first call to the @fun{gtk:snapshot-pop} function, the bottom image
  for the blend operation will be recorded. After that call, the top image to be
  blended will be recorded until the second call to the @fun{gtk:snapshot-pop}
  function.

  Calling this function requires two subsequent calls to the
  @fun{gtk:snapshot-pop} function.
  @see-class{gtk:snapshot}
  @see-symbol{gsk:blend-mode}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (blend-mode gsk:blend-mode))

(export 'snapshot-push-blend)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blur
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blur" snapshot-push-blur) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[radius]{a double float with the blur radius to use, must be postive}
  @begin{short}
    Blurs an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
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
;;; gtk_snapshot_push_gl_shader                            not implemented
;;; ----------------------------------------------------------------------------

;;; No OpenGl support implemented.

#+nil
(cffi:defcfun ("gtk_snapshot_push_gl_shader" snapshot-push-gl-shader) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[shader]{a @class{gsk:gl-shader} object  with the code to run}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the rectangle to
    render into}
  @argument[take-args]{a @class{g:bytes} instance with the data block with
    arguments for the shader}
  @begin{short}
    Push a @class{gsk:gl-shader-node} instance.
  @end{short}
  The node uses the given @class{gsk:gl-shader} instance and uniform values.
  Additionally this takes a list of @arg{n_children} other nodes which will be
  passed to the @class{gsk:gl-shader-node} instance.

  The @arg{take_args} argument is a block of data to use for uniform arguments,
  as per types and offsets defined by the shader. Normally this is generated by
  the @fun{gsk:gl-shader-format-args} function or the
  @class{gsk:shader-args-builder} instance.

  The snapshotter takes ownership of @arg{take_args}, so the caller should not
  free it after this.

  If the renderer does not support GL shaders, or if there is any problem when
  compiling the shader, then the node will draw pink. You should use the
  @fun{gsk:gl-shader-compile} function to ensure the shader will work for the
  renderer before using it.

  If the shader requires textures (see the @fun{gsk:gl-shader-n-textures}
  function), then it is expected that you call the
  @fun{gtk:snapshot-gl-shader-pop-texture} function the number of times that are
  required. Each of these calls will generate a node that is added as a child to
  the @class{gsk:gl-shader-node} instance, which in turn will render these
  offscreen and pass as a texture to the shader.

  Once all textures (if any) are pop:ed, you must call the regular the
  @fun{gtk:snapshot-pop} function.

  If you want to use pre-existing textures as input to the shader rather than
  rendering new ones, use the @fun{gtk:snapshot-append-texture} function to push
  a texture node. These will be used directly rather than being re-rendered.

  For details on how to write shaders, see the @class{gsk:gl-shader}
  documentation.
  @see-class{gtk:snapshot}
  @see-class{gsk:gl-shader}
  @see-symbol{graphene:rect-t}
  @see-class{g:bytes}"
  (snapshot (g:object snapshot))
  (shader (g:object gsk:gl-shader))
  (bounds (:pointer (:struct graphene:rect-t)))
  (take-args (g:boxed g:bytes)))

#+nil
(export 'snapshot-push-gl-shader)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_mask
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_snapshot_push_mask" snapshot-push-mask) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[mask-mode]{a @symbol{maks-mode} value to use}
  @begin{short}
    Until the first call to the @fun{gtk:snapshot-pop} function, the mask image
    for the mask operation will be recorded.
  @end{short}
  After that call, the source image will be recorded until the second call to
  the @fun{gtk:snapshot-pop} function.

  Calling this function requires 2 subsequent calls to the
  @fun{gtk:snapshot-pop} function.

  Since 4.10
  @see-class{gtk:snapshot}
  @see-symbol{gsk:mask-mode}"
  (snapshot (g:object snapshot))
  (mask-mode gsk:mask-mode))

#+gtk-4-10
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_pop" snapshot-pop) :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Removes the top element from the stack of render nodes, and appends it to
    the node underneath it.
  @end{short}
  @see-class{gtk:snapshot}"
  (snapshot (g:object snapshot)))

(export 'snapshot-pop)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_gl_shader_pop_texture                     not implemented
;;; ----------------------------------------------------------------------------

;; TODO: No OpenGL support implemented.

#+nil
(cffi:defcfun ("gtk_snapshot_gl_shader_pop_texture"
               snapshot-gl-shader-pop-texture) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Removes the top element from the stack of render nodes and adds it to the
    nearest @class{gsk:gl-shader-node} instance below it.
  @end{short}
  This must be called the same number of times as the number of textures is
  needed for the shader in the @fun{gtk:snapshot-push-gl-shader} function.
  @see-class{gtk:snapshot}
  @see-class{gsk:gl-shader-node}
  @see-function{gtk:snapshot-gl-shader}"
  (snapshot (g:object snapshot)))

#+nil
(export 'snapshot-gl-shader-pop-texture)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_save
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_save" snapshot-save) :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Makes a copy of the current state of @arg{snapshot} and saves it on an
    internal stack.
  @end{short}
  When the @fun{gtk:snapshot-restore} function is called, the snapshot will be
  restored to the saved state. Multiple calls to the @fun{gtk:snapshot-save}
  function and the @fun{gtk:snapshot-restore} function can be nested. Each call
  to the @fun{gtk:snapshot-restore} function restores the state from the
  matching paired @fun{gtk:snapshot-save} function.

  It is necessary to clear all saved states with corresponding calls to the
  @fun{gtk:snapshot-restore} function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-restore}"
  (snapshot (g:object snapshot)))

(export 'snapshot-save)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_restore
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_restore" snapshot-restore) :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Restores snapshot to the state saved by a preceding call to the
    @fun{gtk:snapshot-save} function and removes that state from the stack of
    saved states.
  @end{short}
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-save}"
  (snapshot (g:object snapshot)))

(export 'snapshot-restore)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_transform" snapshot-transform) :void
 #+liber-documentation
 "@version{#2023-10-22}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[transform]{a @class{gsk:transform} instance}
  @begin{short}
    Transforms snapshot‘s coordinate system with the given transform.
  @end{short}
  @see-class{gtk:snapshot}
  @see-class{gsk:transform}"
  (snapshot (g:object snapshot))
  (transform (g:boxed gsk:transform)))

(export 'snapshot-transform)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_transform_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_transform_matrix" snapshot-transform-matrix) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the matrix
    to muliply the transform with}
  @begin{short}
    Transforms snapshot‘s coordinate system with the given matrix.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:matrix-t}"
  (snapshot (g:object snapshot))
  (matrix (:pointer (:struct graphene:matrix-t))))

(export 'snapshot-transform-matrix)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_translate" snapshot-translate) :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[point]{a @symbol{graphene:point-t} instance with the point to
    translate the snapshot by}
  @begin{short}
    Translates the coordinate system of the snapshot by @arg{point} in
    2-dimensional space.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symobl{graphene:point-t}"
  (snapshot (g:object snapshot))
  (point (:pointer (:struct graphene:point-t))))

(export 'snapshot-translate)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_translate_3d" snapshot-translate-3d) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[point]{a @symbol{graphene:point3d-t} instance with the point to
    translate the snapshot by}
  @begin{short}
    Translates snapshot‘s coordinate system by @arg{point}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symobl{graphene:point-t}"
  (snapshot (g:object snapshot))
  (point (:pointer (:struct graphene:point3d-t))))

(export 'snapshot-translate-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_rotate" %snapshot-rotate) :void
  (snapshot (g:object snapshot))
  (angle :float))

(defun snapshot-rotate (snapshot angle)
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[angle]{a number coerced to a float with rotation angle, in degrees
    (clockwise)}
  @begin{short}
    Rotates the coordinate system of the snapshot by @arg{angle} degrees in 2D
    space - or in 3D speak, rotates around the z axis.
  @end{short}
  The rotation happens around the origin point of (0, 0) in the  current
  coordinate system of the snapshot.

  To rotate around axes other than the z axis, use the
  @fun{gsk:transform-rotate-3d} function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-rotate-3d}"
  (%snapshot-rotate snapshot (coerce angle 'single-float)))

(export 'snapshot-rotate)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_rotate_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_rotate_3d" %snapshot-rotate-3d) :void
  (snapshot (g:object snapshot))
  (angle :float)
  (axis (:pointer (:struct graphene:vec3-t))))

(defun snapshot-rotate-3d (snapshot angle axis)
 #+liber-documentation
 "@version{#2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[angle]{a number coerced to a float with rotation angle, in degrees
    (clockwise)}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with the rotation axis}
  @begin{short}
    Rotates the coordinate system of the snapshot by @arg{angle} degrees around
    @arg{axis}.
  @end{short}
  For a rotation in 2D space, use the @fun{gtk:snapshot-rotate} function.
  @see-class{gtk:snapshot}
  @see-symbol{graphene:vec3-t}
  @see-function{gtk:snapshot-rotate}"
  (%snapshot-rotate-3d snapshot (coerce angle 'single-float) axis))

(export 'snapshot-rotate-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_scale" %snapshot-scale) :void
  (snapshot (g:object snapshot))
  (xfactor :float)
  (yfactor :float))

(defun snapshot-scale (snapshot xfactor yfactor)
 #+liber-documentation
 "@version{2023-2-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[xfactor]{a number coerced to a float with the scaling factor on
    the x axis}
  @argument[yfactor]{a number coerced to a float with the scaling factor on
    the y axis}
  @begin{short}
    Scales the coordinate system of the snapshot in 2-dimensional space by the
    given factors.
  @end{short}
  Use the @fun{gtk:snapshot-scale-3d} function to scale in all 3 dimensions.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-scale-3d}"
  (%snapshot-scale snapshot (coerce xfactor 'single-float)
                            (coerce yfactor 'single-float)))

(export 'snapshot-scale)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_scale_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_scale_3d" %snapshot-scale-3d) :void
  (snapshot (g:object snapshot))
  (xfactor :float)
  (yfactor :float)
  (zfactor :float))

(defun snapshot-scale-3d (snapshot xfactor yfactor zfactor)
 #+liber-documentation
 "@version{#2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[xfactor]{a number coerced to a float with the scaling factor on
    the x axis}
  @argument[yfactor]{a number coerced to a float with the scaling factor on
    the y axis}
  @argument[zfactor]{a number coerced to a float with the scaling factor on
    the z axis}
  @begin{short}
    Scales the coordinate system of the snapshot by the given factors.
  @end{short}
  Use the @fun{gtk:snapshot-scale} function to scale in 2-dimensional space.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-scale}"
  (%snapshot-scale-3d snapshot (coerce xfactor 'single-float)
                               (coerce yfactor 'single-float)
                               (coerce zfactor 'single-float)))

(export 'snapshot-scale-3d)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_perspective
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_perspective" %snapshot-perspective) :void
  (snapshot (g:object snapshot))
  (depth :float))

(defun snapshot-perspective (snapshot depth)
 #+liber-documentation
 "@version{#2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[depth]{a number coerced to a float with the distance of z=0 plane}
  @begin{short}
    Applies a perspective projection transform.
  @end{short}
  See the @fun{gsk:transform-perspective} function for a discussion on the
  details.
  @see-class{gtk:snapshot}
  @see-function{gsk:transform-perspective}"
  (%snapshot-perspective snapshot (coerce depth 'single-float)))

(export 'snapshot-perspective)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_node
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_node" snapshot-append-node) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{short}
    Appends @arg{node} to the current render node of @arg{snapshot}, without
    changing the current node.
  @end{short}
  If @arg{snapshot} does not have a current node yet, node will become the
  initial node.
  @see-class{gtk:snapshot}
  @see-class{gsk-render-node}"
  (snapshot (g:object snapshot))
  (node gsk:render-node))

(export 'snapshot-append-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_cairo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_cairo" snapshot-append-cairo)
    (:pointer (:struct cairo:context-t))
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds for the
    new node}
  @return{The @symbol{cairo:context-t} instance suitable for drawing the
    contents of the newly created render node.}
  @begin{short}
    Creates a new @class{gsk:cairo-node} instance and appends it to the current
    render node of @arg{snapshot}, without changing the current node.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}"
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-cairo)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_texture" snapshot-append-texture) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[texture]{a @class{gdk:texture} object with the texture to render}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds for the
    new node}
  @begin{short}
    Creates a new render node drawing the texture into the given bounds and
    appends it to the current render node of @arg{snapshot}.
  @end{short}
  If the texture needs to be scaled to fill bounds, linear filtering is used.
  See the @fun{gtk:snapshot-append-scaled-texture} function if you need other
  filtering, such as nearest-neighbour.
  @see-class{gtk:snapshot}
  @see-class{gdk:texture}
  @see-symbol{graphene:rect-t}
  @see-function{gtk:snapshot-append-scaled-texture}"
  (snapshot (g:object snapshot))
  (texture (g:object gdk:texture))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-texture)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_color
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_color" snapshot-append-color) :void
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[color]{a @class{gdk:rgba} instance with the color to draw}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds for the
    node}
  @begin{short}
    Creates a new render node drawing the color into the given bounds and
    appends it to the current render node of @arg{snapshot}.
  @end{short}
  You should try to avoid calling this function if @arg{color} is transparent.
  @see-class{gtk:snapshot}
  @see-class{gdk:rgba}
  @see-symbol{graphene:rect-t}"
  (snapshot (g:object snapshot))
  (color (g:boxed gdk:rgba))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-color)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_layout" snapshot-append-layout) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[layout]{a @class{pango:layout} instance}
  @argument[color]{a @class{gdk:rgba} instance}
  @short{No description available.}
  @see-class{gtk:snapshot}
  @see-class{pango:layout}
  @see-class{gdk:rgba}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_border" %snapshot-append-border) :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (width (:pointer :float))
  (color :pointer))

(defun snapshot-append-border (snapshot outline width color)
 #+liber-documentation
 "@version{2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance with the outline of
    the border}
  @argument[width]{a list of 4 float with the stroke width of the border on
    the top, right, bottom, and left side respectivly}
  @argument[color]{a list of 4 @class{gdk:rgba} instances with the color used
    on the top, right, bottom, and left side}
  @begin{short}
    Appends a stroked border rectangle inside the given outline.
  @end{short}
  The four sides of the border can have different widths and colors.
  @see-class{gtk:snapshot}
  @see-symbol{gsk:rounded-rect}
  @see-class{gdk:rgba}"
  (cffi:with-foreign-object (width-ptr :float 4)
    (iter (for i from 0 below 4)
          (for borderwidth in width)
          (setf (cffi:mem-aref width-ptr :float i)
                (coerce borderwidth 'single-float)))
    (glib:with-foreign-boxed-array (n-colors colors-ptr gdk:rgba color)
      (%snapshot-append-border snapshot
                               outline
                               width-ptr
                               colors-ptr))))

(export 'snapshot-append-border)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_inset_shadow
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_inset_shadow" %snapshot-append-inset-shadow)
    :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(defun snapshot-append-inset-shadow (snapshot outline color dx dy spread radius)
 #+liber-documentation
 "@version{#2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance with the outline of
    the border}
  @argument[color]{a @class{gdk:rgba} instance with the color of the method}
  @argument[dx]{a float with the horizontal offset of shadow}
  @argument[dy]{a float with the vertical offset of shadow}
  @argument[spread]{a float value how far the shadow spreads towards the inside}
  @argument[radius]{a float value how much blur to apply to the shadow}
  @begin{short}
    Appends an inset shadow into the box given by @arg{outline}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{gsk:rounded-rect}
  @see-class{gdk:rgba}"
  (%snapshot-append-inset-shadow snapshot
                                 outline
                                 color
                                 (coerce dx 'single-float)
                                 (coerce dy 'single-float)
                                 (coerce spread 'single-float)
                                 (coerce radius 'single-float)))

(export 'snapshot-append-inset-shadow)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_outset_shadow
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_outset_shadow"
               %snapshot-append-outset-shadow) :void
 #+liber-documentation
 "@version{#2023-12-2}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance with the outline of
    the region surrounded by the shadow}
  @argument[color]{a @class{gdk:rgba} instance with the color of the shadow}
  @argument[dx]{a float with the horizontal offset of shadow}
  @argument[dy]{a float with the vertical offset of shadow}
  @argument[spread]{a float value how far the shadow spreads towards the
    outside}
  @argument[radius]{a float value how much blur to apply to the shadow}
  @begin{short}
    Appends an outset shadow node around the box given by @arg{outline}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{gsk:rounded-rect}
  @see-class{gdk:rgba}"
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (color (g:boxed gdk:rgba))
  (dx :float)
  (dy :float)
  (spread :float)
  (radius :float))

(defun snapshot-append-outset-shadow (snapshot
                                      outline
                                      color
                                      dx dy
                                      spread radius)
  (%snapshot-append-outset-shadow snapshot
                                  outline
                                  color
                                  (coerce dx 'single-float)
                                  (coerce dy 'single-float)
                                  (coerce spread 'single-float)
                                  (coerce radius 'single-float)))

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
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_snapshot_append_scaled_texture"
               snapshot-append-scaled-texture) :void
 #+liber-documentation
 "@version{#2023-10-23}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[texture]{a @class{gdk:texture} instance with the texture to render}
  @argument[filter]{a @class{gsk:scaling-filter} instance with the filter to
    use}
  @argument[bounds]{a @symbol{graphene:rect-t} instance with the bounds for the
    new node}
  @begin{short}
    Creates a new render node drawing the texture into the given bounds and
    appends it to the current render node of @arg{snapshot}.
  @end{short}
  In contrast to the @fun{gtk:snapshot-append-texture} function, this function
  provides control about how the filter that is used when scaling.

  Since 4.10
  @see-class{gtk:snapshot}
  @see-class{gdk:texture}
  @see-class{gsk:scaling-filter}
  @see-symbol{graphene:rect-t}
  @see-function{gtk:snapshot-append-texture}"
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
