;;; ----------------------------------------------------------------------------
;;; gtk4.snapshot.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;;     gtk_snapshot_free_to_node                           not implemented
;;;     gtk_snapshot_free_to_paintable                      not implemented
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
;;;     gtk_snapshot_push_gl_shader                        Deprecated 4.16
;;;     gtk_snapshot_push_mask                             Since 4.10
;;;     gtk_snapshot_push_fill                             Since 4.14
;;;     gtk_snapshot_push_stroke                           Since 4.14
;;;
;;;     gtk_snapshot_pop
;;;     gtk_snapshot_gl_shader_pop_texture                 Deprecated 4.16
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
;;;     gtk_snapshot_append_fill                           Since 4.14
;;;     gtk_snapshot_append_stroke                         Since 4.14
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

(gobject:define-gobject "GtkSnapshot" snapshot
  (:superclass gdk:snapshot
   :export t
   :interfaces ()
   :type-initializer "gtk_snapshot_get_type")
  nil)

#+liber-documentation
(setf (documentation 'snapshot 'type)
 "@version{2025-2-12}
  @begin{short}
    The @class{gtk:snapshot} object is an auxiliary object that assists in
    creating @class{gsk:render-node} objects for widgets.
  @end{short}
  It functions in a similar way to a Cairo context, and maintains a stack of
  render nodes and their associated transformations.

  The node at the top of the stack is the the one that the snapshot functions
  operate on. Use the @code{gtk:snapshot-push-...} functions and
  @fun{gtk:snapshot-pop} function to change the current node.

  The typical way to obtain a @class{gtk:snapshot} object is as an argument to
  the @fun{gtk:widget-snapshot} virtual function. If you need to create
  your own @class{gtk:snapshot} object, use the @fun{gtk:snapshot-new} function.
  @see-constructor{gtk:snapshot-new}
  @see-class{gdk:snapshot}
  @see-class{gsk:render-node}
  @see-function{gtk:widget-snapshot}")

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_new
;;; ----------------------------------------------------------------------------

(declaim (inline snapshot-new))

(defun snapshot-new ()
 #+liber-documentation
 "@version{2025-2-12}
  @return{The newly allocated @class{gtk:snapshot} object.}
  @short{Creates a new @class{gtk:snapshot} object.}
  @see-class{gtk:snapshot}"
  (make-instance 'snapshot))

(export 'snapshot-new)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_node
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_node" %snapshot-to-node) gsk:render-node
  (snapshot (g:object snapshot)))

(defun snapshot-to-node (snapshot)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{return}
    The constructed @class{gsk:render-node} instance or @code{nil} if there
    are no nodes to render.
  @end{return}
  @begin{short}
    Returns the render node that was constructed by @arg{snapshot}.
  @end{short}
  Note that this function may return @code{nil} if nothing has been added to the
  snapshot or if its content does not produce pixels to be rendered.

  After calling this function, it is no longer possible to add more nodes to
  @arg{snapshot}.
  @see-class{gtk:snapshot}
  @see-class{gsk:render-node}"
  (let ((node (%snapshot-to-node snapshot)))
    (unless (cffi:null-pointer-p node) node)))

(export 'snapshot-to-node)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_to_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_to_paintable" %snapshot-to-paintable)
    (g:object gdk:paintable :return)
  (snapshot (g:object snapshot))
  (size (:pointer (:struct graphene:size-t))))

(defun snapshot-to-paintable (snapshot &optional size)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[size]{a @symbol{graphene:size-t} instance for the size of the
    resulting paintable or the @code{nil} default value to use the bounds of
    the snapshot}
  @return{The new @class{gdk:paintable} object.}
  @begin{short}
    Returns a paintable encapsulating the render node that was constructed by
    @arg{snapshot}.
  @end{short}
  After calling this function, it is no longer possible to add more nodes to
  the snapshot.
  @see-class{gtk:snapshot}
  @see-class{gdk:paintable}
  @see-symbol{graphene:size-t}"
  (let ((size (or size (cffi:null-pointer))))
    (%snapshot-to-paintable snapshot size)))

(export 'snapshot-to-paintable)

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
;;; gtk_snapshot_push_opacity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_opacity" %snapshot-push-opacity) :void
  (snapshot (g:object snapshot))
  (opacity :double))

(defun snapshot-push-opacity (snapshot opacity)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[opacity]{a number coerced to a double float for the opacity to use}
  @begin{short}
    Modifies the opacity of an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
  (%snapshot-push-opacity snapshot (coerce opacity 'double-float)))

(export 'snapshot-push-opacity)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_color_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_color_matrix" snapshot-push-color-matrix)
    :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance for the color matrix
    to use}
  @argument[offset]{a @symbol{graphene:vec4-t} instance for the color offset
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

(cffi:defcfun ("gtk_snapshot_push_repeat" %snapshot-push-repeat) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (child-bounds (:pointer (:struct graphene:rect-t))))

(defun snapshot-push-repeat (snapshot bounds &optional child-bounds)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds within
    which to repeat}
  @argument[child-bounds]{a @symbol{graphene:rect-t} instance for the bounds
    of the child or @code{nil} to use the full size of the collected child node}
  @begin{short}
    Creates a node that repeats the child node.
  @end{short}
  The child is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-function{gtk:snapshot-pop}"
  (let ((child-bounds (or child-bounds (cffi:null-pointer))))
    (%snapshot-push-repeat snapshot bounds child-bounds)))

(export 'snapshot-push-repeat)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_clip
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_clip" snapshot-push-clip) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the rectangle
    to clip to}
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{gsk:rounded-rect} instance for the rounded
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

(cffi:defcfun ("gtk_snapshot_push_cross_fade" %snapshot-push-cross-fade) :void
  (snapshot (g:object snapshot))
  (progress :double))

(defun snapshot-push-cross-fade (snapshot progress)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[progress]{a number coerced to a double float for the progress
    between to 0.0 and 1.0}
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
  (%snapshot-push-cross-fade snapshot (coerce progress 'double-float)))

(export 'snapshot-push-cross-fade)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blend
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blend" snapshot-push-blend) :void
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[mode]{a @symbol{gsk:blend-mode} value to use}
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
  (mode gsk:blend-mode))

(export 'snapshot-push-blend)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_blur
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_blur" %snapshot-push-blur) :void
  (snapshot (g:object snapshot))
  (radius :double))

(defun snapshot-push-blur (snapshot radius)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[radius]{a number coerced to a double float for the blur radius
    to use, must be postive}
  @begin{short}
    Blurs an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
  (%snapshot-push-blur snapshot (coerce radius 'double-float)))

(export 'snapshot-push-blur)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_shadow
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_push_shadow" %snapshot-push-shadow) :void
  (snapshot (g:object snapshot))
  (shadow :pointer)
  (n-shadows :size))

(defun snapshot-push-shadow (snapshot shadows)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[shadows]{a list of the form @code{'((color1 dx1 dy1 radius1)
    (color2 dx2 dy2 radius2) ...)} with the shadows to apply}
  @argument[color]{a @class{gdk:rgba} instance for the color of the shadow}
  @argument[dx]{a number coerced to a single float for the horizontal offset
    of the shadow}
  @argument[dy]{a number coerced to a single float for the vertical offset
    of the shadow}
  @argument[radius]{a number coerced to a single float for the radius of the
    shadow}
  @begin{short}
    Applies a shadow to an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.
  @see-class{gtk:snapshot}
  @see-function{gtk:snapshot-pop}"
  (let ((n-shadows (length shadows))
        (cstruct-size (cffi:foreign-type-size '(:struct gdk::rgba-cstruct))))
    (cffi:with-foreign-object (shadows-ptr '(:struct gsk::%shadow) n-shadows)
      (iter (for i from 0 below n-shadows)
            (for (color dx dy radius) in shadows)
            (for ptr = (cffi:mem-aptr shadows-ptr '(:struct gsk::%shadow) i))
            (glib::copy-boxed-slots-to-foreign color ptr 'gdk:rgba)
            (cffi:incf-pointer ptr cstruct-size)
            (setf (cffi:mem-ref ptr :float) (coerce dx 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce dy 'single-float))
            (cffi:incf-pointer ptr (cffi:foreign-type-size :float))
            (setf (cffi:mem-ref ptr :float) (coerce radius 'single-float)))
      (%snapshot-push-shadow snapshot shadows-ptr n-shadows))))

(export 'snapshot-push-shadow)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_debug
;;;
;;; Inserts a debug node with a message.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_gl_shader                            Depreacted 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_mask                                  Since 4.10
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_snapshot_push_mask" snapshot-push-mask) :void
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[mode]{a @symbol{gsk:mask-mode} value to use}
  @begin{short}
    Until the first call to the @fun{gtk:snapshot-pop} function, the mask image
    for the mask operation will be recorded.
  @end{short}
  After that call, the source image will be recorded until the second call to
  the @fun{gtk:snapshot-pop} function.

  Calling this function requires two subsequent calls to the
  @fun{gtk:snapshot-pop} function.

  Since 4.10
  @see-class{gtk:snapshot}
  @see-symbol{gsk:mask-mode}
  @see-function{gtk:snapshot-pop}"
  (snapshot (g:object snapshot))
  (mode gsk:mask-mode))

#+gtk-4-10
(export 'snapshot-push-mask)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_fill
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_snapshot_push_fill" snapshot-push-fill) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[path]{a @class{gsk:path} instance for the path describing the area
    to fill}
  @argument[rule]{a @symbol{gsk:fill-rule} value to use}
  @begin{short}
    Fills the area given by @arg{path} and @arg{rule} with an image and discards
    everything outside of it.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function. If you want to fill the path with a color, the
  @fun{gtk:snapshot-append-fill} function may be more convenient.

  Since 4.14
  @see-class{gtk:snapshot}
  @see-class{gsk:path}
  @see-symbol{gsk:fill-rule}
  @see-function{gtk:snapshot-pop}
  @see-function{gtk:snapshot-append-fill}"
  (snapshot (g:object snapshot))
  (path (g:boxed gsk:path))
  (rule gsk:fill-rule))

#+gtk-4-14
(export 'snapshot-push-fill)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_push_stroke
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_snapshot_push_stroke" snapshot-push-stroke) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[path]{a @class{gsk:path} instance for the path to stroke}
  @argument[stroke]{a @class{gsk:stroke} instance for the stroke attributes}
  @begin{short}
    Strokes the given @arg{path} with the attributes given by @arg{stroke} and
    an image.
  @end{short}
  The image is recorded until the next call to the @fun{gtk:snapshot-pop}
  function.

  Note that the strokes are subject to the same transformation as everything
  else, so uneven scaling will cause horizontal and vertical strokes to have
  different widths.

  If you want to stroke the path with a color, the
  @fun{gtk:snapshot-append-stroke} function may be more convenient.

  Since 4.14
  @see-class{gtk:snapshot}
  @see-class{gsk:path}
  @see-class{gsk:stroke}
  @see-function{gtk:snapshot-pop}
  @see-function{gtk:snapshot-append-stroke}"
  (snapshot (g:object snapshot))
  (path (g:boxed gsk:path))
  (stroke (g:boxed gsk:stroke)))

#+gtk-4-14
(export 'snapshot-push-stroke)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_pop
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_pop" snapshot-pop) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Removes the top element from the stack of render nodes, and appends it to
    the node underneath it.
  @end{short}
  @see-class{gtk:snapshot}"
  (snapshot (g:object snapshot)))

(export 'snapshot-pop)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_gl_shader_pop_texture                     Deprecated 4.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_save
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_save" snapshot-save) :void
 #+liber-documentation
 "@version{2025-2-12}
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @begin{short}
    Restores @arg{snapshot} to the state saved by a preceding call to the
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[transform]{a @class{gsk:transform} instance}
  @begin{short}
    Transforms the coordinate system of the snapshot with the given
    @arg{transform}.
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance for the matrix
    to muliply the transform with}
  @begin{short}
    Transforms the coordinate system of the snapshot with the given
    @arg{matrix}.
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[point]{a @symbol{graphene:point-t} instance for the point to
    translate @arg{snapshot} by}
  @begin{short}
    Translates the coordinate system of the snapshot by @arg{point} in
    2-dimensional space.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:point-t}"
  (snapshot (g:object snapshot))
  (point (:pointer (:struct graphene:point-t))))

(export 'snapshot-translate)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_translate_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_translate_3d" snapshot-translate-3d) :void
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[point]{a @symbol{graphene:point3d-t} instance for the point to
    translate @arg{snapshot} by}
  @begin{short}
    Translates the coordinate system of the snapshot by @arg{point}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:point-t}"
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[angle]{a number coerced to a single float for the rotation angle,
    in degrees (clockwise)}
  @begin{short}
    Rotates the coordinate system of the snapshot by @arg{angle} degrees in 2D
    space - or in 3D speak, rotates around the Z axis.
  @end{short}
  The rotation happens around the origin point of @code{(0,0)} in the  current
  coordinate system of the snapshot.

  To rotate around axes other than the Z axis, use the
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[angle]{a number coerced to a single float for the rotation angle,
    in degrees (clockwise)}
  @argument[axis]{a @symbol{graphene:vec3-t} instance for the rotation axis}
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[xfactor]{a number coerced to a single float for the scaling factor
    on the X axis}
  @argument[yfactor]{a number coerced to a single float for the scaling factor
    on the Y axis}
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[xfactor]{a number coerced to a single float for the scaling factor
    on the X axis}
  @argument[yfactor]{a number coerced to a single float for the scaling factor
    on the Y axis}
  @argument[zfactor]{a number coerced to a single float for the scaling factor
    on the Z axis}
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[depth]{a number coerced to a single float for the distance of
    the @code{Z=0} plane}
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[node]{a @class{gsk:render-node} instance}
  @begin{short}
    Appends @arg{node} to the current render node of @arg{snapshot}, without
    changing the current node.
  @end{short}
  If @arg{snapshot} does not have a current node yet, @arg{node} will become
  the initial node.
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
    new node}
  @begin{return}
    The @symbol{cairo:context-t} instance suitable for drawing the contents
    of the newly created render node.
  @end{return}
  @begin{short}
    Creates a new @class{gsk:cairo-node} instance and appends it to the current
    render node of @arg{snapshot}, without changing the current node.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-symbol{cairo:context-t}
  @see-class{gsk:cairo-node}"
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t))))

(export 'snapshot-append-cairo)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_texture
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_texture" snapshot-append-texture) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[texture]{a @class{gdk:texture} object for the texture to render}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
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
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[color]{a @class{gdk:rgba} instance for the color to draw}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[layout]{a @class{pango:layout} instance to render}
  @argument[color]{a @class{gdk:rgba} instance for the foreground color to
    render the layout in}
  @begin{short}
    Creates render nodes for rendering @arg{layout} in the given foregound
    color and appends them to the current node of snapshot without changing the
    current node.
  @end{short}
  The current foreground color of the theme for a widget can be obtained with
  the @fun{gtk:widget-color} function.

  Note that if the layout does not produce any visible output, then nodes may
  not be added to the snapshot.
  @see-class{gtk:snapshot}
  @see-class{pango:layout}
  @see-class{gdk:rgba}
  @see-function{gtk:widget-color}"
  (snapshot (g:object snapshot))
  (layout (g:object pango:layout))
  (color (g:boxed gdk:rgba)))

(export 'snapshot-append-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_linear_gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_linear_gradient"
               %snapshot-append-linear-gradient) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (stops :pointer)
  (n-stops :size))

(defun snapshot-append-linear-gradient (snapshot bounds start end stops)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[start]{a @symbol{graphene:point-t} instance for the point at which
    the linear gradient will begin}
  @argument[end]{a @symbol{graphene:point-t} instance for the point at which
    the linear gradient will finish}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2
    color2) ...)} with the offsets and colors defining the gradient}
  @argument[offset]{a number coerced to a single float for the offset of the
    color stop}
  @argument[color]{a @class{gdk:rgba} instance for the color at the given
    offset}
  @begin{short}
    Appends a linear gradient node with the given color stops to @arg{snapshot}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct gsk::%color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr '(:struct gsk::%color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%snapshot-append-linear-gradient snapshot
                                        bounds
                                        start
                                        end
                                        stops-ptr
                                        n-stops))))

(export 'snapshot-append-linear-gradient)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_repeating_linear_gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_repeating_linear_gradient"
               %snapshot-append-repeating-linear-gradient) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (start (:pointer (:struct graphene:point-t)))
  (end (:pointer (:struct graphene:point-t)))
  (stops :pointer)
  (n-stops :size))

(defun snapshot-append-repeating-linear-gradient (snapshot
                                                  bounds start end stops)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the rectangle to
    render the linear gradient into}
  @argument[start]{a @symbol{graphene:point-t} instance for the point at which
    the linear gradient will begin}
  @argument[end]{a @symbol{graphene:point-t} instance for the point at which
    the linear gradient will finish}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @argument[offset]{a number coerced to a single float for the offset of the
    color stop}
  @argument[color]{a @class{gdk:rgba} instance for the color at the given
    offset}
  @begin{short}
    Appends a repeating linear gradient node with the given stops to
    @arg{snapshot}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct gsk::%color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr '(:struct gsk::%color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%snapshot-append-repeating-linear-gradient snapshot
                                                  bounds
                                                  start
                                                  end
                                                  stops-ptr
                                                  n-stops))))

(export 'snapshot-append-repeating-linear-gradient)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_conic_gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_conic_gradient"
               %snapshot-append-conic-gradient) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (rotation :float)
  (stops :pointer)
  (n-stops :size))

(defun snapshot-append-conic-gradient (snapshot bounds center rotation stops)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
    render node}
  @argument[center]{a @symbol{graphene:point-t} instance for the center of the
    gradient}
  @argument[rotation]{a number coerced to a single float for the rotation of
    the gradient, in degrees}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @argument[offset]{a number coerced to a single float for the offset of the
    color stop}
  @argument[color]{a @class{gdk:rgba} instance for the color at the given
    offset}
  @begin{short}
    Appends a conic gradient node with the given stops to @arg{snapshot}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct gsk::%color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr '(:struct gsk::%color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%snapshot-append-conic-gradient snapshot
                                       bounds
                                       center
                                       (coerce rotation 'single-float)
                                       stops-ptr
                                       n-stops))))

(export 'snapshot-append-conic-gradient)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_border
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_border" %snapshot-append-border) :void
  (snapshot (g:object snapshot))
  (outline (:pointer (:struct gsk:rounded-rect)))
  (width (:pointer :float))
  (color :pointer))

(defun snapshot-append-border (snapshot outline widths colors)
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance for the outline of
    the border}
  @argument[widths]{a list of 4 numbers coerced to single floats for the stroke
    width of the border on the top, right, bottom, and left side respectivly}
  @argument[colors]{a list of 4 @class{gdk:rgba} instances for the colors used
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
          (for borderwidth in widths)
          (setf (cffi:mem-aref width-ptr :float i)
                (coerce borderwidth 'single-float)))
    (glib:with-gboxed-array (n-colors colors-ptr gdk:rgba colors)
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
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance for the outline of
    the border}
  @argument[color]{a @class{gdk:rgba} instance for the color of the shadow}
  @argument[dx]{a number coerced to a single float for the horizontal offset
    of the shadow}
  @argument[dy]{a number coerced to a single float for the vertical offset
    of the shadow}
  @argument[spread]{a number coerced to a single float representing how far
    the shadow spreads towards the inside}
  @argument[radius]{a number coerced to a single float representing how much
    blur to apply to the shadow}
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
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[outline]{a @symbol{gsk:rounded-rect} instance for the outline of
    the region surrounded by the shadow}
  @argument[color]{a @class{gdk:rgba} instance for the color of the shadow}
  @argument[dx]{a number coerced to a single float for the horizontal offset of
    shadow}
  @argument[dy]{a number coerced to a single float for the vertical offset of
    shadow}
  @argument[spread]{a number coerced to a single float representing how far the
    shadow spreads towards the outside}
  @argument[radius]{a number coerced to a single float representing how much
    blur to apply to the shadow}
  @begin{short}
    Appends an outset shadow node around the box given by @arg{outline}.
  @end{short}
  @see-class{gtk:snapshot}
  @see-symbol{gsk:rounded-rect}
  @see-class{gdk:rgba}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_radial_gradient"
               %snapshot-append-radial-gradient) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (stops :pointer)
  (n-stops :size))

(defun snapshot-append-radial-gradient (snapshot
                                        bounds
                                        center
                                        hradius vradius
                                        start end stops)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the rectangle to
    render the radial gradient into}
  @argument[center]{a @symbol{graphene:point-t} instance for the center of
    the radial gradient}
  @argument[hradius]{a number coerced to a single float for the horizontal
    radius}
  @argument[vradius]{a number coerced to a single float for the vertical radius}
  @argument[start]{a number coerced to a single float for the percentage >= 0
    that defines the start of the radial gradient around @arg{center}}
  @argument[end]{a number coerced to a single float for the percentage >= 0
    that defines the end of the radial gradient around @arg{center}}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @argument[offset]{a number coerced to a single float for the offset of the
    color stop}
  @argument[color]{a @class{gdk:rgba} instance for the color at the given
    offset}
  @begin{short}
    Appends a radial gradient node with the given stops to @arg{snapshot}.
  @end{short}
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct gsk::%color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct gsk::%color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%snapshot-append-radial-gradient snapshot
                                        bounds
                                        center
                                        (coerce hradius 'single-float)
                                        (coerce vradius 'single-float)
                                        (coerce start 'single-float)
                                        (coerce end 'single-float)
                                        stops-ptr
                                        n-stops))))

(export 'snapshot-append-radial-gradient)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_repeating_radial_gradient
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_snapshot_append_repeating_radial_gradient"
               %snapshot-append-repeating-radial-gradient) :void
  (snapshot (g:object snapshot))
  (bounds (:pointer (:struct graphene:rect-t)))
  (center (:pointer (:struct graphene:point-t)))
  (hradius :float)
  (vradius :float)
  (start :float)
  (end :float)
  (stops :pointer)
  (n-stops :size))

(defun snapshot-append-repeating-radial-gradient (snapshot
                                                  bounds
                                                  center
                                                  hradius vradius
                                                  start end
                                                  stops)
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the rectangle to
    render the radial gradient into}
  @argument[center]{a @symbol{graphene:point-t} instance for the center of
    the radial gradient}
  @argument[hradius]{a number coerced to a single float for the horizontal
    radius}
  @argument[vradius]{a number coerced to a single float for the vertical radius}
  @argument[start]{a number coerced to a single float for the percentage >= 0
    that defines the start of the radial gradient around @arg{center}}
  @argument[end]{a number coerced to a single float for the percentage >= 0
    that defines the end of the radial gradient around @arg{center}}
  @argument[stops]{a list of the form @code{'((offset1 color1) (offset2 color2)
    ...)} with the offsets and colors defining the gradient}
  @argument[offset]{a number coerced to a single float for the offset of the
    color stop}
  @argument[color]{a @class{gdk:rgba} instance for the color at the given
    offset}
  @begin{short}
    Appends a repeating radial gradient node with the given stops to
    @arg{snapshot}.
  @end{short}
  @see-class{gsk:linear-gradient-node}
  @see-symbol{graphene:rect-t}
  @see-symbol{graphene:point-t}"
  (let ((n-stops (length stops)))
    (cffi:with-foreign-object (stops-ptr '(:struct gsk::%color-stop) n-stops)
      (iter (for i from 0 below n-stops)
            (for (offset color) in stops)
            (for ptr = (cffi:mem-aptr stops-ptr
                                      '(:struct gsk::%color-stop) i))
            (setf (cffi:mem-ref ptr :float) (coerce offset 'single-float))
            (glib::copy-boxed-slots-to-foreign
                color
                (cffi:inc-pointer ptr
                                  (cffi:foreign-type-size :float))
                'gdk:rgba))
      (%snapshot-append-repeating-radial-gradient snapshot
                                                  bounds
                                                  center
                                                  (coerce hradius 'single-float)
                                                  (coerce vradius 'single-float)
                                                  (coerce start 'single-float)
                                                  (coerce end 'single-float)
                                                  stops-ptr
                                                  n-stops))))

(export 'snapshot-append-repeating-radial-gradient)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_scaled_texture
;;; ----------------------------------------------------------------------------

#+gtk-4-10
(cffi:defcfun ("gtk_snapshot_append_scaled_texture"
               snapshot-append-scaled-texture) :void
 #+liber-documentation
 "@version{#2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[texture]{a @class{gdk:texture} object for the texture to render}
  @argument[filter]{a @symbol{gsk:scaling-filter} value for the filter to use}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of the
    new node}
  @begin{short}
    Creates a new render node drawing the texture into the given bounds and
    appends it to the current render node of @arg{snapshot}.
  @end{short}
  In contrast to the @fun{gtk:snapshot-append-texture} function, this function
  provides control over the filter that is used for scaling.

  Since 4.10
  @see-class{gtk:snapshot}
  @see-class{gdk:texture}
  @see-symbol{gsk:scaling-filter}
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
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_snapshot_append_fill" snapshot-append-fill) :void
 #+liber-documentation
 "@version{2025-2-12}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[path]{a @class{gsk:path} instance for the path describing the area
    to fill}
  @argument[rule]{a @symbol{gsk:fill-rule} value to use}
  @argument[color]{a @class{gdk:rgba} instance for the color to fill the path
    with}
  @begin{short}
    A convenience method to fill a path with a color.
  @end{short}
  See the @fun{gtk:snapshot-push-fill} function if you need to fill a path with
  more complex content than a color.

  Since 4.14
  @see-class{gtk:snapshot}
  @see-class{gsk:path}
  @see-symbol{gsk:fill-rule}
  @see-class{gdk:rgba}
  @see-function{gtk:snapshot-push-fill}"
  (snapshot (g:object snapshot))
  (path (g:boxed gsk:path))
  (rule gsk:fill-rule)
  (color (g:boxed gdk:rgba)))

#+gtk-4-14
(export 'snapshot-append-fill)

;;; ----------------------------------------------------------------------------
;;; gtk_snapshot_append_stroke
;;; ----------------------------------------------------------------------------

#+gtk-4-14
(cffi:defcfun ("gtk_snapshot_append_stroke" snapshot-append-stroke) :void
 #+liber-documentation
 "@version{2025-3-1}
  @argument[snapshot]{a @class{gtk:snapshot} object}
  @argument[path]{a @class{gsk:path} instance for the path to stroke}
  @argument[stroke]{a @class{gsk:stroke} instance for the stroke attributes}
  @argument[color]{a @class{gdk:rgba} instance for the color to fill the path
    with}
  @begin{short}
    A convenience method to stroke a path with a color.
  @end{short}
  See the @fun{gtk:snapshot-push-stroke} function if you need to stroke a path
  with more complex content than a color.

  Since 4.14
  @see-class{gtk:snapshot}
  @see-class{gsk:path}
  @see-class{gsk:stroke}
  @see-function{gtk:snapshot-push-stroke}"
  (snapshot (g:object snapshot))
  (path (g:boxed gsk:path))
  (stroke (g:boxed gsk:stroke))
  (color (g:boxed gdk:rgba)))

#+gtk-4-14
(export 'snapshot-append-stroke)

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
