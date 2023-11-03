;;; ----------------------------------------------------------------------------
;;; gsk4.transform.lisp
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
;;; GskTransform
;;;
;;;     A description for transform operations
;;;
;;; Types and Values
;;;
;;;     GskTransform
;;;     GskTransformCategory
;;;
;;; Functions
;;;
;;;     gsk_transform_new
;;;     gsk_transform_ref
;;;     gsk_transform_unref
;;;     gsk_transform_get_category
;;;     gsk_transform_print
;;;     gsk_transform_to_string
;;;     gsk_transform_parse
;;;     gsk_transform_to_matrix
;;;     gsk_transform_to_2d

;;;     gsk_transform_to_2d_components                     Since 4.6

;;;     gsk_transform_to_affine
;;;     gsk_transform_to_translate
;;;     gsk_transform_transform
;;;     gsk_transform_invert
;;;     gsk_transform_matrix
;;;     gsk_transform_translate
;;;     gsk_transform_translate_3d
;;;     gsk_transform_rotate
;;;     gsk_transform_rotate_3d
;;;     gsk_transform_scale
;;;     gsk_transform_scale_3d

;;;     gsk_transform_skew                                 Since 4.6

;;;     gsk_transform_perspective
;;;     gsk_transform_equal
;;;     gsk_transform_transform_bounds
;;;     gsk_transform_transform_point
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; enum GskTransformCategory
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GskTransformCategory" transform-category
  (:export t
   :type-initializer "gsk_transform_category_get_type")
   :unknown
   :any
   :3d
   :2d
   :2d-affine
   :2d-translate
   :identity)

#+liber-documentation
(setf (liber:alias-for-symbol 'transform-category)
      "GEnum"
      (liber:symbol-documentation 'transform-category)
 "@version{2023-10-31}
  @begin{short}
    The categories of matrices relevant for GSK and GTK.
  @end{short}
  Note that the enumeration values are ordered in a way that any category
  includes matrices of all later categories. So if you want to for example
  check if a matrix is a 2D matrix, the category value of the matrix is greater
  or  equal the @code{:2d} value.

  Also keep in mind that rounding errors may cause matrices to not conform to
  their categories. Otherwise, matrix operations done via multiplication will
  not worsen categories. So for the matrix multiplication @code{C = A * B},
  @code{category(C) = (MIN (category(A), category(B))}.
  @begin[Example]{dictionary}
    With the helper function
    @begin{pre}
(defun category-value (category)
  (cffi:convert-to-foreign category 'gsk:transform-category))
    @end{pre}
    you can check if a matrix with @code{category} is a 2D matrix
    with
    @begin{pre}
(>= (category-value category) (category-value :2d))
    @end{pre}
  @end{dictionary}
  @begin[Implementation]{dictionary}
    @begin{pre}
(gobject:define-g-enum \"GskTransformCategory\" transform-category
  (:export t
   :type-initializer \"gsk_transform_category_get_type\")
   :unknown
   :any
   :3d
   :2d
   :2d-affine
   :2d-translate
   :identity)
    @end{pre}
    @begin[code]{table}
      @entry[:unknown]{The category of the matrix has not been determined.}
      @entry[:any]{Analyzing the matrix concluded that it does not fit in any
        other category.}
      @entry[:3d]{The matrix is a 3D matrix. This means that the w column (the
        last column) has the values (0, 0, 0, 1).}
      @entry[:2d]{The matrix is a 2D matrix. This is equivalent to the
        @fun{graphene:matrix-is-2d} function returning @em{true}. In particular,
        this means that Cairo can deal with the matrix.}
      @entry[:2d-affine]{The matrix is a combination of 2D scale and 2D
        translation operations. In particular, this means that any rectangle
        can be transformed exactly using this matrix.}
      @entry[:2d-translate]{The matrix is a 2D translation.}
      @entry[:identity]{The matrix is the identity matrix.}
    @end{table}
  @end{dictionary}
  @see-class{gsk:transform}")

;;; ----------------------------------------------------------------------------
;;; GskTransform
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "gsk_transform_get_type" :size))

;; gsk_transform_new is not documented but present in the GSK library
(cffi:defcfun ("gsk_transform_new" %transform-new) :pointer)

(glib:define-g-boxed-opaque transform "GskTransform"
  :alloc (%transform-new))

#+liber-documentation
(setf (liber:alias-for-class 'transform)
      "GBoxed"
      (documentation 'transform 'type)
 "@version{2023-10-31}
  @begin{short}
    The @class{gsk:transform} structure is a structure to describe transform
    matrices.
  @end{short}
  Unlike the @symbol{graphene:matrix-t} structure, the @class{gsk:transform}
  structure retains the steps in how a transform was constructed, and allows
  inspecting them. It is modeled after the way CSS describes transforms.

  The @class{gsk:transform} instances are immutable and cannot be changed after
  creation. This means code can safely expose them as properties of objects
  without having to worry about others changing them.
  @see-constructor{gsk:transform-new}
  @see-symbol{graphene:matrix-t}")

(export 'transform)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_new ()
;;; ----------------------------------------------------------------------------

(defun transform-new ()
 #+liber-documentation
 "@version{2023-10-31}
  @return{A new @class{gsk:transform} instance.}
  @short{Creates a new transform matrix.}
  @see-class{gsk:transform}"
  (make-instance 'transform))

(export 'transform-new)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_ref ()
;;;
;;; GskTransform *
;;; gsk_transform_ref (GskTransform *self);
;;;
;;; Acquires a reference on the given GskTransform.
;;;
;;; self :
;;;     a GskTransform.
;;;
;;; Returns :
;;;     the GskTransform with an additional reference.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_ref" %transform-ref) (g:boxed transform)
  (transform (g:boxed transform)))

;;; ----------------------------------------------------------------------------
;;; gsk_transform_unref ()
;;;
;;; void
;;; gsk_transform_unref (GskTransform *self);
;;;
;;; Releases a reference on the given GskTransform.
;;;
;;; If the reference was the last, the resources associated to the self are
;;; freed.
;;;
;;; self :
;;;     a GskTransform.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_unref" %transform-unref) :void
  (transform (g:boxed transform)))

;;; ----------------------------------------------------------------------------
;;; gsk_transform_get_category ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_get_category" transform-category)
    transform-category
 #+liber-documentation
 "@version{2023-10-31}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{A @symbol{gsk:transform-category} value with the category of the
    transform.}
  @short{Returns the category this transform belongs to.}
  @see-class{gsk:transform}
  @see-symbol{gsk:transform-category}"
  (transform (g:boxed transform)))

(export 'transform-category)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_print ()
;;;
;;; void
;;; gsk_transform_print (GskTransform *self,
;;;                      GString *string);
;;;
;;; Converts self into a human-readable string representation suitable for
;;; printing that can later be parsed with gsk_transform_parse().
;;;
;;; self :
;;;     a GskTransform.
;;;
;;; string :
;;;     The string to print into
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_string" transform-to-string) :string
 #+liber-documentation
 "@version{2023-10-31}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{A string for @arg{transform}.}
  @begin{short}
    Converts a matrix into a string that is suitable for printing and can later
    be parsed with the @fun{gsk:transform-parse} function.
  @end{short}
  @see-class{gsk:transform}
  @see-function{gsk:transform-parse}"
  (transform (g:boxed transform)))

(export 'transform-to-string)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_parse ()
;;; ----------------------------------------------------------------------------

;; FIXME: The implementation of gsk:transform-parse does not work. We get a
;; GskTransform instance, if we access the instance we get a memory fault.

(cffi:defcfun ("gsk_transform_parse" %transform-parse) :boolean
  (str :string)
  (transform (g:boxed transform :return)))

(defun transform-parse (str)
 "@version{#2023-10-31}
  @argument[str]{a string to parse}
  @return{A @class{gsk:transform} instance with the transform}
  @begin{short}
    Parses the given string into a transform.
  @end{short}
  Strings printed via the @fun{transform-to-string} function can be read in
  again successfully using this function. If the @arg{str} argument does not
  describe a valid transform, @em{false} is returned.
  @see-class{gsk:transform}
  @see-function{gsk:transform-to-string}"
  (let ((transform (transform-new)))
    (when (%transform-parse str (%transform-ref transform))
      transform)))

(export 'transform-parse)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_matrix" %transform-to-matrix) :void
  (transform (g:boxed transform))
  (matrix (:pointer (:struct graphene:matrix-t))))

(defun transform-to-matrix (transform matrix)
 #+liber-documentation
 "@version{2023-10-31}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the matrix
    to set}
  @return{The @symbol{graphene:matrix-t} instance with the matrix.}
  @begin{short}
    Computes the actual value of @arg{transform} and stores it in @arg{matrix}.
  @end{short}
  The previous value of @arg{matrix} will be ignored.
  @see-class{gsk:transform}
  @see-symbol{graphene:matrix-t}"
  (%transform-to-matrix transform matrix)
  matrix)

(export 'transform-to-matrix)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_2d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_2d" %transform-to-2d) :void
  (transform (g:boxed transform))
  (xx (:pointer :float))
  (yx (:pointer :float))
  (xy (:pointer :float))
  (yy (:pointer :float))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun  transform-to-2d (transform)
 #+liber-documentation
 "@version{#2023-11-3}
  @argument[transform]{a @class{gtk:transform} instance}
  @return{A @code{(xx xy yx yy dx dy)} list with the single float values of the
    2D transformation matrix.}
  @begin{short}
    Converts a @class{gsk:transform} instace to a 2D transformation matrix.
  @end{short}
  The @arg{transform} argument must be a 2D transformation. If you are not sure,
  use the @fun{gsk:transform-category} function to check.

  The returned values have the following layout:
  @begin{pre}
| xx yx |   |  a  b  0 |
| xy yy | = |  c  d  0 |
| dx dy |   | tx ty  1 |
  @end{pre}
  This function can be used to convert between a @class{gsk:transform} instance
  and a matrix type from other 2D drawing libraries, in particular Cairo.
  @see-class{gsk:transform}
  @see-functon{gsk:transform-category}"
  (cffi:with-foreign-objects ((xx :float) (yx :float)
                              (xy :float) (yy :float)
                              (dx :float) (dy :float))
    (%transform-to-2d transform xx yx xy yy dx dy)
    (list (cffi:mem-ref xx :float) (cffi:mem-ref xy :float)
          (cffi:mem-ref yx :float) (cffi:mem-ref yy :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-2d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_2d_components ()
;;;
;;; Converts a GskTransform to 2D transformation factors.
;;;
;;; Since 4.6
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_2d_components" %transform-to-2d-components)
    :void
  (transform (g:boxed transform))
  (xskew (:pointer :float))
  (yskew (:pointer :float))
  (xscale (:pointer :float))
  (yscale (:pointer :float))
  (angle (:pointer :float))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun transform-to-2d-components (transform)
  (cffi:with-foreign-objects ((xskew :float)  (yskew :float)
                              (xscale :float) (yscale :float)
                              (angle :float)  (dx :float) (dy :float))
    (%transform-to-2d-components transform
                                 xskew yskew xscale yscale angle dx dy)
    (list (cffi:mem-ref xskew :float)  (cffi:mem-ref yskew :float)
          (cffi:mem-ref xscale :float) (cffi:mem-ref yscale :float)
          (cffi:mem-ref angle :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-2d-components)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_affine ()
;;;
;;; void
;;; gsk_transform_to_affine (GskTransform *self,
;;;                          float *out_scale_x,
;;;                          float *out_scale_y,
;;;                          float *out_dx,
;;;                          float *out_dy);
;;;
;;; Converts a GskTransform to 2D affine transformation factors. self must be a
;;; 2D transformation. If you are not sure, use gsk_transform_get_category() >=
;;; GSK_TRANSFORM_CATEGORY_2D_AFFINE to check.
;;;
;;; self :
;;;     a GskTransform
;;;
;;; out_scale_x :
;;;     return location for the scale factor in the x direction.
;;;
;;; out_scale_y :
;;;     return location for the scale factor in the y direction.
;;;
;;; out_dx :
;;;     return location for the translation in the x direction.
;;;
;;; out_dy :
;;;     return location for the translation in the y direction.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_affine" %transform-to-affine) :void
  (transform (g:boxed transform))
  (xscale (:pointer :float))
  (yscale (:pointer :float))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun transform-to-affine (transform)
  (cffi:with-foreign-objects ((xscale :float) (yscale :float)
                              (dx :float) (dy :float))
    (%transform-to-affine transform xscale yscale dx dy)
    (list (cffi:mem-ref xscale :float) (cffi:mem-ref yscale :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-affine)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_translate ()
;;;
;;; void
;;; gsk_transform_to_translate (GskTransform *self,
;;;                             float *out_dx,
;;;                             float *out_dy);
;;;
;;; Converts a GskTransform to a translation operation. self must be a 2D
;;; transformation. If you are not sure, use gsk_transform_get_category() >=
;;; GSK_TRANSFORM_CATEGORY_2D_TRANSLATE to check.
;;;
;;; self :
;;;     a GskTransform
;;;
;;; out_dx :
;;;     return location for the translation in the x direction.
;;;
;;; out_dy :
;;;     return location for the translation in the y direction.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_translate" %transform-to-translate) :void
  (transform (g:boxed transform))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun transform-to-translate (transform)
  (cffi:with-foreign-objects ((dx :float) (dy :float))
    (%transform-to-translate transform dx dy)
    (list (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-translate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform ()
;;;
;;; GskTransform *
;;; gsk_transform_transform (GskTransform *next,
;;;                          GskTransform *other);
;;;
;;; Applies all the operations from other to next .
;;;
;;; next :
;;;     Transform to apply other to.
;;;
;;; other :
;;;     Transform to apply.
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform" %transform-transform)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (other (g:boxed transform)))

(defun transform-transform (transform other)
  (%transform-transform (%transform-ref transform)
                        (%transform-ref other)))

(export 'transform-transform)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_invert ()
;;; ----------------------------------------------------------------------------

;; TODO: Check the return value. Will @code{nil} be returned?!

(cffi:defcfun ("gsk_transform_invert" %transform-invert)
    (g:boxed transform :return)
  (transform (g:boxed transform)))

(defun transform-invert (transform)
 #+liber-documentation
 "@version{2023-10-31}
  @argument[transform]{a @class{gsk:transform} instance to invert}
  @return{A @class{gsk:transform} instance with the inverted transform or
    @code{nil} if the transform cannot be inverted.}
  @begin{short}
    Inverts the given transform.
  @end{short}
  If @arg{transform} is not invertible, @code{nil} is returned. Note that
  inverting @code{nil} also returns @code{nil}, which is the correct inverse of
  @code{nil}. If you need to differentiate between those cases, you should check
  @arg{transform} is not @code{nil} before calling this function.
  @see-class{gsk:transform}"
  (%transform-invert (%transform-ref transform)))

(export 'transform-invert)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_matrix" %transform-matrix)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (matrix (:pointer (:struct graphene:matrix-t))))

(defun transform-matrix (transform matrix)
 #+liber-documentation
 "@version{#2022-9-17}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Multiplies @arg{transform} with the given matrix.
  @end{short}
  @see-class{gsk:transform}
  @see-symbol{graphene:matrix-t}"
  (%transform-matrix (%transform-ref transform) matrix))

(export 'transform-matrix)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_translate ()
;;;
;;; GskTransform *
;;; gsk_transform_translate (GskTransform *next,
;;;                          const graphene_point_t *point);
;;;
;;; Translates next in 2dimensional space by point .
;;;
;;; next :
;;;     the next transform.
;;;
;;; point :
;;;     the point to translate the transform by
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_translate" %transform-translate)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point-t))))

(defun transform-translate (transform point)
    (%transform-translate (%transform-ref transform) point))

(export 'transform-translate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_translate_3d ()
;;;
;;; GskTransform *
;;; gsk_transform_translate_3d (GskTransform *next,
;;;                             const graphene_point3d_t *point);
;;;
;;; Translates next by point .
;;;
;;; next :
;;;     the next transform.
;;;
;;; point :
;;;     the point to translate the transform by
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_translate_3d" %transform-translate-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point3d-t))))

(defun transform-translate-3d (transform point)
  (%transform-translate-3d (%transform-ref transform) point))

(export 'transform-translate-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_rotate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_rotate" %transform-rotate)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (angle :float))

(defun transform-rotate (transform angle)
 #+liber-documentation
 "@version{2023-10-30}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[angle]{a number coerced to a single float with the rotation angle,
    in degrees (clockwise)}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Rotates @arg{transform} angle degrees in 2D - or in 3D speak, around the z
    axis.
  @end{short}
  @see-class{gsk:transform}"
  (%transform-rotate (%transform-ref transform) (coerce angle 'single-float)))

(export 'transform-rotate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_rotate_3d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_rotate_3d" %transform-rotate-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (angle :float)
  (axis (:pointer (:struct graphene:vec3-t))))

(defun transform-rotate-3d (transform angle axis)
 #+liber-documentation
 "@version{2023-10-30}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[angle]{a number coerced to a single float with the rotation angle,
    in degrees (clockwise)}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with the rotation axis}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Rotates @arg{transform} angle degrees around @arg{axis}.
  @end{short}
  For a rotation in 2D space, use the @fun{gsk:transform-rotate} function.
  @see-class{gsk:transform}
  @see-function{gsk:transform-rotate}"
  (%transform-rotate-3d (%transform-ref transform)
                        (coerce angle 'single-float)
                        axis))

(export 'transform-rotate-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_scale" %transform-scale)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (xfactor :float)
  (yfactor :float))

(defun transform-scale (transform xfactor yfactor)
 #+liber-documentation
 "@version{2023-10-30}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[xfactor]{a number coerced to a single float with the scaling factor
    on the x axis}
  @argument[yfactor]{a number coerced to a single float with the scaling factor
    on the y axis}
  @return{A new @class{gsk:transform} instance.}
  @begin{short}
    Scales @arg{transform} in 2-dimensional space by the given factors.
  @end{short}
  Use the @fun{gsk:transform-scale-3d} function to scale in all 3 dimensions.
  @see-class{gsk:transform}"
  (%transform-scale (%transform-ref transform)
                    (coerce xfactor 'single-float)
                    (coerce yfactor 'single-float)))

(export 'transform-scale)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_scale_3d ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_scale_3d" %transform-scale-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (xfactor :float)
  (yfactor :float)
  (zfactor :float))

(defun transform-scale-3d (transform xfactor yfactor zfactor)
 #+liber-documentation
 "@version{2023-10-30}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[xfactor]{a number coerced to a single float with the scaling factor
    on the x axis}
  @argument[yfactor]{a number coerced to a single float with the scaling factor
    on the y axis}
  @argument[zfactor]{a number coerced to a single float with the scaling factor
    on the z axis}
  @return{A new @class{gsk:transform} instance.}
  @begin{short}
    Scales @arg{transform} by the given factors.
  @end{short}
  @see-class{gsk:transform}"
  (%transform-scale-3d (%transform-ref transform)
                       (coerce xfactor 'single-float)
                       (coerce yfactor 'single-float)
                       (coerce zfactor 'single-float)))

(export 'transform-scale-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_skew
;;;
;;; Applies a skew transform.
;;;
;;; Since 4.6
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_skew" %transform-skew) (g:boxed transform :return)
  (transform (g:boxed transform :return))
  (xskew :float)
  (yskew :float))

(defun transform-skew (transform xskew yskew)
  (%transform-skew (%transform-ref transform )
                   (coerce xskew 'single-float)
                   (coerce yskew 'single-float)))

(export 'transform-skew)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_perspective ()
;;;
;;; GskTransform *
;;; gsk_transform_perspective (GskTransform *next,
;;;                            float depth);
;;;
;;; Applies a perspective projection transform. This transform scales points in
;;; X and Y based on their Z value, scaling points with positive Z values away
;;; from the origin, and those with negative Z values towards the origin. Points
;;; on the z=0 plane are unchanged.
;;;
;;; next :
;;;     the next transform.
;;;
;;; depth :
;;;
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_perspective" %transform-perspective)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (depth :float))

(defun transform-perspective (transform depth)
 #+liber-documentation
 "@version{#2023-10-30}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[depth]{a number coerced to a single float with the distance of the
    z=0 plane, lower values give a more flattened pyramid and therefore a more
    pronounced perspective effect}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Applies a perspective projection transform.
  @end{short}
  This transform scales points in X and Y based on their Z value, scaling points
  with positive Z values away from the origin, and those with negative Z values
  towards the origin. Points on the z=0 plane are unchanged.
  @see-class{gsk:transform}"
  (%transform-perspective (%transform-ref transform)
                          (coerce depth 'single-float)))

(export 'transform-perspective)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_equal" transform-equal) :boolean
 #+liber-documentation
 "@version{#2023-10-30}
  @argument[first]{a first @class{gsk:transform} instance}
  @argument[second]{a second @class{gsk:transform} instance}
  @return{@em{True} if the two transforms perform the same operation.}
  @short{Checks two transforms for equality.}
  @see-class{gsk:transform}"
  (first (g:boxed transform))
  (second (g:boxed transform)))

(export 'transform-equal)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform_bounds ()
;;;
;;; void
;;; gsk_transform_transform_bounds (GskTransform *self,
;;;                                 const graphene_rect_t *rect,
;;;                                 graphene_rect_t *out_rect);
;;;
;;; Transforms a graphene_rect_t using the given transform self . The result is
;;; the bounding box containing the coplanar quad.
;;;
;;; self :
;;;     a GskTransform
;;;
;;; rect :
;;;     a graphene_rect_t
;;;
;;; out_rect :
;;;     return location for the bounds of the transformed rectangle.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform_bounds" %transform-transform-bounds)
    :void
  (transform (g:boxed transform))
  (rect (:pointer (:struct graphene:rect-t)))
  (bounds (:pointer (:struct graphene:rect-t))))

(defun transform-transform-bounds (transform rect out)
  (%transform-transform-bounds transform rect out)
  out)

(export 'transform-transform-bounds)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform_point ()
;;;
;;; void
;;; gsk_transform_transform_point (GskTransform *self,
;;;                                const graphene_point_t *point,
;;;                                graphene_point_t *out_point);
;;;
;;; Transforms a graphene_point_t using the given transform self .
;;;
;;; self :
;;;     a GskTransform
;;;
;;; point :
;;;     a graphene_point_t
;;;
;;; out_point :
;;;     return location for the transformed point.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform_point" %transform-transform-point) :void
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point-t)))
  (out (:pointer (:struct graphene:point-t))))

(defun transform-transform-point (transform point out)
  (%transform-transform-point transform point out)
  out)

(export 'transform-transform-point)

;;; --- End of file gsk.transform.lisp -----------------------------------------
