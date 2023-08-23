;;; ----------------------------------------------------------------------------
;;; gsk.transform.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
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
;;;     gsk_transform_ref
;;;     gsk_transform_unref
;;;     gsk_transform_get_category
;;;     gsk_transform_print
;;;     gsk_transform_to_string
;;;     gsk_transform_parse
;;;     gsk_transform_to_matrix
;;;     gsk_transform_to_2d
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
 "@version{#2022-9-13}
  @begin{short}
    The categories of matrices relevant for GSK and GTK.
  @end{short}

  Note that any category includes matrices of all later categories. So if you
  want to for example check if a matrix is a 2D matrix, `category >=
  GSK_TRANSFORM_CATEGORY_2D` is the way to do this.

  Also keep in mind that rounding errors may cause matrices to not conform to
  their categories. Otherwise, matrix operations done via multiplication will
  not worsen categories. So for the matrix multiplication `C = A * B`,
  `category(C) = MIN (category(A), category(B))`.
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
    @entry[:2d]{The matrix is a 2D matrix. This is equivalent to
      graphene_matrix_is_2d() returning %TRUE. In particular, this means that
      Cairo can deal with the matrix.}
    @entry[:2d-affine]{The matrix is a combination of 2D scale and 2D
      translation operations. In particular, this means that any rectangle can
      be transformed exactly using this matrix.}
    @entry[:2d-translate]{The matrix is a 2D translation.}
    @entry[:identity]{The matrix is the identity matrix.}
  @end{table}")

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
 "@version{#2022-2-7}
  @begin{short}
    The @sym{gsk:transform} object is an object to describe transform matrices.
  @end{short}
  Unlike the @symbol{graphene:matrix-t} structure, the @sym{transform} object
  retains the steps in how a transform was constructed, and allows inspecting
  them. It is modeled after the way CSS describes transforms.

  The @sym{transform} objects are immutable and cannot be changed after
  creation. This means code can safely expose them as properties of objects
  without having to worry about others changing them.
  @see-symbol{graphene:matrix-t}")

(export 'transform)

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

(cffi:defcfun ("gsk_transform_ref" transform-ref)
    (g:boxed transform)
  (transform (g:boxed transform)))

(export 'transform-ref)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_unref ()
;;;void
;;;gsk_transform_unref (GskTransform *self);
;;;Releases a reference on the given GskTransform.

;;;If the reference was the last, the resources associated to the self are freed.

;;;Parameters
;;;self

;;;a GskTransform.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_get_category ()
;;;
;;; GskTransformCategory
;;; gsk_transform_get_category (GskTransform *self);
;;;
;;; Returns the category this transform belongs to.
;;;
;;; self :
;;;     A GskTransform.
;;;
;;; Returns :
;;;     The category of the transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_get_category" transform-category) 
    transform-category
  (transform (g:boxed transform)))

(export 'transform-category)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_print ()
;;;void
;;;gsk_transform_print (GskTransform *self,
;;;                     GString *string);
;;;Converts self into a human-readable string representation suitable for printing that can later be parsed with gsk_transform_parse().

;;;Parameters
;;;self

;;;a GskTransform.

;;;[allow-none]
;;;string

;;;The string to print into
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_string ()
;;;
;;; char *
;;; gsk_transform_to_string (GskTransform *self);
;;;
;;; Converts a matrix into a string that is suitable for printing and can later
;;; be parsed with gsk_transform_parse().
;;;
;;; This is a wrapper around gsk_transform_print(), see that function for
;;; details.
;;;
;;; self :
;;;     a GskTransform.
;;;
;;; Returns :
;;;     A new string for self
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_string" transform-to-string) :string
  (transform (g:boxed transform)))

(export 'transform-to-string)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_parse ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_parse" %transform-parse) :boolean
  (str :string)
  (transform (g:boxed transform)))

(defun transform-parse (str)
 "@version{#2022-10-2}
  @argument[str]{a string to parse}
  @return{A @class{transform} object with the transform}
  @begin{short}
    Parses the given string into a transform.
  @end{short}
  Strings printed via the @fun{transform-to-string} function can be read in
  again successfully using this function.

  If the @arg{str} argument does not describe a valid transform, @em{false} is
  returned.
  @see-class{transform}
  @see-function{transform-to-string}"
  (let ((transform (%transform-new)))
    (when (%transform-parse str transform)
      ;; TODO: Check this. IS referencing needed?
      (transform-ref transform))))

(export 'transform-parse)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_to_matrix ()
;;;void
;;;gsk_transform_to_matrix (GskTransform *self,
;;;                         graphene_matrix_t *out_matrix);
;;;Computes the actual value of self and stores it in out_matrix . The previous value of out_matrix will be ignored.

;;;Parameters
;;;self

;;;a GskTransform.

;;;[allow-none]
;;;out_matrix

;;;The matrix to set.

;;;[out caller-allocates]
;;;gsk_transform_to_2d ()
;;;void
;;;gsk_transform_to_2d (GskTransform *self,
;;;                     float *out_xx,
;;;                     float *out_yx,
;;;                     float *out_xy,
;;;                     float *out_yy,
;;;                     float *out_dx,
;;;                     float *out_dy);
;;;Converts a GskTransform to a 2D transformation matrix. self must be a 2D transformation. If you are not sure, use gsk_transform_get_category() >= GSK_TRANSFORM_CATEGORY_2D to check.

;;;The returned values have the following layout:

;;;| xx yx |   |  a  b  0 |
;;;| xy yy | = |  c  d  0 |
;;;| dx dy |   | tx ty  1 |
;;;This function can be used to convert between a GskTransform and a matrix type from other 2D drawing libraries, in particular Cairo.

;;;Parameters
;;;self

;;;a 2D GskTransform

;;;
;;;out_xx

;;;return location for the xx member.

;;;[out]
;;;out_yx

;;;return location for the yx member.

;;;[out]
;;;out_xy

;;;return location for the xy member.

;;;[out]
;;;out_yy

;;;return location for the yy member.

;;;[out]
;;;out_dx

;;;return location for the x0 member.

;;;[out]
;;;out_dy

;;;return location for the y0 member.

;;;[out]
;;;gsk_transform_to_affine ()
;;;void
;;;gsk_transform_to_affine (GskTransform *self,
;;;                         float *out_scale_x,
;;;                         float *out_scale_y,
;;;                         float *out_dx,
;;;                         float *out_dy);
;;;Converts a GskTransform to 2D affine transformation factors. self must be a 2D transformation. If you are not sure, use gsk_transform_get_category() >= GSK_TRANSFORM_CATEGORY_2D_AFFINE to check.

;;;Parameters
;;;self

;;;a GskTransform

;;;
;;;out_scale_x

;;;return location for the scale factor in the x direction.

;;;[out]
;;;out_scale_y

;;;return location for the scale factor in the y direction.

;;;[out]
;;;out_dx

;;;return location for the translation in the x direction.

;;;[out]
;;;out_dy

;;;return location for the translation in the y direction.

;;;[out]
;;;gsk_transform_to_translate ()
;;;void
;;;gsk_transform_to_translate (GskTransform *self,
;;;                            float *out_dx,
;;;                            float *out_dy);
;;;Converts a GskTransform to a translation operation. self must be a 2D transformation. If you are not sure, use gsk_transform_get_category() >= GSK_TRANSFORM_CATEGORY_2D_TRANSLATE to check.

;;;Parameters
;;;self

;;;a GskTransform

;;;
;;;out_dx

;;;return location for the translation in the x direction.

;;;[out]
;;;out_dy

;;;return location for the translation in the y direction.

;;;[out]
;;;gsk_transform_transform ()
;;;GskTransform *
;;;gsk_transform_transform (GskTransform *next,
;;;                         GskTransform *other);
;;;Applies all the operations from other to next .

;;;Parameters
;;;next

;;;Transform to apply other to.

;;;[allow-none][transfer full]
;;;other

;;;Transform to apply.

;;;[allow-none]
;;;Returns
;;;The new transform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_invert ()
;;;
;;; GskTransform *
;;; gsk_transform_invert (GskTransform *self);
;;;
;;; Inverts the given transform.
;;;
;;; If self is not invertible, NULL is returned. Note that inverting NULL also
;;; returns NULL, which is the correct inverse of NULL. If you need to
;;; differentiate between those cases, you should check self is not NULL before
;;; calling this function.
;;;
;;; self :
;;;     Transform to invert.
;;;
;;; Returns :
;;;     The inverted transform or NULL if the transform cannot be inverted.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_invert" transform-invert)
    (g:boxed transform)
  (transform (g:boxed transform)))

(export 'transform-invert)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_matrix" transform-matrix) (g:boxed transform)
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
  (transform (g:boxed transform))
  (matrix (:pointer (:struct graphene:matrix-t))))

(export 'transform-matrix)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_translate ()
;;;GskTransform *
;;;gsk_transform_translate (GskTransform *next,
;;;                         const graphene_point_t *point);
;;;Translates next in 2dimensional space by point .

;;;Parameters
;;;next

;;;the next transform.

;;;[allow-none][transfer full]
;;;point

;;;the point to translate the transform by

;;;
;;;Returns
;;;The new transform

;;;gsk_transform_translate_3d ()
;;;GskTransform *
;;;gsk_transform_translate_3d (GskTransform *next,
;;;                            const graphene_point3d_t *point);
;;;Translates next by point .

;;;Parameters
;;;next

;;;the next transform.

;;;[allow-none][transfer full]
;;;point

;;;the point to translate the transform by

;;;
;;;Returns
;;;The new transform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_rotate ()
;;;
;;; GskTransform *
;;; gsk_transform_rotate (GskTransform *next,
;;;                       float angle);
;;;
;;; Rotates next angle degrees in 2D - or in 3Dspeak, around the z axis.
;;;
;;; next :
;;;     the next transform.
;;;
;;; angle :
;;;     the rotation angle, in degrees (clockwise)
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_rotate" transform-rotate)
    (g:boxed transform)
  (transform (g:boxed transform))
  (angle :float))

(export 'transform-rotate)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_rotate_3d ()
;;;GskTransform *
;;;gsk_transform_rotate_3d (GskTransform *next,
;;;                         float angle,
;;;                         const graphene_vec3_t *axis);
;;;Rotates next angle degrees around axis .

;;;For a rotation in 2D space, use gsk_transform_rotate().

;;;Parameters
;;;next

;;;the next transform.

;;;[allow-none][transfer full]
;;;angle

;;;the rotation angle, in degrees (clockwise)

;;;
;;;axis

;;;The rotation axis

;;;
;;;Returns
;;;The new transform
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_scale ()
;;;
;;; GskTransform *
;;; gsk_transform_scale (GskTransform *next,
;;;                      float factor_x,
;;;                      float factor_y);
;;;
;;; Scales next in 2-dimensional space by the given factors. Use
;;; gsk_transform_scale_3d() to scale in all 3 dimensions.
;;;
;;; next :
;;;     the next transform.
;;;
;;; factor_x :
;;;     scaling factor on the X axis
;;;
;;; factor_y :
;;;     scaling factor on the Y axis
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_scale" transform-scale)
    (g:boxed transform)
  (transform (g:boxed transform))
  (xfactor :float)
  (yfactor :float))

(export 'transform-scale)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_scale_3d ()
;;;GskTransform *
;;;gsk_transform_scale_3d (GskTransform *next,
;;;                        float factor_x,
;;;                        float factor_y,
;;;                        float factor_z);
;;;Scales next by the given factors.

;;;Parameters
;;;next

;;;the next transform.

;;;[allow-none][transfer full]
;;;factor_x

;;;scaling factor on the X axis

;;;
;;;factor_y

;;;scaling factor on the Y axis

;;;
;;;factor_z

;;;scaling factor on the Z axis

;;;
;;;Returns
;;;The new transform
;;; ----------------------------------------------------------------------------

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
;;;     distance of the z=0 plane. Lower values give a more flattened pyramid
;;;     and therefore a more pronounced perspective effect.
;;;
;;; Returns :
;;;     The new transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_perspective" transform-perspective)
    (g:boxed transform)
  (transform (g:boxed transform))
  (depth :float))

(export 'transform-perspective)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_equal ()
;;;
;;; gboolean
;;; gsk_transform_equal (GskTransform *first,
;;;                      GskTransform *second);
;;;
;;; Checks two transforms for equality.
;;;
;;; first :
;;;     the first transform.
;;;
;;; second :
;;;     the second transform.
;;;
;;; Returns :
;;;     TRUE if the two transforms perform the same operation.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_equal" transform-equal) :boolean
  (first (g:boxed transform))
  (second (g:boxed transform)))

(export 'transform-equal)

;;; ----------------------------------------------------------------------------
;;;gsk_transform_transform_bounds ()
;;;void
;;;gsk_transform_transform_bounds (GskTransform *self,
;;;                                const graphene_rect_t *rect,
;;;                                graphene_rect_t *out_rect);
;;;Transforms a graphene_rect_t using the given transform self . The result is the bounding box containing the coplanar quad.

;;;Parameters
;;;self

;;;a GskTransform

;;;
;;;rect

;;;a graphene_rect_t

;;;
;;;out_rect

;;;return location for the bounds of the transformed rectangle.

;;;[out caller-allocates]
;;;gsk_transform_transform_point ()
;;;void
;;;gsk_transform_transform_point (GskTransform *self,
;;;                               const graphene_point_t *point,
;;;                               graphene_point_t *out_point);
;;;Transforms a graphene_point_t using the given transform self .

;;;Parameters
;;;self

;;;a GskTransform

;;;
;;;point

;;;a graphene_point_t

;;;
;;;out_point

;;;return location for the transformed point.

;;; --- End of file gsk.transform.lisp -----------------------------------------
