;;; ----------------------------------------------------------------------------
;;; gsk4.transform.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
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
;;;     gsk_transform_ref                                   not exported
;;;     gsk_transform_unref                                 not exported
;;;     gsk_transform_get_category
;;;     gsk_transform_print                                 not implemented
;;;     gsk_transform_parse
;;;     gsk_transform_to_string
;;;     gsk_transform_to_matrix
;;;     gsk_transform_to_2d
;;;     gsk_transform_to_2d_components                      Since 4.6
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
;;;     gsk_transform_skew                                  Since 4.6
;;;     gsk_transform_perspective
;;;     gsk_transform_equal
;;;     gsk_transform_transform_bounds
;;;     gsk_transform_transform_point
;;; ----------------------------------------------------------------------------

(in-package :gsk)

;;; ----------------------------------------------------------------------------
;;; GskTransformCategory
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
 "@version{2024-4-21}
  @begin{declaration}
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
  @end{declaration}
  @begin{values}
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
  @end{values}
  @begin{short}
    The categories of matrices relevant for GSK and GTK.
  @end{short}
  Note that the enumeration values are ordered in a way that any category
  includes matrices of all later categories. So if you want to for example
  check if a matrix is a 2D matrix, the category value of the matrix is greater
  or  equal the @code{:2d} value.

  Also keep in mind that rounding errors may cause matrices to not conform to
  their categories. Otherwise, matrix operations done via multiplication will
  not worsen categories. So for the matrix multiplication @code{C = A * B} the
  result has the category @code{category(C) = (MIN (category(A), category(B))}.
  @begin{examples}
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
  @end{examples}
  @see-class{gsk:transform}")

;;; ----------------------------------------------------------------------------
;;; GskTransform
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "gsk_transform_get_type" :size))

;; gsk_transform_new is not documented but present in the GSK library
(cffi:defcfun ("gsk_transform_new" %transform-new) :pointer)

(glib:define-g-boxed-opaque transform "GskTransform"
  :export t
  :type-initializer "gsk_transform_get_type"
  :alloc (%transform-new))

#+liber-documentation
(setf (liber:alias-for-class 'transform)
      "GBoxed"
      (documentation 'transform 'type)
 "@version{2024-4-21}
  @begin{declaration}
    @begin{pre}
(glib:define-g-boxed-opaque transform ¸\"GskTransform\"
  :export t
  :type-initializer \"gsk_transform_get_type\"
  :alloc (%transform-new))
    @end{pre}
  @end{declaration}
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
  @see-constructor{gsk:transform-parse}
  @see-symbol{graphene:matrix-t}")

(export 'transform)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_new
;;; ----------------------------------------------------------------------------

(defun transform-new ()
 #+liber-documentation
 "@version{2024-4-21}
  @return{The new @class{gsk:transform} instance.}
  @short{Creates a new transform matrix.}
  @see-class{gsk:transform}"
  (make-instance 'transform))

(export 'transform-new)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_ref ()                                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_ref" %transform-ref) (g:boxed transform)
  (transform (g:boxed transform)))

;;; ----------------------------------------------------------------------------
;;; gsk_transform_unref ()                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_unref" %transform-unref) :void
  (transform (g:boxed transform)))

;;; ----------------------------------------------------------------------------
;;; gsk_transform_get_category
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_get_category" transform-category)
    transform-category
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{The @symbol{gsk:transform-category} value with the category of the
    transform matrix.}
  @short{Returns the category this transform matrix belongs to.}
  @see-class{gsk:transform}
  @see-symbol{gsk:transform-category}"
  (transform (g:boxed transform)))

(export 'transform-category)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_print ()                                  not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gsk_transform_parse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_parse" %transform-parse) :boolean
  (str :string)
  (transform :pointer))

(defun transform-parse (str)
 "@version{2024-4-21}
  @argument[str]{a string to parse}
  @return{The @class{gsk:transform} instance with the transform matrix.}
  @begin{short}
    Parses the given string into a transform matrix.
  @end{short}
  Strings printed via the @fun{gsk:transform-to-string} function can be read in
  again successfully using this function. If the @arg{str} argument does not
  describe a valid transform matrix, @em{false} is returned.
  @begin{examples}
    @begin{pre}
(gsk:transform-parse \"translate(1,2)\")
=> #<GSK:TRANSFORM {100A325D83@}>
(gsk:transform-to-translate *)
=> (1.0 2.0)
(gsk:transform-parse \"translate(1,2) scale(3)\")
=> #<GSK:TRANSFORM {100A327283@}>
(gsk:transform-to-2d *)
=> (3.0 0.0 0.0 3.0 1.0 2.0)
(gsk:transform-parse \"xxxx\") => NIL
    @end{pre}
  @end{examples}
  @see-class{gsk:transform}
  @see-function{gsk:transform-to-string}"
    (cffi:with-foreign-object (transform :pointer)
      (when (%transform-parse str transform)
        (cffi:convert-from-foreign (cffi:mem-ref transform :pointer)
                                   '(g:boxed transform :return)))))

(export 'transform-parse)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_string" transform-to-string) :string
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{The string for @arg{transform}.}
  @begin{short}
    Converts a tansform matrix into a string that is suitable for printing and
    can later be parsed with the @fun{gsk:transform-parse} function.
  @end{short}
  @begin{examples}
    @begin{pre}
(gsk:transform-to-string (gsk:transform-new)) => \"none\"
(graphene:with-point (p 1/2 2)
  (let ((transform (gsk:transform-translate (gsk:transform-new) p)))
    (gsk:transform-to-string transform)))
=> \"translate(0.5, 2)\"
(gsk:transform-to-string (gsk:transform-parse \"translate(1,2)\"))
=> \"translate(1, 2)\"
    @end{pre}
  @end{examples}
  @see-class{gsk:transform}
  @see-function{gsk:transform-parse}"
  (transform (g:boxed transform)))

(export 'transform-to-string)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_matrix" %transform-to-matrix) :void
  (transform (g:boxed transform))
  (matrix (:pointer (:struct graphene:matrix-t))))

(defun transform-to-matrix (transform matrix)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[matrix]{a @symbol{graphene:matrix-t} instance with the matrix
    to set}
  @return{The @symbol{graphene:matrix-t} instance with the matrix.}
  @begin{short}
    Computes the actual values of the transform matrix and stores it in
    @arg{matrix}.
  @end{short}
  The previous values of @arg{matrix} will be ignored.
  @see-class{gsk:transform}
  @see-symbol{graphene:matrix-t}"
  (%transform-to-matrix transform matrix)
  matrix)

(export 'transform-to-matrix)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_2d
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
 "@version{2024-3-21}
  @argument[transform]{a @class{gtk:transform} instance}
  @return{The @code{(xx xy yx yy dx dy)} list with the single float values of
    the 2D transformation matrix.}
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
  and a matrix from other 2D drawing libraries, in particular Cairo.
  @see-class{gsk:transform}
  @see-function{gsk:transform-category}"
  (cffi:with-foreign-objects ((xx :float) (yx :float)
                              (xy :float) (yy :float)
                              (dx :float) (dy :float))
    (%transform-to-2d transform xx yx xy yy dx dy)
    (list (cffi:mem-ref xx :float) (cffi:mem-ref xy :float)
          (cffi:mem-ref yx :float) (cffi:mem-ref yy :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-2d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_2d_components
;;; ----------------------------------------------------------------------------

#+gtk-4-6
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

#+gtk-4-6
(defun transform-to-2d-components (transform)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{The @code{(xskew yskew xscale yscale angle dx dy)} list with
    single float values for the 2D transformation factors}
  @begin{short}
    Converts a @class{gsk:transform} instance to 2D transformation factors.
  @end{short}
  The @arg{transform} argument must be a 2D transformation. If you are not
  sure, use the @fun{gsk:transform-category} function to check.

  Since 4.6
  @begin{examples}
    Recreate an equivalent transform matrix from the factors returned by this
    function.
    @begin{pre}
(graphene:with-point (p)
  (gsk:transform-skew
      (gsk:transform-scale
          (gsk:transform-rotate
              (gsk:transform-translate (gsk:transform-new)
                                       (graphene:point-init p dx dy))
              angle)
          xscale yscale)
      xskew yskew))
    @end{pre}
  @end{examples}
  @see-class{gsk:transform}
  @see-function{gsk:transform-category}"
  (cffi:with-foreign-objects ((xskew :float)  (yskew :float)
                              (xscale :float) (yscale :float)
                              (angle :float)  (dx :float) (dy :float))
    (%transform-to-2d-components transform
                                 xskew yskew xscale yscale angle dx dy)
    (list (cffi:mem-ref xskew :float)  (cffi:mem-ref yskew :float)
          (cffi:mem-ref xscale :float) (cffi:mem-ref yscale :float)
          (cffi:mem-ref angle :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

#+gtk-4-6
(export 'transform-to-2d-components)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_affine
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_affine" %transform-to-affine) :void
  (transform (g:boxed transform))
  (xscale (:pointer :float))
  (yscale (:pointer :float))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun transform-to-affine (transform)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{The @code{(xscale yscale dx dy)} list of single float values with the
    2D affine transformation factors}
  @begin{short}
    Converts a @class{gsk:transform} instance to 2D affine transformation
    factors.
  @end{short}
  The @arg{transform} argument must be a 2D transformation. If you are not
  sure, use the @fun{gsk:transform-category} function to check.
  @see-class{gsk:transform}
  @see-function{gsk:transform-category}"
  (cffi:with-foreign-objects ((xscale :float) (yscale :float)
                              (dx :float) (dy :float))
    (%transform-to-affine transform xscale yscale dx dy)
    (list (cffi:mem-ref xscale :float) (cffi:mem-ref yscale :float)
          (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-affine)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_to_translate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_to_translate" %transform-to-translate) :void
  (transform (g:boxed transform))
  (dx (:pointer :float))
  (dy (:pointer :float)))

(defun transform-to-translate (transform)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @return{The @code{(dx dy)} list with single float values for a translation
    operation.}
  @begin{short}
    Converts a @class{gsk:transform} instance to a translation operation.
  @end{short}
  The @arg{transform} arugment must be a 2D transformation. If you are not
  sure, use the @fun{gsk:transform-category} function to check.
  @see-class{gsk:transform}
  @see-function{gsk:transform-category}"
  (cffi:with-foreign-objects ((dx :float) (dy :float))
    (%transform-to-translate transform dx dy)
    (list (cffi:mem-ref dx :float) (cffi:mem-ref dy :float))))

(export 'transform-to-translate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform" %transform-transform)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (other (g:boxed transform)))

(defun transform-transform (transform other)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[other]{a @class{gsk:transform} instance}
  @return{The new @class{gsk:transform} instance.}
  @short{Applies all the operations from @arg{other} to @arg{transform}.}
  @see-class{gsk:transform}"
  (%transform-transform (%transform-ref transform)
                        (%transform-ref other)))

(export 'transform-transform)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_invert
;;; ----------------------------------------------------------------------------

;; TODO: Will @code{nil} be returned?! Check this.

(cffi:defcfun ("gsk_transform_invert" %transform-invert)
    (g:boxed transform :return)
  (transform (g:boxed transform)))

(defun transform-invert (transform)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance to invert}
  @return{The @class{gsk:transform} instance with the inverted transform or
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
;;; gsk_transform_matrix
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_matrix" %transform-matrix)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (matrix (:pointer (:struct graphene:matrix-t))))

(defun transform-matrix (transform matrix)
 #+liber-documentation
 "@version{2024-4-21}
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
;;; gsk_transform_translate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_translate" %transform-translate)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point-t))))

(defun transform-translate (transform point)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[point]{a @symbol{graphene:point-t} instance with the point to
    translate the transform matrix by}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Translates the transform matrix in 2dimensional space by @arg{point}.
  @end{short}
  @see-class{gsk:transform}
  @see-symbol{graphene:point-t}"
  (%transform-translate (%transform-ref transform) point))

(export 'transform-translate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_translate_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_translate_3d" %transform-translate-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point3d-t))))

(defun transform-translate-3d (transform point)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[point]{a @symbol{graphene:point3d-t} instance with the point to
    translate the transform matrix by}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Translates the transform matrix by @arg{point}.
  @end{short}
  @see-class{gsk:transform}
  @see-symbol{graphene:point3d-t}"
  (%transform-translate-3d (%transform-ref transform) point))

(export 'transform-translate-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_rotate
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_rotate" %transform-rotate)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (angle :float))

(defun transform-rotate (transform angle)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[angle]{a number coerced to a single float with the rotation angle,
    in degrees (clockwise)}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Rotates the transformation matrix @arg{angle} degrees in 2D, or in 3D speak,
    around the z axis.
  @end{short}
  @see-class{gsk:transform}"
  (%transform-rotate (%transform-ref transform) (coerce angle 'single-float)))

(export 'transform-rotate)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_rotate_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_rotate_3d" %transform-rotate-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (angle :float)
  (axis (:pointer (:struct graphene:vec3-t))))

(defun transform-rotate-3d (transform angle axis)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[angle]{a number coerced to a single float with the rotation angle,
    in degrees and clockwise}
  @argument[axis]{a @symbol{graphene:vec3-t} instance with the rotation axis}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Rotates the transform matrix @arg{angle} degrees around @arg{axis}.
  @end{short}
  For a rotation in 2D space, use the @fun{gsk:transform-rotate} function.
  @see-class{gsk:transform}
  @see-symbol{graphene:vec3-t}
  @see-function{gsk:transform-rotate}"
  (%transform-rotate-3d (%transform-ref transform)
                        (coerce angle 'single-float)
                        axis))

(export 'transform-rotate-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_scale
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_scale" %transform-scale)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (xfactor :float)
  (yfactor :float))

(defun transform-scale (transform xfactor yfactor)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[xfactor]{a number coerced to a single float with the scaling factor
    on the x axis}
  @argument[yfactor]{a number coerced to a single float with the scaling factor
    on the y axis}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Scales @arg{transform} in 2-dimensional space by the given factors.
  @end{short}
  Use the @fun{gsk:transform-scale-3d} function to scale in all 3 dimensions.
  @see-class{gsk:transform}
  @see-function{gsk:transform-scale-3d}"
  (%transform-scale (%transform-ref transform)
                    (coerce xfactor 'single-float)
                    (coerce yfactor 'single-float)))

(export 'transform-scale)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_scale_3d
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_scale_3d" %transform-scale-3d)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (xfactor :float)
  (yfactor :float)
  (zfactor :float))

(defun transform-scale-3d (transform xfactor yfactor zfactor)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[xfactor]{a number coerced to a single float with the scaling factor
    on the x axis}
  @argument[yfactor]{a number coerced to a single float with the scaling factor
    on the y axis}
  @argument[zfactor]{a number coerced to a single float with the scaling factor
    on the z axis}
  @return{A new @class{gsk:transform} instance.}
  @begin{short}
    Scales the transform matrix by the given factors.
  @end{short}
  @see-class{gsk:transform}"
  (%transform-scale-3d (%transform-ref transform)
                       (coerce xfactor 'single-float)
                       (coerce yfactor 'single-float)
                       (coerce zfactor 'single-float)))

(export 'transform-scale-3d)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_skew
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gsk_transform_skew" %transform-skew) (g:boxed transform :return)
  (transform (g:boxed transform))
  (xskew :float)
  (yskew :float))

#+gtk-4-6
(defun transform-skew (transform xskew yskew)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[xskew]{a number coerced to a single float with the skew factor
    on the x axis, in degrees}
  @argument[yskew]{a number coerced to a single float with the skew factor
    on the y axis, in degrees}
  @return{The new @class{gsk:transform} instance.}
  @short{Applies a skew transform.}

  Since 4.6
  @see-class{gsk:transform}"
  (%transform-skew (%transform-ref transform )
                   (coerce xskew 'single-float)
                   (coerce yskew 'single-float)))

#+gtk-4-6
(export 'transform-skew)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_perspective
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_perspective" %transform-perspective)
    (g:boxed transform :return)
  (transform (g:boxed transform))
  (depth :float))

(defun transform-perspective (transform depth)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[depth]{a number coerced to a single float with the distance of the
    z=0 plane, lower values give a more flattened pyramid and therefore a more
    pronounced perspective effect}
  @return{The new @class{gsk:transform} instance.}
  @begin{short}
    Applies a perspective projection transform.
  @end{short}
  This transform scales points in x and y based on their z value, scaling points
  with positive z values away from the origin, and those with negative z values
  towards the origin. Points on the z=0 plane are unchanged.
  @see-class{gsk:transform}"
  (%transform-perspective (%transform-ref transform)
                          (coerce depth 'single-float)))

(export 'transform-perspective)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_equal" transform-equal) :boolean
 #+liber-documentation
 "@version{2024-4-21}
  @argument[first]{a first @class{gsk:transform} instance}
  @argument[second]{a second @class{gsk:transform} instance}
  @return{@em{True} if the two transforms perform the same operation.}
  @short{Checks two transforms for equality.}
  @see-class{gsk:transform}"
  (first (g:boxed transform))
  (second (g:boxed transform)))

(export 'transform-equal)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform_bounds" %transform-transform-bounds)
    :void
  (transform (g:boxed transform))
  (rect (:pointer (:struct graphene:rect-t)))
  (bounds (:pointer (:struct graphene:rect-t))))

(defun transform-transform-bounds (transform rect bounds)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[rect]{a @symbol{graphene:rect-t} instance}
  @argument[bounds]{a @symbol{graphene:rect-t} instance for the bounds of
    the transformed rectangle}
  @return{The @symbol{graphene:rect-t} instance with the bounds.}
  @begin{short}
    Transforms a @symbol{graphene:rect-t} instance using the given transform
    matrix.
  @end{short}
  The result is the bounding box containing the coplanar quad.
  @see-class{gsk:transform}
  @see-symbol{graphene:rect-t}"
  (%transform-transform-bounds transform rect bounds)
  bounds)

(export 'transform-transform-bounds)

;;; ----------------------------------------------------------------------------
;;; gsk_transform_transform_point
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gsk_transform_transform_point" %transform-transform-point) :void
  (transform (g:boxed transform))
  (point (:pointer (:struct graphene:point-t)))
  (result (:pointer (:struct graphene:point-t))))

(defun transform-transform-point (transform point result)
 #+liber-documentation
 "@version{2024-4-21}
  @argument[transform]{a @class{gsk:transform} instance}
  @argument[point]{a @symbol{graphene:point-t} instance}
  @argument[result]{a @symbol{graphene:point-t} instance for the transformed
    point}
  @return{The @symbol{graphene:point-t} instance with the transformed point.}
  @begin{short}
    Transforms a @symbol{graphene:point-t} instance using the given transform
    matrix.
  @end{short}
  @see-class{gsk:transform}
  @see-symbol{graphene:point-t}"
  (%transform-transform-point transform point result)
  result)

(export 'transform-transform-point)

;;; --- End of file gsk4.transform.lisp ----------------------------------------
