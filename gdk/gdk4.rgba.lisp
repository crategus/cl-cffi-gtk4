;;; ----------------------------------------------------------------------------
;;; gdk4.rgba.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; RGBA Colors
;;;
;;;     RGBA colors
;;;
;;; Types and Values
;;;
;;;     GdkRGBA
;;;
;;; Functions
;;;
;;;     gdk_rgba_copy
;;;     gdk_rgba_free
;;;     gdk_rgba_is_clear
;;;     gdk_rgba_is_opaque
;;;     gdk_rgba_parse
;;;     gdk_rgba_hash
;;;     gdk_rgba_equal
;;;     gdk_rgba_to_string
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkRGBA
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkRGBA
;;; ----------------------------------------------------------------------------

(eval-when (:execute :load-toplevel :compile-toplevel)
  (cffi:foreign-funcall "gdk_rgba_get_type" :size))

(define-g-boxed-cstruct rgba "GdkRGBA"
  (red :float :initform 0.0)
  (green :float :initform 0.0)
  (blue :float :initform 0.0)
  (alpha :float :initform 0.0))

#+liber-documentation
(setf (liber:alias-for-class 'rgba)
      "GBoxed"
      (documentation 'rgba 'type)
 "@version{2023-1-22}
  @begin{short}
    The @sym{gdk:rgba} structure is used to represent a (possibly translucent)
    color, in a way that is compatible with Cairo's notion of color.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct rgba \"GdkRGBA\"
  (red :float :initform 0.0)
  (green :float :initform 0.0)
  (blue :float :initform 0.0)
  (alpha :float :initform 0.0))
  @end{pre}
  @begin[code]{table}
    @entry[red]{The intensity of the red channel from 0.0 to 1.0 inclusive.}
    @entry[green]{The intensity of the green channel from 0.0 to 1.0 inclusive.}
    @entry[blue]{The intensity of the blue channel from 0.0 to 1.0 inclusive.}
    @entry[alpha]{The opacity of the color from 0.0 for completely translucent
      to 1.0 for opaque.}
  @end{table}
  @see-constructor{gdk:rgba-new}
  @see-constructor{gdk:rgba-copy}
  @see-slot{gdk:rgba-red}
  @see-slot{gdk:rgba-green}
  @see-slot{gdk:rgba-blue}
  @see-slot{gdk:rgba-alpha}")

(export (boxed-related-symbols 'rgba))

(unexport 'make-rgba)
(unexport 'copy-rgba)

;;; ----------------------------------------------------------------------------
;;; Accessors
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'rgba-red)
      "Accessor"
      (documentation 'rgba-red 'function)
 "@version{2023-1-22}
  @syntax[]{(gdk:rgba-red instance) => red}
  @syntax[]{(setf (gdk:rgba-red instance) red)}
  @argument[instance]{a @struct{gdk:rgba} color}
  @argument[red]{a number coerced to a float with the intensity of the red
  channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{red} slot of the @struct{gdk:rgba} color.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-blue}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-green)
      "Accessor"
      (documentation 'rgba-green 'function)
 "@version{2023-1-22}
  @syntax[]{(gdk:rgba-green instance) => green}
  @syntax[]{(setf (gdk:rgba-green instance) green)}
  @argument[instance]{a @struct{gdk:rgba} color}
  @argument[green]{a number coerced to a float with intensity of the green
    channel from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{green} slot of the @struct{gdk:rgba} color.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-blue}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-blue)
      "Accessor"
      (documentation 'rgba-blue 'function)
 "@version{2023-1-22}
  @syntax[]{(gdk:rgba-blue instance) => blue}
  @syntax[]{(setf (gdk:rgba-blue instance) blue)}
  @argument[instance]{a @struct{gdk:rgba} color}
  @argument[blue]{a number coerced to a float with intensity of the blue channel
    from 0.0 to 1.0}
  @begin{short}
    Accessor of the @code{blue} slot of the @struct{gdk:rgba} color.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-alpha}")

#+liber-documentation
(setf (liber:alias-for-function 'rgba-alpha)
      "Accessor"
      (documentation 'rgba-alpha 'function)
 "@version{2023-1-22}
  @syntax[]{(gdk:rgba-alpha instance) => alpha}
  @syntax[]{(setf (gdk:rgba-alpha instance) alpha)}
  @argument[instance]{a @struct{gdk:rgba} color}
  @argument[alpha]{a number coerced to a float with opacity of the color from
    0.0 for completely translucent to 1.0 for opaque}
  @begin{short}
    Accessor of the @code{alpha} slot of the @struct{gdk:rgba} color.
  @end{short}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-red}
  @see-function{gdk:rgba-green}
  @see-function{gdk:rgba-blue}")

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_new
;;; ----------------------------------------------------------------------------

(defun rgba-new (&key (red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
 "@version{2023-1-22}
  @argument[red]{a number with the intensity of the red channel from 0.0 to 1.0
    inclusive}
  @argument[green]{a number with the intensity of the green channel from 0.0 to
    1.0 inclusive}
  @argument[blue]{a number with intensity of the blue channel from 0.0 to 1.0
    inclusive}
  @argument[alpha]{a number with the opacity of the color from 0.0 for
    completely translucent to 1.0 for opaque}
  @begin{short}
    Creates a @struct{gdk:rgba} color.
  @end{short}
  @begin[Note]{dictionary}
    The numbers are coerced to float values.
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-copy}"
  (make-rgba :red (coerce red 'float)
             :green (coerce green 'float)
             :blue (coerce blue 'float)
             :alpha (coerce alpha 'float)))

(export 'rgba-new)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_copy
;;; ----------------------------------------------------------------------------

(defun rgba-copy (rgba)
 #+liber-documentation
 "@version{2023-1-22}
  @argument[rgba]{a @struct{gdk:rgba} color}
  @return{A newly allocated @struct{gdk:rgba} color, with the same contents
    as @arg{rgba}.}
  @short{Makes a copy of a @struct{gdk:rgba} color.}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-new}"
  (copy-rgba rgba))

(export 'rgba-copy)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_free ()                                       not needed
;;;
;;; void gdk_rgba_free (GdkRGBA *rgba);
;;;
;;; Frees a GdkRGBA struct created with gdk_rgba_copy()
;;;
;;; rgba :
;;;     a GdkRGBA
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_is_clear
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_is_clear" rgba-is-clear) :boolean
 #+liber-documentation
 "@version{2023-1-22}
  @argument[rgba]{a @class{gdk:rgba} color}
  @return{@em{True} if the RGBA color is clear.}
  @begin{short}
    Checks if a RGBA color is transparent.
  @end{short}
    That is, drawing with the color would not produce any change.
  @see-class{gdk:rgba}"
  (rgba (g:boxed rgba)))

(export 'rgba-is-clear)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_is_opaque
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_is_opaque" rgba-is-opaque) :boolean
 #+liber-documentation
 "@version{2023-1-22}
  @argument[rgba]{a @class{gdk:rgba} color}
  @return{@em{True} if the RGBA color is opaque.}
  @begin{short}
    Checks if a RGBA color is transparent.
  @end{short}
  That is, drawing with the color will not retain any results from previous
  contents.
  @see-class{gdk:rgba}"
  (rgba (g:boxed rgba)))

(export 'rgba-is-opaque)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_parse
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_parse" %rgba-parse) :boolean
  (rgba (g:boxed rgba :return))
  (str :string))

(defun rgba-parse (str)
 #+liber-documentation
 "@version{2023-1-22}
  @argument[str]{a string specifying the color}
  @return{A @struct{gdk:rgba} color with the filled in values.}
  @begin{short}
    Parses a textual representation of a color, and returns a RGBA instance
    filling in the @code{red}, @code{green}, @code{blue} and @code{alpha}
    fields.
  @end{short}
  The string can be either one of:
  @begin{itemize}
    @item{A standard name taken from the X11 @code{rgb.txt} file.}
    @item{A hex value in the form @code{rgb}, @code{rrggbb}, @code{rrrgggbbb}
      or @code{rrrrggggbbbb}.}
    @item{A RGB color in the form @code{rgb(r,g,b)}. In this case the color
      will have full opacity.}
    @item{A RGBA color in the form @code{rgba(r,g,b,a)}.}
  @end{itemize}
  Where @code{r}, @code{g}, @code{b} and @code{a} are respectively the red,
  green, blue and alpha color values. In the last two cases, @code{r}, @code{g}
  and @code{b} are either integers in the range 0 to 255 or precentage values
  in the range 0% to 100%, and @code{a} is a floating point value in the range
  0.0 to 1.0.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:rgba-parse \"LightGreen\")
=> #S(GDK-RGBA :RED 0.5647059
               :GREEN 0.93333334
               :BLUE 0.5647059
               :ALPHA 1.0)
(gdk:rgba-parse \"#90ee90\")
=> #S(GDK-RGBA :RED 0.5647059
               :GREEN 0.93333334
               :BLUE 0.5647059
               :ALPHA 1.0)
    @end{pre}
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-to-string}"
  (let ((rgba (make-rgba)))
    (when (%rgba-parse rgba str)
      rgba)))

(export 'rgba-parse)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_hash
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_hash" rgba-hash) :uint
 #+liber-documentation
 "@version{2023-1-22}
  @argument[color]{a @struct{gdk:rgba} color}
  @return{An unsigned integer with the hash value for @arg{color}.}
  @begin{short}
    A hash function suitable for using for a hash table that stores
    RGBA colors.
  @end{short}
  @see-struct{gdk:rgba}"
  (color (g:boxed rgba)))

(export 'rgba-hash)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_equal
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_equal" rgba-equal) :boolean
 #+liber-documentation
 "@version{2023-1-22}
  @argument[color1]{a @struct{gdk:rgba} color}
  @argument[color2]{another @struct{gdk:rgba} color}
  @return{@em{True} if the two colors compare equal.}
  @short{Compares two RGBA colors.}
  @see-struct{gdk:rgba}"
  (color1 (g:boxed rgba))
  (color2 (g:boxed rgba)))

(export 'rgba-equal)

;;; ----------------------------------------------------------------------------
;;; gdk_rgba_to_string
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_rgba_to_string" rgba-to-string) :string
 #+liber-documentation
 "@version{2023-1-22}
  @argument[color]{a @struct{gdk:rgba} color}
  @return{A string with the textual specification of @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the form @code{rgb(r,g,b)}
    or @code{rgba(r,g,b,a)}, where @code{r}, @code{g}, @code{b} and @code{a}
    represent the red, green, blue and alpha values respectively.
  @end{short}
  @code{r}, @code{g}, and @code{b} are represented as integers in the range 0
  to 255, and @code{a} is represented as a floating point value in the range
  0.0 to 1.0.

  These string forms are supported by the CSS3 colors module, and can be parsed
  by the @fun{gdk:rgba-parse} function.

  Note that this string representation may loose some precision, since @code{r},
  @code{g} and @code{b} are represented as 8-bit integers. If this is a concern,
  you should use a different representation.
  @begin[Example]{dictionary}
    @begin{pre}
(gdk:rgba-to-string (gdk:rgba-new :red 1.0))
=> \"rgba(255,0,0,0)\"
(gdk:rgba-parse *)
=> #S(GDK-RGBA :RED 1.0 :GREEN 0.0 :BLUE 0.0 :ALPHA 0.0)
    @end{pre}
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-function{gdk:rgba-parse}"
  (color (g:boxed rgba)))

(export 'rgba-to-string)

;;; --- End of file gdk4.rgba.lisp ---------------------------------------------
