;;; ----------------------------------------------------------------------------
;;; gsk.package.lisp
;;;
;;; The documentation of this file is taken from the GSK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
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

(defpackage :gsk
  (:use :iterate :common-lisp)
  (:import-from :cffi)
  (:import-from :glib)
  (:import-from :gobject))

(in-package :gsk)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :gsk) t)
 "GTK Scene Graph Kit (GSK) is the rendering and scene graph API for GTK
  introduced with version 3.90. GSK lies between the graphical control elements
  (widgets) and the rendering. Like GDK, GSK is part of GTK and licensed under
  the GNU Lesser General Public License (LGPL). This is the API documentation
  of a Lisp binding to GSK.
  @begin[GskRenderer]{section}
    @about-class{renderer}
    @about-generic{renderer-realized}
    @about-generic{renderer-surface}
    @about-function{renderer-new-for-surface}
    @about-function{renderer-realize}
    @about-function{renderer-unrealize}
    @about-function{renderer-is-realized}
    @about-function{renderer-render}
    @about-function{renderer-render-texture}
    @about-class{cairo-renderer}
    @about-function{cairo-renderer-new}
    @about-class{gl-renderer}
    @about-function{gl-renderer-new}
    @about-class{ngl-renderer}
    @about-function{ngl-renderer-new}
  @end{section}
  @begin[GskPathMeasure]{section}
    @about-class{path-measure}
    @about-function{path-measure-new}
    @about-function{path-measure-new-with-tolerance}
    @about-function{path-measure-length}
    @about-function{path-measure-path}
    @about-function{path-measure-point}
    @about-function{path-measure-tolerance}
    @about-function{path-measure-ref}
    @about-function{path-measure-unref}
  @end{section}
  @begin[GtkPathPoint]{section}
    @about-symbol{path-direction}
    @about-class{path-point}
    @about-function{path-point-curvature}
    @about-function{path-point-distance}
    @about-function{path-point-position}
    @about-function{path-point-rotation}
    @about-function{path-point-tangent}
    @about-function{path-point-compare}
    @about-function{path-point-copy}
    @about-function{path-point-equal}
    @about-function{path-point-free}
  @end{section}
  @begin[GskStroke]{section}
    @about-symbol{line-cap}
    @about-symbol{line-join}
    @about-class{stroke}
    @about-function{stroke-new}
    @about-function{stroke-copy}
    @about-function{stroke-free}
    @about-function{stroke-equal}
    @about-function{stroke-dash}
    @about-function{stroke-dash-offset}
    @about-function{stroke-line-cap}
    @about-function{stroke-line-join}
    @about-function{stroke-line-width}
    @about-function{stroke-miter-limit}
    @about-function{stroke-to-cairo}
  @end{section}
  @begin[GskPath]{section}
    @about-symbol{path-foreach-flags}
    @about-symbol{path-operation}
    @about-symbol{fill-rule}
    @about-class{path}
    @about-function{path-parse}
    @about-symbol{path-foreach-func}
    @about-function{path-foreach}
    @about-function{path-bounds}
    @about-function{path-closest-point}
    @about-function{path-start-point}
    @about-function{path-end-point}
    @about-function{path-stroke-bounds}
    @about-function{path-in-fill}
    @about-function{path-is-closed}
    @about-function{path-is-empty}
    @about-function{path-print}
    @about-function{path-ref}
    @about-function{path-unref}
    @about-function{path-to-cairo}
    @about-function{path-to-string}
  @end{section}
  @begin[GskPathBuilder]{section}
    @about-class{path-builder}
    @about-function{path-builder-new}
    @about-function{path-builder-to-path}
    @about-function{path-builder-free-to-path}
    @about-function{path-builder-current-point}
    @about-function{path-builder-close}
    @about-function{path-builder-add-rect}
    @about-function{path-builder-add-circle}
    @about-function{path-builder-add-layout}
    @about-function{path-builder-add-path}
    @about-function{path-builder-add-reverse-path}
    @about-function{path-builder-add-rounded-rect}
    @about-function{path-builder-add-segment}
    @about-function{path-builder-add-cairo-path}
    @about-function{path-builder-move-to}
    @about-function{path-builder-rel-move-to}
    @about-function{path-builder-line-to}
    @about-function{path-builder-rel-line-to}
    @about-function{path-builder-arc-to}
    @about-function{path-builder-rel-arc-to}
    @about-function{path-builder-conic-to}
    @about-function{path-builder-rel-conic-to}
    @about-function{path-builder-cubic-to}
    @about-function{path-builder-rel-cubic-to}
    @about-function{path-builder-quad-to}
    @about-function{path-builder-rel-quad-to}
    @about-function{path-builder-html-arc-to}
    @about-function{path-builder-rel-html-arc-to}
    @about-function{path-builder-svg-arc-to}
    @about-function{path-builder-rel-svg-arc-to}
    @about-function{path-builder-ref}
    @about-function{path-builder-unref}
  @end{section}
  @begin[GskRenderNode]{section}
    @about-symbol{render-node-type}
    @about-symbol{serialization-error}
    @about-symbol{parse-location}
    @about-symbol{scaling-filter}
    @about-symbol{color-stop}
    @about-symbol{shadow}
    @about-symbol{blend-mode}
    @about-symbol{mask-mode}
    @about-class{render-node}
    @about-function{render-node-ref}
    @about-function{render-node-unref}
    @about-function{render-node-node-type}
    @about-function{render-node-draw}
    @about-symbol{parse-error-func}
    @about-function{render-node-serialize}
    @about-function{render-node-deserialize}
    @about-function{render-node-write-to-file}
    @about-function{render-node-bounds}
    @about-class{container-node}
    @about-function{container-node-new}
    @about-function{container-node-n-children}
    @about-function{container-node-child}
    @about-class{cairo-node}
    @about-function{cairo-node-new}
    @about-function{cairo-node-draw-context}
    @about-function{cairo-node-surface}
    @about-class{color-node}
    @about-function{color-node-new}
    @about-function{color-node-color}
    @about-class{linear-gradient-node}
    @about-function{linear-gradient-node-new}
    @about-function{linear-gradient-node-start}
    @about-function{linear-gradient-node-end}
    @about-function{linear-gradient-node-n-color-stops}
    @about-function{linear-gradient-node-color-stops}
    @about-class{repeating-linear-gradient-node}
    @about-function{repeating-linear-gradient-node-new}
    @about-class{radial-gradient-node}
    @about-function{radial-gradient-node-new}
    @about-function{radial-gradient-node-n-color-stops}
    @about-function{radial-gradient-node-color-stops}
    @about-function{radial-gradient-node-start}
    @about-function{radial-gradient-node-end}
    @about-function{radial-gradient-node-hradius}
    @about-function{radial-gradient-node-vradius}
    @about-function{radial-gradient-node-center}
    @about-class{repeating-radial-gradient-node}
    @about-function{repeating-radial-gradient-node-new}
    @about-class{conic-gradient-node}
    @about-function{conic-gradient-node-new}
    @about-function{conic-gradient-node-n-color-stops}
    @about-function{conic-gradient-node-color-stops}
    @about-function{conic-gradient-node-center}
    @about-function{conic-gradient-node-rotation}
    @about-class{border-node}
    @about-function{border-node-new}
    @about-function{border-node-outline}
    @about-function{border-node-widths}
    @about-function{border-node-colors}
    @about-class{texture-node}
    @about-function{texture-node-new}
    @about-function{texture-node-texture}
    @about-class{inset-shadow-node}
    @about-function{inset-shadow-node-new}
    @about-function{inset-shadow-node-outline}
    @about-function{inset-shadow-node-color}
    @about-function{inset-shadow-node-dx}
    @about-function{inset-shadow-node-dy}
    @about-function{inset-shadow-node-spread}
    @about-function{inset-shadow-node-blur-radius}
    @about-class{outset-shadow-node}
    @about-function{outset-shadow-node-new}
    @about-function{outset-shadow-node-outline}
    @about-function{outset-shadow-node-color}
    @about-function{outset-shadow-node-dx}
    @about-function{outset-shadow-node-dy}
    @about-function{outset-shadow-node-spread}
    @about-function{outset-shadow-node-blur-radius}
    @about-class{transform-node}
    @about-function{transform-node-new}
    @about-function{transform-node-child}
    @about-function{transform-node-transform}
    @about-class{opacity-node}
    @about-function{opacity-node-new}
    @about-function{opacity-node-child}
    @about-function{opacity-node-opacity}
    @about-class{color-matrix-node}
    @about-function{color-matrix-node-new}
    @about-function{color-matrix-node-child}
    @about-function{color-matrix-node-color-matrix}
    @about-function{color-matrix-node-color-offset}
    @about-class{repeat-node}
    @about-function{repeat-node-new}
    @about-function{repeat-node-child}
    @about-function{repeat-node-child-bounds}
    @about-class{clip-node}
    @about-function{clip-node-new}
    @about-function{clip-node-child}
    @about-function{clip-node-clip}
    @about-class{rounded-clip-node}
    @about-function{rounded-clip-node-new}
    @about-function{rounded-clip-node-child}
    @about-function{rounded-clip-node-clip}
    @about-class{shadow-node}
    @about-function{shadow-node-new}
    @about-function{shadow-node-shadow}
    @about-function{shadow-node-n-shadows}
    @about-function{shadow-node-child}
    @about-class{blend-node}
    @about-function{blend-node-new}
    @about-function{blend-node-bottom-child}
    @about-function{blend-node-top-child}
    @about-function{blend-node-blend-mode}
    @about-class{cross-fade-node}
    @about-function{cross-fade-node-new}
    @about-function{cross-fade-node-start-child}
    @about-function{cross-fade-node-end-child}
    @about-function{cross-fade-node-progress}
    @about-class{text-node}
    @about-function{text-node-new}
    @about-function{text-node-font}
    @about-function{text-node-glyphs}
    @about-function{text-node-color}
    @about-function{text-node-has-color-glyphs}
    @about-function{text-node-num-glyphs}
    @about-function{text-node-offset}
    @about-class{blur-node}
    @about-function{blur-node-new}
    @about-function{blur-node-child}
    @about-function{blur-node-radius}
    @about-class{debug-node}
    @about-function{debug-node-new}
    @about-function{debug-node-child}
    @about-function{debug-node-message}
    @about-class{gl-shader-node}
    @about-function{gl-shader-node-new}
    @about-function{gl-shader-node-n-children}
    @about-function{gl-shader-node-child}
    @about-function{gl-shader-node-args}
    @about-function{gl-shader-node-shader}
  @end{section}
  @begin[GskRoundedRect]{section}
    @about-symbol{corner}
    @about-symbol{rounded-rect}
    @about-function{rounded-rect-bounds}
    @about-function{rounded-rect-corner}
    @about-function{rounded-rect-init}
    @about-function{rounded-rect-init-copy}
    @about-function{rounded-rect-init-from-rect}
    @about-function{rounded-rect-normalize}
    @about-function{rounded-rect-offset}
    @about-function{rounded-rect-shrink}
    @about-function{rounded-rect-is-rectilinear}
    @about-function{rounded-rect-contains-point}
    @about-function{rounded-rect-contains-rect}
    @about-function{rounded-rect-intersects-rect}
  @end{section}
  @begin[GskTransform]{section}
    @about-symbol{transform-category}
    @about-class{transform}
    @about-function{transform-new}
    @about-function{transform-ref}
    @about-function{transform-unref}
    @about-function{transform-category}
    @about-function{transform-print}
    @about-function{transform-parse}
    @about-function{transform-to-string}
    @about-function{transform-to-matrix}
    @about-function{transform-to-2d}
    @about-function{transform-to-2d-components}
    @about-function{transform-to-affine}
    @about-function{transform-to-translate}
    @about-function{transform-transform}
    @about-function{transform-invert}
    @about-function{transform-matrix}
    @about-function{transform-translate}
    @about-function{transform-translate-3d}
    @about-function{transform-rotate}
    @about-function{transform-rotate-3d}
    @about-function{transform-scale}
    @about-function{transform-scale-3d}
    @about-function{transform-skew}
    @about-function{transform-perspective}
    @about-function{transform-equal}
    @about-function{transform-transform-bounds}
    @about-function{transform-transform-point}
  @end{section}
  @begin[GskGLShader]{section}
    @about-symbol{gl-uniform-type}
    @about-symbol{shader-args-builder}
    @about-class{gl-shader}
    @about-generic{gl-shader-resource}
    @about-generic{gl-shader-source}
    @about-function{gl-shader-new-from-bytes}
    @about-function{gl-shader-new-from-resource}
    @about-function{gl-shader-compile}
    @about-function{gl-shader-source}
    @about-function{gl-shader-resource}
    @about-function{gl-shader-n-textures}
    @about-function{gl-shader-n-uniforms}
    @about-function{gl-shader-uniform-name}
    @about-function{gl-shader-find-uniform-by-name}
    @about-function{gl-shader-uniform-type}
    @about-function{gl-shader-uniform-offset}
    @about-function{gl-shader-args-size}
    @about-function{gl-shader-arg-float}
    @about-function{gl-shader-arg-int}
    @about-function{gl-shader-arg-uint}
    @about-function{gl-shader-arg-bool}
    @about-function{gl-shader-arg-vec2}
    @about-function{gl-shader-arg-vec3}
    @about-function{gl-shader-arg-vec4}
    @about-function{gl-shader-format-args-va}
    @about-function{gl-shader-format-args}
    @about-function{shader-args-builder-new}
    @about-function{shader-args-builder-to-args}
    @about-function{shader-args-builder-free-to-args}
    @about-function{shader-args-builder-unref}
    @about-function{shader-args-builder-ref}
    @about-function{shader-args-builder-set-float}
    @about-function{shader-args-builder-set-int}
    @about-function{shader-args-builder-set-uint}
    @about-function{shader-args-builder-set-bool}
    @about-function{shader-args-builder-set-vec2}
    @about-function{shader-args-builder-set-vec3}
    @about-function{shader-args-builder-set-vec4}
  @end{section}
 ")

;;; --- End of file gsk.package.lisp -------------------------------------------
