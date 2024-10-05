;;; ----------------------------------------------------------------------------
;;; gdk4.paintable.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 -2024 Dieter Kaiser
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
;;; GdkPaintable
;;;
;;;     An interface for a paintable region
;;;
;;; Types and Values
;;;
;;;     GdkPaintable
;;;     GdkPaintableFlags
;;;
;;;     GdkSnapshot
;;;
;;; Functions
;;;
;;;     gdk_paintable_get_current_image
;;;     gdk_paintable_snapshot
;;;     gdk_paintable_get_flags
;;;     gdk_paintable_get_intrinsic_width
;;;     gdk_paintable_get_intrinsic_height
;;;     gdk_paintable_get_intrinsic_aspect_ratio
;;;     gdk_paintable_compute_concrete_size
;;;     gdk_paintable_invalidate_contents
;;;     gdk_paintable_invalidate_size
;;;     gdk_paintable_new_empty
;;;
;;; Signals
;;;
;;;     invalidate-contents
;;;     invalidate-size
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkPaintable
;;;
;;;     GObject
;;;     ╰── GdkSnapshot
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkPaintableFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkPaintableFlags" paintable-flags
  (:export t
   :type-initializer "gdk_paintable_flags_get_type")
  (:static-size #.(ash 1 0))
  (:static-contents #.(ash 1 1)))

#+liber-documentation
(setf (liber:alias-for-symbol 'paintable-flags)
      "GFlags"
      (liber:symbol-documentation 'paintable-flags)
 "@version{2024-5-5}
  @begin{declaration}
(gobject:define-g-flags \"GdkPaintableFlags\" paintable-flags
  (:export t
   :type-initializer \"gdk_paintable_flags_get_type\")
  (:static-size #.(ash 1 0))
  (:static-contents #.(ash 1 1)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:static-size]{The size is immutable. The @code{\"invalidate-size\"}
      signal will never be emitted.}
      @entry[:static-contents]{The content is immutable. The
        @code{\"invalidate-contents\"} signal will never be emitted.}
    @end{table}
  @end{values}
  @begin{short}
    Flags about a @class{gdk:paintable} object.
  @end{short}
  Implementations use these for optimizations such as caching.
  @see-class{gdk:paintable}")

;;; ----------------------------------------------------------------------------
;;; GdkSnapshot
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkSnapshot" snapshot
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_snapshot_get_type")
  nil)

#+liber-documentation
(setf (documentation 'snapshot 'type)
 "@version{2023-7-30}
  @begin{short}
    Base type for snapshot operations.
  @end{short}
  The subclass of the @class{gdk:snapshot} class used by GTK is the
  @class{gtk:snapshot} class.
  @see-class{gtk:snapshot}")

;;; ----------------------------------------------------------------------------
;;; GdkPaintable
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GdkPaintable" paintable
  (:export t
   :type-initializer "gdk_paintable_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'paintable)
      "Interface"
      (documentation 'paintable 'type)
 "@version{2024-5-5}
  @begin{short}
    The @class{gdk:paintable} interface is a simple interface used by GDK and
    GTK to represent objects that can be painted anywhere at any size without
    requiring any sort of layout.
  @end{short}
  The interface is inspired by similar concepts elsewhere, such as
  @code{ClutterContent}, HTML/CSS Paint Sources, or SVG Paint Servers.

  A @class{gdk:paintable} object can be snapshot at any time and size using the
  @fun{gdk:paintable-snapshot} function. How the paintable interprets that size
  and if it scales or centers itself into the given rectangle is implementation
  defined, though if you are implementing a @class{gdk:paintable} object and
  do not know what to do, it is suggested that you scale your paintable
  ignoring any potential aspect ratio.

  The contents that a @class{gdk:paintable} object produces may depend on the
  @class{gdk:snapshot} object passed to it. For example, paintables may decide
  to use more detailed images on higher resolution screens or when OpenGL is
  available. A @class{gdk:paintable} object will however always produce the
  same output for the same snapshot.

  A @class{gdk:paintable} object may change its contents, meaning that it will
  now produce a different output with the same snapshot. Once that happens, it
  will call the @fun{gdk:paintable-invalidate-contents} function which will
  emit the @code{\"invalidate-contents\"} signal. If a paintable is known to
  never change its contents, it will set the @code{:static-contents} flag. If a
  consumer cannot deal with changing contents, it may call the
  @fun{gdk:paintable-current-image} function which will return a static
  paintable and use that.

  A paintable can report an intrinsic (or preferred) size or aspect ratio it
  wishes to be rendered at, though it does not have to. Consumers of the
  interface can use this information to layout thepaintable appropriately. Just
  like the contents, the size of a paintable can change. A paintable will
  indicate this by calling the @fun{gdk:paintable-invalidate-size} function
  which will emit the @code{\"invalidate-size\"} signal. And just like for
  contents, if a paintable is known to never change its size, it will set the
  @code{:static-size} flag.

  Besides API for applications, there are some functions that are only useful
  for implementing subclasses and should not be used by applications:
  @fun{gdk:paintable-invalidate-contents}, @fun{gdk:paintable-invalidate-size},
  @fun{gdk:paintable-new-empty}.
  @begin[Interface structure]{dictionary}
    @begin{pre}
(gobject:define-vtable (\"GdkPaintable\" paintable)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GdkPaintable interface
  (snapshot (:void (paintable (g:object paintable))
                   (snapshot (g:object snapshot))
                   (width :double)
                   (height :double)))
  (get-current-image ((g:object paintable) (paintable (g:object paintable))))
  (get-flags (paintable-flags (paintable (g:object paintable))))
  (get-intrinsic-width (:int (paintable (g:object paintable))))
  (get-intrinsic-height (:int (paintable (g:object paintable))))
  (get-intrinsic-aspect-ratio (:double (paintable (g:object paintable)))))
    @end{pre}
    The list of functions that can be implemented for the @class{gdk:paintable}
    interface. The following methods are provided and can be specialized
    for a subclass which derives from the @class{gdk:paintable} interface:
    @begin[code]{table}
      @entry[gdk:paintable-snapshot-impl]{Method called from the
        @fun{gdk:paintable-snapshot} function.}
      @entry[gdk:paintable-get-current-image-impl]{Method called from the
        @fun{gdk:paintable-current-image} function.}
      @entry[gdk:paintable-get-flags-impl]{Method called from the
        @fun{gdk:paintable-flags} function.}
      @entry[gdk:paintable-get-intrinsic-width-impl]{Method called from the
        @fun{gdk:paintable-intrinsic-with} function.}
      @entry[gdk:paintable-get-intrinsic-height-impl]{Method called from the
        @fun{gdk:paintable-intrinsic-height} function.}
      @entry[gdk:paintable-get-intrinsic-aspect-ratio-impl]{Method called from
        the @fun{gdk:paintable-intrinsic-aspect-ratio} function.}
    @end{table}
    Note that apart from the @fun{gdk:paintable-snapshot-impl} method, no
    method of this interface is mandatory to implement, though it is a good
    idea to implement the @fun{gdk:paintable-get-current-image-impl} method for
    non-static paintables and the @fun{gdk:paintable-get-flags-impl} method if
    the paintable is not dynamic as the default implementation returns no flags
    and that will make the implementation likely quite slow.
  @end{dictionary}
  @begin{examples}
    This is a complete example for the implementation of a subclass which
    implements the @class{gdk:paintable} interface.
    @begin{pre}
;; Implementation of a NUCLEAR-ICON subclass
(gobject:define-gobject-subclass \"GdkNuclearIcon\" nuclear-icon
  (:superclass g:object
   :export t
   :interfaces (\"GdkPaintable\"))
  ((rotation
    nuclear-icon-rotation
    \"rotation\" \"gdouble\" t t)))

;; This is the function that draws the actual icon
(defun nuclear-snapshot (snapshot foreground background width height rotation)
  (let ((size (min width height))
        (radius 0.3)
        (cr nil))
    (graphene:with-rects ((rect1 0 0 width height)
                          (rect2 (/ (- width size) 2.0)
                                 (/ (- height size) 2.0)
                                 size size))
      (gtk:snapshot-append-color snapshot background rect1)
      (setf cr (gtk:snapshot-append-cairo snapshot rect2))
      (cairo-set-source-rgba cr foreground)
      (cairo:translate cr (/ width 2.0) (/ height 2.0))
      (cairo:scale cr size size)
      (cairo:rotate cr rotation)
      (cairo:arc cr 0 0 0.1 (- pi) pi)
      (cairo:fill cr)
      (setf (cairo:line-width cr) radius)
      (setf (cairo:dash cr 0.0) (list (/ (* radius pi) 3)))
      (cairo:arc cr 0 0 radius (- pi) pi)
      (cairo:stroke cr)
      (cairo:destroy cr))))

;; Here, we implement the methods required by the GdkPaintable interface
(defmethod paintable-snapshot-impl ((paintable nuclear-icon)
                                    snapshot width height)
  (nuclear-snapshot snapshot
                    (rgba-new :red 0 :green 0 :blue 0 :alpha 1)
                    (rgba-new :red 0.9 :green 0.75 :blue 0.15 :alpha 1)
                    width
                    height
                    (nuclear-icon-rotation paintable)))

(defmethod paintable-get-flags-impl ((paintable nuclear-icon))
  (list :static-contents :static-size))
    @end{pre}
  @end{examples}
  @begin[Signal Details]{dictionary}
    @subheading{The \"invalidate-contents\" signal}
      @begin{pre}
lambda (paintable)    :run-last
      @end{pre}
      Emitted when the contents of the paintable change. Examples for such an
      event would be videos changing to the next frame or the icon theme for an
      icon changing.
      @begin[code]{table}
        @entry[paintable]{The @class{gdk:paintable} object.}
      @end{table}
    @subheading{The \"invalidate-size\" signal}
      @begin{pre}
lambda (paintable)    :run-last
      @end{pre}
      Emitted when the intrinsic size of the paintable changes. This means the
      values reported by at least one of the
      @fun{gdk:paintable-intrinsic-width}, @fun{gdk:paintable-intrinsic-height}
      or @fun{gdk:paintable-intrinsic-aspect-ratio} function has changed.
      Examples for such an event would be a paintable displaying the contents
      of a toplevel surface being resized.
      @begin[code]{table}
        @entry[paintable]{The @class{gdk:paintable} object.}
      @end{table}
  @end{dictionary}
  @see-class{gdk:texture}
  @see-class{gtk:image}
  @see-class{gtk:snapshot}")

;;; ----------------------------------------------------------------------------
;;; GdkPaintable Inferface vtable
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GdkPaintable" paintable)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GdkPaintable interface
  (snapshot (:void (paintable (g:object paintable))
                   (snapshot (g:object snapshot))
                   (width :double)
                   (height :double)))
  (get-current-image ((g:object paintable) (paintable (g:object paintable))))
  (get-flags (paintable-flags (paintable (g:object paintable))))
  (get-intrinsic-width (:int (paintable (g:object paintable))))
  (get-intrinsic-height (:int (paintable (g:object paintable))))
  (get-intrinsic-aspect-ratio (:double (paintable (g:object paintable)))))

;;; ----------------------------------------------------------------------------

;; Define default methods for the virtual functions of the interface.
;; The default functions of the C library are not available, therefor we
;; define the equivalent functionality in Lisp.

;;; --- gdk:paintable-snapshot-impl --------------------------------------------

(defmethod paintable-snapshot-impl ((paintable paintable) snapshot width height)
  (error "Paintable of type ~a  does not implement GDK:PAINTABLE-SNAPSHOT-IMPL"
         (g:type-name (g:type-from-instance paintable))))

#+liber-documentation
(setf (liber:alias-for-function 'paintable-snapshot-impl)
      "Method"
      (documentation 'paintable-snapshot-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-snapshot} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-snapshot-impl ((paintable paintable) snapshot width height)
  (error \"Paintable of type ~a  does not implement GDK:PAINTABLE-SNAPSHOT-IMPL\"
         (g:type-name (g:type-from-instance paintable))))
    @end{pre}
    This method must be implemented for a subclass which implements the
    @class{gdk:paintable} interface. The default implementation signals an
    error. See the @fun{gdk:paintable-snapshot} function for the syntax. The
    @class{gdk:paintable} documentation shows a complete example of implementing
    the interface.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-snapshot}")

(export 'paintable-snapshot-impl)

;;; --- gdk:paintable-get-current-image-impl -----------------------------------

(defmethod paintable-get-current-image-impl ((paintable paintable))
  (if (not (set-difference '(:static-size :static-contents)
                           (paintable-get-flags-impl paintable)))
      ;; Paintable is immutable, return it
      paintable
      ;; Create a new paintable object
      (let ((width (paintable-get-intrinsic-width-impl paintable))
            (height (paintable-get-intrinsic-height-impl paintable)))
        (if (or (<= width 0) (<= height 0))
          (paintable-new-empty width height)
          (let ((snapshot (g:object-new "GtkSnapshot")))
            (paintable-snapshot paintable snapshot width height)
            ;; The GTK package is not availabe at this point. We call the
            ;; C function directly.
            (cffi:foreign-funcall "gtk_snapshot_free_to_paintable"
                                  (g:object snapshot) snapshot
                                  :pointer (cffi:null-pointer)))))))

#+liber-documentation
(setf (liber:alias-for-function 'paintable-get-current-image-impl)
      "Method"
      (documentation 'paintable-get-current-image-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-current-image} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-get-current-image-impl ((paintable paintable))
  (if (not (set-difference '(:static-size :static-contents)
                           (paintable-get-flags-impl paintable)))
      ;; Paintable is immutable, return it
      paintable
      ;; Create a new paintable object
      (let ((width (paintable-get-intrinsic-width-impl paintable))
            (height (paintable-get-intrinsic-height-impl paintable)))
        (if (or (<= width 0) (<= height 0))
          (paintable-new-empty width height)
          (let ((snapshot (g:object-new \"GtkSnapshot\")))
            (paintable-snapshot paintable snapshot width height)
            ;; The GTK package is not availabe at this point. We call the
            ;; C function directly.
            (cffi:foreign-funcall \"gtk_snapshot_free_to_paintable\"
                                  (g:object snapshot) snapshot
                                  :pointer (cffi:null-pointer)))))))
    @end{pre}
    The implementation of this method is not mandatory. The default
    implementation returns the paintable itself, when it is a static paintable.
    Otherwise, the default method returns an empty paintable or creates a
    paintable for the intrinsic width and height.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-current-image}")

(export 'paintable-get-current-image-impl)

;;; --- gdk:paintable-get-flags-impl -------------------------------------------

(defmethod paintable-get-flags-impl ((paintable paintable))
  '())

#+liber-documentation
(setf (liber:alias-for-function 'paintable-get-flags-impl)
      "Method"
      (documentation 'paintable-get-flags-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-flags} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-get-flags-impl ((paintable paintable))
  '())
    @end{pre}
  The default method returns no flags set.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-flags}")

(export 'paintable-get-flags-impl)

;;; --- gdk:paintable-get-intrinsic-width-impl ---------------------------------

(defmethod paintable-get-intrinsic-width-impl ((paintable paintable))
  0)

#+liber-documentation
(setf (liber:alias-for-function 'paintable-get-intrinsic-width-impl)
      "Method"
      (documentation 'paintable-get-intrinsic-width-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-intrinsic-width} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-get-intrinsic-width-impl ((paintable paintable))
  0)
    @end{pre}
  The default method returns 0 for the intrinsic width.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-intrinsic-width}")

(export 'paintable-get-intrinsic-width-impl)

;;; --- gdk:paintable-get-intrinsic-height-impl --------------------------------

(defmethod paintable-get-intrinsic-height-impl ((paintable paintable))
  0)

#+liber-documentation
(setf (liber:alias-for-function 'paintable-get-intrinsic-height-impl)
      "Method"
      (documentation 'paintable-get-intrinsic-height-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-intrinsic-height} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-get-intrinsic-height-impl ((paintable paintable))
  0)
    @end{pre}
  The default method returns 0 for the intrinsic height.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-intrinsic-height}")

(export 'paintable-get-intrinsic-height-impl)

;;; --- gdk:paintable-get-intrinsic-aspect-ration-impl -------------------------

(defmethod paintable-get-intrinsic-aspect-ratio-impl ((paintable paintable))
  (let ((width (paintable-get-intrinsic-width-impl paintable))
        (height (paintable-get-intrinsic-height-impl paintable)))
    (if (or (<= width 0) (<= height 0))
        0.0d0
        (coerce (/ width height) 'double-float))))

#+liber-documentation
(setf (liber:alias-for-function 'paintable-get-intrinsic-aspect-ratio-impl)
      "Method"
      (documentation 'paintable-get-intrinsic-aspect-ratio-impl 'function)
 "@version{2023-12-2}
  @begin{short}
    Method called from the @fun{gdk:paintable-intrinsic-aspect-ratio} function.
  @end{short}
  @begin[Default method]{dictionary}
    @begin{pre}
(defmethod paintable-get-intrinsic-aspect-ratio-impl ((paintable paintable))
  (let ((width (paintable-get-intrinsic-width-impl paintable))
        (height (paintable-get-intrinsic-height-impl paintable)))
    (if (or (<= width 0) (<= height 0))
        0.0d0
        (coerce (/ width height) 'double-float))))
    @end{pre}
  The default method returns 0.0d0 or the width to height ratio as a double
  float.
  @end{dictionary}
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-intrinsic-aspect-ratio}")

(export 'paintable-get-intrinsic-aspect-ratio-impl)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_get_current_image
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_get_current_image" paintable-current-image)
    (g:object paintable)
 #+liber-documentation
 "@version{#2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The immutable @class{gdk:paintable} object for the current contents
    of @arg{paintable}.}
  @begin{short}
    Gets an immutable paintable for the current contents displayed by
    @arg{paintable}.
  @end{short}
  This is useful when you want to retain the current state of an animation, for
  example to take a screenshot of a running animation. If the paintable is
  already immutable, it will return itself.
  @see-class{gdk:paintable}"
  (paintable (g:object paintable)))

(export 'paintable-current-image)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_snapshot
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_snapshot" paintable-snapshot) :void
 #+liber-documentation
 "@version{#2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @argument[snapshot]{a @class{gdk:snapshot} object}
  @argument[width]{a double float with the width to snapshot in}
  @argument[height]{a double float with the height to snapshot in}
  @return{The @class{gdk:snapshot} object to snapshot to.}
  @begin{short}
    Snapshots the given @arg{paintable} with the given @arg{width} and
    @arg{height} at the current @code{(0,0)} offset of the snapshot.
  @end{short}
  If @arg{width} and @arg{height} are not larger than zero, this function will
  do nothing.
  @see-class{gdk:paintable}
  @see-class{gdk:snapshot}"
  (paintable (g:object paintable))
  (snapshot (g:object snapshot))
  (width :double)
  (height :double))

(export 'paintable-snapshot)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_get_flags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_get_flags" paintable-flags) paintable-flags
 #+liber-documentation
 "@version{2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The @symbol{gdk:paintable-flags} value for this paintable.}
  @begin{short}
    Get flags for the paintable.
  @end{short}
  This is oftentimes useful for optimizations. See the
  @symbol{gdk:paintable-flags} documentation for the flags and what they mean.
  @see-class{gdk:paintable}
  @see-symbol{gdk:paintable-flags}"
  (paintable (g:object paintable)))

(export 'paintable-flags)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_get_intrinsic_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_get_intrinsic_width" paintable-intrinsic-width)
    :int
 #+liber-documentation
 "@version{2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The integer with the the intrinsic width of @arg{paintable} or 0 if
    none.}
  @begin{short}
    Gets the preferred width the paintable would like to be displayed at.
  @end{short}
  Consumers of this interface can use this to reserve enough space to draw the
  paintable.

  This is a purely informational value and does not in any way limit the values
  that may be passed to the @fun{gdk:paintable-snapshot} function.

  If the paintable does not have a preferred width, it returns 0. Negative
  values are never returned.
  @see-class{gdk:paintable}"
  (paintable (g:object paintable)))

(export 'paintable-intrinsic-width)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_get_intrinsic_height
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_get_intrinsic_height" paintable-intrinsic-height)
    :int
 #+liber-documentation
 "@version{2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The integer with the the intrinsic height of @arg{paintable} or 0 if
    none.}
  @begin{short}
    Gets the preferred height the paintable would like to be displayed at.
  @end{short}
  Consumers of this interface can use this to reserve enough space to draw the
  paintable.

  This is a purely informational value and does not in any way limit the values
  that may be passed to the @fun{gdk:paintable-snapshot} function.

  If the paintable does not have a preferred height, it returns 0. Negative
  values are never returned.
  @see-class{gdk:paintable}"
  (paintable (g:object paintable)))

(export 'paintable-intrinsic-height)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_get_intrinsic_aspect_ratio
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_get_intrinsic_aspect_ratio"
               paintable-intrinsic-aspect-ratio) :double
 #+liber-documentation
 "@version{2024-5-5}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The double float with the intrinsic aspect ratio of @arg{paintable}
    or 0 if none.}
  @begin{short}
    Gets the preferred aspect ratio the paintable would like to be displayed at.
  @end{short}
  The aspect ratio is the width divided by the height, so a value of 0.5 means
  that the paintable prefers to be displayed twice as high as it is wide.
  Consumers of this interface can use this to preserve aspect ratio when
  displaying the paintable.

  This is a purely informational value and does not in any way limit the values
  that may be passed to the @fun{gdk:paintable-snapshot} function.

  Usually when a paintable returns nonzero values from the
  @fun{gdk:paintable-intrinsic-width} and @fun{gdk:paintable-intrinsic-height}
  functions the aspect ratio should conform to those values, though that is not
  required.

  If the paintable does not have a preferred aspect ratio, it returns 0.
  Negative values are never returned.
  @see-class{gdk:paintable}
  @see-function{gdk:paintable-snapshot}"
  (paintable (g:object paintable)))

(export 'paintable-intrinsic-aspect-ratio)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_compute_concrete_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_compute_concrete_size"
               %paintable-compute-concrete-size) :void
  (paintable (g:object paintable))
  (swidth :double)
  (sheight :double)
  (dwidth :double)
  (dheight :double)
  (cwidth (:pointer :double))
  (cheight (:pointer :double)))

(defun paintable-compute-concrete-size (paintable swidth sheight dwidth dheight)
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[paintable]{a @class{gdk:paintable} object}
  @argument[swidth]{a double float with the width @arg{paintable} could be
    drawn into or 0.0 if unknown}
  @argument[sheight]{a double float with the height @arg{paintable} could be
    drawn into or 0.0 if unknown}
  @argument[dwidth]{a double float with the width @arg{paintable} would be
    drawn into if no other constraints were given}
  @argument[dheight]{a double float with the height @arg{paintable} would be
    drawn into if no other constraints were given}
  @begin{return}
    @arg{cwidth} - a double float with the concrete width computed @br{}
    @arg{cheight} - a double float with the concrete height computed
  @end{return}
  @begin{short}
    Applies the sizing algorithm outlined in
    https://drafts.csswg.org/css-images-3/default-sizing to the given
    @arg{paintable}.
  @end{short}
  See that link for more details.

  It is not necessary to call this function when both @arg{swidth} and
  @arg{sheight} are known, but it is useful to call this function in
  @code{GtkWidget:measure} implementations to compute the other dimension when
  only one dimension is given.
  @see-class{gdk:paintable}"
  (cffi:with-foreign-objects ((cwidth :double) (cheight :double))
    (%paintable-compute-concrete-size paintable
                                      swidth sheight
                                      dwidth dheight
                                      cwidth cheight)
    (values (cffi:mem-ref cwidth :double)
            (cffi:mem-ref cheight :double))))

(export 'paintable-compute-concrete-size)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_invalidate_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_invalidate_contents"
               paintable-invalidate-contents) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Called by implementations of a @class{gdk:paintable} subclass to invalidate
    their contents.
  @end{short}
  Unless the contents are invalidated, implementations must guarantee that
  multiple calls of the @fun{gdk:paintable-snapshot} function produce the same
  output.

  This function will emit the @code{\"invalidate-contents\"} signal. If a
  paintable reports the @code{:static-contents} flag, it must not call this
  function.
  @see-class{gdk:paintable}
  @see-class{gdk:snapshot}
  @see-symbol{gdk:paintable-flags}"
  (paintable (g:object paintable)))

(export 'paintable-invalidate-contents)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_invalidate_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_invalidate_size" paintable-invalidate-size) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Called by implementations of @class{gdk:paintable} subclasses to invalidate
    their size.
  @end{short}
  As long as the size is not invalidated, @arg{paintable} must return the same
  values for its intrinsic width, height and aspect ratio.

  This function will emit the @code{\"invalidate-size\"} signal. If a paintable
  reports the @code{:static-size} flag, it must not call this function.
  @see-class{gdk:paintable}
  @see-symbol{gdk:paintable-flags}"
  (paintable (g:object paintable)))

(export 'paintable-invalidate-size)

;;; ----------------------------------------------------------------------------
;;; gdk_paintable_new_empty
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_paintable_new_empty" paintable-new-empty)
    (g:object paintable)
 #+liber-documentation
 "@version{#2024-5-5}
  @argument[width]{an integer with the intrinsic width to report. Can be 0
    for no width}
  @argument[height]{an integer with the intrinsic height to report. Can be 0
    for no height}
  @return{The new @class{gdk:paintable} object.}
  @begin{short}
    Returns a paintable that has the given intrinsic size and draws nothing.
  @end{short}
  This is often useful for implementing the
  @code{GdkPaintableInterface.get_current_image()} virtual function when the
  paintable is in an incomplete state.
  @see-class{gdk:paintable}"
  (width :int)
  (height :int))

(export 'paintable-new-empty)

;;; ---- End of file gdk4.paintable.lisp ---------------------------------------
