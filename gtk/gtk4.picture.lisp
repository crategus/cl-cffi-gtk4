;;; ----------------------------------------------------------------------------
;;; gtk.picture.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkPicture
;;;
;;;     A widget displaying a GdkPaintable
;;;
;;; Types and Values
;;;
;;;     GtkPicture
;;;     GtkContentFit                                      Since 4.8
;;;
;;; Accessors
;;;
;;;     gtk_picture_set_alternative_text
;;;     gtk_picture_get_alternative_text
;;;     gtk_picture_set_can_shrink
;;;     gtk_picture_get_can_shrink
;;;     gtk_picture_set_file
;;;     gtk_picture_get_file
;;;     gtk_picture_set_keep_aspect_ratio
;;;     gtk_picture_get_keep_aspect_ratio
;;;     gtk_picture_set_paintable
;;;     gtk_picture_get_paintable
;;;     gtk_picture_set_content_fit                        Since 4.8
;;;     gtk_picture_get_content_fit                        Since 4.8
;;;
;;; Functions
;;;
;;;     gtk_picture_new
;;;     gtk_picture_new_for_paintable
;;;     gtk_picture_new_for_pixbuf
;;;     gtk_picture_new_for_file
;;;     gtk_picture_new_for_filename
;;;     gtk_picture_new_for_resource
;;;     gtk_picture_set_pixbuf
;;;     gtk_picture_set_filename
;;;     gtk_picture_set_resource
;;;
;;; Properties
;;;
;;;     alternative-text
;;;     can-shrink
;;;     content-fit                                        Since 4.8
;;;     file
;;;     keep-aspect-ratio
;;;     paintable
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPicture
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkContentFit
;;;
;;; Controls how a content should be made to fit inside an allocation.
;;;
;;; Available since: 4.8
;;;
;;; GTK_CONTENT_FIT_FILL :
;;;     Make the content fill the entire allocation, without taking its aspect
;;;     ratio in consideration. The resulting content will appear as stretched
;;;     if its aspect ratio is different from the allocation aspect ratio.
;;;
;;; GTK_CONTENT_FIT_CONTAIN :
;;;     Scale the content to fit the allocation, while taking its aspect ratio
;;;     in consideration. The resulting content will appear as letterboxed if
;;;     its aspect ratio is different from the allocation aspect ratio.
;;;
;;; GTK_CONTENT_FIT_COVER :
;;;     Cover the entire allocation, while taking the content aspect ratio in
;;;     consideration. The resulting content will appear as clipped if its
;;;     aspect ratio is different from the allocation aspect ratio.
;;;
;;; GTK_CONTENT_FIT_SCALE_DOWN :
;;;     The content is scaled down to fit the allocation, if needed, otherwise
;;;     its original size is used.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkPicture
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkPicture" picture
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_picture_get_type")
  ((alternative-text
    picture-alternative-text
    "alternative-text" "gchararray" t t)
   (can-shrink
    picture-can-shrink
    "can-shrink" "gboolean" t t)
   #+gtk-4-8
   (content-fit
    picture-content-fit
    "content-fit" "GtkContentFit" t t)
   (file
    picture-file
    "file" "GFile" t t)
   (keep-aspect-ratio
    picture-keep-aspect-ratio
    "keep-aspect-ratio" "gboolean" t t)
   (paintable
    picture-paintable
    "paintable" "GdkPaintable" t t)))

#+liber-documentation
(setf (documentation 'picture 'type)
 "@version{#2022-7-29}
  @begin{short}
    The @sym{picture} widget displays a @class{gdk:paintable} object.
  @end{short}

  @image[picture]{Figure: GtkPicture}

  Many convenience functions are provided to make pictures simple to use. For
  example, if you want to load an image from a file, and then display that,
  there is a convenience function to do this:
  @begin{pre}
(let ((picture (picture-new-for-filename \"myfile.png\")))
   ... )
  @end{pre}
  If the file is not loaded successfully, the picture will contain a \"broken
  image\" icon similar to that used in many web browsers. If you want to handle
  errors in loading the file yourself, for example by displaying an error
  message, then load the image with the @fun{gdk-texture-new-from-file}
  function, then create the @sym{picture} widget with the
  @fun{picture-new-for-paintable} function.

  Sometimes an application will want to avoid depending on external data files,
  such as image files. See the documentation of the @class{g-resource} API for
  details. In this case, the @fun{picture-new-for-resource} and
  @fun{picture-set-resource} functions should be used.

  @subheading{Sizing the paintable}
  You can influence how the paintable is displayed inside the @sym{picture}
  widget. By turning off the @code{keep-aspect-ratio} property you can allow the
  paintable to get stretched. The @code{can-shrink} property can be unset to
  make sure that paintables are never made smaller than their ideal size - but
  be careful if you do not know the size of the paintable in use, like when
  displaying user-loaded images. This can easily cause the picture to grow
  larger than the screen. And the @slot[widget]{halign} and
  @slot[widget]{valign} properties can be used to make sure the paintable
  does not fill all available space but is instead displayed at its original
  size.
  @begin[CSS nodes]{dictionary}
    The @sym{picture} implementation has a single CSS node with the name
    @code{picture}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{picture} implementation uses the @code{:img} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{picture-alternative-tex}
  @see-slot{picture-can-shrink}
  @see-slot{picture-file}
  @see-slot{picture-keep-aspect-ratio}
  @see-slot{picture-paintable}
  @see-constructor{picture-new}
  @see-constructor{picture-new-for-paintable}
  @see-constructor{picture-new-for-pixbuf}
  @see-constructor{picture-new-for-file}
  @see-constructor{picture-new-for-filename}
  @see-constructor{picture-new-for-resource}
  @see-class{gdk:paintable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- picture-alternative-text -------------------------------------------

;; TODO: To unset the text we have to use the NULL-POINTER value. Can we
;; implement a function with uses this with the NIL value?

#+liber-documentation
(setf (documentation (liber:slot-documentation "alternative-text"
                                               'picture) t)
 "The @code{alternative-text} property of type @code{:string} (Read / Write)
  @br{}
  The alternative textual description for the picture. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'picture-alternative-text)
      "Accessor"
      (documentation 'picture-alternative-text 'function)
 "@version{#2022-7-29}
  @syntax[]{(picture-alternative-text object) => text}
  @syntax[]{(setf (picture-alternative-text object) text)}
  @argument[object]{a @class{picture} widget}
  @argument[text]{a string with the alternative textual description for the
    picture}
  @begin{short}
    Accessor of the @slot[picture]{alternative-text} slot of the
    @class{picture} class.
  @end{short}
  The @sym{picture-alternative-text} function gets the alternative textual
  description of the picture or returns @code{nil} if the picture cannot be
  described textually. The @sym{(setf picture-alternative-text)} function
  sets an alternative textual description. It is equivalent to the \"alt\"
  attribute for images on websites. This text will be made available to
  accessibility tools. If the picture cannot be described textually, set this
  property to @code{null-pointer}.
  @see-class{picture}")

;;; --- picture-can-shrink -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-shrink" 'picture) t)
 "The @code{can-shrink} property of type @code{:boolean} (Read / Write) @br{}
  Whether the picture can be made smaller than the self it contains. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'picture-can-shrink)
      "Accessor"
      (documentation 'picture-can-shrink 'function)
 "@version{#2022-7-29}
  @syntax[]{(picture-can-shrink object) => setting}
  @syntax[]{(setf (picture-can-shrink object) setting)}
  @argument[object]{a @class{picture} widget}
  @argument[setting]{a boolean whether the picture can be made smaller than it
    contents}
  @begin{short}
    Accessor of the @slot[picture]{can-shrink} slot of the
    @class{picture} class.
  @end{short}
  If set to the @em{true} value, the picture can be made smaller than its
  contents. The contents will then be scaled down when rendering. If you want
  to still force a minimum size manually, consider using the
  @fun{gtk:widget-set-size-request} function. Also of note is that a similar
  function for growing does not exist because the grow behavior can be
  controlled via the @fun{gtk:widget-halign} and @fun{gtk:widget-valign}
  functions.
  @see-class{picture}
  @see-function{gtk:widget-size-request}
  @see-function{gtk:widget-halign}
  @see-function{gtk:widget-valign}")

;;; --- picture-file -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'picture) t)
 "The @code{file} property of type @class{g-file} (Read / Write) @br{}
  The file that is displayed or @code{nil} if none.")

#+liber-documentation
(setf (liber:alias-for-function 'picture-file)
      "Accessor"
      (documentation 'picture-file 'function)
 "@version{#2022-7-29}
  @syntax[]{(picture-file object) => file}
  @syntax[]{(setf (picture-file object) file)}
  @argument[object]{a @class{picture} widget}
  @argument[file]{a @class{g-file} object that is displayed or @code{nil} if
    none}
  @begin{short}
    Accessor of the @slot[picture]{file} slot of the @class{picture}
    class.
  @end{short}
  The @sym{picture-file} function gets the file currently displayed if
  the picture is displaying a file. If the picture is not displaying a file,
  for example when the @fun{picture-paintable} function was used, then
  @code{nil} is returned. The @sym{(setf picture-file)} function makes the
  picture load and display @arg{file}. See the @fun{picture-new-for-file}
  documentation for more details.
  @see-class{picture}
  @see-function{picture-paintable}
  @see-function{picture-new-for-file}")

;;; --- picture-keep-aspect-ratio ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "keep-aspect-ratio"
                                               'picture) t)
 "The @code{keep-aspect-ratio} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the picture will render its contents trying to preserve the aspect
  ratio of the contents. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'picture-keep-aspect-ratio)
      "Accessor"
      (documentation 'picture-keep-aspect-ratio 'function)
 "@version{#2022-7-29}
  @syntax[]{(picture-keep-aspect-ratio object) => setting}
  @syntax[]{(setf (picture-keep-aspect-ratio object) setting)}
  @argument[object]{a @class{picture} widget}
  @argument[setting]{a boolean whether the picture will render its contents
    trying to preserve the aspect ratio of the contents}
  @begin{short}
    Accessor of the @slot[picture]{keep-aspect-ratio} slot of the
    @class{picture} class.
  @end{short}
  If set to the @em{true} value, the picture will render its contents according
  to its aspect ratio. That means that empty space may show up at the top/bottom
  or left/right of the picture. If set to the @em{false} value or if the
  contents provide no aspect ratio, the contents will be stretched over the
  whole area of the picture.
  @see-class{picture}")

;;; --- picture-paintable --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paintable" 'picture) t)
 "The @code{paintable} property of type @class{gdk:paintable} (Read / Write)
  @br{}
  The paintable to be displayed by the picture.")

#+liber-documentation
(setf (liber:alias-for-function 'picture-paintable)
      "Accessor"
      (documentation 'picture-paintable 'function)
 "@version{#2022-7-29}
  @syntax[]{(picture-keep-paintable object) => paintable}
  @syntax[]{(setf (picture-paintable object) paintable)}
  @argument[object]{a @class{picture} widget}
  @argument[paintable]{a @class{gdk:paintable} object or @code{nil}}
  @begin{short}
    Accessor of the @slot[picture]{paintable} slot of the
    @class{picture} class.
  @end{short}
  The @sym{picture-paintable} function gets the paintable being displayed by
  the picture. The @sym{(setf picture-paintable)} function makes the picture
  display the given paintable. If the paintable is @code{nil}, nothing will be
  displayed. See the @fun{picture-new-for-paintable} documentation for more
  details.
  @see-class{picture}
  @see-class{gdk:paintable}
  @see-function{picture-new-for-paintable}")

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new ()
;;; ----------------------------------------------------------------------------

(defun picture-new ()
 #+liber-documentation
 "@version{#2022-7-29}
  @return{A newly created @class{picture} widget.}
  @short{Creates a new empty picture.}
  @see-class{picture}"
  (make-instance 'picture))

(export 'picture-new)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new_for_paintable ()
;;; ----------------------------------------------------------------------------

(defun picture-new-for-paintable (paintable)
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[paintable]{a @class{gdk:paintable} object, or @code{nil}}
  @return{A new @class{picture} widget.}
  @begin{short}
    Creates a new picture displaying @arg{paintable}.
  @end{short}
  The @class{picture} widget will track changes to the paintable and update
  its size and contents in response to it.
  @see-class{picture}
  @see-class{gdk:paintable}"
  (make-instance 'picture
                 :paintable paintable))

(export 'picture-new-for-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new_for_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_new_for_pixbuf" picture-new-for-pixbuf)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object, or @code{nil}}
  @return{A new @class{picture} widget}
  @begin{short}
    Creates a new picture displaying @arg{pixbuf}.
  @end{short}
  This is a utility function that calls the @fun{picture-new-for-paintable}
  function. See that function for details.

  The pixbuf must not be modified after passing it to this function.
  @see-class{picture}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{picture-new-for-paintable}"
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'picture-new-for-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new_for_file ()
;;; ----------------------------------------------------------------------------

(defun picture-new-for-file (file)
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[file]{a @class{g-file} object}
  @return{A new @class{picture} widget.}
  @begin{short}
    Creates a new picture displaying the given file.
  @end{short}
  If the file is not found or cannot be loaded, the resulting picture is empty.

  If you need to detect failures to load the file, use the
  @fun{gdk-texture-new-from-file} function to load the file yourself, then
  create the picture from the texture.
  @see-class{picture}
  @see-class{g-file}
  @see-function{gdk-texture-new-from-file}"
  (make-instance 'picture
                 :file file))

(export 'picture-new-for-file)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new_for_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_new_for_filename" picture-new-for-filename)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[filename]{a string with a filename}
  @return{A new @class{picture} widget.}
  @begin{short}
    Creates a new picture displaying the file @arg{filename}.
  @end{short}
  This is a utility function that calls the @fun{picture-new-for-file}
  function. See that function for details.
  @see-class{picture}
  @see-function{picture-new-for-file}"
  (filename :string))

(export 'picture-new-for-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_new_for_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_new_for_resource" picture-new-for-resource)
     (g:object widget)
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[path]{a string with the resource path}
  @return{A new @class{picture} widget.}
  @begin{short}
    Creates a new picture displaying the resource at @arg{path}.
  @end{short}
  This is a utility function that calls the @fun{picture-new-for-file}
  function. See that function for details.
  @see-class{picture}
  @see-function{picture-new-for-file}"
  (path :string))

(export 'picture-new-for-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_set_pixbuf ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_set_pixbuf" picture-set-pixbuf) :void
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[picture]{a @class{picture} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object, or @code{nil}}
  @begin{short}
    See the @fun{picture-new-for-pixbuf} function for details.
  @end{short}
  This is a utility function that calls the @fun{picture-paintable}
  function,
  @see-class{picture}
  @see-function{picture-new-for-pixbuf}
  @see-function{picture-paintable}"
  (picture (g:object picture))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'picture-set-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_set_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_set_filename" picture-set-filename) :void
 #+liber-documentation
 "@version{#2022-7-29}
  @argument[picture]{a @class{picture} widget}
  @argument[filename]{a string with the filename}
  @begin{short}
    Makes the picture load and display the given @arg{filename}.
  @end{short}
  This is a utility function that calls the @fun{picture-file} function,
  @see-class{picture}
  @see-function{picture-file}"
  (picture (g:object picture))
  (filename :string))

(export 'picture-set-filename)

;;; ----------------------------------------------------------------------------
;;; gtk_picture_set_resource ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_picture_set_resource" picture-set-resource) :void

 #+liber-documentation
 "@version{#2022-7-29}
  @argument[picture]{a @class{picture} widget}
  @argument[path]{a string with the resource path}
  @begin{short}
    Makes the picture load and display the resource at the given resource path.
  @end{short}
  This is a utility function that calls the @fun{picture-file} function,
  @see-class{picture}
  @see-function{picture-file}"
  (picture (g:object picture))
  (path :string))

(export 'picture-set-resource)

;;; --- End of file gtk.picture.lisp -------------------------------------------
