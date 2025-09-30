;;; ----------------------------------------------------------------------------
;;; gtk4.image.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkImage
;;;
;;;     A widget displaying an image
;;;
;;; Types and Values
;;;
;;;     GtkImage
;;;     GtkImageType
;;;
;;; Accessors
;;;
;;;     gtk_image_get_gicon
;;;     gtk_image_get_icon_name
;;;     gtk_image_set_icon_size
;;;     gtk_image_get_icon_size
;;;     gtk_image_get_paintable
;;;     gtk_image_set_pixel_size
;;;     gtk_image_get_pixel_size
;;;     gtk_image_get_storage_type
;;;
;;; Functions
;;;
;;;     gtk_image_new
;;;     gtk_image_new_from_file
;;;     gtk_image_new_from_resource
;;;     gtk_image_new_from_pixbuf                           Deprecated 4.12
;;;     gtk_image_new_from_paintable
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_clear
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_resource
;;;     gtk_image_set_from_pixbuf                           Deprecated 4.12
;;;     gtk_image_set_from_paintable
;;;     gtk_image_set_from_icon_name
;;;     gtk_image_set_from_gicon
;;;
;;; Properties
;;;
;;;     file
;;;     gicon
;;;     icon-name
;;;     icon-size
;;;     paintable
;;;     pixel-size
;;;     resource
;;;     storage-type
;;;     use-fallback
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkImage
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkImageType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkImageType" image-type
  (:export t
   :type-initializer "gtk_image_type_get_type")
  (:empty 0)
  (:icon-name 1)
  (:gicon 2)
  (:paintable 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'image-type)
      "GEnum"
      (liber:symbol-documentation 'image-type)
 "@version{2025-04-26}
  @begin{declaration}
(gobject:define-genum \"GtkImageType\" image-type
  (:export t
   :type-initializer \"gtk_image_type_get_type\")
  (:empty 0)
  (:icon-name 1)
  (:gicon 2)
  (:paintable 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:empty]{There is no image displayed by the widget.}
      @entry[:icon-name]{The widget contains a named icon.}
      @entry[:gicon]{The widget contains a @class{g:icon} object.}
      @entry[:paintable]{The widget contains a @class{gdk:paintable} object.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes the image data representation used by a @class{gtk:image} widget.
  @end{short}
  If you want to get the image from the widget, you can only get the currently
  stored representation. For example, if the @fun{gtk:image-storage-type}
  function returns the @val[gtk:image-type]{:paintable} value, then you can call
  the @fun{gtk:image-paintable} function. For empty images, you can request any
  storage type, but they will all return @code{nil} values.
  @see-class{gtk:image}
  @see-class{gdk:paintable}
  @see-class{g:icon}
  @see-function{gtk:image-storage-type}
  @see-function{gtk:image-paintable}")

;;; ----------------------------------------------------------------------------
;;; GtkImage
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkImage" image
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_image_get_type")
  ((file
    image-file
    "file" "gchararray" t t)
   (gicon
    image-gicon
    "gicon" "GIcon" t t)
   (icon-name
    image-icon-name
    "icon-name" "gchararray" t t)
   (icon-size
    image-icon-size
    "icon-size" "GtkIconSize" t t)
   (paintable
    image-paintable
    "paintable" "GdkPaintable" t t)
   (pixel-size
    image-pixel-size
    "pixel-size" "gint" t t)
   (resource
    image-resource
    "resource" "gchararray" t t)
   (storage-type
    image-storage-type
    "storage-type" "GtkImageType" t nil)
   (use-fallback
    image-use-fallback
    "use-fallback" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'image 'type)
 "@version{2025-09-28}
  @begin{short}
    The @class{gtk:image} widget displays an image.
  @end{short}
  Various kinds of objects can be displayed as an image. Most typically, you
  would load a @class{gdk:texture} object from a file, and then display that.

  @image[image]{Figure: GtkImage}

  There is a convenience @fun{gtk:image-new-from-file} function to do this,
  used as follows:
  @begin{pre}
(let ((image (gtk:image-new-from-file \"myfile.png\")))
  ... )
  @end{pre}
  If the file is not loaded successfully, the image will contain a
  \"broken image\" icon similar to that used in many web browsers. If you want
  to handle errors in loading the file yourself, for example by displaying an
  error message, then load the image with the @fun{gdk:texture-new-from-file}
  function, then create the @class{gtk:image} widget with the
  @fun{gtk:image-new-from-paintable} function.

  Sometimes an application will want to avoid depending on external data files,
  such as image files. See the documentation of the @class{g:resource} API for
  details. In this case, the @slot[gtk:image]{resource} property, the
  @fun{gtk:image-new-from-resource} and @fun{gtk:image-set-from-resource}
  functions should be used.

  The @class{gtk:image} widget displays its image as an icon, with a size that
  is determined by the application. See the @class{gtk:picture} widget if you
  want to show an image at is actual size.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:image} implementation has a single CSS node with the name
    @code{image}. The @code{.normal-icons} or @code{.large-icons} style classes
    may appear, depending on the @slot[gtk:image]{icon-size} property.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:image} implementation uses the
    @val[gtk:accessible-role]{:img} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @see-constructor{gtk:image-new}
  @see-constructor{gtk:image-new-from-file}
  @see-constructor{gtk:image-new-from-resource}
  @see-constructor{gtk:image-new-from-pixbuf}
  @see-constructor{gtk:image-new-from-paintable}
  @see-constructor{gtk:image-new-from-icon-name}
  @see-constructor{gtk:image-new-from-gicon}
  @see-slot{gtk:image-file}
  @see-slot{gtk:image-gicon}
  @see-slot{gtk:image-icon-name}
  @see-slot{gtk:image-icon-size}
  @see-slot{gtk:image-paintable}
  @see-slot{gtk:image-pixel-size}
  @see-slot{gtk:image-resource}
  @see-slot{gtk:image-storage-type}
  @see-slot{gtk:image-use-fallback}
  @see-class{gdk:paintable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:image-file ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'image) t)
 "The @code{file} property of type @code{:string} (Read / Write) @br{}
  The name of the file to load and display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-file)
      "Accessor"
      (documentation 'image-file 'function)
 "@version{2025-08-03}
  @syntax{(gtk:image-file object) => filename}
  @syntax{(setf (gtk:image-file object) filename)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[filename]{a string for the name of the file to load and display}
  @begin{short}
    The accessor for the @slot[gtk:image]{file} slot of the
    @class{gtk:image} class gets or sets the name of the file to load and
    display.
  @end{short}
  @see-class{gtk:image}")

;;; --- gtk:image-gicon --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'image) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  The icon displayed in the image. If the icon theme is changed, the image will
  be updated automatically for themed icons.")

#+liber-documentation
(setf (liber:alias-for-function 'image-gicon)
      "Accessor"
      (documentation 'image-gicon 'function)
 "@version{2025-09-28}
  @syntax{(gtk:image-gicon object) => gicon}
  @syntax{(setf (gtk:image-gicon object) gicon)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[gicon]{a @class{g:icon} icon}
  @begin{short}
    The accessor for the @slot[gtk:image]{gicon} slot of the @class{gtk:image}
    class gets or sets the icon displayed in the image.
  @end{short}
  If the icon theme is changed, the image will be updated automatically for
  themed icons.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-class{g:themed-icon}")

;;; --- gtk:image-icon-name ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'image) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-name)
      "Accessor"
      (documentation 'image-icon-name 'function)
 "@version{2025-08-03}
  @syntax{(gtk:image-icon-name object) => name}
  @syntax{(setf (gtk:image-icon-name object) name)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[name]{a string for the name of the icon}
  @begin{short}
    The accessor for the @slot[gtk:image]{icon-name} slot of the
    @class{gtk:image} class gets or sets the name of the icon in the icon theme.
  @end{short}
  If the icon theme is changed, the image will be updated automatically.
  @see-class{gtk:image}")

;;; --- gtk:image-icon-size ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size" 'image) t)
 "The @code{icon-size} property of type @sym{gtk:icon-size} (Read / Write) @br{}
  The symbolic size to display icons at. @br{}
  Default value: @val[gtk:icon-size]{:inherit}")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-size)
      "Accessor"
      (documentation 'image-icon-size 'function)
 "@version{2025-08-03}
  @syntax{(gtk:image-icon-size object) => size}
  @syntax{(setf (gtk:image-icon-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{a @sym{gtk:icon-size} value}
  @begin{short}
    The accessor for the @slot[gtk:image]{icon-size} slot of the
    @class{gtk:image} class gets or sets the symbolic size to display icons at.
  @end{short}
  @see-class{gtk:image}
  @see-symbol{gtk:icon-size}")

;;; --- gtk:image-paintable ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paintable" 'image) t)
 "The @code{paintable} property of type @class{gdk:paintable} (Read / Write)
  @br{}
  The paintable to display.")

#+liber-documentation
(setf (liber:alias-for-function 'image-paintable)
      "Accessor"
      (documentation 'image-paintable 'function)
 "@version{2025-08-03}
  @syntax{(gtk:image-paintable object) => paintable}
  @syntax{(setf (gtk:image-paintable object) paintable)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    The accessor for the @slot[gtk:image]{paintable} slot of the
    @class{gtk:image} class get or sets the paintable being displayed by the
    image.
  @end{short}
  The @sym{gtk:image-type} value of the image must be
  @val[gtk:image-type]{:empty} or @val[gtk:image-type]{:paintable}, see the
  @fun{gtk:image-storage-type} function.
  @see-class{gtk:image}
  @see-class{gdk:paintable}
  @see-symbol{gtk:image-type}
  @see-function{gtk:image-storage-type}")

;;; --- gtk:image-pixel-size ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixel-size" 'image) t)
 "The @code{pixel-size} property of type @code{:int} (Read / Write) @br{}
  Can be used to specify a fixed size overriding the @slot[gtk:image]{icon-size}
  property for images of @val[gtk:image-type]{:icon-name} type. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'image-pixel-size)
      "Accessor"
      (documentation 'image-pixel-size 'function)
 "@version{2025-09-28}
  @syntax{(gtk:image-pixel-size object) => size}
  @syntax{(setf (gtk:image-pixel-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{an integer for the pixel size}
  @begin{short}
    The accessor for the @slot[gtk:image]{pixel-size} slot of the
    @class{gtk:image} class gets or sets the pixel size used for named icons
    of @val[gtk:image-type]{:icon-name} type.
  @end{short}
  If the pixel size is set to a value not equal to -1, it is used instead of
  the @slot[gtk:image]{icon-size} property.
  @see-class{gtk:image}
  @see-function{gtk:image-icon-size}")

;;; --- gtk:image-resource -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resource" 'image) t)
 "The @code{resource} property of type @code{:string} (Read / Write) @br{}
  The path to the resource file for the image to display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-resource)
      "Accessor"
      (documentation 'image-resource 'function)
 "@version{2025-09-28}
  @syntax{(gtk:image-resource object) => path}
  @syntax{(setf (gtk:image-resource object) path)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[path]{a string for a resource path}
  @begin{short}
    The accessor for the @slot[gtk:image]{resource} slot of the
    @class{gtk:image} class gets or sets a path to the resource file for the
    image to display.
  @end{short}
  @see-class{gtk:image}")

;;; --- gtk:image-storage-type -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "storage-type" 'image) t)
 "The @code{storage-type} property of type @sym{gtk:image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @val[gtk:image-type]{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'image-storage-type)
      "Accessor"
      (documentation 'image-storage-type 'function)
 "@version{2025-09-28}
  @syntax{(gtk:image-storage-type object) => type}
  @syntax{(setf (gtk:image-storage-type object) type)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[type]{a value of the @sym{gtk:image-type} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:image]{storage-type} slot of the
    @class{gtk:image} class gets or sets the type of representation being used
    by the @class{gtk:image} widget to store image data.
  @end{short}
  If the @class{gtk:image} widget has no image data, the return value will be
  @val[gtk:image-type]{:empty}.
  @see-class{gtk:image}
  @see-symbol{gtk:image-type}")

;;; --- gtk:image-use-fallback -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-fallback" 'image) t)
 "The @code{use-fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether the icon displayed in the image will use standard icon names fallback.
  The value of this property is only relevant for images of
  @val[gtk:image-type]{:icon-name} and @val[gtk:image-type]{:gicon} type. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'image-use-fallback)
      "Accessor"
      (documentation 'image-use-fallback 'function)
 "@version{2025-08-03}
  @syntax{(gtk:image-use-fallback object) => setting}
  @syntax{(setf (gtk:image-use-fallback object) setting)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[setting]{a boolean whether to use standard icon names fallback}
  @begin{short}
    The accessor for the @slot[gtk:image]{use-fallback} slot of the
    @class{gtk:image} class gets or sets whether the icon displayed in the
    @fun{gtk:image} widget will use standard icon names fallback.
  @end{short}
  The value of this property is only relevant for images of
  @val[gtk:image-type]{:icon-name} and @val[gtk:image-type]{:gicon} type.
  @see-class{gtk:image}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_new
;;; ----------------------------------------------------------------------------

(declaim (inline image-new))

(defun image-new ()
 #+liber-documentation
 "@version{2025-04-26}
  @return{The newly created @class{gtk:image} widget.}
  @short{Creates a new empty image.}
  @see-class{gtk:image}"
  (make-instance 'image))

(export 'image-new)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_file" %image-new-from-file) (g:object widget)
  (filename :string))

(defun image-new-from-file (path)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[path]{a pathname or namestring for the file to load}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates a new image that displays the file loaded from @arg{path}.
  @end{short}
  This function always returns a valid @class{gtk:image} widget. If the file is
  not found or cannot be loaded, the resulting @class{gtk:image} widget will
  display a \"broken image\" icon.

  If you need to detect failures to load the file, use the
  @fun{gdk:texture-new-from-file} function to load the file yourself, then
  create the @class{gtk:image} widget from the texture.

  The storage type, see the @fun{gtk:image-storage-type} function, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk:image}
  @see-function{gdk:texture-new-from-file}
  @see-function{gtk:image-storage-type}"
  (%image-new-from-file (namestring path)))

(export 'image-new-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_resource" image-new-from-resource)
    (g:object image)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[resource]{a string for a resource path}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image that displays the resource loaded from @arg{resource}.
  @end{short}
  This function always returns a valid @class{gtk:image} widget. If the file is
  not found or can not be loaded, the resulting @class{gtk:image} widget will
  display a \"broken image\" icon.

  If you need to detect failures to load the file, use the
  @fun{gdk-pixbuf:pixbuf-new-from-resource} function to load the file yourself,
  then create the @class{gtk:image} widget from the pixbuf, or for animations,
  use the @fun{gdk-pixbuf:pixbuf-animation-new-from-resource} function.

  The storage type, see the @fun{gtk:image-storage-type} function, of the
  returned image is not defined, it will be whatever is appropriate for
  displaying the file.
  @see-class{gtk:image}
  @see-function{gdk-pixbuf:pixbuf-new-from-resource}
  @see-function{gdk-pixbuf:pixbuf-animation-new-from-resource}"
  (resource :string))

(export 'image-new-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_pixbuf" %image-new-from-pixbuf)
    (g:object image)
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(defun image-new-from-pixbuf (pixbuf)
 #+liber-documentation
 "@version{2025-04-26}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates an image that displays the given @arg{pixbuf}.
  @end{short}
  Note that this function just creates an image from the pixbuf. The image
  created will not react to state changes. Should you want that, you should use
  the @fun{gtk:image-new-from-icon-name} function.
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:image-new-from-paintable} and @fun{gdk:texture-new-for-pixbuf}
    functions instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-new-from-icon-name}
  @see-function{gtk:image-new-from-paintable}
  @see-function{gdk:texture-new-for-pixbuf}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:IMAGE-NEW-FROM-PIXBUF is deprecated since 4.12"))
  (%image-new-from-pixbuf pixbuf))

(export 'image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_paintable" image-new-from-paintable)
    (g:object image)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{The new @class{gtk:image} widget.}
  @begin{short}
    Creates a new image that displays the given @arg{paintable}.
  @end{short}
  The image will track changes to the paintable and update its size and contents
  in response to it.
  @see-class{gtk:image}
  @see-class{gdk:paintable}"
  (paintable (g:object gdk:paintable)))

(export 'image-new-from-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_icon_name" image-new-from-icon-name)
    (g:object image)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[name]{a string for an icon name}
  @return{The new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image that displays an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:image}
  @see-function{gtk:image-set-from-icon-name}"
  (name :string))

(export 'image-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_gicon" image-new-from-gicon)
    (g:object image)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[icon]{a @class{g:icon} object}
  @return{The new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image that displays an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:image}
  @see-class{g:icon}"
  (icon (g:object g:icon)))

(export 'image-new-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_image_clear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_clear" image-clear) :void
 #+liber-documentation
 "@version{2025-04-26}
  @argument[image]{a @class{gtk:image} widget}
  @short{Resets the image to be empty.}
  @see-class{gtk:image}"
  (image (g:object image)))

(export 'image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_file" %image-set-from-file) :void
  (image (g:object image))
  (file :string))

(defun image-set-from-file (image file)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[file]{a pathname or namestring for the file to load}
  @begin{short}
    Sets the image to display the given @arg{file}.
  @end{short}
  See the @fun{gtk:image-new-from-file} function for details.
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-file}"
  (%image-set-from-file image (namestring file)))

(export 'image-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_resource" image-set-from-resource) :void
 #+liber-documentation
 "@version{2025-09-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[resource]{a string for a resource path}
  @begin{short}
    Sets the image to display the given @arg{resource}.
  @end{short}
  See the @fun{gtk:image-new-from-resource} function for details.
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-resource}"
  (image (g:object image))
  (resource :string))

(export 'image-set-from-resource)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_pixbuf
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_pixbuf" %image-set-from-pixbuf) :void
  (image (g:object image))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(defun image-set-from-pixbuf (image pixbuf)
 #+liber-documentation
 "@version{2025-09-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Sets the image to display the given @arg{pixbuf}.
  @end{short}
  @begin[Warning]{dictionary}
    This function is deprecated since 4.12. Use the
    @fun{gtk:image-set-from-paintable} function instead.
  @end{dictionary}
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-set-from-paintable}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:IMAGE-SET-FROM-PIXBUF is deprecated since 4.12"))
    (%image-set-from-pixbuf image pixbuf))

(export 'image-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_paintable" image-set-from-paintable) :void
 #+liber-documentation
 "@version{2025-09-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Sets the image to display the given @arg{paintable}.
  @end{short}
  See the @fun{gtk:image-new-from-paintable} function for more details.
  @see-class{gtk:image}
  @see-class{gdk:paintable}
  @see-function{gtk:image-new-from-paintable}"
  (image (g:object image))
  (paintable (g:object gdk:paintable)))

(export 'image-set-from-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_icon_name" image-set-from-icon-name) :void
 #+liber-documentation
 "@version{2025-09-28}
  @argument[image]{a @class{gtk:image} widget}
  @argument[name]{a string for an icon name}
  @begin{short}
    Sets the image to display the given @arg{name}.
  @end{short}
  See the @fun{gtk:image-new-from-icon-name} function for details.
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-icon-name}"
  (image (g:object image))
  (name :string))

(export 'image-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_gicon" image-set-from-gicon) :void
 #+liber-documentation
 "@version{2025-04-26}
  @argument[image]{a @class{gtk:image} widget}
  @argument[icon]{a @class{g:icon} object for the themed icon}
  @begin{short}
    Sets the image to display the themed icon.
  @end{short}
  See the @fun{gtk:image-new-from-gicon} function for details.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-function{gtk:image-new-from-gicon}"
  (image (g:object image))
  (icon (g:object g:icon)))

(export 'image-set-from-gicon)

;;; --- End of file gtk4.image.lisp --------------------------------------------
