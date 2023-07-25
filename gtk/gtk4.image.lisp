;;; ----------------------------------------------------------------------------
;;; gtk4.image.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;;     gtk_image_new_from_pixbuf
;;;     gtk_image_new_from_paintable
;;;     gtk_image_new_from_icon_name
;;;     gtk_image_new_from_gicon
;;;     gtk_image_clear
;;;     gtk_image_set_from_file
;;;     gtk_image_set_from_resource
;;;     gtk_image_set_from_pixbuf
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

(gobject:define-g-enum "GtkImageType" image-type
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
 "@version{#2022-1-28}
  @begin{short}
    Describes the image data representation used by a @class{gtk:image} widget.
  @end{short}
  If you want to get the image from the widget, you can only get the currently
  stored representation. e.g. if the @fun{gtk:image-storage-type} function
  returns the @code{:paintable} value, then you can call the
  @fun{gtk:image-paintable} function. For empty images, you can request any
  storage type, but they will all return @code{nil} values.
  @begin{pre}
(gobject:define-g-enum \"GtkImageType\" image-type
  (:export t
   :type-initializer \"gtk_image_type_get_type\")
  (:empty 0)
  (:icon-name 1)
  (:gicon 2)
  (:paintable 3))
  @end{pre}
  @begin[code]{table}
    @entry[:empty]{There is no image displayed by the widget.}
    @entry[:icon-name]{The widget contains a named icon.}
    @entry[:gicon]{The widget contains a @class{g:icon} object.}
    @entry[:paintable]{The widget contains a @class{gdk:paintable} object.}
  @end{table}
  @see-class{gtk:image}
  @see-class{gdk:paintable}
  @see-class{g:icon}
  @see-function{gtk:image-storage-type}")

;;; ----------------------------------------------------------------------------
;;; GtkImage
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkImage" image
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
 "@version{2023-3-19}
  @begin{short}
    The @sym{gtk:image} widget displays an image.
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
  function, then create the @sym{gtk:image} widget with the
  @fun{gtk:image-new-from-paintable} function.

  Sometimes an application will want to avoid depending on external data files,
  such as image files. See the documentation of the @class{g:resource} API for
  details. In this case, the @code{resource} property, the
  @fun{gtk:image-new-from-resource} and @fun{gtk:image-set-from-resource}
  functions should be used.

  The @class{gtk:image} widget displays its image as an icon, with a size that
  is determined by the application. See the @class{gtk:picture} widget if you
  want to show an image at is actual size.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:image} implementation has a single CSS node with the name
    @code{image}. The @code{.normal-icons} or @code{.large-icons} style classes
    may appear, depending on the @code{icon-size} property.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:image} implementation uses the @code{:img} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:image-file}
  @see-slot{gtk:image-gicon}
  @see-slot{gtk:image-icon-name}
  @see-slot{gtk:image-icon-size}
  @see-slot{gtk:image-paintable}
  @see-slot{gtk:image-pixel-size}
  @see-slot{gtk:image-resource}
  @see-slot{gtk:image-storage-type}
  @see-slot{gtk:image-use-fallback}
  @see-constructor{gtk:image-new}
  @see-constructor{gtk:image-new-from-file}
  @see-constructor{gtk:image-new-from-resource}
  @see-constructor{gtk:image-new-from-pixbuf}
  @see-constructor{gtk:image-new-from-paintable}
  @see-constructor{gtk:image-new-from-icon-name}
  @see-constructor{gtk:image-new-from-gicon}
  @see-class{gdk:paintable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- image-file -------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "file" 'image) t)
 "The @code{file} property of type @code{:string} (Read / Write) @br{}
  The name of the file to load and display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-file)
      "Accessor"
      (documentation 'image-file 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-file object) => filename}
  @syntax[]{(setf (gtk:image-file object) filename)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[filename]{a string with the name of the file to load and display}
  @begin{short}
    Accessor of the @slot[gtk:image]{file} slot of the @class{gtk:image} class.
  @end{short}

  The name of the file to load and display.
  @see-class{gtk:image}")

;;; --- image-gicon ------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gicon" 'image) t)
 "The @code{gicon} property of type @class{g:icon} (Read / Write) @br{}
  The icon displayed in the image. For themed icons, if the
  icon theme is changed, the image will be updated automatically.")

#+liber-documentation
(setf (liber:alias-for-function 'image-gicon)
      "Accessor"
      (documentation 'image-gicon 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-gicon object) => gicon}
  @syntax[]{(setf (gtk:image-gicon object) gicon)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[gicon]{a @class{g:icon} icon}
  @begin{short}
    Accessor of the @slot[gtk:image]{gicon} slot of the @class{gtk:image} class.
  @end{short}

  The icon displayed in the image. For themed icons, if the icon theme is
  changed, the image will be updated automatically.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-class{g:themed-icon}")

;;; --- image-icon-name --------------------------------------------------------

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
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-icon-name object) => icon-name}
  @syntax[]{(setf (gtk:image-icon-name object) icon-name)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[icon-name]{a string with the name of the icon}
  @begin{short}
    Accessor of the @slot[gtk:image]{icon-name} slot of the @class{gtk:image}
    class.
  @end{short}

  The name of the icon in the icon theme. If the icon theme is changed, the
  image will be updated automatically.
  @see-class{gtk:image}
  @see-function{gtk:image-get-icon-name}")

;;; --- image-icon-size --------------------------------------------------------

;; TODO: Check the implementation of gtk:icon-size. Can we use an
;; enumeration value instead of an integer?

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-size" 'image) t)
 "The @code{icon-size} property of type @symbol{gtk:icon-size} (Read / Write)
  @br{}
  The symbolic size to display icons at.")

#+liber-documentation
(setf (liber:alias-for-function 'image-icon-size)
      "Accessor"
      (documentation 'image-icon-size 'function)
 "@version{#2023-3-19}
  @syntax[]{(gtk:image-icon-size object) => size}
  @syntax[]{(setf (gtk:image-icon-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{a @symbol{gtk:icon-size} value}
  @begin{short}
    Accessor of the @slot[gtk:image]{icon-size} slot of the @class{gtk:image}
    class.
  @end{short}
  The symbolic size to display icons at.
  @begin[Note]{dictionary}
    In C the @code{icon-size} property is implemented as an integer type.
    Therefore the @sym{gtk:image-icon-size} accessor returns an integer value
    and not a keyword value of the @symbol{gtk:icon-size} enumeration.
  @end{dictionary}
  @see-class{gtk:image}
  @see-symbol{gtk:icon-size}")

;;; --- image-paintable --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paintable" 'image) t)
 "The @code{paintable} property of type @class{gdk:paintable} (Read / Write)
  @br{}
  A paintable to display.")

#+liber-documentation
(setf (liber:alias-for-function 'image-paintable)
      "Accessor"
      (documentation 'image-paintable 'function)
 "@version{#2022-1-28}
  @syntax[]{(gtk:image-paintable object) => paintable}
  @syntax[]{(setf (gtk:image-paintable object) paintable)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Accessor of the @slot[gtk:image]{paintable} slot of the @class{gtk:image}
    class.
  @end{short}

  The @sym{gtk:image-paintable} function gets the paintable being displayed by
  the image. The @sym{(setf gtk:image-paintable)} function sets the paintable.

  The @symbol{gtk:image-type} storage type of the image must be @code{:empty}
  or @code{:paintable}, see the @fun{gtk:image-storage-type} function.
  @see-class{gtk:image}
  @see-class{gdk:paintable}
  @see-symbol{gtk:image-type}
  @see-function{gtk:image-storage-type}")

;;; --- image-pixel-size -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixel-size" 'image) t)
 "The @code{pixel-size} property of type @code{:int} (Read / Write) @br{}
  Can be used to specify a fixed size overriding the @code{icon-size} property
  for images of @code{:icon-name} type. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'image-pixel-size)
      "Accessor"
      (documentation 'image-pixel-size 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-pixel-size object) => size}
  @syntax[]{(setf (gtk:image-pixel-size object) size)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[size]{an integer with the new pixel size}
  @begin{short}
    Accessor of the @slot[gtk:image]{pixel-size} slot of the @class{gtk:image}
    class.
  @end{short}

  The @sym{gtk:image-pixel-size} function sets the pixel size used for named
  icons. The @sym{(setf gtk:image-pixel-size)} function sets the pixel size. If
  the pixel size is set to a value not equal to -1, it is used instead of the
  @slot[gtk:image]{icon-size} property.
  @see-class{gtk:image}
  @see-function{gtk:image-icon-size}")

;;; --- image-resource ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resource" 'image) t)
 "The @code{resource} property of type @code{:string} (Read / Write) @br{}
  A path to a resource file to display. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'image-resource)
      "Accessor"
      (documentation 'image-resource 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-resource object) => path}
  @syntax[]{(setf (gtk:image-resource object) path)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[path]{a string with a resource path}
  @begin{short}
    Accessor of the @slot[gtk:image]{stock} slot of the @class{gtk:image} class.
  @end{short}

  A path to a resource file to display.
  @see-class{gtk:image}")

;;; --- image-storage-type -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "storage-type" 'image) t)
 "The @code{storage-type} property of type @symbol{gtk:image-type} (Read) @br{}
  The representation being used for image data. @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'image-storage-type)
      "Accessor"
      (documentation 'image-storage-type 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-storage-type object) => type}
  @syntax[]{(setf (gtk:image-storage-type object) type)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:image]{storage-type} slot of the @class{gtk:image}
    class.
  @end{short}

  The @sym{gtk:image-storage-type} function gets the type of representation
  being used by the @class{gtk:image} widget to store image data. The
  @sym{(setf gtk:image-storage-type)} function sets the image type.

  If the @class{gtk:image} widget has no image data, the return value will be
  @code{:empty}.
  @see-class{gtk:image}
  @see-symbol{gtk:image-type}")

;;; --- image-use-fallback -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-fallback" 'image) t)
 "The @code{use-fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether the icon displayed in the image will use standard icon names fallback.
  The value of this property is only relevant for images of @code{:icon-name}
  and @code{:gicon} type. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'image-use-fallback)
      "Accessor"
      (documentation 'image-use-fallback 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:image-use-fallback object) => use-fallback}
  @syntax[]{(setf (gtk:image-use-fallback object) use-fallback)}
  @argument[object]{a @class{gtk:image} widget}
  @argument[use-fallback]{a boolean whether to use standard icon names fallback}
  @begin{short}
    Accessor of the @slot[gtk:image]{use-fallback} slot of the @class{gtk:image}
    class.
  @end{short}

  Whether the icon displayed in the @sym{gtk:image} widget will use standard
  icon names fallback. The value of this property is only relevant for images
  of @code{:icon-name} and @code{:gicon} type.
  @see-class{gtk:image}")

;;; ----------------------------------------------------------------------------
;;; gtk_image_new
;;; ----------------------------------------------------------------------------

(declaim (inline image-new))

(defun image-new ()
 #+liber-documentation
 "@version{#2022-7-20}
  @return{A newly created @class{gtk:image} widget.}
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
 "@version{2023-1-29}
  @argument[path]{a pathname or namestring with the name of the file}
  @return{A new @class{gtk:image} widget.}
  @begin{short}
    Creates a new image displaying the file @arg{path}.
  @end{short}
  If the file is not found or cannot be loaded, the resulting @class{gtk:image}
  widget will display a \"broken image\" icon. This function never returns
  @code{nil}, it always returns a valid @class{gtk:image} widget.

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
 "@version{#2021-12-17}
  @argument[resource]{a string with a resource path}
  @return{A new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying the resource file in @arg{resource}.
  @end{short}

  If the file is not found or can not be loaded, the resulting @class{gtk:image}
  widget will display a \"broken image\" icon. This function always returns a
  valid @class{gtk:image} widget.

  If the file contains an animation, the image will contain an animation.

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

(cffi:defcfun ("gtk_image_new_from_pixbuf" image-new-from-pixbuf)
    (g:object image)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @return{A new @class{gtk:image} widget.}
  @begin{short}
    Creates an image displaying @arg{pixbuf}.
  @end{short}

  Note that this function just creates an image from the pixbuf. The image
  created will not react to state changes. Should you want that, you should use
  the @fun{gtk:image-new-from-icon-name} function.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-new-from-icon-name}"
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'image-new-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_paintable ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_paintable" image-new-from-paintable)
    (g:object image)
 #+liber-documentation
 "@version{#2022-7-20}
  @argument[paintable]{a @class{gdk:paintable} object}
  @return{A new @class{gtk:image} widget.}
  @begin{short}
    Creates a new image displaying the paintable.
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
 "@version{#2022-7-20}
  @argument[name]{a string with an icon name}
  @return{A new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:image}
  @see-function{gtk:image-set-from-icon-name}"
  (name :string))

(export 'image-new-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_new_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_new_from_gicon" image-new-from-gicon)
    (g:object image)
 #+liber-documentation
 "@version{#2022-7-20}
  @argument[icon]{a @class{g:icon} object}
  @return{A new @class{gtk:image} widget displaying the themed icon.}
  @begin{short}
    Creates an image displaying an icon from the current icon theme.
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
 "@version{#2021-12-22}
  @argument[image]{a @class{gtk:image} widget}
  @short{Resets the image to be empty.}
  @see-class{gtk:image}"
  (image (g:object image)))

(export 'image-clear)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_file
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_file" image-set-from-file) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[filename]{a string with a filename}
  @begin{short}
    Sets the image to display @arg{filename}.
  @end{short}
  See the @fun{gtk:image-new-from-file} function for details.
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-file}"
  (image (g:object image))
  (filename :string))

(export 'image-set-from-file)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_resource
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_resource" image-set-from-resource) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[resource]{a string with a resource path}
  @begin{short}
    Sets the image to display @arg{resource}.
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

(cffi:defcfun ("gtk_image_set_from_pixbuf" image-set-from-pixbuf) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[pixbuf]{a @class{gdk-pixbuf:pixbuf} object}
  @begin{short}
    Sets the image to display @arg{pixbuf}.
  @end{short}
  See the @fun{gtk:image-new-from-pixbuf} function for more details.
  @see-class{gtk:image}
  @see-class{gdk-pixbuf:pixbuf}
  @see-function{gtk:image-new-from-pixbuf}"
  (image (g:object image))
  (pixbuf (g:object gdk-pixbuf:pixbuf)))

(export 'image-set-from-pixbuf)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_paintable ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_paintable" image-set-from-paintable) :void
 #+liber-documentation
 "@version{#2022-7-20}
  @argument[image]{a @class{gtk:image} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Sets the image to display @arg{paintable}.
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
 "@version{#2021-12-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[icon-name]{a string with an icon name}
  @begin{short}
    Sets the image to display @arg{icon-name}.
  @end{short}
  See the @fun{gtk:image-new-from-icon-name} function for details.
  @see-class{gtk:image}
  @see-function{gtk:image-new-from-icon-name}"
  (image (g:object image))
  (icon-name :string)
  (icon-size icon-size))

(export 'image-set-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_image_set_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_image_set_from_gicon" image-set-from-gicon) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[image]{a @class{gtk:image} widget}
  @argument[icon]{a @class{g:icon} icon}
  @argument[size]{a value of the @symbol{gtk:icon-size} enumeration}
  @begin{short}
    Sets the image to display @arg{icon}.
  @end{short}
  See the @fun{gtk:image-new-from-gicon} function for details.
  @see-class{gtk:image}
  @see-class{g:icon}
  @see-symbol{gtk:icon-size}
  @see-function{gtk:image-new-from-gicon}"
  (image (g:object image))
  (icon (g:object g:icon))
  (size icon-size))

(export 'image-set-from-gicon)

;;; --- End of file gtk4.image.lisp --------------------------------------------
