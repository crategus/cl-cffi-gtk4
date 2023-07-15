;;; ----------------------------------------------------------------------------
;;; gtk4.icon-theme.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkIconTheme
;;;
;;;     Looking up icons by name
;;;
;;; Types and Values
;;;
;;;     GtkIconTheme
;;;     GtkIconLookupFlags
;;;
;;;     GTK_ICON_THEME_ERROR
;;;     GTK_TYPE_ICON_THEME_ERROR
;;;     GTK_TYPE_ICON_LOOKUP_FLAGS
;;;
;;;     GtkIconThemeError
;;;
;;; Accessors
;;;
;;;     gtk_icon_theme_get_display
;;;     gtk_icon_theme_get_icon_names
;;;     gtk_icon_theme_set_resource_path
;;;     gtk_icon_theme_get_resource_path
;;;     gtk_icon_theme_set_search_path
;;;     gtk_icon_theme_get_search_path
;;;     gtk_icon_theme_set_theme_name
;;;     gtk_icon_theme_get_theme_name
;;;
;;; Functions
;;;
;;;     gtk_icon_theme_new
;;;     gtk_icon_theme_get_for_display
;;;     gtk_icon_theme_add_search_path
;;;     gtk_icon_theme_add_resource_path
;;;     gtk_icon_theme_has_icon
;;;     gtk_icon_theme_lookup_icon
;;;     gtk_icon_theme_lookup_by_gicon
;;;     gtk_icon_theme_get_icon_sizes
;;;
;;; Properties
;;;
;;;     display
;;;     icon-names
;;;     resource-path
;;;     search-path
;;;     theme-name
;;;
;;; Signals
;;;
;;;     changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkIconTheme
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkIconLookupFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkIconLookupFlags" icon-lookup-flags
  (:export t
   :type-initializer "gtk_icon_lookup_flags_get_type")
  (:none 0)
  (:force-regular  #.(ash 1 0))
  (:force-symbolic #.(ash 1 1))
  (:preload        #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'icon-lookup-flags)
      "GFlags"
      (liber:symbol-documentation 'icon-lookup-flags)
 "@version{#2022-7-2}
  @begin{short}
    Used to specify options for the @fun{gtk:icon-theme-lookup-icon} function.
  @end{short}
  @begin{pre}
(gobject:define-g-flags \"GtkIconLookupFlags\" icon-lookup-flags
  (:export t
   :type-initializer \"gtk_icon_lookup_flags_get_type\")
  (:force-regular  #.(ash 1 0))
  (:force-symbolic #.(ash 1 1))
  (:preload        #.(ash 1 2)))
   @end{pre}
  @begin[code]{table}
    @entry[:force-regular]{Try to always load regular icons, even when symbolic
      icon names are given.}
    @entry[:force-symbolic]{Try to always load symbolic icons, even when regular
      icon names are given.}
    @entry[:preload]{Starts loading the texture in the background so it is ready
      when later needed.}
  @end{table}
  @see-class{gtk:icon-theme}")

;;; ----------------------------------------------------------------------------
;;; GTK_ICON_THEME_ERROR
;;;
;;; #define GTK_ICON_THEME_ERROR gtk_icon_theme_error_quark ()
;;;
;;; The GQuark used for GtkIconThemeError errors.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_TYPE_ICON_THEME_ERROR
;;;
;;; #define GTK_TYPE_ICON_THEME_ERROR (gtk_icon_theme_error_get_type ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_TYPE_ICON_LOOKUP_FLAGS
;;;
;;; #define GTK_TYPE_ICON_LOOKUP_FLAGS (gtk_icon_lookup_flags_get_type ())
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GtkIconThemeError
;;;
;;; Error codes for GtkIconTheme operations.
;;;
;;;
;;; GTK_ICON_THEME_NOT_FOUND :
;;;     The icon specified does not exist in the theme
;;;
;;; GTK_ICON_THEME_FAILED :
;;;     An unspecified error occurred.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct GtkIconTheme
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkIconTheme" icon-theme
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_icon_theme_get_type")
  ((display
    icon-theme-display
    "display" "GdkDisplay" t t)
   (icon-names
    icon-theme-icon-names
    "icon-names" "GStrv" t nil)
   (resource-path
    icon-theme-resource-path
    "resource-path" "GStrv" t t)
   (search-path
    icon-theme-search-path
    "search-path" "GStrv" t t)
   (theme-name
    icon-theme-theme-name
    "theme-name" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'icon-theme 'type)
 "@version{#2022-7-3}
  @begin{short}
    The @sym{gtk:icon-theme} class provides a facility for looking up icons by
    name and size.
  @end{short}
  The main reason for using a name rather than simply providing a filename is
  to allow different icons to be used depending on what icon theme is selected
  by the user. The operation of icon themes on Linux and Unix follows the Icon
  Theme Specification. There is a default icon theme, named Hicolor where
  applications should install their icons, but more additional application
  themes can be installed as operating system vendors and users choose.

  In many cases, named themes are used indirectly, via the @class{gtk:image}
  widget rather than directly, but looking up icons directly is also simple. The
  @sym{gtk:icon-theme} object acts as a database of all the icons in the current
  theme. You can create new @sym{gtk:icon-theme} objects, but it is much more
  efficient to use the standard icon theme of the @class{gtk:widget} widget so
  that the icon information is shared with other people looking up icons.
  @begin[Example]{dictionary}
    In the case where the default screen is being used, looking up an icon can
    be as simple as:
    @begin{pre}
(let* ((theme (gtk:icon-theme-for-display (gdk:display-default)))
       (paintable (gtk:icon-theme-lookup-icon theme
                                              \"gtk-ok\"    ; icon name
                                              nil         ; fallbacks
                                              48          ; size
                                              1           ; scale
                                              :none)))    ; no flags
   ... )
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (theme)    :run-last
      @end{pre}
      Emitted when the current icon theme is switched or GTK detects that a
      change has occurred in the contents of the current icon theme.
      @begin[code]{table}
        @entry[theme]{The @sym{gtk:icon-theme} object.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:icon-theme-display}
  @see-slot{gtk:icon-theme-icon-names}
  @see-slot{gtk:icon-theme-resource-path}
  @see-slot{gtk:icon-theme-search-path}
  @see-slot{gtk:icon-theme-theme-name}
  @see-class{gtk:icon-paintable}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessors Details
;;; ----------------------------------------------------------------------------

;;; --- icon-theme-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'icon-theme) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The display that the icon theme object is attached to.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-theme-display)
      "Accessor"
      (documentation 'icon-theme-display 'function)
 "@version{#2022-7-3}
  @syntax[]{(gtk:icon-theme-display object) => display}
  @syntax[]{(setf (gtk:icon-theme-display object) display)}
  @argument[object]{a @class{gtk:icon-theme} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gtk:icon-theme]{display} slot of the
    @class{gtk:icon-theme} class.
  @end{short}

  The @sym{gtk:icon-theme-display} function returns the display that the
  @class{gtk:icon-theme} object was created for. The
  @sym{(setf gtk:icon-theme-display)} function sets the display.
  @see-class{gtk:icon-theme}
  @see-class{gdk:display}")

;;; --- icon-theme-icon-names --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-names" 'icon-theme) t)
 "The @code{icon-names} property of type @type{glib:strv-t} (Read) @br{}
  The icon names that are supported by the icon theme.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-theme-icon-names)
      "Accessor"
      (documentation 'icon-theme-icon-names 'function)
 "@version{#2022-7-3}
  @syntax[]{(gtk:icon-theme-icon-names object) => names}
  @argument[object]{a @class{gtk:icon-theme} object}
  @argument[names]{a list of strings with the icon names}
  @begin{short}
    Accessor of the @slot[gtk:icon-theme]{icon-names} slot of the
    @class{gtk:icon-theme} class.
  @end{short}

  The @sym{gtk:icon-theme-icon-names} function lists the names of icons in the
  current icon theme.
  @see-class{gtk:icon-theme}")

;;; --- icon-theme-resource-path -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resource-path" 'icon-theme) t)
 "The @code{resource-path} property of type @type{glib:strv-t} (Read / Write)
  @br{}
  Resource paths that will be looked at when looking for icons, similar to
  search paths. The resources are considered as part of the hicolor icon theme
  and must be located in subdirectories that are defined in the hicolor icon
  theme, such as @file{@@path/16x16/actions/run.png}. Icons that are directly
  placed in the resource path instead of a subdirectory are also considered as
  ultimate fallback.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-theme-resource-path)
      "Accessor"
      (documentation 'icon-theme-resource-path 'function)
 "@version{#2022-7-3}
  @syntax[]{(gtk:icon-theme-resource-path object) => path}
  @syntax[]{(setf (gtk:icon-theme-resource-path object) path}
  @argument[object]{a @class{gtk:icon-theme} object}
  @argument[path]{a list of strings with the resource paths}
  @begin{short}
    Accessor of the @slot[gtk:icon-theme]{resource-path} slot of the
    @class{gtk:icon-theme} class.
  @end{short}

  The @sym{gtk:icon-theme-resource-path} function gets the current resource
  paths. The @sym{(setf gtk:icon-theme-resource-path)} sets the resource paths
  that will be looked at when looking for icons, similar to search paths.

  The resources are considered as part of the hicolor icon theme and must be
  located in subdirectories that are defined in the hicolor icon theme, such as
  @file{@@path/16x16/actions/run.png} or @file{@@path/scalable/actions/run.svg}.

  Icons that are directly placed in the resource path instead of a subdirectory
  are also considered as ultimate fallback, but they are treated like unthemed
  icons.
  @see-class{gtk:icon-theme}")

;;; --- icon-theme-search-path -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "search-path" 'icon-theme) t)
 "The @code{search-path} property of type @type{glib:strv-t} (Read / Write)
  @br{}
  The search path for this icon theme. When looking for icons, GTK will search
  for a subdirectory of one or more of the directories in the search path with
  the same name as the icon theme containing an @file{index.theme} file.
  Themes from multiple of the path elements are combined to allow themes to be
  extended by adding icons in the home directory of the user.")

#+liber-documentation
(setf (liber:alias-for-function 'icon-theme-search-path)
      "Accessor"
      (documentation 'icon-theme-search-path 'function)
 "@version{#2022-7-3}
  @syntax[]{(gtk:icon-theme-search-path object) => path}
  @syntax[]{(setf (gtk:icon-theme-search-path object) path}
  @argument[object]{a @class{gtk:icon-theme} object}
  @argument[path]{a list of strings with the search paths}
  @begin{short}
    Accessor of the @slot[gtk:icon-theme]{search-path} slot of the
    @class{gtk:icon-theme} class.
  @end{short}

  The @sym{gtk:icon-theme-search-path} function gets the current search path.
  The @sym{(setf gtk:icon-theme-resource-path)} function sets the search path
  for the icon theme object. When looking for an icon theme, GTK will search for
  a subdirectory of one or more of the directories in path with the same name as
  the icon theme containing an @file{index.theme} file. Themes from multiple of
  the path elements are combined to allow themes to be extended by adding icons
  in the home directory of the user.

  In addition if an icon found is not found either in the current icon theme or
  the default icon theme, and an image file with the right name is found
  directly in one of the elements of @arg{path}, then that image will be used
  for the icon name. This is legacy feature, and new icons should be put into
  the fallback icon theme, which is called hicolor, rather than directly on the
  icon path.
  @see-class{gtk:icon-theme}")

;;; --- icon-theme-theme-name --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "theme-name" 'icon-theme) t)
 "The @code{theme-name} property of type @code{:string} (Read / Write) @br{}
  The name of the icon theme that is being used. Unless set to a different
  value, this will be the value of the @slot[gtk:settings]{gtk-icon-theme-name}
  property of the @class{gtk:settings} object associated to the display of the
  icon theme object. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'icon-theme-theme-name)
      "Accessor"
      (documentation 'icon-theme-theme-name 'function)
 "@version{#2022-7-3}
  @syntax[]{(gtk:icon-theme-theme-name object) => name}
  @syntax[]{(setf (gtk:icon-theme-theme-name object) name}
  @argument[object]{a @class{gtk:icon-theme} object}
  @argument[name]{a string with the current icon theme name}
  @begin{short}
    Accessor of the @slot[gtk:icon-theme]{theme-name} slot of the
    @class{gtk:icon-theme} class.
  @end{short}

  The @sym{gtk:icon-theme-theme-name} function gets the current icon theme name.
  The @sym{(setf gtk:icon-theme-resource-path)} function sets the name of the
  icon theme that the @class{gtk:icon-theme} object uses overriding system
  configuration. This function cannot be called on the icon theme objects
  returned from the @fun{gtk:icon-theme-for-display} function.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-for-default}")

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_new ()
;;; ----------------------------------------------------------------------------

(defun icon-theme-new ()
 #+liber-documentation
 "@version{#2022-7-3}
  @return{The newly created @class{gtk:icon-theme} object.}
  @begin{short}
    Creates a new icon theme object.
  @end{short}
  Icon theme objects are used to lookup up an icon by name in a particular icon
  theme. Usually, you will want to use the @fun{gtk:icon-theme-for-display}
  function rather than creating a new icon theme object for scratch.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-for-display}"
  (make-instance 'icon-theme))

(export 'icon-theme-new)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_for_display () -> icon-theme-for-display
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_get_for_display" icon-theme-for-display)
    (g:object icon-theme)
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[display]{a @class{gdk:display} object}
  @return{A unique @class{gtk:icon-theme} object associated with the given
    display. This icon theme is associated with the display and can be used as
    long as the display is open.}
  @begin{short}
    Gets the icon theme object associated with @arg{display}.
  @end{short}
  If this function has not previously been called for the given display, a new
  icon theme object will be created and associated with the display. Icon theme
  objects are fairly expensive to create, so using this function is usually a
  better choice than calling than the @fun{gtk:icon-theme-new} function and
  setting the display yourself. By using this function a single icon theme
  object will be shared between users.
  @see-class{gtk:icon-theme}
  @see-class{gdk:display}
  @see-class{gtk:icon-theme-new}"
  (display (g:object gdk:display)))

(export 'icon-theme-for-display)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_search_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_add_search_path" icon-theme-add-search-path) 
    :void
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a string with the directory name to append to the icon path}
  @begin{short}
    Appends a directory to the search path.
  @end{short}
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-search-path}"
  (theme (g:object icon-theme))
  (path :string))

(export 'icon-theme-add-search-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_add_resource_path ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_add_resource_path"
               icon-theme-add-resource-path) :void
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[path]{a string with a resource path}
  @begin{short}
    Adds a resource path that will be looked at when looking for icons, similar
    to search paths.
  @end{short}
  This function should be used to make application specific icons available as
  part of the icon theme.
  @see-class{gtk:icon-theme}
  @see-function{gtk:icon-theme-search-path}
  @see-function{gtk:icon-theme-resource-path}"
  (theme (g:object icon-theme))
  (path :string))

(export 'icon-theme-add-resource-path)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_has_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_has_icon" icon-theme-has-icon) :boolean
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string with the name of an icon}
  @return{@em{True} if the icon theme includes an icon for @arg{name}.}
  @begin{short}
    Checks whether an icon theme includes an icon for a particular name.
  @end{short}
  @see-class{gtk:icon-theme}"
  (theme (g:object icon-theme))
  (name :string))

(export 'icon-theme-has-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_lookup_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_icon" icon-theme-lookup-icon)
    (g:object icon-paintable)
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string with the name of an icon}
  @argument[fallbacks]{a list of strings with fallback icons}
  @argument[size]{an integer with the desired icon size}
  @argument[scale]{an integer with the window scale this will be displayed on}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration}
  @argument[flags]{a value of the @symbol{gtk:icon-lookup-flags} flags}
  @return{A @class{gtk:icon-paintable} object containing the icon.}
  @begin{short}
    Looks up a named icon for a desired size and window scale, returning a
    @class{gtk:icon-paintable} object.
  @end{short}
  The icon can then be rendered by using it as a @class{gdk:paintable} object,
  or you can get information such as the filename and size.

  If the icon name is not available and fallbacks are provided, they will be
  tried in order.

  If no matching icon is found, then a paintable that renders the \"missing
  icon\" icon is returned. If you need to do something else for missing icons
  you need to use the @fun{gtk:icon-theme-has-icon} function.

  Note that you probably want to listen for icon theme changes and update the
  icon.
  @see-class{gtk:icon-theme}
  @see-class{gtk:icon-paintable}
  @see-class{gdk:paintable}
  @see-symbol{gtk:text-direction}
  @see-symbol{gtk:icon-lookup-flags}"
  (theme (g:object icon-theme))
  (name :string)
  (fallbacks glib:strv-t)
  (size :int)
  (scale :int)
  (direction text-direction)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-icon)

;;; ----------------------------------------------------------------------------
;;;gtk_icon_theme_lookup_by_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_icon_theme_lookup_by_gicon" icon-theme-lookup-by-gicon)
    (g:object icon-paintable)
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[icon]{a @class{g:icon} object to look up}
  @argument[size]{an integer with the desired icon size}
  @argument[scale]{an integer with the desired scale}
  @argument[direction]{a value of the @symbol{gtk:text-direction} enumeration}
  @argument[flags]{a value of the @symbol{gtk:icon-lookup-flags} flags}
  @return{A @class{gtk:icon-paintable} object containing the icon.}
  @begin{short}
    Looks up an icon for a desired size and window scale, returning a
    @class{gtk:icon-paintable} object.
  @end{short}
  The icon can then be rendered by using it as a @class{gdk:paintable} object,
  or you can get information such as the filename and size.
  @see-class{gtk:icon-theme}
  @see-class{gtk:icon-paintable}
  @see-class{gdk:paintable}
  @see-symbol{gtk:text-direction}
  @see-symbol{gtk:icon-lookup-flags}"
  (theme (g:object icon-theme))
  (icon (g:object g:icon))
  (size :int)
  (scale :int)
  (direction text-direction)
  (flags icon-lookup-flags))

(export 'icon-theme-lookup-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_icon_theme_get_icon_sizes () -> icon-theme-icon-sizes
;;; ----------------------------------------------------------------------------

;; TODO: Check this implementation. Return a list of integer?!

(cffi:defcfun ("gtk_icon_theme_get_icon_sizes" %icon-theme-icon-sizes) :pointer
  (theme (g:object icon-theme))
  (name :string))

(defun icon-theme-icon-sizes (theme name)
 #+liber-documentation
 "@version{#2022-7-3}
  @argument[theme]{a @class{gtk:icon-theme} object}
  @argument[name]{a string with the name of an icon}
  @return{A Lisp array of integer with the sizes at which the icon is
    available.}
  @begin{short}
    Returns an array of integers describing the sizes at which the icon is
    available without scaling.
  @end{short}
  A size of -1 means that the icon is available in a scalable format.
  @see-class{gtk:icon-theme}"
  (let ((ptr (%icon-theme-icon-sizes theme name)))
    (cffi:foreign-array-to-lisp ptr '(:array :int 1 ) :adjustable t)))

(export 'icon-theme-icon-sizes)

;;; --- End of file gtk.icon-theme.lisp ----------------------------------------
