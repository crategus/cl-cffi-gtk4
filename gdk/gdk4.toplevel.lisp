;;; ----------------------------------------------------------------------------
;;; gdk4.toplevel.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
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
;;;
;;; Types and Values
;;;
;;;     GdkToplevelState
;;;     GdkFullScreenMode
;;;     GdkSurfaceEdge
;;;     GdkTitlebarGesture                                 Since 4.4
;;;
;;;     GdkToplevel
;;;
;;; Accessors
;;;
;;;     gdk_toplevel_set_decorated
;;;     gdk_toplevel_set_deletable
;;;     gdk_toplevel_set_icon_list
;;;     gdk_toplevel_set_modal
;;;     gdk_toplevel_set_startup_id
;;;     gdk_toplevel_get_state
;;;     gdk_toplevel_set_title
;;;     gdk_toplevel_set_transient_for
;;;
;;; Functions
;;;
;;;     gdk_toplevel_present
;;;     gdk_toplevel_minimize
;;;     gdk_toplevel_lower
;;;     gdk_toplevel_focus
;;;     gdk_toplevel_show_window_menu
;;;     gdk_toplevel_supports_edge_constraints
;;;     gdk_toplevel_inhibit_system_shortcuts
;;;     gdk_toplevel_restore_system_shortcuts
;;;     gdk_toplevel_begin_resize
;;;     gdk_toplevel_begin_move
;;;     gdk_toplevel_titlebar_gesture                      Since 4.4
;;;
;;; Properties
;;;
;;;     decorated
;;;     deletable
;;;     fullscreen-mode
;;;     icon-list
;;;     modal
;;;     shortcuts-inhibited
;;;     startup-id
;;;     state
;;;     title
;;;     transient-for
;;;
;;; Signals
;;;
;;;     compute-size
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkToplevel
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkToplevelState
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GdkToplevelState" toplevel-state
  (:export t
   :type-initializer "gdk_toplevel_state_get_type")
  (:minimized #.(ash 1 0))
  (:maximized #.(ash 1 1))
  (:sticky #.(ash 1 2))
  (:fullscreen #.(ash 1 3))
  (:above #.(ash 1 4))
  (:below #.(ash 1 5))
  (:focused #.(ash 1 6))
  (:tiled #.(ash 1 7))
  (:top-tiled #.(ash 1 8))
  (:top-resizable #.(ash 1 9))
  (:right-tiled #.(ash 1 10))
  (:right-resizable #.(ash 1 11))
  (:bottom-tiled #.(ash 1 12))
  (:bottom-resizable #.(ash 1 13))
  (:left-tiled #.(ash 1 14))
  (:left-resizable #.(ash 1 15)))

#+liber-documentation
(setf (liber:alias-for-symbol 'toplevel-state)
      "GFlags"
      (liber:symbol-documentation 'toplevel-state)
 "@version{2024-7-12}
  @begin{declaration}
(gobject:define-gflags \"GdkToplevelState\" toplevel-state
  (:export t
   :type-initializer \"gdk_toplevel_state_get_type\")
  (:minimized #.(ash 1 0))
  (:maximized #.(ash 1 1))
  (:sticky #.(ash 1 2))
  (:fullscreen #.(ash 1 3))
  (:above #.(ash 1 4))
  (:below #.(ash 1 5))
  (:focused #.(ash 1 6))
  (:tiled #.(ash 1 7))
  (:top-tiled #.(ash 1 8))
  (:top-resizable #.(ash 1 9))
  (:right-tiled #.(ash 1 10))
  (:right-resizable #.(ash 1 11))
  (:bottom-tiled #.(ash 1 12))
  (:bottom-resizable #.(ash 1 13))
  (:left-tiled #.(ash 1 14))
  (:left-resizable #.(ash 1 15)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:minimized]{The surface is minimized.}
      @entry[:maximized]{The surface is maximized.}
      @entry[:sticky]{The surface is sticky.}
      @entry[:fullscreen]{The surface is maximized without decorations.}
      @entry[:above]{The surface is kept above other surfaces.}
      @entry[:below]{The surface is kept below other surfaces.}
      @entry[:focused]{The surface is presented as focused (with active
        decorations).}
      @entry[:tiled]{The surface is in a tiled state.}
      @entry[:top-tiled]{Whether the top edge is tiled.}
      @entry[:top-resizable]{Whether the top edge is resizable.}
      @entry[:right-tiled]{Whether the right edge is tiled.}
      @entry[:right-resizable]{Whether the right edge is resizable.}
      @entry[:bottom-tiled]{Whether the bottom edge is tiled.}
      @entry[:bottom-resizable]{Whether the bottom edge is resizable.}
      @entry[:left-tiled]{Whether the left edge is tiled.}
      @entry[:left-resizable]{Whether the left edge is resizable.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies the state of a toplevel surface.
  @end{short}
  On platforms that support information about individual edges, the
  @code{:state-tiled} state will be set whenever any of the individual tiled
  states is set. On platforms that lack that support, the tiled state will give
  an indication of tiledness without any of the per-edge states being set.
  @see-class{gdk:toplevel}")

;;; ----------------------------------------------------------------------------
;;; GdkFullscreenMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkFullscreenMode" fullscreen-mode
  (:export t
   :type-initializer "gdk_fullscreen_mode_get_type")
  (:on-current-monitor 0)
  (:on-all-monitor 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'fullscreen-mode)
      "GEnum"
      (liber:symbol-documentation 'fullscreen-mode)
 "@version{2024-7-12}
  @begin{declaration}
(gobject:define-genum \"GdkFullscreenMode\" fullscreen-mode
  (:export t
   :type-initializer \"gdk_fullscreen_mode_get_type\")
  (:on-current-monitor 0)
  (:on-all-monitor 1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:on-current-monitor]{Fullscreen on current monitor only.}
      @entry[:on-all-monitor]{Span across all monitors when fullscreen.}
    @end{table}
  @end{values}
  @begin{short}
    Indicates which monitor (in a multi-head setup) a surface should span over
    when in fullscreen mode.
  @end{short}
  @see-class{gdk:toplevel}")

;;; ----------------------------------------------------------------------------
;;; GdkSurfaceEdge
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkSurfaceEdge" surface-edge
  (:export t
   :type-initializer "gdk_surface_edge_get_type")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'surface-edge)
      "GEnum"
      (liber:symbol-documentation 'surface-edge)
 "@version{2024-7-12}
  @begin{declaration}
(gobject:define-genum \"GdkSurfaceEdge\" surface-edge
  (:export t
   :type-initializer \"gdk_surface_edge_get_type\")
  (:north-west 0)
  (:north 1)
  (:north-east 2)
  (:west 3)
  (:east 4)
  (:south-west 5)
  (:south 6)
  (:south-east 7))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:north-west]{The top left corner.}
      @entry[:north]{The top edge.}
      @entry[:north-east]{The top right corner.}
      @entry[:west]{The left edge.}
      @entry[:east]{The right edge.}
      @entry[:south-west]{The lower left corner.}
      @entry[:south]{The lower edge.}
      @entry[:south-east]{The lower right corner.}
    @end{table}
  @end{values}
  @begin{short}
    Determines a surface edge or corner.
  @end{short}
  @see-class{gdk:toplevel}")

;;; ----------------------------------------------------------------------------
;;; GdkTitlebarGesture
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(gobject:define-genum "GdkTitlebarGesture" titlebar-gesture
  (:export t
   :type-initializer "gdk_titlebar_gesture_get_type")
  (:double-click 0)
  (:right-click 1)
  (:middle-click 2))

#+(and gtk-4-4 liber-documentation)
(setf (liber:alias-for-symbol 'titlebar-gesture)
      "GEnum"
      (liber:symbol-documentation 'titlebar-gesture)
 "@version{2024-7-12}
  @begin{declaration}
(gobject:define-genum \"GdkTitlebarGesture\" titlebar-gesture
  (:export t
   :type-initializer \"gdk_titlebar_gesture_get_type\")
  (:double-click 0)
  (:right-click 1)
  (:middle-click 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:double-click]{No description available.}
      @entry[:right-click]{No description available.}
      @entry[:middle-click]{No description available.}
    @end{table}
  @end{values}
  @begin{short}
    The kind of title bar gesture to emit with the
    @fun{gdk:toplevel-titlebar-gesture} function.
  @end{short}

  Since 4.4
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-titlebar-gesture}")

;;; ----------------------------------------------------------------------------
;;; GdkToplevel
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GdkToplevel" toplevel
  (:superclass g:object
   :export t
   :type-initializer "gdk_toplevel_get_type")
  ((decorated
    toplevel-decorated
    "decorated" "gboolean" t t)
   (deletable
    toplevel-deletable
    "deletable" "gboolean" t t)
   (fullscreen-mode
    toplevel-fullscreen-mode
    "fullscreen-mode" "GdkFullscreenMode" t t)
   (icon-list
    toplevel-icon-list
    "icon-list" "gpointer" t t)
   (modal
    toplevel-modal
    "modal" "gboolean" t t)
   (shortcuts-inhibited
    toplevel-shortcuts-inhibited
    "shortcuts-inhibited" "gboolean" t nil)
   (startup-id
    toplevel-startup-id
    "startup-id" "gchararray" t t)
   (state
    toplevel-state
    "state" "GdkToplevelState" t nil)
   (title
    toplevel-title
    "title" "gchararray" t t)
   (transient-for
    toplevel-transient-for
    "transient-for" "GdkSurface" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'toplevel)
      "Interface"
      (documentation 'toplevel 'type)
 "@version{2014-4-7}
  @begin{short}
    The @class{gdk:toplevel} object is a freestanding toplevel surface.
  @end{short}
  The @class{gdk:toplevel} interface provides useful APIs for interacting with
  the windowing system, such as controlling maximization and size of the
  surface, setting icons and transient parents for dialogs.
  @begin[Signal Details]{dictionary}
    @subheading{The \"compute-size\" signal}
      @begin{pre}
lambda (toplevel size)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[toplevel]{The @class{gdk:toplevel} object.}
        @entry[size]{The @symbol{gdk:toplevel-size} instance.}
      @end{table}
      Compute the desired size of the toplevel, given the information passed
      via the @symbol{gdk:toplevel-size} instance. It will normally be emitted
      during or after the @fun{gdk:toplevel-present} function, depending on the
      configuration received by the windowing system. It may also be emitted at
      any other point in time, in response to the windowing system spontaneously
      changing the configuration. It is the responsibility of the toplevel user
      to handle this signal. Failing to do so will result in an arbitrary size
      being used as a result.
  @end{dictionary}
  @see-slot{gdk:toplevel-decorated}
  @see-slot{gdk:toplevel-deletable}
  @see-slot{gdk:toplevel-fullscreen-mode}
  @see-slot{gdk:toplevel-icon-list}
  @see-slot{gdk:toplevel-modal}
  @see-slot{gdk:toplevel-shortcuts-inhibited}
  @see-slot{gdk:toplevel-startup-id}
  @see-slot{gdk:toplevel-state}
  @see-slot{gdk:toplevel-title}
  @see-slot{gdk:toplevel-transient-for}
  @see-class{gdk:surface}
  @see-class{gdk:popup}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:toplevel-decorated -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "decorated" 'toplevel) t)
 "The @code{decorated} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toplevel is decorated. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-decorated)
      "Accessor"
      (documentation 'toplevel-decorated 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-decorated object) => decorated}
  @syntax{(setf (gdk:toplevel-decorated object) decorated)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[decorated]{a boolean whether to request decorations}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{decorated} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  Setting @arg{decorated} to @em{false} hints the desktop environment that the
  surface has its own, client-side decorations and does not need to have window
  decorations added.
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-deletable -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "deletable" 'toplevel) t)
 "The @code{deletable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toplevel is deletable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-deletable)
      "Accessor"
      (documentation 'toplevel-deletable 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-deletable object) => deletable}
  @syntax{(setf (gdk:toplevel-deletable object) deletable)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[deletable]{a boolean whether to request a delete button}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{deletable} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  Setting @arg{deletable} to @em{true} hints the desktop environment that it
  should offer the user a way to close the surface.
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-fullscreen-mode -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fullscreen-mode" 'toplevel) t)
 "The @code{fullscreen-mode} property of type @symbol{gdk:fullscreen-mode}
  (Read / Write) @br{}
  The fullscreen mode of the toplevel. @br{}
  Default value: @code{:on-current-monitor}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-fullscreen-mode)
      "Accessor"
      (documentation 'toplevel-fullscreen-mode 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-fullscreen-mode object) => mode}
  @syntax{(setf (gdk:toplevel-fullscreen-mode object) mode)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[mode]{a boolean with the fullscreen mode of the surface}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{fullscreen-mode} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-icon-list -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-list" 'toplevel) t)
 "The @code{icon-list} property of type @code{:pointer} (Read / Write) @br{}
  The list of icon textures.")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-icon-list)
      "Accessor"
      (documentation 'toplevel-icon-list 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-icon-list object) => surfaces}
  @syntax{(setf (gdk:toplevel-icon-list object) surfaces)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[surfaces]{a list of textures to use as icon, of different sizes}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{icon-list} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  The @setf{gdk:toplevel-icon-list} function sets a list of icons for the
  surface. One of these will be used to represent the surface in iconic form.
  The icon may be shown in window lists or task bars. Which icon size is shown
  depends on the window manager. The window manager can scale the icon but
  setting several size icons can give better image quality.

  Note that some platforms do not support surface icons.
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-modal -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'toplevel) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toplevel is modal. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-modal)
      "Accessor"
      (documentation 'toplevel-modal 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-modal object) => modal}
  @syntax{(setf (gdk:toplevel-modal object) modal)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[modal]{@em{true} if the surface is modal, @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{modal} slot of the @class{gdk:toplevel}
    class.
  @end{short}
  The application can use this hint to tell the window manager that a certain
  surface has modal behaviour. The window manager can use this information to
  handle modal surfaces in a special way.

  You should only use this on surfaces for which you have previously called
  the @fun{gdk:toplevel-transient-for} function.
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-transient-for}")

;;; --- gdk:toplevel-shortcuts-inhibited ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "shortcuts-inhibited"
                                               'toplevel) t)
 "The @code{shortcuts-inhibited} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether keyboard shortcuts are inhibited. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-shortcuts-inhibited)
      "Accessor"
      (documentation 'toplevel-shortcuts-inhibited 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-shortcuts-inhibited object) => inhibited}
  @syntax{(setf (gdk:toplevel-shortcuts-inhibited object) inhibited)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[inhibited]{a boolean whether keyboard shortcuts are inhibited}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{shortcuts-inhibited} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-startup-id ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "startup-id" 'toplevel) t)
 "The @code{startup-id} property of type @code{:string} (Read / Write) @br{}
  The startup ID of the surface. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-startup-id)
      "Accessor"
      (documentation 'toplevel-startup-id 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-startup-id object) => startup-id}
  @syntax{(setf (gdk:toplevel-startup-id object) startup-id)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[startup-id]{a string with startup-notification identifier}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{startup-id} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  When using GTK, typically you should use the @fun{gtk:window-startup-id}
  function instead of this low-level function.
  @see-class{gdk:toplevel}
  @see-function{gtk:window-startup-id}")

;;; --- gdk:toplevel-state -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'toplevel) t)
 "The @code{state} property of type @symbol{gdk:toplevel-state} (Read) @br{}
  The state of the toplevel.")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-state)
      "Accessor"
      (documentation 'toplevel-state 'function)
 "@version{#2024-4-7}
  @syntax{(gdk:toplevel-state object) => state}
  @syntax{(setf (gdk:toplevel-state object) state)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[state]{a @symbol{gdk:toplevel-state} value}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{state} slot of the @class{gdk:toplevel}
    class.
  @end{short}
  Gets the bitwise OR of the currently active surface state flags, from the
  @symbol{gdk:toplevel-state} enumeration.
  @see-class{gdk:toplevel}
  @see-symbol{gdk:toplevel-state}")

;;; --- gdk:toplevel-title -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'toplevel) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the surface. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-title)
      "Accessor"
      (documentation 'toplevel-title 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-title object) => title}
  @syntax{(setf (gdk:toplevel-title object) title)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[state]{a string with the title of surface}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{state} slot of the @class{gdk:toplevel}
    class.
  @end{short}
  Sets the title of a toplevel surface, to be displayed in the titlebar, in
  lists of windows, etc.
  @see-class{gdk:toplevel}")

;;; --- gdk:toplevel-transient-for ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transient-for" 'toplevel) t)
 "The @code{transient-for} property of type @class{gdk:surface} (Read / Write)
  @br{}
  The transient parent of the surface.")

#+liber-documentation
(setf (liber:alias-for-function 'toplevel-transient-for)
      "Accessor"
      (documentation 'toplevel-transient-for 'function)
 "@version{#2023-4-8}
  @syntax{(gdk:toplevel-transient-for object) => parent}
  @syntax{(setf (gdk:toplevel-transient-for object) parent)}
  @argument[object]{a @class{gdk:toplevel} object}
  @argument[parent]{a toplevel @class{gdk:surface} object}
  @begin{short}
    Accessor of the @slot[gdk:toplevel]{transient-for} slot of the
    @class{gdk:toplevel} class.
  @end{short}
  Indicates to the window manager that surface is a transient dialog associated
  with the application surface @arg{parent}. This allows the window manager to
  do things like center surface on parent and keep surface above @arg{parent}.

  See the @fun{gtk:window-transient-for} function if you are using
  @class{gtk:window} or @class{gtk:dialog} widgets.
  @see-class{gdk:toplevel}
  @see-class{gtk:window}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_present ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_present" toplevel-present) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object to show}
  @argument[layout]{a @class{gdk:toplevel-layout} instance used to layout}
  @begin{short}
    Present @arg{toplevel} after having processed the
    @class{gtk:toplevel-layout} rules.
  @end{short}
  If the toplevel was previously not showing, it will be showed, otherwise it
  will change layout according to @arg{layout}.

  GDK may emit the @code{\"compute-size\"} signal to let the user of this
  toplevel compute the preferred size of the toplevel surface. See
  @code{\"compute-size\"} signal for details.

  Presenting is asynchronous and the specified layout parameters are not
  guaranteed to be respected.
  @see-class{gdk:toplevel-present}
  @see-class{gdk:toplevel-layout}"
  (toplevel (g:object toplevel))
  (layout (g:boxed toplevel-layout)))

(export 'toplevel-present)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_minimize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_minimize" toplevel-minimize) :boolean
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @return{@em{True} if the surface was minimized.}
  @begin{short}
    Asks to minimize the toplevel.
  @end{short}
  The windowing system may choose to ignore the request.
  @see-class{gdk:toplevel}"
  (toplevel (g:object toplevel)))

(export 'toplevel-minimize)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_lower ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_lower" toplevel-lower) :boolean
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @return{@em{True} if the surface was lowered.}
  @begin{short}
    Asks to lower the toplevel below other windows.
  @end{short}
  The windowing system may choose to ignore the request.
  @see-class{gdk:toplevel}"
  (toplevel (g:object toplevel)))

(export 'toplevel-lower)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_focus ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_focus" toplevel-focus) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @argument[timestamp]{an unsigned integer with the timestamp of the event
    triggering the surface focus}
  @begin{short}
    Sets keyboard focus to surface .
  @end{short}
  In most cases, the @fun{gtk:window-present-with-time} function should be used
  on a @class{gtk:window} widget, rather than calling this function.
  @see-class{gdk:toplevel}
  @see-class{gtk:window}
  @see-function{gtk:window-present-with-time}"
  (toplevel (g:object toplevel))
  (timestamp :uint))

(export 'toplevel-focus)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_show_window_menu ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_show_window_menu" toplevel-show-window-menu)
    :boolean
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @argument[event]{a @class{gdk:event} instance to show the menu for}
  @return{@em{True} if the window menu was shown and @em{false} otherwise.}
  @begin{short}
    Asks the windowing system to show the window menu.
  @end{short}
  The window menu is the menu shown when right-clicking the titlebar on
  traditional windows managed by the window manager. This is useful for windows
  using client-side decorations, activating it with a right-click on the window
  decorations.
  @see-class{gdk:toplevel}
  @see-class{gdk:event}"
  (toplevel (g:object toplevel))
  (event event))

(export 'toplevel-show-window-menu)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_supports_edge_constraints ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_supports_edge_constraints"
               toplevel-supports-edge-constraints) :boolean
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @return{@em{True} if the desktop environment supports tiled window states.}
  @begin{short}
    Returns whether the desktop environment supports tiled window states.
  @end{short}
  @see-class{gdk:toplevel}"
  (toplevel (g:object toplevel)))

(export 'toplevel-supports-edge-constraints)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_inhibit_system_shortcuts ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_inhibit_system_shortcuts"
               toplevel-inhibit-system-shortcuts) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object requesting system keyboard
    shortcuts}
  @argument[event]{a @class{gdk:event} instance that is triggering the inhibit
    request, or @code{nil} if none is available}
  @begin{short}
    Requests that the toplevel inhibit the system shortcuts, asking the desktop
    environment/windowing system to let all keyboard events reach the surface,
    as long as it is focused, instead of triggering system actions.
  @end{short}

  If granted, the rerouting remains active until the default shortcuts
  processing is restored with the @fun{gdk:toplevel-restore-system-shortcuts}
  function, or the request is revoked by the desktop environment, windowing
  system or the user.

  A typical use case for this API is remote desktop or virtual machine viewers
  which need to inhibit the default system keyboard shortcuts so that the
  remote session or virtual host gets those instead of the local environment.

  The windowing system or desktop environment may ask the user to grant or deny
  the request or even choose to ignore the request entirely.

  The caller can be notified whenever the request is granted or revoked by
  listening to the @slot[gdk:toplevel]{shortcuts-inhibited} property.
  @see-class{gdk:toplevel}
  @see-class{gdk:event}
  @see-function{gdk:toplevel-restore-system-shortcuts}
  @see-function{gdk:toplevel-shortcuts-inhibited}"
  (toplevel (g:object toplevel))
  (event event))

(export 'toplevel-inhibit-system-shortcuts)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_restore_system_shortcuts ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_restore_system_shortcuts"
               toplevel-restore-system-shortcuts) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @begin{short}
    Restore default system keyboard shortcuts which were previously requested
    to be inhibited by the @fun{gdk:toplevel-inhibit-system-shortcuts}
    function.
  @end{short}
  @see-class{gdk:toplevel}
  @see-function{gdk:toplevel-inhibit-system-shortcuts}"
  (toplevel (g:object toplevel)))

(export 'toplevel-restore-system-shortcuts)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_begin_resize ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_begin_resize" toplevel-begin-resize) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @argument[edge]{a @symbol{gdk:surface-edge} value with the edge or corner
    from which the drag is started}
  @argument[device]{a @class{gdk:device} object used for the operation}
  @argument[button]{an integer with the button being used to drag, or 0 for a
    keyboard-initiated drag}
  @argument[x]{a double float with the surface x coordinate of mouse click that
    began the drag}
  @argument[y]{a double float with the surface y coordinate of mouse click that
    began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag, use the @fun{gdk:event-time} function.}
  @begin{short}
    Begins an interactive resize operation, for a toplevel surface.
  @end{short}
  You might use this function to implement a window resize grip.
  @see-class{gdk:toplevel}
  @see-class{gdk:device}
  @see-symbol{gdk:surface-edge}
  @see-function{gdk:event-time}"
  (toplevel (g:object toplevel))
  (edge surface-edge)
  (device (g:object device))
  (button :int)
  (x :double)
  (y :double)
  (timestamp :uint32))

(export 'toplevel-begin-resize)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_begin_move ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_toplevel_begin_move" toplevel-begin-move) :void
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @argument[device]{a @class{gdk:device} object used for the operation}
  @argument[button]{an integer with the button being used to drag, or 0 for a
    keyboard-initiated drag}
  @argument[x]{a double float with the surface x coordinate of mouse click that
    began the drag}
  @argument[y]{a double float with the surface y coordinate of mouse click that
    began the drag}
  @argument[timestamp]{an unsigned integer with the timestamp of mouse click
    that began the drag}
  @begin{short}
    Begins an interactive move operation, for a toplevel surface.
  @end{short}
  You might use this function to implement draggable titlebars.
  @see-class{gdk:toplevel}
  @see-class{gdk:device}
  @see-function{gdk:event-time}"
  (toplevel (g:object toplevel))
  (device (g:object device))
  (button :int)
  (x :double)
  (y :double)
  (timestamp :uint32))

(export 'toplevel-begin-move)

;;; ----------------------------------------------------------------------------
;;; gdk_toplevel_titlebar_gesture                          Since 4.4
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gdk_toplevel_titlebar_gesture" toplevel-titlebar-gesture)
    :boolean
 #+liber-documentation
 "@version{#2023-8-1}
  @argument[toplevel]{a @class{gdk:toplevel} object}
  @argument[gesture]{a @symbol{gdk:titlebar-gesture} value}
  @return{The boolean value.}
  @begin{short}
    No description available.
  @end{short}

  Since 4.4
  @see-class{gdk:toplevel}
  @see-symbol{gdk:titlebar-gesture}"
  (toplevel (g:object toplevel))
  (gesture titlebar-gesture))

#+gtk-4-4
(export 'toplevel-titlebar-gesture)

;;; --- End of file gdk4.toplevel.lisp -----------------------------------------
