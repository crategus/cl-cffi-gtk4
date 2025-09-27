(in-package :gtk-test)

(def-suite gdk-toplevel :in gdk-suite)
(in-suite gdk-toplevel)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkToplevelState

(test gdk-toplevel-state
  ;; Check type
  (is (g:type-is-flags "GdkToplevelState"))
  ;; Check registered name
  (is (eq 'gdk:toplevel-state
          (glib:symbol-for-gtype "GdkToplevelState")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkToplevelState")
          (g:gtype (cffi:foreign-funcall "gdk_toplevel_state_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_TOPLEVEL_STATE_MINIMIZED" "GDK_TOPLEVEL_STATE_MAXIMIZED"
               "GDK_TOPLEVEL_STATE_STICKY" "GDK_TOPLEVEL_STATE_FULLSCREEN"
               "GDK_TOPLEVEL_STATE_ABOVE" "GDK_TOPLEVEL_STATE_BELOW"
               "GDK_TOPLEVEL_STATE_FOCUSED" "GDK_TOPLEVEL_STATE_TILED"
               "GDK_TOPLEVEL_STATE_TOP_TILED" "GDK_TOPLEVEL_STATE_TOP_RESIZABLE"
               "GDK_TOPLEVEL_STATE_RIGHT_TILED"
               "GDK_TOPLEVEL_STATE_RIGHT_RESIZABLE"
               "GDK_TOPLEVEL_STATE_BOTTOM_TILED"
               "GDK_TOPLEVEL_STATE_BOTTOM_RESIZABLE"
               "GDK_TOPLEVEL_STATE_LEFT_TILED"
               "GDK_TOPLEVEL_STATE_LEFT_RESIZABLE"
               "GDK_TOPLEVEL_STATE_SUSPENDED")
             (glib-test:list-flags-item-names "GdkToplevelState")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768
               65536)
             (glib-test:list-flags-item-values "GdkToplevelState")))
  ;; Check nick names
  (is (equal '("minimized" "maximized" "sticky" "fullscreen" "above" "below"
               "focused" "tiled" "top-tiled" "top-resizable" "right-tiled"
               "right-resizable" "bottom-tiled" "bottom-resizable" "left-tiled"
               "left-resizable" "suspended")
             (glib-test:list-flags-item-nicks "GdkToplevelState")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkToplevelState" GDK:TOPLEVEL-STATE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_toplevel_state_get_type")
                                     (:MINIMIZED 1)
                                     (:MAXIMIZED 2)
                                     (:STICKY 4)
                                     (:FULLSCREEN 8)
                                     (:ABOVE 16)
                                     (:BELOW 32)
                                     (:FOCUSED 64)
                                     (:TILED 128)
                                     (:TOP-TILED 256)
                                     (:TOP-RESIZABLE 512)
                                     (:RIGHT-TILED 1024)
                                     (:RIGHT-RESIZABLE 2048)
                                     (:BOTTOM-TILED 4096)
                                     (:BOTTOM-RESIZABLE 8192)
                                     (:LEFT-TILED 16384)
                                     (:LEFT-RESIZABLE 32768)
                                     (:SUSPENDED 65536))
             (gobject:get-gtype-definition "GdkToplevelState"))))

;;;     GdkFullscreenMode

(test gdk-fullscreen-mode
  ;; Check type
  (is (g:type-is-enum "GdkFullscreenMode"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkFullscreenMode")
          (g:gtype (cffi:foreign-funcall "gdk_fullscreen_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:fullscreen-mode
          (glib:symbol-for-gtype "GdkFullscreenMode")))
  ;; Check names
  (is (equal '("GDK_FULLSCREEN_ON_CURRENT_MONITOR"
               "GDK_FULLSCREEN_ON_ALL_MONITORS")
             (glib-test:list-enum-item-names "GdkFullscreenMode")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-enum-item-values "GdkFullscreenMode")))
  ;; Check nick names
  (is (equal '("current-monitor" "all-monitors")
             (glib-test:list-enum-item-nicks "GdkFullscreenMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkFullscreenMode" GDK:FULLSCREEN-MODE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_fullscreen_mode_get_type")
                                    (:CURRENT-MONITOR 0)
                                    (:ALL-MONITORS 1))
             (gobject:get-gtype-definition "GdkFullscreenMode"))))

;;;     GdkSurfaceEdge

(test gdk-surface-edge
  ;; Check type
  (is (g:type-is-enum "GdkSurfaceEdge"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSurfaceEdge")
          (g:gtype (cffi:foreign-funcall "gdk_surface_edge_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:surface-edge
          (glib:symbol-for-gtype "GdkSurfaceEdge")))
  ;; Check names
  (is (equal '("GDK_SURFACE_EDGE_NORTH_WEST" "GDK_SURFACE_EDGE_NORTH"
               "GDK_SURFACE_EDGE_NORTH_EAST" "GDK_SURFACE_EDGE_WEST"
               "GDK_SURFACE_EDGE_EAST" "GDK_SURFACE_EDGE_SOUTH_WEST"
               "GDK_SURFACE_EDGE_SOUTH" "GDK_SURFACE_EDGE_SOUTH_EAST")
             (glib-test:list-enum-item-names "GdkSurfaceEdge")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GdkSurfaceEdge")))
  ;; Check nick names
  (is (equal '("north-west" "north" "north-east" "west" "east" "south-west"
               "south" "south-east")
             (glib-test:list-enum-item-nicks "GdkSurfaceEdge")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkSurfaceEdge" GDK:SURFACE-EDGE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_surface_edge_get_type")
                                    (:NORTH-WEST 0)
                                    (:NORTH 1)
                                    (:NORTH-EAST 2)
                                    (:WEST 3)
                                    (:EAST 4)
                                    (:SOUTH-WEST 5)
                                    (:SOUTH 6)
                                    (:SOUTH-EAST 7))
             (gobject:get-gtype-definition "GdkSurfaceEdge"))))

;;;     GdkTitlebarGesture                                 Since 4.4

(test gdk-titlebar-gesture
  ;; Check type
  (is (g:type-is-enum "GdkTitlebarGesture"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkTitlebarGesture")
          (g:gtype (cffi:foreign-funcall "gdk_titlebar_gesture_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:titlebar-gesture
          (glib:symbol-for-gtype "GdkTitlebarGesture")))
  ;; Check names
  (is (equal '("GDK_TITLEBAR_GESTURE_DOUBLE_CLICK"
               "GDK_TITLEBAR_GESTURE_RIGHT_CLICK"
               "GDK_TITLEBAR_GESTURE_MIDDLE_CLICK")
             (glib-test:list-enum-item-names "GdkTitlebarGesture")))
  ;; Check values
  (is (equal '(1 2 3)
             (glib-test:list-enum-item-values "GdkTitlebarGesture")))
  ;; Check nick names
  (is (equal '("double-click" "right-click" "middle-click")
             (glib-test:list-enum-item-nicks "GdkTitlebarGesture")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkTitlebarGesture" GDK:TITLEBAR-GESTURE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_titlebar_gesture_get_type")
                                    (:DOUBLE-CLICK 1)
                                    (:RIGHT-CLICK 2)
                                    (:MIDDLE-CLICK 3))
             (gobject:get-gtype-definition "GdkTitlebarGesture"))))

;;;     GdkToplevel

(test gdk-toplevel-interface
  ;; Check type
  (is (g:type-is-interface "GdkToplevel"))
  ;; Check registered name
  (is (eq 'gdk:toplevel
          (glib:symbol-for-gtype "GdkToplevel")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkToplevel")
          (g:gtype (cffi:foreign-funcall "gdk_toplevel_get_type" :size))))
  ;; Check interface Prerequisites
  (is (equal '("GdkSurface")
             (glib-test:list-interface-prerequisites "GdkToplevel")))
  ;; Check interface properties
  (is (equal '("decorated" "deletable" "fullscreen-mode" "icon-list" "modal"
               "shortcuts-inhibited" "startup-id" "state" "title"
               "transient-for")
             (glib-test:list-interface-properties "GdkToplevel")))
  ;; Check signals
  (is (equal '("compute-size")
             (glib-test:list-signals "GdkToplevel")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GdkToplevel" GDK:TOPLEVEL
                            (:EXPORT T
                             :TYPE-INITIALIZER "gdk_toplevel_get_type")
                            (DECORATED TOPLEVEL-DECORATED
                             "decorated" "gboolean" T T)
                            (DELETABLE TOPLEVEL-DELETABLE
                             "deletable" "gboolean" T T)
                            (FULLSCREEN-MODE TOPLEVEL-FULLSCREEN-MODE
                             "fullscreen-mode" "GdkFullscreenMode" T T)
                            (ICON-LIST TOPLEVEL-ICON-LIST
                             "icon-list" "gpointer" T T)
                            (MODAL TOPLEVEL-MODAL "modal" "gboolean" T T)
                            (SHORTCUTS-INHIBITED TOPLEVEL-SHORTCUTS-INHIBITED
                             "shortcuts-inhibited" "gboolean" T NIL)
                            (STARTUP-ID TOPLEVEL-STARTUP-ID
                             "startup-id" "gchararray" T T)
                            (STATE TOPLEVEL-STATE
                             "state" "GdkToplevelState" T NIL)
                            (TITLE TOPLEVEL-TITLE "title" "gchararray" T T)
                            (TRANSIENT-FOR TOPLEVEL-TRANSIENT-FOR
                             "transient-for" "GdkSurface" T T))
             (gobject:get-gtype-definition "GdkToplevel"))))

;;; ----------------------------------------------------------------------------

#+windows
(gobject:define-gobject "GdkWin32Toplevel" win32-toplevel
  (:superclass g:object
   :export t
   :interfaces ("GdkToplevel")
   :type-initializer "gdk_display_get_type")
  nil)

#+windows
(test gdk-win32-toplevel-class
  ;; Check type
  (is (g:type-is-object "GdkWin32Toplevel"))
  ;; Check registered name
  (is (eq 'g:object
          (glib:symbol-for-gtype "GdkWin32Toplevel")))
  ;; Check type initializer
  #+nil ; not exported
  (is (eq (g:gtype "GObject")
          (g:gtype (cffi:foreign-funcall "gdk_win32_toplevel_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GdkWin32Surface")
          (g:type-parent "GdkWin32Toplevel")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkWin32Toplevel")))
  ;; Check interfaces
  (is (equal '("GdkToplevel")
             (glib-test:list-interfaces "GdkWin32Toplevel")))
  ;; Check properties
  (is (equal '("decorated" "deletable" "fullscreen-mode" "icon-list" "modal"
               "shortcuts-inhibited" "startup-id" "state" "title"
               "transient-for")
             (glib-test:list-properties "GdkWin32Toplevel")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkWin32Toplevel")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkWin32Toplevel" WIN32-TOPLEVEL
                      (:SUPERCLASS GDK-WIN32-SURFACE
                       :EXPORT T
                       :INTERFACES ("GdkToplevel"))
                      NIL)
             (gobject:get-gtype-definition "GdkWin32Toplevel"))))

;;; --- Properties -------------------------------------------------------------

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

;;; --- Signals ----------------------------------------------------------------

;;;     compute-size

;;; --- Functions --------------------------------------------------------------

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

;;; 2025-09-25
