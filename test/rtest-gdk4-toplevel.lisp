(in-package :gtk-test)

(def-suite gdk-toplevel :in gdk-suite)
(in-suite gdk-toplevel)

;;; Types and Values
;;;
;;;     GdkToplevelState
;;;     GdkFullScreenMode
;;;     GdkSurfaceEdge
;;;     GdkTitlebarGesture                                 Since 4.4

;;;     GdkToplevel

(test gdk-toplevel-interface
  ;; Type check
  (is (g:type-is-interface "GdkToplevel"))
  ;; Check the registered name
  (is (eq 'gdk:toplevel
          (glib:symbol-for-gtype "GdkToplevel")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkToplevel")
          (g:gtype (cffi:foreign-funcall "gdk_toplevel_get_type" :size))))
  ;; Check the interface Prerequisites
  (is (equal '("GdkSurface")
             (list-interface-prerequisites "GdkToplevel")))
  ;; Check the interface properties
  (is (equal '("decorated" "deletable" "fullscreen-mode" "icon-list" "modal"
 "shortcuts-inhibited" "startup-id" "state" "title" "transient-for")
             (list-interface-properties "GdkToplevel")))
  ;; Check the signals
  (is (equal '("compute-size")
             (list-signals "GdkToplevel")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkToplevel" GDK-TOPLEVEL
                            (:EXPORT T :TYPE-INITIALIZER
                             "gdk_toplevel_get_type")
                            (DECORATED GDK-TOPLEVEL-DECORATED "decorated"
                             "gboolean" T T)
                            (DELETABLE GDK-TOPLEVEL-DELETABLE "deletable"
                             "gboolean" T T)
                            (FULLSCREEN-MODE GDK-TOPLEVEL-FULLSCREEN-MODE
                             "fullscreen-mode" "GdkFullscreenMode" T T)
                            (ICON-LIST GDK-TOPLEVEL-ICON-LIST "icon-list"
                             "gpointer" T T)
                            (MODAL GDK-TOPLEVEL-MODAL "modal" "gboolean" T T)
                            (SHORTCUTS-INHIBITED
                             GDK-TOPLEVEL-SHORTCUTS-INHIBITED
                             "shortcuts-inhibited" "gboolean" T NIL)
                            (STARTUP-ID GDK-TOPLEVEL-STARTUP-ID "startup-id"
                             "gchararray" T T)
                            (STATE GDK-TOPLEVEL-STATE "state"
                             "GdkToplevelState" T NIL)
                            (TITLE GDK-TOPLEVEL-TITLE "title" "gchararray" T T)
                            (TRANSIENT-FOR GDK-TOPLEVEL-TRANSIENT-FOR
                             "transient-for" "GdkSurface" T T))
             (gobject:get-g-type-definition "GdkToplevel"))))

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

;;; --- 2023-7-30 --------------------------------------------------------------
