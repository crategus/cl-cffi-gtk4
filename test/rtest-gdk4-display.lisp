(in-package :gtk-test)

(def-suite gdk-display :in gdk-suite)
(in-suite gdk-display)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDisplay

(test gdk-display-class
  ;; Type check
  (is (g:type-is-object "GdkDisplay"))
  ;; Check the registered name
  (is (eq 'gdk:display
          (gobject:symbol-for-gtype "GdkDisplay")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDisplay")
          (g:gtype (cffi:foreign-funcall "gdk_display_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDisplay")))
  ;; Check the children
  (is (equal '("GdkBroadwayDisplay" "GdkWaylandDisplay" "GdkX11Display")
             (list-children "GdkDisplay")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDisplay")))
  ;; Check the properties
  (is (equal '("composited" "input-shapes" "rgba")
             (list-properties "GdkDisplay")))
  ;; Check the signals
  (is (equal '("closed" "opened" "seat-added" "seat-removed" "setting-changed")
             (list-signals "GdkDisplay")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDisplay" GDK-DISPLAY
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_display_get_type")
                       ((COMPOSITED GDK-DISPLAY-COMPOSITED "composited"
                         "gboolean" T NIL)
                        (INPUT-SHAPES GDK-DISPLAY-INPUT-SHAPES "input-shapes"
                         "gboolean" T NIL)
                        (RGBA GDK-DISPLAY-RGBA "rgba" "gboolean" T NIL)))
             (gobject:get-g-type-definition "GdkDisplay"))))

;;; --- Properties -------------------------------------------------------------

;;;     composited
;;;     input-shapes
;;;     rgba

;;; --- Signals ----------------------------------------------------------------

;;;     closed
;;;     opened
;;;     seat-added
;;;     seat-removed
;;;     setting-changed

;;; --- Functions --------------------------------------------------------------

;;;     gdk_display_open
;;;     gdk_display_get_default
;;;     gdk_display_get_name
;;;     gdk_display_device_is_grabbed
;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_close
;;;     gdk_display_is_closed
;;;     gdk_display_is_rgba
;;;     gdk_display_is_composited
;;;     gdk_display_supports_input_shapes
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete                Since 4.10 deprecated
;;;     gdk_display_get_default_seat
;;;     gdk_display_list_seats
;;;     gdk_display_get_monitors
;;;     gdk_display_get_monitor_at_surface
;;;     gdk_display_get_clipboard
;;;     gdk_display_get_primary_clipboard
;;;     gdk_display_get_setting
;;;     gdk_display_get_startup_notification_id            Since 4.10 deprecated
;;;     gdk_display_put_event                              Since 4.10 deprecated
;;;     gdk_display_map_keyval
;;;     gdk_display_map_keycode
;;;     gdk_display_translate_key
;;;     gdk_display_prepare_gl                             Since 4.4
;;;     gdk_display_create_gl_context                      Since 4.6

;;; --- 2023-4-15 --------------------------------------------------------------
