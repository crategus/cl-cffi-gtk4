(in-package :gtk-test)

(def-suite gdk-surface :in gdk-suite)
(in-suite gdk-surface)

;;; --- Type and Values --------------------------------------------------------

;;;     GdkSurface

(test gdk-surface-class
  ;; Type check
  (is (g:type-is-object "GdkSurface"))
  ;; Check the registered name
  (is (eq 'gdk:surface
          (glib:symbol-for-gtype "GdkSurface")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkSurface")
          (g:gtype (cffi:foreign-funcall "gdk_surface_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSurface")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkSurface")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkSurface")))
  ;; Check the class properties
  (is (equal '("cursor" "display" "frame-clock" "height" "mapped" "scale-factor"
               "width")
             (list-properties "GdkSurface")))
  ;; Check the list of signals
  (is (equal '("enter-monitor" "event" "layout" "leave-monitor" "render")
             (list-signals "GdkSurface")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkSurface" GDK-SURFACE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_surface_get_type")
                       ((CURSOR GDK-SURFACE-CURSOR "cursor" "GdkCursor" T T)
                        (DISPLAY GDK-SURFACE-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (FRAME-CLOCK GDK-SURFACE-FRAME-CLOCK "frame-clock"
                         "GdkFrameClock" T NIL)
                        (HEIGHT GDK-SURFACE-HEIGHT "height" "gint" T NIL)
                        (MAPPED GDK-SURFACE-MAPPED "mapped" "gboolean" T NIL)
                        (SCALE-FACTOR GDK-SURFACE-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (WIDTH GDK-SURFACE-WIDTH "width" "gint" T NIL)))
             (gobject:get-g-type-definition "GdkSurface"))))

;;; --- Properties -------------------------------------------------------------

;;;     cursor
;;;     display
;;;     frame-clock
;;;     height
;;;     mapped
;;;     scale-factor
;;;     width

;;; --- Signals ----------------------------------------------------------------

;;;     enter-monitor
;;;     event
;;;     layout
;;;     leave-monitor
;;;     render

;;; --- Functions --------------------------------------------------------------

;;;     gdk_surface_new_popup
;;;     gdk_surface_new_toplevel

;;;     gdk_surface_beep
;;;     gdk_surface_create_cairo_context
;;;     gdk_surface_create_gl_context
;;;     gdk_surface_create_similar_surface
;;;     gdk_surface_create_vulkan_context
;;;     gdk_surface_destroy
;;;     gdk_surface_get_device_cursor
;;;     gdk_surface_get_device_position
;;;     gdk_surface_hide
;;;     gdk_surface_is_destroyed
;;;     gdk_surface_queue_render
;;;     gdk_surface_request_layout
;;;     gdk_surface_set_device_cursor
;;;     gdk_surface_set_input_region
;;;     gdk_surface_set_opaque_region
;;;     gdk_surface_translate_coordinates

;;; --- 2023-5-29 --------------------------------------------------------------
