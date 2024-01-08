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
  (is (equal '("GdkWaylandSurface" "GdkX11Surface")
             (list-children "GdkSurface")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkSurface")))
  ;; Check the class properties
  (is (equal '("cursor" "display" "frame-clock" "height" "mapped" "scale"
               "scale-factor" "width")
             (list-properties "GdkSurface")))
  ;; Check the list of signals
  (is (equal '("enter-monitor" "event" "layout" "leave-monitor" "render")
             (list-signals "GdkSurface")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkSurface" GDK-SURFACE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "gdk_surface_get_type")
                               ((CURSOR GDK-SURFACE-CURSOR "cursor" "GdkCursor"
                                 T T)
                                (DISPLAY GDK-SURFACE-DISPLAY "display"
                                 "GdkDisplay" T NIL)
                                (FRAME-CLOCK GDK-SURFACE-FRAME-CLOCK
                                 "frame-clock" "GdkFrameClock" T NIL)
                                (HEIGHT GDK-SURFACE-HEIGHT "height" "gint" T
                                 NIL)
                                (MAPPED GDK-SURFACE-MAPPED "mapped" "gboolean"
                                 T NIL)
                                (SCALE GDK-SURFACE-SCALE "scale" "gdouble" T
                                 NIL)
                                (SCALE-FACTOR GDK-SURFACE-SCALE-FACTOR
                                 "scale-factor" "gint" T NIL)
                                (WIDTH GDK-SURFACE-WIDTH "width" "gint" T NIL)))
             (gobject:get-g-type-definition "GdkSurface"))))

;;; --- Properties -------------------------------------------------------------

;;;     cursor
;;;     display
;;;     frame-clock
;;;     height
;;;     mapped
;;;     scale                                              Since 4.12
;;;     scale-factor
;;;     width

(test gdk-surface-properties
  (let* ((surface (gdk:surface-new-toplevel (gdk:display-default))))
    ;; Access the slots
    (is-false (gdk:surface-cursor surface))
    (is (typep (gdk:surface-display surface) 'gdk:display))
    (is (typep (gdk:surface-frame-clock surface) 'gdk:frame-clock))
    (is (= 1 (gdk:surface-height surface)))
    (is-false (gdk:surface-mapped surface))
    (is (= 1.0d0 (gdk:surface-scale surface)))
    (is (= 1 (gdk:surface-scale-factor surface)))
    (is (= 1 (gdk:surface-width surface)))))

;;; --- Signals ----------------------------------------------------------------

;;;     enter-monitor

(test gdk-surface-enter-monitor-signal
  (let ((query (g:signal-query (g:signal-lookup "enter-monitor" "GdkSurface"))))
    (is (string= "enter-monitor" (g:signal-query-signal-name query)))
    (is (string= "GdkSurface" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkMonitor")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     event

(test gdk-surface-event-signal
  (let ((query (g:signal-query (g:signal-lookup "event" "GdkSurface"))))
    (is (string= "event" (g:signal-query-signal-name query)))
    (is (string= "GdkSurface" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gpointer")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     layout

(test gdk-surface-layout-signal
  (let ((query (g:signal-query (g:signal-lookup "layout" "GdkSurface"))))
    (is (string= "layout" (g:signal-query-signal-name query)))
    (is (string= "GdkSurface" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint" "gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     leave-monitor

(test gdk-surface-leave-monitor-signal
  (let ((query (g:signal-query (g:signal-lookup "leave-monitor" "GdkSurface"))))
    (is (string= "leave-monitor" (g:signal-query-signal-name query)))
    (is (string= "GdkSurface" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkMonitor")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     render

(test gdk-surface-render-signal
  (let ((query (g:signal-query (g:signal-lookup "render" "GdkSurface"))))
    (is (string= "render" (g:signal-query-signal-name query)))
    (is (string= "GdkSurface" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("CairoRegion")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_surface_new_popup

(test gdk-surface-new-popup
  (let* ((surface (gdk:surface-new-toplevel (gdk:display-default))))
    (is (typep (gdk:surface-new-popup surface t) 'gdk:surface))
    (is (typep (gdk:surface-new-popup surface nil) 'gdk:surface))))

;;;     gdk_surface_new_toplevel

(test gdk-surface-new-toplevel
  (is (typep (gdk:surface-new-toplevel (gdk:display-default)) 'gdk:surface)))

;;;     gdk_surface_destroy
;;;     gdk_surface_is_destroyed

(test gdk-surface-destroy/is-destroyed
  (let ((surface (gdk:surface-new-toplevel (gdk:display-default))))
    (is-false (gdk:surface-is-destroyed surface))
    (is-false (gdk:surface-destroy surface))
    (is-true (gdk:surface-is-destroyed surface))))

;;;     gdk_surface_hide
;;;     gdk_surface_translate_coordinates
;;;     gdk_surface_beep
;;;     gdk_surface_set_opaque_region
;;;     gdk_surface_create_gl_context
;;;     gdk_surface_create_vulkan_context
;;;     gdk_surface_create_cairo_context
;;;     gdk_surface_create_similar_surface                 Deprecated 4.12
;;;     gdk_surface_queue_render
;;;     gdk_surface_request_layout
;;;     gdk_surface_set_input_region
;;;     gdk_surface_get_device_position
;;;     gdk_surface_get_device_cursor
;;;     gdk_surface_set_device_cursor

;;; 2024-1-8
