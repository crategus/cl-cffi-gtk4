(in-package :gtk-test)

(def-suite gdk-surface :in gdk-suite)
(in-suite gdk-surface)

(defvar *display-default* (gdk:display-default))

;;; --- Type and Values --------------------------------------------------------

;;;     GdkSurface

(test gdk-surface-class
  ;; Check type
  (is (g:type-is-object "GdkSurface"))
  ;; Check registered name
  (is (eq 'gdk:surface
          (glib:symbol-for-gtype "GdkSurface")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSurface")
          (g:gtype (cffi:foreign-funcall "gdk_surface_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkSurface")))
  ;; Check children
  #-windows
  (is (member "GdkWaylandSurface"
              (glib-test:list-children "GdkSurface") :test #'string=))
  #+windows
  (is (equal '("GdkWin32Surface")
             (glib-test:list-children "GdkSurface")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkSurface")))
  ;; Check class properties
  (is (equal '("cursor" "display" "frame-clock" "height" "mapped" "scale"
               "scale-factor" "width")
             (glib-test:list-properties "GdkSurface")))
  ;; Check signals
  (is (equal '("enter-monitor" "event" "layout" "leave-monitor" "render")
             (glib-test:list-signals "GdkSurface")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkSurface" GDK:SURFACE
                      (:SUPERCLASS G:OBJECT
                       :EXPORT T
                       :INTERFACES NIL
                       :TYPE-INITIALIZER "gdk_surface_get_type")
                      ((CURSOR SURFACE-CURSOR "cursor" "GdkCursor" T T)
                       (DISPLAY SURFACE-DISPLAY "display" "GdkDisplay" T NIL)
                       (FRAME-CLOCK SURFACE-FRAME-CLOCK
                        "frame-clock" "GdkFrameClock" T NIL)
                       (HEIGHT SURFACE-HEIGHT "height" "gint" T NIL)
                       (MAPPED SURFACE-MAPPED "mapped" "gboolean" T NIL)
                       (SCALE SURFACE-SCALE "scale" "gdouble" T NIL)
                       (SCALE-FACTOR SURFACE-SCALE-FACTOR
                        "scale-factor" "gint" T NIL)
                       (WIDTH SURFACE-WIDTH "width" "gint" T NIL)))
             (gobject:get-gtype-definition "GdkSurface"))))

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
  (glib-test:with-check-memory (surface :strong 2)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel *display-default*))
               'gdk:surface))
    ;; Set cursor
    (is (typep (setf (gdk:surface-cursor surface)
                     (gdk:cursor-new-from-name "help")) 'gdk:cursor))
    ;; Get cursor
    (is (typep (gdk:surface-cursor surface) 'gdk:cursor))
    (is (typep (gdk:surface-display surface) 'gdk:display))
    (is (typep (gdk:surface-frame-clock surface) 'gdk:frame-clock))
    (is (= 1 (gdk:surface-height surface)))
    (is-false (gdk:surface-mapped surface))
    (is (= 1.0d0 (gdk:surface-scale surface)))
    (is (= 1 (gdk:surface-scale-factor surface)))
    (is (= 1 (gdk:surface-width surface)))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_surface_new_toplevel

(test gdk-surface-new-toplevel
  (glib-test:with-check-memory (surface :strong 1)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel (gdk:display-default)))
               'gdk:surface))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_new_popup

;; TODO: We cannot remove the references for surface. Why?

#+nil
(test gdk-surface-new-popup
  (glib-test:with-check-memory ((surface 3) popup1 popup2 :strong 2)
    (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
    (is (typep (setf popup1 (gdk:surface-new-popup surface t)) 'gdk:surface))
    (is (typep (setf popup2 (gdk:surface-new-popup surface nil)) 'gdk:surface))
    ;; Remove references
    (is-false (gdk:surface-destroy popup1))
    (is-false (gdk:surface-destroy popup2))
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_destroy
;;;     gdk_surface_is_destroyed

(test gdk-surface-destroy/is-destroyed
  (glib-test:with-check-memory (surface :strong 1)
    (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
    (is-false (gdk:surface-is-destroyed surface))
    (is-false (gdk:surface-destroy surface))
    (is-true (gdk:surface-is-destroyed surface))))

;;;     gdk_surface_hide

(test gdk-surface-hide
  (glib-test:with-check-memory (surface :strong 1)
    (setf surface (gdk:surface-new-toplevel (gdk:display-default)))
    (is-false (gdk:surface-hide surface))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_translate_coordinates

(test gdk-surface-translate-coordinates
  (glib-test:with-check-memory ((toplevel 3) from to :strong 2)
    (is (typep (setf toplevel
                     (gdk:surface-new-toplevel (gdk:display-default)))
               'gdk:surface))
    (is (typep (setf from
                     (gdk:surface-new-popup toplevel nil)) 'gdk:surface))
    (is (typep (setf to
                     (gdk:surface-new-popup toplevel nil)) 'gdk:surface))
    (is (equal '(10.0d0 10.0d0)
               (multiple-value-list
                 (gdk:surface-translate-coordinates from to 10 10))))
    ;; Remove references
    (is-false (gdk:surface-destroy from))
    (is-true (gdk:surface-is-destroyed from))
    (is-false (gdk:surface-destroy to))
    (is-true (gdk:surface-is-destroyed to))
    (is-false (gdk:surface-destroy toplevel))
    (is-true (gdk:surface-is-destroyed toplevel))))

;;;     gdk_surface_beep

(test gdk-surface-beep
  (glib-test:with-check-memory (surface :strong 1)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel (gdk:display-default)))
               'gdk:surface))
    (is-false (gdk:surface-beep surface))
    ;; Remove referenced
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_set_opaque_region

(test gdk-surface-set-opaque-region
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (surface :strong 1)
      (let ((region (cairo:region-create-rectangle 10 20 100 200)))
        (is (typep (setf surface
                         (gdk:surface-new-toplevel (gdk:display-default)))
                   'gdk:surface))
        (is-false (gdk:surface-set-opaque-region surface region))
        (is-false (gdk:surface-set-opaque-region surface nil))
        ;; Remove references
        (is-false (gdk:surface-destroy surface))))))

;;;     gdk_surface_create_gl_context

(test gdk-surface-create-gl-context
  (glib-test:with-check-memory ((surface 2) context :strong 1)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel (gdk:display-default)))
               'gdk:surface))
    (is (typep (setf context
                     (gdk:surface-create-gl-context surface)) 'gdk:gl-context))
    ;; Remove references
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_create_vulkan_context

;;;     gdk_surface_create_cairo_context

(test gdk-surface-create-cairo-context
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory ((surface 2) context :strong 1)
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (is (typep (setf context
                       (gdk:surface-create-cairo-context surface))
                 'gdk:cairo-context))
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;;     gdk_surface_create_similar_surface                 Deprecated 4.12

(test gdk-surface-create-similar-surface
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (surface :strong 1)
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (is (cffi:pointerp
              (gdk:surface-create-similar-surface surface :color 100 200)))
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;;     gdk_surface_queue_render

;; TODO: Does not emit the "render" signal !?

(test gdk-surface-queue-render
  (let (msg)
    (glib-test:with-check-memory (surface :strong 1)
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (g:signal-connect surface "render"
                        (lambda (surface region)
                          (push "render" msg)
                          (is (typep surface 'gdk:surface))
                          (is (cffi:pointerp region))
                          gdk:+event-stop+))
      (is-false (gdk:surface-queue-render surface))
      (is-false msg)
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;;     gdk_surface_request_layout

(test gdk-surface-request-layout
  (glib-test:with-check-memory (surface :strong 1)
    (is (typep (setf surface
                     (gdk:surface-new-toplevel (gdk:display-default)))
               'gdk:surface))
    (is-false (gdk:surface-request-layout surface))
    ;; Remove referenced
    (is-false (gdk:surface-destroy surface))))

;;;     gdk_surface_set_input_region

(test gdk-surface-set-input-region
  (glib-test:with-check-memory (surface :strong 1)
    (let ((region (cairo:region-create-rectangle 10 20 100 200)))
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (is-false (gdk:surface-set-input-region surface region))
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;;     gdk_surface_get_device_position

(test gdk-surface-device-position
  (glib-test:with-check-memory (surface :strong 2)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-pointer seat)))
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (is-false (gdk:surface-device-position surface device))
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;;     gdk_surface_get_device_cursor
;;;     gdk_surface_set_device_cursor

(test gdk-surface-device-position
  (glib-test:with-check-memory (surface :strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-pointer seat)))
      (is (typep (setf surface
                       (gdk:surface-new-toplevel (gdk:display-default)))
                 'gdk:surface))
      (is (typep (setf (gdk:surface-device-cursor surface device)
                       (gdk:cursor-new-from-name "help")) 'gdk:cursor))
      (is (typep (gdk:surface-device-cursor surface device) 'gdk:cursor))
      ;; Remove references
      (is-false (gdk:surface-destroy surface)))))

;;; 2025-09-24
