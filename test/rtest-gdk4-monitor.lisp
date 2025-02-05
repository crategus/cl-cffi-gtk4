(in-package :gtk-test)

(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSubpixelLayout

(test gdk-subpixel-layout
  ;; Check type
  (is (g:type-is-enum "GdkSubpixelLayout"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkSubpixelLayout")
          (g:gtype (cffi:foreign-funcall "gdk_subpixel_layout_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:subpixel-layout
          (glib:symbol-for-gtype "GdkSubpixelLayout")))
  ;; Check names
  (is (equal '("GDK_SUBPIXEL_LAYOUT_UNKNOWN" "GDK_SUBPIXEL_LAYOUT_NONE"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR")
             (glib-test:list-enum-item-names "GdkSubpixelLayout")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5)
             (glib-test:list-enum-item-values "GdkSubpixelLayout")))
  ;; Check nick names
  (is (equal '("unknown" "none" "horizontal-rgb" "horizontal-bgr" "vertical-rgb"
               "vertical-bgr")
             (glib-test:list-enum-item-nicks "GdkSubpixelLayout")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkSubpixelLayout" GDK:SUBPIXEL-LAYOUT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_subpixel_layout_get_type")
                                    (:UNKNOWN 0)
                                    (:NONE 1)
                                    (:HORIZONTAL-RGB 2)
                                    (:HORIZONTAL-BGR 3)
                                    (:VERTICAL-RGB 4)
                                    (:VERTICAL-BGR 5))
             (gobject:get-gtype-definition "GdkSubpixelLayout"))))

;;;     GdkMonitor

(test gdk-monitor-class
  ;; Check type
  (is (g:type-is-object "GdkMonitor"))
  ;; Check registered name
  (is (eq 'gdk:monitor
          (glib:symbol-for-gtype "GdkMonitor")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkMonitor")
          (g:gtype (cffi:foreign-funcall "gdk_monitor_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkMonitor")))
  ;; Check children
  #-windows
  (is (member "GdkWaylandMonitor"
              (glib-test:list-children "GdkMonitor") :test #'string=))
  #+windows
  (is (equal '("GdkWin32Monitor")
             (glib-test:list-children "GdkMonitor")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkMonitor")))
  ;; Check properties
  (is (equal '("connector" "description" "display" "geometry" "height-mm"
               "manufacturer" "model" "refresh-rate" "scale" "scale-factor"
               "subpixel-layout" "valid" "width-mm")
             (glib-test:list-properties "GdkMonitor")))
  ;; Check signals
  (is (equal '("invalidate")
             (glib-test:list-signals "GdkMonitor")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkMonitor" GDK:MONITOR
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_monitor_get_type")
                       ((CONNECTOR MONITOR-CONNECTOR
                         "connector" "gchararray" T NIL)
                        (DESCRIPTION MONITOR-DESCRIPTION
                         "description" "gchararray" T NIL)
                        (DISPLAY MONITOR-DISPLAY
                         "display" "GdkDisplay" T NIL)
                        (GEOMETRY MONITOR-GEOMETRY
                         "geometry" "GdkRectangle" T NIL)
                        (HEIGHT-MM MONITOR-HEIGHT-MM
                         "height-mm" "gint" T NIL)
                        (MANUFACTURER MONITOR-MANUFACTURER
                         "manufacturer" "gchararray" T NIL)
                        (MODEL MONITOR-MODEL "model" "gchararray" T NIL)
                        (REFRESH-RATE MONITOR-REFRESH-RATE
                         "refresh-rate" "gint" T NIL)
                        (SCALE MONITOR-SCALE "scale" "gdouble" T NIL)
                        (SCALE-FACTOR MONITOR-SCALE-FACTOR
                         "scale-factor" "gint" T NIL)
                        (SUBPIXEL-LAYOUT MONITOR-SUBPIXEL-LAYOUT
                         "subpixel-layout" "GdkSubpixelLayout" T NIL)
                        (VALID MONITOR-VALID "valid" "gboolean" T NIL)
                        (WIDTH-MM MONITOR-WIDTH-MM "width-mm" "gint" T NIL)))
             (gobject:get-gtype-definition "GdkMonitor"))))

;;; --- Signals ----------------------------------------------------------------

;;;     invalidate

(test gdk-monitor-invalidate-signal
  (let ((query (g:signal-query (g:signal-lookup "invalidate" "GdkMonitor"))))
    (is (string= "invalidate" (g:signal-query-signal-name query)))
    (is (string= "GdkMonitor" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-FIRST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Properties -------------------------------------------------------------

(test gdk-monitor-properties
  (glib-test:with-check-memory (:strong 4)
    (let ((monitor (first (gdk:display-monitors (gdk:display-default)))))
      (is (g:type-is-a (g:type-from-instance monitor) "GdkMonitor"))
      #-windows
      (is (stringp (gdk:monitor-connector monitor)))
      #+windows
      (is-false (gdk:monitor-connector monitor))
      #+crategus
      (is (or (string= "Iiyama North America 24\""
                       (gdk:monitor-description monitor))
              (string= "Iiyama North America 27\""
                       (gdk:monitor-description monitor))
              (string= "Eingebaute Anzeige"
                       (gdk:monitor-description monitor))))
      #+windows
      (is-false (gdk:monitor-description monitor))
      (is (typep (gdk:monitor-display monitor) 'gdk:display))
      (is (typep (gdk:monitor-geometry monitor) 'gdk:rectangle))
      (is (integerp (gdk:monitor-height-mm monitor)))
      (is (stringp (gdk:monitor-manufacturer monitor)))
      (is (stringp (gdk:monitor-model monitor)))
      (is (integerp (gdk:monitor-refresh-rate monitor)))
      (is (integerp (gdk:monitor-scale-factor monitor)))
      ;; TODO:Implement general test for a enum value
      (is (eq :unknown (gdk:monitor-subpixel-layout monitor)))
      (is-true (gdk:monitor-valid monitor))
      (is (integerp (gdk:monitor-width-mm monitor))))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_monitor_is_valid

(test gdk-monitor-is-valid
  (glib-test:with-check-memory (:strong 4)
    (let ((monitor (first (gdk:display-monitors (gdk:display-default)))))
      (is-true (gdk:monitor-is-valid monitor)))))

;;; 2025-2-1
