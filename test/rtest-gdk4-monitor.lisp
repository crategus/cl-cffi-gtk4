(in-package :gtk-test)

(def-suite gdk-monitor :in gdk-suite)
(in-suite gdk-monitor)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkSubpixelLayout

(test gdk-subpixel-layout
  ;; Check the type
  (is (g:type-is-enum "GdkSubpixelLayout"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkSubpixelLayout")
          (g:gtype (cffi:foreign-funcall "gdk_subpixel_layout_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:subpixel-layout
          (glib:symbol-for-gtype "GdkSubpixelLayout")))
  ;; Check the names
  (is (equal '("GDK_SUBPIXEL_LAYOUT_UNKNOWN" "GDK_SUBPIXEL_LAYOUT_NONE"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB"
               "GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR")
             (list-enum-item-name "GdkSubpixelLayout")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5)
             (list-enum-item-value "GdkSubpixelLayout")))
  ;; Check the nick names
  (is (equal '("unknown" "none" "horizontal-rgb" "horizontal-bgr" "vertical-rgb"
               "vertical-bgr")
             (list-enum-item-nick "GdkSubpixelLayout")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkSubpixelLayout"
                             GDK-SUBPIXEL-LAYOUT
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_subpixel_layout_get_type")
                             (:UNKNOWN 0)
                             (:NONE 1)
                             (:HORIZONTAL-RGB 2)
                             (:HORIZONTAL-BGR 3)
                             (:VERTICAL-RGB 4)
                             (:VERTICAL-BGR 5))
             (gobject:get-g-type-definition "GdkSubpixelLayout"))))

;;;     GdkMonitor

(test gdk-monitor-class
  ;; Type check
  (is (g:type-is-object "GdkMonitor"))
  ;; Check the registered name
  (is (eq 'gdk:monitor
          (glib:symbol-for-gtype "GdkMonitor")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkMonitor")
          (g:gtype (cffi:foreign-funcall "gdk_monitor_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkMonitor")))
  ;; Check the children
  #-windows
  (is (equal '("GdkBroadwayMonitor" "GdkWaylandMonitor" "GdkX11Monitor")
             (list-children "GdkMonitor")))
  #+windows
  (is (equal '("GdkWin32Monitor")
             (list-children "GdkMonitor")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkMonitor")))
  ;; Check the properties
  (is (equal '("connector" "description" "display" "geometry" "height-mm"
               "manufacturer" "model" "refresh-rate" "scale-factor"
               "subpixel-layout" "valid" "width-mm")
             (list-properties "GdkMonitor")))
  ;; Check the signals
  (is (equal '("invalidate")
             (list-signals "GdkMonitor")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkMonitor" GDK-MONITOR
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_monitor_get_type")
                       ((CONNECTOR GDK-MONITOR-CONNECTOR "connector"
                         "gchararray" T NIL)
                        (DESCRIPTION GDK-MONITOR-DESCRIPTION "description"
                         "gchararray" T NIL)
                        (DISPLAY GDK-MONITOR-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (GEOMETRY GDK-MONITOR-GEOMETRY "geometry"
                         "GdkRectangle" T NIL)
                        (HEIGHT-MM GDK-MONITOR-HEIGHT-MM "height-mm" "gint" T
                         NIL)
                        (MANUFACTURER GDK-MONITOR-MANUFACTURER "manufacturer"
                         "gchararray" T NIL)
                        (MODEL GDK-MONITOR-MODEL "model" "gchararray" T NIL)
                        (REFRESH-RATE GDK-MONITOR-REFRESH-RATE "refresh-rate"
                         "gint" T NIL)
                        (SCALE-FACTOR GDK-MONITOR-SCALE-FACTOR "scale-factor"
                         "gint" T NIL)
                        (SUBPIXEL-LAYOUT GDK-MONITOR-SUBPIXEL-LAYOUT
                         "subpixel-layout" "GdkSubpixelLayout" T NIL)
                        (VALID GDK-MONITOR-VALID "valid" "gboolean" T NIL)
                        (WIDTH-MM GDK-MONITOR-WIDTH-MM "width-mm" "gint" T
                         NIL)))
             (gobject:get-g-type-definition "GdkMonitor"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-monitor-properties.1
  (let ((monitor (first (gdk:display-monitors (gdk:display-default)))))

    (is (g:type-is-a (g:type-from-instance monitor) "GdkMonitor"))
    #-windows
    (is (stringp (gdk:monitor-connector monitor)))
    #+windows
    (is-false (gdk:monitor-connector monitor))
    #-windows
    (is (stringp (gdk:monitor-description monitor)))
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
    (is (integerp (gdk:monitor-width-mm monitor)))))

#+crategus
(test gdk-monitor-properties.2
  (let ((monitor (first (gdk:display-monitors (gdk:display-default)))))
    (is (g:type-is-a (g:type-from-instance monitor) "GdkMonitor"))
    (is (string= "DP-1" (gdk:monitor-connector monitor)))
    (is (string= "Iiyama North America 24\""
                 (gdk:monitor-description monitor)))
    (is (typep (gdk:monitor-display monitor) 'gdk:display))
    (is (gdk:rectangle-equal (gdk:rectangle-new :width 1920 :height 1080)
                             (gdk:monitor-geometry monitor)))
    (is (= 300 (gdk:monitor-height-mm monitor)))
    (is (string= "IVM" (gdk:monitor-manufacturer monitor)))
    (is (string= "PL2483H" (gdk:monitor-model monitor)))
    (is (= 60000 (gdk:monitor-refresh-rate monitor)))
    (is (= 1 (gdk:monitor-scale-factor monitor)))
    (is (eq :unknown (gdk:monitor-subpixel-layout monitor)))
    (is-true (gdk:monitor-valid monitor))
    (is (= 530 (gdk:monitor-width-mm monitor)))))

#+crategus
(test gdk-monitor-properties.3
  (let ((monitor (second (gdk:display-monitors (gdk:display-default)))))
    (when monitor
      (is (g:type-is-a (g:type-from-instance monitor) "GdkMonitor"))

      (is (string= "eDP-1" (gdk:monitor-connector monitor)))
      (is (string= "Eingebaute Anzeige" (gdk:monitor-description monitor)))
      (is (typep (gdk:monitor-display monitor) 'gdk:display))
      (is (gdk:rectangle-equal (gdk:rectangle-new :x 1920 :y 94
                                                  :width 1920 :height 1080)
                               (gdk:monitor-geometry monitor)))
      (is (= 170 (gdk:monitor-height-mm monitor)))
      (is (string= "LGD" (gdk:monitor-manufacturer monitor)))
      (is (string= "0x046d" (gdk:monitor-model monitor)))
      (is (= 60020 (gdk:monitor-refresh-rate monitor)))
      (is (= 1 (gdk:monitor-scale-factor monitor)))
      (is (eq :unknown (gdk:monitor-subpixel-layout monitor)))
      (is-true (gdk:monitor-valid monitor))
      (is (= 310 (gdk:monitor-width-mm monitor))))))

;;; --- Signals ----------------------------------------------------------------

;;;     invalidate

;;; --- Functions --------------------------------------------------------------

;;;     gdk_monitor_is_valid

;;; --- 2023-7-31 --------------------------------------------------------------
