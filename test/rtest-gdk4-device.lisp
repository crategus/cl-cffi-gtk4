(in-package :gtk-test)

(def-suite gdk-device :in gdk-suite)
(in-suite gdk-device)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkInputSource

(test gdk-input-source
  ;; Check type
  (is (g:type-is-enum "GdkInputSource"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkInputSource")
          (g:gtype (cffi:foreign-funcall "gdk_input_source_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:input-source
          (glib:symbol-for-gtype "GdkInputSource")))
  ;; Check names
  (is (equal '("GDK_SOURCE_MOUSE" "GDK_SOURCE_PEN" "GDK_SOURCE_KEYBOARD"
               "GDK_SOURCE_TOUCHSCREEN" "GDK_SOURCE_TOUCHPAD"
               "GDK_SOURCE_TRACKPOINT" "GDK_SOURCE_TABLET_PAD")
             (glib-test:list-enum-item-names "GdkInputSource")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "GdkInputSource")))
  ;; Check nick names
  (is (equal '("mouse" "pen" "keyboard" "touchscreen" "touchpad" "trackpoint"
               "tablet-pad")
             (glib-test:list-enum-item-nicks "GdkInputSource")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkInputSource" GDK:INPUT-SOURCE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_input_source_get_type")
                                    (:MOUSE 0)
                                    (:PEN 1)
                                    (:KEYBOARD 2)
                                    (:TOUCHSCREEN 3)
                                    (:TOUCHPAD 4)
                                    (:TRACKPOINT 5)
                                    (:TABLET-PAD 6))
             (gobject:get-gtype-definition "GdkInputSource"))))

;;;     GdkAxisUse

(test gdk-axis-use
  ;; Check type
  (is (g:type-is-enum "GdkAxisUse"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkAxisUse")
          (g:gtype (cffi:foreign-funcall "gdk_axis_use_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:axis-use
          (glib:symbol-for-gtype "GdkAxisUse")))
  ;; Check names
  (is (equal '("GDK_AXIS_IGNORE" "GDK_AXIS_X" "GDK_AXIS_Y" "GDK_AXIS_DELTA_X"
               "GDK_AXIS_DELTA_Y" "GDK_AXIS_PRESSURE" "GDK_AXIS_XTILT"
               "GDK_AXIS_YTILT" "GDK_AXIS_WHEEL" "GDK_AXIS_DISTANCE"
               "GDK_AXIS_ROTATION" "GDK_AXIS_SLIDER" "GDK_AXIS_LAST")
             (glib-test:list-enum-item-names "GdkAxisUse")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12)
             (glib-test:list-enum-item-values "GdkAxisUse")))
  ;; Check nick names
  (is (equal '("ignore" "x" "y" "delta-x" "delta-y" "pressure" "xtilt" "ytilt"
               "wheel" "distance" "rotation" "slider" "last")
             (glib-test:list-enum-item-nicks "GdkAxisUse")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkAxisUse" GDK:AXIS-USE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gdk_axis_use_get_type")
                                    (:IGNORE 0)
                                    (:X 1)
                                    (:Y 2)
                                    (:DELTA-X 3)
                                    (:DELTA-Y 4)
                                    (:PRESSURE 5)
                                    (:XTILT 6)
                                    (:YTILT 7)
                                    (:WHEEL 8)
                                    (:DISTANCE 9)
                                    (:ROTATION 10)
                                    (:SLIDER 11)
                                    (:LAST 12))
             (gobject:get-gtype-definition "GdkAxisUse"))))

;;;     GdkAxisFlags

(test gdk-axis-flags
  ;; Check type
  (is (g:type-is-flags "GdkAxisFlags"))
  ;; Check registered name
  (is (eq 'gdk:axis-flags
          (glib:symbol-for-gtype "GdkAxisFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkAxisFlags")
          (g:gtype (cffi:foreign-funcall "gdk_axis_flags_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_AXIS_FLAG_X" "GDK_AXIS_FLAG_Y" "GDK_AXIS_FLAG_DELTA_X"
               "GDK_AXIS_FLAG_DELTA_Y" "GDK_AXIS_FLAG_PRESSURE"
               "GDK_AXIS_FLAG_XTILT" "GDK_AXIS_FLAG_YTILT" "GDK_AXIS_FLAG_WHEEL"
               "GDK_AXIS_FLAG_DISTANCE" "GDK_AXIS_FLAG_ROTATION"
               "GDK_AXIS_FLAG_SLIDER")
             (glib-test:list-flags-item-names "GdkAxisFlags")))
  ;; Check values
  (is (equal '(2 4 8 16 32 64 128 256 512 1024 2048)
             (glib-test:list-flags-item-values "GdkAxisFlags")))
  ;; Check nick names
  (is (equal '("x" "y" "delta-x" "delta-y" "pressure" "xtilt" "ytilt" "wheel"
               "distance" "rotation" "slider")
             (glib-test:list-flags-item-nicks "GdkAxisFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GdkAxisFlags" GDK:AXIS-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gdk_axis_flags_get_type")
                                     (:X 2)
                                     (:Y 4)
                                     (:DELTA-X 8)
                                     (:DELTA-Y 16)
                                     (:PRESSURE 32)
                                     (:XTILT 64)
                                     (:YTILT 128)
                                     (:WHEEL 256)
                                     (:DISTANCE 512)
                                     (:ROTATION 1024)
                                     (:SLIDER 2048))
             (gobject:get-gtype-definition "GdkAxisFlags"))))

;;;     GdkDevice

(test gdk-device-class
  ;; Check type
  (is (g:type-is-object "GdkDevice"))
  ;; Check registered name
  (is (eq 'gdk:device
          (glib:symbol-for-gtype "GdkDevice")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDevice")
          (g:gtype (cffi:foreign-funcall "gdk_device_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDevice")))
  ;; Check children
  #-windows
  (is (member "GdkWaylandDevice"
              (glib-test:list-children "GdkDevice") :test #'string=))
  #+windows
  (is (equal '("GdkDeviceVirtual" "GdkDeviceWin32")
             (glib-test:list-children "GdkDevice")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDevice")))
  ;; Check properties
  (is (equal '("caps-lock-state" "direction" "display" "has-bidi-layouts"
               "has-cursor" "modifier-state" "n-axes" "name" "num-lock-state"
               "num-touches" "product-id" "scroll-lock-state" "seat" "source"
               "tool" "vendor-id")
             (glib-test:list-properties "GdkDevice")))
  ;; Check signals
  (is (equal '("changed" "tool-changed")
             (glib-test:list-signals "GdkDevice")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDevice" GDK:DEVICE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_get_type")
                       ((CAPS-LOCK-STATE DEVICE-CAPS-LOCK-STATE
                         "caps-lock-state" "gboolean" T NIL)
                        (DIRECTION DEVICE-DIRECTION
                         "direction" "PangoDirection" T NIL)
                        (DISPLAY DEVICE-DISPLAY "display" "GdkDisplay" T NIL)
                        (HAS-BIDI-LAYOUTS DEVICE-HAS-BIDI-LAYOUTS
                         "has-bidi-layouts" "gboolean" T NIL)
                        (HAS-CURSOR DEVICE-HAS-CURSOR
                         "has-cursor" "gboolean" T NIL)
                        (MODIFIER-STATE DEVICE-MODIFIER-STATE
                         "modifier-state" "GdkModifierType" T NIL)
                        (N-AXES DEVICE-N-AXES "n-axes" "guint" T NIL)
                        (NAME DEVICE-NAME "name" "gchararray" T NIL)
                        (NUM-LOCK-STATE DEVICE-NUM-LOCK-STATE
                         "num-lock-state" "gboolean" T NIL)
                        (NUM-TOUCHES DEVICE-NUM-TOUCHES
                         "num-touches" "guint" T NIL)
                        (PRODUCT-ID DEVICE-PRODUCT-ID
                         "product-id" "gchararray" T NIL)
                        (SCROLL-LOCK-STATE DEVICE-SCROLL-LOCK-STATE
                         "scroll-lock-state" "gboolean" T NIL)
                        (SEAT DEVICE-SEAT "seat" "GdkSeat" T T)
                        (SOURCE DEVICE-SOURCE "source" "GdkInputSource" T NIL)
                        (TOOL DEVICE-TOOL "tool" "GdkDeviceTool" T NIL)
                        (VENDOR-ID DEVICE-VENDOR-ID
                         "vendor-id" "gchararray" T NIL)))
             (gobject:get-gtype-definition "GdkDevice"))))

;;; --- Signals ----------------------------------------------------------------

;;;     changed

(test gdk-device-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "changed" "GdkDevice"))))
    (is (string= "changed" (g:signal-query-signal-name query)))
    (is (string= "GdkDevice" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     tool-changed

(test gdk-device-tool-changed-signal
  (let ((query (g:signal-query (g:signal-lookup "tool-changed" "GdkDevice"))))
    (is (string= "tool-changed" (g:signal-query-signal-name query)))
    (is (string= "GdkDevice" (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("GdkDeviceTool")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; --- Properties -------------------------------------------------------------

(test gdk-device-properties.1
  (glib-test:with-check-memory (:strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-pointer seat)))
      (is-false (gdk:device-caps-lock-state device))
      (is (eq :neutral (gdk:device-direction device)))
      (is (eq (gdk:display-default) (gdk:device-display device)))
      (is-false (gdk:device-has-bidi-layouts device))
      (is-true (gdk:device-has-cursor device))
      (is-false (gdk:device-modifier-state device))
      #-windows
      (is (= 2 (gdk:device-n-axes device)))
      #+windows
      (is (= 2 (gdk:device-n-axes device)))
      #-windows
      (is (string= "Core Pointer" (gdk:device-name device)))
      #+windows
      (is (string= "Virtual Core Pointer" (gdk:device-name device)))
      (is-false (gdk:device-num-lock-state device))
      (is (= 0 (gdk:device-num-touches device)))
      (is-false (gdk:device-product-id device))
      (is-false (gdk:device-scroll-lock-state device))
      (is (eq seat (gdk:device-seat device)))
      (is (eq :mouse (gdk:device-source device)))
      (is-false (gdk:device-tool device))
      (is-false (gdk:device-vendor-id device)))))

(test gdk-device-properties.2
  (glib-test:with-check-memory (:strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-keyboard seat)))
      (is-false (gdk:device-caps-lock-state device))
      (is (eq :ltr (gdk:device-direction device)))
      (is (eq (gdk:display-default) (gdk:device-display device)))
      (is-false (gdk:device-has-bidi-layouts device))
      (is-false (gdk:device-has-cursor device))
      (is-false (gdk:device-modifier-state device))
      (is (= 0 (gdk:device-n-axes device)))
      #-windows
      (is (string= "Core Keyboard" (gdk:device-name device)))
      #+windows
      (is (string= "Virtual Core Keyboard" (gdk:device-name device)))
      (is (typep (gdk:device-num-lock-state device) 'boolean))
      (is (= 0 (gdk:device-num-touches device)))
      (is-false (gdk:device-product-id device))
      (is-false (gdk:device-scroll-lock-state device))
      (is (eq seat (gdk:device-seat device)))
      (is (eq :keyboard (gdk:device-source device)))
      (is-false (gdk:device-tool device))
      (is-false (gdk:device-vendor-id device)))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_device_get_surface_at_position

(test gdk-device-surface-at-position
  (glib-test:with-check-memory (:strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-pointer seat)))
      (is-false (gdk:device-surface-at-position device)))))

;;;     gdk_device_get_timestamp                           Since 4.2

(test gdk-device-timestamp.1
  (glib-test:with-check-memory (:strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-pointer seat)))
      (is (= 0 (gdk:device-timestamp device))))))

(test gdk-device-timestamp.2
  (glib-test:with-check-memory (:strong 3)
    (let* ((seat (gdk:display-default-seat (gdk:display-default)))
           (device (gdk:seat-keyboard seat)))
      (is (= 0 (gdk:device-timestamp device))))))

;;; 2024-12-20
