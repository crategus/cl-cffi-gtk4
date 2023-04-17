(in-package :gtk-test)

(def-suite gdk-device :in gdk-suite)
(in-suite gdk-device)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkInputSource

(test gdk-input-source
  ;; Check the type
  (is (g:type-is-enum "GdkInputSource"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkInputSource")
          (g:gtype (cffi:foreign-funcall "gdk_input_source_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:input-source
          (gobject:symbol-for-gtype "GdkInputSource")))
  ;; Check the names
  (is (equal '("GDK_SOURCE_MOUSE" "GDK_SOURCE_PEN" "GDK_SOURCE_KEYBOARD"
               "GDK_SOURCE_TOUCHSCREEN" "GDK_SOURCE_TOUCHPAD"
               "GDK_SOURCE_TRACKPOINT" "GDK_SOURCE_TABLET_PAD")
             (list-enum-item-name "GdkInputSource")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6)
             (list-enum-item-value "GdkInputSource")))
  ;; Check the nick names
  (is (equal '("mouse" "pen" "keyboard" "touchscreen" "touchpad" "trackpoint"
               "tablet-pad")
             (list-enum-item-nick "GdkInputSource")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkInputSource"
                             GDK-INPUT-SOURCE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_input_source_get_type")
                             (:MOUSE 0)
                             (:PEN 1)
                             (:KEYBOARD 2)
                             (:TOUCHSCREEN 3)
                             (:TOUCHPAD 4)
                             (:TRACKPOINT 5)
                             (:TABLET-PAD 6))
             (gobject:get-g-type-definition "GdkInputSource"))))

;;;     GdkAxisUse

(test gdk-axis-use
  ;; Check the type
  (is (g:type-is-enum "GdkAxisUse"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAxisUse")
          (g:gtype (cffi:foreign-funcall "gdk_axis_use_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:axis-use
          (gobject:symbol-for-gtype "GdkAxisUse")))
  ;; Check the names
  (is (equal '("GDK_AXIS_IGNORE" "GDK_AXIS_X" "GDK_AXIS_Y" "GDK_AXIS_DELTA_X"
               "GDK_AXIS_DELTA_Y" "GDK_AXIS_PRESSURE" "GDK_AXIS_XTILT"
               "GDK_AXIS_YTILT" "GDK_AXIS_WHEEL" "GDK_AXIS_DISTANCE"
               "GDK_AXIS_ROTATION" "GDK_AXIS_SLIDER" "GDK_AXIS_LAST")
             (list-enum-item-name "GdkAxisUse")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12)
             (list-enum-item-value "GdkAxisUse")))
  ;; Check the nick names
  (is (equal '("ignore" "x" "y" "delta-x" "delta-y" "pressure" "xtilt" "ytilt"
               "wheel" "distance" "rotation" "slider" "last")
             (list-enum-item-nick "GdkAxisUse")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkAxisUse"
                             GDK-AXIS-USE
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
             (gobject:get-g-type-definition "GdkAxisUse"))))

;;;     GdkAxisFlags

(test gdk-axis-flags
  ;; Check the type
  (is (g:type-is-flags "GdkAxisFlags"))
  ;; Check the registered name
  (is (eq 'gdk:axis-flags
          (gobject:symbol-for-gtype "GdkAxisFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkAxisFlags")
          (g:gtype (cffi:foreign-funcall "gdk_axis_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("GDK_AXIS_FLAG_X" "GDK_AXIS_FLAG_Y" "GDK_AXIS_FLAG_DELTA_X"
               "GDK_AXIS_FLAG_DELTA_Y" "GDK_AXIS_FLAG_PRESSURE"
               "GDK_AXIS_FLAG_XTILT" "GDK_AXIS_FLAG_YTILT" "GDK_AXIS_FLAG_WHEEL"
               "GDK_AXIS_FLAG_DISTANCE" "GDK_AXIS_FLAG_ROTATION"
               "GDK_AXIS_FLAG_SLIDER")
             (list-flags-item-name "GdkAxisFlags")))
  ;; Check the values
  (is (equal '(2 4 8 16 32 64 128 256 512 1024 2048)
             (list-flags-item-value "GdkAxisFlags")))
  ;; Check the nick names
  (is (equal '("x" "y" "delta-x" "delta-y" "pressure" "xtilt" "ytilt" "wheel"
               "distance" "rotation" "slider")
             (list-flags-item-nick "GdkAxisFlags")))
  ;; Check the flags definition
  (is (equal '(DEFINE-G-FLAGS "GdkAxisFlags"
                              GDK-AXIS-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_axis_flags_get_type")
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
             (gobject:get-g-type-definition "GdkAxisFlags"))))

;;;     GdkDeviceToolType

(test gdk-device-tool-type
  ;; Check the type
  (is (g:type-is-enum "GdkDeviceToolType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceToolType")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:device-tool-type
          (gobject:symbol-for-gtype "GdkDeviceToolType")))
  ;; Check the names
  (is (equal '("GDK_DEVICE_TOOL_TYPE_UNKNOWN" "GDK_DEVICE_TOOL_TYPE_PEN"
               "GDK_DEVICE_TOOL_TYPE_ERASER" "GDK_DEVICE_TOOL_TYPE_BRUSH"
               "GDK_DEVICE_TOOL_TYPE_PENCIL" "GDK_DEVICE_TOOL_TYPE_AIRBRUSH"
               "GDK_DEVICE_TOOL_TYPE_MOUSE" "GDK_DEVICE_TOOL_TYPE_LENS")
             (list-enum-item-name "GdkDeviceToolType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "GdkDeviceToolType")))
  ;; Check the nick names
  (is (equal '("unknown" "pen" "eraser" "brush" "pencil" "airbrush" "mouse"
               "lens")
             (list-enum-item-nick "GdkDeviceToolType")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "GdkDeviceToolType"
                             GDK-DEVICE-TOOL-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_device_tool_type_get_type")
                             (:UNKNOWN 0)
                             (:PEN 1)
                             (:ERASER 2)
                             (:BRUSH 3)
                             (:PENCIL 4)
                             (:AIRBRUSH 5)
                             (:MOUSE 6)
                             (:LENS 7))
             (gobject:get-g-type-definition "GdkDeviceToolType"))))

;;;     GdkDeviceTool

(test gdk-device-tool-class
  ;; Type check
  (is (g:type-is-object "GdkDeviceTool"))
  ;; Check the registered name
  (is (eq 'gdk:device-tool
          (gobject:symbol-for-gtype "GdkDeviceTool")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceTool")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDeviceTool")))
  ;; Check the children
  (is (equal '()
             (list-children "GdkDeviceTool")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDeviceTool")))
  ;; Check the properties
  (is (equal '("axes" "hardware-id" "serial" "tool-type")
             (list-properties "GdkDeviceTool")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GdkDeviceTool")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDeviceTool" GDK-DEVICE-TOOL
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_tool_get_type")
                       ((AXES GDK-DEVICE-TOOL-AXES "axes" "GdkAxisFlags" T NIL)
                        (HARDWARE-ID GDK-DEVICE-TOOL-HARDWARE-ID "hardware-id"
                         "guint64" T NIL)
                        (SERIAL GDK-DEVICE-TOOL-SERIAL "serial" "guint64" T
                         NIL)
                        (TOOL-TYPE GDK-DEVICE-TOOL-TOOL-TYPE "tool-type"
                         "GdkDeviceToolType" T NIL)))
             (gobject:get-g-type-definition "GdkDeviceTool"))))

;;; --- Properties -------------------------------------------------------------

;;;     axes
;;;     hardware-id
;;;     serial
;;;     tool-type

;;;     GdkDevice

(test gdk-device-class
  ;; Type check
  (is (g:type-is-object "GdkDevice"))
  ;; Check the registered name
  (is (eq 'gdk:device
          (gobject:symbol-for-gtype "GdkDevice")))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDevice")
          (g:gtype (cffi:foreign-funcall "gdk_device_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDevice")))
  ;; Check the children
  (is (equal '("GdkBroadwayDevice" "GdkWaylandDevice")
             (list-children "GdkDevice")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GdkDevice")))
  ;; Check the properties
  (is (equal '("caps-lock-state" "direction" "display" "has-bidi-layouts"
               "has-cursor" "modifier-state" "n-axes" "name" "num-lock-state"
               "num-touches" "product-id" "scroll-lock-state" "seat" "source"
               "tool" "vendor-id")
             (list-properties "GdkDevice")))
  ;; Check the signals
  (is (equal '("changed" "tool-changed")
             (list-signals "GdkDevice")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GdkDevice" GDK-DEVICE
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_get_type")
                       ((CAPS-LOCK-STATE GDK-DEVICE-CAPS-LOCK-STATE
                         "caps-lock-state" "gboolean" T NIL)
                        (DIRECTION GDK-DEVICE-DIRECTION "direction"
                         "PangoDirection" T NIL)
                        (DISPLAY GDK-DEVICE-DISPLAY "display" "GdkDisplay" T
                         NIL)
                        (HAS-BIDI-LAYOUTS GDK-DEVICE-HAS-BIDI-LAYOUTS
                         "has-bidi-layouts" "gboolean" T NIL)
                        (HAS-CURSOR GDK-DEVICE-HAS-CURSOR "has-cursor"
                         "gboolean" T NIL)
                        (MODIFIER-STATE GDK-DEVICE-MODIFIER-STATE
                         "modifier-state" "GdkModifierType" T NIL)
                        (N-AXES GDK-DEVICE-N-AXES "n-axes" "guint" T NIL)
                        (NAME GDK-DEVICE-NAME "name" "gchararray" T NIL)
                        (NUM-LOCK-STATE GDK-DEVICE-NUM-LOCK-STATE
                         "num-lock-state" "gboolean" T NIL)
                        (NUM-TOUCHES GDK-DEVICE-NUM-TOUCHES "num-touches"
                         "guint" T NIL)
                        (PRODUCT-ID GDK-DEVICE-PRODUCT-ID "product-id"
                         "gchararray" T NIL)
                        (SCROLL-LOCK-STATE GDK-DEVICE-SCROLL-LOCK-STATE
                         "scroll-lock-state" "gboolean" T NIL)
                        (SEAT GDK-DEVICE-SEAT "seat" "GdkSeat" T T)
                        (SOURCE GDK-DEVICE-SOURCE "source" "GdkInputSource" T
                         NIL)
                        (TOOL GDK-DEVICE-TOOL "tool" "GdkDeviceTool" T NIL)
                        (VENDOR-ID GDK-DEVICE-VENDOR-ID "vendor-id"
                         "gchararray" T NIL)))
             (gobject:get-g-type-definition "GdkDevice"))))

;;; --- Properties -------------------------------------------------------------

;;;     caps-lock-state
;;;     direction
;;;     display
;;;     has-bidi-layouts
;;;     has-cursor
;;;     modifier-state
;;;     n-axes
;;;     name
;;;     num-lock-state
;;;     num-touches
;;;     product-id
;;;     scroll-lock-state
;;;     seat
;;;     source
;;;     tool
;;;     vendor-id

;;; --- Signals ----------------------------------------------------------------

;;;     changed
;;;     tool-changed

;;; --- Functions --------------------------------------------------------------

;;;     gdk_device_get_surface_at_position
;;;     gdk_device_get_timestamp                           Since 4.2

;;; --- 2023-4-15 --------------------------------------------------------------
