(in-package :gtk-test)

(def-suite gdk-device-tool :in gdk-suite)
(in-suite gdk-device-tool)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDeviceToolType

(test gdk-device-tool-type
  ;; Check the type
  (is (g:type-is-enum "GdkDeviceToolType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GdkDeviceToolType")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gdk:device-tool-type
          (glib:symbol-for-gtype "GdkDeviceToolType")))
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
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkDeviceToolType"
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
          (glib:symbol-for-gtype "GdkDeviceTool")))
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
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GdkDeviceTool" GDK-DEVICE-TOOL
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

(test gdk-device-tool-properties
  (let ((tool (make-instance 'gdk:device-tool)))
    (is-false (gdk:device-tool-axes tool))
    (is (= 0 (gdk:device-tool-hardware-id tool)))
    (is (= 0 (gdk:device-tool-serial tool)))
    (is (eq :unknown (gdk:device-tool-tool-type tool)))))

;;; 2024-1-7
