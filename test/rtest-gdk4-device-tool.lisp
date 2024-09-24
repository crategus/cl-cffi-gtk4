(in-package :gtk-test)

(def-suite gdk-device-tool :in gdk-suite)
(in-suite gdk-device-tool)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDeviceToolType

(test gdk-device-tool-type
  ;; Check type
  (is (g:type-is-enum "GdkDeviceToolType"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDeviceToolType")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:device-tool-type
          (glib:symbol-for-gtype "GdkDeviceToolType")))
  ;; Check names
  (is (equal '("GDK_DEVICE_TOOL_TYPE_UNKNOWN" "GDK_DEVICE_TOOL_TYPE_PEN"
               "GDK_DEVICE_TOOL_TYPE_ERASER" "GDK_DEVICE_TOOL_TYPE_BRUSH"
               "GDK_DEVICE_TOOL_TYPE_PENCIL" "GDK_DEVICE_TOOL_TYPE_AIRBRUSH"
               "GDK_DEVICE_TOOL_TYPE_MOUSE" "GDK_DEVICE_TOOL_TYPE_LENS")
             (glib-test:list-enum-item-names "GdkDeviceToolType")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7)
             (glib-test:list-enum-item-values "GdkDeviceToolType")))
  ;; Check nick names
  (is (equal '("unknown" "pen" "eraser" "brush" "pencil" "airbrush" "mouse"
               "lens")
             (glib-test:list-enum-item-nicks "GdkDeviceToolType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GdkDeviceToolType" GDK:DEVICE-TOOL-TYPE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "gdk_device_tool_type_get_type")
                                    (:UNKNOWN 0)
                                    (:PEN 1)
                                    (:ERASER 2)
                                    (:BRUSH 3)
                                    (:PENCIL 4)
                                    (:AIRBRUSH 5)
                                    (:MOUSE 6)
                                    (:LENS 7))
             (gobject:get-gtype-definition "GdkDeviceToolType"))))

;;;     GdkDeviceTool

(test gdk-device-tool-class
  ;; Check type
  (is (g:type-is-object "GdkDeviceTool"))
  ;; Check registered name
  (is (eq 'gdk:device-tool
          (glib:symbol-for-gtype "GdkDeviceTool")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDeviceTool")
          (g:gtype (cffi:foreign-funcall "gdk_device_tool_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GdkDeviceTool")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GdkDeviceTool")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "GdkDeviceTool")))
  ;; Check properties
  (is (equal '("axes" "hardware-id" "serial" "tool-type")
             (glib-test:list-properties "GdkDeviceTool")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GdkDeviceTool")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GdkDeviceTool" GDK:DEVICE-TOOL
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "gdk_device_tool_get_type")
                       ((AXES DEVICE-TOOL-AXES "axes" "GdkAxisFlags" T NIL)
                        (HARDWARE-ID DEVICE-TOOL-HARDWARE-ID
                         "hardware-id" "guint64" T NIL)
                        (SERIAL DEVICE-TOOL-SERIAL "serial" "guint64" T NIL)
                        (TOOL-TYPE DEVICE-TOOL-TOOL-TYPE
                         "tool-type" "GdkDeviceToolType" T NIL)))
             (gobject:get-gtype-definition "GdkDeviceTool"))))

;;; --- Properties -------------------------------------------------------------

(test gdk-device-tool-properties
  (let ((tool (make-instance 'gdk:device-tool)))
    (is-false (gdk:device-tool-axes tool))
    (is (= 0 (gdk:device-tool-hardware-id tool)))
    (is (= 0 (gdk:device-tool-serial tool)))
    (is (eq :unknown (gdk:device-tool-tool-type tool)))))

;;; 2024-9-19
