(in-package :gtk-test)

(def-suite gdk-device-pad :in gdk-suite)
(in-suite gdk-device-pad)

;;; --- Types and Values -------------------------------------------------------

;;;     GdkDevicePadFeature

(test gdk-device-pad-feature
  ;; Check type
  (is (g:type-is-enum "GdkDevicePadFeature"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDevicePadFeature")
          (g:gtype (cffi:foreign-funcall "gdk_device_pad_feature_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gdk:device-pad-feature
          (glib:symbol-for-gtype "GdkDevicePadFeature")))
  ;; Check names
  (is (equal '("GDK_DEVICE_PAD_FEATURE_BUTTON" "GDK_DEVICE_PAD_FEATURE_RING"
               "GDK_DEVICE_PAD_FEATURE_STRIP")
             (gtk-test:list-enum-item-name "GdkDevicePadFeature")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GdkDevicePadFeature")))
  ;; Check nick names
  (is (equal '("button" "ring" "strip")
             (gtk-test:list-enum-item-nick "GdkDevicePadFeature")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkDevicePadFeature"
                             GDK-DEVICE-PAD-FEATURE
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gdk_device_pad_feature_get_type")
                             (:BUTTON 0)
                             (:RING 1)
                             (:STRIP 2))
             (gobject:get-g-type-definition "GdkDevicePadFeature"))))

;;;     GdkDevicePad

(test gdk-device-pad-interface
  ;; Check type
  (is (g:type-is-interface "GdkDevicePad"))
  ;; Check registered name
  (is (eq 'gdk:device-pad
          (glib:symbol-for-gtype "GdkDevicePad")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkDevicePad")
          (g:gtype (cffi:foreign-funcall "gdk_device_pad_get_type" :size))))
  ;; Check interface Prerequisites
  (is (equal '("GdkDevice")
             (gtk-test:list-interface-prerequisites "GdkDevicePad")))
  ;; Check interface properties
  (is (equal '()
             (gtk-test:list-interface-properties "GdkDevicePad")))
  ;; Check signals
  (is (equal '()
             (gtk-test:list-signals "GdkDevicePad")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GdkDevicePad" GDK-DEVICE-PAD
                    (:EXPORT T :TYPE-INITIALIZER "gdk_device_pad_get_type"))
             (gobject:get-g-type-definition "GdkDevicePad"))))

;;; --- Functions --------------------------------------------------------------

;;;     gdk_device_pad_get_n_groups
;;;     gdk_device_pad_get_group_n_modes
;;;     gdk_device_pad_get_n_features
;;;     gdk_device_pad_get_feature_group

;;; 2024-7-4
