(in-package :gtk-test)

(def-suite gdk-enumerations :in gdk-suite)
(in-suite gdk-enumerations)

;;;     GdkGravity

(test gdk-gravity
  ;; Check type
  (is (g:type-is-enum "GdkGravity"))
  ;; Check type initializer
  (is (eq (g:gtype "GdkGravity")
          (g:gtype (cffi:foreign-funcall "gdk_gravity_get_type" :size))))
  ;; Check registered name
  (is (eq 'gdk:gravity
          (glib:symbol-for-gtype "GdkGravity")))
  ;; Check the names
  (is (equal '("GDK_GRAVITY_NORTH_WEST" "GDK_GRAVITY_NORTH"
               "GDK_GRAVITY_NORTH_EAST" "GDK_GRAVITY_WEST" "GDK_GRAVITY_CENTER"
               "GDK_GRAVITY_EAST" "GDK_GRAVITY_SOUTH_WEST" "GDK_GRAVITY_SOUTH"
               "GDK_GRAVITY_SOUTH_EAST" "GDK_GRAVITY_STATIC")
             (list-enum-item-name "GdkGravity")))
  ;; Check values
  (is (equal '(1 2 3 4 5 6 7 8 9 10)
             (list-enum-item-value "GdkGravity")))
  ;; Check nick names
  (is (equal '("north-west" "north" "north-east" "west" "center" "east"
               "south-west" "south" "south-east" "static")
             (list-enum-item-nick "GdkGravity")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GdkGravity"
                             GDK-GRAVITY
                             (:EXPORT T
                              :TYPE-INITIALIZER "gdk_gravity_get_type")
                             (:NORTH-WEST 1)
                             (:NORTH 2)
                             (:NORTH-EAST 3)
                             (:WEST 4)
                             (:CENTER 5)
                             (:EAST 6)
                             (:SOUTH-WEST 7)
                             (:SOUTH 8)
                             (:SOUTH-EAST 9)
                             (:STATIC 10))
             (gobject:get-g-type-definition "GdkGravity"))))

;;;     GDK_MODIFIER_MASK

(test gdk-modifier-mask
  (is (= 469769999
         (cffi:convert-to-foreign gdk:+modifier-mask+ 'gdk:modifier-type))))

;;;     GdkModifierType

(test gdk-modifier-type
  ;; Check type
  (is (g:type-is-flags "GdkModifierType"))
  ;; Check registered name
  (is (eq 'gdk:modifier-type
          (glib:symbol-for-gtype "GdkModifierType")))
  ;; Check type initializer
  (is (eq (g:gtype "GdkModifierType")
          (g:gtype (cffi:foreign-funcall "gdk_modifier_type_get_type" :size))))
  ;; Check names
  (is (equal '("GDK_NO_MODIFIER_MASK" "GDK_SHIFT_MASK" "GDK_LOCK_MASK"
               "GDK_CONTROL_MASK" "GDK_ALT_MASK" "GDK_BUTTON1_MASK"
               "GDK_BUTTON2_MASK" "GDK_BUTTON3_MASK" "GDK_BUTTON4_MASK"
               "GDK_BUTTON5_MASK" "GDK_SUPER_MASK" "GDK_HYPER_MASK"
               "GDK_META_MASK")
             (list-flags-item-name "GdkModifierType")))
  ;; Check values
  (is (equal '(0 1 2 4 8 256 512 1024 2048 4096 67108864 134217728 268435456)
             (list-flags-item-value "GdkModifierType")))
  ;; Check nick names
  (is (equal '("no-modifier-mask" "shift-mask" "lock-mask" "control-mask"
               "alt-mask" "button1-mask" "button2-mask" "button3-mask"
               "button4-mask" "button5-mask" "super-mask" "hyper-mask"
               "meta-mask")
             (list-flags-item-nick "GdkModifierType")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "GdkModifierType"
                              GDK-MODIFIER-TYPE
                              (:EXPORT T
                               :TYPE-INITIALIZER "gdk_modifier_type_get_type")
                              (:no-modifier-mask 0)
                              (:SHIFT-MASK 1)
                              (:LOCK-MASK 2)
                              (:CONTROL-MASK 4)
                              (:ALT-MASK 8)
                              (:BUTTON1-MASK 256)
                              (:BUTTON2-MASK 512)
                              (:BUTTON3-MASK 1024)
                              (:BUTTON4-MASK 2048)
                              (:BUTTON5-MASK 4096)
                              (:SUPER-MASK 67108864)
                              (:HYPER-MASK 134217728)
                              (:META-MASK 268435456))
             (gobject:get-g-type-definition "GdkModifierType"))))

;;; 2024-5-25
