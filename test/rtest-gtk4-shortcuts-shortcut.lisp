(in-package :gtk-test)

(def-suite gtk-shortcuts-shortcut :in gtk-suite)
(in-suite gtk-shortcuts-shortcut)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutType

(test gtk-shortcut-type
  ;; Check the type
  (is (g:type-is-enum "GtkShortcutType"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutType")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-type
          (glib:symbol-for-gtype "GtkShortcutType")))
  ;; Check the names
  (is (equal '("GTK_SHORTCUT_ACCELERATOR"
               "GTK_SHORTCUT_GESTURE_PINCH"
               "GTK_SHORTCUT_GESTURE_STRETCH"
               "GTK_SHORTCUT_GESTURE_ROTATE_CLOCKWISE"
               "GTK_SHORTCUT_GESTURE_ROTATE_COUNTERCLOCKWISE"
               "GTK_SHORTCUT_GESTURE_TWO_FINGER_SWIPE_LEFT"
               "GTK_SHORTCUT_GESTURE_TWO_FINGER_SWIPE_RIGHT"
               "GTK_SHORTCUT_GESTURE"
               "GTK_SHORTCUT_GESTURE_SWIPE_LEFT"
               "GTK_SHORTCUT_GESTURE_SWIPE_RIGHT")
             (list-enum-item-name "GtkShortcutType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9)
             (list-enum-item-value "GtkShortcutType")))
  ;; Check the nick names
  (is (equal '("accelerator" "gesture-pinch" "gesture-stretch"
               "gesture-rotate-clockwise" "gesture-rotate-counterclockwise"
               "gesture-two-finger-swipe-left" "gesture-two-finger-swipe-right"
               "gesture" "gesture-swipe-left" "gesture-swipe-right")
             (list-enum-item-nick "GtkShortcutType")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkShortcutType" GTK-SHORTCUT-TYPE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_shortcut_type_get_type")
                                     (:ACCELERATOR 0)
                                     (:GESTURE-PINCH 1)
                                     (:GESTURE-STRETCH 2)
                                     (:GESTURE-ROTATE-CLOCKWISE 3)
                                     (:GESTURE-ROTATE-COUNTERCLOCKWISE 4)
                                     (:GESTURE-TWO-FINGER-SWIPE-LEFT 5)
                                     (:GESTURE-TWO-FINGER-SWIPE-RIGHT 6)
                                     (:GESTURE 7)
                                     (:GESTURE-SWIPE-LEFT 8)
                                     (:GESTURE-SWIPE-RIGHT 9))
             (gobject:get-g-type-definition "GtkShortcutType"))))

;;;     GtkShortcutsShortcut

(test gtk-shortcuts-shortcut-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsShortcut"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-shortcut
          (glib:symbol-for-gtype "GtkShortcutsShortcut")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsShortcut")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_shortcut_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkShortcutsShortcut")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutsShortcut")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkShortcutsShortcut")))
  ;; Check the properties
  (is (equal '("accel-size-group" "accelerator" "action-name" "direction" "icon"
               "icon-set" "shortcut-type" "subtitle" "subtitle-set" "title"
               "title-size-group")
             (list-properties "GtkShortcutsShortcut")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkShortcutsShortcut")))
  ;; CSS name
  (is (string= "shortcut"
               (gtk:widget-class-css-name "GtkShortcutsShortcut")))
  ;; CSS classes
  (is (equal '()
             (gtk:widget-css-classes (make-instance 'gtk:shortcuts-shortcut))))
  ;; Accessible role
  (is (eq :label (gtk:widget-class-accessible-role "GtkShortcutsShortcut")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutsShortcut"
                                             GTK-SHORTCUTS-SHORTCUT
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_shortcuts_shortcut_get_type")
                               ((ACCEL-SIZE-GROUP
                                 GTK-SHORTCUTS-SHORTCUT-ACCEL-SIZE-GROUP
                                 "accel-size-group" "GtkSizeGroup" NIL T)
                                (ACCELERATOR GTK-SHORTCUTS-SHORTCUT-ACCELERATOR
                                 "accelerator" "gchararray" T T)
                                (ACTION-NAME GTK-SHORTCUTS-SHORTCUT-ACTION-NAME
                                 "action-name" "gchararray" T T)
                                (DIRECTION GTK-SHORTCUTS-SHORTCUT-DIRECTION
                                 "direction" "GtkTextDirection" T T)
                                (ICON GTK-SHORTCUTS-SHORTCUT-ICON "icon"
                                 "GIcon" T T)
                                (ICON-SET GTK-SHORTCUTS-SHORTCUT-ICON-SET
                                 "icon-set" "gboolean" T T)
                                (SHORTCUT-TYPE
                                 GTK-SHORTCUTS-SHORTCUT-SHORTCUT-TYPE
                                 "shortcut-type" "GtkShortcutType" T T)
                                (SUBTITLE GTK-SHORTCUTS-SHORTCUT-SUBTITLE
                                 "subtitle" "gchararray" T T)
                                (SUBTITLE-SET
                                 GTK-SHORTCUTS-SHORTCUT-SUBTITLE-SET
                                 "subtitle-set" "gboolean" T T)
                                (TITLE GTK-SHORTCUTS-SHORTCUT-TITLE "title"
                                 "gchararray" T T)
                                (TITLE-SIZE-GROUP
                                 GTK-SHORTCUTS-SHORTCUT-TITLE-SIZE-GROUP
                                 "title-size-group" "GtkSizeGroup" NIL T)))
             (gobject:get-g-type-definition "GtkShortcutsShortcut"))))

;;; --- Properties -------------------------------------------------------------

;;;     accel-size-group
;;;     accelerator
;;;     action-name
;;;     direction
;;;     icon
;;;     icon-set
;;;     shortcut-type
;;;     subtitle
;;;     subtitle-set
;;;     title
;;;     title-size-group

(test gtk-shortcuts-shortcut
 (let ((shortcut (make-instance 'gtk:shortcuts-shortcut)))
    (signals (error) (gtk:shortcuts-shortcut-accel-size-group shortcut))
    (is-false (gtk:shortcuts-shortcut-accelerator shortcut))
    (is-false (gtk:shortcuts-shortcut-action-name shortcut))
    (is (eq :none (gtk:shortcuts-shortcut-direction shortcut)))
    (is-false (gtk:shortcuts-shortcut-icon shortcut))
    (is-false (gtk:shortcuts-shortcut-icon-set shortcut))
    (is (eq :accelerator (gtk:shortcuts-shortcut-shortcut-type shortcut)))
    (is (string= "" (gtk:shortcuts-shortcut-subtitle shortcut)))
    (is-false (gtk:shortcuts-shortcut-subtitle-set shortcut))
    (is (string= "" (gtk:shortcuts-shortcut-title shortcut)))
    (signals (error) (gtk:shortcuts-shortcut-title-size-group shortcut))))

;;; 2024-2-18
