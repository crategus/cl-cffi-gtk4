(in-package :gtk-test)

(def-suite gtk-menu-button :in gtk-suite)
(in-suite gtk-menu-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkArrowType

(test gtk-arrow-type
  ;; Check type
  (is (g:type-is-enum "GtkArrowType"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkArrowType")
          (g:gtype (cffi:foreign-funcall "gtk_arrow_type_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:arrow-type
          (glib:symbol-for-gtype "GtkArrowType")))
  ;; Check names
  (is (equal '("GTK_ARROW_UP" "GTK_ARROW_DOWN" "GTK_ARROW_LEFT"
               "GTK_ARROW_RIGHT" "GTK_ARROW_NONE")
             (list-enum-item-name "GtkArrowType")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (list-enum-item-value "GtkArrowType")))
  ;; Check nick names
  (is (equal '("up" "down" "left" "right" "none")
             (list-enum-item-nick "GtkArrowType")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkArrowType"
                             GTK-ARROW-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "gtk_arrow_type_get_type")
                             (:UP 0)
                             (:DOWN 1)
                             (:LEFT 2)
                             (:RIGHT 3)
                             (:NONE 4))
             (gobject:get-g-type-definition "GtkArrowType"))))

;;;     GtkMenuButton

(test gtk-menu-button-class
  ;; Check type
  (is (g:type-is-object "GtkMenuButton"))
  ;; Check registered name
  (is (eq 'gtk:menu-button
          (glib:symbol-for-gtype "GtkMenuButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkMenuButton")
          (g:gtype (cffi:foreign-funcall "gtk_menu_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkMenuButton")))
  ;; Check children
  (is (equal '()
             (list-children "GtkMenuButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (list-interfaces "GtkMenuButton")))
  ;; Check properties
  (is (equal '("active" "always-show-arrow" "can-shrink" "child" "direction"
               "has-frame" "icon-name" "label" "menu-model" "popover" "primary"
               "use-underline")
             (list-properties "GtkMenuButton")))
  ;; Check signals
  (is (equal '("activate")
             (list-signals "GtkMenuButton")))
  ;; Check CSS name
  (is (string= "menubutton"
               (gtk:widget-class-css-name "GtkMenuButton")))
  ;; Check CSS classes
  (is (equal '("popup")
             (gtk:widget-css-classes (make-instance 'gtk:menu-button))))
  ;; Check accessible role
  (is (eq :button (gtk:widget-class-accessible-role "GtkMenuButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkMenuButton" GTK-MENU-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER "gtk_menu_button_get_type")
                               ((ACTIVE GTK-MENU-BUTTON-ACTIVE "active"
                                 "gboolean" T T)
                                (ALWAYS-SHOW-ARROW
                                 GTK-MENU-BUTTON-ALWAYS-SHOW-ARROW
                                 "always-show-arrow" "gboolean" T T)
                                (CAN-SHRINK GTK-MENU-BUTTON-CAN-SHRINK
                                 "can-shrink" "gboolean" T T)
                                (CHILD GTK-MENU-BUTTON-CHILD "child"
                                 "GtkWidget" T T)
                                (DIRECTION GTK-MENU-BUTTON-DIRECTION
                                 "direction" "GtkArrowType" T T)
                                (HAS-FRAME GTK-MENU-BUTTON-HAS-FRAME
                                 "has-frame" "gboolean" T T)
                                (ICON-NAME GTK-MENU-BUTTON-ICON-NAME
                                 "icon-name" "gchararray" T T)
                                (LABEL GTK-MENU-BUTTON-LABEL "label"
                                 "gchararray" T T)
                                (MENU-MODEL GTK-MENU-BUTTON-MENU-MODEL
                                 "menu-model" "GMenuModel" T T)
                                (POPOVER GTK-MENU-BUTTON-POPOVER "popover"
                                 "GtkPopover" T T)
                                (PRIMARY GTK-MENU-BUTTON-PRIMARY "primary"
                                 "gboolean" T T)
                                (USE-UNDERLINE GTK-MENU-BUTTON-USE-UNDERLINE
                                 "use-underline" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkMenuButton"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-menu-button-properties
  (let ((button (make-instance 'gtk:menu-button)))
; gtk:menu-button-active is present since 4.10
    (is-false (gtk:menu-button-active button))
    (is-false (gtk:menu-button-always-show-arrow button))
    (is-false (gtk:menu-button-can-shrink button))
    (is-false (gtk:menu-button-child button))
    (is (eq :down (gtk:menu-button-direction button)))
    (is-true (gtk:menu-button-has-frame button))
    (is-false (gtk:menu-button-icon-name button))
    (is-false (gtk:menu-button-label button))
    (is-false (gtk:menu-button-menu-model button))
    (is-false (gtk:menu-button-popover button))
    (is-false (gtk:menu-button-primary button))
    (is-false (gtk:menu-button-use-underline button))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

;;; --- Functions --------------------------------------------------------------

;;;     gtk_menu_button_new

(test gtk-menu-button-new
  (let ((button (gtk:menu-button-new)))
    (is-false (gtk:menu-button-active button))
    (is-false (gtk:menu-button-always-show-arrow button))
    (is-false (gtk:menu-button-can-shrink button))
    (is-false (gtk:menu-button-child button))
    (is (eq :down (gtk:menu-button-direction button)))
    (is-true (gtk:menu-button-has-frame button))
    (is-false (gtk:menu-button-icon-name button))
    (is-false (gtk:menu-button-label button))
    (is-false (gtk:menu-button-menu-model button))
    (is-false (gtk:menu-button-popover button))
    (is-false (gtk:menu-button-primary button))
    (is-false (gtk:menu-button-use-underline button))))

;;;     gtk_menu_button_popup
;;;     gtk_menu_button_popdown

;;;     GtkMenuButtonCreatePopupFunc
;;;     gtk_menu_button_set_create_popup_func

;;; 2024-4-20
