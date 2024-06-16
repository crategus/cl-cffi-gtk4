(in-package :gtk-test)

(def-suite gtk-popover-menu :in gtk-suite)
(in-suite gtk-popover-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverMenuFlags

;;;     GtkPopoverMenu

(test gtk-popover-menu-class
  ;; Check type
  (is (g:type-is-object "GtkPopoverMenu"))
  ;; Check registered name
  (is (eq 'gtk:popover-menu
          (glib:symbol-for-gtype "GtkPopoverMenu")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopoverMenu")
          (g:gtype (cffi:foreign-funcall "gtk_popover_menu_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkPopover")
          (g:type-parent "GtkPopoverMenu")))
  ;; Check children
  (is (equal '()
             (list-children "GtkPopoverMenu")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (list-interfaces "GtkPopoverMenu")))
  ;; Check properties
  (is (equal '("flags" "menu-model" "visible-submenu")
             (list-properties "GtkPopoverMenu")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkPopoverMenu")))
  ;; Check CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopoverMenu")))
  ;; Check accessible role
  (is (eq :menu (gtk:widget-class-accessible-role "GtkPopoverMenu")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPopoverMenu" GTK-POPOVER-MENU
                               (:SUPERCLASS GTK-POPOVER :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkNative"
                                 "GtkShortcutManager")
                                :TYPE-INITIALIZER "gtk_popover_menu_get_type")
                               ((FLAGS GTK-POPOVER-MENU-FLAGS "flags"
                                 "GtkPopoverMenuFlags" T T)
                                (MENU-MODEL GTK-POPOVER-MENU-MENU-MODEL
                                 "menu-model" "GMenuModel" T T)
                                (VISIBLE-SUBMENU
                                 GTK-POPOVER-MENU-VISIBLE-SUBMENU
                                 "visible-submenu" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkPopoverMenu"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-popover-menu-properties
  (let ((popover (make-instance 'gtk:popover-menu)))
    (is-false (gtk:popover-menu-flags popover))
    (is-false (gtk:popover-menu-menu-model popover))
    (is-false (gtk:popover-menu-visible-submenu popover))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_menu_new_from_model
;;;     gtk_popover_menu_new_from_model_full
;;;     gtk_popover_menu_add_child
;;;     gtk_popover_menu_remove_child

;;; 2024-5-28
