(in-package :gtk-test)

(def-suite gtk-popover-menu :in gtk-suite)
(in-suite gtk-popover-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverMenuFlags

;;;     GtkPopoverMenu

(test gtk-popover-menu-class
  ;; Type check
  (is (g:type-is-object "GtkPopoverMenu"))
  ;; Check the registered name
  (is (eq 'gtk:popover-menu
          (glib:symbol-for-gtype "GtkPopoverMenu")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPopoverMenu")
          (g:gtype (cffi:foreign-funcall "gtk_popover_menu_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkPopover")
          (g:type-parent "GtkPopoverMenu")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPopoverMenu")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (list-interfaces "GtkPopoverMenu")))
  ;; Check the properties
  (is (equal '("menu-model" "visible-submenu")
             (list-properties "GtkPopoverMenu")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPopoverMenu")))
  ;; CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopoverMenu")))
  ;; CSS classes
  (is (equal '("background" "menu")
             (gtk:widget-css-classes (make-instance 'gtk:popover-menu))))
  ;; Accessible role
  (is (eq :menu (gtk:widget-class-accessible-role "GtkPopoverMenu")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPopoverMenu" GTK-POPOVER-MENU
                       (:SUPERCLASS GTK-POPOVER :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_popover_menu_get_type")
                       ((MENU-MODEL GTK-POPOVER-MENU-MENU-MODEL "menu-model"
                         "GMenuModel" T T)
                        (VISIBLE-SUBMENU GTK-POPOVER-MENU-VISIBLE-SUBMENU
                         "visible-submenu" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkPopoverMenu"))))

;;; --- Properties -------------------------------------------------------------

;;;     menu-model
;;;     visible-submenu

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_menu_new_from_model
;;;     gtk_popover_menu_new_from_model_full
;;;     gtk_popover_menu_add_child
;;;     gtk_popover_menu_remove_child

;;; --- 2023-11-2 --------------------------------------------------------------
