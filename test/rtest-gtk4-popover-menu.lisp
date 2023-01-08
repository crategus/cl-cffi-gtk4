(in-package :gtk-test)

(def-suite gtk-popover-menu :in gtk-suite)
(in-suite gtk-popover-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverMenuFlags

;;;     GtkPopoverMenu

(test popover-menu-class
  ;; Type check
  (is (g:type-is-object "GtkPopoverMenu"))
  ;; Check the registered name
  (is (eq 'gtk:popover-menu
          (gobject:symbol-for-gtype "GtkPopoverMenu")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPopoverMenu")
          (g:gtype (foreign-funcall "gtk_popover_menu_get_type" :size))))
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
  ;; CSS information
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopoverMenu")))
  (is (string=
"[popover.background.menu:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:popover-menu))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPopoverMenu" GTK-POPOVER-MENU
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

;;; 2022-11-14
