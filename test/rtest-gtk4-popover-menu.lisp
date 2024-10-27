(in-package :gtk-test)

(def-suite gtk-popover-menu :in gtk-suite)
(in-suite gtk-popover-menu)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverMenuFlags

(test gtk-popover-menu-flags
  ;; Check type
  (is (g:type-is-flags "GtkPopoverMenuFlags"))
  ;; Check registered name
  (is (eq 'gtk:popover-menu-flags
          (glib:symbol-for-gtype "GtkPopoverMenuFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopoverMenuFlags")
          (g:gtype (cffi:foreign-funcall "gtk_popover_menu_flags_get_type"
                                         :size))))
  ;; Check names
  (is (equal '("GTK_POPOVER_MENU_SLIDING" "GTK_POPOVER_MENU_NESTED")
             (glib-test:list-flags-item-names "GtkPopoverMenuFlags")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-flags-item-values "GtkPopoverMenuFlags")))
  ;; Check nick names
  (is (equal '("sliding" "nested")
             (glib-test:list-flags-item-nicks "GtkPopoverMenuFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GtkPopoverMenuFlags"
                                     GTK:POPOVER-MENU-FLAGS
                       (:EXPORT T
                        :TYPE-INITIALIZER "gtk_popover_menu_flags_get_type")
                       (:SLIDING 0)
                       (:NESTED 1))
             (gobject:get-gtype-definition "GtkPopoverMenuFlags"))))

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
             (glib-test:list-children "GtkPopoverMenu")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (glib-test:list-interfaces "GtkPopoverMenu")))
  ;; Check properties
  (is (equal '("flags" "menu-model" "visible-submenu")
             (glib-test:list-properties "GtkPopoverMenu")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPopoverMenu")))
  ;; Check CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopoverMenu")))
  ;; Check accessible role
  (is (eq :menu (gtk:widget-class-accessible-role "GtkPopoverMenu")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPopoverMenu" GTK:POPOVER-MENU
                      (:SUPERCLASS GTK:POPOVER
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkNative" "GtkShortcutManager")
                       :TYPE-INITIALIZER "gtk_popover_menu_get_type")
                      ((FLAGS POPOVER-MENU-FLAGS
                        "flags" "GtkPopoverMenuFlags" T T)
                       (MENU-MODEL POPOVER-MENU-MENU-MODEL
                        "menu-model" "GMenuModel" T T)
                       (VISIBLE-SUBMENU POPOVER-MENU-VISIBLE-SUBMENU
                        "visible-submenu" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkPopoverMenu"))))

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

;;; 2024-10-26
