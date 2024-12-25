(in-package :gtk-test)

(def-suite gtk-popover-menu-bar :in gtk-popover-widgets)
(in-suite gtk-popover-menu-bar)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopoverMenuBar

(test gtk-popover-menu-bar-class
  ;; Check type
  (is (g:type-is-object "GtkPopoverMenuBar"))
  ;; Check registered name
  (is (eq 'gtk:popover-menu-bar
          (glib:symbol-for-gtype "GtkPopoverMenuBar")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopoverMenuBar")
          (g:gtype (cffi:foreign-funcall "gtk_popover_menu_bar_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPopoverMenuBar")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkPopoverMenuBar")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
             (glib-test:list-interfaces "GtkPopoverMenuBar")))
  ;; Check properties
  (is (equal '("menu-model")
             (glib-test:list-properties "GtkPopoverMenuBar")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkPopoverMenuBar")))
  ;; Check CSS name
  (is (string= "menubar"
               (gtk:widget-class-css-name "GtkPopoverMenuBar")))
  ;; Check accessible role
  (is (eq :menu-bar (gtk:widget-class-accessible-role "GtkPopoverMenuBar")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPopoverMenuBar" GTK:POPOVER-MENU-BAR
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_popover_menu_bar_get_type")
                       ((MENU-MODEL POPOVER-MENU-BAR-MENU-MODEL
                         "menu-model" "GMenuModel" T T)))
             (gobject:get-gtype-definition "GtkPopoverMenuBar"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-popover-menu-bar-properties
  (let ((popover (make-instance 'gtk:popover-menu-bar)))
    (is-false (gtk:popover-menu-bar-menu-model popover))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_menu_bar_new_from_model
;;;     gtk_popover_menu_bar_add_child
;;;     gtk_popover_menu_bar_remove_child

;;; 2024-10-26
