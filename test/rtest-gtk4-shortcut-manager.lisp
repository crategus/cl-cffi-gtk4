(in-package :gtk-test)

(def-suite gtk-shortcut-manager :in gtk-keyboard-shortcuts)
(in-suite gtk-shortcut-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutManager

(test shortcut-manager
  ;; Check type
  (is (g:type-is-interface "GtkShortcutManager"))
  ;; Check registered name
  (is (eq 'gtk:shortcut-manager
          (glib:symbol-for-gtype "GtkShortcutManager")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutManager")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_manager_get_type" :size))))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "GtkShortcutManager")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkShortcutManager"
                                         GTK:SHORTCUT-MANAGER
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_shortcut_manager_get_type"))
             (gobject:get-gtype-definition "GtkShortcutManager"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcut_manager_add_controller
;;;     gtk_shortcut_manager_remove_controller

;;; 2024-11-1
