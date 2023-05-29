(in-package :gtk-test)

(def-suite gtk-shortcut-manager :in gtk-suite)
(in-suite gtk-shortcut-manager)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutManager

(test shortcut-manager
  ;; Type check
  (is (g:type-is-interface "GtkShortcutManager"))
  ;; Check the registered name
  (is (eq 'gtk:shortcut-manager
          (glib:symbol-for-gtype "GtkShortcutManager")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutManager")
          (g:gtype (cffi:foreign-funcall "gtk_shortcut_manager_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "GtkShortcutManager")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "GtkShortcutManager" GTK-SHORTCUT-MANAGER
                    (:EXPORT T :TYPE-INITIALIZER
                     "gtk_shortcut_manager_get_type"))
             (gobject:get-g-type-definition "GtkShortcutManager"))))

;;; --- 2023-5-29 --------------------------------------------------------------
