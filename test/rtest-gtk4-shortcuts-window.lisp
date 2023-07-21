(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-suite)
(in-suite gtk-shortcuts-window)

;;; --- Types and Values -------------------------------------------------------

;;; GtkShortcutsWindow

(test shortcuts-window-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsWindow"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-window
          (glib:symbol-for-gtype "GtkShortcutsWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsWindow")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkShortcutsWindow")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutsWindow")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (list-interfaces "GtkShortcutsWindow")))
  ;; Check the properties
  (is (equal '("section-name" "view-name")
             (list-properties "GtkShortcutsWindow")))
  ;; Check the signals
  (is (equal '("close" "search")
             (list-signals "GtkShortcutsWindow")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  (is (string=
"[window.background.csd:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context
                       (make-instance 'gtk:shortcuts-window))
                   :none)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutsWindow" 
                                             GTK-SHORTCUTS-WINDOW
                       (:SUPERCLASS GTK-WINDOW :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkRoot" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_shortcuts_window_get_type")
                       ((SECTION-NAME GTK-SHORTCUTS-WINDOW-SECTION-NAME
                         "section-name" "gchararray" T T)
                        (VIEW-NAME GTK-SHORTCUTS-WINDOW-VIEW-NAME "view-name"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkShortcutsWindow"))))

;;; --- Properties -------------------------------------------------------------

;;;     section-name
;;;     view-name

;;; --- Signals ----------------------------------------------------------------

;;;     close
;;;     search

;;; --- 2023-5-29 --------------------------------------------------------------
