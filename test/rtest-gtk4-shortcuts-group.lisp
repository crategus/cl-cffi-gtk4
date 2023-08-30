(in-package :gtk-test)

(def-suite gtk-shortcuts-group :in gtk-suite)
(in-suite gtk-shortcuts-group)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsGroup

(test gtk-shortcuts-group-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsGroup"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-group
          (glib:symbol-for-gtype "GtkShortcutsGroup")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsGroup")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_group_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkShortcutsGroup")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutsGroup")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkShortcutsGroup")))
  ;; Check the properties
  (is (equal '("accel-size-group" "height" "title" "title-size-group" "view")
             (list-properties "GtkShortcutsGroup")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkShortcutsGroup")))
  ;; CSS name
  (is (string= "shortcuts-group"
               (gtk:widget-class-css-name "GtkShortcutsGroup")))
  ;; CSS style context
  (is (string=
"shortcuts-group.vertical:dir(ltr)
  label:dir(ltr)
"
               (print-style-context "GtkShortcutsGroup")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutsGroup"
                                             GTK-SHORTCUTS-GROUP
                               (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER
                                "gtk_shortcuts_group_get_type")
                               ((ACCEL-SIZE-GROUP
                                 GTK-SHORTCUTS-GROUP-ACCEL-SIZE-GROUP
                                 "accel-size-group" "GtkSizeGroup" NIL T)
                                (HEIGHT GTK-SHORTCUTS-GROUP-HEIGHT "height"
                                 "guint" T NIL)
                                (TITLE GTK-SHORTCUTS-GROUP-TITLE "title"
                                 "gchararray" T T)
                                (TITLE-SIZE-GROUP
                                 GTK-SHORTCUTS-GROUP-TITLE-SIZE-GROUP
                                 "title-size-group" "GtkSizeGroup" NIL T)
                                (VIEW GTK-SHORTCUTS-GROUP-VIEW "view"
                                 "gchararray" T T)))
             (gobject:get-g-type-definition "GtkShortcutsGroup"))))

;;; --- Properties -------------------------------------------------------------

;;;     accel-size-group
;;;     height
;;;     title
;;;     title-size-group
;;;     view

;;; --- 2023-8-28 --------------------------------------------------------------
