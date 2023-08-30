(in-package :gtk-test)

(def-suite gtk-shortcuts-section :in gtk-suite)
(in-suite gtk-shortcuts-section)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsSection

(test gtk-shortcuts-section-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsSection"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-section
          (glib:symbol-for-gtype "GtkShortcutsSection")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsSection")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_section_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkShortcutsSection")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkShortcutsSection")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (list-interfaces "GtkShortcutsSection")))
  ;; Check the properties
  (is (equal '("max-height" "section-name" "title" "view-name")
             (list-properties "GtkShortcutsSection")))
  ;; Check the signals
  (is (equal '("change-current-page")
             (list-signals "GtkShortcutsSection")))
  ;; CSS name
  (is (string= "shortcuts-section"
               (gtk:widget-class-css-name "GtkShortcutsSection")))
  ;; CSS style context
  (is (string=
"shortcuts-section.vertical:dir(ltr)
  stack:dir(ltr)
  box:dir(ltr)
    [stackswitcher:dir(ltr)]
    [button.text-button:dir(ltr)]
      label:dir(ltr)
"
               (print-style-context "GtkShortcutsSection")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkShortcutsSection"
                                             GTK-SHORTCUTS-SECTION
                               (:SUPERCLASS GTK-BOX :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkOrientable")
                                :TYPE-INITIALIZER
                                "gtk_shortcuts_section_get_type")
                               ((MAX-HEIGHT GTK-SHORTCUTS-SECTION-MAX-HEIGHT
                                 "max-height" "guint" T T)
                                (SECTION-NAME
                                 GTK-SHORTCUTS-SECTION-SECTION-NAME
                                 "section-name" "gchararray" T T)
                                (TITLE GTK-SHORTCUTS-SECTION-TITLE "title"
                                 "gchararray" T T)
                                (VIEW-NAME GTK-SHORTCUTS-SECTION-VIEW-NAME
                                 "view-name" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkShortcutsSection"))))

;;; --- Properties -------------------------------------------------------------

;;;     max-height
;;;     section-name
;;;     title
;;;     view-name

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page

;;; --- 2023-8-28 --------------------------------------------------------------
