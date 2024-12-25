(in-package :gtk-test)

(def-suite gtk-shortcuts-section :in gtk-shortcuts-widgets)
(in-suite gtk-shortcuts-section)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkShortcutsSection

(test gtk-shortcuts-section-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutsSection"))
  ;; Check registered name
  (is (eq 'gtk:shortcuts-section
          (glib:symbol-for-gtype "GtkShortcutsSection")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutsSection")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_section_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkBox")
          (g:type-parent "GtkShortcutsSection")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcutsSection")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkOrientable")
             (glib-test:list-interfaces "GtkShortcutsSection")))
  ;; Check properties
  (is (equal '("max-height" "section-name" "title" "view-name")
             (glib-test:list-properties "GtkShortcutsSection")))
  ;; Check signals
  (is (equal '("change-current-page")
             (glib-test:list-signals "GtkShortcutsSection")))
  ;; Check CSS name
  (is (string= "shortcuts-section"
               (gtk:widget-class-css-name "GtkShortcutsSection")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkShortcutsSection")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutsSection"
                                      GTK:SHORTCUTS-SECTION
                      (:SUPERCLASS GTK:BOX
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkOrientable")
                       :TYPE-INITIALIZER "gtk_shortcuts_section_get_type")
                      ((MAX-HEIGHT SHORTCUTS-SECTION-MAX-HEIGHT
                        "max-height" "guint" T T)
                       (SECTION-NAME SHORTCUTS-SECTION-SECTION-NAME
                        "section-name" "gchararray" T T)
                       (TITLE SHORTCUTS-SECTION-TITLE "title" "gchararray" T T)
                       (VIEW-NAME SHORTCUTS-SECTION-VIEW-NAME
                        "view-name" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkShortcutsSection"))))

;;; --- Properties -------------------------------------------------------------

;;;     max-height
;;;     section-name
;;;     title
;;;     view-name

(test gtk-shortcuts-section-properties
  (let ((section (make-instance 'gtk:shortcuts-section)))
    (is (= 15 (gtk:shortcuts-section-max-height section)))
    (is-false (gtk:shortcuts-section-section-name section))
    (is-false (gtk:shortcuts-section-title section))
    (is-false (gtk:shortcuts-section-view-name section))))

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page

(test gtk-shortcuts-section-change-current-page-signal
  (let ((query (g:signal-query (g:signal-lookup "change-current-page"
                                                "GtkShortcutsSection"))))
    (is (string= "change-current-page" (g:signal-query-signal-name query)))
    (is (string= "GtkShortcutsSection"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "gboolean" (g:type-name (g:signal-query-return-type query))))
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; 2024-9-20
