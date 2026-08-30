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

;;; --- Signals ----------------------------------------------------------------

;;;     change-current-page

(test gtk-shortcuts-section-change-current-page-signal
  (let* ((name "change-current-page")
         (gtype (g:gtype "GtkShortcutsSection"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "gboolean") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '("gint")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

;;;     max-height
;;;     section-name
;;;     title
;;;     view-name

(test gtk-shortcuts-section-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (section)
    (is (typep (setf section (make-instance 'gtk:shortcuts-section))
               'gtk:shortcuts-section))
    (is (= 15 (gtk:shortcuts-section-max-height section)))
    (is-false (gtk:shortcuts-section-section-name section))
    (is-false (gtk:shortcuts-section-title section))
    (is-false (gtk:shortcuts-section-view-name section)))))

;;; 2026-07-24
