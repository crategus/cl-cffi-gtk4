(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-shortcuts-widgets)
(in-suite gtk-shortcuts-window)

;;; --- Types and Values -------------------------------------------------------

;;; GtkShortcutsWindow

(test gtk-shortcuts-window-class
  ;; Check type
  (is (g:type-is-object "GtkShortcutsWindow"))
  ;; Check registered name
  (is (eq 'gtk:shortcuts-window
          (glib:symbol-for-gtype "GtkShortcutsWindow")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkShortcutsWindow")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkShortcutsWindow")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkShortcutsWindow")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (glib-test:list-interfaces "GtkShortcutsWindow")))
  ;; Check properties
  (is (equal '("section-name" "view-name")
             (glib-test:list-properties "GtkShortcutsWindow")))
  ;; Check signals
  (is (equal '("close" "search")
             (glib-test:list-signals "GtkShortcutsWindow")))
  ;; Check CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkShortcutsWindow")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkShortcutsWindow" GTK:SHORTCUTS-WINDOW
                      (:SUPERCLASS GTK:WINDOW
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                        "GtkNative" "GtkRoot" "GtkShortcutManager")
                       :TYPE-INITIALIZER "gtk_shortcuts_window_get_type")
                      ((SECTION-NAME SHORTCUTS-WINDOW-SECTION-NAME
                        "section-name" "gchararray" T T)
                       (VIEW-NAME SHORTCUTS-WINDOW-VIEW-NAME
                        "view-name" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkShortcutsWindow"))))

;;; --- Signals ----------------------------------------------------------------

;;;     close

(test gtk-shortcuts-window-close-signal
  (let* ((name "close")
         (gtype (g:gtype "GtkShortcutsWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     search

(test gtk-shortcuts-window-search-signal
  (let* ((name "search")
         (gtype (g:gtype "GtkShortcutsWindow"))
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (eq gtype (g:signal-query-owner-type query)))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (eq (g:gtype "void") (g:signal-query-return-type query)))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Properties -------------------------------------------------------------

(test gtk-shortcuts-window-properties
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory (window)
      ;; Create shortcuts window
      (is (typep (setf window
                       (make-instance 'gtk:shortcuts-window)) 'gtk:shortcuts-window))
      ;; Property SECTION-NAME
      (is (string= "internal-search"
                   (gtk:shortcuts-window-section-name window)))
      ;; Property VIEW-NAME
      (is-false (gtk:shortcuts-window-view-name window))
      ;; Destroy shortcuts window
      (is-false (gtk:window-destroy window)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_shortcuts_window_add_section

;;; 2026-07-24
