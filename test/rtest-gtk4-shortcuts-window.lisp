(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-suite)
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
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type"
                                         :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWindow")
          (g:type-parent "GtkShortcutsWindow")))
  ;; Check children
  (is (equal '()
             (gtk-test:list-children "GtkShortcutsWindow")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (gtk-test:list-interfaces "GtkShortcutsWindow")))
  ;; Check properties
  (is (equal '("section-name" "view-name")
             (gtk-test:list-properties "GtkShortcutsWindow")))
  ;; Check signals
  (is (equal '("close" "search")
             (gtk-test:list-signals "GtkShortcutsWindow")))
  ;; Check CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  ;; Check accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkShortcutsWindow")))
  ;; Check class definition
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

(test gtk-shortcuts-window-properties
  (let ((shortcuts (make-instance 'gtk:shortcuts-window)))
    (is (string= "internal-search"
                 (gtk:shortcuts-window-section-name shortcuts)))
    (is-false (gtk:shortcuts-window-view-name shortcuts))))

;;; --- Signals ----------------------------------------------------------------

;;;     close

(test gtk-shortcuts-window-close-signal
  (let ((query (g:signal-query (g:signal-lookup "close" "GtkShortcutsWindow"))))
    (is (string= "close" (g:signal-query-signal-name query)))
    (is (string= "GtkShortcutsWindow"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;;     search

(test gtk-shortcuts-window-search-signal
  (let ((query (g:signal-query (g:signal-lookup "search" "GtkShortcutsWindow"))))
    (is (string= "search" (g:signal-query-signal-name query)))
    (is (string= "GtkShortcutsWindow"
                 (g:type-name (g:signal-query-owner-type query))))
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))
    (is-false (g:signal-query-signal-detail query))))

;;; 2024-7-3
