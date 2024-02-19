(in-package :gtk-test)

(def-suite gtk-shortcuts-window :in gtk-suite)
(in-suite gtk-shortcuts-window)

;;; --- Types and Values -------------------------------------------------------

;;; GtkShortcutsWindow

(test gtk-shortcuts-window-class
  ;; Type check
  (is (g:type-is-object "GtkShortcutsWindow"))
  ;; Check the registered name
  (is (eq 'gtk:shortcuts-window
          (glib:symbol-for-gtype "GtkShortcutsWindow")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkShortcutsWindow")
          (g:gtype (cffi:foreign-funcall "gtk_shortcuts_window_get_type"
                                         :size))))
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
  ;; CSS classes
  (is (equal '("background" "csd" "shortcuts")
             (gtk:widget-css-classes (make-instance 'gtk:shortcuts-window))))
  ;; Accessible role
  (is (eq :generic (gtk:widget-class-accessible-role "GtkShortcutsWindow")))
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

;;; 2024-2-18
