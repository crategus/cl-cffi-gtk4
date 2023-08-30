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
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkShortcutsWindow")))
  (is (string=
"[window.background.csd:dir(ltr)]
  box.vertical:dir(ltr)
    searchbar:dir(ltr)
      revealer:dir(ltr)
        box:dir(ltr)
          entry.search:dir(ltr)
            image:dir(ltr)
            text:dir(ltr)
              placeholder:dir(ltr)
              undershoot.left:dir(ltr)
              undershoot.right:dir(ltr)
            image:dir(ltr)
          [button.close.image-button:dir(ltr)]
            image:dir(ltr)
    stack:dir(ltr)
      scrolledwindow:dir(ltr)
        viewport:dir(ltr)
          box.shortcuts-search-results.vertical:dir(ltr)
            box.vertical:dir(ltr)
            box.vertical:dir(ltr)
        scrollbar.bottom.horizontal:dir(ltr)
          range.horizontal:dir(ltr)
            trough:dir(ltr)
              slider:dir(ltr)
        scrollbar.right.vertical:dir(ltr)
          range.vertical:dir(ltr)
            trough:dir(ltr)
              slider:dir(ltr)
        overshoot.left:dir(ltr)
        undershoot.left:dir(ltr)
        overshoot.right:dir(ltr)
        undershoot.right:dir(ltr)
        overshoot.top:dir(ltr)
        undershoot.top:dir(ltr)
        overshoot.bottom:dir(ltr)
        undershoot.bottom:dir(ltr)
        junction:dir(ltr)
      grid.dim-label.horizontal:dir(ltr)
        image:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
  headerbar.titlebar:dir(ltr)
    windowhandle:dir(ltr)
      box:dir(ltr)
        box.horizontal.start:dir(ltr)
          [windowcontrols.empty.start:dir(ltr)]
          button.image-button.toggle:dir(ltr)
            image:dir(ltr)
        stack:dir(ltr)
          label.title:dir(ltr)
          label.title:dir(ltr)
          menubutton.flat.popup:dir(ltr)
            button.image-button.toggle:dir(ltr)
              arrow.down:dir(ltr)
            [popover.background:dir(ltr)]
              contents:dir(ltr)
                list:dir(ltr)
              arrow:dir(ltr)
        box.end.horizontal:dir(ltr)
          windowcontrols.end:dir(ltr)
            button.minimize:dir(ltr)
              image:dir(ltr)
            button.close:dir(ltr)
              image:dir(ltr)
"
               (print-style-context "GtkShortcutsWindow")))
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

;;; --- 2023-8-28 --------------------------------------------------------------
