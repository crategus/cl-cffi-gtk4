(in-package :gtk-test)

(def-suite gtk-page-setup-unix-dialog :in gtk-suite)
(in-suite gtk-page-setup-unix-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPageSetupUnixDialog

(test gtk-page-setup-unix-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkPageSetupUnixDialog"))
  ;; Check the registered name
  (is (eq 'gtk:page-setup-unix-dialog
          (glib:symbol-for-gtype "GtkPageSetupUnixDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPageSetupUnixDialog")
          (g:gtype (cffi:foreign-funcall "gtk_page_setup_unix_dialog_get_type" 
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkPageSetupUnixDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkPageSetupUnixDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot")
             (list-interfaces "GtkPageSetupUnixDialog")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkPageSetupUnixDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkPageSetupUnixDialog")))
  ;; CSS name
  (is (string= "window"
               (gtk:widget-class-css-name "GtkPageSetupUnixDialog")))
  ;; CSS style context
  (is (string=
"[window.background.csd.dialog:dir(ltr)]
  box.dialog-vbox.vertical:dir(ltr)
    box.vertical:dir(ltr)
      grid.horizontal:dir(ltr)
        label:dir(ltr)
        dropdown:dir(ltr)
          button.toggle:dir(ltr)
            box.horizontal:dir(ltr)
              stack:dir(ltr)
                label:dir(ltr)
                row.activatable:dir(ltr)
                  label:dir(ltr)
              arrow:dir(ltr)
          [popover.background.menu:dir(ltr)]
            contents:dir(ltr)
              box.vertical:dir(ltr)
                [box.dropdown-searchbar.horizontal:dir(ltr)]
                  entry.search:dir(ltr)
                    image:dir(ltr)
                    text:dir(ltr)
                      placeholder:dir(ltr)
                      undershoot.left:dir(ltr)
                      undershoot.right:dir(ltr)
                    image:dir(ltr)
                scrolledwindow:dir(ltr)
                  listview.view:dir(ltr)
                    row.activatable:selected:dir(ltr)
                      label:dir(ltr)
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
            arrow:dir(ltr)
        label:dir(ltr)
        dropdown:dir(ltr)
          button.toggle:dir(ltr)
            box.horizontal:dir(ltr)
              stack:dir(ltr)
                label:dir(ltr)
                row.activatable:dir(ltr)
                  label:dir(ltr)
              arrow:dir(ltr)
          [popover.background.menu:dir(ltr)]
            contents:dir(ltr)
              box.vertical:dir(ltr)
                [box.dropdown-searchbar.horizontal:dir(ltr)]
                  entry.search:dir(ltr)
                    image:dir(ltr)
                    text:dir(ltr)
                      placeholder:dir(ltr)
                      undershoot.left:dir(ltr)
                      undershoot.right:dir(ltr)
                    image:dir(ltr)
                scrolledwindow:dir(ltr)
                  listview.view:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:selected:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable:dir(ltr)
                      label:dir(ltr)
                    row.activatable.separator:dir(ltr)
                      label:dir(ltr)
                    row.activatable.separator:dir(ltr)
                      label:dir(ltr)
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
            arrow:dir(ltr)
        label:dir(ltr)
        label:dir(ltr)
        checkbutton.text-button:dir(ltr):checked
          radio:dir(ltr):checked
          label:dir(ltr)
        checkbutton.text-button:dir(ltr):checked
          radio:dir(ltr):checked
          label:dir(ltr)
        checkbutton.text-button:dir(ltr):checked
          radio:dir(ltr):checked
          label:dir(ltr)
        checkbutton.text-button:dir(ltr)
          radio:dir(ltr)
          label:dir(ltr)
    [box.dialog-action-box.horizontal:dir(ltr)]
      box.dialog-action-area.horizontal:dir(ltr)
  headerbar.titlebar:dir(ltr)
    windowhandle:dir(ltr)
      box:dir(ltr)
        box.horizontal.start:dir(ltr)
          button.text-button:dir(ltr)
            label:dir(ltr)
        label.title:dir(ltr)
        box.end.horizontal:dir(ltr)
          button.default.suggested-action.text-button:dir(ltr)
            label:dir(ltr)
"
               (print-style-context "GtkPageSetupUnixDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPageSetupUnixDialog"
                               GTK-PAGE-SETUP-UNIX-DIALOG
                               (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkNative" "GtkRoot"
                                 "GtkShortcutManager")
                                :TYPE-INITIALIZER
                                "gtk_page_setup_unix_dialog_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkPageSetupUnixDialog"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_page_setup_unix_dialog_new
;;;     gtk_page_setup_unix_dialog_set_page_setup
;;;     gtk_page_setup_unix_dialog_get_page_setup
;;;     gtk_page_setup_unix_dialog_set_print_settings
;;;     gtk_page_setup_unix_dialog_get_print_settings

;;; --- 2023-8-28 --------------------------------------------------------------
