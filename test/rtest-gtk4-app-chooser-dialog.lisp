(in-package :gtk-test)

(def-suite gtk-app-chooser-dialog :in gtk-suite)
(in-suite gtk-app-chooser-dialog)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserDialog

(test gtk-app-chooser-dialog-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserDialog"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-dialog
          (glib:symbol-for-gtype "GtkAppChooserDialog")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooserDialog")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_dialog_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkDialog")
          (g:type-parent "GtkAppChooserDialog")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserDialog")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" "GtkNative"
               "GtkShortcutManager" "GtkRoot" "GtkAppChooser")
             (list-interfaces "GtkAppChooserDialog")))
  ;; Check the properties
  (is (equal '("content-type" "gfile" "heading")
             (list-properties "GtkAppChooserDialog")))
  ;; Check the signals
  (is (equal '()
             (list-signals "GtkAppChooserDialog")))
  ;; CSS information
  (is (string= "window"
               (gtk:widget-class-css-name "GtkAppChooserDialog")))
  #-windows
  (is (string=
"[window.background.csd.dialog:dir(ltr)]
  box.dialog-vbox.vertical:dir(ltr)
    box.vertical:dir(ltr)
      searchbar:dir(ltr)
        revealer:dir(ltr)
          box:dir(ltr)
            entry.search:disabled:dir(ltr)
              image:disabled:dir(ltr)
              text:disabled:dir(ltr)
                undershoot.left:disabled:dir(ltr)
                undershoot.right:disabled:dir(ltr)
              image:disabled:dir(ltr)
            [button.close.image-button:dir(ltr)]
              image:dir(ltr)
      box.vertical:dir(ltr)
        [label:dir(ltr)]
        appchooser:dir(ltr)
          overlay:dir(ltr)
            scrolledwindow.frame:dir(ltr)
              treeview.view:dir(ltr)
                header:dir(ltr)
                  button:dir(ltr)
                    box.horizontal:dir(ltr)
                      box.horizontal:dir(ltr)
                        label:dir(ltr)
                      sort-indicator:dir(ltr)
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
            box.vertical:dir(ltr)
              image.dim-label:dir(ltr)
              label.dim-label:dir(ltr)
        button.text-button:dir(ltr)
          label:dir(ltr)
        [button.text-button:dir(ltr)]
          label:dir(ltr)
    [box.dialog-action-box.horizontal:dir(ltr)]
      box.dialog-action-area.horizontal:dir(ltr)
  headerbar.titlebar:dir(ltr)
    windowhandle:dir(ltr)
      box:dir(ltr)
        box.horizontal.start:dir(ltr)
          button.text-button:dir(ltr)
            label:dir(ltr)
        box.vertical:dir(ltr)
          label.title:dir(ltr)
        box.end.horizontal:dir(ltr)
          button.image-button.toggle:disabled:dir(ltr)
            image:disabled:dir(ltr)
          button.default.suggested-action.text-button:disabled:dir(ltr)
            label:disabled:dir(ltr)
"
               (print-style-context "GtkAppChooserDialog")))
  #+windows
  (is (string=
"[window.background.dialog:dir(ltr)]
"
               (print-style-context "GtkAppChooserDialog")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAppChooserDialog"
                                             GTK-APP-CHOOSER-DIALOG
                       (:SUPERCLASS GTK-DIALOG :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkAppChooser" "GtkBuildable"
                         "GtkConstraintTarget" "GtkNative" "GtkRoot"
                         "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_app_chooser_dialog_get_type")
                       ((GFILE GTK-APP-CHOOSER-DIALOG-GFILE "gfile" "GFile" T
                         NIL)
                        (HEADING GTK-APP-CHOOSER-DIALOG-HEADING "heading"
                         "gchararray" T T)))
             (gobject:get-g-type-definition "GtkAppChooserDialog"))))

;;; --- Properties -------------------------------------------------------------

;;;     gfile
;;;     heading

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget

;;; --- 2023-5-29 --------------------------------------------------------------
