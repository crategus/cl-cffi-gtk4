(in-package :gtk-test)

(def-suite gtk-app-chooser-button :in gtk-suite)
(in-suite gtk-app-chooser-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAppChooserButton

(test gtk-app-chooser-button-class
  ;; Type check
  (is (g:type-is-object "GtkAppChooserButton"))
  ;; Check the registered name
  (is (eq 'gtk:app-chooser-button
          (glib:symbol-for-gtype "GtkAppChooserButton")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAppChooserButton")
          (g:gtype (cffi:foreign-funcall "gtk_app_chooser_button_get_type"
                                         :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkAppChooserButton")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkAppChooserButton")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkAppChooser")
             (list-interfaces "GtkAppChooserButton")))
  ;; Check the properties
  (is (equal '("content-type" "heading" "modal" "show-default-item"
               "show-dialog-item")
             (list-properties "GtkAppChooserButton")))
  ;; Check the signals
  (is (equal '("activate" "changed" "custom-item-activated")
             (list-signals "GtkAppChooserButton")))
  ;; CSS name
  (is (string= "appchooserbutton"
               (gtk:widget-class-css-name "GtkAppChooserButton")))
  ;; CSS style context
  (is (string=
"appchooserbutton:dir(ltr)
  combobox:dir(ltr)
    box.horizontal.linked:dir(ltr)
      button.combo:disabled:dir(ltr)
        box.horizontal:disabled:dir(ltr)
          cellview:disabled:dir(ltr)
          arrow:disabled:dir(ltr)
    [popover.background.menu:dir(ltr)]
      contents:dir(ltr)
        scrolledwindow:dir(ltr)
          viewport:dir(ltr)
            stack:dir(ltr)
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
"
               (print-style-context "GtkAppChooserButton")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkAppChooserButton"
                                             GTK-APP-CHOOSER-BUTTON
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkAppChooser" "GtkBuildable"
                                 "GtkConstraintTarget")
                                :TYPE-INITIALIZER
                                "gtk_app_chooser_button_get_type")
                               ((HEADING GTK-APP-CHOOSER-BUTTON-HEADING
                                 "heading" "gchararray" T T)
                                (MODAL GTK-APP-CHOOSER-BUTTON-MODAL "modal"
                                 "gboolean" T T)
                                (SHOW-DEFAULT-ITEM
                                 GTK-APP-CHOOSER-BUTTON-SHOW-DEFAULT-ITEM
                                 "show-default-item" "gboolean" T T)
                                (SHOW-DIALOG-ITEM
                                 GTK-APP-CHOOSER-BUTTON-SHOW-DIALOG-ITEM
                                 "show-dialog-item" "gboolean" T T)))
             (gobject:get-g-type-definition "GtkAppChooserButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     heading
;;;     modal
;;;     show-default-item
;;;     show-dialog-item

(test gtk-app-chooser-button-properties
  (let ((button (make-instance 'gtk:app-chooser-button)))
    (is-false (gtk:app-chooser-button-heading button))
    (is-true (gtk:app-chooser-button-modal button))
    (is-false (gtk:app-chooser-button-show-default-item button))
    (is-false (gtk:app-chooser-button-show-dialog-item button))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate                                           Since 4.4
;;;     changed
;;;     custom-item-activated

;;; --- Functions --------------------------------------------------------------

;;;     gtk_app_chooser_button_new

(test gtk-app-chooser-button-new
  (let ((button (gtk:app-chooser-button-new "text/plain")))
    (is (typep button 'gtk:app-chooser-button))
    (is (string= "text/plain" (gtk:app-chooser-content-type button)))))

;;;     gtk_app_chooser_button_append_custom_item
;;;     gtk_app_chooser_button_append_separator
;;;     gtk_app_chooser_button_set_active_custom_item

;;; --- 2023-8-29 --------------------------------------------------------------
