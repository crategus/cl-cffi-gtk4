(in-package :gtk-test)

(def-suite gtk-emoji-chooser :in gtk-suite)
(in-suite gtk-emoji-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEmojiChooser

(test gtk-emoji-chooser-class
  ;; Type check
  (is (g:type-is-object "GtkEmojiChooser"))
  ;; Check the registered name
  (is (eq 'gtk:emoji-chooser
          (glib:symbol-for-gtype "GtkEmojiChooser")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEmojiChooser")
          (g:gtype (cffi:foreign-funcall "gtk_emoji_chooser_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkPopover")
          (g:type-parent "GtkEmojiChooser")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEmojiChooser")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (list-interfaces "GtkEmojiChooser")))
  ;; Check the properties
  (is (equal '()
             (list-properties "GtkEmojiChooser")))
  ;; Check the signals
  (is (equal '("emoji-picked")
             (list-signals "GtkEmojiChooser")))
  ;; CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkEmojiChooser")))
  ;; CSS style context
  (is (string=
"[popover.background.emoji-picker:dir(ltr)]
  contents:dir(ltr)
    box.vertical:dir(ltr)
      box.emoji-searchbar.horizontal:dir(ltr)
        entry.search:dir(ltr)
          image:dir(ltr)
          text:dir(ltr)
            undershoot.left:dir(ltr)
            undershoot.right:dir(ltr)
          image:dir(ltr)
      stack:dir(ltr)
        box.vertical:dir(ltr)
          scrolledwindow.view:dir(ltr)
            viewport:dir(ltr)
              box.vertical:dir(ltr)
                flowbox.horizontal:dir(ltr)
                  emoji:dir(ltr)
                    label:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
                label:dir(ltr)
                flowbox.horizontal:dir(ltr)
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
          flowbox.emoji-toolbar.horizontal:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
            flowboxchild:dir(ltr)
              button.emoji-section.flat.image-button:dir(ltr)
                image:dir(ltr)
        grid.dim-label.horizontal:dir(ltr)
          image.dim-label:dir(ltr)
          label:dir(ltr)
          label.dim-label:dir(ltr)
  arrow:dir(ltr)
"
               (print-style-context "GtkEmojiChooser")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEmojiChooser" GTK-EMOJI-CHOOSER
                               (:SUPERCLASS GTK-POPOVER :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkNative"
                                 "GtkShortcutManager")
                                :TYPE-INITIALIZER "gtk_emoji_chooser_get_type")
                               NIL)
             (gobject:get-g-type-definition "GtkEmojiChooser"))))

;;; --- Signals ----------------------------------------------------------------

;;;     emoji-picked

;;; --- Functions --------------------------------------------------------------

;;;     gtk_emoji_chooser_new

(test gtk-emoji-chooser-new
  (is (typep (gtk:emoji-chooser-new) 'gtk:emoji-chooser)))

;;; --- 2023-8-28 --------------------------------------------------------------
