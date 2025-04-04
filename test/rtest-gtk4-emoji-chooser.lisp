(in-package :gtk-test)

(def-suite gtk-emoji-chooser :in gtk-selector-widgets)
(in-suite gtk-emoji-chooser)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEmojiChooser

(test gtk-emoji-chooser-class
  ;; Check type
  (is (g:type-is-object "GtkEmojiChooser"))
  ;; Check registered name
  (is (eq 'gtk:emoji-chooser
          (glib:symbol-for-gtype "GtkEmojiChooser")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEmojiChooser")
          (g:gtype (cffi:foreign-funcall "gtk_emoji_chooser_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkPopover")
          (g:type-parent "GtkEmojiChooser")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "GtkEmojiChooser")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (glib-test:list-interfaces "GtkEmojiChooser")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "GtkEmojiChooser")))
  ;; Check signals
  (is (equal '("emoji-picked")
             (glib-test:list-signals "GtkEmojiChooser")))
  ;; Check CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkEmojiChooser")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkEmojiChooser" GTK:EMOJI-CHOOSER
                       (:SUPERCLASS GTK:POPOVER
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_emoji_chooser_get_type")
                       NIL)
             (gobject:get-gtype-definition "GtkEmojiChooser"))))

;;; --- Signals ----------------------------------------------------------------

;;;     emoji-picked

;;; --- Functions --------------------------------------------------------------

;;;     gtk_emoji_chooser_new

(test gtk-emoji-chooser-new
  (is (typep (gtk:emoji-chooser-new) 'gtk:emoji-chooser)))

;;; 2024-9-20
