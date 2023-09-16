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

;;; --- 2023-9-13 --------------------------------------------------------------
