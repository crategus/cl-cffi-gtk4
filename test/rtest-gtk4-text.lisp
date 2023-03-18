(in-package :gtk-test)

(def-suite gtk-text :in gtk-suite)
(in-suite gtk-text)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkText

(test text-class
  ;; Type check
  (is (g:type-is-object "GtkText"))
  ;; Check the registered name
  (is (eq 'gtk:text
          (gobject:symbol-for-gtype "GtkText")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkText")
          (g:gtype (cffi:foreign-funcall "gtk_text_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkText")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkText")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable")
             (list-interfaces "GtkText")))
  ;; Check the class properties
  (is (equal '("activates-default" "attributes" "buffer" "cursor-position"
               "editable" "enable-emoji-completion" "enable-undo" "extra-menu"
               "im-module" "input-hints" "input-purpose" "invisible-char"
               "invisible-char-set" "max-length" "max-width-chars"
               "overwrite-mode" "placeholder-text" "propagate-text-width"
               "scroll-offset" "selection-bound" "tabs" "text"
               "truncate-multiline" "visibility" "width-chars" "xalign")
             (list-properties "GtkText")))
  ;; Check the list of signals
  (is (equal '("activate" "backspace" "copy-clipboard" "cut-clipboard"
               "delete-from-cursor" "insert-at-cursor" "insert-emoji"
               "move-cursor" "paste-clipboard" "preedit-changed"
               "toggle-overwrite")
             (list-signals "GtkText")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkText" GTK-TEXT
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkEditable")
                        :TYPE-INITIALIZER "gtk_text_get_type")
                       ((ACTIVATES-DEFAULT GTK-TEXT-ACTIVATES-DEFAULT
                         "activates-default" "gboolean" T T)
                        (ATTRIBUTES GTK-TEXT-ATTRIBUTES "attributes"
                         "PangoAttrList" T T)
                        (BUFFER GTK-TEXT-BUFFER "buffer" "GtkEntryBuffer" T T)
                        (ENABLE-EMOJI-COMPLETION
                         GTK-TEXT-ENABLE-EMOJI-COMPLETION
                         "enable-emoji-completion" "gboolean" T T)
                        (EXTRA-MENU GTK-TEXT-EXTRA-MENU "extra-menu"
                         "GMenuModel" T T)
                        (IM-MODULE GTK-TEXT-IM-MODULE "im-module" "gchararray"
                         T T)
                        (INPUT-HINTS GTK-TEXT-INPUT-HINTS "input-hints"
                         "GtkInputHints" T T)
                        (INPUT-PURPOSE GTK-TEXT-INPUT-PURPOSE "input-purpose"
                         "GtkInputPurpose" T T)
                        (INVISIBLE-CHAR GTK-TEXT-INVISIBLE-CHAR
                         "invisible-char" "guint" T T)
                        (INVISIBLE-CHAR-SET GTK-TEXT-INVISIBLE-CHAR-SET
                         "invisible-char-set" "gboolean" T T)
                        (MAX-LENGTH GTK-TEXT-MAX-LENGTH "max-length" "gint" T
                         T)
                        (OVERWRITE-MODE GTK-TEXT-OVERWRITE-MODE
                         "overwrite-mode" "gboolean" T T)
                        (PLACEHOLDER-TEXT GTK-TEXT-PLACEHOLDER-TEXT
                         "placeholder-text" "gchararray" T T)
                        (PROPAGATE-TEXT-WIDTH GTK-TEXT-PROPAGATE-TEXT-WIDTH
                         "propagate-text-width" "gboolean" T T)
                        (SCROLL-OFFSET GTK-TEXT-SCROLL-OFFSET "scroll-offset"
                         "gint" T NIL)
                        (TABS GTK-TEXT-TABS "tabs" "PangoTabArray" T T)
                        (TRUNCATE-MULTILINE GTK-TEXT-TRUNCATE-MULTILINE
                         "truncate-multiline" "gboolean" T T)
                        (VISIBILITY GTK-TEXT-VISIBILITY "visibility" "gboolean"
                         T T)))
             (gobject:get-g-type-definition "GtkText"))))

;;; --- Properties -------------------------------------------------------------

;;;     activates-default
;;;     attributes
;;;     buffer
;;;     enable-emoji-completion
;;;     extra-menu
;;;     im-module
;;;     input-hints
;;;     input-purpose
;;;     invisible-char
;;;     invisible-char-set
;;;     max-length
;;;     overwrite-mode
;;;     placeholder-text
;;;     propagate-text-width
;;;     scroll-offset
;;;     tabs
;;;     truncate-multiline
;;;     visibility

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     paste-clipboard
;;;     preedit-changed
;;;     toggle-overwrite

;;; --- Functions --------------------------------------------------------------

;;;     gtk_text_new
;;;     gtk_text_new_with_buffer
;;;     gtk_text_unset_invisible_char
;;;     gtk_text_get_text_length
;;;     gtk_text_grab_focus_without_selecting

;;; --- 2023-3-18 --------------------------------------------------------------
