(in-package :gtk-test)

(def-suite gtk-entry :in gtk-suite)
(in-suite gtk-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryIconPosition

(test entry-icon-position
  ;; Check the type
  (is (g:type-is-enum "GtkEntryIconPosition"))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEntryIconPosition")
          (g:gtype (cffi:foreign-funcall "gtk_entry_icon_position_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'gtk:entry-icon-position
          (glib:symbol-for-gtype "GtkEntryIconPosition")))
  ;; Check the names
  (is (equal '("GTK_ENTRY_ICON_PRIMARY" "GTK_ENTRY_ICON_SECONDARY")
             (list-enum-item-name "GtkEntryIconPosition")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "GtkEntryIconPosition")))
  ;; Check the nick names
  (is (equal '("primary" "secondary")
             (list-enum-item-nick "GtkEntryIconPosition")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkEntryIconPosition"
                             GTK-ENTRY-ICON-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_entry_icon_position_get_type")
                             (:PRIMARY 0)
                             (:SECONDARY 1))
             (gobject:get-g-type-definition "GtkEntryIconPosition"))))

;;;     GtkEntry

(test entry-class
  ;; Type check
  (is (g:type-is-object "GtkEntry"))
  ;; Check the registered name
  (is (eq 'gtk:entry
          (glib:symbol-for-gtype "GtkEntry")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkEntry")
          (g:gtype (cffi:foreign-funcall "gtk_entry_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkEntry")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkEntry")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable" "GtkCellEditable")
             (list-interfaces "GtkEntry")))
  ;; Check the properties
  (is (equal '("activates-default" "attributes" "buffer" "completion"
               "cursor-position" "editable" "editing-canceled"
               "enable-emoji-completion" "enable-undo" "extra-menu" "has-frame"
               "im-module" "input-hints" "input-purpose" "invisible-char"
               "invisible-char-set" "max-length" "max-width-chars"
               "overwrite-mode" "placeholder-text" "primary-icon-activatable"
               "primary-icon-gicon" "primary-icon-name" "primary-icon-paintable"
               "primary-icon-sensitive" "primary-icon-storage-type"
               "primary-icon-tooltip-markup" "primary-icon-tooltip-text"
               "progress-fraction" "progress-pulse-step" "scroll-offset"
               "secondary-icon-activatable" "secondary-icon-gicon"
               "secondary-icon-name" "secondary-icon-paintable"
               "secondary-icon-sensitive" "secondary-icon-storage-type"
               "secondary-icon-tooltip-markup" "secondary-icon-tooltip-text"
               "selection-bound" "show-emoji-icon" "tabs" "text" "text-length"
               "truncate-multiline" "visibility" "width-chars" "xalign")
             (list-properties "GtkEntry")))
  ;; Check the signals
  (is (equal '("activate" "icon-press" "icon-release")
             (list-signals "GtkEntry")))
  ;; CSS information
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkEntry")))
  (is (string=
"entry:dir(ltr)
  text:dir(ltr)
    undershoot.left:dir(ltr)
    undershoot.right:dir(ltr)
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:entry))
                   :recurse)))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkEntry" GTK-ENTRY
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkCellEditable"
                         "GtkConstraintTarget" "GtkEditable")
                        :TYPE-INITIALIZER "gtk_entry_get_type")
                       ((ACTIVATES-DEFAULT GTK-ENTRY-ACTIVATES-DEFAULT
                         "activates-default" "gboolean" T T)
                        (ATTRIBUTES GTK-ENTRY-ATTRIBUTES "attributes"
                         "PangoAttrList" T T)
                        (BUFFER GTK-ENTRY-BUFFER "buffer" "GtkEntryBuffer" T T)
                        (COMPLETION GTK-ENTRY-COMPLETION "completion"
                         "GtkEntryCompletion" T T)
                        (ENABLE-EMOJI-COMPLETION
                         GTK-ENTRY-ENABLE-EMOJI-COMPLETION
                         "enable-emoji-completion" "gboolean" T T)
                        (EXTRA-MENU GTK-ENTRY-EXTRA-MENU "extra-menu"
                         "GMenuModel" T T)
                        (HAS-FRAME GTK-ENTRY-HAS-FRAME "has-frame" "gboolean" T
                         T)
                        (IM-MODULE GTK-ENTRY-IM-MODULE "im-module" "gchararray"
                         T T)
                        (INPUT-HINTS GTK-ENTRY-INPUT-HINTS "input-hints"
                         "GtkInputHints" T T)
                        (INPUT-PURPOSE GTK-ENTRY-INPUT-PURPOSE "input-purpose"
                         "GtkInputPurpose" T T)
                        (INVISIBLE-CHAR GTK-ENTRY-INVISIBLE-CHAR
                         "invisible-char" "guint" T T)
                        (INVISIBLE-CHAR-SET GTK-ENTRY-INVISIBLE-CHAR-SET
                         "invisible-char-set" "gboolean" T T)
                        (MAX-LENGTH GTK-ENTRY-MAX-LENGTH "max-length" "gint" T
                         T)
                        (OVERWRITE-MODE GTK-ENTRY-OVERWRITE-MODE
                         "overwrite-mode" "gboolean" T T)
                        (PLACEHOLDER-TEXT GTK-ENTRY-PLACEHOLDER-TEXT
                         "placeholder-text" "gchararray" T T)
                        (PRIMARY-ICON-ACTIVATABLE
                         GTK-ENTRY-PRIMARY-ICON-ACTIVATABLE
                         "primary-icon-activatable" "gboolean" T T)
                        (PRIMARY-ICON-GICON GTK-ENTRY-PRIMARY-ICON-GICON
                         "primary-icon-gicon" "GIcon" T T)
                        (PRIMARY-ICON-NAME GTK-ENTRY-PRIMARY-ICON-NAME
                         "primary-icon-name" "gchararray" T T)
                        (PRIMARY-ICON-PAINTABLE
                         GTK-ENTRY-PRIMARY-ICON-PAINTABLE
                         "primary-icon-paintable" "GdkPaintable" T T)
                        (PRIMARY-ICON-SENSITIVE
                         GTK-ENTRY-PRIMARY-ICON-SENSITIVE
                         "primary-icon-sensitive" "gboolean" T T)
                        (PRIMARY-ICON-STORAGE-TYPE
                         GTK-ENTRY-PRIMARY-ICON-STORAGE-TYPE
                         "primary-icon-storage-type" "GtkImageType" T NIL)
                        (PRIMARY-ICON-TOOLTIP-MARKUP
                         GTK-ENTRY-PRIMARY-ICON-TOOLTIP-MARKUP
                         "primary-icon-tooltip-markup" "gchararray" T T)
                        (PRIMARY-ICON-TOOLTIP-TEXT
                         GTK-ENTRY-PRIMARY-ICON-TOOLTIP-TEXT
                         "primary-icon-tooltip-text" "gchararray" T T)
                        (PROGRESS-FRACTION GTK-ENTRY-PROGRESS-FRACTION
                         "progress-fraction" "gdouble" T T)
                        (PROGRESS-PULSE-STEP GTK-ENTRY-PROGRESS-PULSE-STEP
                         "progress-pulse-step" "gdouble" T T)
                        (SCROLL-OFFSET GTK-ENTRY-SCROLL-OFFSET "scroll-offset"
                         "gint" T NIL)
                        (SECONDARY-ICON-ACTIVATABLE
                         GTK-ENTRY-SECONDARY-ICON-ACTIVATABLE
                         "secondary-icon-activatable" "gboolean" T T)
                        (SECONDARY-ICON-GICON GTK-ENTRY-SECONDARY-ICON-GICON
                         "secondary-icon-gicon" "GIcon" T T)
                        (SECONDARY-ICON-NAME GTK-ENTRY-SECONDARY-ICON-NAME
                         "secondary-icon-name" "gchararray" T T)
                        (SECONDARY-ICON-PAINTABLE
                         GTK-ENTRY-SECONDARY-ICON-PAINTABLE
                         "secondary-icon-paintable" "GdkPaintable" T T)
                        (SECONDARY-ICON-SENSITIVE
                         GTK-ENTRY-SECONDARY-ICON-SENSITIVE
                         "secondary-icon-sensitive" "gboolean" T T)
                        (SECONDARY-ICON-STORAGE-TYPE
                         GTK-ENTRY-SECONDARY-ICON-STORAGE-TYPE
                         "secondary-icon-storage-type" "GtkImageType" T NIL)
                        (SECONDARY-ICON-TOOLTIP-MARKUP
                         GTK-ENTRY-SECONDARY-ICON-TOOLTIP-MARKUP
                         "secondary-icon-tooltip-markup" "gchararray" T T)
                        (SECONDARY-ICON-TOOLTIP-TEXT
                         GTK-ENTRY-SECONDARY-ICON-TOOLTIP-TEXT
                         "secondary-icon-tooltip-text" "gchararray" T T)
                        (SHOW-EMOJI-ICON GTK-ENTRY-SHOW-EMOJI-ICON
                         "show-emoji-icon" "gboolean" T T)
                        (TABS GTK-ENTRY-TABS "tabs" "PangoTabArray" T T)
                        (TEXT-LENGTH GTK-ENTRY-TEXT-LENGTH "text-length"
                         "guint" T NIL)
                        (TRUNCATE-MULTILINE GTK-ENTRY-TRUNCATE-MULTILINE
                         "truncate-multiline" "gboolean" T T)
                        (VISIBILITY GTK-ENTRY-VISIBILITY "visibility"
                         "gboolean" T T)))
             (gobject:get-g-type-definition "GtkEntry"))))

;;; --- Properties -------------------------------------------------------------

;;;     activates-default
;;;     attributes
;;;     buffer
;;;     completion
;;;     enable-emoji-completion
;;;     extra-menu
;;;     has-frame
;;;     im-module
;;;     input-hints
;;;     input-purpose
;;;     invisible-char
;;;     invisible-char-set
;;;     max-length
;;;     overwrite-mode
;;;     placeholder-text
;;;     primary-icon-activatable
;;;     primary-icon-gicon
;;;     primary-icon-name
;;;     primary-icon-paintable
;;;     primary-icon-sensitive
;;;     primary-icon-storage-type
;;;     primary-icon-tooltip-markup
;;;     primary-icon-tooltip-text
;;;     progress-fraction
;;;     progress-pulse-step
;;;     scroll-offset
;;;     secondary-icon-activatable
;;;     secondary-icon-gicon
;;;     secondary-icon-name
;;;     secondary-icon-paintable
;;;     secondary-icon-sensitive
;;;     secondary-icon-storage-type
;;;     secondary-icon-tooltip-markup
;;;     secondary-icon-tooltip-text
;;;     show-emoji-icon
;;;     tabs
;;;     text-length
;;;     truncate-multiline
;;;     visibility

;;; --- Signals ----------------------------------------------------------------

;;;     activate
;;;     icon-press
;;;     icon-release

;;; --- Functions --------------------------------------------------------------

;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_reset_im_context
;;;     gtk_entry_set_icon_from_paintable
;;;     gtk_entry_set_icon_from_icon_name
;;;     gtk_entry_set_icon_from_gicon
;;;     gtk_entry_get_icon_storage_type
;;;     gtk_entry_get_icon_paintable
;;;     gtk_entry_get_icon_name
;;;     gtk_entry_get_icon_gicon
;;;     gtk_entry_set_icon_activatable
;;;     gtk_entry_get_icon_activatable
;;;     gtk_entry_set_icon_sensitive
;;;     gtk_entry_get_icon_sensitive
;;;     gtk_entry_get_icon_at_pos
;;;     gtk_entry_set_icon_tooltip_text
;;;     gtk_entry_get_icon_tooltip_text
;;;     gtk_entry_set_icon_tooltip_markup
;;;     gtk_entry_get_icon_tooltip_markup
;;;     gtk_entry_set_icon_drag_source
;;;     gtk_entry_get_current_icon_drag_source
;;;     gtk_entry_get_icon_area
;;;     gtk_entry_grab_focus_without_selecting

;;; --- 2023-5-29 --------------------------------------------------------------
