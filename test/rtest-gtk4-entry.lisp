(in-package :gtk-test)

(def-suite gtk-entry :in gtk-suite)
(in-suite gtk-entry)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkEntryIconPosition

(test gtk-entry-icon-position
  ;; Check type
  (is (g:type-is-enum "GtkEntryIconPosition"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntryIconPosition")
          (g:gtype (cffi:foreign-funcall "gtk_entry_icon_position_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:entry-icon-position
          (glib:symbol-for-gtype "GtkEntryIconPosition")))
  ;; Check names
  (is (equal '("GTK_ENTRY_ICON_PRIMARY" "GTK_ENTRY_ICON_SECONDARY")
             (list-enum-item-name "GtkEntryIconPosition")))
  ;; Check values
  (is (equal '(0 1)
             (list-enum-item-value "GtkEntryIconPosition")))
  ;; Check nick names
  (is (equal '("primary" "secondary")
             (list-enum-item-nick "GtkEntryIconPosition")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkEntryIconPosition"
                             GTK-ENTRY-ICON-POSITION
                             (:EXPORT T
                              :TYPE-INITIALIZER
                              "gtk_entry_icon_position_get_type")
                             (:PRIMARY 0)
                             (:SECONDARY 1))
             (gobject:get-g-type-definition "GtkEntryIconPosition"))))

;;;     GtkEntry

(test gtk-entry-class
  ;; Check type
  (is (g:type-is-object "GtkEntry"))
  ;; Check registered name
  (is (eq 'gtk:entry
          (glib:symbol-for-gtype "GtkEntry")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkEntry")
          (g:gtype (cffi:foreign-funcall "gtk_entry_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkEntry")))
  ;; Check children
  #-windows
  (if *first-run-gtk-test*
      (is (equal '()
                 (list-children "GtkEntry")))
      (is (equal '("GtkFileChooserEntry")
                 (list-children "GtkEntry"))))
  #+windows
  (is (equal '()
             (list-children "GtkEntry")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkEditable" "GtkCellEditable")
             (list-interfaces "GtkEntry")))
  ;; Check properties
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
  ;; Check signals
  (is (equal '("activate" "icon-press" "icon-release")
             (list-signals "GtkEntry")))
  ;; Check CSS information
  (is (string= "entry"
               (gtk:widget-class-css-name "GtkEntry")))
  ;; Check accessible role
  (is (eq :text-box (gtk:widget-class-accessible-role "GtkEntry")))
  ;; Check class definition
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

(test gtk-entry-properties
  (let ((entry (make-instance 'gtk:entry)))
    (is-false (gtk:entry-activates-default entry))
    (is-false (gtk:entry-attributes entry))
    (is (typep (gtk:entry-buffer entry) 'gtk:entry-buffer))
    (is-false (gtk:entry-completion entry))
    (is-false (gtk:entry-enable-emoji-completion entry))
    (is-false (gtk:entry-extra-menu entry))
    (is-true (gtk:entry-has-frame entry))
    (is-false (gtk:entry-im-module entry))
    (is-false (gtk:entry-input-hints entry))
    (is (eq :free-form (gtk:entry-input-purpose entry)))
    (is (= 0 (gtk:entry-invisible-char entry)))
    (is-false (gtk:entry-invisible-char-set entry))
    (is (= 0 (gtk:entry-max-length entry)))
    (is-false (gtk:entry-overwrite-mode entry))
    (is-false (gtk:entry-placeholder-text entry))
    (is-true (gtk:entry-primary-icon-activatable entry))
    (is-false (gtk:entry-primary-icon-gicon entry))
    (is-false (gtk:entry-primary-icon-name entry))
    (is-false (gtk:entry-primary-icon-paintable entry))
    (is-true (gtk:entry-primary-icon-sensitive entry))
    (is (eq :empty (gtk:entry-primary-icon-storage-type entry)))
    (is-false (gtk:entry-primary-icon-tooltip-markup entry))
    (is-false (gtk:entry-primary-icon-tooltip-text entry))
    (is (= 0.0d0 (gtk:entry-progress-fraction entry)))
    (is (= 0.0d0 (gtk:entry-progress-pulse-step entry)))
    (is (= 0 (gtk:entry-scroll-offset entry)))
    (is-true (gtk:entry-secondary-icon-activatable entry))
    (is-false (gtk:entry-secondary-icon-gicon entry))
    (is-false (gtk:entry-secondary-icon-name entry))
    (is-false (gtk:entry-secondary-icon-paintable entry))
    (is-true (gtk:entry-secondary-icon-sensitive entry))
    (is (eq :empty (gtk:entry-secondary-icon-storage-type entry)))
    (is-false (gtk:entry-secondary-icon-tooltip-markup entry))
    (is-false (gtk:entry-secondary-icon-tooltip-text entry))
    (is-false (gtk:entry-show-emoji-icon entry))
    (is-false (gtk:entry-tabs entry))
    (is (= 0 (gtk:entry-text-length entry)))
    (is-false (gtk:entry-truncate-multiline entry))
    (is-true (gtk:entry-visibility entry))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate

(test gtk-entry-activate-signal
  (let* ((name "activate") (gtype "GtkEntry")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:ACTION :RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '()
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     icon-press

(test gtk-entry-icon-press-signal
  (let* ((name "icon-press") (gtype "GtkEntry")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkEntryIconPosition")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;;     icon-release

(test gtk-entry-icon-release-signal
  (let* ((name "icon-release") (gtype "GtkEntry")
         (query (g:signal-query (g:signal-lookup name gtype))))
    ;; Retrieve name and gtype
    (is (string= name (g:signal-query-signal-name query)))
    (is (string= gtype (g:type-name (g:signal-query-owner-type query))))
    ;; Check flags
    (is (equal '(:RUN-LAST)
               (sort (g:signal-query-signal-flags query) #'string<)))
    ;; Check return type
    (is (string= "void" (g:type-name (g:signal-query-return-type query))))
    ;; Check parameter types
    (is (equal '("GtkEntryIconPosition")
               (mapcar #'g:type-name (g:signal-query-param-types query))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_entry_new

(test gtk-entry-new
  (is (typep (gtk:entry-new) 'gtk:entry)))

;;;     gtk_entry_new_with_buffer

(test gtk-entry-new-with-buffer
  (let ((buffer (gtk:entry-buffer-new "some text"))
        entry)
    (is (typep (setf entry (gtk:entry-new-with-buffer buffer)) 'gtk:entry))
    (is (eq buffer (gtk:entry-buffer entry)))))

;;;     gtk_entry_unset_invisible_char

(test gtk-entry-unset-invisible-char
  (let ((entry (gtk:entry-new)))
    (is (= 0 (gtk:entry-invisible-char entry)))
    (is-false (gtk:entry-invisible-char-set entry))
    (is (= 43 (setf (gtk:entry-invisible-char entry) (char-code #\+))))
    (is (= 43 (gtk:entry-invisible-char entry)))
    (is-true (gtk:entry-invisible-char-set entry))
    (is-false (gtk:entry-unset-invisible-char entry))
    ;; Default char is #\BULLET and not \*
    #-windows
    (is (= (char-code #\BULLET) (gtk:entry-invisible-char entry)))
    #+windows
    (is (= (char-code #\BLACK_CIRCLE) (gtk:entry-invisible-char entry)))
    (is-false (gtk:entry-invisible-char-set entry))))

;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment

(test gtk-entry-alignment
  (let ((entry (gtk:entry-new)))
    (is (= 0.0 (gtk:entry-alignment entry)))
    (is (= 0.5 (setf (gtk:entry-alignment entry) 1/2)))
    (is (= 0.5 (gtk:entry-alignment entry)))))

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

;;; 2024-5-21
