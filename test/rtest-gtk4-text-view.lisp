(in-package :gtk-test)

(def-suite gtk-text-view :in gtk-suite)
(in-suite gtk-text-view)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTextViewLayer
;;;     GtkTextWindowType
;;;     GtkTextExtendSelection
;;;     GtkWrapMode                                        gtk.enumerations.lisp
;;;     GtkTextChildAnchor

;;;     GTK_TEXT_VIEW_PRIORITY_VALIDATE

;;;     GtkTextView

(test gtk-text-view-class
  ;; Type check
  (is (g:type-is-object "GtkTextView"))
  ;; Check the registered name
  (is (eq 'gtk:text-view
          (glib:symbol-for-gtype "GtkTextView")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTextView")
          (g:gtype (cffi:foreign-funcall "gtk_text_view_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkTextView")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTextView")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget" 
               "GtkScrollable")
             (list-interfaces "GtkTextView")))
  ;; Check the properties
  (is (equal '("accepts-tab" "bottom-margin" "buffer" "cursor-visible" 
               "editable" "extra-menu" "hadjustment" "hscroll-policy" 
               "im-module" "indent" "input-hints" "input-purpose" 
               "justification" "left-margin" "monospace" "overwrite"
               "pixels-above-lines" "pixels-below-lines" "pixels-inside-wrap" 
               "right-margin" "tabs" "top-margin" "vadjustment" "vscroll-policy" 
               "wrap-mode")
             (list-properties "GtkTextView")))
  ;; Check the signals
  (is (equal '("backspace" "copy-clipboard" "cut-clipboard" "delete-from-cursor"
               "extend-selection" "insert-at-cursor" "insert-emoji" 
               "move-cursor" "move-viewport" "paste-clipboard" "preedit-changed" 
               "select-all" "set-anchor" "toggle-cursor-visible" 
               "toggle-overwrite")
             (list-signals "GtkTextView")))
  ;; CSS name
  (is (string= "textview"
               (gtk:widget-class-css-name "GtkTextView")))
  ;; Accessible role
  (is (eq :TEXT-BOX (gtk:widget-class-accessible-role "GtkTextView")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkTextView" GTK-TEXT-VIEW
                               (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                                ("GtkAccessible" "GtkBuildable"
                                 "GtkConstraintTarget" "GtkScrollable")
                                :TYPE-INITIALIZER "gtk_text_view_get_type")
                               ((ACCEPTS-TAB GTK-TEXT-VIEW-ACCEPTS-TAB
                                 "accepts-tab" "gboolean" T T)
                                (BOTTOM-MARGIN GTK-TEXT-VIEW-BOTTOM-MARGIN
                                 "bottom-margin" "gint" T T)
                                (BUFFER GTK-TEXT-VIEW-BUFFER "buffer"
                                 "GtkTextBuffer" T T)
                                (CURSOR-VISIBLE GTK-TEXT-VIEW-CURSOR-VISIBLE
                                 "cursor-visible" "gboolean" T T)
                                (EDITABLE GTK-TEXT-VIEW-EDITABLE "editable"
                                 "gboolean" T T)
                                (EXTRA-MENU GTK-TEXT-VIEW-EXTRA-MENU
                                 "extra-menu" "GMenuModel" T T)
                                (IM-MODULE GTK-TEXT-VIEW-IM-MODULE "im-module"
                                 "gchararray" T T)
                                (INDENT GTK-TEXT-VIEW-INDENT "indent" "gint" T
                                 T)
                                (INPUT-HINTS GTK-TEXT-VIEW-INPUT-HINTS
                                 "input-hints" "GtkInputHints" T T)
                                (INPUT-PURPOSE GTK-TEXT-VIEW-INPUT-PURPOSE
                                 "input-purpose" "GtkInputPurpose" T T)
                                (JUSTIFICATION GTK-TEXT-VIEW-JUSTIFICATION
                                 "justification" "GtkJustification" T T)
                                (LEFT-MARGIN GTK-TEXT-VIEW-LEFT-MARGIN
                                 "left-margin" "gint" T T)
                                (MONOSPACE GTK-TEXT-VIEW-MONOSPACE "monospace"
                                 "gboolean" T T)
                                (OVERWRITE GTK-TEXT-VIEW-OVERWRITE "overwrite"
                                 "gboolean" T T)
                                (PIXELS-ABOVE-LINES
                                 GTK-TEXT-VIEW-PIXELS-ABOVE-LINES
                                 "pixels-above-lines" "gint" T T)
                                (PIXELS-BELOW-LINES
                                 GTK-TEXT-VIEW-PIXELS-BELOW-LINES
                                 "pixels-below-lines" "gint" T T)
                                (PIXELS-INSIDE-WRAP
                                 GTK-TEXT-VIEW-PIXELS-INSIDE-WRAP
                                 "pixels-inside-wrap" "gint" T T)
                                (RIGHT-MARGIN GTK-TEXT-VIEW-RIGHT-MARGIN
                                 "right-margin" "gint" T T)
                                (TABS GTK-TEXT-VIEW-TABS "tabs" "PangoTabArray"
                                 T T)
                                (TOP-MARGIN GTK-TEXT-VIEW-TOP-MARGIN
                                 "top-margin" "gint" T T)
                                (WRAP-MODE GTK-TEXT-VIEW-WRAP-MODE "wrap-mode"
                                 "GtkWrapMode" T T)))
             (gobject:get-g-type-definition "GtkTextView"))))

;;; --- Properties -------------------------------------------------------------

;;;     accepts-tab
;;;     bottom-margin
;;;     buffer
;;;     cursor-visible
;;;     editable
;;;     extra-menu
;;;     im-module
;;;     indent
;;;     input-hints
;;;     input-purpose
;;;     justification
;;;     left-margin
;;;     monospace
;;;     overwrite
;;;     pixels-above-lines
;;;     pixels-below-lines
;;;     pixels-inside-wrap
;;;     right-margin
;;;     tabs
;;;     top-margin
;;;     wrap-mode

;;; --- Signals ----------------------------------------------------------------

;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     extend-selection
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     move-viewport
;;;     paste-clipboard
;;;     preedit-changed
;;;     select-all
;;;     set-anchor
;;;     toggle-cursor-visible
;;;     toggle-overwrite

;;;
;;; Functions
;;;
;;;     gtk_text_view_new
;;;     gtk_text_view_new_with_buffer
;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;     gtk_text_view_forward_display_line
;;;     gtk_text_view_backward_display_line
;;;     gtk_text_view_forward_display_line_end
;;;     gtk_text_view_backward_display_line_start
;;;     gtk_text_view_starts_display_line
;;;     gtk_text_view_move_visually
;;;     gtk_text_view_add_child_at_anchor
;;;     gtk_text_view_remove
;;;     gtk_text_child_anchor_new
;;;     gtk_text_child_anchor_get_widgets
;;;     gtk_text_child_anchor_get_deleted
;;;     gtk_text_view_get_gutter
;;;     gtk_text_view_set_gutter
;;;     gtk_text_view_add_overlay
;;;     gtk_text_view_move_overlay
;;;     gtk_text_view_reset_cursor_blink
;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context
;;;     gtk_text_view_get_ltr_context                      Since 4.4
;;;     gtk_text_view_get_rtl_context                      Since 4.4

;;; --- 2023-9-1 ---------------------------------------------------------------
