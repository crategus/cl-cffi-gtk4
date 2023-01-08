(in-package :gtk-test)

(def-suite gtk-popover :in gtk-suite)
(in-suite gtk-popover)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopover

(test popover-class
  ;; Type check
  (is (g:type-is-object "GtkPopover"))
  ;; Check the registered name
  (is (eq 'gtk:popover
          (gobject:symbol-for-gtype "GtkPopover")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPopover")
          (g:gtype (foreign-funcall "gtk_popover_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPopover")))
  ;; Check the children
  (is (equal '("GtkEmojiChooser" "GtkPopoverMenu" "GtkTreePopover")
             (list-children "GtkPopover")))
  ;; Check the interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (list-interfaces "GtkPopover")))
  ;; Check the properties
  (is (equal '("autohide" "cascade-popdown" "child" "default-widget" "has-arrow"
               "mnemonics-visible" "pointing-to" "position")
             (list-properties "GtkPopover")))
  ;; Check the signals
  (is (equal '("activate-default" "closed")
             (list-signals "GtkPopover")))
  ;; CSS information
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopover")))
  (is (string=
"[popover.background:dir(ltr)]
"
               (gtk:style-context-to-string
                   (gtk:widget-style-context (make-instance 'gtk:popover))
                   :none)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkPopover" GTK-POPOVER
                       (:SUPERCLASS GTK-WIDGET :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_popover_get_type")
                       ((AUTOHIDE GTK-POPOVER-AUTOHIDE "autohide" "gboolean" T
                         T)
                        (CASCADE-POPDOWN GTK-POPOVER-CASCADE-POPDOWN
                         "cascade-popdown" "gboolean" T T)
                        (CHILD GTK-POPOVER-CHILD "child" "GtkWidget" T T)
                        (DEFAULT-WIDGET GTK-POPOVER-DEFAULT-WIDGET
                         "default-widget" "GtkWidget" T T)
                        (HAS-ARROW GTK-POPOVER-HAS-ARROW "has-arrow" "gboolean"
                         T T)
                        (MNEMONICS-VISIBLE GTK-POPOVER-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (POINTING-TO GTK-POPOVER-POINTING-TO "pointing-to"
                         "GdkRectangle" T T)
                        (POSITION GTK-POPOVER-POSITION "position"
                                  "GtkPositionType" T T)))
             (gobject:get-g-type-definition "GtkPopover"))))

;;; --- Properties -------------------------------------------------------------

;;;     autohide
;;;     cascade-popdown
;;;     child
;;;     default-widget
;;;     has-arrow
;;;     mnemonics-visible
;;;     pointing-to
;;;     position

;;; --- Signals ----------------------------------------------------------------

;;;     activate-default
;;;     closed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_new
;;;     gtk_popover_popup
;;;     gtk_popover_popdown
;;;     gtk_popover_present
;;;     gtk_popover_set_offset
;;;     gtk_popover_get_offset

;;; 2022-11-14
