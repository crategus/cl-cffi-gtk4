(in-package :gtk-test)

(def-suite gtk-popover :in gtk-suite)
(in-suite gtk-popover)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopover

(test gtk-popover-class
  ;; Type check
  (is (g:type-is-object "GtkPopover"))
  ;; Check the registered name
  (is (eq 'gtk:popover
          (glib:symbol-for-gtype "GtkPopover")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkPopover")
          (g:gtype (cffi:foreign-funcall "gtk_popover_get_type" :size))))
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
  ;; CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopover")))
  ;; CSS classes
  (is (equal '("background")
             (gtk:widget-css-classes (make-instance 'gtk:popover))))
  ;; Accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkPopover")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkPopover" GTK-POPOVER
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

;;; --- 2023-11-2 --------------------------------------------------------------
