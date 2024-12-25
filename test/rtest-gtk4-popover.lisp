(in-package :gtk-test)

(def-suite gtk-popover :in gtk-popover-widgets)
(in-suite gtk-popover)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkPopover

(test gtk-popover-class
  ;; Check type
  (is (g:type-is-object "GtkPopover"))
  ;; Check registered name
  (is (eq 'gtk:popover
          (glib:symbol-for-gtype "GtkPopover")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkPopover")
          (g:gtype (cffi:foreign-funcall "gtk_popover_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkWidget")
          (g:type-parent "GtkPopover")))
  ;; Check children
  (is (subsetp '("GtkEmojiChooser" "GtkPopoverMenu")
               (glib-test:list-children "GtkPopover") :test #'string=))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkShortcutManager" "GtkNative")
             (glib-test:list-interfaces "GtkPopover")))
  ;; Check properties
  (is (equal '("autohide" "cascade-popdown" "child" "default-widget" "has-arrow"
               "mnemonics-visible" "pointing-to" "position")
             (glib-test:list-properties "GtkPopover")))
  ;; Check signals
  (is (equal '("activate-default" "closed")
             (glib-test:list-signals "GtkPopover")))
  ;; Check CSS name
  (is (string= "popover"
               (gtk:widget-class-css-name "GtkPopover")))
  ;; Check accessible role
  (is (eq :widget (gtk:widget-class-accessible-role "GtkPopover")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkPopover" GTK:POPOVER
                       (:SUPERCLASS GTK:WIDGET
                        :EXPORT T
                        :INTERFACES
                        ("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
                         "GtkNative" "GtkShortcutManager")
                        :TYPE-INITIALIZER "gtk_popover_get_type")
                       ((AUTOHIDE POPOVER-AUTOHIDE "autohide" "gboolean" T T)
                        (CASCADE-POPDOWN POPOVER-CASCADE-POPDOWN
                         "cascade-popdown" "gboolean" T T)
                        (CHILD POPOVER-CHILD "child" "GtkWidget" T T)
                        (DEFAULT-WIDGET POPOVER-DEFAULT-WIDGET
                         "default-widget" "GtkWidget" T T)
                        (HAS-ARROW POPOVER-HAS-ARROW
                         "has-arrow" "gboolean" T T)
                        (MNEMONICS-VISIBLE POPOVER-MNEMONICS-VISIBLE
                         "mnemonics-visible" "gboolean" T T)
                        (POINTING-TO POPOVER-POINTING-TO
                         "pointing-to" "GdkRectangle" T T)
                        (POSITION POPOVER-POSITION
                         "position" "GtkPositionType" T T)))
             (gobject:get-gtype-definition "GtkPopover"))))

;;; --- Properties -------------------------------------------------------------

(test gtk-popover-properties
  (let ((popover (make-instance 'gtk:popover)))
    (is-true (gtk:popover-autohide popover))
    (is-false (gtk:popover-cascade-popdown popover))
    (is-false (gtk:popover-child popover))
    (is-false (gtk:popover-default-widget popover))
    (is-true (gtk:popover-has-arrow popover))
    (is-false (gtk:popover-mnemonics-visible popover))
    (is (gdk:rectangle-equal (gdk:rectangle-new :x 0 :y 0 :width 0 :height 0)
                             (gtk:popover-pointing-to popover)))
    (is (eq :bottom (gtk:popover-position popover)))
    ;; Check memory management
    (is (= 1 (g:object-ref-count popover)))))

;;; --- Signals ----------------------------------------------------------------

;;;     activate-default
;;;     closed

;;; --- Functions --------------------------------------------------------------

;;;     gtk_popover_new

(test gtk-popover-new
  (is (typep (gtk:popover-new) 'gtk:popover)))

;;;     gtk_popover_popup
;;;     gtk_popover_popdown
;;;     gtk_popover_present
;;;     gtk_popover_set_offset
;;;     gtk_popover_get_offset

;;; 2024-9-20
