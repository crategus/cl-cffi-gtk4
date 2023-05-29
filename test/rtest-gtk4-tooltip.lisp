(in-package :gtk-testsuite)

(def-suite gtk-tooltip :in gtk-suite)
(in-suite gtk-tooltip)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTooltip

(test gtk-tooltip-class
  ;; Type check
  (is (g:type-is-object "GtkTooltip"))
  ;; Check the registered name
  (is (eq 'gtk:tooltip
          (glib:symbol-for-gtype "GtkTooltip")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkTooltip")
          (g:gtype (cffi:foreign-funcall "gtk_tooltip_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "GtkTooltip")))
  ;; Check the children
  (is (equal '()
             (list-children "GtkTooltip")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "GtkTooltip")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "GtkTooltip")))
  ;; Check the list of signals
  (is (equal '()
             (list-signals "GtkTooltip")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTooltip" GTK-TOOLTIP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tooltip_get_type")
                       NIL)
             (gobject:get-g-type-definition "GtkTooltip"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tooltip_set_markup
;;;     gtk_tooltip_set_text
;;;     gtk_tooltip_set_icon
;;;     gtk_tooltip_set_icon_from_icon_name
;;;     gtk_tooltip_set_icon_from_gicon
;;;     gtk_tooltip_set_custom
;;;     gtk_tooltip_set_tip_area

;;; --- 2023-5-29 --------------------------------------------------------------
