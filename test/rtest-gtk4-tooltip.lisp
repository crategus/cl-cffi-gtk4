(def-suite gtk-tooltip :in gtk-suite)
(in-suite gtk-tooltip)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkTooltip

(test gtk-tooltip-class
  ;; Type check
  (is (g:type-is-object "GtkTooltip"))
  ;; Check the registered name
  (is (eq 'gtk-tooltip
          (gobject:symbol-for-gtype "GtkTooltip")))
  ;; Check the type initializer
  (is (eq (gtype "GtkTooltip")
          (gtype (foreign-funcall "gtk_tooltip_get_type" g-size))))
  ;; Check the parent
  (is (eq (gtype "GObject")
          (g-type-parent "GtkTooltip")))
  ;; Check the children
  (is (equal '()
             (mapcar #'g-type-name (g-type-children "GtkTooltip"))))
  ;; Check the interfaces
  (is (equal '()
             (mapcar #'g-type-name (g-type-interfaces "GtkTooltip"))))
  ;; Check the class properties
  (is (equal '()
             (list-class-property-names "GtkTooltip")))
  ;; Check the list of signals
  (is (equal '()
             (sort (mapcar #'g-signal-name
                           (g-signal-list-ids "GtkTooltip"))
                   #'string<)))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "GtkTooltip" GTK-TOOLTIP
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "gtk_tooltip_get_type")
                       NIL)
             (get-g-type-definition "GtkTooltip"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_tooltip_set_markup
;;;     gtk_tooltip_set_text
;;;     gtk_tooltip_set_icon
;;;     gtk_tooltip_set_icon_from_icon_name
;;;     gtk_tooltip_set_icon_from_gicon
;;;     gtk_tooltip_set_custom
;;;     gtk_tooltip_set_tip_area

;;; 2022-7-10
