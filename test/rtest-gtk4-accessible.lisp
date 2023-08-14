(in-package :gtk-test)

(def-suite gtk-accessible :in gtk-suite)
(in-suite gtk-accessible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessible

(test gtk-accessible-interface
  ;; Type check
  (is (g:type-is-interface "GtkAccessible"))
  ;; Check the registered name
  (is (eq 'gtk:accessible
          (glib:symbol-for-gtype "GtkAccessible")))
  ;; Check the type initializer
  (is (eq (g:gtype "GtkAccessible")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_get_type" :size))))
  ;; Check the interface prerequisites
  (is (equal '("GObject")
             (list-interface-prerequisites "GtkAccessible")))
  ;; Check the interface properties.
  (is (equal '("accessible-role")
             (list-interface-properties "GtkAccessible")))
  ;; Check the interface signals
  (is (equal '()
             (list-signals "GtkAccessible")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "GtkAccessible"
                                  GTK-ACCESSIBLE
                                  (:EXPORT T
                                   :TYPE-INITIALIZER "gtk_accessible_get_type")
                                  (ACCESSIBLE-ROLE
                                   GTK-ACCESSIBLE-ACCESSIBLE-ROLE
                                   "accessible-role" "GtkAccessibleRole" T T))
             (gobject:get-g-type-definition "GtkAccessible"))))

;;; --- Properties -------------------------------------------------------------

;;;     accessible-role

(test gtk-accessible-properties.1
  (is (eq :button
          (gtk:accessible-accessible-role (make-instance 'gtk:button)))))

(test gtk-accessible-properties.2
  (let ((children (remove nil
                          (mapcar #'glib:symbol-for-gtype
                                  (mapcar #'g:type-name
                                          (g:type-children "GtkWidget"))))))
     ;; Remove GTK:LIST-BASE. It is an abstract widget class.
     (setf children (remove 'gtk:list-base children))
     (is (equal '(:BUTTON :CHECKBOX :COMBO-BOX :GRID :GRID-CELL :GROUP :IMG
                  :LABEL :LIST :LIST-ITEM :MENU-BAR :METER :NONE :PROGRESS-BAR
                  :SCROLLBAR :SEARCH :SEARCH-BOX :SEPARATOR :SPIN-BUTTON
                  :SWITCH :TAB-LIST :TEXT-BOX :TREE-GRID :WIDGET :WINDOW)
                (sort (remove-duplicates
                          (mapcar #'gtk:accessible-accessible-role
                                  (mapcar #'make-instance children))
                          :test #'eq)
                      #'string<)))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_update_state
;;;     gtk_accessible_update_state_value
;;;     gtk_accessible_reset_state
;;;     gtk_accessible_update_property
;;;     gtk_accessible_update_property_value
;;;     gtk_accessible_reset_property
;;;     gtk_accessible_update_relation
;;;     gtk_accessible_update_relation_value
;;;     gtk_accessible_reset_relation
;;;     gtk_accessible_property_init_value
;;;     gtk_accessible_relation_init_value
;;;     gtk_accessible_state_init_value

;;; --- 2023-8-13 --------------------------------------------------------------
