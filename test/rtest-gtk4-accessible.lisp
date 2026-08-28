(in-package :gtk-test)

(def-suite gtk-accessible :in gtk-accessibility)
(in-suite gtk-accessible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessibleList                                   Since 4.14

(test gtk-accessible-list-boxed
  ;; Check type
  (is (g:type-is-boxed "GtkAccessibleList"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessibleList")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_list_get_type" :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-list
          (glib:symbol-for-gtype "GtkAccessibleList"))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_list_new_from_array                  not implemented

;;;     gtk_accessible_list_new_from_list                   Since 4.14
;;;     gtk_accessible_list_get_objects                     Since 4.14

(test gtk-accessible-list-new-from-list
  (glib-test:with-check-memory (button label box)
    (setf button (gtk:button-new))
    (setf label (gtk:label-new))
    (setf box (gtk:box-new))
    (let* ((widgets (list button label box))
           (accessiblelist (gtk:accessible-list-new-from-list widgets)))
      (is (every (lambda (x) (typep x 'gtk:widget))
                 (gtk:accessible-list-objects accessiblelist))))))

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessible

(test gtk-accessible-interface
  ;; Check type
  (is (g:type-is-interface "GtkAccessible"))
  ;; Check registered name
  (is (eq 'gtk:accessible
          (glib:symbol-for-gtype "GtkAccessible")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessible")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("GObject")
             (glib-test:list-interface-prerequisites "GtkAccessible")))
  ;; Check interface properties
  (is (equal '("accessible-role")
             (glib-test:list-interface-properties "GtkAccessible")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "GtkAccessible")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "GtkAccessible" GTK:ACCESSIBLE
                      (:EXPORT T
                       :TYPE-INITIALIZER "gtk_accessible_get_type")
                      (ACCESSIBLE-ROLE ACCESSIBLE-ACCESSIBLE-ROLE
                       "accessible-role" "GtkAccessibleRole" T T))
             (gobject:get-gtype-definition "GtkAccessible"))))

;;; --- Properties -------------------------------------------------------------

;;;     accessible-role

(test gtk-accessible-properties
  (glib-test:with-check-memory ()
    (is (eq :button
            (gtk:accessible-accessible-role (make-instance 'gtk:button))))))

;; TODO: This test adds a lot of strong references. We do not execute it.

#+nil
(test gtk-accessible-properties.2
  (let ((gtk-init:*warn-deprecated* nil))
    (glib-test:with-check-memory ()
      (let ((children (remove nil
                              (mapcar #'glib:symbol-for-gtype
                                      (mapcar #'g:type-name
                                              (g:type-children "GtkWidget"))))))
         ;; Remove GTK:LIST-BASE. It is an abstract widget class.
         (setf children (remove 'gtk:list-base children))
         (setf children (remove 'gtk:window children))
         (setf children (remove 'gtk:font-chooser-widget children))
         (is (equal '(:BUTTON :CHECKBOX :COMBO-BOX :GENERIC :GRID
                      :GRID-CELL :GROUP :IMG :LABEL :LIST :LIST-ITEM
                      :MENU-BAR :METER :NONE :PROGRESS-BAR :SCROLLBAR
                      :SEARCH :SEARCH-BOX :SEPARATOR :SPIN-BUTTON :SWITCH
                      :TAB-LIST :TEXT-BOX :TREE-GRID)
                    (sort (remove-duplicates
                              (mapcar #'gtk:accessible-accessible-role
                                      (mapcar #'make-instance children))
                              :test #'eq)
                          #'string<)))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_property_init_value                  not exported
;;;     gtk_accessible_relation_init_value                  not exported
;;;     gtk_accessible_state_init_value                     not exported

;;;     gtk_accessible_update_property

(test gtk-accessible-update-property
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-update-property button
                                              :autocomplete :inline
                                              :description "description"
                                              :has-popup t
                                              :key-shortcuts
                                              "F2,Alt+F,Control+Shift+N"
                                              :label "label"
                                              :level 2
                                              :modal t
                                              :multi-line t
                                              :multi-selectable t
                                              :orientation :vertical
                                              :placeholder "placeholder"
                                              :read-only t
                                              :required t
                                              :role-description "role-description"
                                              :sort :ascending
                                              :value-max 10.0d0
                                              :value-min  0.0d0
                                              :value-now  5.0d0
                                              :value-text "value-text"
                                              :help-text "help-text"))))

;;;     gtk_accessible_update_relation

(test gtk-accessible-update-relation
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-update-relation button
                                              :active-descendant button
                                              :col-count 100
                                              :col-index 50
                                              :col-index-text "col-index-text"
                                              :col-span 3
                                              :controls nil
                                              :described-by nil
                                              :details nil
                                              :error-message nil
                                              :flow-to nil
                                              :labelled-by nil
                                              :owns nil
                                              :pos-in-set 20
                                              :row-count 10
                                              :row-index 5
                                              :row-index-text "row-index-text"
                                              :row-span 3
                                              :set-size  5
;                                             :label-for nil
;                                             :description-for nil
;                                             :controlled-by nil
;                                             :details-for nil
;                                             :error-message-for nil
;                                             :flow-from nil
                                              ))))

;;;     gtk_accessible_update_state

;; FIXME: Check the test for the :VISITED value

(test gtk-accessible-update-state
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-update-state button
                                           :busy t
                                           :checked :mixed
                                           :disabled t
                                           :expanded 1 ; an integer, not a boolean
                                           :hidden t
                                           :invalid :spelling
                                           :pressed :true
                                           :selected 1 ; an integer, not a boolean
;                                          :visited t
                                           ))))

;;;     gtk_accessible_reset_property

(test gtk-accessible-reset-property
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-reset-property button :autocomplete))
    (is-false (gtk:accessible-reset-property button :description))
    (is-false (gtk:accessible-reset-property button :has-popup))
    (is-false (gtk:accessible-reset-property button :key-shortcuts))))

;;;     gtk_accessible_reset_relation

(test gtk-accessible-reset-relation
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-reset-relation button :active-descendant))
    (is-false (gtk:accessible-reset-relation button :col-count))
    (is-false (gtk:accessible-reset-relation button :col-index))
    (is-false (gtk:accessible-reset-relation button :col-index-text))))

;;;     gtk_accessible_reset_state

(test gtk-accessible-reset-state
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-reset-state button :busy))
    (is-false (gtk:accessible-reset-state button :checked))
    (is-false (gtk:accessible-reset-state button :disabled))
    (is-false (gtk:accessible-reset-state button :expanded))))

;;;     gtk_accessible_update_next_accessible_sibling       Since 4.10

;; TODO: This test does not update as expected. Create a working test.

#+nil
(test gtk-accessible-update-next-accessible-sibling
  (glib-test:with-check-memory ((button 3) child (box 2) :strong 2)
    (setf button (gtk:button-new-with-label "text"))
    (setf child (gtk:button-child button))
    (setf box (gtk:box-new))

    (is (eq :button (gtk:accessible-accessible-role button)))
    (is (eq :label (gtk:accessible-accessible-role child)))

    (is (eq button (gtk:accessible-accessible-parent child)))
    (is (eq child (gtk:accessible-first-accessible-child button)))

    (is (eq box (setf (gtk:button-child button) box)))
    (is (eq button (setf (gtk:accessible-accessible-parent box) button)))

    (is-false (gtk:accessible-update-next-accessible-sibling child box))

    (is (eq button (gtk:accessible-accessible-parent box)))
    (is (eq box (gtk:accessible-first-accessible-child button)))))

;;;     gtk_accessbile_upadate_platform_state               Since 4.18
;;;     gtk_accessible_get_platform_state                   Since 4.10

(test gtk-accessible-update-platform-state
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))

    (is-true (gtk:accessible-platform-state button :focusable))
    (is-false (gtk:accessible-platform-state button :focused))
    (is-false (gtk:accessible-platform-state button :active))

    (is-false (setf (gtk:widget-focusable button) nil))

    (is-false (gtk:accessible-platform-state button :focusable))
    (is-false (gtk:accessible-platform-state button :focused))
    (is-false (gtk:accessible-platform-state button :active))))

;;;     gtk_accessible_get_accessible_parent                Since 4.10
;;;     gtk_accessible_set_accessible_parent                Since 4.10

(test gtk-accessible-accessible-parent
  (glib-test:with-check-memory ((button 2) toggle :strong 1)
    (setf button (gtk:button-new))
    (setf toggle (gtk:toggle-button-new))
    (is-false (gtk:accessible-accessible-parent button))
    (is-false (gtk:accessible-accessible-parent toggle))
    (is (eq button
            (setf (gtk:accessible-accessible-parent toggle) button)))
    (is (eq button (gtk:accessible-accessible-parent toggle)))
    ;; Remove reference
    (is-false (setf (gtk:accessible-accessible-parent toggle) nil))))

;;;     gtk_accessible_get_at_context                       Since 4.10

;; TODO: We get a strong reference for CONTEXT. Why?

(test gtk-accessible-at-context
  (glib-test:with-check-memory ((context 2) button :strong 1)
    (is (typep (setf button (gtk:button-new)) 'gtk:button))
    (is (typep (setf context
                     (gtk:accessible-at-context button)) 'gtk:at-context))))

;;;     gtk_accessible_get_bounds                           Since 4.10

(test gtk-accessible-bounds
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new-with-label "text"))
    (is-false (gtk:accessible-bounds button))))

;;;     gtk_accessible_get_first_accessible_child           Since 4.10
;;;     gtk_accessible_get_next_accessible_sibling          Since 4.10

(test gtk-accessible-first/next-accessible
  (glib-test:with-check-memory (button :strong 1)
    (setf button (gtk:button-new-with-label "label"))
    (is (typep (gtk:accessible-first-accessible-child button) 'gtk:label))
    (is-false (gtk:accessible-next-accessible-sibling button))))

;;;     gtk_accessible_get_accessible_id                    Since 4.22

(test gtk-accessible-accessible-id
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-accessible-id button))))

;;;     gtk_accessible_announce                             Since 4.14

(test gtk-accessible-announce
  (glib-test:with-check-memory (button)
    (setf button (gtk:button-new))
    (is-false (gtk:accessible-announce button "message" :medium))))

;;; 2026-08-25
