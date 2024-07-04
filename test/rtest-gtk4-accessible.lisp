(in-package :gtk-test)

(def-suite gtk-accessible :in gtk-suite)
(in-suite gtk-accessible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessiblePlatformState

(test gtk-accessible-platfom-state
  ;; Check type
  (is (g:type-is-enum "GtkAccessiblePlatformState"))
  ;; Check type initializer
  (is (eq (g:gtype "GtkAccessiblePlatformState")
          (g:gtype (cffi:foreign-funcall "gtk_accessible_platform_state_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'gtk:accessible-platform-state
          (glib:symbol-for-gtype "GtkAccessiblePlatformState")))
  ;; Check names
  (is (equal '("GTK_ACCESSIBLE_PLATFORM_STATE_FOCUSABLE"
               "GTK_ACCESSIBLE_PLATFORM_STATE_FOCUSED"
               "GTK_ACCESSIBLE_PLATFORM_STATE_ACTIVE")
             (gtk-test:list-enum-item-name "GtkAccessiblePlatformState")))
  ;; Check values
  (is (equal '(0 1 2)
             (gtk-test:list-enum-item-value "GtkAccessiblePlatformState")))
  ;; Check nick names
  (is (equal '("focusable" "focused" "active")
             (gtk-test:list-enum-item-nick "GtkAccessiblePlatformState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "GtkAccessiblePlatformState"
                                     GTK-ACCESSIBLE-PLATFORM-STATE
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "gtk_accessible_platform_state_get_type")
                                     (:FOCUSABLE 0)
                                     (:FOCUSED 1)
                                     (:ACTIVE 2))
             (gobject:get-g-type-definition "GtkAccessiblePlatformState"))))

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
             (gtk-test:list-interface-prerequisites "GtkAccessible")))
  ;; Check interface properties
  (is (equal '("accessible-role")
             (gtk-test:list-interface-properties "GtkAccessible")))
  ;; Check interface signals
  (is (equal '()
             (gtk-test:list-signals "GtkAccessible")))
  ;; Check interface definition
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

;; TODO: Gives an unexpected error on Windows

#-windows
(test gtk-accessible-properties.2
  (let ((*gtk-warn-deprecated* nil)) ; no warnings for deprecated widgets
    (let ((children (remove nil
                            (mapcar #'glib:symbol-for-gtype
                                    (mapcar #'g:type-name
                                            (g:type-children "GtkWidget"))))))
       ;; Remove GTK:LIST-BASE. It is an abstract widget class.
       (setf children (remove 'gtk:list-base children))
       (is (equal '(:APPLICATION :BUTTON :CHECKBOX :COMBO-BOX :GENERIC :GRID
                    :GRID-CELL :GROUP :IMG :LABEL :LIST :LIST-ITEM
                    :MENU-BAR :METER :NONE :PROGRESS-BAR :SCROLLBAR
                    :SEARCH :SEARCH-BOX :SEPARATOR :SPIN-BUTTON :SWITCH
                    :TAB-LIST :TEXT-BOX :TREE-GRID)
                  (sort (remove-duplicates
                            (mapcar #'gtk:accessible-accessible-role
                                    (mapcar #'make-instance children))
                            :test #'eq)
                        #'string<))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_accessible_get_accessible_parent               Since 4.10
;;;     gtk_accessible_set_accessible_parent               Since 4.10
;;;     gtk_accessible_get_at_context                      Since 4.10
;;;     gtk_accessible_get_bounds                          Since 4.10
;;;     gtk_accessible_get_first_accessible_child          Since 4.10
;;;     gtk_accessible_get_next_accessible_sibling         Since 4.10
;;;     gtk_accessible_get_platform_state                  Since 4.10
;;;
;;;     gtk_accessible_reset_property
;;;     gtk_accessible_reset_relation
;;;     gtk_accessible_reset_state
;;;
;;;     gtk_accessible_update_next_accessible_sibling      Since 4.10
;;;
;;;     gtk_accessible_update_property
;;;     gtk_accessible_update_property_value
;;;     gtk_accessible_update_relation
;;;     gtk_accessible_update_relation_value
;;;     gtk_accessible_update_state
;;;     gtk_accessible_update_state_value
;;;
;;;     gtk_accessible_property_init_value
;;;     gtk_accessible_relation_init_value
;;;     gtk_accessible_state_init_value

;;; 2024-4-11
