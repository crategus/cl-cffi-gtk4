(in-package :gtk-test)

(def-suite gtk-accessible :in gtk-accessibility)
(in-suite gtk-accessible)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkAccessiblePlatformState

(test gtk-accessible-platform-state
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
             (glib-test:list-enum-item-names "GtkAccessiblePlatformState")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "GtkAccessiblePlatformState")))
  ;; Check nick names
  (is (equal '("focusable" "focused" "active")
             (glib-test:list-enum-item-nicks "GtkAccessiblePlatformState")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GtkAccessiblePlatformState"
                                    GTK:ACCESSIBLE-PLATFORM-STATE
                       (:EXPORT T
                        :TYPE-INITIALIZER
                        "gtk_accessible_platform_state_get_type")
                       (:FOCUSABLE 0)
                       (:FOCUSED 1)
                       (:ACTIVE 2))
             (gobject:get-gtype-definition "GtkAccessiblePlatformState"))))

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

(test gtk-accessible-properties.1
  (is (eq :button
          (gtk:accessible-accessible-role (make-instance 'gtk:button)))))

;; TODO: Gives an unexpected error on Windows

#-windows
(test gtk-accessible-properties.2
  (let ((gtk-init:*warn-deprecated* nil)) ; no warnings for deprecated widgets
    (let ((children (remove nil
                            (mapcar #'glib:symbol-for-gtype
                                    (mapcar #'g:type-name
                                            (g:type-children "GtkWidget"))))))
       ;; Remove GTK:LIST-BASE. It is an abstract widget class.
       (setf children (remove 'gtk:list-base children))
       (setf children (remove 'gtk:window children))
       (is (equal '(:BUTTON :CHECKBOX :COMBO-BOX :GENERIC :GRID
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

;;;     gtk_accessible_list_new_from_array                  not implemented

;;;     gtk_accessible_list_new_from_list                   Since 4.14
;;;     gtk_accessible_list_get_objects                     Since 4.14

(test gtk-accessible-list-new-from-list
  (let* ((button (gtk:button-new))
         (label (gtk:label-new))
         (box (gtk:box-new))
         (widgets (list button label box))
         (accessibles (gtk:accessible-list-new-from-list widgets)))
    (is (every (lambda (x) (typep x 'gtk:widget))
               (gtk:accessible-list-objects accessibles)))))

;;;     gtk_accessible_get_accessible_parent               Since 4.10
;;;     gtk_accessible_set_accessible_parent               Since 4.10

(test gtk-accessible-accessible-parent
  (let ((button (gtk:button-new))
        (toggle (gtk:toggle-button-new)))

    (is-false (gtk:accessible-accessible-parent button))
    (is-false (gtk:accessible-accessible-parent toggle))

    (is (eq button
            (setf (gtk:accessible-accessible-parent toggle) button)))
    (is (eq button (gtk:accessible-accessible-parent toggle)))

    (is (= 2 (g:object-ref-count button)))
    (is (= 1 (g:object-ref-count toggle)))))


;;;     gtk_accessible_get_at_context                      Since 4.10

(test gtk-accessible-at-context
  (let ((button (gtk:button-new))
        context)
    (is (typep (setf context
                     (gtk:accessible-at-context button)) 'gtk:at-context))
    ;; Check memory management
    (is (= 2 (g:object-ref-count context)))
    (is (= 1 (g:object-ref-count button)))))

;;;     gtk_accessible_get_bounds                          Since 4.10

(test gtk-accessible-bounds
  (let ((button (gtk:button-new-with-label "text")))
    (is-false (gtk:accessible-bounds button))))

;;;     gtk_accessible_get_first_accessible_child          Since 4.10
;;;     gtk_accessible_get_next_accessible_sibling         Since 4.10
;;;     gtk_accessible_get_platform_state                  Since 4.10

;;;     gtk_accessible_reset_property
;;;     gtk_accessible_reset_relation
;;;     gtk_accessible_reset_state
;;;
;;;     gtk_accessible_update_next_accessible_sibling      Since 4.10
;;;
;;;     gtk_accessible_update_property
;;;     gtk_accessible_update_relation
;;;     gtk_accessible_update_state

;;;     gtk_accessible_property_init_value
;;;     gtk_accessible_relation_init_value
;;;     gtk_accessible_state_init_value

;;;     gtk_accessible_announce                             Since 4.14

;;; 2024-12-14
