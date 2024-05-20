(in-package :gtk-test)

(def-suite gtk-lock-button :in gtk-suite)
(in-suite gtk-lock-button)

;;; --- Types and Values -------------------------------------------------------

;;;     GtkLockButton

(test gtk-lock-button-class
  ;; Check type
  (is (g:type-is-object "GtkLockButton"))
  ;; Check registered name
  (is (eq 'gtk:lock-button
          (glib:symbol-for-gtype "GtkLockButton")))
  ;; Check type initializer
  (is (eq (g:gtype "GtkLockButton")
          (g:gtype (cffi:foreign-funcall "gtk_lock_button_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GtkButton")
          (g:type-parent "GtkLockButton")))
  ;; Check children
  (is (equal '()
             (list-children "GtkLockButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (list-interfaces "GtkLockButton")))
  ;; Check properties
  (is (equal '("permission" "text-lock" "text-unlock" "tooltip-lock"
               "tooltip-not-authorized" "tooltip-unlock")
             (list-properties "GtkLockButton")))
  ;; Check signals
  (is (equal '()
             (list-signals "GtkLockButton")))
  ;; Check CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLockButton")))
  ;; Check accessible role
  (is (eq :button (gtk:widget-class-accessible-role "GtkLockButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "GtkLockButton" GTK-LOCK-BUTTON
                       (:SUPERCLASS GTK-BUTTON :EXPORT T :INTERFACES
                        ("GtkAccessible" "GtkActionable" "GtkBuildable"
                         "GtkConstraintTarget")
                        :TYPE-INITIALIZER "gtk_lock_button_get_type")
                       ((PERMISSION GTK-LOCK-BUTTON-PERMISSION "permission"
                         "GPermission" T T)
                        (TEXT-LOCK GTK-LOCK-BUTTON-TEXT-LOCK "text-lock"
                         "gchararray" T T)
                        (TEXT-UNLOCK GTK-LOCK-BUTTON-TEXT-UNLOCK "text-unlock"
                         "gchararray" T T)
                        (TOOLTIP-LOCK GTK-LOCK-BUTTON-TOOLTIP-LOCK
                         "tooltip-lock" "gchararray" T T)
                        (TOOLTIP-NOT-AUTHORIZED
                         GTK-LOCK-BUTTON-TOOLTIP-NOT-AUTHORIZED
                         "tooltip-not-authorized" "gchararray" T T)
                        (TOOLTIP-UNLOCK GTK-LOCK-BUTTON-TOOLTIP-UNLOCK
                         "tooltip-unlock" "gchararray" T T)))
             (gobject:get-g-type-definition "GtkLockButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     permission
;;;     text-lock
;;;     text-unlock
;;;     tooltip-lock
;;;     tooltip-not-authorized
;;;     tooltip-unlock

;;; --- Functions --------------------------------------------------------------

;;;     gtk_lock_button_new

;;; 2024-5-18
