(in-package :gtk-test)

(def-suite gtk-lock-button :in gtk-deprecated)
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
             (glib-test:list-children "GtkLockButton")))
  ;; Check interfaces
  (is (equal '("GtkAccessible" "GtkBuildable" "GtkConstraintTarget"
               "GtkActionable")
             (glib-test:list-interfaces "GtkLockButton")))
  ;; Check properties
  (is (equal '("permission" "text-lock" "text-unlock" "tooltip-lock"
               "tooltip-not-authorized" "tooltip-unlock")
             (glib-test:list-properties "GtkLockButton")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "GtkLockButton")))
  ;; Check CSS name
  (is (string= "button"
               (gtk:widget-class-css-name "GtkLockButton")))
  ;; Check accessible role
  (is (eq :button (gtk:widget-class-accessible-role "GtkLockButton")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "GtkLockButton" GTK:LOCK-BUTTON
                      (:SUPERCLASS GTK:BUTTON
                       :EXPORT T
                       :INTERFACES
                       ("GtkAccessible" "GtkActionable" "GtkBuildable"
                        "GtkConstraintTarget")
                       :TYPE-INITIALIZER "gtk_lock_button_get_type")
                      ((PERMISSION LOCK-BUTTON-PERMISSION
                        "permission" "GPermission" T T)
                       (TEXT-LOCK LOCK-BUTTON-TEXT-LOCK
                        "text-lock" "gchararray" T T)
                       (TEXT-UNLOCK LOCK-BUTTON-TEXT-UNLOCK
                        "text-unlock" "gchararray" T T)
                       (TOOLTIP-LOCK LOCK-BUTTON-TOOLTIP-LOCK
                        "tooltip-lock" "gchararray" T T)
                       (TOOLTIP-NOT-AUTHORIZED LOCK-BUTTON-TOOLTIP-NOT-AUTHORIZED
                        "tooltip-not-authorized" "gchararray" T T)
                       (TOOLTIP-UNLOCK LOCK-BUTTON-TOOLTIP-UNLOCK
                        "tooltip-unlock" "gchararray" T T)))
             (gobject:get-gtype-definition "GtkLockButton"))))

;;; --- Properties -------------------------------------------------------------

;;;     permission
;;;     text-lock
;;;     text-unlock
;;;     tooltip-lock
;;;     tooltip-not-authorized
;;;     tooltip-unlock

#+crategus
(test gtk-lock-button-properties
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (button)
      (is (typep (setf button (make-instance 'gtk:lock-button)) 'gtk:lock-button))
      (is-false (gtk:lock-button-permission button))
      (is (string= "Sperren" (gtk:lock-button-text-lock button)))
      (is (string= "Entsperren" (gtk:lock-button-text-unlock button)))
      (is (string= "Dialog ist entsperrt.
Klicken Sie, um Ände-
rungen zu verhindern" (gtk:lock-button-tooltip-lock button)))
      (is (string= "Die Systemrichtlinien verhindern Änderungen.
Bitte kontaktieren Sie Ihren Systemadministrator"
                   (gtk:lock-button-tooltip-not-authorized button)))
      (is (string= "Dialog ist gesperrt.
Klicken Sie für
Änderungen" (gtk:lock-button-tooltip-unlock button))))))

;;; --- Functions --------------------------------------------------------------

;;;     gtk_lock_button_new

(test gtk-lock-button-new
  (let ((*gtk-warn-deprecated* nil))
    (glib-test:with-check-memory (button permission)
      (is (typep (setf permission (g:simple-permission-new t))
                 'g:simple-permission))
      (is (typep (setf button (gtk:lock-button-new permission))
                 'gtk:lock-button))
      ;; Remove references
      (is-false (setf (gtk:lock-button-permission button) nil)))))

;;; 2025-08-20
